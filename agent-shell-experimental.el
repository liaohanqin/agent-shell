;;; agent-shell-experimental.el --- Experimental ACP features -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; Experimental ACP features for agent-shell.
;;
;; session/push: Server-initiated prompt push.  The server sends
;; a request to the client, followed by session/update notifications,
;; concluded by an session_push_end notification.  The client
;; then responds to the original request.
;;
;; _session/steering: Steer a prompt into the turn already running,
;; instead of queueing it until the turn ends.  The client sends the
;; request; the agent answers whether the prompt joined the running
;; turn, or why it could not.  The steered prompt's own output arrives
;; as ordinary session/update notifications on the turn already in
;; flight, so nothing else changes.
;;
;; session/steer: CodeBuddy's own steering method.  Same idea as
;; _session/steering under a different name, and never advertised, so
;; it is picked by agent identifier rather than by what the handshake
;; said.  It answers with `steered' and an optional `reason' instead of
;; an `outcome'.  See `agent-shell--steering-dialect'.

;;; Code:

(require 'map)
(require 'agent-shell-prompt)
(eval-when-compile
  (require 'cl-lib))

(declare-function acp-send-response "acp")
(declare-function acp-make-error "acp")
(declare-function agent-shell--active-requests-p "agent-shell")
(declare-function agent-shell--cancel-idle-timer "agent-shell")
(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--expand-truncated-regions "agent-shell")
(declare-function agent-shell--prompt-content-blocks "agent-shell")
(declare-function agent-shell--prompt-queue-enqueue "agent-shell-prompt-queue")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--append-transcript "agent-shell")
(declare-function agent-shell--indent-markdown-headers "agent-shell")
(declare-function agent-shell--reset-undo-history "agent-shell")
(declare-function agent-shell--make-boxed-message "agent-shell")
(declare-function agent-shell--send-request "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--steering-dialect "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--update-text "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function shell-maker-busy "shell-maker")
(declare-function agent-shell-heartbeat-start "agent-shell-heartbeat")
(declare-function agent-shell-heartbeat-stop "agent-shell-heartbeat")
(declare-function shell-maker-insert-end-of-prompt-marker "shell-maker")

(defvar agent-shell-show-busy-indicator)
(defvar agent-shell--transcript-file)
(defvar shell-maker--busy)

(cl-defun agent-shell-experimental--on-session-push-request (&key state acp-request)
  "Handle an incoming session/push ACP-REQUEST with STATE.

The server pushes a prompt to the client, followed by session/update
notifications.  The client sends the response after receiving an
session_push_end notification.

If the client is busy (an active session/prompt or session/push is
in progress), the request is immediately rejected with an error."
  (if (seq-find (lambda (r)
                  (member (map-elt r :method)
                          '("session/prompt" "session/push")))
                (map-elt state :active-requests))
      ;; Busy. Reject push request.
      (acp-send-response
       :client (map-elt state :client)
       :response (agent-shell-experimental--make-session-push-response
                  :request-id (map-elt acp-request 'id)
                  :error (acp-make-error :code -32000
                                         :message "Busy")))
    (let ((request (agent-shell-experimental--normalize-request acp-request)))
      ;; Track as active so notifications are not treated as stale.
      (unless (assq :active-requests state)
        (nconc state (list (cons :active-requests nil))))
      (map-put! state :active-requests
                (cons request (map-elt state :active-requests))))
    ;; Remove trailing empty shell prompt before push notifications render.
    (agent-shell-experimental--remove-trailing-prompt)
    ;; Give the pushed agent content its own end-of-prompt boundary so it
    ;; renders as an agent turn.  Without it the content attaches to the
    ;; previous turn with no agent marker, and chat mode (which anchors the
    ;; agent label on the marker) mislabels it as a user `Me' turn.
    (when-let* ((buffer (map-elt state :buffer)))
      (with-current-buffer buffer
        ;; A persistent prompt survives the removal above, and the marker
        ;; appends at `point-max', which is below it.  Narrow so the
        ;; boundary lands where the pushed content will render.
        (agent-shell--with-buffer-narrowed-to (agent-shell--live-prompt-start)
          (shell-maker-insert-end-of-prompt-marker))))
    ;; Mark busy so requests are queued rather than sent mid-push.
    ;; Cleared on session_push_end via `agent-shell--finish-output'.
    (setq shell-maker--busy t)
    (when agent-shell-show-busy-indicator
      (agent-shell-heartbeat-start
       :heartbeat (map-elt state :heartbeat)))
    (map-put! state :last-entry-type "session/push")))

(defun agent-shell-experimental--remove-trailing-prompt ()
  "Remove the trailing empty shell prompt if it is at end of buffer.

No-op under `agent-shell-persistent-prompt-enabled', where the shell is meant
to keep a prompt at the buffer end at all times: the push renders above
it, and deleting it here would take any unsubmitted input with it and
leave the shell with nowhere to type."
  (when-let* (((not agent-shell-persistent-prompt-enabled))
              (comint-last-prompt)
              (prompt-start (car comint-last-prompt))
              (prompt-end (cdr comint-last-prompt))
              ((= (marker-position prompt-end) (point-max))))
    (let ((inhibit-read-only t))
      (delete-region (marker-position prompt-start) (point-max)))))

(cl-defun agent-shell-experimental--on-session-push-end (&key state on-finished)
  "Handle session_push_end notification with STATE.

Finds the active push prompt request, sends the response, and
removes it from active requests.  Calls ON-FINISHED when done
to allow the caller to finalize (e.g. display a new shell prompt)."
  (when-let* ((push-request (seq-find (lambda (r)
                                        (equal (map-elt r :method) "session/push"))
                                      (map-elt state :active-requests))))
    (acp-send-response
     :client (map-elt state :client)
     :response (agent-shell-experimental--make-session-push-response
                :request-id (map-elt push-request :id)))
    (map-put! state :active-requests
              (seq-remove (lambda (r)
                            (equal (map-elt r :method) "session/push"))
                          (map-elt state :active-requests)))
    (agent-shell-heartbeat-stop
     :heartbeat (map-elt state :heartbeat))
    (map-put! state :last-entry-type "session_push_end")
    (when on-finished
      (funcall on-finished))))

(cl-defun agent-shell-experimental--make-session-push-response (&key request-id error)
  "Instantiate a \"session/push\" response.

REQUEST-ID is the ID of the incoming server request this responds to.
ERROR is an optional error object if the push prompt was rejected."
  (unless request-id
    (error ":request-id is required"))
  (if error
      `((:request-id . ,request-id)
        (:error . ,error))
    `((:request-id . ,request-id)
      (:result . nil))))

(defun agent-shell-experimental--methods ()
  "Return the list of experimental methods that replay session notifications."
  '("session/push"))

(cl-defun agent-shell-experimental--make-session-steering-request (&key session-id prompt)
  "Instantiate a \"_session/steering\" request for SESSION-ID carrying PROMPT.

Not part of the ACP spec -- the leading underscore marks it as an
extension -- but implemented under this name by the Claude and Codex
adapters, which advertise it as `_meta.steering.supported' in their
`initialize' response.  The ACP proposal that would standardise this is
`session/inject' (RFD 1261), still unmerged and targeting v2.

PROMPT is a vector of content blocks, the same shape `session/prompt'
takes.

The `idleBehavior' opt-in asks the agent to do nothing and say so when
no turn is running, rather than starting a detached turn out of our
sight.  Only the Claude adapter honours it; Codex's parameter parser
passes unknown keys through and ignores them, so a steer that races the
end of a turn can still come back `startedNewTurn' there.

For example:

  (agent-shell-experimental--make-session-steering-request
   :session-id \"sess-1\"
   :prompt [((type . \"text\") (text . \"just the filenames\"))])

  => ((:method . \"_session/steering\")
      (:params . ((sessionId . \"sess-1\")
                  (prompt . [((type . \"text\")
                              (text . \"just the filenames\"))])
                  (_meta . ((steering
                             . ((idleBehavior . \"promptRequired\"))))))))"
  (unless session-id
    (error ":session-id is required"))
  (unless prompt
    (error ":prompt is required"))
  `((:method . "_session/steering")
    (:params . ((sessionId . ,session-id)
                (prompt . ,(vconcat prompt))
                (_meta . ((steering . ((idleBehavior . "promptRequired")))))))))

(cl-defun agent-shell-experimental--make-session-steer-request (&key session-id prompt)
  "Instantiate a \"session/steer\" request for SESSION-ID carrying PROMPT.

Not part of the ACP spec and never advertised: CodeBuddy implements this
method but says nothing about steering in its `initialize' response, so
the request is picked by agent identifier.  See
`agent-shell--steering-dialect'.

PROMPT is a vector of content blocks, the same shape `session/prompt'
takes.  The agent answers whether the prompt landed: `steered' true, or
`steered' false with a `reason' of \"idle\" when no turn was running.
The method's \"stale\" reason needs an `expectedRequestId' this client
never sends, so a steer is either taken or answered idle.

For example:

  (agent-shell-experimental--make-session-steer-request
   :session-id \"sess-1\"
   :prompt [((type . \"text\") (text . \"just the filenames\"))])

  => ((:method . \"session/steer\")
      (:params . ((sessionId . \"sess-1\")
                  (contentBlocks . [((type . \"text\")
                                     (text . \"just the filenames\"))]))))"
  (unless session-id
    (error ":session-id is required"))
  (unless prompt
    (error ":prompt is required"))
  `((:method . "session/steer")
    (:params . ((sessionId . ,session-id)
                (contentBlocks . ,(vconcat prompt))))))

(defun agent-shell-experimental--steering-outcome (dialect acp-response)
  "Return the steering outcome ACP-RESPONSE reports for DIALECT.

The outcome vocabulary `agent-shell-experimental--send-steering' acts
on.  DIALECT is as `agent-shell--steering-dialect' names it: agents
speaking the advertised `_session/steering' extension answer with
`outcome' itself, while CodeBuddy's `session/steer' answers with
`steered' and an optional `reason'.  Its `idle' reason means no turn was
running and the agent started none of its own -- the extension's
\"promptRequired\" situation reached from the other direction -- so it
maps there, and the prompt is submitted normally when this shell is
free, or reported and kept (at the prompt, or queued when there is no
live one) when it is still busy.  Anything else is nil, which
`agent-shell-experimental--send-steering' reads as a steer that did not
land.

For example:

  (agent-shell-experimental--steering-outcome
   \\='session-steer
   \\='((steered . t) (ownerRequestId . \"req-7\")))
  => \"injected\""
  (pcase dialect
    ('session-steer
     (cond ((eq (map-elt acp-response 'steered) t)
            "injected")
           ((equal (map-elt acp-response 'reason) "idle")
            "promptRequired")))
    (_ (map-elt acp-response 'outcome))))

(cl-defun agent-shell-experimental--send-steering (&key state prompt)
  "Steer PROMPT into the turn STATE's session is currently running.

Must be called from the shell buffer: PROMPT is converted with
`agent-shell--prompt-content-blocks', which reads the buffer's prompt
capabilities.

Steers with whichever method the agent answers to: the advertised
`_session/steering' extension, or CodeBuddy's `session/steer', which is
never advertised (see `agent-shell--steering-dialect').  Both answers
are normalised by `agent-shell-experimental--steering-outcome' into the
outcomes below, so CodeBuddy's `idle' acts like \"promptRequired\".

Acts on the `outcome' the agent answers with:

  \"injected\"        - the prompt joined the turn that was running, so
                      render it as the user prompt it is.  Claude, Codex
                      and CodeBuddy.
  \"startedNewTurn\"  - no turn was running and the agent started one of
                      its own, which we did not ask for and cannot track.
                      Claude and Codex.
  \"promptRequired\"  - no turn was running and the agent left the prompt
                      with us to submit normally.  Claude, and only
                      because the request opts into it; Codex ignores the
                      opt-in, though codex-acp#363 would bring it in line.
                      CodeBuddy answers `idle' instead.
  \"failed\"          - the agent could not apply the steer.  Codex only.

Agents differ on which they answer with, so an unrecognised outcome, and
a request that fails outright, are treated as a steer that did not land:
reported in the shell, and the running turn interrupted.

A CodeBuddy \"promptRequired\" is the exception: its `idle' also covers run
states that are merely not steerable (preparing, compacting, or between
model and tool work), where the turn is still running.  When this shell
is still busy, the prompt is put back for the user to resend and the
turn is left running rather than interrupted.

Nothing here queues a prompt the agent took.  A declined one is put back
at the live prompt when the shell keeps one, and queued only when there
is nowhere else to hold it."
  (let ((dialect (agent-shell--steering-dialect state))
        (content-blocks (agent-shell--prompt-content-blocks
                         (agent-shell--expand-truncated-regions prompt))))
    (agent-shell--send-request
     :state state
     :client (map-elt state :client)
     :request (pcase dialect
                ('session-steer
                 (agent-shell-experimental--make-session-steer-request
                  :session-id (map-nested-elt state '(:session :id))
                  :prompt content-blocks))
                ;; The advertised extension, and what a state naming no
                ;; dialect keeps sending; callers reach here only after
                ;; `agent-shell-steering-supported-p'.
                (_
                 (agent-shell-experimental--make-session-steering-request
                  :session-id (map-nested-elt state '(:session :id))
                  :prompt content-blocks)))
     :buffer (current-buffer)
     :on-failure (lambda (acp-error _raw-message)
                   (agent-shell--update-fragment
                    :state (agent-shell--state)
                    :block-id (format "%s-steer-declined"
                                      (map-elt (agent-shell--state) :request-count))
                    :body (agent-shell--make-boxed-message
                           ;; Unlike an agent answering with an outcome, a
                           ;; request that failed outright says nothing about
                           ;; why, so carry its error text.
                           :text (if (map-elt acp-error 'message)
                                     (format "Note: Steered prompt declined (%s)."
                                             (map-elt acp-error 'message))
                                   "Note: Steered prompt declined."))
                    :create-new t
                    :above-last-prompt (not (agent-shell--active-requests-p
                                             (agent-shell--state))))
                   (map-put! (agent-shell--state) :last-entry-type "steering_declined")
                   ;; Interrupted so the agent does not carry on for a long
                   ;; while in a direction the user believes they already
                   ;; corrected.  A declined steer does not say whether
                   ;; continuing is harmless, and the cost of guessing wrong
                   ;; is far higher one way than interrupting current work.
                   (agent-shell-interrupt t))
     :on-success
     (lambda (acp-response)
       (let ((outcome (agent-shell-experimental--steering-outcome
                       dialect acp-response)))
         (pcase outcome
           ;; Claude, Codex and CodeBuddy all answer with this one.
           ("injected"
            (agent-shell-experimental--render-steered-prompt
             :state (agent-shell--state) :prompt prompt))
           ;; Claude, and only because the request opts into it with
           ;; _meta.steering.idleBehavior set to "promptRequired";
           ;; CodeBuddy's `idle' normalises here.  A shell that has not
           ;; yet processed its own `session/prompt' response is still
           ;; busy, where submitting errors ("Busy, try later") without
           ;; inserting.  Claude's busy case falls through to declined;
           ;; CodeBuddy's keeps the prompt, see below.
           ((and "promptRequired" (guard (not (shell-maker-busy))))
            (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t))
           ;; Claude and Codex both answer with this one.  No `session/prompt'
           ;; owns this turn, so nothing signals when it ends: its output
           ;; arrives out of turn and the shell does not show as busy.  See
           ;; claude-agent-acp#903.
           ;;
           ;; TODO: Track the turn once an end signal exists.  claude-agent-acp
           ;; proposes `_session/turn_ended' for exactly this in #997, which
           ;; would let us close the turn out rather than just warn about it.
           ("startedNewTurn"
            (agent-shell--update-fragment
             :state (agent-shell--state)
             :block-id (format "%s-steer-detached-turn"
                               (map-elt (agent-shell--state) :request-count))
             :body (agent-shell--make-boxed-message
                    :text "Note: Steered prompt still in progress.")
             :create-new t
             :above-last-prompt (not (agent-shell--active-requests-p
                                      (agent-shell--state))))
            (map-put! (agent-shell--state) :last-entry-type "steering_detached_turn")
            ;; The turn ending is what started this timer, and it would now
            ;; announce the agent idle while the turn it just began runs.
            ;; Nothing re-arms it for this turn, since that happens on the
            ;; `session/prompt' response there will never be.  A permission
            ;; request during the turn still arms it on its own.
            (agent-shell--cancel-idle-timer))
           ;; Codex's "failed", a "promptRequired" this shell is too busy to
           ;; act on, CodeBuddy's other refusals, and anything an agent we
           ;; do not know about answers with.
           (_
            (let* ((keep-prompt
                    ;; CodeBuddy answers "idle" whenever its run state cannot
                    ;; take a steer (preparing, compacting, between model and
                    ;; tool work), not only when the turn ended.  The turn is
                    ;; then still running, so interrupting would discard work
                    ;; the user never asked to drop: put the prompt back and
                    ;; let them decide instead.
                    (and (eq dialect 'session-steer)
                         (equal outcome "promptRequired")
                         (shell-maker-busy)))
                   ;; Kept before the note is written, so the note can say
                   ;; where the prompt went: back at the prompt for the user
                   ;; to resend, or queued until the turn ends when the shell
                   ;; keeps no live prompt.
                   (kept-as (and keep-prompt
                                 (agent-shell-experimental--keep-declined-prompt
                                  :state (agent-shell--state) :prompt prompt))))
              (agent-shell--update-fragment
               :state (agent-shell--state)
               :block-id (format "%s-steer-declined"
                                 (map-elt (agent-shell--state) :request-count))
               :body (agent-shell--make-boxed-message
                      :text (if outcome
                                (format "Note: Steered prompt declined (%s).%s"
                                        outcome
                                        (pcase kept-as
                                          ('kept " Prompt kept at the prompt.")
                                          ('queued
                                           " Prompt queued until the turn ends.")
                                          (_ "")))
                              "Note: Steered prompt declined."))
               :create-new t
               :above-last-prompt (or kept-as
                                       (not (agent-shell--active-requests-p
                                             (agent-shell--state)))))
              (map-put! (agent-shell--state) :last-entry-type "steering_declined")
              (unless keep-prompt
                ;; Interrupted so the agent does not carry on for hours in a
                ;; direction the user believes they already corrected.  A
                ;; declined steer does not say whether continuing is
                ;; harmless, and the cost of guessing wrong is far higher
                ;; one way than the other.
                (agent-shell-interrupt t))))))))))

(cl-defun agent-shell-experimental--keep-declined-prompt (&key state prompt)
  "Keep PROMPT after the agent declined to steer it into STATE's turn.

Puts PROMPT back at STATE's live prompt for the user to edit and send
again, and returns `kept'.  A busy shell that keeps no live prompt has
nowhere to put it, so the prompt is queued instead and `queued' is
returned."
  (with-current-buffer (map-elt state :buffer)
    (if (agent-shell--can-insert-into-prompt-p)
        (progn (agent-shell--insert-to-shell-buffer
                ;; Explicit, not left to `agent-shell--shell-buffer' to
                ;; resolve.
                :text prompt :no-focus t
                :shell-buffer (map-elt state :buffer))
               'kept)
      (agent-shell--prompt-queue-enqueue :prompt prompt)
      'queued)))

(cl-defun agent-shell-experimental--render-steered-prompt (&key state prompt)
  "Render PROMPT into STATE's shell as the user prompt it is.

Claude and Codex never echo a steered prompt back, and the client
renders it or it does not appear at all.  CodeBuddy forwards the
injected prompt as a `user_message_chunk', but the shell only renders
such a chunk while a `session/load' or `session/push' is active
\(`agent-shell--on-notification'), and the turn's own `session/prompt'
leaves it a suppressed no-op here, so the rendering below is the one
that stands.

Uses the field/face shape comint gives a live prompt, so
`comint-next-prompt', `agent-shell-next-item', copying and screen-reader
prompt navigation treat it as a user prompt rather than as a new kind of
entry.  `shell-maker-insert-end-of-prompt-marker' then closes it, which
is what bounds the prompt for anything measuring it by searching forward
for the marker; without it such a search runs to the next submission and
takes the rest of the turn's output as this prompt's body.

The `[steer]' prefix carries the one thing this shape would otherwise
lose -- that the prompt was injected into a running turn rather than
typed before one.

The turn can end while the steer is in flight, leaving a live input
prompt at the buffer end by the time the agent answers.  Rendering above
it then keeps this out of comint's input area, where submitting would
send it as input."
  (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
  (agent-shell--append-transcript
   :text (format "## User (steered) (%s)\n\n%s\n\n"
                 (format-time-string "%F %T")
                 (agent-shell--indent-markdown-headers prompt))
   :file-path agent-shell--transcript-file)
  (with-current-buffer (map-elt state :buffer)
    ;; Both inserts land above a live input prompt.
    ;; `shell-maker-insert-end-of-prompt-marker' documents that narrowing as
    ;; the way to synthesize history above a live prompt.
    (let ((late-prompt-start (and (or agent-shell-persistent-prompt-enabled
                                      (not (shell-maker-busy)))
                                  (agent-shell--live-prompt-start))))
      (agent-shell--with-buffer-narrowed-to late-prompt-start
            (agent-shell--update-text
             :state state
             :block-id (format "%s-steered-user_message_chunk"
                               (map-elt state :chunked-group-count))
             :text (concat (propertize (map-nested-elt state '(:agent-config :shell-prompt))
                                       'font-lock-face 'agent-shell-prompt
                                       'field 'output)
                           (propertize (concat "[steer] " prompt)
                                       'font-lock-face 'agent-shell-input))
             :create-new t)
            (shell-maker-insert-end-of-prompt-marker)
            ;; Under the narrowing `point-max' is the live prompt's start,
            ;; so this keeps that prompt on its own line.
            (when late-prompt-start
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "\n"))))
      (when late-prompt-start
        ;; Rendering above the prompt pushed any unsubmitted input down, so
        ;; undo entries recorded for it point at the shifted text.
        (agent-shell--reset-undo-history)))
    ;; An accepted steer is a user submission. Announce it.
    (agent-shell--emit-event
     :event 'input-submitted
     :data (list (cons :prompt (substring-no-properties prompt)))))
  ;; Deliberately not "user_message_chunk": that value asks the replay path
  ;; to insert the end-of-prompt marker on the next notification, and one
  ;; has already gone in above.
  (map-put! state :last-entry-type "steered_user_message"))

(defun agent-shell-experimental--normalize-request (request)
  "Normalize REQUEST from JSON symbol keys to keyword keys.

Incoming JSON-parsed requests use symbol keys (e.g. \\='method),
while internal request objects use keyword keys (e.g. :method).
This function converts the known keys that `acp--request-sender'
manually translates on the way out.

Example:

  \\='((method . \"session/push\")
    (id . 3)
    (params . ((prompt . [...]))))

becomes:

  \\='((:method . \"session/push\")
    (:id . 3)
    (:params . ((prompt . [...]))))"
  (seq-map (lambda (pair)
             (let ((key (car pair)))
               (cons (pcase key
                       ('method :method)
                       ('params :params)
                       ('id :id)
                       ('jsonrpc :jsonrpc)
                       (_ key))
                     (cdr pair))))
           request))

(provide 'agent-shell-experimental)

;;; agent-shell-experimental.el ends here
