;;; agent-shell-codebuddy.el --- CodeBuddy agent configurations -*- lexical-binding: t; -*-

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
;; This file includes CodeBuddy-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--mcp-servers "agent-shell")
(declare-function agent-shell--dwim "agent-shell")
(defvar agent-shell-mcp-servers)

(cl-defun agent-shell-codebuddy-make-authentication (&key api-key)
  "Create CodeBuddy authentication configuration.

API-KEY is the CodeBuddy API key string or function that returns it.

Only API-KEY should be provided."
  (unless api-key
    (error "Must specify :api-key"))
  `((:api-key . ,api-key)))

(defcustom agent-shell-codebuddy-authentication
  nil
  "Configuration for CodeBuddy authentication.
For API key (string):

  (setq agent-shell-codebuddy-authentication
        (agent-shell-codebuddy-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-codebuddy-authentication
        (agent-shell-codebuddy-make-authentication :api-key (lambda () ...)))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-codebuddy-command
  '("codebuddy" "--acp")
  "Command and parameters for the CodeBuddy client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-codebuddy-environment
  nil
  "Environment variables for the CodeBuddy client.

This should be a list of environment variables to be used when
starting the CodeBuddy client process.

Example usage to set custom environment variables:

  (setq agent-shell-codebuddy-environment
        (`agent-shell-make-environment-variables'
         \"CODEBUDDY_API_KEY\" \"your-key\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-codebuddy-make-agent-config ()
  "Create a CodeBuddy agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'codebuddy
   :mode-line-name "CodeBuddy"
   :buffer-name "CodeBuddy"
   :shell-prompt "CodeBuddy> "
   :shell-prompt-regexp "CodeBuddy> "
   :icon-name "https://cnb.cool/codebuddy/codebuddy-code/-/git/raw/main/codebuddy-cat.png"
   :welcome-function #'agent-shell-codebuddy--welcome-message
   :client-maker (lambda (buffer)
                   (agent-shell-codebuddy-make-client :buffer buffer))
   :install-instructions "See https://www.codebuddy.ai/docs/cli/acp for installation."))

(defun agent-shell-codebuddy-start-agent ()
  "Start an interactive CodeBuddy agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-codebuddy-make-agent-config)
                     :new-shell t))

(defun agent-shell-codebuddy--mcp-config-json ()
  "Convert `agent-shell-mcp-servers' to a JSON string for --mcp-config.

CodeBuddy's --mcp-config flag expects an object keyed by server name,
whereas the ACP protocol uses an array with a \"name\" field and headers
as a [{name value}] array.  This function performs both conversions:

- Outer structure: array -> object keyed by server name
- Headers: [{name value}] array -> {\"Header-Name\": \"value\"} object"
  (when agent-shell-mcp-servers
    (let* ((servers (agent-shell--mcp-servers))
           (servers-ht (make-hash-table :test 'equal)))
      (mapc (lambda (server)
              (let* ((name (map-elt server 'name))
                     (cfg (map-filter (lambda (k _) (not (eq k 'name))) server))
                     (headers (map-elt cfg 'headers))
                     (headers-ht
                      (when (and headers (> (length headers) 0))
                        (let ((ht (make-hash-table :test 'equal)))
                          (mapc (lambda (h)
                                  (puthash (map-elt h 'name)
                                           (map-elt h 'value)
                                           ht))
                                (append headers nil))
                          ht))))
                (when headers-ht
                  (map-put! cfg 'headers headers-ht))
                (puthash name cfg servers-ht)))
            (append servers nil))
      (json-serialize `((mcpServers . ,servers-ht))))))

(cl-defun agent-shell-codebuddy-make-client (&key buffer)
  "Create a CodeBuddy client using BUFFER as context.

Uses `agent-shell-codebuddy-authentication' for authentication configuration.

When `agent-shell-mcp-servers' is set, passes MCP servers via
\"--mcp-config\" and \"--strict-mcp-config\" CLI flags.  This ensures
the dynamic MCP scope takes effect at startup (before MCP
initialization), correctly overriding any servers with the same name
in the user-level ~/.codebuddy/.mcp.json."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (let* ((api-key (agent-shell-codebuddy-key))
         (mcp-json (agent-shell-codebuddy--mcp-config-json))
         (command-params (if mcp-json
                             (append (cdr agent-shell-codebuddy-command)
                                     (list "--mcp-config" mcp-json
                                           "--strict-mcp-config"))
                           (cdr agent-shell-codebuddy-command))))
    (agent-shell--make-acp-client :command (car agent-shell-codebuddy-command)
                                  :command-params command-params
                                  :environment-variables (append (cond (api-key
                                                                        (list (format "CODEBUDDY_API_KEY=%s" api-key)))
                                                                       (t
                                                                        (error "Missing CodeBuddy API key (see agent-shell-codebuddy-authentication)")))
                                                                 agent-shell-codebuddy-environment)
                                  :context-buffer buffer)))

(defun agent-shell-codebuddy-key ()
  "Get the CodeBuddy API key."
  (cond ((stringp (map-elt agent-shell-codebuddy-authentication :api-key))
         (map-elt agent-shell-codebuddy-authentication :api-key))
        ((functionp (map-elt agent-shell-codebuddy-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-codebuddy-authentication :api-key))
           (error
            (error "API key not found.  Check out `agent-shell-codebuddy-authentication'"))))
        (t
         nil)))

(defun agent-shell-codebuddy--welcome-message (config)
  "Return CodeBuddy welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-codebuddy--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-codebuddy--ascii-art ()
  "CodeBuddy ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
  ██████╗   ██████╗   ██████╗   ███████╗  ██████╗   ██╗   ██╗  ██████╗   ██████╗   ██╗   ██╗
 ██╔════╝  ██╔═══██╗  ██╔══██╗  ██╔════╝  ██╔══██╗  ██║   ██║  ██╔══██╗  ██╔══██╗  ╚██╗ ██╔╝
 ██║       ██║   ██║  ██║  ██║  █████╗    ██████╔╝  ██║   ██║  ██║  ██║  ██║  ██║   ╚████╔╝
 ██║       ██║   ██║  ██║  ██║  ██╔══╝    ██╔══██╗  ██║   ██║  ██║  ██║  ██║  ██║    ╚██╔╝
 ╚██████╗  ╚██████╔╝  ██████╔╝  ███████╗  ██████╔╝  ╚██████╔╝  ██████╔╝  ██████╔╝     ██║
  ╚═════╝   ╚═════╝   ╚═════╝   ╚══════╝  ╚═════╝    ╚═════╝   ╚═════╝   ╚═════╝      ╚═╝
  ██████╗   ██████╗   ██████╗   ███████╗
 ██╔════╝  ██╔═══██╗  ██╔══██╗  ██╔════╝
 ██║       ██║   ██║  ██║  ██║  █████╗
 ██║       ██║   ██║  ██║  ██║  ██╔══╝
 ╚██████╗  ╚██████╔╝  ██████╔╝  ███████╗
  ╚═════╝   ╚═════╝   ╚═════╝   ╚══════╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#16a34a" :inherit fixed-pitch)
                                       '(:foreground "#15803d" :inherit fixed-pitch)))))

(provide 'agent-shell-codebuddy)

;;; agent-shell-codebuddy.el ends here
