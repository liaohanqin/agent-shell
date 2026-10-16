;;; agent-shell-codebuddy-tests.el --- Tests for agent-shell-codebuddy -*- lexical-binding: t; -*-

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
;; Tests for agent-shell-codebuddy.
;;

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell)
(require 'agent-shell-codebuddy)

(ert-deftest agent-shell-codebuddy-make-client-with-mcp-servers-test ()
  "Test CodeBuddy client passes --mcp-config without --strict-mcp-config.

When `agent-shell-mcp-servers' is set, the dynamic MCP scope is passed
via --mcp-config, but the user-level ~/.codebuddy/.mcp.json must remain
active, so --strict-mcp-config must NOT be passed."
  (let* ((agent-shell-codebuddy-authentication '(:api-key "test-api-key"))
         (agent-shell-codebuddy-command '("codebuddy" "--acp"))
         (agent-shell-mcp-servers
          '(((name . "test-server")
             (type . "http")
             (url . "https://example.com/mcp"))))
         (test-buffer (get-buffer-create "*codebuddy-mcp-test-buffer*"))
         (client (agent-shell-codebuddy-make-client :buffer test-buffer))
         (params (map-elt client :command-params)))
    (unwind-protect
        (progn
          (should (equal (map-elt client :command) "codebuddy"))
          (should (equal (car params) "--acp"))
          (should (member "--mcp-config" params))
          (should-not (member "--strict-mcp-config" params))
          ;; The --mcp-config value must serialize to an object keyed by
          ;; server name and survive JSON round-trip.
          (let* ((idx (seq-position params "--mcp-config"))
                 (json (nth (1+ idx) params))
                 (parsed (json-parse-string json)))
            (should (equal (map-nested-elt parsed '("mcpServers" "test-server" "type"))
                           "http"))))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest agent-shell-codebuddy-make-client-without-mcp-servers-test ()
  "Test CodeBuddy client omits --mcp-config when no servers are set."
  (let* ((agent-shell-codebuddy-authentication '(:api-key "test-api-key"))
         (agent-shell-codebuddy-command '("codebuddy" "--acp"))
         (agent-shell-mcp-servers nil)
         (test-buffer (get-buffer-create "*codebuddy-no-mcp-test-buffer*"))
         (client (agent-shell-codebuddy-make-client :buffer test-buffer))
         (params (map-elt client :command-params)))
    (unwind-protect
        (progn
          (should (equal params '("--acp")))
          (should-not (member "--mcp-config" params))
          (should-not (member "--strict-mcp-config" params)))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest agent-shell-codebuddy-ascii-art-test ()
  "Test CodeBuddy ASCII art renders intact with brand colors.

The wordmark uses solid ANSI Shadow glyphs (block and box-drawing
characters, no slash strokes), and the cat pixels must carry the
CodeBuddy teal."
  (let* ((art (agent-shell-codebuddy--ascii-art))
         ;; The cat's left ear: the cat rows are the only lines that
         ;; start with an indented block-character run.  The wordmark
         ;; (gradient-colored) is indented further, so anchor at the
         ;; line start to land inside the cat-colored columns.
         (cat-pixel (string-match "^   ████" art)))
    (should (equal (substring-no-properties art)
                   "                             ██████╗  ██████╗  ██████╗  ███████╗
                            ██╔════╝ ██╔═══██╗ ██╔══██╗ ██╔════╝
   ████          ████       ██║      ██║   ██║ ██║  ██║ █████╗
   ████▄▄▄▄▄▄▄▄▄▄████       ██║      ██║   ██║ ██║  ██║ ██╔══╝
▄▄▄█                █▄▄▄    ╚██████╗ ╚██████╔╝ ██████╔╝ ███████╗
████                ████     ╚═════╝  ╚═════╝  ╚═════╝  ╚══════╝
████    ██    ██    ████
████    ██    ██    ████    ██████╗  ██╗   ██╗ ██████╗  ██████╗  ██╗   ██╗
████                ████    ██╔══██╗ ██║   ██║ ██╔══██╗ ██╔══██╗ ╚██╗ ██╔╝
   ██████████████████       ██████╔╝ ██║   ██║ ██║  ██║ ██║  ██║  ╚████╔╝
                            ██╔══██╗ ██║   ██║ ██║  ██║ ██║  ██║   ╚██╔╝
                            ██████╔╝ ╚██████╔╝ ██████╔╝ ██████╔╝    ██║
                            ╚═════╝   ╚═════╝  ╚═════╝  ╚═════╝     ╚═╝"))
    (should (equal (plist-get (get-text-property cat-pixel
                                                 'font-lock-face art)
                              :foreground)
                   (if (eq (frame-parameter nil 'background-mode) 'dark)
                       "#00BC90" "#00916e")))
    ;; Half-block glyphs must not inherit a CJK fixed-pitch font
    ;; (full-width blocks break the pixel grid).
    (should (equal (plist-get (get-text-property cat-pixel
                                                 'font-lock-face art)
                              :family)
                   (agent-shell-codebuddy--welcome-font-family)))))

(ert-deftest agent-shell-codebuddy-welcome-font-family-test ()
  "Test the half-width-block font preference order.

DejaVu Sans Mono ships with most GNU/Linux systems but not macOS,
where Menlo is the built-in match."
  (cl-letf (((symbol-function 'find-font) (lambda (_spec) 'font)))
    (should (equal (agent-shell-codebuddy--welcome-font-family)
                   "DejaVu Sans Mono")))
  (cl-letf (((symbol-function 'find-font)
             (lambda (spec)
               (when (equal (format "%s" (font-get spec :family)) "Menlo")
                 'font))))
    (should (equal (agent-shell-codebuddy--welcome-font-family)
                   "Menlo")))
  (cl-letf (((symbol-function 'find-font) (lambda (_spec) nil)))
    (should (equal (agent-shell-codebuddy--welcome-font-family)
                   "monospace"))))

(provide 'agent-shell-codebuddy-tests)
;;; agent-shell-codebuddy-tests.el ends here
