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

(provide 'agent-shell-codebuddy-tests)
;;; agent-shell-codebuddy-tests.el ends here
