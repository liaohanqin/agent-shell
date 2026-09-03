;;; agent-shell-godot-tests.el --- Tests for agent-shell-godot -*- lexical-binding: t; -*-

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
;; Tests for agent-shell-godot.
;;

;;; Code:

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-codebuddy)
(require 'agent-shell-godot)

(ert-deftest agent-shell-godot-mcp-server-default-test ()
  "Default `agent-shell-godot-mcp-server' returns a stdio godot server."
  (let ((agent-shell-godot-mcp-command '("npx" "-y" "godot-mcp-server")))
    (should (equal (agent-shell-godot-mcp-server)
                   '(((name . "godot")
                      (command . "npx")
                      (args . ("-y" "godot-mcp-server"))
                      (env . ())))))))

(ert-deftest agent-shell-godot-mcp-server-disabled-test ()
  "`agent-shell-godot-mcp-server' returns nil when the command is nil."
  (let ((agent-shell-godot-mcp-command nil))
    (should (equal (agent-shell-godot-mcp-server) nil))))

(ert-deftest agent-shell-godot-make-agent-config-mcp-servers-test ()
  "The Godot config wires the MCP server into `:mcp-servers'."
  (let ((agent-shell-godot-mcp-command '("npx" "-y" "godot-mcp-server")))
    (let ((config (agent-shell-godot-make-agent-config)))
      (should (equal (map-elt config :identifier) 'codebuddy))
      (should (equal (map-elt config :mcp-servers)
                     '(((name . "godot")
                        (command . "npx")
                        (args . ("-y" "godot-mcp-server"))
                        (env . ()))))))))

(ert-deftest agent-shell-godot-make-agent-config-no-mcp-test ()
  "The Godot config omits `:mcp-servers' when the command is nil."
  (let ((agent-shell-godot-mcp-command nil))
    (let ((config (agent-shell-godot-make-agent-config)))
      (should (equal (map-elt config :identifier) 'codebuddy))
      (should (equal (map-elt config :mcp-servers) nil)))))

(ert-deftest agent-shell-godot-make-agent-config-welcome-function-test ()
  "The Godot config overrides the welcome function with its own."
  (let ((config (agent-shell-godot-make-agent-config)))
    (should (equal (map-elt config :welcome-function)
                   #'agent-shell-godot--welcome-message))))

(ert-deftest agent-shell-godot-plugin-install-instructions-test ()
  "The plugin install instructions mention the addon path and enablement."
  (let ((text (agent-shell-godot--plugin-install-instructions)))
    (should (string-match-p "addons/godot_mcp" text))
    (should (string-match-p "editor_plugins" text))
    (should (string-match-p "godot_mcp" text))
    (should (string-match-p "Project Settings" text))
    (should (string-match-p "Single-instance" text))
    (should (string-match-p "toolbar" text))))

(provide 'agent-shell-godot-tests)
;;; agent-shell-godot-tests.el ends here
