;;; agent-shell-godot.el --- Godot-flavored CodeBuddy agent configuration -*- lexical-binding: t; -*-

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
;; A CodeBuddy agent configuration that wires in the Godot MCP server
;; (https://github.com/tomyud1/godot-mcp), so a CodeBuddy agent can
;; drive the Godot editor.  Only CodeBuddy is supported; no other
;; provider is integrated here.
;;
;; The Godot MCP server is injected via the agent config's
;; `:mcp-servers', so it is scoped to this agent alone and does not
;; touch the global `agent-shell-mcp-servers'.  See
;; `agent-shell-godot-mcp-command' to override the launcher.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'agent-shell-codebuddy)

(declare-function agent-shell-codebuddy-make-agent-config "agent-shell-codebuddy")
(declare-function agent-shell-codebuddy--welcome-message "agent-shell-codebuddy")
(declare-function agent-shell--dwim "agent-shell")

(defcustom agent-shell-godot-mcp-command
  '("npx" "-y" "godot-mcp-server")
  "Command and parameters to launch the Godot MCP server.

The first element is the command name, and the rest are command
parameters.  Set to nil to disable MCP injection and start a plain
CodeBuddy session.

The default launches `tomyud1/godot-mcp' via npx.  The Godot editor
must be running with the Godot MCP plugin enabled (see the project
README) for the MCP tools to connect."
  :type '(choice (const :tag "Disable" nil)
                 (repeat string))
  :group 'agent-shell)

(defun agent-shell-godot-mcp-server ()
  "Return the Godot MCP server configuration, or nil when disabled.

Builds a one-element list holding an ACP-style MCP server alist,
matching the `:mcp-servers' shape (a list of servers)."
  (when agent-shell-godot-mcp-command
    `(((name . "godot")
       (command . ,(car agent-shell-godot-mcp-command))
       (args . ,(cdr agent-shell-godot-mcp-command))
       (env . ())))))

(defun agent-shell-godot--welcome-message (config)
  "Return the Godot agent welcome message using `shell-maker' CONFIG.

Builds on the CodeBuddy welcome message and appends instructions for
installing the Godot MCP plugin, so the agent can set it up itself."
  (concat (agent-shell-codebuddy--welcome-message config)
          "\n\n"
          (agent-shell-godot--plugin-install-instructions)))

(defun agent-shell-godot--plugin-install-instructions ()
  "Return instructions for setting up the Godot MCP plugin.

The plugin is a plain Godot addon shipped in the `tomyud1/godot-mcp'
repository under `addons/godot_mcp/'.  The agent installs the addon
files and project.godot enablement; the user must toggle the plugin in
the editor GUI and restart, since those steps cannot be scripted."
  (string-trim "
Godot MCP plugin setup (run when asked to drive a Godot project):

1. Install the addon files into the Godot project:
   - The plugin lives at https://github.com/tomyud1/godot-mcp under addons/godot_mcp/
   - Clone shallow: git clone --depth 1 https://github.com/tomyud1/godot-mcp.git
   - Copy addons/godot_mcp/ into <project>/addons/godot_mcp/ (create addons/ if absent)

2. Enable the plugin by appending to <project>/project.godot:
   [editor_plugins]
   enabled=PackedStringArray(\"godot_mcp\")

3. Ask the user to do these GUI-only steps in the Godot editor:
   - Project > Project Settings > Plugins > tick \"Godot MCP\"
   - Restart the editor

4. After restart, the top toolbar shows an \"MCP:\" status label (NOT the
   top-right corner). It turns green \"MCP: Agent Active\" once an agent
   connects, or orange \"MCP: No Agent\" when the server is up but no agent
   is attached.

Notes:
- The plugin needs Node.js for the MCP server side (npx -y godot-mcp-server).
- Single-instance limit: one MCP server talks to one Godot editor at a
  time. Don't run two Godot editors for different projects concurrently,
  or they'll contend for the same WebSocket port (127.0.0.1:6505)."))

(defun agent-shell-godot-make-agent-config ()
  "Create a Godot-flavored CodeBuddy agent configuration.

Returns the CodeBuddy config with the Godot MCP server wired in via
`:mcp-servers' and a Godot-specific welcome message."
  (agent-shell-codebuddy-make-agent-config
   :mcp-servers (agent-shell-godot-mcp-server)
   :welcome-function #'agent-shell-godot--welcome-message))

;;;###autoload
(defun agent-shell-godot-start-agent ()
  "Start a CodeBuddy agent shell wired to the Godot MCP server."
  (interactive)
  (require 'agent-shell)
  (agent-shell--dwim :config (agent-shell-godot-make-agent-config)
                     :new-shell t))

(provide 'agent-shell-godot)

;;; agent-shell-godot.el ends here
