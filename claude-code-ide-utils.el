;;; claude-code-ide-utils.el --- Claude Code IDE utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Meguru Mokke <meguru.mokke@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (transient "0.4.0"))
;; Keywords: ai, claude, ide, tools
;; URL: https://github.com/mixi-sb/claude-code-ide.el

;;; Commentary:

;; Shared utilities for Claude Code IDE integration including:
;; - Configuration and customization
;; - Transient menus and key bindings
;; - Minor mode definitions
;; - Common utility functions

;;; Code:

(require 'transient)
(require 'project)

;; Forward declare functions to avoid compiler warnings
(declare-function claude-code-ide-start "claude-code-ide-session")
(declare-function claude-code-ide-current-directory "claude-code-ide-session")
(declare-function claude-code-ide-toggle "claude-code-ide-session")
(declare-function claude-code-ide-switch-to-buffer "claude-code-ide-session")
(declare-function claude-code-ide-kill "claude-code-ide-session")
(declare-function claude-code-ide-status "claude-code-ide-session")
(declare-function claude-code-ide-send-command "claude-code-ide-session")
(declare-function claude-code-ide-send-region "claude-code-ide-session")
(declare-function claude-code-ide-send-file "claude-code-ide-session")
(declare-function claude-code-ide-mcp-toggle-mode "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-start-server "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-stop-server "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-status "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp-enable-logging "claude-code-ide-mcp")
(declare-function claude-code-ide-slash-help "claude-code-ide-session")
(declare-function claude-code-ide-slash-clear "claude-code-ide-session")

;;;; Customization

(defgroup claude-code-ide nil
  "Claude Code IDE integration for Emacs."
  :group 'tools
  :prefix "claude-code-ide-")

(defcustom claude-code-ide-program "claude"
  "Program to run when starting Claude Code."
  :type 'string
  :group 'claude-code-ide)

(defcustom claude-code-ide-program-switches nil
  "List of command line switches to pass to Claude Code program."
  :type '(repeat string)
  :group 'claude-code-ide)

(defcustom claude-code-ide-buffer-name "*claude-code*"
  "Name of the Claude Code buffer."
  :type 'string
  :group 'claude-code-ide)

(defcustom claude-code-ide-startup-delay 0.1
  "Delay in seconds after starting Claude before displaying buffer."
  :type 'number
  :group 'claude-code-ide)

(defcustom claude-code-ide-large-buffer-threshold 1000
  "Size threshold above which buffers are considered large."
  :type 'integer
  :group 'claude-code-ide)

;;;###autoload (autoload 'claude-code-ide-start "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-current-directory "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-toggle "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-switch-to-buffer "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-kill "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-status "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-send-command "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-send-region "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-send-file "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-mcp-toggle-mode "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-start-server "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-stop-server "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-status "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-enable-logging "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-slash-help "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-slash-clear "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-slash-model "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-slash-quit "claude-code-ide-session")

;;;; Transient Menus

;;;###autoload
(transient-define-prefix claude-code-ide-menu ()
  "Claude Code IDE main menu."
  [["Session"
    ("s" "Start Claude" claude-code-ide-start)
    ("d" "Start in current directory" claude-code-ide-current-directory)
    ("t" "Toggle window" claude-code-ide-toggle)
    ("b" "Switch to buffer" claude-code-ide-switch-to-buffer)
    ("k" "Kill Claude" claude-code-ide-kill)
    ("S" "Status" claude-code-ide-status)]
   ["Send Content"
    ("c" "Send command" claude-code-ide-send-command)
    ("r" "Send region/buffer" claude-code-ide-send-region)
    ("f" "Send file" claude-code-ide-send-file)]
   ["MCP IDE Mode"
    ("M" "Toggle MCP mode" claude-code-ide-mcp-toggle-mode)
    (">" "Start MCP server" claude-code-ide-mcp-start-server)
    ("<" "Stop MCP server" claude-code-ide-mcp-stop-server)
    ("?" "MCP status" claude-code-ide-mcp-status)
    ("L" "Enable MCP logging" claude-code-ide-mcp-enable-logging)]
   ["Slash Commands"
    ("/" "Help" claude-code-ide-slash-help)
    ("C" "Clear" claude-code-ide-slash-clear)
    ("m" "Model info" claude-code-ide-slash-model)
    ("q" "Quit" claude-code-ide-slash-quit)]])

;;;; Key Bindings

;;;###autoload (autoload 'claude-code-ide-command-map "claude-code-ide-utils")
(defvar claude-code-ide-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'claude-code-ide-start)
    (define-key map "d" #'claude-code-ide-current-directory)
    (define-key map "t" #'claude-code-ide-toggle)
    (define-key map "b" #'claude-code-ide-switch-to-buffer)
    (define-key map "k" #'claude-code-ide-kill)
    (define-key map "S" #'claude-code-ide-status)
    (define-key map "c" #'claude-code-ide-send-command)
    (define-key map "r" #'claude-code-ide-send-region)
    (define-key map "f" #'claude-code-ide-send-file)
    (define-key map "m" #'claude-code-ide-menu)
    (define-key map "M" #'claude-code-ide-mcp-toggle-mode)
    (define-key map ">" #'claude-code-ide-mcp-start-server)
    (define-key map "<" #'claude-code-ide-mcp-stop-server)
    (define-key map "?" #'claude-code-ide-mcp-status)
    (define-key map "L" #'claude-code-ide-mcp-enable-logging)
    (define-key map "/" #'claude-code-ide-slash-help)
    (define-key map "C" #'claude-code-ide-slash-clear)
    map)
  "Keymap for Claude Code IDE commands.")

;;;; Minor Mode

;;;###autoload
(define-minor-mode claude-code-ide-mode
  "Minor mode for Claude Code IDE integration."
  :lighter " Claude"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c c") claude-code-ide-command-map)
            map)
  :group 'claude-code-ide)

;;;###autoload
(define-globalized-minor-mode global-claude-code-ide-mode
  claude-code-ide-mode
  (lambda () (claude-code-ide-mode 1)))

(provide 'claude-code-ide-utils)
;;; claude-code-ide-utils.el ends here