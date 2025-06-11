;;; claude-code-ide.el --- Claude Code IDE integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Meguru Mokke <meguru.mokke@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (transient))
;; Keywords: ai, claude, ide, tools
;; URL: https://github.com/mixi-sb/claude-code-ide.el

;;; Commentary:

;; A comprehensive Claude Code IDE integration for Emacs, providing both
;; terminal-based and advanced MCP (Model Context Protocol) IDE functionality.
;;
;; This package provides:
;; - Terminal mode: Direct Claude CLI interaction via vterm package
;; - MCP IDE mode: Advanced WebSocket-based IDE integration
;; - Interactive transient menus and comprehensive keybindings
;; - File operations, selection tracking, and workspace management

;;; Code:

;; Load all required modules
(require 'claude-code-ide-utils)
(require 'claude-code-ide-session)
(require 'claude-code-ide-mcp)

;; Re-export main interactive functions for backward compatibility
;;;###autoload (autoload 'claude-code-ide-start "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-current-directory "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-toggle "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-switch-to-buffer "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-kill "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-send-command "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-send-region "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-send-buffer "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-send-file "claude-code-ide-session")
;;;###autoload (autoload 'claude-code-ide-status "claude-code-ide-session")

;;;###autoload (autoload 'claude-code-ide-mcp-start-server "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-stop-server "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-status "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-enable-logging "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-log-workspace "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-debug-lockfile "claude-code-ide-mcp")
;;;###autoload (autoload 'claude-code-ide-mcp-toggle-mode "claude-code-ide-mcp")

;;;###autoload (autoload 'claude-code-ide-menu "claude-code-ide-utils")
;;;###autoload (autoload 'claude-code-ide-mode "claude-code-ide-utils")
;;;###autoload (autoload 'global-claude-code-ide-mode "claude-code-ide-utils")

(provide 'claude-code-ide)
;;; claude-code-ide.el ends here
