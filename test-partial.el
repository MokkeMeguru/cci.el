;;; claude-code-ide-mcp.el --- Claude Code MCP/WebSocket IDE functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Meguru Mokke <meguru.mokke@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (claude-code-ide-utils "0.1.0") (json "1.4"))
;; Keywords: ai, claude, ide, tools
;; URL: https://github.com/mixi-sb/claude-code-ide.el

;;; Commentary:

;; MCP (Model Context Protocol) and WebSocket server functionality for
;; advanced Claude Code IDE integration including:
;; - RFC 6455 compliant WebSocket server implementation
;; - MCP protocol JSON-RPC 2.0 message processing
;; - File operations with line/column positioning
;; - Real-time selection tracking and context sharing
;; - Workspace awareness and project detection

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;; Forward declarations for customizations
(defvar claude-code-ide-buffer-name)
(defvar claude-code-ide-large-buffer-threshold)
(defvar claude-code-ide-program)
(defvar claude-code-ide-program-switches)
(defvar claude-code-ide-startup-delay)

;;;; WebSocket Protocol Constants

(defconst claude-code-ide--ws-magic-string "258EAFA5-E1F6-98D4-13B2-2B0876B481C5"
  "WebSocket magic string for handshake.")

(defconst claude-code-ide--ws-opcode-text 1
  "WebSocket text frame opcode.")

(defconst claude-code-ide--ws-opcode-close 8
  "WebSocket close frame opcode.")

(defconst claude-code-ide--ws-opcode-ping 9
  "WebSocket ping frame opcode.")

(defconst claude-code-ide--ws-opcode-pong 10
  "WebSocket pong frame opcode.")
(provide 'test-partial)
