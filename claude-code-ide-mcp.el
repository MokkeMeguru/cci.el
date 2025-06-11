;;; claude-code-ide-mcp.el --- Claude Code MCP/WebSocket IDE functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Meguru Mokke <meguru.mokke@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (claude-code-ide-utils "0.1.0") (json "1.4"))
;; Keywords: ai, claude, ide, tools
;; URL: https://github.com/mixi-sb/claude-code-ide.el

;;; Commentary:

;; ⚠️  WARNING: MCP functionality is temporarily disabled due to WebSocket connection issues.
;;
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
(defvar claude-code-ide-term-name)

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

;;;; Variables

;; WebSocket server variables
(defvar claude-code-ide--ws-server-process nil
  "TCP server process for WebSocket connections.")

(defvar claude-code-ide--ws-connections '()
  "List of active WebSocket connections.")

;; MCP server variables
(defvar claude-code-ide-mcp--server-process nil
  "WebSocket MCP server process.")

(defvar claude-code-ide-mcp--clients '()
  "List of connected MCP clients.")

(defvar claude-code-ide-mcp--port nil
  "Current MCP server port.")

(defvar claude-code-ide-mcp--lockfile-dir
  (expand-file-name "~/.claude/ide/")
  "Directory for Claude IDE lock files.")

(defvar claude-code-ide-mcp--lockfile-path nil
  "Path to current MCP server lockfile.")

;;;; MCP Configuration

(defcustom claude-code-ide-mcp-enabled nil
  "Enable MCP IDE integration mode.
⚠️  WARNING: MCP functionality is temporarily disabled due to
WebSocket connection issues.
When enabled, uses WebSocket server for true IDE integration.
When disabled, uses terminal mode for basic functionality."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom claude-code-ide-mcp-port-range '(55000 . 56000)
  "Port range for MCP WebSocket server."
  :type '(cons integer integer)
  :group 'claude-code-ide)

(defcustom claude-code-ide-mcp-auto-start nil
  "Automatically start MCP server when enabling IDE mode.
⚠️  WARNING: MCP functionality is temporarily disabled due to
WebSocket connection issues."
  :type 'boolean
  :group 'claude-code-ide)

;;;; WebSocket Utility Functions

(defun claude-code-ide--ws-sha1-hash (string)
  "Calculate SHA1 hash of STRING and return as base64."
  (base64-encode-string
   (secure-hash 'sha1 string nil nil t)))

(defun claude-code-ide--ws-parse-http-headers (request)
  "Parse HTTP headers from REQUEST string."
  (let ((lines (split-string request "[\r\n]+"))  ; Handle different line endings
        (headers '()))
    (claude-code-ide-mcp--log-message "DEBUG" (format "Parsing %d lines" (length lines)))
    (dolist (line (cdr lines))
      (when (and (> (length line) 0)  ; Skip empty lines
                 (string-match "^\\([^:]+\\):\\s-*\\(.+\\)" line))
        (let ((key (downcase (match-string 1 line)))
              (value (match-string 2 line)))
          (claude-code-ide-mcp--log-message "DEBUG" (format "Header: %s = %s" key value))
          (push (cons key value) headers))))
    headers))

(defun claude-code-ide--ws-create-handshake-response (key &optional extensions)
  "Create WebSocket handshake response for KEY, accepting compression EXTENSIONS."
  (let ((accept-key (claude-code-ide--ws-sha1-hash
                     (concat key claude-code-ide--ws-magic-string))))
    ;; Always include MCP protocol and handle extensions
    (if (and extensions (string-match "permessage-deflate" extensions))
        (format "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: %s\r\nSec-WebSocket-Extensions: permessage-deflate\r\nSec-WebSocket-Protocol: mcp\r\n\r\n"
                accept-key)
      (format "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: %s\r\nSec-WebSocket-Protocol: mcp\r\n\r\n"
              accept-key))))

;;;; WebSocket Frame Processing

(defun claude-code-ide--ws-parse-frame (data)
  "Parse WebSocket frame from DATA string."
  (when (>= (length data) 2)
    (let* ((byte0 (aref data 0))
           (byte1 (aref data 1))
           (fin (= (logand byte0 128) 128))
           (opcode (logand byte0 15))
           (masked (= (logand byte1 128) 128))
           (payload-len (logand byte1 127))
           (offset 2))
      
      ;; Handle extended payload length
      (cond
       ((= payload-len 126)
        (when (>= (length data) 4)
          (setq payload-len (+ (lsh (aref data 2) 8) (aref data 3))
                offset 4)))
       ((= payload-len 127)
        (when (>= (length data) 10)
          ;; For simplicity, only handle payload lengths that fit in 32 bits
          (setq payload-len (+ (lsh (aref data 6) 24)
                               (lsh (aref data 7) 16)
                               (lsh (aref data 8) 8)
                               (aref data 9))
                offset 10))))
      
      ;; Handle masking key
      (when masked
        (setq offset (+ offset 4)))
      
      ;; Extract payload if we have enough data
      (when (>= (length data) (+ offset payload-len))
        (let ((payload (substring data offset (+ offset payload-len))))
          ;; Unmask payload if needed
          (when masked
            (let ((mask (substring data (- offset 4) offset)))
              (dotimes (i (length payload))
                (aset payload i (logxor (aref payload i) 
                                        (aref mask (mod i 4)))))))

          `((fin . ,fin)
            (opcode . ,opcode)
            (payload . ,payload)
            (length . ,(+ offset payload-len))))))))

(defun claude-code-ide--ws-create-frame (opcode payload)
  "Create WebSocket frame with OPCODE and PAYLOAD."
  (let* ((payload-bytes (encode-coding-string payload 'utf-8))
         (payload-len (length payload-bytes))
         (frame-len (cond
                     ((< payload-len 126) (+ 2 payload-len))
                     ((< payload-len 65536) (+ 4 payload-len))
                     (t (+ 10 payload-len))))
         (frame (make-string frame-len 0))
         (offset 0))
    
    ;; First byte: FIN=1, RSV=000, OPCODE
    (aset frame offset (logior 128 opcode))
    (setq offset (1+ offset))
    
    ;; Payload length
    (cond
     ((< payload-len 126)
      (aset frame offset payload-len)
      (setq offset (1+ offset)))
     ((< payload-len 65536)
      (aset frame offset 126)
      (aset frame (1+ offset) (lsh payload-len -8))
      (aset frame (+ offset 2) (logand payload-len 255))
      (setq offset (+ offset 3)))
     (t
      (aset frame offset 127)
      ;; For simplicity, only support 32-bit lengths
      (aset frame (+ offset 5) 0)
      (aset frame (+ offset 6) (logand (lsh payload-len -24) 255))
      (aset frame (+ offset 7) (logand (lsh payload-len -16) 255))
      (aset frame (+ offset 8) (logand (lsh payload-len -8) 255))
      (aset frame (+ offset 9) (logand payload-len 255))
      (setq offset (+ offset 10))))
    
    ;; Copy payload
    (dotimes (i payload-len)
      (aset frame (+ offset i) (aref payload-bytes i)))
    
    frame))

;;;; JSON-RPC 2.0 Implementation

(defvar claude-code-ide--jsonrpc-request-id 0
  "Counter for JSON-RPC request IDs.")

(defun claude-code-ide--jsonrpc-parse-message (message)
  "Parse JSON-RPC MESSAGE string into Lisp structure."
  (condition-case err
      (json-read-from-string message)
    (error
     (claude-code-ide-mcp--log-message "ERROR" 
                                       (format "Failed to parse JSON: %s" (error-message-string err)))
     nil)))

(defun claude-code-ide--jsonrpc-create-response (id result)
  "Create JSON-RPC response with ID and RESULT."
  (json-encode `((jsonrpc . "2.0")
                 (result . ,result)
                 (id . ,id))))

(defun claude-code-ide--jsonrpc-create-error (id code message &optional data)
  "Create JSON-RPC error response with ID, CODE, MESSAGE and optional DATA."
  (json-encode `((jsonrpc . "2.0")
                 (error . ((code . ,code)
                           (message . ,message)
                           ,@(when data `((data . ,data)))))
                 (id . ,id))))

(defun claude-code-ide--jsonrpc-create-notification (method params)
  "Create JSON-RPC notification with METHOD and PARAMS."
  (json-encode `((jsonrpc . "2.0")
                 (method . ,method)
                 (params . ,params))))

;;;; MCP Protocol Variables

(defvar claude-code-ide--mcp-tools-registered nil
  "Flag to track if MCP tools have been registered.")

(defvar claude-code-ide--mcp-tools
  '(("initialize" . claude-code-ide--mcp-initialize)
    ("tools/list" . claude-code-ide--mcp-tools-list)
    ("file/open" . claude-code-ide--mcp-file-open)
    ("file/read" . claude-code-ide--mcp-file-read)
    ("file/save" . claude-code-ide--mcp-file-save)
    ("file/close" . claude-code-ide--mcp-file-close)
    ("editor/selection" . claude-code-ide--mcp-editor-selection)
    ("editor/position" . claude-code-ide--mcp-editor-position)
    ("editor/buffers" . claude-code-ide--mcp-editor-buffers)
    ("editor/context" . claude-code-ide--mcp-editor-context)
    ("workspace/folders" . claude-code-ide--mcp-workspace-folders)
    ("workspace/files" . claude-code-ide--mcp-workspace-files))
  "Mapping of MCP method names to handler functions.")

;;;; MCP Logging

(defun claude-code-ide-mcp--log-message (level message)
  "Log MESSAGE with LEVEL to the MCP debug buffer."
  (when (get-buffer "*claude-mcp-log*")
    (with-current-buffer "*claude-mcp-log*"
      (goto-char (point-max))
      (insert (format "[%s] %s: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      level message))
      (goto-char (point-max)))))

;;;; MCP Server Management

(defun claude-code-ide-mcp--find-available-port ()
  "Find an available port in the configured range."
  (let ((start-port (car claude-code-ide-mcp-port-range))
        (end-port (cdr claude-code-ide-mcp-port-range))
        (port nil))
    (catch 'found
      (dotimes (i (1+ (- end-port start-port)))
        (let ((test-port (+ start-port i)))
          (condition-case nil
              (progn
                (delete-process
                 (make-network-process
                  :name "claude-port-test"
                  :buffer nil
                  :host 'local
                  :service test-port
                  :server t))
                (setq port test-port)
                (throw 'found port))
            (error nil)))))
    (or port (error "No available port found in range %d-%d" start-port end-port))))

(defun claude-code-ide-mcp--create-lockfile (port)
  "Create lockfile for MCP server on PORT."
  (unless (file-exists-p claude-code-ide-mcp--lockfile-dir)
    (make-directory claude-code-ide-mcp--lockfile-dir t))
  (let ((lockfile (expand-file-name (format "%d.lock" port)
                                    claude-code-ide-mcp--lockfile-dir))
        (workspace-root (directory-file-name
                         (expand-file-name (or (locate-dominating-file default-directory ".git")
                                               (locate-dominating-file default-directory "package.json")
                                               default-directory)))))
    (with-temp-file lockfile
      (insert (json-encode `((pid . ,(emacs-pid))
                             (workspaceFolders . (,workspace-root))
                             (ideName . "Emacs")
                             (transport . "ws")))))
    (setq claude-code-ide-mcp--lockfile-path lockfile))

  (defun claude-code-ide-mcp--cleanup-lockfile ()
    "Clean up MCP server lockfile."
    (when claude-code-ide-mcp--lockfile-path
      (ignore-errors
        (delete-file claude-code-ide-mcp--lockfile-path))
      (setq claude-code-ide-mcp--lockfile-path nil))))

;;;###autoload
(defun claude-code-ide-mcp-start-server ()
  "Start MCP WebSocket server for IDE integration."
  (interactive)
  (message "⚠️  MCP server functionality is temporarily disabled due to WebSocket connection issues. Please use terminal mode instead.")
  (when claude-code-ide-mcp--server-process
    (claude-code-ide-mcp-stop-server)))

;;;###autoload
(defun claude-code-ide-mcp-stop-server ()
  "Stop MCP WebSocket server."
  (interactive)
  (when claude-code-ide-mcp--server-process
    ;; Stop the server (this also closes all connections)
    (delete-process claude-code-ide-mcp--server-process)
    (setq claude-code-ide-mcp--server-process nil)
    (setq claude-code-ide-mcp--port nil)
    (setq claude-code-ide-mcp--clients '())
    (claude-code-ide-mcp--cleanup-lockfile)
    (message "MCP server stopped")))

;;;###autoload
(defun claude-code-ide-mcp-status ()
  "Show MCP server status and connection information."
  (interactive)
  (if claude-code-ide-mcp--server-process
      (let ((client-count (length claude-code-ide-mcp--clients)))
        (message "MCP server running on port %d, %d client(s) connected (⚠️  temporarily disabled)"
                 claude-code-ide-mcp--port client-count))
    (message "MCP server not running (⚠️  temporarily disabled due to WebSocket issues)")))

;;;###autoload
(defun claude-code-ide-mcp-enable-logging ()
  "Enable MCP message logging for debugging."
  (interactive)
  (with-current-buffer (get-buffer-create "*claude-mcp-log*")
    (erase-buffer)
    (insert "=== MCP Debug Log Started ===\n")
    (goto-char (point-max)))
  (message "MCP logging enabled - check *claude-mcp-log* buffer"))

;;;###autoload
(defun claude-code-ide-mcp-toggle-mode ()
  "Toggle between MCP IDE mode and terminal mode."
  (interactive)
  (if claude-code-ide-mcp-enabled
      (progn
        (setq claude-code-ide-mcp-enabled nil)
        (claude-code-ide-mcp-stop-server)
        (message "Switched to terminal mode"))
    (message "⚠️  MCP IDE mode is temporarily disabled due to WebSocket connection issues. Terminal mode is available.")))

;;;; MCP Protocol Handlers

(defun claude-code-ide--mcp-register-tools ()
  "Register all MCP tools and log registration."
  (unless claude-code-ide--mcp-tools-registered
    (dolist (tool claude-code-ide--mcp-tools)
      (claude-code-ide-mcp--log-message "TOOL"
                                        (format "Registered MCP tool: %s" (car tool))))
    (setq claude-code-ide--mcp-tools-registered t)
    (claude-code-ide-mcp--log-workspace)))

(defun claude-code-ide--mcp-initialize (params)
  "Handle MCP initialize request with PARAMS."
  (claude-code-ide-mcp--log-message "MCP" "Initialize request received")
  `((protocolVersion . "2024-11-05")
    (capabilities . ((tools . t)
                     (resources . t)
                     (prompts . t)))
    (serverInfo . ((name . "Emacs Claude Code IDE")
                   (version . "0.1.0")))))

(defun claude-code-ide--mcp-tools-list (params)
  "Handle MCP tools/list request with PARAMS."
  (claude-code-ide-mcp--log-message "MCP" "Tools list request received")
  `((tools . (((name . "file/open")
               (description . "Open a file at specified line/column"))
              ((name . "file/read")
               (description . "Read file contents with optional range"))
              ((name . "file/save")
               (description . "Save file changes to disk"))
              ((name . "file/close")
               (description . "Close file buffer"))
              ((name . "editor/selection")
               (description . "Get current text selection"))
              ((name . "editor/position")
               (description . "Get current cursor position"))
              ((name . "editor/buffers")
               (description . "List open buffers"))
              ((name . "editor/context")
               (description . "Get context around cursor"))
              ((name . "workspace/folders")
               (description . "Get workspace folder information"))
              ((name . "workspace/files")
               (description . "List files in workspace"))))))

;;;; WebSocket Server Handlers

(defun claude-code-ide--ws-server-filter (process string)
  "Handle incoming data STRING from WebSocket client PROCESS."
  (condition-case err
      (progn
        (claude-code-ide-mcp--log-message "RECEIVED"
                                          (format "Data from %s: %s" process (substring string 0 (min 200 (length string)))))

        ;; Check if this is a WebSocket handshake
        (if (string-match "GET .* HTTP/1.1" string)
            (claude-code-ide--ws-handle-handshake process string)
          ;; Handle WebSocket frames
          (progn
            (claude-code-ide-mcp--log-message "FRAME-RAW" (format "Raw frame data length: %d bytes" (length string)))
            (claude-code-ide--ws-handle-frames process string))))
    (error
     (claude-code-ide-mcp--log-message "ERROR"
                                       (format "Error in server filter: %s" (error-message-string err))))))

(defun claude-code-ide--ws-server-sentinel (process status)
  "Handle WebSocket server PROCESS status changes: STATUS."
  (claude-code-ide-mcp--log-message "CONNECTION"
                                    (format "Process %s: %s" process (string-trim status)))
  (when (string-match "open" status)
    (let ((client-name (format "client-%d" (random 10000))))
      (process-put process 'client-name client-name)
      (push process claude-code-ide-mcp--clients)
      (claude-code-ide-mcp--log-message "CLIENT" (format "New client connected: %s" client-name))))

  (when (or (string-match "closed" status) (string-match "finished" status))
    (let ((client-name (process-get process 'client-name)))
      (setq claude-code-ide-mcp--clients
            (delq process claude-code-ide-mcp--clients))
      (claude-code-ide-mcp--log-message "CLIENT" (format "Client disconnected: %s" client-name)))))

(defun claude-code-ide--ws-handle-handshake (process request)
  "Handle WebSocket handshake REQUEST from PROCESS."
  (claude-code-ide-mcp--log-message "HANDSHAKE" "Processing WebSocket handshake")

  (let* ((headers (claude-code-ide--ws-parse-http-headers request))
         (ws-key (alist-get "sec-websocket-key" headers nil nil 'equal))
         (ws-protocol (alist-get "sec-websocket-protocol" headers nil nil 'equal))
         (ws-extensions (alist-get "sec-websocket-extensions" headers nil nil 'equal)))

    (claude-code-ide-mcp--log-message "HANDSHAKE"
                                      (format "WebSocket-Key: %s, Protocol: %s, Extensions: %s"
                                              ws-key ws-protocol ws-extensions))

    (if ws-key
        (let ((response (claude-code-ide--ws-create-handshake-response ws-key ws-extensions)))
          (claude-code-ide-mcp--log-message "HANDSHAKE" "Sending handshake response")
          (process-send-string process response)
          (process-put process 'websocket-ready t)
          (claude-code-ide-mcp--log-message "HANDSHAKE" "WebSocket handshake completed"))
      (claude-code-ide-mcp--log-message "ERROR" "Invalid handshake request - no WebSocket key")
      (delete-process process))))

(defun claude-code-ide--ws-handle-frames (process data)
  "Handle WebSocket frames in DATA from PROCESS."
  (claude-code-ide-mcp--log-message "FRAME-DEBUG" (format "Frame handler called with %d bytes, websocket-ready: %s"
                                                          (length data) (process-get process 'websocket-ready)))
  (when (process-get process 'websocket-ready)
    (let ((remaining-data data)
          (client-name (or (process-get process 'client-name) "unknown")))

      (cl-block frame-loop
        (while (and remaining-data (> (length remaining-data) 0))
          (let ((frame (claude-code-ide--ws-parse-frame remaining-data)))
            (if frame
                (let ((opcode (alist-get 'opcode frame))
                      (payload (alist-get 'payload frame))
                      (frame-length (alist-get 'length frame)))

                  (cond
                   ;; Text frame - handle JSON-RPC message
                   ((= opcode claude-code-ide--ws-opcode-text)
                    (let ((message (decode-coding-string payload 'utf-8)))
                      (claude-code-ide-mcp--log-message "FRAME"
                                                        (format "Text frame from %s: %s" client-name message))
                      (claude-code-ide--handle-jsonrpc-message process message)))

                   ;; Ping frame - respond with pong
                   ((= opcode claude-code-ide--ws-opcode-ping)
                    (claude-code-ide-mcp--log-message "PING" (format "Ping from %s" client-name))
                    (let ((pong-frame (claude-code-ide--ws-create-frame claude-code-ide--ws-opcode-pong payload)))
                      (process-send-string process pong-frame)))

                   ;; Close frame - close connection
                   ((= opcode claude-code-ide--ws-opcode-close)
                    (claude-code-ide-mcp--log-message "CLOSE" (format "Close frame from %s" client-name))
                    (delete-process process)
                    (cl-return-from frame-loop)))

                  ;; Move to next frame
                  (setq remaining-data (substring remaining-data frame-length)))
              ;; No complete frame available
              (cl-return-from frame-loop))))))))

(defun claude-code-ide--handle-jsonrpc-message (process message)
  "Handle JSON-RPC MESSAGE from PROCESS."
  (let* ((parsed (claude-code-ide--jsonrpc-parse-message message))
         (method (alist-get 'method parsed))
         (params (alist-get 'params parsed))
         (id (alist-get 'id parsed))
         (client-name (or (process-get process 'client-name) "unknown")))

    (claude-code-ide-mcp--log-message "REQUEST" (format "Method: %s, ID: %s" method id))

    (if method
        (let ((handler (alist-get method claude-code-ide--mcp-tools)))
          (if handler
              (condition-case err
                  (let ((result (funcall handler params)))
                    (when id  ; Only send response if there's an ID (not a notification)
                      (let ((response (claude-code-ide--jsonrpc-create-response id result)))
                        (claude-code-ide-mcp--log-message "SEND"
                                                          (format "Sending to %s: %s" client-name response))
                        (claude-code-ide--ws-send-text process response))))
                (error
                 (when id
                   (let ((error-response (claude-code-ide--jsonrpc-create-error
                                          id -32603 (format "Internal error: %s" (error-message-string err)))))
                     (claude-code-ide--ws-send-text process error-response)))))
            ;; Method not found
            (when id
              (let ((error-response (claude-code-ide--jsonrpc-create-error
                                     id -32601 (format "Method not found: %s" method))))
                (claude-code-ide--ws-send-text process error-response)))))
      ;; Invalid request
      (when id
        (let ((error-response (claude-code-ide--jsonrpc-create-error
                               id -32600 "Invalid Request")))
          (claude-code-ide--ws-send-text process error-response))))))

(defun claude-code-ide--ws-send-text (process text)
  "Send TEXT as WebSocket text frame to PROCESS."
  (when (process-get process 'websocket-ready)
    (let ((frame (claude-code-ide--ws-create-frame claude-code-ide--ws-opcode-text text)))
      (process-send-string process frame))))

;;;; MCP Tool Implementations

;; File Operations
(defun claude-code-ide--mcp-file-open (params)
  "Handle file/open MCP request with PARAMS."
  (let* ((uri (alist-get 'uri params))
         (line (alist-get 'line params))
         (column (alist-get 'column params))
         (file-path (when uri (substring uri 7))))  ; Remove file:// prefix

    (claude-code-ide-mcp--log-message "FILE-OPEN" (format "Params: %s" params))

    (if (and file-path (file-exists-p file-path))
        (progn
          (find-file file-path)
          (when line
            (goto-char (point-min))
            (forward-line (1- line)))
          (when column
            (move-to-column column))

          `((success . t)
            (buffer . ,(buffer-name))
            (line . ,(line-number-at-pos))
            (column . ,(current-column))))
      `((success . nil)
        (error . "File not found")))))

(defun claude-code-ide--mcp-file-read (params)
  "Handle file/read MCP request with PARAMS."
  (let* ((uri (alist-get 'uri params))
         (start-line (alist-get 'start_line params))
         (end-line (alist-get 'end_line params))
         (file-path (when uri (substring uri 7))))

    (if (and file-path (file-exists-p file-path))
        (with-temp-buffer
          (insert-file-contents file-path)
          (let ((content (if (and start-line end-line)
                             (let ((lines (split-string (buffer-string) "\n")))
                               (mapconcat 'identity (cl-subseq lines (1- start-line) end-line) "\n"))
                           (buffer-string))))
            `((success . t)
              (content . ,content)
              (size . ,(length content)))))
      `((success . nil)
        (error . "File not found")))))

(defun claude-code-ide--mcp-file-save (params)
  "Handle file/save MCP request with PARAMS."
  (let* ((uri (alist-get 'uri params))
         (content (alist-get 'content params))
         (file-path (when uri (substring uri 7))))

    (if file-path
        (condition-case err
            (progn
              (with-temp-file file-path
                (when content
                  (insert content)))
              ;; Auto-revert any open buffer with this file
              (let ((buffer (get-file-buffer file-path)))
                (when buffer
                  (with-current-buffer buffer
                    (revert-buffer t t t))))
              `((success . t)
                (saved . t)))
          (error
           `((success . nil)
             (error . ,(error-message-string err)))))
      `((success . nil)
        (error . "No file path provided")))))

(defun claude-code-ide--mcp-file-close (params)
  "Handle file/close MCP request with PARAMS."
  (let* ((uri (alist-get 'uri params))
         (file-path (when uri (substring uri 7))))

    (if file-path
        (let ((buffer (get-file-buffer file-path)))
          (if buffer
              (progn
                (kill-buffer buffer)
                `((success . t)
                  (closed . t)))
            `((success . t)
              (closed . nil)
              (message . "Buffer not open"))))
      `((success . nil)
        (error . "No file path provided")))))

;; Editor State Operations
(defun claude-code-ide--mcp-editor-selection (params)
  "Handle editor/selection MCP request with PARAMS."
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties start end)))
        `((active . t)
          (start . ,start)
          (end . ,end)
          (text . ,text)
          (line_start . ,(line-number-at-pos start))
          (line_end . ,(line-number-at-pos end))))
    `((active . nil))))

(defun claude-code-ide--mcp-editor-position (params)
  "Handle editor/position MCP request with PARAMS."
  `((line . ,(line-number-at-pos))
    (column . ,(current-column))
    (position . ,(point))
    (buffer . ,(buffer-name))
    (uri . ,(when (buffer-file-name)
              (concat "file://" (buffer-file-name))))
    (total_lines . ,(count-lines (point-min) (point-max)))))

(defun claude-code-ide--mcp-editor-buffers (params)
  "Handle editor/buffers MCP request with PARAMS."
  (let ((include-special (alist-get 'include_special params))
        (buffers '())
        (current-buf (current-buffer)))

    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        (when (or include-special (not (string-match-p "^[ *]" name)))
          (with-current-buffer buffer
            (push `((name . ,name)
                    (uri . ,(when (buffer-file-name)
                              (concat "file://" (buffer-file-name))))
                    (modified . ,(buffer-modified-p))
                    (size . ,(buffer-size))
                    (major_mode . ,(symbol-name major-mode))
                    (active . ,(eq buffer current-buf)))
                  buffers)))))

    `((buffers . ,(reverse buffers))
      (total . ,(length buffers)))))

(defun claude-code-ide--mcp-editor-context (params)
  "Handle editor/context MCP request with PARAMS."
  (let* ((lines-before (or (alist-get 'lines_before params) 5))
         (lines-after (or (alist-get 'lines_after params) 5))
         (current-line (line-number-at-pos))
         (start-line (max 1 (- current-line lines-before)))
         (end-line (+ current-line lines-after))
         (total-lines (count-lines (point-min) (point-max))))

    (setq end-line (min end-line total-lines))

    `((current_line . ,current-line)
      (current_column . ,(current-column))
      (context_before . ,(claude-code-ide--get-lines start-line (1- current-line)))
      (context_after . ,(claude-code-ide--get-lines (1+ current-line) end-line))
      (current_line_text . ,(thing-at-point 'line t))
      (file_type . ,(symbol-name major-mode)))))

;; Workspace Operations
(defun claude-code-ide--mcp-workspace-folders (params)
  "Handle workspace/folders MCP request with PARAMS."
  (let ((project-root (claude-code-ide--find-project-root)))
    `((folders . (((uri . ,(concat "file://" project-root))
                   (name . ,(file-name-nondirectory (directory-file-name project-root)))))))))

(defun claude-code-ide--mcp-workspace-files (params)
  "Handle workspace/files MCP request with PARAMS."
  (let* ((project-root (claude-code-ide--find-project-root))
         (pattern (alist-get 'pattern params))
         (max-files (or (alist-get 'max_files params) 100))
         (files '())
         (count 0))

    (when (< count max-files)
      (dolist (file (directory-files-recursively project-root ".*" nil))
        (when (< count max-files)
          (unless (string-match-p "/\\." file)  ; Skip hidden directories
            (push `((uri . ,(concat "file://" file))
                    (name . ,(file-name-nondirectory file))
                    (size . ,(file-attribute-size (file-attributes file))))
                  files)
            (setq count (1+ count))))))

    `((files . ,(reverse files))
      (total . ,count))))

;; Utility function for context extraction
(defun claude-code-ide--get-lines (start-line end-line)
  "Get text content from START-LINE to END-LINE."
  (when (and (> start-line 0) (> end-line 0) (<= start-line end-line))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((start-pos (point)))
        (forward-line (- end-line start-line 1))
        (buffer-substring-no-properties start-pos (point))))))

;;;; Workspace Utilities

(defun claude-code-ide--find-project-root ()
  "Find project root directory using common markers."
  (or (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "package.json")
      (locate-dominating-file default-directory "Cargo.toml")
      (locate-dominating-file default-directory "pom.xml")
      (locate-dominating-file default-directory "Makefile")
      default-directory))

(defun claude-code-ide-mcp--log-workspace ()
  "Log current workspace information."
  (let ((workspace-root (claude-code-ide--find-project-root))
        (current-dir default-directory)
        (git-dir (locate-dominating-file default-directory ".git")))

    (claude-code-ide-mcp--log-message "WORKSPACE" "=== Current Workspace Information ===")
    (claude-code-ide-mcp--log-message "WORKSPACE" (format "Project Root: %s" workspace-root))
    (claude-code-ide-mcp--log-message "WORKSPACE" (format "Current Directory: %s" current-dir))
    (when git-dir
      (claude-code-ide-mcp--log-message "WORKSPACE" (format "Git repository: %s.git" git-dir)))
    (claude-code-ide-mcp--log-message "WORKSPACE" (format "Active buffers: %d" (length (buffer-list))))
    (claude-code-ide-mcp--log-message "WORKSPACE" "===================================")))

;; Ensure cleanup on exit
;; (add-hook 'kill-emacs-hook #'claude-code-ide-mcp--cleanup-lockfile)

(provide 'claude-code-ide-mcp)
;;; claude-code-ide-mcp.el ends here
