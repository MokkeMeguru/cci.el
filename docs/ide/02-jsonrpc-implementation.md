# JSON-RPC 2.0 Implementation for MCP

## Overview

This document details the JSON-RPC 2.0 implementation that forms the core of MCP (Model Context Protocol) communication between Claude CLI and our Emacs integration.

## JSON-RPC 2.0 Specification Summary

JSON-RPC 2.0 uses three message types:

### Request Format
```json
{
  "jsonrpc": "2.0",
  "method": "file/open",
  "params": {"uri": "file:///path/to/file.el", "line": 42},
  "id": 1
}
```

### Response Format (Success)
```json
{
  "jsonrpc": "2.0",
  "result": {"success": true},
  "id": 1
}
```

### Response Format (Error)
```json
{
  "jsonrpc": "2.0",
  "error": {"code": -32601, "message": "Method not found"},
  "id": 1
}
```

### Notification Format (No Response Expected)
```json
{
  "jsonrpc": "2.0",
  "method": "editor/selection",
  "params": {"start": 10, "end": 25, "text": "selected text"}
}
```

## Implementation Architecture

### Core Data Structures

```elisp
;; Client connection state
(cl-defstruct claude-code-ide--mcp-client
  process        ; WebSocket process
  id            ; Unique client ID
  state         ; Connection state (:connecting :connected :disconnected)
  pending-requests) ; Hash table of request ID -> callback

;; JSON-RPC message state
(cl-defstruct claude-code-ide--jsonrpc-message
  jsonrpc       ; Version string "2.0"
  method        ; Method name (for requests/notifications)
  params        ; Parameters object/array
  id            ; Request ID (nil for notifications)
  result        ; Result (for responses)
  error)        ; Error object (for error responses)
```

### Request/Response Correlation

```elisp
;; Global request ID counter
(defvar claude-code-ide--jsonrpc-next-id 1
  "Next JSON-RPC request ID to use.")

;; Pending requests per client
(defvar claude-code-ide--jsonrpc-pending-requests (make-hash-table :test 'equal)
  "Hash table of client-id -> (request-id -> callback).")

(defun claude-code-ide--jsonrpc-generate-id ()
  "Generate unique JSON-RPC request ID."
  (cl-incf claude-code-ide--jsonrpc-next-id))
```

### Message Processing Pipeline

```elisp
(defun claude-code-ide--jsonrpc-parse-message (json-string)
  "Parse JSON-RPC message from JSON-STRING."
  (condition-case err
      (let* ((parsed (json-parse-string json-string :object-type 'alist))
             (jsonrpc (alist-get 'jsonrpc parsed))
             (method (alist-get 'method parsed))
             (params (alist-get 'params parsed))
             (id (alist-get 'id parsed))
             (result (alist-get 'result parsed))
             (error (alist-get 'error parsed)))

        ;; Validate JSON-RPC version
        (unless (string= jsonrpc "2.0")
          (error "Invalid JSON-RPC version: %s" jsonrpc))

        ;; Create message structure
        (make-claude-code-ide--jsonrpc-message
         :jsonrpc jsonrpc
         :method method
         :params params
         :id id
         :result result
         :error error))
    (error
     (claude-code-ide-mcp--log-message "ERROR"
       (format "Failed to parse JSON-RPC: %s" (error-message-string err)))
     nil)))

(defun claude-code-ide--jsonrpc-serialize-message (message)
  "Serialize JSON-RPC MESSAGE to JSON string."
  (let ((data '()))
    ;; Always include version
    (push '(jsonrpc . "2.0") data)

    ;; Add method for requests/notifications
    (when (claude-code-ide--jsonrpc-message-method message)
      (push (cons 'method (claude-code-ide--jsonrpc-message-method message)) data))

    ;; Add params if present
    (when (claude-code-ide--jsonrpc-message-params message)
      (push (cons 'params (claude-code-ide--jsonrpc-message-params message)) data))

    ;; Add ID for requests/responses (not notifications)
    (when (claude-code-ide--jsonrpc-message-id message)
      (push (cons 'id (claude-code-ide--jsonrpc-message-id message)) data))

    ;; Add result for successful responses
    (when (claude-code-ide--jsonrpc-message-result message)
      (push (cons 'result (claude-code-ide--jsonrpc-message-result message)) data))

    ;; Add error for error responses
    (when (claude-code-ide--jsonrpc-message-error message)
      (push (cons 'error (claude-code-ide--jsonrpc-message-error message)) data))

    (json-encode data)))
```

## Message Dispatch System

```elisp
;; MCP tool registry
(defvar claude-code-ide--mcp-tools (make-hash-table :test 'equal)
  "Registry of MCP tool implementations.")

(defun claude-code-ide--register-mcp-tool (method handler)
  "Register MCP tool HANDLER for METHOD."
  (puthash method handler claude-code-ide--mcp-tools))

(defun claude-code-ide--jsonrpc-dispatch (client message)
  "Dispatch JSON-RPC MESSAGE from CLIENT to appropriate handler."
  (cond
   ;; Handle requests
   ((and (claude-code-ide--jsonrpc-message-method message)
         (claude-code-ide--jsonrpc-message-id message))
    (claude-code-ide--handle-request client message))

   ;; Handle responses
   ((claude-code-ide--jsonrpc-message-id message)
    (claude-code-ide--handle-response client message))

   ;; Handle notifications
   ((claude-code-ide--jsonrpc-message-method message)
    (claude-code-ide--handle-notification client message))

   (t
    (claude-code-ide-mcp--log-message "ERROR" "Invalid JSON-RPC message"))))

(defun claude-code-ide--handle-request (client message)
  "Handle JSON-RPC request MESSAGE from CLIENT."
  (let* ((method (claude-code-ide--jsonrpc-message-method message))
         (params (claude-code-ide--jsonrpc-message-params message))
         (id (claude-code-ide--jsonrpc-message-id message))
         (handler (gethash method claude-code-ide--mcp-tools)))

    (if handler
        (condition-case err
            ;; Call handler and send response
            (let ((result (funcall handler client params)))
              (claude-code-ide--jsonrpc-send-response client id result))
          (error
           ;; Send error response
           (claude-code-ide--jsonrpc-send-error client id
             `((code . -32603)
               (message . "Internal error")
               (data . ,(error-message-string err))))))
      ;; Method not found
      (claude-code-ide--jsonrpc-send-error client id
        `((code . -32601)
          (message . "Method not found")
          (data . ,method))))))
```

## Error Handling Standards

### JSON-RPC Error Codes
- `-32700`: Parse error
- `-32600`: Invalid Request
- `-32601`: Method not found
- `-32602`: Invalid params
- `-32603`: Internal error

### Implementation
```elisp
(defun claude-code-ide--jsonrpc-send-error (client id error)
  "Send JSON-RPC error response to CLIENT."
  (let ((message (make-claude-code-ide--jsonrpc-message
                  :jsonrpc "2.0"
                  :id id
                  :error error)))
    (claude-code-ide--send-message client message)))

(defun claude-code-ide--jsonrpc-send-response (client id result)
  "Send JSON-RPC success response to CLIENT."
  (let ((message (make-claude-code-ide--jsonrpc-message
                  :jsonrpc "2.0"
                  :id id
                  :result result)))
    (claude-code-ide--send-message client message)))
```

## Integration with WebSocket Layer

The JSON-RPC implementation connects to the WebSocket layer through these functions:

```elisp
;; Called when WebSocket receives a text frame
(defun claude-code-ide--ws-handle-text-frame (client payload)
  "Handle WebSocket text frame containing JSON-RPC message."
  (let ((message (claude-code-ide--jsonrpc-parse-message payload)))
    (when message
      (claude-code-ide--jsonrpc-dispatch client message))))

;; Send JSON-RPC message via WebSocket
(defun claude-code-ide--send-message (client jsonrpc-message)
  "Send JSON-RPC MESSAGE to CLIENT via WebSocket."
  (let ((json-string (claude-code-ide--jsonrpc-serialize-message jsonrpc-message)))
    (claude-code-ide--ws-send-text-frame client json-string)))
```

## Testing Strategy

### Unit Tests for JSON-RPC
```elisp
;; Test message parsing
(let ((json "{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}"))
  (claude-code-ide--jsonrpc-parse-message json))

;; Test message serialization
(let ((msg (make-claude-code-ide--jsonrpc-message
            :jsonrpc "2.0"
            :method "test"
            :id 1)))
  (claude-code-ide--jsonrpc-serialize-message msg))
```

## Performance Considerations

- **Async Processing**: JSON parsing/serialization should not block Emacs
- **Request Correlation**: Use hash tables for O(1) request lookup
- **Error Recovery**: Malformed JSON should not crash the server
- **Memory Management**: Clean up completed requests to prevent leaks

The next document will detail the WebSocket frame implementation that carries these JSON-RPC messages.