# WebSocket Frame Implementation

## Overview

WebSocket frames carry JSON-RPC messages between Claude CLI and our Emacs MCP server. This document details the RFC 6455 frame parsing and generation implementation.

## WebSocket Frame Format (RFC 6455)

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-------+-+-------------+-------------------------------+
|F|R|R|R| opcode|M| Payload len |    Extended payload length    |
|I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
|N|V|V|V|       |S|             |   (if payload len==126/127)   |
| |1|2|3|       |K|             |                               |
+-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
|     Extended payload length continued, if payload len == 127  |
+ - - - - - - - - - - - - - - - +-------------------------------+
|                               |Masking-key, if MASK set to 1  |
+-------------------------------+-------------------------------+
| Masking-key (continued)       |          Payload Data         |
+-------------------------------- - - - - - - - - - - - - - - - +
:                     Payload Data continued ...                :
+ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
|                     Payload Data continued ...                |
+---------------------------------------------------------------+
```

## Frame Types Used

- **Text Frame (0x1)**: JSON-RPC messages
- **Close Frame (0x8)**: Connection termination
- **Ping Frame (0x9)**: Keep-alive from client
- **Pong Frame (0xA)**: Keep-alive response

## Implementation Components

### Frame Parsing

```elisp
(defun claude-code-ide--ws-parse-frame (data)
  "Parse WebSocket frame from DATA bytes."
  (when (>= (length data) 2)
    (let* ((byte1 (aref data 0))
           (byte2 (aref data 1))
           (fin (not (zerop (logand byte1 #x80))))
           (opcode (logand byte1 #x0F))
           (masked (not (zerop (logand byte2 #x80))))
           (payload-len (logand byte2 #x7F))
           (offset 2))
      
      ;; Handle extended payload length
      (cond
       ((= payload-len 126)
        (when (>= (length data) (+ offset 2))
          (setq payload-len (+ (* (aref data offset) 256)
                               (aref data (1+ offset))))
          (cl-incf offset 2)))
       ((= payload-len 127)
        (when (>= (length data) (+ offset 8))
          ;; For simplicity, only handle payload lengths up to 32-bit
          (setq payload-len (+ (* (aref data (+ offset 4)) 16777216)
                               (* (aref data (+ offset 5)) 65536)
                               (* (aref data (+ offset 6)) 256)
                               (aref data (+ offset 7))))
          (cl-incf offset 8))))
      
      ;; Handle masking key
      (when masked
        (when (>= (length data) (+ offset 4))
          (cl-incf offset 4))) ; Skip masking key for now
      
      ;; Extract payload if complete
      (when (>= (length data) (+ offset payload-len))
        (let ((payload (substring data offset (+ offset payload-len))))
          ;; TODO: Apply unmasking if needed
          (list :fin fin
                :opcode opcode
                :payload payload
                :total-length (+ offset payload-len)))))))
```

### Frame Generation

```elisp
(defun claude-code-ide--ws-create-frame (opcode payload)
  "Create WebSocket frame with OPCODE and PAYLOAD."
  (let* ((payload-bytes (if (stringp payload)
                            (encode-coding-string payload 'utf-8)
                          payload))
         (payload-len (length payload-bytes))
         (header '()))
    
    ;; First byte: FIN=1, RSV=000, OPCODE
    (push (logior #x80 opcode) header)
    
    ;; Second byte and payload length
    (cond
     ((< payload-len 126)
      (push payload-len header))
     ((< payload-len 65536)
      (push 126 header)
      (push (/ payload-len 256) header)
      (push (% payload-len 256) header))
     (t
      (push 127 header)
      ;; For simplicity, only support up to 32-bit lengths
      (push 0 header) (push 0 header) (push 0 header) (push 0 header)
      (push (/ payload-len 16777216) header)
      (push (/ (% payload-len 16777216) 65536) header)
      (push (/ (% payload-len 65536) 256) header)
      (push (% payload-len 256) header)))
    
    ;; Combine header and payload
    (concat (apply #'unibyte-string (reverse header)) payload-bytes)))
```

### Connection State Management

```elisp
(defun claude-code-ide--ws-handle-connection (process)
  "Handle new WebSocket connection PROCESS."
  (let ((client (make-claude-code-ide--mcp-client
                 :process process
                 :id (format "client-%d" (random 10000))
                 :state :connecting
                 :buffer ""
                 :handshake-done nil
                 :pending-requests (make-hash-table :test 'equal))))
    
    ;; Add to client list
    (push client claude-code-ide-mcp--clients)
    
    ;; Set up process handlers
    (set-process-filter process
      (lambda (proc data)
        (claude-code-ide--ws-process-data client data)))
    
    (set-process-sentinel process
      (lambda (proc status)
        (claude-code-ide--ws-handle-disconnect client status)))
    
    (claude-code-ide-mcp--log-message "CONNECT" 
      (format "New client connection: %s" (claude-code-ide--mcp-client-id client)))
    
    client))
```

### Data Processing Pipeline

```elisp
(defun claude-code-ide--ws-process-data (client data)
  "Process incoming DATA for CLIENT."
  (condition-case err
      (with-slots (buffer handshake-done) client
        (setf buffer (concat buffer data))
        
        (if handshake-done
            ;; Process WebSocket frames
            (claude-code-ide--ws-process-frames client)
          ;; Handle WebSocket handshake
          (claude-code-ide--ws-handle-handshake client)))
    (error
     (claude-code-ide-mcp--log-message "ERROR" 
       (format "Data processing error: %s" (error-message-string err))))))

(defun claude-code-ide--ws-process-frames (client)
  "Process WebSocket frames in CLIENT buffer."
  (with-slots (buffer) client
    (let ((processed 0))
      (while (> (length buffer) processed)
        (let ((frame (claude-code-ide--ws-parse-frame 
                      (substring buffer processed))))
          (if frame
              (let ((frame-len (plist-get frame :total-length))
                    (opcode (plist-get frame :opcode))
                    (payload (plist-get frame :payload)))
                
                ;; Process frame based on opcode
                (cond
                 ((= opcode claude-code-ide--ws-opcode-text)
                  (claude-code-ide--ws-handle-text-frame client payload))
                 ((= opcode claude-code-ide--ws-opcode-close)
                  (claude-code-ide--ws-handle-close-frame client))
                 ((= opcode claude-code-ide--ws-opcode-ping)
                  (claude-code-ide--ws-handle-ping-frame client payload)))
                
                (cl-incf processed frame-len))
            ;; Incomplete frame, wait for more data
            (cl-return))))
      
      ;; Remove processed data from buffer
      (setf buffer (substring buffer processed)))))
```

### Frame Handler Functions

```elisp
(defun claude-code-ide--ws-handle-text-frame (client payload)
  "Handle WebSocket text frame containing JSON-RPC message."
  (let ((json-string (decode-coding-string payload 'utf-8)))
    (claude-code-ide-mcp--log-message "FRAME" 
      (format "Text frame from %s: %s" 
              (claude-code-ide--mcp-client-id client) json-string))
    
    ;; Parse and dispatch JSON-RPC message
    (let ((message (claude-code-ide--jsonrpc-parse-message json-string)))
      (when message
        (claude-code-ide--jsonrpc-dispatch client message)))))

(defun claude-code-ide--ws-handle-ping-frame (client payload)
  "Handle WebSocket ping frame by sending pong response."
  (claude-code-ide--ws-send-frame client claude-code-ide--ws-opcode-pong payload))

(defun claude-code-ide--ws-handle-close-frame (client)
  "Handle WebSocket close frame."
  (claude-code-ide-mcp--log-message "CLOSE" 
    (format "Client %s closing connection" (claude-code-ide--mcp-client-id client)))
  (claude-code-ide--ws-send-frame client claude-code-ide--ws-opcode-close ""))
```

### Frame Transmission

```elisp
(defun claude-code-ide--ws-send-frame (client opcode payload)
  "Send WebSocket frame to CLIENT with OPCODE and PAYLOAD."
  (let ((frame (claude-code-ide--ws-create-frame opcode payload)))
    (condition-case err
        (process-send-string (claude-code-ide--mcp-client-process client) frame)
      (error
       (claude-code-ide-mcp--log-message "ERROR" 
         (format "Failed to send frame: %s" (error-message-string err)))))))

(defun claude-code-ide--ws-send-text-frame (client text)
  "Send text frame to CLIENT with TEXT payload."
  (claude-code-ide--ws-send-frame client claude-code-ide--ws-opcode-text text))
```

## Integration Points

### Updated Message Sending
The JSON-RPC `claude-code-ide--send-message` function becomes:

```elisp
(defun claude-code-ide--send-message (client jsonrpc-message)
  "Send JSON-RPC MESSAGE to CLIENT via WebSocket."
  (let ((json-string (claude-code-ide--jsonrpc-serialize-message jsonrpc-message)))
    (claude-code-ide-mcp--log-message "SEND" 
      (format "Sending to %s: %s" 
              (claude-code-ide--mcp-client-id client) json-string))
    (claude-code-ide--ws-send-text-frame client json-string)))
```

### Server Process Update
The server creation needs updated connection handling:

```elisp
;; Replace the simple filter with proper WebSocket handling
:filter (lambda (proc string)
          (let ((client (claude-code-ide--find-client-by-process proc)))
            (if client
                (claude-code-ide--ws-process-data client string)
              ;; New connection - handle handshake
              (claude-code-ide--ws-handle-new-connection proc string))))
```

## Error Handling

- **Malformed Frames**: Log error, close connection gracefully
- **Partial Frames**: Buffer data until complete frame received  
- **Connection Loss**: Clean up client state and pending requests
- **Protocol Violations**: Send close frame with appropriate status code

## Performance Considerations

- **Frame Buffering**: Accumulate partial frames efficiently
- **Memory Management**: Clean up completed frames and disconnected clients
- **Async Processing**: Don't block Emacs during frame processing
- **Connection Limits**: Reasonable maximum concurrent connections

The next document will detail the MCP tool implementations that use this WebSocket/JSON-RPC foundation.