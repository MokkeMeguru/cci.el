# MCP File Operations Tools

## Overview

File operations are the core of Claude Code IDE integration, allowing Claude to directly interact with Emacs buffers and the filesystem. This document details the implementation of `file/*` MCP tools.

## MCP File Tool Specifications

### `file/open` - Open File with Positioning

**Purpose**: Open a file in Emacs and optionally position cursor at specific line/column

**Request Format**:
```json
{
  "jsonrpc": "2.0",
  "method": "file/open",
  "params": {
    "uri": "file:///absolute/path/to/file.el",
    "line": 42,
    "column": 10
  },
  "id": 1
}
```

**Response Format**:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "success": true,
    "buffer": "file.el",
    "line": 42,
    "column": 10
  },
  "id": 1
}
```

**Implementation Strategy**:
- Parse URI to extract file path
- Use `find-file` to open in Emacs
- Use `goto-line` and `move-to-column` for positioning
- Return buffer name and final position

### `file/read` - Read File Contents

**Purpose**: Read file contents to provide context to Claude

**Request Format**:
```json
{
  "jsonrpc": "2.0",
  "method": "file/read",
  "params": {
    "uri": "file:///absolute/path/to/file.el",
    "start_line": 1,
    "end_line": 100
  },
  "id": 2
}
```

**Response Format**:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "content": "file contents here...",
    "lines": 50,
    "encoding": "utf-8"
  },
  "id": 2
}
```

**Implementation Strategy**:
- Open file in temp buffer if not already open
- Extract specified line range or entire file
- Return content with metadata
- Handle encoding and large file considerations

### `file/save` - Save File Changes

**Purpose**: Save changes to a file (typically after Claude modifies content)

**Request Format**:
```json
{
  "jsonrpc": "2.0",
  "method": "file/save",
  "params": {
    "uri": "file:///absolute/path/to/file.el",
    "content": "updated file content...",
    "create_if_not_exists": true
  },
  "id": 3
}
```

**Response Format**:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "success": true,
    "bytes_written": 1024,
    "backup_created": true
  },
  "id": 3
}
```

**Implementation Strategy**:
- Find or create buffer for file
- Replace buffer contents with new content
- Use `save-buffer` to write to disk
- Handle backup creation and permissions

### `file/close` - Close File Handle

**Purpose**: Close file buffer when Claude no longer needs it

**Request Format**:
```json
{
  "jsonrpc": "2.0",
  "method": "file/close",
  "params": {
    "uri": "file:///absolute/path/to/file.el",
    "save_if_modified": true
  },
  "id": 4
}
```

**Response Format**:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "success": true,
    "was_modified": false
  },
  "id": 4
}
```

**Implementation Strategy**:
- Find buffer by file path
- Optionally save if modified
- Use `kill-buffer` to close
- Return modification status

## Implementation Architecture

### URI Handling
```elisp
(defun claude-code-ide--parse-file-uri (uri)
  "Parse file:// URI to extract absolute path."
  (when (string-prefix-p "file://" uri)
    (url-unhex-string (substring uri 7))))

(defun claude-code-ide--create-file-uri (path)
  "Create file:// URI from absolute PATH."
  (concat "file://" (url-hexify-string (expand-file-name path))))
```

### Buffer Management
```elisp
(defun claude-code-ide--find-file-buffer (file-path)
  "Find existing buffer for FILE-PATH or nil if not open."
  (get-file-buffer (expand-file-name file-path)))

(defun claude-code-ide--ensure-file-buffer (file-path)
  "Ensure file buffer exists, creating if necessary."
  (or (claude-code-ide--find-file-buffer file-path)
      (find-file-noselect file-path)))
```

### Position Handling
```elisp
(defun claude-code-ide--goto-position (buffer line column)
  "Move cursor to LINE and COLUMN in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when line
      (forward-line (1- line)))
    (when column
      (move-to-column (1- column)))
    (list :line (line-number-at-pos)
          :column (current-column))))
```

### Content Operations
```elisp
(defun claude-code-ide--read-buffer-content (buffer &optional start-line end-line)
  "Read content from BUFFER, optionally limited to line range."
  (with-current-buffer buffer
    (if (and start-line end-line)
        (let ((start-pos (progn (goto-char (point-min))
                                (forward-line (1- start-line))
                                (point)))
              (end-pos (progn (goto-char (point-min))
                              (forward-line end-line)
                              (point))))
          (buffer-substring-no-properties start-pos end-pos))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun claude-code-ide--replace-buffer-content (buffer content)
  "Replace entire content of BUFFER with CONTENT."
  (with-current-buffer buffer
    (erase-buffer)
    (insert content)
    (set-buffer-modified-p t)))
```

## Error Handling

### File System Errors
- **File not found**: Return JSON-RPC error -32602 (Invalid params)
- **Permission denied**: Return JSON-RPC error -32603 (Internal error)
- **Invalid URI**: Return JSON-RPC error -32602 (Invalid params)

### Buffer Errors
- **Buffer creation failed**: Return internal error with details
- **Save failed**: Return error with filesystem details
- **Position out of range**: Clamp to valid range, return actual position

### Implementation Example
```elisp
(defun claude-code-ide--handle-file-error (error-data)
  "Convert file operation error to JSON-RPC error format."
  (cond
   ((string-match "No such file" error-data)
    `((code . -32602)
      (message . "File not found")
      (data . ,error-data)))
   ((string-match "Permission denied" error-data)
    `((code . -32603)
      (message . "Permission denied")
      (data . ,error-data)))
   (t
    `((code . -32603)
      (message . "Internal error")
      (data . ,error-data)))))
```

## Integration with Emacs Features

### Auto-save and Backup
- Respect Emacs auto-save settings
- Create backup files according to user configuration
- Handle version control integration (Git status updates)

### File Watching
- Monitor file changes from external sources
- Notify Claude of external modifications
- Handle file reload conflicts

### Encoding and Charset
- Detect file encoding automatically
- Preserve original encoding on save
- Handle UTF-8, Latin-1, and other common encodings

## Performance Considerations

### Large File Handling
- Implement streaming for files >1MB
- Use line-based reading for large files
- Consider memory usage for content operations

### Buffer Efficiency
- Reuse existing buffers when possible
- Clean up temporary buffers after operations
- Avoid unnecessary buffer switches

### Async Operations
- File I/O operations should not block Emacs
- Use process filters for large operations
- Provide progress feedback for slow operations

## Security Considerations

### Path Validation
- Restrict access to allowed directories
- Prevent directory traversal attacks
- Validate file URIs before processing

### Permission Checking
- Verify read/write permissions before operations
- Handle readonly files gracefully
- Respect Emacs file security settings

The next document will detail the editor state tracking tools that complement these file operations.