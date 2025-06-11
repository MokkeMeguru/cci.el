# MCP Editor State Tools

## Overview

Editor state tools provide real-time awareness of cursor position, text selection, and buffer state to Claude. These tools enable Claude to understand the current editing context and provide contextually relevant assistance.

## MCP Editor Tool Specifications

### `editor/selection` - Text Selection Tracking

**Purpose**: Track current text selection and cursor position in active buffer

**Request Format**:
```json
{
  "jsonrpc": "2.0",
  "method": "editor/selection",
  "params": {},
  "id": 5
}
```

**Response Format**:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "active": true,
    "start": 150,
    "end": 200,
    "text": "selected text content",
    "line_start": 10,
    "line_end": 12,
    "column_start": 5,
    "column_end": 15
  },
  "id": 5
}
```

**Implementation Strategy**:
- Use `region-active-p` to check if selection is active
- Get selection bounds with `region-beginning` and `region-end`
- Convert positions to line/column coordinates
- Extract selected text with `buffer-substring`

### `editor/position` - Cursor Position Tracking

**Purpose**: Get current cursor position and buffer information

**Request Format**:
```json
{
  "jsonrpc": "2.0",
  "method": "editor/position",
  "params": {},
  "id": 6
}
```

**Response Format**:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "line": 42,
    "column": 15,
    "position": 1024,
    "buffer": "claude-code-ide.el",
    "uri": "file:///path/to/claude-code-ide.el",
    "total_lines": 100
  },
  "id": 6
}
```

**Implementation Strategy**:
- Use `point` for absolute position
- Use `line-number-at-pos` and `current-column` for coordinates
- Get buffer name and file path for context
- Count total lines for context

### `editor/buffers` - Open Buffer Enumeration

**Purpose**: List all open buffers and their states

**Request Format**:
```json
{
  "jsonrpc": "2.0",
  "method": "editor/buffers",
  "params": {
    "include_special": false
  },
  "id": 7
}
```

**Response Format**:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "buffers": [
      {
        "name": "claude-code-ide.el",
        "uri": "file:///path/to/claude-code-ide.el",
        "modified": true,
        "size": 2048,
        "major_mode": "emacs-lisp-mode",
        "active": true
      }
    ],
    "total": 1
  },
  "id": 7
}
```

**Implementation Strategy**:
- Use `buffer-list` to enumerate buffers
- Filter special buffers (starting with space or *) unless requested
- Get buffer properties: modified state, size, mode, file association
- Mark current buffer as active

### `editor/context` - Rich Context Information

**Purpose**: Provide comprehensive editing context around cursor

**Request Format**:
```json
{
  "jsonrpc": "2.0",
  "method": "editor/context",
  "params": {
    "lines_before": 5,
    "lines_after": 5
  },
  "id": 8
}
```

**Response Format**:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "current_line": 42,
    "current_column": 15,
    "context_before": "previous 5 lines...",
    "context_after": "next 5 lines...",
    "current_line_text": "current line content",
    "function_name": "defun claude-code-ide-start",
    "file_type": "emacs-lisp"
  },
  "id": 8
}
```

**Implementation Strategy**:
- Extract lines before/after current position
- Identify current function/class context using imenu or parsing
- Determine file type from major mode
- Provide rich contextual information for Claude

## Implementation Architecture

### Position Utilities
```elisp
(defun claude-code-ide--get-cursor-position ()
  "Get current cursor position with line/column information."
  (list (cons 'line (line-number-at-pos))
        (cons 'column (current-column))
        (cons 'position (point))))

(defun claude-code-ide--position-to-line-column (pos)
  "Convert absolute position POS to line/column coordinates."
  (save-excursion
    (goto-char pos)
    (list (cons 'line (line-number-at-pos))
          (cons 'column (current-column)))))
```

### Selection Utilities
```elisp
(defun claude-code-ide--get-selection-info ()
  "Get current selection information."
  (if (region-active-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties start end))
             (start-pos (claude-code-ide--position-to-line-column start))
             (end-pos (claude-code-ide--position-to-line-column end)))
        `((active . t)
          (start . ,start)
          (end . ,end)
          (text . ,text)
          (line_start . ,(alist-get 'line start-pos))
          (line_end . ,(alist-get 'line end-pos))
          (column_start . ,(alist-get 'column start-pos))
          (column_end . ,(alist-get 'column end-pos))))
    `((active . nil))))
```

### Buffer Management
```elisp
(defun claude-code-ide--get-buffer-info (buffer &optional active)
  "Get information about BUFFER, marking as ACTIVE if specified."
  (with-current-buffer buffer
    (let* ((file-name (buffer-file-name))
           (uri (when file-name (claude-code-ide--create-file-uri file-name))))
      `((name . ,(buffer-name))
        (uri . ,uri)
        (modified . ,(buffer-modified-p))
        (size . ,(buffer-size))
        (major_mode . ,(symbol-name major-mode))
        (active . ,active)))))

(defun claude-code-ide--list-buffers (&optional include-special)
  "List all buffers, optionally including special buffers."
  (let ((buffers (buffer-list))
        (current (current-buffer))
        (result '()))
    (dolist (buffer buffers)
      (let ((name (buffer-name buffer)))
        (when (or include-special
                  (not (string-match-p "^[ *]" name)))
          (push (claude-code-ide--get-buffer-info
                 buffer (eq buffer current)) result))))
    (reverse result)))
```

### Context Extraction
```elisp
(defun claude-code-ide--get-context-lines (lines-before lines-after)
  "Get context lines around current position."
  (let* ((current-line (line-number-at-pos))
         (start-line (max 1 (- current-line lines-before)))
         (end-line (+ current-line lines-after))
         (total-lines (count-lines (point-min) (point-max))))

    (setq end-line (min end-line total-lines))

    `((context_before . ,(claude-code-ide--get-lines start-line (1- current-line)))
      (context_after . ,(claude-code-ide--get-lines (1+ current-line) end-line))
      (current_line_text . ,(thing-at-point 'line t)))))

(defun claude-code-ide--get-lines (start-line end-line)
  "Get text content from START-LINE to END-LINE."
  (when (and (> start-line 0) (> end-line 0) (<= start-line end-line))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((start-pos (point)))
        (forward-line (- end-line start-line -1))
        (buffer-substring-no-properties start-pos (point))))))
```

### Function Context Detection
```elisp
(defun claude-code-ide--get-function-context ()
  "Get current function/class context."
  (condition-case nil
      (which-function)
    (error nil)))

(defun claude-code-ide--get-file-type ()
  "Get file type based on major mode."
  (let ((mode-name (symbol-name major-mode)))
    (cond
     ((string-match "emacs-lisp" mode-name) "emacs-lisp")
     ((string-match "python" mode-name) "python")
     ((string-match "javascript\\|js" mode-name) "javascript")
     ((string-match "typescript\\|ts" mode-name) "typescript")
     ((string-match "java" mode-name) "java")
     ((string-match "c++" mode-name) "cpp")
     ((string-match "c-mode" mode-name) "c")
     (t "text"))))
```

## Real-time Notifications

### Selection Change Notifications
```elisp
(defvar claude-code-ide--last-selection nil
  "Last known selection state for change detection.")

(defun claude-code-ide--check-selection-change ()
  "Check if selection has changed and notify clients."
  (let ((current-selection (claude-code-ide--get-selection-info)))
    (unless (equal current-selection claude-code-ide--last-selection)
      (setq claude-code-ide--last-selection current-selection)
      (claude-code-ide--notify-selection-change current-selection))))

(defun claude-code-ide--notify-selection-change (selection-info)
  "Send selection change notification to all connected clients."
  (dolist (client claude-code-ide-mcp--clients)
    (claude-code-ide--jsonrpc-send-notification
     client "editor/selection_changed" selection-info)))
```

### Position Change Notifications
```elisp
(defvar claude-code-ide--last-position nil
  "Last known cursor position for change detection.")

(defun claude-code-ide--check-position-change ()
  "Check if cursor position has changed and notify clients."
  (let ((current-position (claude-code-ide--get-cursor-position)))
    (unless (equal current-position claude-code-ide--last-position)
      (setq claude-code-ide--last-position current-position)
      (claude-code-ide--notify-position-change current-position))))
```

### Hook Integration
```elisp
;; Set up hooks for real-time tracking
(add-hook 'post-command-hook #'claude-code-ide--check-selection-change)
(add-hook 'post-command-hook #'claude-code-ide--check-position-change)
```

## Performance Considerations

### Efficient Change Detection
- Use `equal` comparison to avoid unnecessary notifications
- Cache previous state to minimize computation
- Throttle notifications to prevent flooding

### Selective Context
- Allow clients to specify context window size
- Provide options to include/exclude special buffers
- Limit text extraction for large selections

### Async Processing
- Process editor state requests asynchronously
- Use timers for periodic state updates
- Avoid blocking Emacs during state extraction

The next document will detail workspace awareness tools that complement editor state tracking.