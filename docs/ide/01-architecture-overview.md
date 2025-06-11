# Claude Code IDE Architecture Overview

## Current Implementation Status

The current `claude-code-ide-mcp.el` provides basic WebSocket server infrastructure but **lacks actual MCP protocol implementation**. This document outlines the architecture needed for complete IDE functionality.

## Architecture Layers

```
┌─────────────────────────────────────────┐
│           Emacs IDE Interface           │  <- User interaction layer
├─────────────────────────────────────────┤
│          MCP Tool Dispatch              │  <- Phase 1A: Need to implement
├─────────────────────────────────────────┤
│        JSON-RPC 2.0 Processing          │  <- Phase 1A: Need to implement  
├─────────────────────────────────────────┤
│       WebSocket Frame Handling          │  <- Phase 1A: Need to complete
├─────────────────────────────────────────┤
│       TCP/WebSocket Infrastructure       │  <- ✅ Already implemented
└─────────────────────────────────────────┘
```

## Current Foundation (What We Have)

### ✅ TCP Server Infrastructure
- **Function**: `claude-code-ide-mcp-start-server`
- **Capability**: Creates TCP listener on available port (3000-3999)
- **Status**: Working but minimal

### ✅ WebSocket Handshake Support  
- **Function**: `claude-code-ide--ws-create-handshake-response`
- **Capability**: SHA1 hashing, accepts `permessage-deflate` compression
- **Status**: Working for initial connection

### ✅ Lockfile System
- **Function**: `claude-code-ide-mcp--create-lockfile`
- **Capability**: Creates `~/.claude/ide/server-{port}.lock` for Claude CLI discovery
- **Status**: Compatible with claudecode.nvim approach

## Missing Critical Components (What We Need)

### ❌ WebSocket Frame Processing
**Current Status**: Only handshake implemented, no actual frame parsing/generation

**Required Implementation**:
```elisp
;; Frame parsing for incoming messages
(defun claude-code-ide--ws-parse-frame (data))

;; Frame generation for outgoing messages  
(defun claude-code-ide--ws-create-frame (opcode payload))

;; Message handling pipeline
(defun claude-code-ide--ws-handle-message (client frame))
```

### ❌ JSON-RPC 2.0 Protocol
**Current Status**: No JSON-RPC implementation

**Required Implementation**:
```elisp
;; Request/response correlation
(defun claude-code-ide--jsonrpc-send-request (client method params))
(defun claude-code-ide--jsonrpc-send-response (client id result))
(defun claude-code-ide--jsonrpc-send-error (client id error))

;; Message dispatch
(defun claude-code-ide--jsonrpc-dispatch (client message))
```

### ❌ MCP Tool Registry
**Current Status**: No MCP tools implemented

**Required Implementation**:
```elisp
;; Tool registration system
(defvar claude-code-ide--mcp-tools (make-hash-table :test 'equal))

;; Tool implementations
(claude-code-ide--register-mcp-tool "file/open" #'claude-code-ide--mcp-file-open)
(claude-code-ide--register-mcp-tool "editor/selection" #'claude-code-ide--mcp-editor-selection)
```

## WebSocket Protocol Flow

### Connection Establishment
1. **TCP Accept** → Current filter just logs, needs frame processing
2. **HTTP Upgrade Request** → Parse WebSocket handshake (✅ working)
3. **WebSocket Response** → Send upgrade response (✅ working)
4. **MCP Initialization** → JSON-RPC capability negotiation (❌ missing)

### Message Processing Loop
1. **Frame Reception** → Parse WebSocket frames (❌ missing)
2. **JSON-RPC Parsing** → Extract method, params, id (❌ missing)
3. **Tool Dispatch** → Route to appropriate MCP tool handler (❌ missing)
4. **Response Generation** → Create JSON-RPC response (❌ missing)
5. **Frame Transmission** → Send WebSocket frame back (❌ missing)

## MCP Protocol Essentials

### Message Types
- **Requests**: Client calls server methods (e.g., `file/open`)
- **Responses**: Server replies with results or errors  
- **Notifications**: One-way messages (e.g., selection updates)

### Core MCP Tools Needed
- `file/open` - Open file with positioning
- `file/save` - Save file changes
- `file/read` - Read file contents
- `editor/selection` - Track text selection
- `editor/position` - Track cursor position
- `workspace/folders` - List workspace folders

## Implementation Priority

### Phase 1A (Day 1 Morning)
1. **Complete WebSocket frame parsing** 
2. **Implement JSON-RPC 2.0 foundation**
3. **Create MCP tool dispatch system**

### Phase 1B (Day 1 Afternoon)  
4. **Implement core file operations**
5. **Add editor state tracking**
6. **Test basic MCP integration**

## Next Steps

The next document will detail the specific JSON-RPC 2.0 implementation approach and WebSocket frame handling that we need to add to the existing codebase.