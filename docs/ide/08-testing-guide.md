# Claude Code IDE Testing Guide

## Testing Overview

This guide walks through testing our complete MCP implementation with Claude CLI to verify the WebSocket → JSON-RPC → MCP tool integration works correctly.

## Step 1: Verify Claude CLI Installation

First, ensure Claude CLI is installed and accessible:

```bash
# Check Claude CLI version
claude --version

# If not installed, follow installation instructions:
# https://docs.anthropic.com/en/docs/claude-code/installation
```

## Step 2: Start MCP Server

### In Emacs:
```elisp
;; 1. Load our implementation
(load-file "/Users/takuya.ebata/GitHub/mixi-sb/claude-code-ide/claude-code-ide-mcp.el")

;; 2. Enable debug logging (important for testing)
M-x claude-code-ide-mcp-enable-logging

;; 3. Start the MCP server
M-x claude-code-ide-mcp-start-server
```

### Expected Output:
```
Claude Code MCP server started on port XXXX
```

### Verify in Log:
```elisp
;; Check *claude-mcp-log* buffer for:
;; [SERVER] MCP server started on port XXXX
;; [TOOL] Registered MCP tool: file/open
;; [TOOL] Registered MCP tool: file/read
;; ... (11 total tools should be registered)
;; [WORKSPACE] === Current Workspace Information ===
;; [WORKSPACE] Project Root: /Users/.../claude-code-ide
;; [WORKSPACE] Current Directory: /Users/.../claude-code-ide
;; [WORKSPACE] Git repository: /Users/.../claude-code-ide/.git
;; ... (detailed workspace info)

;; You can also manually log workspace info anytime:
M-x claude-code-ide-mcp-log-workspace
```

## Step 3: Verify Lockfile Creation

Check that the lockfile is created for Claude CLI discovery:

```bash
# Check lockfile exists
ls ~/.claude/ide/

# Should show: server-XXXX.lock
# Examine lockfile content
cat ~/.claude/ide/server-*.lock
```

**Expected Content:**
```json
{"port":XXXX,"pid":YYYY,"created":[...]}
```

## Step 4: Test with Claude CLI

### Basic Connection Test:
```bash
# Start Claude CLI in MCP mode
claude --mcp
# or
claude --ide

# Claude should auto-discover our server via lockfile
# and establish WebSocket connection
```

### Monitor Connection in Emacs:
```elisp
;; Watch *claude-mcp-log* buffer for:
;; [CONNECTION] Process <process>: open
;; [HANDSHAKE] WebSocket handshake completed for client-XXXX
```

## Step 5: Test MCP Tools

### Test File Operations:

**Open a file:**
```
In Claude: "Open the file /Users/takuya.ebata/GitHub/mixi-sb/claude-code-ide/claude-code-ide.el at line 10"
```

**Expected in Log:**
```
[FRAME] Text frame from client-XXXX: {"jsonrpc":"2.0","method":"file/open","params":{"uri":"file:///Users/...","line":10},"id":1}
[REQUEST] Method: file/open, ID: 1
[FILE-OPEN] Params: ((uri . "file://...")...)
[SEND] Sending to client-XXXX: {"jsonrpc":"2.0","result":{"success":true,...},"id":1}
```

**Expected in Emacs:**
- File should open in buffer
- Cursor should be at line 10

### Test Editor State:

**Get current position:**
```
In Claude: "What's my current cursor position?"
```

**Expected MCP call:**
```
[REQUEST] Method: editor/position, ID: 2
[EDITOR-POSITION] Getting current position
```

### Test Workspace Awareness:

**Get project info:**
```
In Claude: "What files are in this workspace?"
```

**Expected MCP calls:**
```
[REQUEST] Method: workspace/folders, ID: 3
[REQUEST] Method: workspace/files, ID: 4
```

## Step 6: Advanced Testing

### Test Selection Tracking:
1. Select some text in Emacs
2. Ask Claude: "What do I have selected?"
3. Verify `editor/selection` is called and returns correct selection

### Test File Reading:
```
In Claude: "Read the first 20 lines of claude-code-ide.el"
```

### Test File Saving:
```
In Claude: "Add a comment '// Test comment' at the top of this file and save it"
```

## Step 7: Error Testing

### Test Invalid File:
```
In Claude: "Open /nonexistent/file.txt"
```

**Expected Error Response:**
```json
{"jsonrpc":"2.0","error":{"code":-32602,"message":"File not found"},"id":X}
```

### Test Invalid Method:
Send a request with non-existent method to verify error handling.

## Debugging Connection Issues

### Common Issues:

**1. Port Already in Use:**
```elisp
;; Check server status
M-x claude-code-ide-mcp-status

;; Stop and restart if needed
M-x claude-code-ide-mcp-stop-server
M-x claude-code-ide-mcp-start-server
```

**2. Claude CLI Not Finding Server:**
```bash
# Check if lockfile exists and has correct port
ls -la ~/.claude/ide/
cat ~/.claude/ide/server-*.lock

# Verify Claude CLI is looking in right directory
```

**3. WebSocket Handshake Issues:**
```elisp
;; Enable detailed logging and check for:
;; - HTTP upgrade request received
;; - WebSocket key processing
;; - Protocol negotiation (should include "mcp")
```

### Debug Network Connection:
```bash
# Test port is open
netstat -an | grep XXXX

# Test basic connection
telnet localhost XXXX
```

## Performance Testing

### Large File Operations:
1. Test opening large files (>1MB)
2. Test reading large selections
3. Monitor memory usage in Emacs

### Multiple Requests:
1. Send rapid succession of MCP requests
2. Verify responses are properly correlated by ID
3. Check for any blocking behavior

## Success Criteria

### ✅ Connection Test:
- [ ] Claude CLI connects to MCP server
- [ ] WebSocket handshake completes successfully
- [ ] All 11 MCP tools are registered

### ✅ File Operations:
- [ ] `file/open` opens files and positions cursor
- [ ] `file/read` returns file contents correctly
- [ ] `file/save` saves changes to disk
- [ ] `file/close` closes buffers properly

### ✅ Editor State:
- [ ] `editor/position` returns accurate cursor position
- [ ] `editor/selection` tracks text selection
- [ ] `editor/buffers` lists open buffers
- [ ] `editor/context` provides contextual information

### ✅ Workspace:
- [ ] `workspace/folders` detects project root
- [ ] `workspace/files` lists project files
- [ ] Project detection works via .git, package.json, etc.

### ✅ Error Handling:
- [ ] Invalid file paths return proper JSON-RPC errors
- [ ] Malformed requests don't crash server
- [ ] Unknown methods return "Method not found" error

## Automated Test Script

For convenience, create a test script:

```bash
#!/bin/bash
# test-mcp-integration.sh

echo "Testing Claude Code MCP Integration..."

# Check Claude CLI
if ! command -v claude &> /dev/null; then
    echo "❌ Claude CLI not found"
    exit 1
fi

# Check Emacs server
if [ ! -f ~/.claude/ide/server-*.lock ]; then
    echo "❌ MCP server not running"
    exit 1
fi

echo "✅ Prerequisites met"

# Test connection (requires Claude CLI to support test mode)
# claude --mcp --test-connection

echo "Integration test complete"
```

## Next Steps After Testing

Once testing passes:
1. Document any discovered issues
2. Optimize performance bottlenecks
3. Add additional MCP tools as needed
4. Create user documentation for end users
5. Prepare for production deployment

The testing process validates that our complete implementation provides working Claude Code IDE integration within Emacs.