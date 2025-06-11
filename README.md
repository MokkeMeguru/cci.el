# Claude Code IDE.el

A comprehensive Claude Code IDE integration for Emacs that provides AI-assisted development capabilities comparable to VSCode and JetBrains plugins.

## Overview

While VSCode and JetBrains have official Claude Code IDE integration, Emacs users have been underserved. This package fills that gap by implementing both terminal-based interaction and true IDE integration using the Model Context Protocol (MCP).

## Features

### 🚀 **Dual Integration Modes**

#### 🖥️ **Terminal Mode** 
- Direct Claude CLI integration via terminal emulation
- Simple and reliable for basic AI assistance
- Perfect for chat-based interactions and quick queries
- Zero configuration required

#### 🔧 **MCP IDE Mode** ⚠️ **TEMPORARILY DISABLED**
- ⚠️ **Currently disabled due to WebSocket connection issues**
- **True IDE Integration** using Model Context Protocol over WebSocket
- **Real-time File Operations**: Open, edit, save files with precise line/column positioning
- **Selection Tracking**: Claude sees your current text selections and cursor position
- **Workspace Awareness**: Automatic project detection and context sharing
- **WebSocket Compression**: Efficient permessage-deflate compression support
- **RFC 6455 Compliant**: Standards-based WebSocket implementation

### 📝 **Core Capabilities**
- **Session Management**: Start/stop Claude Code with conversation continuity
- **Content Sharing**: Send buffers, regions, files, or entire project context
- **Interactive Menus**: Intuitive transient-based command interface
- **Extensive Configuration**: Customize behavior and keybindings
- **Error Recovery**: Robust handling of network and API issues

## Prerequisites

### Required
1. **Emacs 30.1+** - Modern Emacs with built-in transient support
2. **Claude Code CLI** - Follow the [official installation guide](https://docs.anthropic.com/en/docs/claude-code)
3. **eat package** - Terminal emulation for Claude CLI integration
   ```elisp
   M-x package-install RET eat RET
   ```

### Verification
```bash
# Verify Claude CLI is installed
claude --version

# Verify Emacs version
emacs --version
```

## Installation

### Method 1: Direct Installation (Recommended)

```bash
# Clone the repository
git clone https://github.com/mixi-sb/claude-code-ide.el.git
cd claude-code-ide.el

# Install eat dependency
emacs --batch --eval "(progn (require 'package) (package-initialize) (package-refresh-contents) (package-install 'eat))"

# Compile and install
make install
```

### Method 2: Manual Setup

1. Install the `eat` package:
   ```elisp
   M-x package-install RET eat RET
   ```

2. Clone and configure:
   ```bash
   git clone https://github.com/mixi-sb/claude-code-ide.el.git ~/.emacs.d/claude-code-ide
   ```

3. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/claude-code-ide")
   (require 'claude-code-ide)
   (global-claude-code-ide-mode 1)
   ```

## Quick Start

### Basic Usage (Terminal Mode)

```elisp
;; Start Claude in project directory
M-x claude-code-ide-start
;; or: C-c c s

;; Send current buffer for analysis
C-c c r

;; Ask questions in the Claude terminal
;; Use slash commands like /help, /clear, /model
```

### Advanced Usage (MCP IDE Mode)

```elisp
;; Enable MCP IDE integration
M-x claude-code-ide-mcp-toggle-mode
;; or: C-c c M

;; Start MCP WebSocket server  
M-x claude-code-ide-mcp-start-server
;; or: C-c c >

;; In terminal, start Claude CLI (it will auto-detect the MCP server)
;; $ claude

;; Now Claude can directly manipulate files, track selections, etc.
```

## Key Bindings

All commands use the `C-c c` prefix when `claude-code-ide-mode` is active:

### Session Management
| Key | Command | Description |
|-----|---------|-------------|
| `C-c c s` | `claude-code-ide-start` | Start Claude in project directory |
| `C-c c d` | `claude-code-ide-current-directory` | Start in current directory |
| `C-c c k` | `claude-code-ide-kill` | Kill Claude session |
| `C-c c t` | `claude-code-ide-toggle` | Toggle window visibility |
| `C-c c b` | `claude-code-ide-switch-to-buffer` | Switch to Claude buffer |
| `C-c c S` | `claude-code-ide-status` | Show status |

### Content Interaction  
| Key | Command | Description |
|-----|---------|-------------|
| `C-c c c` | `claude-code-ide-send-command` | Send custom command |
| `C-c c r` | `claude-code-ide-send-region` | Send region/buffer |
| `C-c c f` | `claude-code-ide-send-file` | Send file contents |

### MCP IDE Integration
| Key | Command | Description |
|-----|---------|-------------|
| `C-c c M` | `claude-code-ide-mcp-toggle-mode` | Toggle MCP IDE mode |
| `C-c c >` | `claude-code-ide-mcp-start-server` | Start MCP server |
| `C-c c <` | `claude-code-ide-mcp-stop-server` | Stop MCP server |
| `C-c c ?` | `claude-code-ide-mcp-status` | Show MCP status |
| `C-c c L` | `claude-code-ide-mcp-enable-logging` | Enable debug logging |

### Menu & Utilities
| Key | Command | Description |
|-----|---------|-------------|
| `C-c c m` | `claude-code-ide-menu` | Open main menu |
| `C-c c /` | `claude-code-ide-slash-help` | Send /help |
| `C-c c C` | `claude-code-ide-slash-clear` | Send /clear |

## Configuration

### Basic Setup

```elisp
;; Essential configuration
(use-package claude-code-ide
  :ensure nil ; manually installed
  :config
  ;; Enable globally  
  (global-claude-code-ide-mode 1)
  
  ;; Configure Claude CLI path if needed
  (setq claude-code-ide-program "/usr/local/bin/claude")
  
  ;; Enable MCP IDE mode by default
  (setq claude-code-ide-mcp-enabled t)
  (setq claude-code-ide-mcp-auto-start t))
```

### Advanced Configuration

```elisp
;; Customization options
(setq claude-code-ide-buffer-name "*Claude*")           ; Buffer name
(setq claude-code-ide-startup-delay 0.2)               ; Startup delay  
(setq claude-code-ide-large-buffer-threshold 2000)     ; Large buffer warning
(setq claude-code-ide-mcp-port-range '(3000 . 3010))   ; MCP port range
(setq claude-code-ide-term-name "xterm-256color")      ; Terminal type

;; Custom keybinding prefix
(setq claude-code-ide-keymap-prefix "C-c a") ; Use C-c a instead of C-c c
```

### Customization Interface

```elisp
M-x customize-group RET claude-code-ide RET
```

## Usage Workflows

### 1. Code Review & Analysis

```elisp
;; Select code region
;; C-c c r (send to Claude)
;; Ask: "Review this code for potential issues"
```

### 2. Documentation Generation

```elisp
;; Place cursor in function
;; C-c c c
;; Type: "Generate documentation for this function"
```

### 3. Debugging Assistance

```elisp
;; Send error message and relevant code
;; C-c c r
;; Claude can analyze and suggest fixes
```

### 4. MCP IDE Integration Workflow

```elisp
;; 1. Enable MCP mode
C-c c M

;; 2. Start MCP server
C-c c >  

;; 3. Start Claude CLI in terminal
;; $ claude

;; 4. Claude can now:
;;    - Open files directly: "Open src/main.py at line 45"  
;;    - Edit files: "Add error handling to the login function"
;;    - See your selections: "Explain this selected code"
;;    - Navigate workspace: "Show me all the test files"
```

## MCP Server Features

When MCP IDE mode is enabled, Claude gains these capabilities:

### File Operations
- **Open files** with precise line/column positioning
- **Save documents** with validation and error handling  
- **Close tabs** with optional save-first behavior

### Context Awareness
- **Current selection** tracking with line/column details
- **Open editors** list with modification status
- **Workspace folders** detection and project structure
- **Real-time updates** as you navigate and edit

### WebSocket Protocol
- **Standards compliant** RFC 6455 implementation
- **Compression support** via permessage-deflate extension
- **Robust error handling** with connection recovery
- **Debug logging** for troubleshooting connections

## Troubleshooting

### Common Issues

#### 1. Claude CLI Not Found
```bash
# Check if Claude is in PATH
which claude

# If not found, install Claude CLI or set path:
(setq claude-code-ide-program "/full/path/to/claude")
```

#### 2. eat Package Missing
```elisp
;; Install eat package
M-x package-install RET eat RET
```

#### 3. MCP Connection Issues
```elisp
;; Enable debug logging
M-x claude-code-ide-mcp-enable-logging

;; Check MCP status
M-x claude-code-ide-mcp-status

;; Restart MCP server
M-x claude-code-ide-mcp-stop-server
M-x claude-code-ide-mcp-start-server

;; Check *claude-mcp-log* buffer for details
```

#### 4. WebSocket Connection Failures
- Check firewall settings for localhost connections
- Verify port range 3000-3999 is available
- Look at MCP debug logs for handshake details
- Try restarting both Emacs and Claude CLI

### Debug Information

```elisp
;; Show current status
C-c c S  ; Claude status
C-c c ?  ; MCP server status

;; Enable comprehensive logging
C-c c L  ; MCP debug logging

;; Check log buffers
;; *claude-mcp-log* - MCP protocol messages
;; *Claude Code* - Claude CLI output
```

## Development

### Building from Source

```bash
# Compile package
make compile

# Run linting
make checkdoc

# Run all checks
make all

# Clean build artifacts
make clean
```

### Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Make changes and test thoroughly
4. Run linting: `make all`
5. Submit a pull request

### Architecture

- **Single-file design** - All functionality in `claude-code-ide.el`
- **WebSocket server** - Custom RFC 6455 implementation  
- **MCP protocol** - JSON-RPC 2.0 over WebSocket
- **Terminal integration** - Via `eat` package for CLI mode
- **Transient menus** - Modern Emacs UI patterns

## Comparison

| Feature | claude-code-ide.el | VSCode Extension | JetBrains Plugin |
|---------|-------------------|------------------|------------------|
| Terminal Mode | ✅ | ✅ | ✅ |
| MCP IDE Integration | ✅ | ✅ | ✅ |
| File Operations | ✅ | ✅ | ✅ |
| Selection Tracking | ✅ | ✅ | ✅ |
| WebSocket Compression | ✅ | ✅ | ✅ |
| Emacs Integration | ✅ | ❌ | ❌ |
| Open Source | ✅ | ❌ | ❌ |
| Customizable | ✅ | Limited | Limited |

## References

- **[Claude Code Documentation](https://docs.anthropic.com/en/docs/claude-code)** - Official documentation
- **[claudecode.nvim](https://github.com/coder/claudecode.nvim)** - Neovim implementation (inspiration)
- **[Model Context Protocol](https://spec.modelcontextprotocol.io/)** - MCP specification
- **[WebSocket RFC 6455](https://tools.ietf.org/html/rfc6455)** - WebSocket protocol standard

## License

Licensed under the Apache License 2.0. See LICENSE file for details.

---

**Made for Emacs users who want the full Claude Code experience without leaving their favorite editor.**
