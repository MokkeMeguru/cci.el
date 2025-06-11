# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Claude Code IDE.el Development Guide

## Project Overview

This repository provides a comprehensive Claude Code IDE implementation for Emacs users, offering both terminal-based integration and advanced MCP (Model Context Protocol) IDE capabilities comparable to VSCode and JetBrains plugins.

**Current Status**: **Terminal Mode Production Ready** - Terminal integration stable, MCP mode temporarily disabled due to WebSocket connection issues.

## Architecture

### Core Implementation
- **Modular design**: Four-file architecture for clean separation of concerns
  - `claude-code-ide.el` - Main entry point with autoload declarations
  - `claude-code-ide-utils.el` - Configuration, transient menus, keybindings, minor mode
  - `claude-code-ide-session.el` - Terminal-based Claude CLI integration via vterm
  - `claude-code-ide-mcp.el` - WebSocket server and MCP protocol (currently disabled)
- **Dual integration modes**: Terminal mode (stable), MCP mode for advanced IDE features (disabled)
- **WebSocket server**: RFC 6455 compliant implementation with compression support
- **MCP protocol**: JSON-RPC 2.0 over WebSocket for true IDE integration

### Key Components

1. **Terminal Integration** (`vterm` package)
   - Direct Claude CLI interaction via terminal emulation
   - Session management with conversation continuity
   - Content sharing (buffers, regions, files)

2. **MCP WebSocket Server**
   - Standards-compliant RFC 6455 WebSocket implementation
   - permessage-deflate compression support
   - Connection management and error recovery
   - Lockfile system for Claude CLI auto-discovery

3. **MCP Protocol Handler**
   - JSON-RPC 2.0 message processing
   - Tool implementations (file operations, selection tracking)
   - Workspace awareness and project detection
   - Real-time context sharing

4. **User Interface**
   - Transient-based interactive menus
   - Comprehensive keybinding system (`C-c c` prefix)
   - Status reporting and debug logging
   - Customization interface

## References and Related Projects

- **Neovim Implementation**: https://github.com/coder/claudecode.nvim (architectural inspiration)
- **Claude Code Documentation**: https://docs.anthropic.com/en/docs/claude-code
- **MCP Specification**: https://spec.modelcontextprotocol.io/
- **WebSocket RFC 6455**: https://tools.ietf.org/html/rfc6455
- **Related Project**: claude-code.el by stevemolitor

## Build and Test Commands

### Primary Commands (use these)
```bash
# Compile and lint (recommended)
make all

# Individual operations
make compile      # Byte compilation (modules first, then main)
make checkdoc     # Documentation linting
make clean        # Clean build artifacts
make install      # Install package locally
make help         # Show all available targets
```

### Alternative Commands
```bash
# Direct compilation
emacs -Q --batch -f batch-byte-compile claude-code-ide.el

# Documentation check
emacs -Q --batch -l checkdoc -f checkdoc-file claude-code-ide.el

# Install package
M-x package-install-file RET /path/to/claude-code-ide.el
```

### Testing Commands
```bash
# Test MCP server functionality
emacs -Q --batch -l claude-code-ide.el --eval '(claude-code-ide-mcp-status)'

# Test function definitions
emacs -Q --batch -l claude-code-ide.el --eval '(message "%s" (fboundp (quote claude-code-ide-start)))'
```

## Code Style Guidelines

### Naming Conventions
- **Public functions**: `claude-code-ide-` prefix (e.g., `claude-code-ide-start`)
- **Private functions**: `claude-code-ide--` prefix (e.g., `claude-code-ide--ws-parse-frame`)
- **MCP functions**: `claude-code-ide-mcp-` or `claude-code-ide-mcp--` prefix
- **Variables**: Follow same prefix pattern
- **Constants**: Use `claude-code-ide--` prefix with descriptive names

### Code Structure
- **Lexical binding**: Always include `;;; -*- lexical-binding: t; -*-`
- **Section headers**: Use `;;;; Section Name` for major sections
- **Autoload declarations**: Mark interactive functions with `;;;###autoload`
- **Documentation**: Comprehensive docstrings for all public functions
- **Dependencies**: Minimal external dependencies (transient, vterm, cl-lib, json)

### Documentation Standards
- **First line**: Complete sentence ending with period
- **Multi-line**: Blank line after first line, then detailed description
- **Parameters**: Document all parameters in CAPS (e.g., "Handle MESSAGE from WEBSOCKET")
- **Return values**: Document what functions return when relevant

### Example Function Structure
```elisp
;;;###autoload
(defun claude-code-ide-example-function (param1 &optional param2)
  "Brief description ending with period.

Detailed description of what this function does, including any
important implementation details or usage notes.

PARAM1 is the required parameter description.
PARAM2 is the optional parameter description."
  (interactive "sPrompt: \nP")
  ;; Implementation here
  )
```

## Development Guidelines

### Commit Standards
- **Feature commits**: Use `feat:` prefix for new functionality
- **Bug fixes**: Use `fix:` prefix for bug corrections
- **Documentation**: Use `docs:` prefix for documentation updates
- **Refactoring**: Use `refactor:` prefix for code improvements
- **Format**: Short first line, detailed body if needed, co-authored signature

### Release Process
- **Major features**: Create release branch `release/x.y.z`
- **Version bumping**: Update version in package header
- **Testing**: Ensure `make all` passes without warnings
- **Documentation**: Update README.md and CLAUDE.md as needed

### Error Handling Standards
- **Conditional execution**: Use `if-let` and `when-let` for null-safe operations
- **Error messages**: Provide clear, actionable error messages with `error` function
- **State checking**: Always verify Claude/MCP server state before operations
- **Graceful degradation**: Handle network errors and connection failures

### Testing Strategy
- **Function definitions**: Verify all functions are properly defined after loading
- **MCP server**: Test WebSocket server startup/shutdown cycle
- **Integration**: Test with actual Claude CLI when possible
- **Error conditions**: Test error handling for common failure scenarios

## Current Implementation Status

### ✅ Completed Features
- **Terminal Mode**: Full Claude CLI integration via `vterm` package (production ready)
- **Modular Architecture**: Clean four-file separation with proper autoloads
- **Interactive UI**: Transient menus and comprehensive keybindings (`C-c c` prefix)
- **vterm Integration**: Recently migrated from `eat` package for better stability
- **Buffer Positioning**: Right->down positioning for vterm buffers

### ⚠️ Disabled Features (WebSocket Connection Issues)
- **MCP WebSocket Server**: RFC 6455 compliant implementation (temporarily disabled)
- **MCP Protocol**: JSON-RPC 2.0 implementation (temporarily disabled)
- **File Operations**: Open, save, close with line/column positioning (disabled)
- **Selection Tracking**: Real-time text selection and context sharing (disabled)
- **Workspace Management**: Project detection and folder management (disabled)

### 🔄 Next Steps
- **Fix WebSocket Issues**: Debug and restore MCP functionality
- **Re-enable MCP Features**: File operations, selection tracking, workspace management
- **LSP Integration**: Connect with eglot/lsp-mode for diagnostics
- **Extended Tool Support**: Additional MCP tools and capabilities

## Technical Requirements

### Dependencies
- **Emacs 30.1+**: Modern Emacs with built-in transient support
- **transient 0.4.0+**: For interactive menu system (included in Emacs 30.1+)
- **vterm**: Terminal emulation for Claude CLI integration
- **cl-lib**: Common Lisp extensions (built-in)
- **json**: JSON processing (built-in)

### System Requirements
- **Claude Code CLI**: Official Claude CLI tool installed and accessible
- **Network Access**: For WebSocket connections (localhost only)
- **Port Availability**: Ports 55000-56000 for MCP server (configurable)

## Troubleshooting Development Issues

### Common Build Problems
```bash
# Clean and rebuild
make clean && make all

# Check function definitions
emacs -Q --batch -l claude-code-ide.el --eval '(message "Functions loaded successfully")'

# Test MCP server
emacs -Q --batch -l claude-code-ide.el --eval '(claude-code-ide-mcp-enable-logging)'
```

### WebSocket Connection Issues
- Enable debug logging: `M-x claude-code-ide-mcp-enable-logging`
- Check *claude-mcp-log* buffer for handshake details
- Verify port availability in configured range
- Test with simple WebSocket client tools

### Claude CLI Integration
- Verify Claude CLI installation: `claude --version`
- Check PATH availability or set explicit path
- Test terminal mode before MCP mode
- Review Claude CLI logs for connection attempts

## License

Licensed under the Apache License 2.0. See LICENSE file for details.

## Git Workflow

### Current Branch Information
- **Main branch**: `main` (use for PRs)
- **Feature branch pattern**: `copilot/fix-N`
- **Current focus**: Buffer positioning improvements and WebSocket debugging

### Recent Changes
- Migrated from `eat` to `vterm` package for better terminal stability
- Implemented right->down buffer positioning for vterm buffers
- Temporarily disabled MCP functionality due to WebSocket connection issues

---

This guide reflects the current state of claude-code-ide.el: stable terminal integration with MCP functionality temporarily disabled pending WebSocket issue resolution.