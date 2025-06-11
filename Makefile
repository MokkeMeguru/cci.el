# Makefile for claude-code-ide.el

# Configuration
EMACS ?= emacs
PACKAGE_NAME = claude-code-ide
MAIN_FILE = $(PACKAGE_NAME).el
MODULE_FILES = claude-code-ide-utils.el claude-code-ide-session.el claude-code-ide-mcp.el
ALL_EL_FILES = $(MAIN_FILE) $(MODULE_FILES)
TEST_FILES = $(wildcard test/*.el)

# Build targets
.PHONY: all compile checkdoc lint test clean install help

# Default target
all: compile checkdoc

# Compile all Emacs Lisp files
compile:
	@echo "Compiling Emacs Lisp files..."
	@$(EMACS) -Q --batch \
		--eval "(setq byte-compile-error-on-warn nil)" \
		--eval "(setq load-path (cons \".\" load-path))" \
		--eval "(batch-byte-compile)" \
		claude-code-ide-utils.el
	@$(EMACS) -Q --batch \
		--eval "(setq byte-compile-error-on-warn nil)" \
		--eval "(setq load-path (cons \".\" load-path))" \
		--eval "(batch-byte-compile)" \
		claude-code-ide-session.el claude-code-ide-mcp.el
	@$(EMACS) -Q --batch \
		--eval "(setq byte-compile-error-on-warn nil)" \
		--eval "(setq load-path (cons \".\" load-path))" \
		--eval "(batch-byte-compile)" \
		$(MAIN_FILE)

# Run checkdoc on all files
checkdoc:
	@echo "Running checkdoc..."
	@$(EMACS) -Q --batch \
		--eval "(require 'checkdoc)" \
		--eval "(setq checkdoc-verbose t)" \
		--eval "(checkdoc-file \"$(MAIN_FILE)\")" \
		$(foreach file,$(MODULE_FILES),--eval "(checkdoc-file \"$(file)\")")

# Lint with package-lint if available
lint: checkdoc
	@echo "Running package-lint..."
	@if $(EMACS) -Q --batch --eval "(require 'package-lint nil t)" 2>/dev/null; then \
		$(EMACS) -Q --batch \
			--eval "(require 'package-lint)" \
			--eval "(package-lint-batch-and-exit)" \
			$(MAIN_FILE); \
	else \
		echo "package-lint not available, skipping..."; \
	fi

# Run tests
test:
	@echo "Running tests..."
	@if [ -n "$(TEST_FILES)" ]; then \
		$(EMACS) -Q --batch \
			--eval "(add-to-list 'load-path \".\")" \
			$(foreach file,$(TEST_FILES),--load $(file)) \
			--eval "(ert-run-tests-batch-and-exit)"; \
	else \
		echo "No test files found."; \
	fi

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@rm -f *.elc test/*.elc

# Install package locally
install: compile
	@echo "Installing $(PACKAGE_NAME) locally..."
	@$(EMACS) --batch \
		--eval "(package-initialize)" \
		--eval "(package-install-file \"$(MAIN_FILE)\")"

# Show help
help:
	@echo "Available targets:"
	@echo "  all       - Compile and run checkdoc (default)"
	@echo "  compile   - Byte-compile all .el files"
	@echo "  checkdoc  - Run checkdoc on all files"
	@echo "  lint      - Run checkdoc and package-lint"
	@echo "  test      - Run ERT tests"
	@echo "  clean     - Remove compiled .elc files"
	@echo "  install   - Install package locally"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Variables:"
	@echo "  EMACS     - Emacs executable (default: emacs)"
	@echo ""
	@echo "Example usage:"
	@echo "  make EMACS=/usr/local/bin/emacs compile"