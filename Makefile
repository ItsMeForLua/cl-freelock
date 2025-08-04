# Auto-detect Lisp environment, with fallback for local installation
LOCAL_PROJECTS_DIR ?= $(shell \
    if [ -d ~/.roswell/local-projects ]; then \
        echo ~/.roswell/local-projects; \
    elif [ -d ~/quicklisp/local-projects ]; then \
        echo ~/quicklisp/local-projects; \
    else \
        echo ~/.roswell/local-projects; \
    fi)

LISP ?= ros run
QLOT ?= $(shell which qlot || if [ -f "$(HOME)/.roswell/bin/qlot" ]; then echo "$(HOME)/.roswell/bin/qlot"; else echo ""; fi)

.PHONY: all deps clean test test-local benchmark benchmark-st install-dev help

all: deps

deps:
	@if [ -z "$(QLOT)" ]; then \
		echo "[ERROR] qlot not found. Please install it (e.g., 'ros install qlot')."; \
		exit 1; \
	fi
	@echo "--> Installing Lisp dependencies with qlot..."
	@$(QLOT) install

# Run tests with qlot
test: deps
	@echo "--> Running tests with qlot..."
	@$(QLOT) exec $(LISP) --non-interactive \
		--eval '(asdf:test-system :cl-freelock-tests)' \
		--eval '(uiop:quit)'

# Run tests with the system's Lisp (no qlot)
test-local:
	@echo "--> Running tests with local Roswell environment..."
	@$(LISP) --non-interactive \
		--eval '(ql:quickload :cl-freelock-tests)' \
		--eval '(asdf:test-system :cl-freelock-tests)' \
		--eval '(uiop:quit)'

benchmark: deps
	@echo "--> Running benchmark suite (multi-threaded default)..."
	@$(QLOT) exec $(LISP) --non-interactive \
		--eval '(ql:quickload :cl-freelock-benchmarks)' \
		--eval '(cl-freelock-benchmarks:run-all-benchmarks)' \
		--eval '(uiop:quit)'

# I love threads
benchmark-st: deps
	@echo "--> Running benchmark suite (single-threaded optimized)..."
	@$(QLOT) exec $(LISP) --non-interactive \
		--eval '(push :cl-freelock-single-threaded *features*)' \
		--eval '(ql:quickload :cl-freelock-benchmarks)' \
		--eval '(cl-freelock-benchmarks:run-all-benchmarks)' \
		--eval '(uiop:quit)'

# Install to local-projects for development by creating a symlink
install-dev:
	@echo "--> Installing to local-projects..."
	@mkdir -p $(LOCAL_PROJECTS_DIR)
	@if [ -L $(LOCAL_PROJECTS_DIR)/cl-freelock ]; then \
		echo "Removing existing symlink..."; \
		rm $(LOCAL_PROJECTS_DIR)/cl-freelock; \
	fi
	@ln -sf $(PWD) $(LOCAL_PROJECTS_DIR)/cl-freelock
	@echo "[OK] Symlinked to $(LOCAL_PROJECTS_DIR)/cl-freelock"

clean:
	@echo "--> Cleaning build artifacts..."
	@rm -rf .qlot/
	@find . -name "*.fasl" -type f -delete
	@echo "Clean complete."

help:
	@echo "cl-freelock Build System"
	@echo ""
	@echo "== Common Commands =="
	@echo "  make deps          - Install dependencies using qlot."
	@echo "  make test          - Run tests in a reproducible qlot environment."
	@echo "  make benchmark     - Run the benchmarks with multi-threaded optimizations."
	@echo "  make benchmark-st  - Run the benchmarks with single-threaded optimizations."
	@echo "  make install-dev   - Symlink the project to your local-projects directory."
	@echo "  make clean         - Clean all build artifacts."
