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
R_SCRIPT ?= Rscript
APPEND ?= nil
CSV_FILE ?= benchmark_results.csv

.PHONY: all deps clean test test-local benchmark benchmark-st benchmark-all install-dev graphs help

all: deps

deps:
	@if [ -z "$(QLOT)" ]; then \
		echo "[ERROR] qlot not found. Please install it (e.g., 'ros install qlot')."; \
		exit 1; \
	fi
	@echo "--> Installing Lisp dependencies with qlot..."
	@$(QLOT) install

install-dev:
	@echo "--> Installing to local-projects..."
	@mkdir -p $(LOCAL_PROJECTS_DIR)
	@if [ -L $(LOCAL_PROJECTS_DIR)/cl-freelock ]; then \
		echo "Removing existing symlink..."; \
		rm $(LOCAL_PROJECTS_DIR)/cl-freelock; \
	fi
	@ln -sf $(PWD) $(LOCAL_PROJECTS_DIR)/cl-freelock
	@echo "[OK] Symlinked to $(LOCAL_PROJECTS_DIR)/cl-freelock"


test: deps
	@echo "--> Running tests with qlot..."
	@$(QLOT) exec $(LISP) --non-interactive \
		--eval '(asdf:test-system :cl-freelock-tests)' \
		--eval '(uiop:quit)'

test-local:
	@echo "--> Running tests with local Roswell environment (no qlot)..."
	@$(LISP) --non-interactive \
		--eval '(ql:quickload :cl-freelock-tests)' \
		--eval '(asdf:test-system :cl-freelock-tests)' \
		--eval '(uiop:quit)'


# Conditionally adds :log-file parameter if CSV_LOG is set.
define run_benchmark
	@$(QLOT) exec $(LISP) --non-interactive \
		$(1) \
		--eval '(ql:quickload :cl-freelock-benchmarks)' \
		--eval '(cl-freelock-benchmarks:run-all-benchmarks $(if $(CSV_LOG),:log-file "$(CSV_LOG)" :append $(APPEND),))' \
		--eval '(uiop:quit)'
endef

benchmark: deps
	@echo "--> Running benchmark suite (multi-threaded default)..."
	@if [ -n "$(CSV_LOG)" ]; then echo "Saving results to $(CSV_LOG)"; fi
	@$(call run_benchmark)

benchmark-st: deps
	@echo "--> Running benchmark suite (single-threaded optimized)..."
	@if [ -n "$(CSV_LOG)" ]; then echo "Saving results to $(CSV_LOG)"; fi
	@$(call run_benchmark, --eval '(push :cl-freelock-single-threaded *features*)')

benchmark-all: deps
	@echo "--> Running ALL benchmark suites..."
	@if [ -z "$(CSV_LOG)" ]; then \
		echo "[ERROR] Must specify CSV_LOG for benchmark-all. Example: make benchmark-all CSV_LOG=results.csv"; \
		exit 1; \
	fi
	@# Run the first benchmark, creating the file (APPEND defaults to nil)
	@$(MAKE) benchmark CSV_LOG=$(CSV_LOG)
	@# Run the second benchmark, explicitly setting APPEND to t for the Lisp function
	@$(MAKE) benchmark-st CSV_LOG=$(CSV_LOG) APPEND=t
	@echo "--> All benchmark data saved to $(CSV_LOG)"

graphs:
	@echo "--> Generating graphs from $(CSV_FILE)..."
	@if ! [ -f "$(CSV_FILE)" ]; then \
		echo "[ERROR] $(CSV_FILE) not found."; \
		echo "Please run 'make benchmark CSV_LOG=$(CSV_FILE)' or 'make benchmark-all CSV_LOG=$(CSV_FILE)' first."; \
		exit 1; \
	fi
	@if command -v Rscript &> /dev/null; then \
		Rscript analyze_benchmarks.r; \
	elif command -v R &> /dev/null; then \
		echo "--> 'Rscript' not found. Using 'R' as a fallback."; \
		R --vanilla --quiet -e "source('analyze_benchmarks.r')"; \
	else \
		echo "[ERROR] Neither 'Rscript' nor 'R' command found."; \
		echo "Please install R. On Arch Linux, use: sudo pacman -S r"; \
		exit 1; \
	fi

clean:
	@echo "--> Cleaning build artifacts..."
	@rm -rf .qlot/
	@rm -rf graphs/
	@rm -f benchmark_results.csv
	@find . -name "*.fasl" -type f -delete
	@echo "Clean complete."

help:
	@echo "cl-freelock Build System"
	@echo ""
	@echo "== Common Commands =="
	@echo "  make deps                  - Install dependencies using qlot."
	@echo "  make test                  - Run tests in a reproducible qlot environment."
	@echo "  make test-local            - Run tests with system Lisp (no qlot)."
	@echo "  make benchmark             - Run default benchmarks. Add CSV_LOG=file.csv to save results."
	@echo "  make benchmark-st          - Run single-threaded benchmarks. Add CSV_LOG=file.csv to save."
	@echo "  make benchmark-all         - Run all benchmarks and save to a file (CSV_LOG is required)."
	@echo "  make graphs                - Generate plots from a CSV file (e.g., make graphs CSV_FILE=results.csv)."
	@echo "  make install-dev           - Symlink the project to your local-projects directory."
	@echo "  make clean                 - Clean all build artifacts."
