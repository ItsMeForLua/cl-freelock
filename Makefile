.PHONY: help clean-tex clean-scripts clean-lwarp clean-docs clean-all-exclude-docs clean-all-include-docs n-tree lwarpmk limages find-html-deps compile-tex docs auto-run
.DEFAULT_GOAL := help

help:
	@echo "Available commands:"
	@echo "  make clean-tex                 Remove TeX build artifacts"
	@echo "  make clean-scripts             Remove script output artifacts"
	@echo "  make clean-lwarp               Remove lwarp artifacts"
	@echo "  make clean-docs                Remove docs/ directory"
	@echo "  make clean-all-exclude-docs    Clean all artifacts except docs/"
	@echo "  make clean-all-include-docs    Clean all artifacts including docs/"
	@echo "  make docs FILE=<name>          Build docs/ from <name>.html"
	@echo "  make lwarpmk FILE=<name>       Run lwarpmk html <name>"
	@echo "  make limages FILE=<name>       Run lwarpmk limages if <name>-images.txt exists"
	@echo "  make find-html-deps FILE=<name> List href/src dependencies from <name>.html"
	@echo "  make compile-tex FILE=<name>   Compile <name>.tex to PDF"
	@echo "  make auto-run FILE=<name>      Run full build pipeline"

clean-tex: 
	@echo "Cleaning directory of tex artifacts..."
	@rm -f *.aux *.bbl *.bcf *.blg *.log *.run.xml *.synctex.gz *.out *.toc
	@rm -f tex/*.aux tex/*.bbl tex/*.bcf tex/*.blg tex/*.log tex/*.run.xml tex/*.synctex.gz tex/*.out tex/*.toc

clean-scripts:
	@echo "Cleaning directory of scripts artifacts..."
	@rm -f *.txt *.json ~*
	@rm -f tex/*.txt tex/*.json tex/~*

clean-lwarp:
	@echo "Cleaning directory of lwarp artifacts..."
	@rm -f *.lwarpmkconf *_html.tex *.cut *.css *.ist *.conf *.xdy *_html.pdf *.sidetoc *_html.html
	@rm -f tex/*.lwarpmkconf tex/*_html.tex tex/*.cut tex/*.css tex/*.ist tex/*.conf tex/*.xdy tex/*_html.pdf tex/*.sidetoc tex/*_html.html

clean-docs:
	@echo "Cleaning docs/ completely..."
	@rm -rf docs/

clean-all-exclude-docs:
	@$(MAKE) clean-tex
	@$(MAKE) clean-scripts
	@$(MAKE) clean-lwarp
	@echo "Cleaned project of all artifacts excluding docs/..."

clean-all-include-docs:
	@$(MAKE) clean-tex
	@$(MAKE) clean-scripts
	@$(MAKE) clean-lwarp
	@$(MAKE) clean-docs
	@echo "Cleaned project of all artifacts including docs/..."

docs:
	@echo "FILE is currently set to: '$(FILE)'"; \
	if [ -z "$(FILE)" ]; then \
		echo "Warning: FILE is empty."; \
		echo "Expected usage: make docs FILE=File-Name-Without-Extension"; \
		exit 1; \
	fi; \
	printf "Proceed? [y/N] "; \
	read ans; \
	case "$$ans" in \
		[yY]|[yY][eE][sS]) ;; \
		*) echo "Aborted."; exit 1 ;; \
	esac
	@echo "Building docs/ directory..."
	@mkdir -p docs
	@cp "tex/$(FILE).html" docs/index.html
	@# 1. Copy manual core support files
	@cp tex/lwarp.css tex/lwarp_formal.css tex/lwarp_sagebrush.css tex/lwarp_mathjax.txt docs/ 2>/dev/null || true
	@# 2. Dynamically discover and copy local assets referenced in the HTML
	@grep -oE '(href|src)="[^"]+"' "tex/$(FILE).html" | \
		sed -n 's/.*="\([^/:][^"]*\)".*/\1/p' | \
		sort -u | \
		while read -r file; do \
			if [ -e "tex/$$file" ]; then \
				mkdir -p "docs/$$(dirname "$$file")"; \
				cp -r "tex/$$file" "docs/$$file"; \
				echo "Copied asset: tex/$$file to docs/$$file"; \
			fi; \
		done

lwarpmk:
	@echo "Running lwarpmk html $(FILE)"
	@cd tex && lwarpmk html "$(FILE)"

limages:
	@echo "Running lwarpmk limages for $(FILE)..."
	@if [ -f "tex/$(FILE)-images.txt" ]; then \
		cd tex && lwarpmk limages "$(FILE)"; \
	else \
		echo "No $(FILE)-images.txt found; skipping limages."; \
	fi

find-html-deps:
	@grep -oE '(href|src)="[^"]+"' 'tex/$(FILE).html'

compile-tex:
	@echo "Compiling $(FILE).tex into $(FILE).pdf..."
	@cd tex && pdflatex "$(FILE).tex"

auto-run:
	@test -n "$(FILE)" || (echo "Usage: make auto-run FILE=YourFile" >&2; exit 1)
	@$(MAKE) clean-all-include-docs FILE="$(FILE)"
	@$(MAKE) compile-tex FILE="$(FILE)"
	@$(MAKE) lwarpmk FILE="$(FILE)"
	@$(MAKE) limages FILE="$(FILE)"
	@$(MAKE) docs FILE="$(FILE)"
	@$(MAKE) clean-all-exclude-docs FILE="$(FILE)"
	@echo "FILE=$(FILE): completed."