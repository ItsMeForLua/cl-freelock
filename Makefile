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
	@rm -f tex/*.lwarpmkconf tex/*_html.tex tex/*.cut tex/*.listing tex/.log tex/*.css tex/*.ist tex/*.conf tex/*.xdy tex/*_html.pdf tex/*.sidetoc tex/*_html.html

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
	@cp tex/*.html docs/ 2>/dev/null || true
	@cp tex/lwarp.css tex/lwarp_formal.css tex/lwarp_sagebrush.css tex/lwarp_mathjax.txt docs/ 2>/dev/null || true
	@grep -oE '(href|src)="[^"]+"' tex/*.html | \
		sed -n 's/.*="\([^/:][^"]*\)".*/\1/p' | \
		sort -u | \
		while read -r file; do \
			if [ -e "tex/$$file" ]; then \
				mkdir -p "docs/$$(dirname "$$file")"; \
				cp -r "tex/$$file" "docs/$$file"; \
				echo "Copied asset: tex/$$file to docs/$$file"; \
			fi; \
		done

# I can't figure out how lwarp handles colored text, so I'm just gonna inject the lwarp.css file post compile for now.
# Disclaimer: I used AI to help speed up the creation of this updated target. I did not feel like writing all that. Front end is not my thing.
# I'm also curious how emacs eww would render our html.
lwarpmk:
	@echo "Running lwarpmk html $(FILE)"
	@cd tex && lwarpmk html "$(FILE)"
	@echo "Injecting custom structural CSS into lwarp.css..."
	@echo "/* --- CUSTOM CODE BLOCK STRUCTURE --- */" >> tex/lwarp.css
	@echo "pre.programlisting { background-color: #f0f0f0; border: 1.5pt solid black; padding: 10px; margin-bottom: 1.5em; overflow-x: auto; white-space: pre-wrap; font-family: monospace; }" >> tex/lwarp.css
	@echo "Injecting Highlight.js into all HTML files..."
	@for html_file in tex/*.html; do \
		echo '<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/stackoverflow-light.min.css">' >> "$$html_file"; \
		echo '<style>.hljs { background: transparent !important; padding: 0 !important; }</style>' >> "$$html_file"; \
		echo '<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>' >> "$$html_file"; \
		echo '<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/lisp.min.js"></script>' >> "$$html_file"; \
		echo '<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/bash.min.js"></script>' >> "$$html_file"; \
		echo '<script>' >> "$$html_file"; \
		echo 'document.querySelectorAll("pre.programlisting").forEach(el => {' >> "$$html_file"; \
		echo '  const rawText = el.textContent;' >> "$$html_file"; \
		echo '  const code = document.createElement("code");' >> "$$html_file"; \
		echo '  // Default to Lisp, only use Bash for actual terminal commands' >> "$$html_file"; \
		echo '  code.className = (rawText.includes("git clone") || rawText.includes("cd ")) ? "language-bash" : "language-lisp";' >> "$$html_file"; \
		echo '  code.textContent = rawText;' >> "$$html_file"; \
		echo '  el.innerHTML = "";' >> "$$html_file"; \
		echo '  el.appendChild(code);' >> "$$html_file"; \
		echo '  hljs.highlightElement(code);' >> "$$html_file"; \
		echo '});' >> "$$html_file"; \
		echo '</script>' >> "$$html_file"; \
	done

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
	@rm -rf tex/docs 2>/dev/null || true
	@rm -rf tex/imgs 2>/dev/null || true
	@rm $(FILE).pdf 2>/dev/null || true
	@echo "FILE=$(FILE): completed."