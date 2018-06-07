file = ROI.plugin.neos

all: Stangle knitr latex
	rm -f $(file).aux $(file).log $(file).bbl $(file).blg $(file).out ##$(file).tex
	cp ROI.plugin.neos.tex ../vignettes/ROI.plugin.neos_Introduction.Rnw

## all: Stangle Sweave latex clean

latex: ${file}.tex neos.bib
	pdflatex ${file}.tex
	bibtex ${file}
	bibtex ${file}
	pdflatex ${file}.tex
	pdflatex ${file}.tex

knitr:
	R -e 'library(knitr); opts_knit$$set(out.format = "latex"); x=knit_theme$$get("edit-emacs"); x$$background = "#f4f3ef"; knit_theme$$set(x); knit2pdf("$(file).Rnw")'

Sweave:
	R CMD Sweave ${file}.Rnw | echo

Stangle:
	R CMD Stangle ${file}.Rnw | echo

clean:
	rm -f $(file).aux $(file).log $(file).bbl $(file).blg $(file).out $(file).tex

