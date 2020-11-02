all: formulation.pdf formulation.docx

clean:
	rm -f formulation.pdf
	rm -f formulation.docx

formulation.pdf: formulation.Rmd
	R -e "rmarkdown::render('formulation.Rmd', clean = TRUE)"
	rm -f formulation.md
	rm -f formulation.tex
	rm -f formulation.log

formulation.docx: formulation.Rmd
	R -e "rmarkdown::render('formulation.Rmd', output_file='formulation2.tex', clean = F)"
	rm -f formulation2.md
	rm -f formulation2.aux
	rm -f formulation2.bbl
	pandoc +RTS -K512m -RTS formulation2.tex -o formulation.docx --highlight-style tango --latex-engine pdflatex --include-in-header preamble.tex --variable graphics=yes --variable 'geometry:margin=1in' --filter /usr/bin/pandoc-citeproc
	rm -f formulation2.log
	rm -f formulation2.tex
	rm -f formulation.utf8.md
	rm -f formulation.knit.md

.PHONY: all clean
