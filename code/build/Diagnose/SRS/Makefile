srs: Diagnose_SRS.pdf

Diagnose_SRS.pdf: Diagnose_SRS.tex
	lualatex $(TEXFLAGS) Diagnose_SRS
	-bibtex $(BIBTEXFLAGS) Diagnose_SRS
	lualatex $(TEXFLAGS) Diagnose_SRS
	lualatex $(TEXFLAGS) Diagnose_SRS

.PHONY: srs
