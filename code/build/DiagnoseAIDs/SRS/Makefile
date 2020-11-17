srs: DiagnoseAIDs_SRS.pdf

DiagnoseAIDs_SRS.pdf: DiagnoseAIDs_SRS.tex
	lualatex $(TEXFLAGS) DiagnoseAIDs_SRS
	-bibtex $(BIBTEXFLAGS) DiagnoseAIDs_SRS
	lualatex $(TEXFLAGS) DiagnoseAIDs_SRS
	lualatex $(TEXFLAGS) DiagnoseAIDs_SRS

.PHONY: srs
