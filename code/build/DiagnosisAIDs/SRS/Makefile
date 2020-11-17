srs: DiagnosisAIDs_SRS.pdf

DiagnosisAIDs_SRS.pdf: DiagnosisAIDs_SRS.tex
	lualatex $(TEXFLAGS) DiagnosisAIDs_SRS
	-bibtex $(BIBTEXFLAGS) DiagnosisAIDs_SRS
	lualatex $(TEXFLAGS) DiagnosisAIDs_SRS
	lualatex $(TEXFLAGS) DiagnosisAIDs_SRS

.PHONY: srs
