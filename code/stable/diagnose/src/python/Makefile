build:

run: build
	python Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py InputFormat.py InputConstraints.py Calculations.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
