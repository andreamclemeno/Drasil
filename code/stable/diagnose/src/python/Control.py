## \file Control.py
# \author Andrea Clemeno
# \brief Controls the flow of the program
import sys

import Calculations
import InputConstraints
import InputFormat
import InputParameters
import OutputFormat

filename = sys.argv[1]
outfile = open("log.txt", "a")
print("var 'filename' assigned ", end="", file=outfile)
print(filename, end="", file=outfile)
print(" in module Control", file=outfile)
outfile.close()
inParams = InputParameters.InputParameters()
InputFormat.get_input(filename, inParams)
InputConstraints.input_constraints(inParams)
k = Calculations.func_k(inParams)
outfile = open("log.txt", "a")
print("var 'k' assigned ", end="", file=outfile)
print(k, end="", file=outfile)
print(" in module Control", file=outfile)
outfile.close()
N_p = Calculations.func_N_p(inParams, k)
outfile = open("log.txt", "a")
print("var 'N_p' assigned ", end="", file=outfile)
print(N_p, end="", file=outfile)
print(" in module Control", file=outfile)
outfile.close()
OutputFormat.write_output(k, N_p)
