## \file Calculations.py
# \author Andrea Clemeno
# \brief Provides functions for calculating the outputs
import math

## \brief Calculates elimination constant (d^-1)
# \param inParams structure holding the input values
# \return elimination constant (d^-1)
def func_k(inParams):
    outfile = open("log.txt", "a")
    print("function func_k called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return (math.log(inParams.N_o) - math.log(inParams.N_t)) / inParams.t_t

## \brief Calculates predicted viral load at time t (mol/mL)
# \param inParams structure holding the input values
# \param k elimination constant (d^-1)
# \return predicted viral load at time t (mol/mL)
def func_N_p(inParams, k):
    outfile = open("log.txt", "a")
    print("function func_N_p called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  k = ", end="", file=outfile)
    print(k, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return inParams.N_o * math.exp(-k * inParams.t_p)
