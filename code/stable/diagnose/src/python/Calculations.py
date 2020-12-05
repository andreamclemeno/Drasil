## \file Calculations.py
# \author Andrea Clemeno
# \brief Provides functions for calculating the outputs
import math

## \brief Calculates elimination constant (s^-1)
# \param inParams structure holding the input values
# \return elimination constant (s^-1)
def func_λ(inParams):
    outfile = open("log.txt", "a")
    print("function func_λ called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return (math.log(inParams.N_o) - math.log(inParams.N_t)) / inParams.t

## \brief Calculates predicted viral load after 30 days (mol/mL)
# \param inParams structure holding the input values
# \param λ elimination constant (s^-1)
# \return predicted viral load after 30 days (mol/mL)
def func_N_p(inParams, λ):
    outfile = open("log.txt", "a")
    print("function func_N_p called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  λ = ", end="", file=outfile)
    print(λ, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return inParams.N_o * math.exp(-λ * inParams.t)
