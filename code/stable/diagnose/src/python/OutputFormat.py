## \file OutputFormat.py
# \author Andrea Clemeno
# \brief Provides the function for writing outputs
## \brief Writes the output values to output.txt
# \param λ elimination constant (d^-1)
# \param N_p predicted viral load after 30 days (mol/mL)
def write_output(λ, N_p):
    outfile = open("log.txt", "a")
    print("function write_output called with inputs: {", file=outfile)
    print("  λ = ", end="", file=outfile)
    print(λ, end="", file=outfile)
    print(", ", file=outfile)
    print("  N_p = ", end="", file=outfile)
    print(N_p, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    outputfile = open("output.txt", "w")
    print("λ = ", end="", file=outputfile)
    print(λ, file=outputfile)
    print("N_p = ", end="", file=outputfile)
    print(N_p, file=outputfile)
    outputfile.close()
