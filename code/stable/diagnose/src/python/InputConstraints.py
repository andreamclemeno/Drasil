## \file InputConstraints.py
# \author Andrea Clemeno
# \brief Provides the function for checking the physical constraints on the input
## \brief Verifies that input values satisfy the physical constraints
# \param inParams structure holding the input values
def input_constraints(inParams):
    outfile = open("log.txt", "a")
    print("function input_constraints called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    if (not(inParams.N_o > 0)) :
        print("N_o has value ", end="")
        print(inParams.N_o, end="")
        print(", but is expected to be ", end="")
        print("above ", end="")
        print(0, end="")
        print(".")
        raise Exception("InputError")
    if (not(inParams.N_t > 0)) :
        print("N_t has value ", end="")
        print(inParams.N_t, end="")
        print(", but is expected to be ", end="")
        print("above ", end="")
        print(0, end="")
        print(".")
        raise Exception("InputError")
    if (not(inParams.t > 0)) :
        print("t has value ", end="")
        print(inParams.t, end="")
        print(", but is expected to be ", end="")
        print("above ", end="")
        print(0, end="")
        print(".")
        raise Exception("InputError")
