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
    if (not(0 < inParams.N_t and inParams.N_t < inParams.N_o)) :
        print("N_t has value ", end="")
        print(inParams.N_t, end="")
        print(", but is expected to be ", end="")
        print("between ", end="")
        print(0, end="")
        print(" and ", end="")
        print(inParams.N_o, end="")
        print(" (N_o)", end="")
        print(".")
        raise Exception("InputError")
    if (not(0 < inParams.t_t and inParams.t_t < inParams.t_p)) :
        print("t_t has value ", end="")
        print(inParams.t_t, end="")
        print(", but is expected to be ", end="")
        print("between ", end="")
        print(0, end="")
        print(" and ", end="")
        print(inParams.t_p, end="")
        print(" (t_p)", end="")
        print(".")
        raise Exception("InputError")
    if (not(inParams.t_p > 0)) :
        print("t_p has value ", end="")
        print(inParams.t_p, end="")
        print(", but is expected to be ", end="")
        print("above ", end="")
        print(0, end="")
        print(".")
        raise Exception("InputError")
