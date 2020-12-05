## \file InputFormat.py
# \author Andrea Clemeno
# \brief Provides the function for reading inputs
## \brief Reads input from a file with the given file name
# \param filename name of the input file
# \param inParams structure holding the input values
def get_input(filename, inParams):
    outfile = open("log.txt", "a")
    print("function get_input called with inputs: {", file=outfile)
    print("  filename = ", end="", file=outfile)
    print(filename, end="", file=outfile)
    print(", ", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    infile = open(filename, "r")
    infile.readline()
    inParams.N_o = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.N_o' assigned ", end="", file=outfile)
    print(inParams.N_o, end="", file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.N_t = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.N_t' assigned ", end="", file=outfile)
    print(inParams.N_t, end="", file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.t = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.t' assigned ", end="", file=outfile)
    print(inParams.t, end="", file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.close()
