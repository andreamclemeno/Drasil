## \file InputFormat.py
# \author Nikitha Krithnan and W. Spencer Smith
# \brief Provides the function for reading inputs
## \brief Reads input from a file with the given file name
# \param inParams structure holding the input values
# \param filename name of the input file
def get_input(inParams, filename):
    outfile = open("log.txt", "a")
    print("function get_input called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", end='', file=outfile)
    print(", ", file=outfile)
    print("  filename = ", end='', file=outfile)
    print(filename, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    infile = open(filename, "r")
    infile.readline()
    inParams.a = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.a' assigned to ", end='', file=outfile)
    print(inParams.a, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.b = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.b' assigned to ", end='', file=outfile)
    print(inParams.b, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.w = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.w' assigned to ", end='', file=outfile)
    print(inParams.w, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.P_btol = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.P_btol' assigned to ", end='', file=outfile)
    print(inParams.P_btol, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.TNT = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.TNT' assigned to ", end='', file=outfile)
    print(inParams.TNT, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.g = infile.readline().rstrip()
    outfile = open("log.txt", "a")
    print("var 'inParams.g' assigned to ", end='', file=outfile)
    print(inParams.g, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.t = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.t' assigned to ", end='', file=outfile)
    print(inParams.t, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.SD_x = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.SD_x' assigned to ", end='', file=outfile)
    print(inParams.SD_x, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.SD_y = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.SD_y' assigned to ", end='', file=outfile)
    print(inParams.SD_y, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.readline()
    inParams.SD_z = float(infile.readline())
    outfile = open("log.txt", "a")
    print("var 'inParams.SD_z' assigned to ", end='', file=outfile)
    print(inParams.SD_z, end='', file=outfile)
    print(" in module InputFormat", file=outfile)
    outfile.close()
    infile.close()
