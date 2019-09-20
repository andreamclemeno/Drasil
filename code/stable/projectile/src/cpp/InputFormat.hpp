/** \file InputFormat.hpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for reading inputs
*/
#ifndef InputFormat_h
#define InputFormat_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

#include "InputParameters.hpp"

/** \brief Reads input from a file with the given file name
    \param inParams structure holding the input values
    \param filename name of the input file
*/
void get_input(InputParameters &inParams, string filename);

#endif
