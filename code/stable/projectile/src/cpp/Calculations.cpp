#include "Calculations.hpp"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <limits>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

#include "InputParameters.hpp"

double func_t_flight(InputParameters &inParams) {
    return ((2 * (inParams.v_launch * sin(inParams.angle))) / 9.8);
}

double func_p_land(InputParameters &inParams) {
    return ((2 * (pow(inParams.v_launch, 2) * (sin(inParams.angle) * cos(inParams.angle)))) / 9.8);
}

double func_d_offset(InputParameters &inParams, double p_land) {
    return (p_land - inParams.p_target);
}

string func_s(InputParameters &inParams, double d_offset) {
    if ((fabs((d_offset / inParams.p_target)) < 2.0e-2)) {
        return "The target was hit.";
    }
    else if ((d_offset < 0)) {
        return "The projectile fell short.";
    }
    else if ((d_offset > 0)) {
        return "The projectile went long.";
    }
    else {
        throw("Undefined case encountered in function func_s");
    }
}

