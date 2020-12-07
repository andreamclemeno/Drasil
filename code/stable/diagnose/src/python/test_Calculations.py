#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec  7 02:29:07 2020

@author: bodhi
"""

import math
import unittest
import Calculations
import InputParameters

inParams = InputParameters.InputParameters()
filename = "input.txt"

infile = open(filename, "r")
infile.readline()
inParams.N_o = float(infile.readline())
    
infile.readline()
inParams.N_t = float(infile.readline())
    
infile.readline()
inParams.t_t = float(infile.readline())
    
infile.readline()
inParams.t_p = float(infile.readline())
infile.close()

k = Calculations.func_k(inParams)

class TestCalculations(unittest.TestCase):
    
    def test_func_k(self):
        result = Calculations.func_k(inParams)
        self.assertEqual(int(result), int(math.log(inParams.N_o) - math.log(inParams.N_t)) / inParams.t_t)
   
    def test_func_N_p(self):
        result = Calculations.func_N_p(inParams,k)
        self.assertEqual(int(result), int(inParams.N_o * math.exp(-k * inParams.t_p)))
    
print('complete')
        
