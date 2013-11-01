#!/usr/bin/env python
## 
## @file    translateMath.py
## @brief   Translates infix formulas into MathML and vice-versa
## @author  Sarah Keating
## @author  Ben Bornstein
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

import sys
import time
import os
import os.path
from libsbml import *

#
#Translates the given infix formula into MathML.
#
#@return the MathML as a string.  The caller owns the memory and is
#responsible for freeing it.
#
def translateInfix(formula):
    math = parseFormula(formula);
    return writeMathMLToString(math);

# 
# Translates the given MathML into an infix formula.  The MathML must
# contain no leading whitespace, but an XML header is optional.
# 
# @return the infix formula as a string.  The caller owns the memory and
# is responsible for freeing it.
# 
def translateMathML(xml):
    math = readMathMLFromString(xml);
    return formulaToString(math);

def main (args):
  """Usage: readSBML filename
  """

  
  print("This program translates infix formulas into MathML and");
  print("vice-versa.  Enter or return on an empty line triggers");
  print("translation. Ctrl-C quits");

  sb = ""  
  try:
    while True:
        print("Enter infix formula or MathML expression (Ctrl-C to quit):");
        print "> ",
    
        line = sys.stdin.readline()
        while line != None:
            trimmed = line.strip();
            length = len(trimmed);
            if (length > 0):
                sb = sb + trimmed;
            else:
                str = sb;
                result = ""
                if (str[0] == '<'):
	    			result = translateMathML(str)
                else:
	    		    result =  translateInfix(str)
    
                print("Result:\n\n" + result + "\n\n");
                sb = "";
                break;
    
            line = sys.stdin.readline()
  except: 
	return 0;
  return 0;

if __name__ == '__main__':
  main(sys.argv)  
