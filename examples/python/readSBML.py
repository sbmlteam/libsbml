#!/usr/bin/env python
## 
## @file    readSBML.py
## @brief   Similar to validateSBML, but without the validation
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

def main (args):
  """Usage: readSBML filename
  """
  
  if (len(args) != 2):
      print("Usage: readSBML filename");
      return 1;
  
  filename = args[1];
  current = time.clock();
  document = readSBML(filename);
  
  errors = document.getNumErrors();
  
  print;
  print("            filename: " + filename);
  print("           file size: " + str(os.stat(filename).st_size));
  print("      read time (ms): " + str(time.clock() - current));
  print(" validation error(s): " + str(errors));
  print;
  
  document.printErrors();
  
  return errors;
    
if __name__ == '__main__':
  main(sys.argv)  
