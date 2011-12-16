#!/usr/bin/env python
## 
## @file    printRegisteredPackages.py
## @brief   Prints the registerd packages for this libSBML
## @author  Frank Bergmann
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

import sys
import os.path
from libsbml import *

print("This version of LibSBML: " + getLibSBMLDottedVersion() + " includes: " + "\n");

for i in range (0, SBMLExtensionRegistry.getNumRegisteredPackages()):
    print("\t" + SBMLExtensionRegistry.getRegisteredPackageName(i));
print("\n");

