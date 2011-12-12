#!/usr/bin/env python
## 
## @file    printsupported.cs
## @brief   Prints all SBML Levels and Versions supported by this version 
##          of libsbml.
## @author  Frank Bergmann
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

import sys
import os.path
from libsbml import *

print("Supported by LibSBML " + getLibSBMLDottedVersion());
print;

supported = SBMLNamespaces.getSupportedNamespaces();
for i in range(0,supported.getSize()):
	current = supported.get(i);
	print("\tSBML Level: " + str(current.getLevel()) + 
					" Version: " + str(current.getVersion()));
print;



