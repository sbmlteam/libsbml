#!/usr/bin/env python
## 
## @file    printModel.py
## @brief   Prints some information about the top-level model
## @author  Sarah Keating
## @author  Ben Bornstein
## @author  Michael Hucka
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

import sys
import os.path
from libsbml import *

def main (args):
  """Usage: printNotes filename
  """
  
  
  if (len(args) != 2):
      print("\n" + "Usage: printSBML filename" + "\n" + "\n");
      return 1;
  
  filename = args[1];
  document = readSBML(filename);
  
  if (document.getNumErrors() > 0):
      printLine("Encountered the following SBML errors:" + "\n");
      document.printErrors();
      return 1;
  
  level = document.getLevel();
  version = document.getVersion();
  
  print("\n"
                        + "File: " + filename
                        + " (Level " + str(level) + ", version " + str(version) + ")" + "\n");
  
  model = document.getModel();
  
  if (model == None):
      print("No model present." + "\n");
      return 1;
  
  idString = "id"
  if (level == 1):
	idString = "name"
  id = "(empty)"
  if (model.isSetId()):
	id = model.getId()
  print("               "
                        + idString
                        + id + "\n");
  
  if (model.isSetSBOTerm()):
      print("      model sboTerm: " + model.getSBOTerm() + "\n");
  
  print("functionDefinitions: " + str(model.getNumFunctionDefinitions()) + "\n");
  print("    unitDefinitions: " + str(model.getNumUnitDefinitions()) + "\n");
  print("   compartmentTypes: " + str(model.getNumCompartmentTypes()) + "\n");
  print("        specieTypes: " + str(model.getNumSpeciesTypes()) + "\n");
  print("       compartments: " + str(model.getNumCompartments()) + "\n");
  print("            species: " + str(model.getNumSpecies()) + "\n");
  print("         parameters: " + str(model.getNumParameters()) + "\n");
  print(" initialAssignments: " + str(model.getNumInitialAssignments()) + "\n");
  print("              rules: " + str(model.getNumRules()) + "\n");
  print("        constraints: " + str(model.getNumConstraints()) + "\n");
  print("          reactions: " + str(model.getNumReactions()) + "\n");
  print("             events: " + str(model.getNumEvents()) + "\n");
  print("\n");
  
  return 0;
 
if __name__ == '__main__':
  main(sys.argv)  
