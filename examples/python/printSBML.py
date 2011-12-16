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
      print("\n" + "Usage: printSBML filename"  );
      return 1;
  
  filename = args[1];
  document = readSBML(filename);
  
  if (document.getNumErrors() > 0):
      printLine("Encountered the following SBML errors:" );
      document.printErrors();
      return 1;
  
  level = document.getLevel();
  version = document.getVersion();
  
  print("\n"
                        + "File: " + filename
                        + " (Level " + str(level) + ", version " + str(version) + ")" );
  
  model = document.getModel();
  
  if (model == None):
      print("No model present." );
      return 1;
  
  idString = "  id: "
  if (level == 1):
	idString = "name: "
  id = "(empty)"
  if (model.isSetId()):
	id = model.getId()
  print("               "
                        + idString
                        + id );
  
  if (model.isSetSBOTerm()):
      print("      model sboTerm: " + model.getSBOTerm() );
  
  print("functionDefinitions: " + str(model.getNumFunctionDefinitions()) );
  print("    unitDefinitions: " + str(model.getNumUnitDefinitions()) );
  print("   compartmentTypes: " + str(model.getNumCompartmentTypes()) );
  print("        specieTypes: " + str(model.getNumSpeciesTypes()) );
  print("       compartments: " + str(model.getNumCompartments()) );
  print("            species: " + str(model.getNumSpecies()) );
  print("         parameters: " + str(model.getNumParameters()) );
  print(" initialAssignments: " + str(model.getNumInitialAssignments()) );
  print("              rules: " + str(model.getNumRules()) );
  print("        constraints: " + str(model.getNumConstraints()) );
  print("          reactions: " + str(model.getNumReactions()) );
  print("             events: " + str(model.getNumEvents()) );
  print("\n");
  
  return 0;
 
if __name__ == '__main__':
  main(sys.argv)  
