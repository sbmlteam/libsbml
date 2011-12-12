#!/usr/bin/env python
## 
## @file    unsetAnnotation.py
## @brief   unset annotation for each element
## @author  Akiya Jouraku
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
  """Usage: unsetAnnotation <input-filename> <output-filename>
  """
  if (len(args) != 3):
      print("\n" + "Usage: unsetAnnotation <input-filename> <output-filename>" + "\n" + "\n");
      return 1;
  
  filename = args[1];
  
  document = readSBML(filename);
  
  
  errors = document.getNumErrors();
  
  if (errors > 0):
      document.printErrors();
      return errors;
  
  m = document.getModel();
  m.unsetAnnotation();
  
  for i in range(0, m.getNumReactions()):
      re = m.getReaction(i);
      re.unsetAnnotation();
  
      for j in range(0, re.getNumReactants()):
          rt = re.getReactant(j);
          rt.unsetAnnotation();
  
      for j in range(0, re.getNumProducts()):
          rt = re.getProduct(j);
          rt.unsetAnnotation();
  
      for j in range(0, re.getNumModifiers()):
          md = re.getModifier(j);
          md.unsetAnnotation();
  
      if (re.isSetKineticLaw()):
          kl = re.getKineticLaw();
          kl.unsetAnnotation();
  
          for j in range(0, kl.getNumParameters()):
              pa = kl.getParameter(j);
              pa.unsetAnnotation();
  
  for i in range(0, m.getNumSpecies()):
      sp = m.getSpecies(i);
      sp.unsetAnnotation();
  
  for i in range(0, m.getNumCompartments()):
      sp = m.getCompartment(i);
      sp.unsetAnnotation();
  
  for i in range(0, m.getNumFunctionDefinitions()):
      sp = m.getFunctionDefinition(i);
      sp.unsetAnnotation();
  
  for i in range(0, m.getNumUnitDefinitions()):
      sp = m.getUnitDefinition(i);
      sp.unsetAnnotation();
  
  for i in range(0, m.getNumParameters()):
      sp = m.getParameter(i);
      sp.unsetAnnotation();
  
  for i in range(0, m.getNumRules()):
      sp = m.getRule(i);
      sp.unsetAnnotation();
  
  for i in range(0, m.getNumInitialAssignments()):
      sp = m.getInitialAssignment(i);
      sp.unsetAnnotation();
  
  for i in range(0, m.getNumEvents()):
      sp = m.getEvent(i);
      sp.unsetAnnotation();
  
      for j in range(0, sp.getNumEventAssignments()):
          ea = sp.getEventAssignment(j);
          ea.unsetAnnotation();
  
  for i in range(0, m.getNumSpeciesTypes()):
      sp = m.getSpeciesType(i);
      sp.unsetAnnotation();
  
  for i in range(0, m.getNumConstraints()):
      sp = m.getConstraint(i);
      sp.unsetAnnotation();
  
  writeSBML(document, args[2]);
  
  return errors;

if __name__ == '__main__':
  main(sys.argv)  
