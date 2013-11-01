#!/usr/bin/env python
## 
## @file    printUnits.py
## @brief   Prints some unit information about the model
## @author  Sarah Keating
## @author  Michael Hucka
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

import sys
import os.path
from libsbml import *

def main (args):
  """Usage: printUnits filename
  """

  if (len(args) != 2):
      print("Usage: printUnits filename");
      return 1;
  
  filename = args[1];
  document = readSBML(filename);
  
  if (document.getNumErrors() > 0):
      print("Encountered the following SBML errors:");
      document.printErrors();
      return 1;
  
  model = document.getModel();
  
  if (model == None):
      print("No model present.");
      return 1;

  for i in range(0, model.getNumSpecies()):
      s = model.getSpecies(i);
      print("Species " + str(i) + ": "
      + UnitDefinition.printUnits(s.getDerivedUnitDefinition()));
  
  for i in range(0,model.getNumCompartments()):
      c = model.getCompartment(i);
      print("Compartment " + str(i) + ": "
                                    + UnitDefinition.printUnits(c.getDerivedUnitDefinition()))
  
  for i in range(0,model.getNumParameters()):
      p = model.getParameter(i);
      print("Parameter " + str(i) + ": "
                                    + UnitDefinition.printUnits(p.getDerivedUnitDefinition()))
  
  for i in range(0,model.getNumInitialAssignments()):
      ia = model.getInitialAssignment(i);
      print("InitialAssignment " + str(i) + ": "
                                    + UnitDefinition.printUnits(ia.getDerivedUnitDefinition()));
      tmp = "no"
      if (ia.containsUndeclaredUnits()):
		tmp = "yes"
      print("        undeclared units: " + tmp);
  
  for i in range(0,model.getNumEvents()):
      e = model.getEvent(i);
      print("Event " + str(i) + ": ");
  
      if (e.isSetDelay()):
          print("Delay: "
                                            + UnitDefinition.printUnits(e.getDelay().getDerivedUnitDefinition()));
          tmp = "no"
          if (e.getDelay().containsUndeclaredUnits()):
		    tmp = "yes"
          print("        undeclared units: " + tmp);
  
      for j in range(0,e.getNumEventAssignments()):
          ea = e.getEventAssignment(j);
          print("EventAssignment " + str(j) + ": "
                                            + UnitDefinition.printUnits(ea.getDerivedUnitDefinition()));
          tmp = "no"
          if (ea.containsUndeclaredUnits()):
		    tmp = "yes"
          print("        undeclared units: " + tmp);
  
  for i in range(0,model.getNumReactions()):
      r = model.getReaction(i);
  
      print("Reaction " + str(i) + ": ");
  
      if (r.isSetKineticLaw()):
          print("Kinetic Law: "
                                            + UnitDefinition.printUnits(r.getKineticLaw().getDerivedUnitDefinition()));
          tmp = "no"
          if (r.getKineticLaw().containsUndeclaredUnits()):
		    tmp = "yes"
          print("        undeclared units: " + tmp);
  
      for j in range(0,r.getNumReactants()):
          sr = r.getReactant(j);
  
          if (sr.isSetStoichiometryMath()):
              print("Reactant stoichiometryMath" + str(j) + ": "
                                                    + UnitDefinition.printUnits(sr.getStoichiometryMath().getDerivedUnitDefinition()));
              tmp = "no"
              if (sr.getStoichiometryMath().containsUndeclaredUnits()):
		        tmp = "yes"
              print("        undeclared units: " + tmp);              
  
      for j in range(0,r.getNumProducts()):
          sr = r.getProduct(j);
  
          if (sr.isSetStoichiometryMath()):
              print("Product stoichiometryMath" + str(j) + ": "
                                                    + UnitDefinition.printUnits(sr.getStoichiometryMath().getDerivedUnitDefinition()));
              tmp = "no"
              if (sr.getStoichiometryMath().containsUndeclaredUnits()):
		        tmp = "yes"
              print("        undeclared units: " + tmp);    
  
  for i in range(0,model.getNumRules()):
      r = model.getRule(i);
      print("Rule " + str(i) + ": "
                                    + UnitDefinition.printUnits(r.getDerivedUnitDefinition()));
      tmp = "no"
      if (r.getStoichiometryMath().containsUndeclaredUnits()):
		tmp = "yes"
      print("        undeclared units: " + tmp);    
  
  return 0;
  
if __name__ == '__main__':
  main(sys.argv)  
