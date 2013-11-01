#!/usr/bin/env python
## 
## @file    printNotes.py
## @brief   Prints notes strings for each element
## @author  Akiya Jouraku
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


import sys
import os.path
from libsbml import *

def printNotes(sb, id=""):
  if (not sb.isSetNotes()):
	return;        
  
  pid = "";
  
  if (sb.isSetId()):
      pid = sb.getId();
  print("----- " + sb.getElementName() + " (" + pid
                    + ") notes -----" + "\n");
  print(sb.getNotesString() + "\n");
  print("\n");

def main (args):
  """Usage: printNotes filename
  """

  if len(args) != 2:
    print(main.__doc__)
    sys.exit(2)

  filename = args[1];
  document = readSBML(filename);
  
  errors = document.getNumErrors();
  
  print("filename: " + filename + "\n");
  
  if (errors > 0):
      document.printErrors();
      return errors;
  
  
  # Model
  
  m = document.getModel();
  printNotes(m);
  
  for i in range(0, m.getNumReactions()):
      re = m.getReaction(i);
      printNotes(re);
  
      # SpeciesReference (Reacatant)
  
      for j in range(0, re.getNumReactants()):
          rt = re.getReactant(j);
          if (rt.isSetNotes()):
			print("     ");
          printNotes(rt, rt.getSpecies());
  
      # SpeciesReference (Product) 
  
      for j in range(0, re.getNumProducts()):
          rt = re.getProduct(j);
          if (rt.isSetNotes()):
			print("     ");
          printNotes(rt, rt.getSpecies());

	  # ModifierSpeciesReference (Modifiers)
  
      for j in range(0, re.getNumModifiers()):
          md = re.getModifier(j);
          if (md.isSetNotes()):
			print("     ");
          printNotes(md, md.getSpecies());
  
      # KineticLaw 
  
      if (re.isSetKineticLaw()):
          kl = re.getKineticLaw();
          if (kl.isSetNotes()):
			print("   ");
          printNotes(kl);
  
          # Parameter   
          for j in range(0, kl.getNumParameters()):
              pa = kl.getParameter(j);
              if (pa.isSetNotes()):
				print("      ");
              printNotes(pa);
  
  # Species 
  for i in range(0, m.getNumSpecies()):
      sp = m.getSpecies(i);
      printNotes(sp);
  
  # Compartments 
  for i in range(0, m.getNumCompartments()):
      sp = m.getCompartment(i);
      printNotes(sp);
  
  # FunctionDefinition 
  for i in range (0, m.getNumFunctionDefinitions()):
      sp = m.getFunctionDefinition(i);
      printNotes(sp);
  
  # UnitDefinition 
  for i in range (0, m.getNumUnitDefinitions()):
      sp = m.getUnitDefinition(i);
      printNotes(sp);
  
  # Parameter 
  for i in range(0, m.getNumParameters()):
      sp = m.getParameter(i);
      printNotes(sp);
  
  # Rule 
  for i in range(0, m.getNumRules()):
      sp = m.getRule(i);
      printNotes(sp);
  
  # InitialAssignment 
  for i in range(0, m.getNumInitialAssignments()):
      sp = m.getInitialAssignment(i);
      printNotes(sp);
  
  # Event 
  for i in range(0,m.getNumEvents()):
      sp = m.getEvent(i);
      printNotes(sp);
  
      # Trigger 
      if (sp.isSetTrigger()):
          tg = sp.getTrigger();
          if (tg.isSetNotes()):
			print("   ");
          printNotes(tg);
  
      # Delay 
      if (sp.isSetDelay()):
          dl = sp.getDelay();
          if (dl.isSetNotes()):
			print("   ");
          printNotes(dl);
  
      # EventAssignment 
      for j in range(0,sp.getNumEventAssignments()):
          ea = sp.getEventAssignment(j);
          if (ea.isSetNotes()):
			print("   ");
          printNotes(ea);
  
  # SpeciesType 
  for i in range(0,m.getNumSpeciesTypes()):
      sp = m.getSpeciesType(i);
      printNotes(sp);
  
  # Constraints 
  for i in range(0,m.getNumConstraints()):
      sp = m.getConstraint(i);
      printNotes(sp);
  
  return errors;
  
if __name__ == '__main__':
  main(sys.argv)  
