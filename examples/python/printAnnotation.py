#!/usr/bin/env python
## 
## @file    printAnnotation.py
## @brief   Prints annotation strings for each element
## @author  Akiya Jouraku
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


import sys
import os.path
from libsbml import *

def printAnnotation(sb, id=""):
  if (not sb.isSetAnnotation()):
	return;        
  
  pid = "";
  
  if (sb.isSetId()):
      pid = sb.getId();
  print("----- " + sb.getElementName() + " (" + pid
                    + ") annotation -----" + "\n");
  print(sb.getAnnotationString() + "\n");
  print("\n");

def main (args):
  """Usage: printAnnotation filename
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
  printAnnotation(m);
  
  for i in range(0, m.getNumReactions()):
      re = m.getReaction(i);
      printAnnotation(re);
  
      # SpeciesReference (Reacatant)
  
      for j in range(0, re.getNumReactants()):
          rt = re.getReactant(j);
          if (rt.isSetAnnotation()):
			print("     ");
          printAnnotation(rt, rt.getSpecies());
  
      # SpeciesReference (Product) 
  
      for j in range(0, re.getNumProducts()):
          rt = re.getProduct(j);
          if (rt.isSetAnnotation()):
			print("     ");
          printAnnotation(rt, rt.getSpecies());

	  # ModifierSpeciesReference (Modifiers)
  
      for j in range(0, re.getNumModifiers()):
          md = re.getModifier(j);
          if (md.isSetAnnotation()):
			print("     ");
          printAnnotation(md, md.getSpecies());
  
      # KineticLaw 
  
      if (re.isSetKineticLaw()):
          kl = re.getKineticLaw();
          if (kl.isSetAnnotation()):
			print("   ");
          printAnnotation(kl);
  
          # Parameter   
          for j in range(0, kl.getNumParameters()):
              pa = kl.getParameter(j);
              if (pa.isSetAnnotation()):
				print("      ");
              printAnnotation(pa);
  
  # Species 
  for i in range(0, m.getNumSpecies()):
      sp = m.getSpecies(i);
      printAnnotation(sp);
  
  # Compartments 
  for i in range(0, m.getNumCompartments()):
      sp = m.getCompartment(i);
      printAnnotation(sp);
  
  # FunctionDefinition 
  for i in range (0, m.getNumFunctionDefinitions()):
      sp = m.getFunctionDefinition(i);
      printAnnotation(sp);
  
  # UnitDefinition 
  for i in range (0, m.getNumUnitDefinitions()):
      sp = m.getUnitDefinition(i);
      printAnnotation(sp);
  
  # Parameter 
  for i in range(0, m.getNumParameters()):
      sp = m.getParameter(i);
      printAnnotation(sp);
  
  # Rule 
  for i in range(0, m.getNumRules()):
      sp = m.getRule(i);
      printAnnotation(sp);
  
  # InitialAssignment 
  for i in range(0, m.getNumInitialAssignments()):
      sp = m.getInitialAssignment(i);
      printAnnotation(sp);
  
  # Event 
  for i in range(0,m.getNumEvents()):
      sp = m.getEvent(i);
      printAnnotation(sp);
  
      # Trigger 
      if (sp.isSetTrigger()):
          tg = sp.getTrigger();
          if (tg.isSetAnnotation()):
			print("   ");
          printAnnotation(tg);
  
      # Delay 
      if (sp.isSetDelay()):
          dl = sp.getDelay();
          if (dl.isSetAnnotation()):
			print("   ");
          printAnnotation(dl);
  
      # EventAssignment 
      for j in range(0,sp.getNumEventAssignments()):
          ea = sp.getEventAssignment(j);
          if (ea.isSetAnnotation()):
			print("   ");
          printAnnotation(ea);
  
  # SpeciesType 
  for i in range(0,m.getNumSpeciesTypes()):
      sp = m.getSpeciesType(i);
      printAnnotation(sp);
  
  # Constraints 
  for i in range(0,m.getNumConstraints()):
      sp = m.getConstraint(i);
      printAnnotation(sp);
  
  return errors;
  
if __name__ == '__main__':
  main(sys.argv)  
