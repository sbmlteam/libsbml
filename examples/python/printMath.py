#!/usr/bin/env python
## 
## @file    printMath.py
## @brief   Prints Rule, Reaction, and Event formulas in a given SBML Document
## @author  Ben Bornstein
## @author  Sarah Keating
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


import sys
import os.path
from libsbml import *

def printFunctionDefinition(n, fd):
    if (fd.isSetMath()):
        print("FunctionDefinition " + str(n) + ", " + fd.getId());

        math = fd.getMath();

        # Print function arguments. 
        if (math.getNumChildren() > 1):
            print("(" + (math.getLeftChild()).getName());

            for n in range (1, math.getNumChildren()):
                print(", " + (math.getChild(n)).getName());

        print(") := ");

        # Print function body. 
        if (math.getNumChildren() == 0):
            print("(no body defined)");
        else:
            math = math.getChild(math.getNumChildren() - 1);
            formula = formulaToString(math);
            print(formula + "\n");


def printRuleMath(n, r):
    if (r.isSetMath()):
        formula = formulaToString(r.getMath());

        if (len(r.getVariable()) > 0):
            print("Rule " + str(n) + ", formula: "
                             + r.getVariable() + " = " + formula + "\n");
        else:
            print("Rule " + str(n) + ", formula: "
                             + formula + " = 0" + "\n");


def printReactionMath(n, r):
    if (r.isSetKineticLaw()):
        kl = r.getKineticLaw();
        if (kl.isSetMath()):
            formula = formulaToString(kl.getMath());
            print("Reaction " + str(n) + ", formula: " + formula + "\n");


def printEventAssignmentMath(n, ea):
    if (ea.isSetMath()):
        variable = ea.getVariable();
        formula = formulaToString(ea.getMath());
        print("  EventAssignment " + str(n)
                              + ", trigger: " + variable + " = " + formula + "\n");


def printEventMath(n, e):
    if (e.isSetDelay()):
        formula = formulaToString(e.getDelay().getMath());
        print("Event " + str(n) + " delay: " + formula + "\n");

    if (e.isSetTrigger()):
        formula = formulaToString(e.getTrigger().getMath());
        print("Event " + str(n) + " trigger: " + formula + "\n");

    for i in range(0,e.getNumEventAssignments()):
        printEventAssignmentMath(i + 1, e.getEventAssignment(i));

    print;

def printMath(m):
    for n in range(0,m.getNumFunctionDefinitions()):
        printFunctionDefinition(n + 1, m.getFunctionDefinition(n));

    for n in range(0,m.getNumRules()):
        printRuleMath(n + 1, m.getRule(n));

	print;

    for n in range(0, m.getNumReactions()):
        printReactionMath(n + 1, m.getReaction(n));

    print;

    for n in range(0,m.getNumEvents()):
        printEventMath(n + 1, m.getEvent(n));


def main (args):
  """Usage: printMath filename
  """
  if (len(args) != 2):
      print("\n" + "Usage: printMath filename" + "\n" + "\n");
      return 1;
  
  filename = args[1];
  document = readSBML(filename);
  
  if (document.getNumErrors() > 0):
      print("Encountered the following SBML errors:" + "\n");
      document.printErrors();
      return 1;
  
  model = document.getModel();
  
  if (model == None):
      print("No model present." + "\n");
      return 1;
  
  printMath(model);
  print;
  return 0;

if __name__ == '__main__':
  main(sys.argv)  
