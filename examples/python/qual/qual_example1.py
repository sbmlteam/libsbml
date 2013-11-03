#!/usr/bin/env python
## 
## @file    qual_example1.py
## @brief   Qual Example
## @author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


import sys
import os.path
from libsbml import *




def main (args):
    # Creates an SBMLNamespaces object with the given SBML level, version
    # package name, package version.
    sbmlns = SBMLNamespaces(3, 1, "qual", 1);

    # Creates an SBMLDocument object 
    document = SBMLDocument(sbmlns);

    # mark qual as required
    document.setPackageRequired("qual", True);

    # create the Model
    model = document.createModel();

    # create the Compartment
    compartment = model.createCompartment();
    compartment.setId("c");
    compartment.setConstant(True);
    
    # Get a QualModelPlugin object plugged in the model object.
    mplugin = model.getPlugin("qual");

    # create the QualitativeSpecies
    qs = mplugin.createQualitativeSpecies();
    qs.setId("s1");
    qs.setCompartment("c");
    qs.setConstant(False);
    qs.setInitialLevel(1);
    qs.setMaxLevel(4);
    qs.setName("sss");

    # create the Transition
    t = mplugin.createTransition();
    t.setId("d");
    t.setSBOTerm(1);

    i = t.createInput();
    i.setId("RD");
    i.setQualitativeSpecies("s1");
    i.setTransitionEffect("none");
    i.setSign("negative");
    i.setThresholdLevel(2);
    i.setName("aa");

    o = t.createOutput();
    o.setId("wd");
    o.setQualitativeSpecies("s1");
    o.setTransitionEffect("production");
    o.setOutputLevel(2);
    o.setName("aa");

    ft = t.createFunctionTerm();
    math = parseL3Formula("geq(s1, 2)");
    ft.setResultLevel(1);
    ft.setMath(math);

    dt = t.createDefaultTerm();
    dt.setResultLevel(2);

    writeSBML(document, "qual_example1.xml");

   

  
if __name__ == '__main__':
  main(sys.argv)  
