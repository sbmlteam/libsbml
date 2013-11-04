/**
 * @file    qual_example1.cs
 * @brief   SBML Qual example
 * @author  Sarah Keating
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using libsbmlcs;


public class qual_example1
{

  public static int Main(string[] args)
  {
    // Creates an SBMLNamespaces object with the given SBML level, version
    // package name, package version.
    SBMLNamespaces sbmlns = new SBMLNamespaces(3, 1, "qual", 1);

    // create the document
    SBMLDocument document = new SBMLDocument(sbmlns);

    // mark qual as required
    document.setPackageRequired("qual", true);
    
    // create the Model
    Model model = document.createModel();

    // create the Compartment
    Compartment compartment = model.createCompartment();
    compartment.setId("c");
    compartment.setConstant(true);

    // Get a QualModelPlugin object plugged in the model object.
    QualModelPlugin mplugin = (QualModelPlugin)(model.getPlugin("qual"));

    // create the QualitativeSpecies
    QualitativeSpecies qs = mplugin.createQualitativeSpecies();
    qs.setId("s1");
    qs.setCompartment("c");
    qs.setConstant(false);
    qs.setInitialLevel(1);
    qs.setMaxLevel(4);
    qs.setName("sss");

    // create the Transition
    Transition t = mplugin.createTransition();
    t.setId("d");
    t.setSBOTerm(1);

    Input i = t.createInput();
    i.setId("RD");
    i.setQualitativeSpecies("s1");
    i.setTransitionEffect(libsbml.INPUT_TRANSITION_EFFECT_NONE);
    i.setSign(libsbml.INPUT_SIGN_NEGATIVE);
    i.setThresholdLevel(2);
    i.setName("aa");

    Output o = t.createOutput();
    o.setId("wd");
    o.setQualitativeSpecies("s1");
    o.setTransitionEffect(libsbml.OUTPUT_TRANSITION_EFFECT_PRODUCTION);
    o.setOutputLevel(2);
    o.setName("aa");

    FunctionTerm ft = t.createFunctionTerm();
    ASTNode math = libsbml.parseL3Formula("geq(s1, 2)");
    ft.setResultLevel(1);
    ft.setMath(math);

    DefaultTerm dt = t.createDefaultTerm();
    dt.setResultLevel(2);

    int result = libsbml.writeSBML(document, "qual_example1.xml");

    if (result == 1)
    {
      Console.WriteLine("Wrote file");
      return 0;
    }
    else
    {
      Console.WriteLine("Failed to write");
      return 1;
    }
    
  }

}
