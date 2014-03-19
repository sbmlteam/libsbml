///**
// * @file    createNormalExample.cs
// * @brief   SBML distrib example
// * @author  Sarah Keating
// *
// *
// * <!--------------------------------------------------------------------------
// * This file is part of libSBML.  Please visit http://sbml.org for more
// * information about SBML, and the latest version of libSBML.
// *
// * Copyright (C) 2009-2013 jointly by the following organizations:
// *     1. California Institute of Technology, Pasadena, CA, USA
// *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
// *
// * Copyright (C) 2006-2008 by the California Institute of Technology,
// *     Pasadena, CA, USA 
// *
// * Copyright (C) 2002-2005 jointly by the following organizations:
// *     1. California Institute of Technology, Pasadena, CA, USA
// *     2. Japan Science and Technology Agency, Japan
// *
// * This library is free software; you can redistribute it and/or modify it
// * under the terms of the GNU Lesser General Public License as published by
// * the Free Software Foundation.  A copy of the license agreement is provided
// * in the file named "LICENSE.txt" included with this software distribution
// * and also available online as http://sbml.org/software/libsbml/license.html
// * ------------------------------------------------------------------------ -->
// */

using System;
using libsbmlcs;


public class CreateNormalExample
{
    
    static void Main(string[] args)
    {

        SBMLNamespaces sbmlns = new SBMLNamespaces(3, 1, "distrib", 1);
        SBMLDocument document = new SBMLDocument(sbmlns);

        // set the required attribute to true
        DistribSBMLDocumentPlugin docPlug = (DistribSBMLDocumentPlugin)
                                            (document->getPlugin("distrib"));
        docPlug.setRequired(true);

        // create the Model

        Model model = document.createModel();

        // create the FunctionDefintion
        
        FunctionDefinition fd = model.createFunctionDefinition();
        fd.setId("mynormal");

        ASTNode math = libsbml.parseFormula("lambda(param1, param2, 0)"); 
        fd.setMath(math);

        //
        // Get a DistribFunctionDefinitionPlugin object plugged in the fd object.
        //
        // The type of the returned value of SBase::getPlugin() function is SBasePlugin, and
        // thus the value needs to be casted for the corresponding derived class. 
        //
        DistribFunctionDefinitionPlugin fdPlugin = 
                    (DistribFunctionDefinitionPlugin)(fd.getPlugin("distrib"));

        // create a DrawFromDistribution object
        DrawFromDistribution draw = fdPlugin.createDrawFromDistribution();

        // create the distribInputs
        DistribInput input1 = draw.createDistribInput();
        input1.setId("mu");
        input1.setIndex(0);

        DistribInput input2 = draw.createDistribInput();
        input2.setId("sigma");
        input2.setIndex(1);

        // create the UncertMLNode object
        UncertMLNode uncert = libsbml.createDistributionNode
                         ("NormalDistribution", "mean, variance", "mu,sigma");

        draw.setUncertML(uncert);

        libsbml.writeSBMLToFile(document, "distrib_example1.xml");
    }
}
