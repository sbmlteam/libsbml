/**
 * @file    createStatisticsExample.cpp
 * @brief   SBML distrib example
 * @author  Sarah Keating
 *
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#include <sbml/SBMLTypes.h>
#include <sbml/packages/distrib/common/DistribExtensionTypes.h>

LIBSBML_CPP_NAMESPACE_USE

int main(int argc,char** argv)
{
  SBMLNamespaces sbmlns(3,1,"distrib",1);

  // create the document

  SBMLDocument *document = new SBMLDocument(&sbmlns);

  // set the required attribute to true
  DistribSBMLDocumentPlugin * docPlug = 
    static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
  docPlug->setRequired(true);


  // create the Model

  Model* model=document->createModel();

  // create the Parameter
  Parameter * p = model->createParameter();
  p->setId("V");
  p->setConstant(true);

  //
  // Get a DistribSBasePlugin object plugged in the parameter object.
  //
  // The type of the returned value of SBase::getPlugin() function is 
  // SBasePlugin*, and
  // thus the value needs to be casted for the corresponding derived class. 
  //
  DistribSBasePlugin* pPlugin = 
    static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));

  // create a Uncertainty object
  Uncertainty * uncert = pPlugin->createUncertainty();

  // create the UncertMLNode object
  UncertMLNode * uncertml = UncertMLNode::createStatisticsNode
                          ("Mean, Variance", "V_pop, V_omega");

  uncert->setUncertML(uncertml);

  writeSBML(document,"distrib_example2.xml");

  delete document;
}
