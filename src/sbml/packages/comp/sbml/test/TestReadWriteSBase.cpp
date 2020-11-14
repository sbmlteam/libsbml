/**
 * \file    TestSBaseObjects.cpp
 * \brief   Implementation of the Tests for the Comp sbase objects
 * \author  Lucian Smith
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

 #include <sbml/common/common.h>

#include <sbml/packages/comp/common/CompExtensionTypes.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

START_TEST (test_comp_rwports)
{
  //Create a model with a port.
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  SBMLDocument orig(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(orig.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = orig.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Parameter* param = model->createParameter();
  param->setId("p1");
  param->setConstant(true);

  Port* port = compmod->createPort();
  port->setId("port1");
  port->setIdRef("p1");
  port->setSBOTerm(10);

  string original = writeSBMLToStdString(&orig);

  // Now round-trip that model.
  SBMLDocument* doc = readSBMLFromString(original.c_str());
  string rtmodel = writeSBMLToStdString(doc);

  fail_unless(original == rtmodel);
  delete doc;
}
END_TEST

  
START_TEST(test_comp_rwmoddefl3v1)
{
  SBMLNamespaces sbmlns(3, 1, "comp", 1);

  SBMLDocument orig(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(orig.getPlugin("comp"));
  compdoc->setRequired(true);

  Model model(&sbmlns);
  model.setId("moddef1");
  model.setName("Model Definition");
  model.setMetaId("md1");
  ModelDefinition md(model);
  compdoc->addModelDefinition(&md);

  ModelDefinition* moddef = compdoc->getModelDefinition(0);
  fail_unless(moddef->isSetId());
  fail_unless(moddef->isSetName());
  fail_unless(moddef->isSetMetaId());

  SBMLWriter writer;
  stringstream stream;
  writer.writeSBML(&orig, stream);
  string original = stream.str();
  fail_unless(original.find("comp:id") == string::npos);
  fail_unless(original.find("comp:name") == string::npos);
  fail_unless(original.find("comp:metaid") == string::npos);
  fail_unless(original.find("id") != string::npos);
  fail_unless(original.find("name") != string::npos);
  fail_unless(original.find("metaid") != string::npos);

  // Now round-trip that model.
  SBMLDocument* doc = readSBMLFromString(original.c_str());
  compdoc = static_cast<CompSBMLDocumentPlugin*> (doc->getPlugin("comp"));
  moddef = compdoc->getModelDefinition(0);
  fail_unless(moddef->isSetId());
  fail_unless(moddef->isSetName());
  fail_unless(moddef->isSetMetaId());

  delete doc;
}
END_TEST


START_TEST(test_comp_rwmoddefl3v2)
{
  SBMLNamespaces sbmlns(3, 2, "comp", 1);

  SBMLDocument orig(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(orig.getPlugin("comp"));
  compdoc->setRequired(true);

  Model model(&sbmlns);
  model.setId("moddef1");
  model.setName("Model Definition");
  model.setMetaId("md1");
  ModelDefinition md(model);
  compdoc->addModelDefinition(&md);

  ModelDefinition* moddef = compdoc->getModelDefinition(0);
  fail_unless(moddef->isSetId());
  fail_unless(moddef->isSetName());
  fail_unless(moddef->isSetMetaId());

  SBMLWriter writer;
  stringstream stream;
  writer.writeSBML(&orig, stream);
  string original = stream.str();

  fail_unless(original.find("comp:id") == string::npos);
  fail_unless(original.find("comp:name") == string::npos);
  fail_unless(original.find("comp:metaid") == string::npos);
  fail_unless(original.find("id") != string::npos);
  fail_unless(original.find("name") != string::npos);
  fail_unless(original.find("metaid") != string::npos);

  // Now round-trip that model.
  SBMLDocument* doc = readSBMLFromString(original.c_str());
  compdoc = static_cast<CompSBMLDocumentPlugin*> (doc->getPlugin("comp"));
  moddef = compdoc->getModelDefinition(0);
  fail_unless(moddef->isSetId());
  fail_unless(moddef->isSetName());
  fail_unless(moddef->isSetMetaId());

  delete doc;
}
END_TEST


Suite *
create_suite_TestReadWriteSBaseObjects(void)
{ 
  TCase *tcase = tcase_create("SBMLCompSBaseObjects");
  Suite *suite = suite_create("SBMLCompSBaseObjects");
  
  tcase_add_test(tcase, test_comp_rwports);
  tcase_add_test(tcase, test_comp_rwmoddefl3v1);
  tcase_add_test(tcase, test_comp_rwmoddefl3v2);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

