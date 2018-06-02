/**
* @file    TestDistribExternalParameters.cpp
* @brief   Unit tests of writing DistribExtension
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
*/

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/distrib/common/DistribExtensionTypes.h>

#include <sbml/conversion/ConversionProperties.h>

#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */

CK_CPPSTART
extern char *TestDataDirectory;


static bool
equals(std::string& expected, std::string& actual)
{
  if (expected == actual) return true;

  cout << "\nStrings are not equal:\n";
  cout << "Expected:\n[" << expected << "]\n";
  cout << "Actual:\n[" << actual << "]\n";

  return false;
}




START_TEST(test_ExternalParameters_write_attributes)
{
  std::string expected = "<externalParameter id=\"ub\" name=\"name1\" value=\"2.3\"/>";
  
  DistribExternalParameter *EP = new DistribExternalParameter(new DistribPkgNamespaces());
  fail_unless(EP->isSetId() == false);
  fail_unless(EP->isSetName() == false);
  fail_unless(EP->isSetValue() == false);
  fail_unless(EP->isSetVar() == false);
  fail_unless(EP->isSetUnits() == false);
  
  EP->setValue(2.3);
  fail_unless(EP->isSetValue() == true);
  fail_unless(util_isEqual(EP->getValue(), 2.3));


  EP->setId("ub");
  fail_unless(EP->isSetId() == true);
  fail_unless(EP->getId() == "ub");

  EP->setName("name1");
  fail_unless(EP->isSetName() == true);
  fail_unless(EP->getName() == "name1");

  ostringstream oss;
  oss << EP->toSBML();
  std::string actual = oss.str();

  fail_unless(equals(expected, actual));

  delete EP;
}
END_TEST


START_TEST(test_ExternalParameters_createFD_l3v1v1)
{
  DistribPkgNamespaces *sbmlns = new DistribPkgNamespaces(3, 1, 1);

  // create the document

  SBMLDocument* document = new SBMLDocument(sbmlns);

  // mark distrib required

  document->setPackageRequired("distrib", true);

  // create the Model

  Model* model = document->createModel();

  // create FunctionDefinition

  FunctionDefinition* fd = model->createFunctionDefinition();
  fd->setId("Exponential2");

  ASTNode* math = SBML_parseL3Formula("lambda(x,y,nan)");
  fd->setMath(math);
  delete math;

  DistribFunctionDefinitionPlugin* fdplugin = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  DistribDrawFromDistribution *draw = fdplugin->createDistribDrawFromDistribution();

  DistribInput *input = draw->createDistribInput();
  input->setId("beta");
  input->setIndex(0);

  DistribExternalDistribution * dist = draw->createDistribExternalDistribution();
  dist->setName("Exponential 2");
  dist->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000353");

  DistribExternalParameter * ep = dist->createDistribExternalParameter();
  ep->setName("Beta");
  ep->setVar("beta");
  ep->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000362");

  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_externalParameters.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST


START_TEST(test_ExternalParameters_createFD_l3v2v1)
{
  DistribPkgNamespaces *sbmlns = new DistribPkgNamespaces(3, 2, 1);

  // create the document

  SBMLDocument* document = new SBMLDocument(sbmlns);

  // mark distrib required

  document->setPackageRequired("distrib", true);

  // create the Model

  Model* model = document->createModel();

  // create FunctionDefinition

  FunctionDefinition* fd = model->createFunctionDefinition();
  fd->setId("Exponential2");

  ASTNode* math = SBML_parseL3Formula("lambda(x,y,nan)");
  fd->setMath(math);
  delete math;

  DistribFunctionDefinitionPlugin* fdplugin =
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  DistribDrawFromDistribution *draw = fdplugin->createDistribDrawFromDistribution();

  DistribInput *input = draw->createDistribInput();
  input->setId("beta");
  input->setIndex(0);

  DistribExternalDistribution * dist = draw->createDistribExternalDistribution();
  dist->setName("Exponential 2");
  dist->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000353");

  DistribExternalParameter * ep = dist->createDistribExternalParameter();
  ep->setName("Beta");
  ep->setVar("beta");
  ep->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000362");

  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_externalParameters_l3v2.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST

START_TEST(test_ExternalParameters_testSetLevelVersion)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_externalParameters_l3v2.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_externalParameters.xml");
  SBMLDocument *document = readSBMLFromFile(fileName.c_str());

  fail_unless(document->setLevelAndVersion(3, 2));
  
  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST


START_TEST(test_ExternalParameters_testSetLevelVersion_2to1)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_externalParameters.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_externalParameters_l3v2.xml");
  SBMLDocument *document = readSBMLFromFile(fileName.c_str());

  fail_unless(document->setLevelAndVersion(3, 1));

  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST

Suite *
create_suite_test_ExternalParameters(void)
{
  Suite *suite = suite_create("TestExternalParameters");
  TCase *tcase = tcase_create("TestExternalParameters");

  tcase_add_test(tcase, test_ExternalParameters_write_attributes);
  tcase_add_test(tcase, test_ExternalParameters_createFD_l3v1v1);
  tcase_add_test(tcase, test_ExternalParameters_createFD_l3v2v1);
  tcase_add_test(tcase, test_ExternalParameters_testSetLevelVersion);
  tcase_add_test(tcase, test_ExternalParameters_testSetLevelVersion_2to1);

  suite_add_tcase(suite, tcase);

  return suite;
}

CK_CPPEND