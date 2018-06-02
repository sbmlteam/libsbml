/**
* @file    TestDistribUncertBound.cpp
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




START_TEST(test_UncertBound_write_attributes)
{
  std::string expected = "<uncertBound id=\"ub\" value=\"2.3\" inclusive=\"true\"/>";
  
  DistribUncertBound *UB = new DistribUncertBound(new DistribPkgNamespaces());
  fail_unless(UB->isSetId() == false);
  fail_unless(UB->isSetValue() == false);
  fail_unless(UB->isSetVar() == false);
  fail_unless(UB->isSetInclusive() == false);
  fail_unless(UB->isSetUnits() == false);
  
  UB->setValue(2.3);
  fail_unless(UB->isSetValue() == true);
  fail_unless(util_isEqual(UB->getValue(), 2.3));


  UB->setInclusive(true);
  fail_unless(UB->isSetInclusive() == true);
  fail_unless(UB->getInclusive() == true);

  UB->setId("ub");
  fail_unless(UB->isSetId() == true);
  fail_unless(UB->getId() == "ub");

  ostringstream oss;
  oss << UB->toSBML();

  fail_unless(equals(expected, oss.str()));

  delete UB;
}
END_TEST


START_TEST(test_UncertBound_createFD_l3v1v1)
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
  fd->setId("distribution");

  ASTNode* math = SBML_parseL3Formula("lambda(x,y,nan)");
  fd->setMath(math);
  delete math;

  DistribFunctionDefinitionPlugin* fdplugin = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  DistribDrawFromDistribution *draw = fdplugin->createDistribDrawFromDistribution();

  DistribInput *input = draw->createDistribInput();
  input->setId("mean");
  input->setIndex(0);

  input = draw->createDistribInput();
  input->setId("stddev");
  input->setIndex(1);

  DistribNormalDistribution *norm = draw->createDistribNormalDistribution();
  DistribUncertValue *uv = norm->createMean();
  uv->setVar("mean");

  uv = norm->createStddev();
  uv->setVar("stddev");

  DistribUncertBound *ub = norm->createTruncationLowerBound();
  ub->setValue(2.3);
  ub->setInclusive(true);

  ub = norm->createTruncationUpperBound();
  ub->setValue(7.3);
  ub->setInclusive(true);

  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_uncertBound.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST


START_TEST(test_UncertBound_createFD_l3v2v1)
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
  fd->setId("distribution");

  ASTNode* math = SBML_parseL3Formula("lambda(x,y,nan)");
  fd->setMath(math);
  delete math;

  DistribFunctionDefinitionPlugin* fdplugin =
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  DistribDrawFromDistribution *draw = fdplugin->createDistribDrawFromDistribution();

  DistribInput *input = draw->createDistribInput();
  input->setId("mean");
  input->setIndex(0);

  input = draw->createDistribInput();
  input->setId("stddev");
  input->setIndex(1);

  DistribNormalDistribution *norm = draw->createDistribNormalDistribution();
  DistribUncertValue *uv = norm->createMean();
  uv->setVar("mean");

  uv = norm->createStddev();
  uv->setVar("stddev");

  DistribUncertBound *ub = norm->createTruncationLowerBound();
  ub->setValue(2.3);
  ub->setInclusive(true);

  ub = norm->createTruncationUpperBound();
  ub->setValue(7.3);
  ub->setInclusive(true);

  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_uncertBound_l3v2.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST

START_TEST(test_UncertBound_testSetLevelVersion)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_uncertBound_l3v2.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_uncertBound.xml");
  SBMLDocument *document = readSBMLFromFile(fileName.c_str());

  fail_unless(document->setLevelAndVersion(3, 2));
  
  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST


START_TEST(test_UncertBound_testSetLevelVersion_2to1)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_uncertBound.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_uncertBound_l3v2.xml");
  SBMLDocument *document = readSBMLFromFile(fileName.c_str());

  fail_unless(document->setLevelAndVersion(3, 1));

  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST

START_TEST(test_UncertBound_testGetInputByIndex)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_uncertBound.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());

  FunctionDefinition* fd = doc->getModel()->getFunctionDefinition(0);
  DistribFunctionDefinitionPlugin* fdplugin =
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  DistribDrawFromDistribution *draw = fdplugin->getDistribDrawFromDistribution();

  DistribInput *i = draw->getDistribInputByIndex(1);
  fail_unless(i != NULL);
  fail_unless(i->getId() == "stddev");
  fail_unless(i->getIndex() == 1);

  i = draw->getDistribInputByIndex(0);
  fail_unless(i != NULL);
  fail_unless(i->getId() == "mean");
  fail_unless(i->getIndex() == 0);

  delete doc;
}
END_TEST

Suite *
create_suite_test_UncertBound(void)
{
  Suite *suite = suite_create("TestUncertBound");
  TCase *tcase = tcase_create("TestUncertBound");

  tcase_add_test(tcase, test_UncertBound_write_attributes);
  tcase_add_test(tcase, test_UncertBound_createFD_l3v1v1);
  tcase_add_test(tcase, test_UncertBound_createFD_l3v2v1);
  tcase_add_test(tcase, test_UncertBound_testSetLevelVersion);
  tcase_add_test(tcase, test_UncertBound_testSetLevelVersion_2to1);
  tcase_add_test(tcase, test_UncertBound_testGetInputByIndex);

  suite_add_tcase(suite, tcase);

  return suite;
}

CK_CPPEND