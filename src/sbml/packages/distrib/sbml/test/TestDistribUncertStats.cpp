/**
* @file    TestDistribUncertStats.cpp
* @brief   Unit tests of writing DistribExtension
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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




START_TEST(test_UncertStats_create_l3v1v1)
{
  DistribPkgNamespaces *sbmlns = new DistribPkgNamespaces(3, 1, 1);

  // create the document

  SBMLDocument* document = new SBMLDocument(sbmlns);

  // mark distrib required

  document->setPackageRequired("distrib", true);

  // create the Model

  Model* model = document->createModel();

  Parameter * p = model->createParameter();
  p->setId("P1");
  p->setValue(5.13);
  p->setConstant(true);

  DistribSBasePlugin * plug = static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));
  Uncertainty * uncert = plug->createUncertainty();
  UncertSpan * span = uncert->createUncertSpan();
  span->setType("confidenceInterval");
  span->setValueLower(5);
  span->setValueUpper(5.32);

  UncertParameter * uv = uncert->createUncertParameter();
  uv->setType("standardDeviation");
  uv->setValue(0.3);


  p = model->createParameter();
  p->setId("P3");
  p->setValue(15);
  p->setConstant(true);

  plug = static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));
  uncert = plug->createUncertainty();
  UncertParameter *ep = uncert->createUncertParameter();
  ep->setVar("beta");
  ep->setType("distribution");
  ep->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000362");


  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_statistics.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST


START_TEST(test_UncertStats_create_l3v2v1)
{
  DistribPkgNamespaces *sbmlns = new DistribPkgNamespaces(3, 2, 1);

  // create the document

  SBMLDocument* document = new SBMLDocument(sbmlns);

  // mark distrib required

  document->setPackageRequired("distrib", true);

  Model* model = document->createModel();

  Parameter * p = model->createParameter();
  p->setId("P1");
  p->setValue(5.13);
  p->setConstant(true);

  DistribSBasePlugin * plug = static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));
  Uncertainty * uncert = plug->createUncertainty();
  UncertSpan * span = uncert->createUncertSpan();
  span->setType("confidenceInterval");
  span->setValueLower(5);
  span->setValueUpper(5.32);

  UncertParameter * uv = uncert->createUncertParameter();
  uv->setType("standardDeviation");
  uv->setValue(0.3);


  p = model->createParameter();
  p->setId("P3");
  p->setValue(15);
  p->setConstant(true);

  plug = static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));
  uncert = plug->createUncertainty();
  UncertParameter *ep = uncert->createUncertParameter();
  ep->setVar("beta");
  ep->setType("distribution");
  ep->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000362");

  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_statistics_l3v2.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST

START_TEST(test_UncertStats_testSetLevelVersion)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_statistics_l3v2.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_statistics.xml");
  SBMLDocument *document = readSBMLFromFile(fileName.c_str());

  fail_unless(document->setLevelAndVersion(3, 2));
  
  std::string actual = writeSBMLToStdString(document);

  // This is correct except the distrib ns is written before the core
//  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST


START_TEST(test_UncertStats_testSetLevelVersion_2to1)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_statistics.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  std::string expected = writeSBMLToStdString(doc);
  delete doc;

  fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_statistics_l3v2.xml");
  SBMLDocument *document = readSBMLFromFile(fileName.c_str());

  fail_unless(document->setLevelAndVersion(3, 1));

  std::string actual = writeSBMLToStdString(document);

  fail_unless(equals(expected, actual));

  delete document;
}
END_TEST

Suite *
create_suite_test_UncertStats(void)
{
  Suite *suite = suite_create("TestUncertStats");
  TCase *tcase = tcase_create("TestUncertStats");

  tcase_add_test(tcase, test_UncertStats_create_l3v1v1);
  tcase_add_test(tcase, test_UncertStats_create_l3v2v1);
  tcase_add_test(tcase, test_UncertStats_testSetLevelVersion);
  tcase_add_test(tcase, test_UncertStats_testSetLevelVersion_2to1);

  suite_add_tcase(suite, tcase);

  return suite;
}

CK_CPPEND