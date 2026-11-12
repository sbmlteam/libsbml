/**
* @file    TestDistribIdName.cpp
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
#include <sbml/SBMLWriter.h>
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


START_TEST(test_IdName_prefix_read_write_l3v1)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_statistics.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  Parameter* p = doc->getModel()->getParameter(0);

  DistribSBasePlugin * plug = static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));
  Uncertainty * uncert = plug->getUncertainty(0);
  fail_unless(uncert != NULL);

  fail_unless(uncert->setId("u1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(uncert->setName("u1name") == LIBSBML_OPERATION_SUCCESS);

  std::string actual = writeSBMLToStdString(doc);
  delete doc;

  fail_unless(actual.find("<distrib:uncertainty distrib:id=\"u1\" distrib:name=\"u1name\">") != std::string::npos);
}
END_TEST


START_TEST(test_IdName_noprefix_read_write_l3v2)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_statistics_l3v2.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  Parameter* p = doc->getModel()->getParameter(0);

  DistribSBasePlugin * plug = static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));
  Uncertainty * uncert = plug->getUncertainty(0);
  fail_unless(uncert != NULL);

  fail_unless(uncert->setId("u1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(uncert->setName("u1name") == LIBSBML_OPERATION_SUCCESS);

  std::string actual = writeSBMLToStdString(doc);
  delete doc;

  fail_unless(actual.find("<distrib:uncertainty id=\"u1\" name=\"u1name\">") != std::string::npos);
  fail_unless(actual.find("distrib:id=") == std::string::npos);
  fail_unless(actual.find("distrib:name=") == std::string::npos);

  doc = readSBMLFromString(actual.c_str());
  p = doc->getModel()->getParameter(0);
  plug = static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));
  fail_unless(plug->getUncertainty(0)->getId() == "u1");
  fail_unless(plug->getUncertainty(0)->getName() == "u1name");
  delete doc;
}
END_TEST


START_TEST(test_IdName_duplicate_id_logged_once_l3v2)
{
  std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string("distrib_statistics_l3v2.xml");
  SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
  Parameter* p = doc->getModel()->getParameter(0);

  DistribSBasePlugin * plug = static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));
  Uncertainty * uncert = plug->getUncertainty(0);
  fail_unless(uncert != NULL);
  fail_unless(uncert->setId("u1") == LIBSBML_OPERATION_SUCCESS);

  std::string text = writeSBMLToStdString(doc);
  delete doc;

  // give the uncertainty a distrib:id that differs from its plain id
  const std::string target = "<distrib:uncertainty id=\"u1\"";
  const std::size_t pos = text.find(target);
  fail_unless(pos != std::string::npos);
  text.insert(pos + target.size(), " distrib:id=\"u2\"");

  doc = readSBMLFromString(text.c_str());
  unsigned int duplicates = 0;
  for (unsigned int n = 0; n < doc->getNumErrors(); n++)
  {
    if (doc->getError(n)->getMessage().find("twice with different values") != std::string::npos)
    {
      duplicates++;
    }
  }
  fail_unless(duplicates == 1);
  fail_unless(doc->getNumErrors() == 1);
  delete doc;
}
END_TEST


Suite *
create_suite_test_IdName(void)
{
  Suite *suite = suite_create("TestIdName");
  TCase *tcase = tcase_create("TestIdName");

  tcase_add_test(tcase, test_IdName_prefix_read_write_l3v1);
  tcase_add_test(tcase, test_IdName_noprefix_read_write_l3v2);
  tcase_add_test(tcase, test_IdName_duplicate_id_logged_once_l3v2);


  suite_add_tcase(suite, tcase);

  return suite;
}

CK_CPPEND