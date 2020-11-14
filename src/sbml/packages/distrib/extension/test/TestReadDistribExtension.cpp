/**
 * @file    TestReadDistribExtension.cpp
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
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/packages/distrib/common/DistribExtensionTypes.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */

CK_CPPSTART

extern char *TestDataDirectory;

START_TEST(test_DistribExtension_read_and_validate)
{
  
  const char* files[4] = { "distrib1-defaultns.xml",  "distrib1ns.xml", 
    "distrib2-defaultns.xml", "distrib2ns.xml" };
  
  

  for (int i = 0; i < 4; ++ i)
  {
    std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string(files[i]);
    SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
    fail_unless(doc != NULL);
    fail_unless(doc->getModel() != NULL);
    doc->setConsistencyChecks(LIBSBML_CAT_UNITS_CONSISTENCY, false);
    unsigned int nerrors = doc->checkConsistency();
    fail_unless(nerrors == 0);
    delete doc;
  }

}
END_TEST

Suite *
create_suite_ReadDistribExtension(void)
{
  Suite *suite = suite_create("ReadDistribExtension");
  TCase *tcase = tcase_create("ReadDistribExtension");

  tcase_add_test(tcase, test_DistribExtension_read_and_validate);
  suite_add_tcase(suite, tcase);

  return suite;
}

CK_CPPEND
