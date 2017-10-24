/**
 * \file    TestCompFlatteningConverter.cpp
 * \brief   Implementation of the Tests for the Comp flattening converter
 * \author  Frank T. Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2017 jointly by the following organizations:
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

#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

START_TEST (test_comp_upconvert)
{
  string filename(TestDataDirectory);
  //string filename("C:\\Development\\libsbml\\src\\sbml\\packages\\comp\\util\\test\\test-data\\");

  filename += "aggregate.xml";  
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());

  fail_unless(doc->setLevelAndVersion(3, 2));


  delete doc;
}
END_TEST


START_TEST(test_comp_downconvert)
{
  string filename(TestDataDirectory);
  //string filename("C:\\Development\\libsbml\\src\\sbml\\packages\\comp\\util\\test\\test-data\\");

  filename += "l3v2_modeldef_rxn.xml";
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());

  fail_unless(doc->setLevelAndVersion(3, 1));


  delete doc;
}
END_TEST



Suite *
create_suite_TestLevelVersionConversion (void)
{ 
  TCase *tcase = tcase_create("SBMLCompLevelVersionConversion");
  Suite *suite = suite_create("SBMLCompLevelVersionConversion");
  
  tcase_add_test(tcase, test_comp_upconvert);
  tcase_add_test(tcase, test_comp_downconvert);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

