/**
 * \file    TestCheckConsistency.cpp
 * \brief   Tests for the Comp extension objects
 * \author  Sarah Keating
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

#include <sbml/packages/comp/extension/CompSBMLDocumentPlugin.h>
#include <sbml/conversion/SBMLConverterRegistry.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>
#include <sbml/packages/comp/validator/CompSBMLErrorTable.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

START_TEST (test_comp_pass_all)
{
  string filename(TestDataDirectory);
  string cfile = filename + "1010101-pass-00-01.xml";  
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());
  unsigned int nerrors = doc->checkConsistency();

  fail_unless (nerrors == 0);

  delete doc;
}
END_TEST


START_TEST (test_comp_fail_20212)
{
  string filename(TestDataDirectory);
  string cfile = filename + "1020212-fail-01-01.xml";  
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());
  doc->checkConsistency();
  unsigned int nerrors = doc->getNumErrors();

  fail_unless (nerrors == 1);
  fail_unless (doc->getError(0)->getErrorId() == CompAttributeRequiredMustBeTrue);

  delete doc;
}
END_TEST

START_TEST (test_comp_fail_90104)
{
  //string filename(TestDataDirectory);
  //string cfile = filename + "invalid.xml";  
  //SBMLDocument* doc = readSBMLFromFile(cfile.c_str());
  //doc->checkConsistency();
  //unsigned int nerrors = doc->getNumErrors();

  //fail_unless (nerrors == 1);
  //fail_unless (doc->getError(0)->getErrorId() == 1090104);

  //delete doc;
}
END_TEST

START_TEST (test_comp_fail_whenflat)
{
  string filename(TestDataDirectory);
  string cfile = filename + "valid.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());
  doc->checkConsistency();
  unsigned int nerrors = doc->getNumErrors();

  fail_unless (nerrors == 6);
  fail_unless (doc->getError(0)->getErrorId() == 1090106);
  fail_unless (doc->getError(1)->getErrorId() == 1090105);
  fail_unless (doc->getError(2)->getErrorId() == 21111);
  fail_unless (doc->getError(3)->getErrorId() == 21111);
  fail_unless (doc->getError(4)->getErrorId() == 21111);
  fail_unless (doc->getError(5)->getErrorId() == 21111);

  delete doc;
}
END_TEST

/* an assignment rule of an undefined variable inside a model definition */
static const char* INVALID_MODEL_DEFINITION =
  "<?xml version='1.0' encoding='UTF-8'?>"
  "<sbml xmlns='http://www.sbml.org/sbml/level3/version1/core' "
  "xmlns:comp='http://www.sbml.org/sbml/level3/version1/comp/version1' "
  "level='3' version='1' comp:required='true'>"
  "<model id='top'><comp:listOfSubmodels>"
  "<comp:submodel comp:id='s0' comp:modelRef='md0'/>"
  "</comp:listOfSubmodels></model>"
  "<comp:listOfModelDefinitions><comp:modelDefinition id='md0'>"
  "<listOfRules><assignmentRule variable='x'>"
  "<math xmlns='http://www.w3.org/1998/Math/MathML'><cn>1</cn></math>"
  "</assignmentRule></listOfRules>"
  "</comp:modelDefinition></comp:listOfModelDefinitions></sbml>";


START_TEST (test_comp_fail_moddef_repeated)
{
  SBMLDocument* doc = readSBMLFromString(INVALID_MODEL_DEFINITION);
  doc->getErrorLog()->clearLog();

  fail_unless (doc->checkConsistency() > 0);
  fail_unless (doc->getErrorLog()->contains(InvalidAssignRuleVariable));

  /* a second check of the same document checks the model definitions again */
  doc->getErrorLog()->clearLog();
  fail_unless (doc->checkConsistency() > 0);
  fail_unless (doc->getErrorLog()->contains(InvalidAssignRuleVariable));

  /* and so does a copy of a document which was checked */
  SBMLDocument copy(*doc);
  fail_unless (copy.checkConsistency() > 0);
  fail_unless (copy.getErrorLog()->contains(InvalidAssignRuleVariable));

  delete doc;
}
END_TEST


START_TEST (test_comp_fail_whenflat_repeated)
{
  string filename(TestDataDirectory);
  string cfile = filename + "valid.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());
  doc->checkConsistency();
  fail_unless (doc->getNumErrors() == 6);

  doc->getErrorLog()->clearLog();
  doc->checkConsistency();
  fail_unless (doc->getNumErrors() == 6);
  fail_unless (doc->getError(0)->getErrorId() == 1090106);
  fail_unless (doc->getError(1)->getErrorId() == 1090105);
  fail_unless (doc->getError(2)->getErrorId() == 21111);

  delete doc;
}
END_TEST

Suite *
create_suite_TestCheckConsistency(void)
{ 
  TCase *tcase = tcase_create("SBMLCompCheckConsistency");
  Suite *suite = suite_create("SBMLCompCheckConsistency");

  tcase_add_test(tcase, test_comp_pass_all);
  tcase_add_test(tcase, test_comp_fail_20212);
  tcase_add_test(tcase, test_comp_fail_90104);
  tcase_add_test(tcase, test_comp_fail_whenflat);
  tcase_add_test(tcase, test_comp_fail_moddef_repeated);
  tcase_add_test(tcase, test_comp_fail_whenflat_repeated);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

