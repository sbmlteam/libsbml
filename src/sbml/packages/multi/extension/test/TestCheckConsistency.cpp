/**
 * \file    TestCheckConsistency.cpp
 * \brief   Tests for the Multi extension objects
 * \author  Fengkai Zhang
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

#include <sbml/packages/multi/extension/MultiSBMLDocumentPlugin.h>
#include <sbml/conversion/SBMLConverterRegistry.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>
#include <sbml/packages/multi/validator/MultiSBMLErrorTable.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

START_TEST (test_multi_pass_all)
{
  string filename(TestDataDirectory);
  string cfile = filename + "7010101-pass-00-01.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());
  unsigned int nerrors = doc->checkConsistency();

  fail_unless (nerrors == 0);

  delete doc;
}
END_TEST


Suite *
create_suite_MultiCheckConsistency(void)
{
  TCase *tcase = tcase_create("MultiCheckConsistency");
  Suite *suite = suite_create("MultiCheckConsistency");

  tcase_add_test(tcase, test_multi_pass_all);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

