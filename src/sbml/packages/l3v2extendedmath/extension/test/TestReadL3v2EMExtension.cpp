/**
 * @file    TestReadL3v2EMExtension.cpp
 * @brief   Unit tests of writing L3v2EMExtension 
 * @author  Sarah M Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * Copyright (C) 2009-2011 jointly by the following organizations: 
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
 * ---------------------------------------------------------------------- -->*/


#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/common/extern.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/l3v2extendedmath/common/L3v2extendedmathExtensionTypes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART

extern char *TestDataDirectory;

START_TEST (test_L3v2EMExtension_read_L3V1V1)
{
  string filename = string(TestDataDirectory) + "l3v2em-example1.xml";
  SBMLDocument *document = readSBMLFromFile(filename.c_str());
  
  fail_unless(document->getPackageName() == "core");

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getPackageName() == "core");

  const ASTNode* math = model->getRule(0)->getMath();

  const char * formula = SBML_formulaToL3String(math);
  fail_unless(strcmp(formula, "quotient(5, 5)") == 0);
  safe_free((void*)(formula));

  delete document;  
}
END_TEST


START_TEST (test_L3v2EMExtension_read_L3V1V1_unknown_elements)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:l3v2extendedmath=\"http://www.sbml.org/sbml/level3/version1/l3v2extendedmath/version1\" level=\"3\" version=\"1\" l3v2extendedmath:required=\"true\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <sarah/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);
  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(document->getNumErrors() == 1);

  delete document;
}
END_TEST


Suite *
create_suite_ReadL3v2EMExtension (void)
{
  Suite *suite = suite_create("ReadL3v2EMExtension");
  TCase *tcase = tcase_create("ReadL3v2EMExtension");

  tcase_add_test( tcase, test_L3v2EMExtension_read_L3V1V1);
  tcase_add_test( tcase, test_L3v2EMExtension_read_L3V1V1_unknown_elements);
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
