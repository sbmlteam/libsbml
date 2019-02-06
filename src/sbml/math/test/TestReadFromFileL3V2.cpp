/**
 * @file    TestReadFromFileL3V2.cpp
 * @brief   Tests for reading MathML from files into ASTNodes.
 * @author  Sarah Keating
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
 * Copyright (C) 2009-2013 jointly by the following organizations: 
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


#include <sbml/common/common.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <sbml/math/ASTNode.h>



#include <string>

#include <check.h>

static bool
equals(const char* expected, const char* actual)
{
  if (!strcmp(expected, actual)) return true;

  printf("\nStrings are not equal:\n");
  printf("Expected:\n[%s]\n", expected);
  printf("Actual:\n[%s]\n", actual);

  return false;
}

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;


START_TEST (test_read_MathML_L3V2)
{
  const char *expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "  <apply>\n"
    "    <max/>\n"
    "    <cn> 2 </cn>\n"
    "    <cn> 3 </cn>\n"
    "  </apply>\n"
    "</math>";

  std::string filename(TestDataDirectory);
  filename += "L3V2Math.xml";


  SBMLDocument *d = readSBML(filename.c_str());

  if (d == NULL)
  {
    fail("readSBML(\"L3V2Math.xml\") returned a NULL pointer.");
  }

  Model * m = d->getModel();
  fail_unless( m != NULL, NULL );

  InitialAssignment *ia = m->getInitialAssignment(0);
  const ASTNode * math = ia->getMath();

  std::string out = writeMathMLToStdString(math);

  fail_unless(equals(expected, out.c_str()));

  const char *expected1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "  <apply>\n"
    "    <times/>\n"
    "    <cn> 2 </cn>\n"
    "    <cn> 3 </cn>\n"
    "  </apply>\n"
    "</math>";

  ia = m->getInitialAssignment(1);
  math = ia->getMath();
  fail_unless(math->getNumPlugins() == 0);

  out = writeMathMLToStdString(math);

  fail_unless(equals(expected1, out.c_str()));

  delete d;
}
END_TEST


Suite *
create_suite_TestReadFromFileL3V2(void)
{ 
  Suite *suite = suite_create("test-data/L3V2Math.xml");
  TCase *tcase = tcase_create("test-data/L3V2Math.xml");


  tcase_add_test(tcase, test_read_MathML_L3V2);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

