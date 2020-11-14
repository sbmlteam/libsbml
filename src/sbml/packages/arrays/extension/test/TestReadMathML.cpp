/**
 * \file    TestReadMathML.cpp
 * \brief   Read MathML unit tests
 * \author  Lucian Smith, from orig by Ben Bornstein
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

#include <iostream>
#include <cstring>
#include <check.h>

#include <sbml/util/util.h>

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include <sbml/xml/XMLNode.h>

LIBSBML_CPP_NAMESPACE_USE

/**
 * Wraps the string s in the appropriate XML or MathML boilerplate.
 */
#define XML_HEADER     "<?xml version='1.0' encoding='UTF-8'?>\n"
#define MATHML_HEADER  "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n"
#define MATHML_HEADER_UNITS  "<math xmlns='http://www.w3.org/1998/Math/MathML'\n"
#define MATHML_HEADER_UNITS2  " xmlns:sbml='http://www.sbml.org/sbml/level3/version2/core'>\n"
#define MATHML_FOOTER  "</math>"

#define wrapXML(s)     XML_HEADER s
#define wrapMathML(s)  XML_HEADER MATHML_HEADER s MATHML_FOOTER
#define wrapMathMLUnits(s)  XML_HEADER MATHML_HEADER_UNITS MATHML_HEADER_UNITS2 s MATHML_FOOTER


static ASTNode* N;
static char*    F;


static void
ReadMathML_setup ()
{
  N = NULL;
  F = NULL;
}


static void
ReadMathML_teardown ()
{
  delete N;
  free(F);
}


CK_CPPSTART


START_TEST (test_element_vector_1)
{
  //const char* s = wrapMathML
  //  (
  //    "<apply>"
  //    "  <cos/>"
  //    "  <cn> 5.7 </cn>"
  //    "</apply>"
  //    );


  const char* s = wrapMathML
  (
    "<vector>"
    "  <cn> 5.7 </cn>"
    "  <cn> 5.7 </cn>"
    "  <cn> 5.7 </cn>"
    "  <cn> 5.7 </cn>"
    "  <cn> 5.7 </cn>"
    "  <cn> 5.7 </cn>"
    "  <cn> 3.2 </cn>"
    "  <cn> 3.2 </cn>"
    "  <cn> 3.2 </cn>"
    "  <cn> 3.2 </cn>"
    "  <cn> 3.2 </cn>"
    "</vector>"
  );


  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless(N->getType() == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless(N->getNumChildren() == 11);
  ASTNode* c = N->getChild(0);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 5.7);
  c = N->getChild(1);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 5.7);
  c = N->getChild(2);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 5.7);
  c = N->getChild(3);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 5.7);
  c = N->getChild(4);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 5.7);
  c = N->getChild(5);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 5.7);
  c = N->getChild(6);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 3.2);
  c = N->getChild(7);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 3.2);
  c = N->getChild(8);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 3.2);
  c = N->getChild(9);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 3.2);
  c = N->getChild(10);
  fail_unless(c->getType() == AST_REAL);
  fail_unless(c->getValue() == 3.2);
}
END_TEST


START_TEST (test_element_selector_1)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <selector/>"
    "  <vector>"
    "    <cn> 5.7 </cn>"
    "    <cn> 5.7 </cn>"
    "    <cn> 5.7 </cn>"
    "    <cn> 5.7 </cn>"
    "    <cn> 5.7 </cn>"
    "    <cn> 5.7 </cn>"
    "    <cn> 3.2 </cn>"
    "    <cn> 3.2 </cn>"
    "    <cn> 3.2 </cn>"
    "    <cn> 3.2 </cn>"
    "    <cn> 3.2 </cn>"
    "  </vector>"
    "  <ci> d0 </ci>"
    "</apply>"
  );


  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_LINEAR_ALGEBRA_SELECTOR );
  fail_unless( N->getNumChildren() == 2       );

  ASTNode* c = N->getChild(0);
  fail_unless(c->getType() == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless(c->getNumChildren() == 11);
  ASTNode* c2 = c->getChild(0);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 5.7);
  c2 = c->getChild(1);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 5.7);
  c2 = c->getChild(2);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 5.7);
  c2 = c->getChild(3);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 5.7);
  c2 = c->getChild(4);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 5.7);
  c2 = c->getChild(5);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 5.7);
  c2 = c->getChild(6);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 3.2);
  c2 = c->getChild(7);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 3.2);
  c2 = c->getChild(8);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 3.2);
  c2 = c->getChild(9);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 3.2);
  c2 = c->getChild(10);
  fail_unless(c2->getType() == AST_REAL);
  fail_unless(c2->getValue() == 3.2);

  c = N->getChild(1);
  fail_unless(c->getType() == AST_NAME);
  fail_unless(!strcmp(c->getName(), "d0"));

}
END_TEST


Suite *
create_suite_ReadMathML ()
{
  Suite *suite = suite_create("ReadMathML");
  TCase *tcase = tcase_create("ReadMathML");

  tcase_add_checked_fixture( tcase, ReadMathML_setup, ReadMathML_teardown);

  tcase_add_test( tcase, test_element_vector_1);
  tcase_add_test( tcase, test_element_selector_1);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
