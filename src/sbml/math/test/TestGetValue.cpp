/**
 * \file    TestGetValue.cpp
 * \brief   Test the getValue function
 * \author  Sarah Keating
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

#include <limits>
#include <iostream>
#include <cstdio>
#include <cstring>

#include <check.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond */

CK_CPPSTART


START_TEST (test_getValue_operator)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <apply>"
     "    <plus/>"
     "    <cn> 2 </cn>"
     "    <cn> 5 </cn>"
     "  </apply>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isNaN(n->getValue()));

  delete n;

  n = new ASTNode(AST_MINUS);
  ASTNode *n1 = new ASTNode(AST_INTEGER);
  n1->setValue(2);
  ASTNode *n2 = new ASTNode(AST_INTEGER);
  n2->setValue(3);
  n->addChild(n1);
  n->addChild(n2);
  fail_unless(util_isNaN(n->getValue()));

  delete n;
}
END_TEST


START_TEST (test_getValue_integer)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <cn type=\"integer\"> 6 </cn>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isEqual(n->getValue(), 6));

  delete n;

  n = new ASTNode(AST_INTEGER);
  n->setValue((int)(7));
  fail_unless(util_isEqual(n->getValue(), 7));

  delete n;
}
END_TEST


START_TEST (test_getValue_real)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <cn> 0.6 </cn>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isEqual(n->getValue(), 0.6));

  delete n;

  n = new ASTNode(AST_REAL);
  n->setValue(0.75);
  fail_unless(util_isEqual(n->getValue(), 0.75));

  delete n;
}
END_TEST


START_TEST (test_getValue_exponential)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <cn type=\"e-notation\"> 6.3 <sep/> 2 </cn>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isEqual(n->getValue(), 630));

  delete n;

  n = new ASTNode(AST_REAL_E);
  n->setValue(4.12, (long)(2));
  fail_unless(util_isEqual(n->getValue(), 412));

  delete n;
}
END_TEST


START_TEST (test_getValue_rational)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <cn type=\"rational\"> 6 <sep/> 4 </cn>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isEqual(n->getValue(), 1.5));

  delete n;

  n = new ASTNode(AST_RATIONAL);
  n->setValue((long)(3), (long)(2));
  fail_unless(util_isEqual(n->getValue(), 1.5));

  delete n;
}
END_TEST


START_TEST (test_getValue_name)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <ci> x </ci>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isNaN(n->getValue()));

  delete n;

  n = new ASTNode(AST_NAME);
  n->setName("w");
  fail_unless(util_isNaN(n->getValue()));

  delete n;
}
END_TEST


START_TEST (test_getValue_name_avogadro)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <csymbol encoding=\"text\" definitionURL=\"http://www.sbml.org/sbml/symbols/avogadro\"> x </csymbol>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isEqual(n->getValue(), 6.02214179e23));

  delete n;

  n = new ASTNode(AST_NAME_AVOGADRO);
  n->setName("w");
  fail_unless(util_isEqual(n->getValue(), 6.02214179e23));

  delete n;
}
END_TEST


START_TEST (test_getValue_name_time)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <csymbol encoding=\"text\" definitionURL=\"http://www.sbml.org/sbml/symbols/time\"> x </csymbol>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isNaN(n->getValue()));

  delete n;

  n = new ASTNode(AST_NAME_TIME);
  n->setName("w");
  fail_unless(util_isNaN(n->getValue()));

  delete n;
}
END_TEST


START_TEST (test_getValue_constant_e)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <exponentiale/>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isEqual(n->getValue(), 2.71828182));

  delete n;

  n = new ASTNode(AST_CONSTANT_E);
  fail_unless(util_isEqual(n->getValue(), 2.71828182));

  delete n;
}
END_TEST


START_TEST (test_getValue_constant_false)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <false/>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isEqual(n->getValue(), 0));

  delete n;

  n = new ASTNode(AST_CONSTANT_FALSE);
  fail_unless(util_isEqual(n->getValue(), 0));

  delete n;
}
END_TEST


START_TEST (test_getValue_constant_pi)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <pi/>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isEqual(n->getValue(), 3.14159292));

  delete n;

  n = new ASTNode(AST_CONSTANT_PI);
  fail_unless(util_isEqual(n->getValue(), 3.14159292));

  delete n;
}
END_TEST


START_TEST (test_getValue_constant_true)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <true/>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isEqual(n->getValue(), 1));

  delete n;

  n = new ASTNode(AST_CONSTANT_TRUE);
  fail_unless(util_isEqual(n->getValue(), 1));

  delete n;
}
END_TEST


START_TEST (test_getValue_function)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "  <apply>"
     "    <leq/>"
     "    <cn> 2 </cn>"
     "    <cn> 5 </cn>"
     "  </apply>"
     "</math>"
    );

  fail_unless( n != NULL );
  fail_unless(util_isNaN(n->getValue()));

  delete n;

  n = new ASTNode(AST_FUNCTION_POWER);
  ASTNode *n1 = new ASTNode(AST_INTEGER);
  n1->setValue(2);
  ASTNode *n2 = new ASTNode(AST_INTEGER);
  n2->setValue(3);
  n->addChild(n1);
  n->addChild(n2);
  fail_unless(util_isNaN(n->getValue()));

  delete n;
}
END_TEST


Suite *
create_suite_TestGetValue ()
{
  Suite *suite = suite_create("TestGetValue");
  TCase *tcase = tcase_create("TestGetValue");

  tcase_add_test( tcase, test_getValue_operator          );
  tcase_add_test( tcase, test_getValue_integer           );
  tcase_add_test( tcase, test_getValue_real              );
  tcase_add_test( tcase, test_getValue_exponential       );
  tcase_add_test( tcase, test_getValue_rational          );
  tcase_add_test( tcase, test_getValue_name              );
  tcase_add_test( tcase, test_getValue_name_avogadro     );
  tcase_add_test( tcase, test_getValue_name_time         );
  tcase_add_test( tcase, test_getValue_constant_e        );
  tcase_add_test( tcase, test_getValue_constant_false    );
  tcase_add_test( tcase, test_getValue_constant_pi       );
  tcase_add_test( tcase, test_getValue_constant_true     );
  tcase_add_test( tcase, test_getValue_function          );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

