/**
 * \file    TestInferRnFunctions.cpp
 * \brief   Test extra functions for inferring rns
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


START_TEST (test_deriv_const)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "    <cn> 4.1 </cn>"
     "</math>"
    );

  fail_unless( n != NULL );
  const std::string& x = "x";

  char* formula = "0.0";
  ASTNode *node = SBML_parseFormula(formula);
  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);

  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_var)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "    <ci> x </ci>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  char* formula = "1.0";
  ASTNode *node = SBML_parseFormula(formula);
  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);

  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_var1)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "    <ci> y </ci>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  char* formula = "0.0";
  ASTNode *node = SBML_parseFormula(formula);
  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);

  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_plus)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <plus/>"
    "    <ci> x </ci>"
    "    <cn> 4.1 </cn>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  char* formula = "1.0";
  ASTNode *node = SBML_parseFormula(formula);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_plus1)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <plus/>"
    "    <ci> x </ci>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 4.1 </cn>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  char* formula = "5.1";
  ASTNode *node = SBML_parseFormula(formula);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_plus2)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <plus/>"
    "    <ci> x </ci>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <ci> x </ci>"
    "    <cn> 4.1 </cn>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  char* formula = "1.0 + 8.2*x";
  ASTNode *node = SBML_parseFormula(formula);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_times)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 4.1 </cn>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  char* formula = "4.1";
  ASTNode *node = SBML_parseFormula(formula);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_divide)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <divide/>"
    "    <cn> 4.1 </cn>"
    "    <ci> x </ci>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  char* formula = "(-4.1)/x^2.0";
  L3ParserSettings * ps = new L3ParserSettings();
  ps->setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings(formula, ps);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_divide1)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <divide/>"
    "    <ci> x </ci>"
    "    <cn> 4.1 </cn>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  char* formula = "4.1/4.1^2.0";
  ASTNode * node = SBML_parseL3Formula(formula);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_divide2)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <divide/>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 1.0 </cn>"
    "  </apply>"
    "    <ci> x </ci>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  char* formula = "0.0";
  ASTNode * node = SBML_parseL3Formula(formula);

  ASTNode *deriv = n->derivative(x);
  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


Suite *
create_suite_TestDerivativeFunctions()
{
  Suite *suite = suite_create("TestDerivFunctions");
  TCase *tcase = tcase_create("TestDerivFunctions");

  tcase_add_test(tcase, test_deriv_const);
  tcase_add_test(tcase, test_deriv_var);
  tcase_add_test(tcase, test_deriv_var1);
  tcase_add_test(tcase, test_deriv_plus);
  tcase_add_test(tcase, test_deriv_plus1);
  tcase_add_test(tcase, test_deriv_plus2);
  tcase_add_test(tcase, test_deriv_times);
  tcase_add_test(tcase, test_deriv_divide);
  tcase_add_test(tcase, test_deriv_divide1);
  tcase_add_test(tcase, test_deriv_divide2);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

