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
#include <sbml/math/L3Parser.h>

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

  ASTNode *node = SBML_parseFormula("0.0");
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

  ASTNode *node = SBML_parseFormula("1.0");
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

  ASTNode *node = SBML_parseFormula("0.0");
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

  ASTNode *node = SBML_parseFormula("1.0");

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

  ASTNode *node = SBML_parseFormula("5.1");

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

  ASTNode *node = SBML_parseFormula("1.0 + 8.2*x");

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

  ASTNode *node = SBML_parseFormula("4.1");

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

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("(-4.1)/x^2.0", &ps);

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

  ASTNode * node = SBML_parseL3Formula("4.1");

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

  ASTNode * node = SBML_parseL3Formula("0.0");

  ASTNode *deriv = n->derivative(x);
  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_minus)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <minus/>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 2.0 </cn>"
    "  </apply>"
    "    <ci> y </ci>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  ASTNode * node = SBML_parseL3Formula("2.0");

  ASTNode *deriv = n->derivative(x);
  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_minus1)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <minus/>"
    "    <ci> y </ci>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <ci> x </ci>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("(-2.0*x)", &ps);

  ASTNode *deriv = n->derivative(x);
  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_minus2)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <minus/>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 2 </cn>"
    "  </apply>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <ci> x </ci>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  ASTNode * node = SBML_parseL3Formula("2.0-(2.0*x)");

  ASTNode *deriv = n->derivative(x);
  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_power)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <power/>"
    "    <ci> x </ci>"
    "  <apply>"
    "    <minus/>"
    "    <cn> 2 </cn>"
    "    <cn> 2 </cn>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  ASTNode * node = SBML_parseL3Formula("0.0");

  ASTNode *deriv = n->derivative(x);
  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_power1)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <power/>"
    "    <ci> x </ci>"
    "    <cn> 2 </cn>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  ASTNode * node = SBML_parseL3Formula("2.0*x");

  ASTNode *deriv = n->derivative(x);
  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_power2)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <power/>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 2 </cn>"
    "  </apply>"
    "    <cn> 3 </cn>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  ASTNode * node = SBML_parseL3Formula("3.0*((2.0*x)^2.0)");

  ASTNode *deriv = n->derivative(x);
  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_root)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <root/>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 2 </cn>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("0.5*((2.0*x)^-0.5)", &ps);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_root1)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <root/>"
    "    <degree><cn> 4 </cn></degree>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 2 </cn>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("0.25*((2.0*x)^-0.75)", &ps);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_log)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <log/>"
    "    <ci> x </ci>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("1.0/(x*ln(10.0))", &ps);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_log1)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <log/>"
    "    <logbase><cn> 4 </cn></logbase>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 2 </cn>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("2.0/(ln(4.0)*(2.0*x))", &ps);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_ln)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <ln/>"
    "    <ci> x </ci>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("1.0/x", &ps);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_ln1)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <ln/>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 2 </cn>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("2.0/(2.0*x)", &ps);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST


START_TEST(test_deriv_exp)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <exp/>"
    "    <ci> x </ci>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("exp(x)", &ps);

  ASTNode *deriv = n->derivative(x);

  fail_unless(deriv->exactlyEqual(*node) == true);
  delete n;
  delete node;
  delete deriv;
}
END_TEST

START_TEST(test_deriv_exp1)
{
  ASTNode *n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <exp/>"
    "  <apply>"
    "    <times/>"
    "    <ci> x </ci>"
    "    <cn> 2 </cn>"
    "  </apply>"
    "  </apply>"
    "</math>"
  );

  fail_unless(n != NULL);
  const std::string& x = "x";

  L3ParserSettings ps;
  ps.setParseCollapseMinus(true);
  ASTNode * node = SBML_parseL3FormulaWithSettings("2.0*exp(2.0*x)", &ps);

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
  tcase_add_test(tcase, test_deriv_minus);
  tcase_add_test(tcase, test_deriv_minus1);
  tcase_add_test(tcase, test_deriv_minus2);
  tcase_add_test(tcase, test_deriv_power);
  tcase_add_test(tcase, test_deriv_power1);
  tcase_add_test(tcase, test_deriv_power2);
  tcase_add_test(tcase, test_deriv_root);
  tcase_add_test(tcase, test_deriv_root1);
  tcase_add_test(tcase, test_deriv_log);
  tcase_add_test(tcase, test_deriv_log1);
  tcase_add_test(tcase, test_deriv_ln);
  tcase_add_test(tcase, test_deriv_ln1);
  tcase_add_test(tcase, test_deriv_exp);
  tcase_add_test(tcase, test_deriv_exp1);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

