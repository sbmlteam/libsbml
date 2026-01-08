/**
 * \file    TestDerivativeFunctions.cpp
 * \brief   Test extra functions for differentiation
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
#include <sbml/math/L3FormulaFormatter.h>
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
static bool
equals(const char* expected, const char* actual)
{
  if (!strcmp(expected, actual)) return true;

  printf("\nStrings are not equal:\n");
  printf("Expected:\n[%s]\n", expected);
  printf("Actual:\n[%s]\n", actual);

  return false;
}

static bool
formulas_equal(const char* expected, ASTNode* actual)
{
  return equals(expected, SBML_formulaToL3String(actual));
}

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

  // test 0 args:
  n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <plus/>"
    "  </apply>"
    "</math>"
  );

  deriv = n->derivative(x);

  delete n;
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


START_TEST(test_deriv_plus3)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("x + foo(x)");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("1.0");

    ASTNode* deriv = n->derivative(x);

    fail_unless(deriv->exactlyEqual(*node) == true);
    delete n;
    delete node;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_plus4)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("foo(x) + x");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("1.0");

    ASTNode* deriv = n->derivative(x);

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

  // also check that it does not crash on no args:
  n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <times/>"
    "  </apply>"
    "</math>"
  );

  deriv = n->derivative(x);

  // should be 0
  fail_unless(deriv->isNumber() == true);
  fail_unless(deriv->getValue() == 0.0);

  delete n;
  delete deriv;

}
END_TEST

START_TEST(test_deriv_times1)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("x * foo(x)");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("foo(x)");

    ASTNode* deriv = n->derivative(x);

    fail_unless(deriv->exactlyEqual(*node) == true);
    delete n;
    delete node;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_times2)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("foo(x) * x");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("foo(x)");

    ASTNode* deriv = n->derivative(x);

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

  // test 0 args: 
  n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <divide/>"
    "  </apply>"
    "</math>"
  );
  deriv = n->derivative(x);

  delete n;
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

START_TEST(test_deriv_divide3)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("foo(x) / x");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("-1.0 * foo(x)/x^2.0");

    ASTNode* deriv = n->derivative(x);

    fail_unless(deriv->exactlyEqual(*node) == true);
    delete n;
    delete node;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_divide4)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("x/foo(x)");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("foo(x)/((foo(x))^2.0)");

    ASTNode* deriv = n->derivative(x);
    //cout << SBML_formulaToL3String(deriv) << endl;
    //cout << SBML_formulaToL3String(node) << endl;

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


  // test 0 args
  n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <minus/>"
    "  </apply>"
    "</math>"
  );

  deriv = n->derivative(x);

  delete n;
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

START_TEST(test_deriv_minus3)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("foo(x) - x");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("-1.0");

    ASTNode* deriv = n->derivative(x);

    fail_unless(deriv->exactlyEqual(*node) == true);
    delete n;
    delete node;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_minus4)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("x - foo(x)");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("1.0");

    ASTNode* deriv = n->derivative(x);

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

  // test 0 arg
  n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <power/>"
    "  </apply>"
    "</math>"
  );

  deriv = n->derivative(x);
  delete n;
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

START_TEST(test_deriv_power3)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("foo(x)^6.0");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("6.0*foo(x)^5.0");

    ASTNode* deriv = n->derivative(x);

    fail_unless(deriv->exactlyEqual(*node) == true);
    delete n;
    delete node;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_power4)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("x^(foo(x))");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("1.0");

    ASTNode* deriv = n->derivative(x);

    //fail_unless(deriv->exactlyEqual(*node) == true);
    fail_unless(deriv == NULL);
    delete n;
    delete node;
    //delete deriv;
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

START_TEST(test_deriv_root2)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseFormula("x^(1.0/foo(x))");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseFormula("1.0");

    ASTNode* deriv = n->derivative(x);
    fail_unless(deriv == NULL);
    delete n;
    delete node;
    //delete deriv;
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
    "</math>"  );

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

  // test 0 args:
  n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <log/>"
    "  </apply>"
    "</math>");

  deriv = n->derivative(x);

  delete n;
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

START_TEST(test_deriv_log2)
{
    L3ParserSettings ps;
    ps.setParseLog(L3P_PARSE_LOG_AS_LOG10);
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3FormulaWithSettings("log(foo(x))", &ps);
    fail_unless(n != NULL);

    ASTNode* deriv = n->derivative(x);
    fail_unless(deriv == NULL);
    delete n;
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

START_TEST(test_deriv_ln2)
{
    L3ParserSettings ps;
    ps.setParseLog(L3P_PARSE_LOG_AS_LN);
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3FormulaWithSettings("ln(foo(x))", &ps);
    fail_unless(n != NULL);

    ASTNode* deriv = n->derivative(x);
    fail_unless(deriv == NULL);
    delete n;
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

  // test 0 args:
  n = readMathMLFromString(
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply>"
    "    <exp/>"
    "  </apply>"
    "</math>"
  );

  deriv = n->derivative(x);

  delete n;
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

START_TEST(test_deriv_exp2)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("exp(foo(x))");
    fail_unless(n != NULL);

    ASTNode* deriv = n->derivative(x);
    fail_unless(deriv == NULL);
    delete n;
}
END_TEST

START_TEST(test_deriv_abs)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("abs(x)");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseL3Formula("x/abs(x)");

    ASTNode* deriv = n->derivative(x);
    fail_unless(deriv->exactlyEqual(*node) == true);
    delete n;
    delete node;
    delete deriv;

    // test 0 args
    n = readMathMLFromString(
      "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
      "  <apply>"
      "    <abs/>"
      "  </apply>"
      "</math>"
    );

    deriv = n->derivative(x);

    delete n;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_abs1)
{
    //d(abs(A)/dx = dA/dx * (A/abs(A))
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("abs(x+2*x)");
    fail_unless(n != NULL);

    ASTNode* node = SBML_parseL3Formula("3.0*((x+2.0*x)/abs(x+2.0*x))");

    ASTNode* deriv = n->derivative(x);
    //cout << SBML_formulaToL3String(deriv) << endl;
    //cout << SBML_formulaToL3String(node) << endl;
    fail_unless(deriv->exactlyEqual(*node) == true);
    delete n;
    delete node;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_abs2)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("abs(foo(x))");
    fail_unless(n != NULL);

    ASTNode* deriv = n->derivative(x);
    fail_unless(deriv == NULL);
    delete n;
}
END_TEST

START_TEST(test_deriv_arccos)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("arccos(x)");
    fail_unless(n != NULL);

    const char* expected = "-1 * (1 - x^2)^-0.5";

    ASTNode* deriv = n->derivative(x);

    fail_unless(formulas_equal(expected, deriv) == true);
    delete n;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_arccos1)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("arccos(x^3)");
    fail_unless(n != NULL);
    ASTNode* deriv = n->derivative(x);

    const char* expected = "-3 * x^2 * (1 - x^3^2)^-0.5";

    fail_unless(formulas_equal(expected, deriv) == true);
    delete n;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_arccos2)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("arccos((x^3)+2*x)");
    fail_unless(n != NULL);
    ASTNode* deriv = n->derivative(x);

    const char* expected = "-2 * (1 - (x^3 + 2 * x)^2)^-0.5 + -3 * x^2 * (1 - (x^3 + 2 * x)^2)^-0.5";

    fail_unless(formulas_equal(expected, deriv) == true);
    delete n;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_arctan)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("arctan(x)");
    fail_unless(n != NULL);

    const char* expected = "(1 + x^2)^-1";

    ASTNode* deriv = n->derivative(x);

    fail_unless(formulas_equal(expected, deriv) == true);
    delete n;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_arctan1)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("arctan(x^2)");
    fail_unless(n != NULL);

    const char* expected = "2 * x * (1 + x^2^2)^-1";

    ASTNode* deriv = n->derivative(x);

    fail_unless(formulas_equal(expected, deriv) == true);
    delete n;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_arcsin)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("arcsin(x)");
    fail_unless(n != NULL);

    const char* expected = "(1 - x^2)^-0.5";

    ASTNode* deriv = n->derivative(x);

    fail_unless(formulas_equal(expected, deriv) == true);
    delete n;
    delete deriv;
}
END_TEST

START_TEST(test_deriv_arcsin1)
{
    const std::string& x = "x";
    ASTNode* n = SBML_parseL3Formula("arcsin(3*x^2+2*x)");
    fail_unless(n != NULL);

    const char* expected = "2 * (1 - (3 * x^2 + 2 * x)^2)^-0.5 + 6 * x * (1 - (3 * x^2 + 2 * x)^2)^-0.5";

    ASTNode* deriv = n->derivative(x);

    fail_unless(formulas_equal(expected, deriv) == true);
    delete n;
    delete deriv;
}
END_TEST


START_TEST(test_deriv_sin)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("sin(x)");
fail_unless(n != NULL);


const char* expected = "cos(x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_sin1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("sin(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "(2 + 6 * x) * cos(3 * x^2 + 2 * x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_cos)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("cos(x)");
fail_unless(n != NULL);


const char* expected = "-1 * sin(x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_cos1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("cos(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * sin(3 * x^2 + 2 * x) + -6 * x * sin(3 * x^2 + 2 * x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_tan)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("tan(x)");
fail_unless(n != NULL);


const char* expected = "cos(x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_tan1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("tan(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * cos(3 * x^2 + 2 * x)^-2 + 6 * x * cos(3 * x^2 + 2 * x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_cosh)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("cosh(x)");
fail_unless(n != NULL);


const char* expected = "sinh(x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_cosh1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("cosh(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * sinh(3 * x^2 + 2 * x) + 6 * x * sinh(3 * x^2 + 2 * x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_cot)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("cot(x)");
fail_unless(n != NULL);


const char* expected = "-1 * sin(x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_cot1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("cot(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * sin(3 * x^2 + 2 * x)^-2 + -6 * x * sin(3 * x^2 + 2 * x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_sinh)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("sinh(x)");
fail_unless(n != NULL);


const char* expected = "cosh(x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_sinh1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("sinh(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * cosh(3 * x^2 + 2 * x) + 6 * x * cosh(3 * x^2 + 2 * x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_tanh)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("tanh(x)");
fail_unless(n != NULL);


const char* expected = "cosh(x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_tanh1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("tanh(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * cosh(3 * x^2 + 2 * x)^-2 + 6 * x * cosh(3 * x^2 + 2 * x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_coth)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("coth(x)");
fail_unless(n != NULL);


const char* expected = "-1 * sinh(x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_coth1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("coth(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * sinh(3 * x^2 + 2 * x)^-2 + -6 * x * sinh(3 * x^2 + 2 * x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_sec)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("sec(x)");
fail_unless(n != NULL);


const char* expected = "sin(x) * cos(x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_sec1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("sec(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * cos(3 * x^2 + 2 * x)^-2 * sin(3 * x^2 + 2 * x) + 6 * x * cos(3 * x^2 + 2 * x)^-2 * sin(3 * x^2 + 2 * x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_csc)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("csc(x)");
fail_unless(n != NULL);


const char* expected = "-1 * cos(x) * sin(x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_csc1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("csc(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * sin(3 * x^2 + 2 * x)^-2 * cos(3 * x^2 + 2 * x) + -6 * x * sin(3 * x^2 + 2 * x)^-2 * cos(3 * x^2 + 2 * x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_sech)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("sech(x)");
fail_unless(n != NULL);


const char* expected = "-1 * sinh(x) * cosh(x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_sech1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("sech(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * cosh(3 * x^2 + 2 * x)^-2 * sinh(3 * x^2 + 2 * x) + -6 * x * cosh(3 * x^2 + 2 * x)^-2 * sinh(3 * x^2 + 2 * x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_csch)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("csch(x)");
fail_unless(n != NULL);


const char* expected = "-1 * cosh(x) * sinh(x)^-2";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_csch1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("csch(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * sinh(3 * x^2 + 2 * x)^-2 * cosh(3 * x^2 + 2 * x) + -6 * x * sinh(3 * x^2 + 2 * x)^-2 * cosh(3 * x^2 + 2 * x)";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arcsinh)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arcsinh(x)");
fail_unless(n != NULL);


const char* expected = "(1 + x^2)^-0.5";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arcsinh1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arcsinh(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * (1 + (3 * x^2 + 2 * x)^2)^-0.5 + 6 * x * (1 + (3 * x^2 + 2 * x)^2)^-0.5";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccosh)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccosh(x)");
fail_unless(n != NULL);


const char* expected = "(x^2 - 1)^-0.5";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccosh1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccosh(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * ((3 * x^2 + 2 * x)^2 - 1)^-0.5 + 6 * x * ((3 * x^2 + 2 * x)^2 - 1)^-0.5";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arctanh)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arctanh(x)");
fail_unless(n != NULL);


const char* expected = "(1 - x^2)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arctanh1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arctanh(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * (1 - (3 * x^2 + 2 * x)^2)^-1 + 6 * x * (1 - (3 * x^2 + 2 * x)^2)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccot)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccot(x)");
fail_unless(n != NULL);


const char* expected = "-1 * (1 + x^2)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccot1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccot(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * (1 + (3 * x^2 + 2 * x)^2)^-1 + -6 * x * (1 + (3 * x^2 + 2 * x)^2)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arcsec)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arcsec(x)");
fail_unless(n != NULL);


const char* expected = "x^-1 * (x^2 - 1)^-0.5";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arcsec1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arcsec(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * ((3 * x^2 + 2 * x)^2 - 1)^-0.5 * (3 * x^2 + 2 * x)^-1 + 6 * x * ((3 * x^2 + 2 * x)^2 - 1)^-0.5 * (3 * x^2 + 2 * x)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccsc)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccsc(x)");
fail_unless(n != NULL);


const char* expected = "-1 * x^-1 * (x^2 - 1)^-0.5";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccsc1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccsc(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * ((3 * x^2 + 2 * x)^2 - 1)^-0.5 * (3 * x^2 + 2 * x)^-1 + -6 * x * ((3 * x^2 + 2 * x)^2 - 1)^-0.5 * (3 * x^2 + 2 * x)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccoth)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccoth(x)");
fail_unless(n != NULL);


const char* expected = "(1 - x^2)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccoth1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccoth(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "2 * (1 - (3 * x^2 + 2 * x)^2)^-1 + 6 * x * (1 - (3 * x^2 + 2 * x)^2)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arcsech)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arcsech(x)");
fail_unless(n != NULL);


const char* expected = "-1 * x^-1 * (1 - x^2)^-0.5";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arcsech1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arcsech(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * (1 - (3 * x^2 + 2 * x)^2)^-0.5 * (3 * x^2 + 2 * x)^-1 + -6 * x * (1 - (3 * x^2 + 2 * x)^2)^-0.5 * (3 * x^2 + 2 * x)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccsch)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccsch(x)");
fail_unless(n != NULL);


const char* expected = "-1 * x^-1 * (1 + x^2)^-0.5";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_arccsch1)
{
const std::string& x = "x";
ASTNode* n = SBML_parseL3Formula("arccsch(3*x^2+2*x)");
fail_unless(n != NULL);


const char* expected = "-2 * (1 + (3 * x^2 + 2 * x)^2)^-0.5 * (3 * x^2 + 2 * x)^-1 + -6 * x * (1 + (3 * x^2 + 2 * x)^2)^-0.5 * (3 * x^2 + 2 * x)^-1";

ASTNode* deriv = n->derivative(x);

fail_unless(formulas_equal(expected, deriv) == true);
delete n;
delete deriv;



}
END_TEST

START_TEST(test_deriv_not_implemented )
{
    const std::string& x = "x";

    L3ParserSettings ps;
    ps.setParseCollapseMinus(true);
    ASTNode* node = SBML_parseL3FormulaWithSettings("foo(x)", &ps);

    ASTNode* deriv = node->derivative(x);

    fail_unless(deriv == NULL);
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
  tcase_add_test(tcase, test_deriv_plus3);
  tcase_add_test(tcase, test_deriv_plus4);
  tcase_add_test(tcase, test_deriv_times);
  tcase_add_test(tcase, test_deriv_times1);
  tcase_add_test(tcase, test_deriv_times2);
  tcase_add_test(tcase, test_deriv_divide);
  tcase_add_test(tcase, test_deriv_divide1);
  tcase_add_test(tcase, test_deriv_divide2);
  tcase_add_test(tcase, test_deriv_divide3);
  tcase_add_test(tcase, test_deriv_divide4);
  tcase_add_test(tcase, test_deriv_minus);
  tcase_add_test(tcase, test_deriv_minus1);
  tcase_add_test(tcase, test_deriv_minus2);
  tcase_add_test(tcase, test_deriv_minus3);
  tcase_add_test(tcase, test_deriv_minus4);
  tcase_add_test(tcase, test_deriv_power);
  tcase_add_test(tcase, test_deriv_power1);
  tcase_add_test(tcase, test_deriv_power2);
  tcase_add_test(tcase, test_deriv_power3);
  tcase_add_test(tcase, test_deriv_power4);
  tcase_add_test(tcase, test_deriv_root);
  tcase_add_test(tcase, test_deriv_root1);
  tcase_add_test(tcase, test_deriv_root2);
  tcase_add_test(tcase, test_deriv_log);
  tcase_add_test(tcase, test_deriv_log1);
  tcase_add_test(tcase, test_deriv_log2);
  tcase_add_test(tcase, test_deriv_ln);
  tcase_add_test(tcase, test_deriv_ln1);
  tcase_add_test(tcase, test_deriv_ln2);
  tcase_add_test(tcase, test_deriv_exp);
  tcase_add_test(tcase, test_deriv_exp1);
  tcase_add_test(tcase, test_deriv_exp2);
  tcase_add_test(tcase, test_deriv_abs);
  tcase_add_test(tcase, test_deriv_abs1);
  tcase_add_test(tcase, test_deriv_abs2);
  tcase_add_test(tcase, test_deriv_arccos);
  tcase_add_test(tcase, test_deriv_arccos1);
  tcase_add_test(tcase, test_deriv_arccos2);
  tcase_add_test(tcase, test_deriv_arctan);
  tcase_add_test(tcase, test_deriv_arctan1);
  tcase_add_test(tcase, test_deriv_arcsin);
  tcase_add_test(tcase, test_deriv_arcsin1);
  tcase_add_test(tcase, test_deriv_cos);
  tcase_add_test(tcase, test_deriv_cos1);
  tcase_add_test(tcase, test_deriv_tan);
  tcase_add_test(tcase, test_deriv_tan1);
  tcase_add_test(tcase, test_deriv_cosh);
  tcase_add_test(tcase, test_deriv_cosh1);
  tcase_add_test(tcase, test_deriv_cot);
  tcase_add_test(tcase, test_deriv_cot1);
  tcase_add_test(tcase, test_deriv_sinh);
  tcase_add_test(tcase, test_deriv_sinh1);
  tcase_add_test(tcase, test_deriv_tanh);
  tcase_add_test(tcase, test_deriv_tanh1);
  tcase_add_test(tcase, test_deriv_coth);
  tcase_add_test(tcase, test_deriv_coth1);
  tcase_add_test(tcase, test_deriv_sec);
  tcase_add_test(tcase, test_deriv_sec1);
  tcase_add_test(tcase, test_deriv_csc);
  tcase_add_test(tcase, test_deriv_csc1);
  tcase_add_test(tcase, test_deriv_sech);
  tcase_add_test(tcase, test_deriv_sech1);
  tcase_add_test(tcase, test_deriv_csch);
  tcase_add_test(tcase, test_deriv_csch1);
  tcase_add_test(tcase, test_deriv_arcsinh);
  tcase_add_test(tcase, test_deriv_arcsinh1);
  tcase_add_test(tcase, test_deriv_arccosh);
  tcase_add_test(tcase, test_deriv_arccosh1);
  tcase_add_test(tcase, test_deriv_arctanh);
  tcase_add_test(tcase, test_deriv_arctanh1);
  tcase_add_test(tcase, test_deriv_arccot);
  tcase_add_test(tcase, test_deriv_arccot1);
  tcase_add_test(tcase, test_deriv_arcsec);
  tcase_add_test(tcase, test_deriv_arcsec1);
  tcase_add_test(tcase, test_deriv_arccsc);
  tcase_add_test(tcase, test_deriv_arccsc1);
  tcase_add_test(tcase, test_deriv_arccoth);
  tcase_add_test(tcase, test_deriv_arccoth1);
  tcase_add_test(tcase, test_deriv_arcsech);
  tcase_add_test(tcase, test_deriv_arcsech1);
  tcase_add_test(tcase, test_deriv_arccsch);
  tcase_add_test(tcase, test_deriv_arccsch1);

  tcase_add_test(tcase, test_deriv_not_implemented);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

