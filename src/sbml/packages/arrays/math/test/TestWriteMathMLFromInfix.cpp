/**
 * \file    TestWriteMathMLFromInfix.cpp
 * \brief   Create nodes using infix and write to MathML
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
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

#include <sbml/math/MathML.h>
#include <sbml/math/L3Parser.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/math/L3FormulaFormatter.h>
#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/ASTNode.h>

/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_HEADER    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define MATHML_HEADER "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
#define MATHML_HEADER_UNITS  "<math xmlns=\"http://www.w3.org/1998/Math/MathML\""
#define MATHML_HEADER_UNITS2  " xmlns:sbml=\"http://www.sbml.org/sbml/level3/version1/core\">\n"
#define MATHML_FOOTER "</math>"

#define wrapMathML(s)   XML_HEADER MATHML_HEADER s MATHML_FOOTER
#define wrapMathMLUnits(s)  XML_HEADER MATHML_HEADER_UNITS MATHML_HEADER_UNITS2 s MATHML_FOOTER

LIBSBML_CPP_NAMESPACE_USE


static ASTNode_t* N;
static char*    S;


void
WriteMathMLFromInfix_setup ()
{
  N = NULL;
  S = NULL;
}


void
WriteMathMLFromInfix_teardown ()
{
  delete N;
  free(S);
}


static bool
equals (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}



#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif


START_TEST (test_parse_brackets_0args)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <ci> a </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("a[]");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_brackets_1args)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("a[x]");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_brackets_2args)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "    <ci> y </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("a[x, y]");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_selector_1args)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <ci> a </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("selector(a)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_selector_2args)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("selector(a,x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_selector_3args)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "    <ci> y </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("selector(a,x,y)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_selector_needParens)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <ci> a </ci>\n"
    "      <ci> b </ci>\n"
    "    </apply>\n"
    "    <ci> x </ci>\n"
    "    <ci> y </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("selector(a+b,x,y)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_selector_needNoParens1)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <apply>\n"
    "      <ci> f </ci>\n"
    "      <ci> a </ci>\n"
    "    </apply>\n"
    "    <ci> x </ci>\n"
    "    <ci> y </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("selector(f(a),x,y)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_selector_needNoParens2)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <vector>\n"
    "      <cn type=\"integer\"> 1 </cn>\n"
    "      <cn type=\"integer\"> 2 </cn>\n"
    "      <cn type=\"integer\"> 3 </cn>\n"
    "      <cn type=\"integer\"> 4 </cn>\n"
    "    </vector>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("selector({1, 2, 3, 4},x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST

#if (0)
START_TEST (test_parse_function_selector_needNoParens3)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <matrix>\n"
    "      <matrixrow>\n"
    "        <cn type=\"integer\"> 1 </cn>\n"
    "        <cn type=\"integer\"> 2 </cn>\n"
    "      </matrixrow>\n"
    "      <matrixrow>\n"
    "        <cn type=\"integer\"> 3 </cn>\n"
    "        <cn type=\"integer\"> 4 </cn>\n"
    "      </matrixrow>\n"
    "    </matrix>\n"
    "    <ci> x </ci>\n"
    "    <ci> y </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("selector({1, 2; 3, 4},x, y)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST
#endif

START_TEST (test_parse_curlybraces_0args)
{
  const char* expected = wrapMathML
  (
    "  <vector/>\n"
  );

  N = SBML_parseL3Formula("{}");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_curlybraces_1args)
{
  const char* expected = wrapMathML
  (
    "  <vector>\n"
    "    <ci> x </ci>\n"
    "  </vector>\n"
  );

  N = SBML_parseL3Formula("{x}");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_curlybraces_2args)
{
  const char* expected = wrapMathML
  (
    "  <vector>\n"
    "    <ci> x </ci>\n"
    "    <apply>\n"
    "      <power/>\n"
    "      <ci> a </ci>\n"
    "      <ci> b </ci>\n"
    "    </apply>\n"
    "  </vector>\n"
  );

  N = SBML_parseL3Formula("{x, a^b}");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_curlybraces_nested)
{
  const char* expected = wrapMathML
  (
    "  <vector>\n"
    "    <vector>\n"
    "      <ci> x </ci>\n"
    "      <ci> y </ci>\n"
    "      <ci> z </ci>\n"
    "    </vector>\n"
    "    <vector>\n"
    "      <ci> p </ci>\n"
    "      <ci> d </ci>\n"
    "      <ci> q </ci>\n"
    "    </vector>\n"
    "  </vector>\n"
  );

  N = SBML_parseL3Formula("{{x, y, z}, {p, d, q}}");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST

#if (0)
START_TEST (test_parse_curlybraces_semicolons)
{

  const char* expected = wrapMathML
  (
    "  <matrix>\n"
    "    <matrixrow>\n"
    "      <ci> x </ci>\n"
    "      <ci> y </ci>\n"
    "      <ci> z </ci>\n"
    "    </matrixrow>\n"
    "    <matrixrow>\n"
    "      <ci> p </ci>\n"
    "      <ci> d </ci>\n"
    "      <ci> q </ci>\n"
    "    </matrixrow>\n"
    "  </matrix>\n"
  );

  N = SBML_parseL3Formula("{x, y, z; p, d, q}");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_curlybraces_semicolons_short)
{
  const char* expected = wrapMathML
  (
    "  <matrix>\n"
    "    <matrixrow>\n"
    "      <ci> x </ci>\n"
    "    </matrixrow>\n"
    "    <matrixrow>\n"
    "      <ci> p </ci>\n"
    "    </matrixrow>\n"
    "  </matrix>\n"
  );

  N = SBML_parseL3Formula("{x; p}");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST

START_TEST (test_parse_function_determinant)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <determinant/>\n"
    "    <ci> a </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("determinant(a)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_det)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <determinant/>\n"
    "    <ci> a </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("det(a)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_transpose)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <transpose/>\n"
    "    <ci> a </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("transpose(a)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_trans)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <transpose/>\n"
    "    <ci> a </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("trans(a)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_vectorproduct)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <vectorproduct/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("vectorproduct(a, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_vectorprod)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <vectorproduct/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("vectorProd(a, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_cross)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <vectorproduct/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("cross(a, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_scalarproduct)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <scalarproduct/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("scalarproduct(a, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_scalarprod)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <scalarproduct/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("scalarprod(a, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_dot)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <scalarproduct/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("dot(a, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_outerproduct)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <outerproduct/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("outerproduct(a, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_outerprod)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <outerproduct/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("outerProd(a, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_outer)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <outerproduct/>\n"
    "    <ci> a </ci>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseL3Formula("outer(a, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST
#endif

START_TEST (test_parse_function_vector_0args)
{
  const char* expected = wrapMathML
  (
    "  <vector/>\n"
  );

  N = SBML_parseL3Formula("vector()");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_vector_1args)
{
  const char* expected = wrapMathML
  (
    "  <vector>\n"
    "    <ci> x </ci>\n"
    "  </vector>\n"
  );

  N = SBML_parseL3Formula("vector(x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_vector_2args)
{
  const char* expected = wrapMathML
  (
    "  <vector>\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <ci> a </ci>\n"
    "      <ci> b </ci>\n"
    "    </apply>\n"
    "    <apply>\n"
    "      <power/>\n"
    "      <ci> x </ci>\n"
    "      <ci> y </ci>\n"
    "    </apply>\n"
    "  </vector>\n"
  );

  N = SBML_parseL3Formula("vector(a+b, x^y)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST

#if (0)
START_TEST (test_parse_function_matrix_empty)
{
  const char* expected = wrapMathML
  (
    "  <matrix/>\n"
  );

  N = SBML_parseL3Formula("matrix()");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_matrix_1row)
{
  const char* expected = wrapMathML
  (
    "  <matrix>\n"
    "    <matrixrow>\n"
    "      <ci> x </ci>\n"
    "      <ci> y </ci>\n"
    "    </matrixrow>\n"
    "  </matrix>\n"
  );

  N = SBML_parseL3Formula("matrix(matrixrow(x, y))");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_matrix_2rows)
{
  const char* expected = wrapMathML
  (
    "  <matrix>\n"
    "    <matrixrow>\n"
    "      <ci> x </ci>\n"
    "      <ci> y </ci>\n"
    "    </matrixrow>\n"
    "    <matrixrow>\n"
    "      <ci> y </ci>\n"
    "      <ci> x </ci>\n"
    "    </matrixrow>\n"
    "  </matrix>\n"
  );

  N = SBML_parseL3Formula("matrix(matrixrow(x, y), matrixrow(y, x))");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_parse_function_matrix_emptyrows)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix(matrixrow(), matrixrow())");
  const char* expected = wrapMathML
  (
    "  <matrix>\n"
    "    <matrixrow/>\n"
    "    <matrixrow/>\n"
    "  </matrix>\n"
  );

  N = SBML_parseL3Formula("matrix(matrixrow(), matrixrow())");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST
#endif

Suite *
create_suite_ArrayInfixToMathML ()
{
  Suite *suite = suite_create("ArrayInfixToMathML");
  TCase *tcase = tcase_create("ArrayInfixToMathML");

  tcase_add_checked_fixture(tcase, WriteMathMLFromInfix_setup, 
                                   WriteMathMLFromInfix_teardown);

  tcase_add_test( tcase, test_parse_brackets_0args);
  tcase_add_test( tcase, test_parse_brackets_1args);
  tcase_add_test( tcase, test_parse_brackets_2args);
  tcase_add_test( tcase, test_parse_function_selector_1args);
  tcase_add_test( tcase, test_parse_function_selector_2args);
  tcase_add_test( tcase, test_parse_function_selector_3args);
  tcase_add_test( tcase, test_parse_function_selector_needParens);
  tcase_add_test( tcase, test_parse_function_selector_needNoParens1);
  tcase_add_test( tcase, test_parse_function_selector_needNoParens2);
#if (0)
  tcase_add_test( tcase, test_parse_function_selector_needNoParens3);
#endif

  tcase_add_test( tcase, test_parse_curlybraces_0args);
  tcase_add_test( tcase, test_parse_curlybraces_1args);
  tcase_add_test( tcase, test_parse_curlybraces_2args);
  tcase_add_test( tcase, test_parse_curlybraces_nested);
#if (0)
  tcase_add_test( tcase, test_parse_curlybraces_semicolons);
  tcase_add_test( tcase, test_parse_curlybraces_semicolons_short);
#endif

#if (0)
  tcase_add_test( tcase, test_parse_function_determinant);
  tcase_add_test( tcase, test_parse_function_det);
  tcase_add_test( tcase, test_parse_function_transpose);
  tcase_add_test( tcase, test_parse_function_trans);
  tcase_add_test( tcase, test_parse_function_vectorproduct);
  tcase_add_test( tcase, test_parse_function_vectorprod);
  tcase_add_test( tcase, test_parse_function_cross);
  tcase_add_test( tcase, test_parse_function_scalarproduct);
  tcase_add_test( tcase, test_parse_function_scalarprod);
  tcase_add_test( tcase, test_parse_function_dot);
  tcase_add_test( tcase, test_parse_function_outerproduct);
  tcase_add_test( tcase, test_parse_function_outerprod);
  tcase_add_test( tcase, test_parse_function_outer);
#endif

  tcase_add_test( tcase, test_parse_function_vector_0args);
  tcase_add_test( tcase, test_parse_function_vector_1args);
  tcase_add_test( tcase, test_parse_function_vector_2args);

#if (0)
  tcase_add_test( tcase, test_parse_function_matrix_empty);
  tcase_add_test( tcase, test_parse_function_matrix_1row);
  tcase_add_test( tcase, test_parse_function_matrix_2rows);
  tcase_add_test( tcase, test_parse_function_matrix_emptyrows);
#endif

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
