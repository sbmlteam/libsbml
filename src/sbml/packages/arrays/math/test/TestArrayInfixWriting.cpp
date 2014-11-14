/**
 * \file    TestArrayInfixWriting.cpp
 * \brief   Read infix with bits parsed by Arrays plugins.
 * \author  Lucian Smith
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

#include <sbml/math/L3Parser.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/math/L3FormulaFormatter.h>
#include <sbml/math/FormulaFormatter.h>

#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif


START_TEST (test_parse_brackets_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("a[]");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "a[]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(a)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_brackets_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("a[x]");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "a[x]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_brackets_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("a[x, y]");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "a[x, y]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(a, x, y)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_selector_0args)
{
  ASTNode_t *r = new ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "selector()") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector()") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_selector_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(a)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "a[]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(a)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_selector_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(a,x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "a[x]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_selector_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(a,x,y)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "a[x, y]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(a, x, y)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_selector_needParens)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(a+b,x,y)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "(a + b)[x, y]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(a + b, x, y)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_selector_needNoParens1)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(f(a),x,y)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "f(a)[x, y]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(f(a), x, y)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_selector_needNoParens2)
{
  ASTNode_t *r = SBML_parseL3Formula("selector({1, 2, 3, 4},x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{1, 2, 3, 4}[x]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(vector(1, 2, 3, 4), x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST

#if (0)
START_TEST (test_parse_function_selector_needNoParens3)
{
  ASTNode_t *r = SBML_parseL3Formula("selector({1, 2; 3, 4},x, y)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{1, 2; 3, 4}[x, y]") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "selector(matrix(matrixrow(1, 2), matrixrow(3, 4)), x, y)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST
#endif

START_TEST (test_parse_curlybraces_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("{}");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vector()") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_curlybraces_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("{x}");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{x}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vector(x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_curlybraces_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("{x, a^b}");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{x, a^b}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vector(x, a^b)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_curlybraces_nested)
{
  ASTNode_t *r = SBML_parseL3Formula("{{x, y, z}, {p, d, q}}");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{{x, y, z}, {p, d, q}}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vector(vector(x, y, z), vector(p, d, q))") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST

#if (0)
START_TEST (test_parse_curlybraces_semicolons)
{
  ASTNode_t *r = SBML_parseL3Formula("{x, y, z; p, d, q}");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{x, y, z; p, d, q}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "matrix(matrixrow(x, y, z), matrixrow(p, d, q))") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_curlybraces_semicolons_short)
{
  ASTNode_t *r = SBML_parseL3Formula("{x; p}");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{x; p}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "matrix(matrixrow(x), matrixrow(p))") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_determinant)
{
  ASTNode_t *r = SBML_parseL3Formula("determinant(a)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "determinant(a)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "determinant(a)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_det)
{
  ASTNode_t *r = SBML_parseL3Formula("det(a)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "determinant(a)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "determinant(a)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_transpose)
{
  ASTNode_t *r = SBML_parseL3Formula("transpose(a)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "transpose(a)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "transpose(a)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_trans)
{
  ASTNode_t *r = SBML_parseL3Formula("trans(a)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "transpose(a)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "transpose(a)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_vectorproduct)
{
  ASTNode_t *r = SBML_parseL3Formula("vectorproduct(a, x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "vectorproduct(a, x)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vectorproduct(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_vectorprod)
{
  ASTNode_t *r = SBML_parseL3Formula("vectorProd(a, x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "vectorproduct(a, x)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vectorproduct(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_cross)
{
  ASTNode_t *r = SBML_parseL3Formula("cross(a, x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "vectorproduct(a, x)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vectorproduct(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_scalarproduct)
{
  ASTNode_t *r = SBML_parseL3Formula("scalarproduct(a, x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "scalarproduct(a, x)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "scalarproduct(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_scalarprod)
{
  ASTNode_t *r = SBML_parseL3Formula("scalarProd(a, x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "scalarproduct(a, x)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "scalarproduct(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_dot)
{
  ASTNode_t *r = SBML_parseL3Formula("dot(a, x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "scalarproduct(a, x)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "scalarproduct(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_outerproduct)
{
  ASTNode_t *r = SBML_parseL3Formula("outerproduct(a, x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "outerproduct(a, x)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "outerproduct(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_outerprod)
{
  ASTNode_t *r = SBML_parseL3Formula("outerProd(a, x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "outerproduct(a, x)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "outerproduct(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_outer)
{
  ASTNode_t *r = SBML_parseL3Formula("outer(a, x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "outerproduct(a, x)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "outerproduct(a, x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST
#endif

START_TEST (test_parse_function_vector_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("vector()");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vector()") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_vector_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("vector(x)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{x}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vector(x)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_vector_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("vector(a+b, x^y)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{a + b, x^y}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "vector(a + b, x^y)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST

#if (0)
START_TEST (test_parse_function_matrix_empty)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix()");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "matrix()") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "matrix()") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_matrix_1row)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix(matrixrow(x, y))");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "matrix(matrixrow(x, y))") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "matrix(matrixrow(x, y))") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_matrix_2rows)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix(matrixrow(x, y), matrixrow(y, x))");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "{x, y; y, x}") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "matrix(matrixrow(x, y), matrixrow(y, x))") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


START_TEST (test_parse_function_matrix_emptyrows)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix(matrixrow(), matrixrow())");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "matrix(matrixrow(), matrixrow())") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "matrix(matrixrow(), matrixrow())") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST
#endif


START_TEST (test_parse_function_sum)
{
  ASTNode_t *r = SBML_parseL3Formula("sum(a, b, c)");
  char* roundtrip = SBML_formulaToL3String(r);
  fail_unless( !strcmp(roundtrip, "sum(a, b, c)") );
  char* oldroundtrip = SBML_formulaToString(r);
  fail_unless( !strcmp(oldroundtrip, "sum(a, b, c)") );

  delete r;
  safe_free(roundtrip);
  safe_free(oldroundtrip);
}
END_TEST


Suite *
create_suite_ArrayInfixWriting ()
{
  Suite *suite = suite_create("ArrayInfixWriting");
  TCase *tcase = tcase_create("ArrayInfixWriting");

  tcase_add_test( tcase, test_parse_brackets_0args);
  tcase_add_test( tcase, test_parse_brackets_1args);
  tcase_add_test( tcase, test_parse_brackets_2args);
  tcase_add_test( tcase, test_parse_function_selector_0args);
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

  tcase_add_test( tcase, test_parse_function_sum);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
