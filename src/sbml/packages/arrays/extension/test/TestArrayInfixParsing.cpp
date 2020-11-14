/**
 * \file    TestArrayInfixParsing.cpp
 * \brief   Read infix with bits parsed by Arrays plugins.
 * \author  Lucian Smith
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

#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif


START_TEST (test_parse_brackets_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("a[]");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_brackets_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("a[x]");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_brackets_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("a[x, y]");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 3);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 2);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "y") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_brackets_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("a[x,y,z]");

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'a[x,y,z]' at position 8:  The 'selector' function may not have more than three arguments ('selector(a, x, y)'):  the first for the vector or matrix from which to select, the second for the index of the vector or the matrixrow of the matrix, and an optional third, which only applies to matrices, for the index of the selected matrixrow of the matrix.  Similarly, the bracketed form may have no more than two ('a[x, y]'), for the same reason."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_doublebrackets)
{
  ASTNode_t *r = SBML_parseL3Formula("a[x][y]");
  ASTNode_t *c = NULL;
  ASTNode_t *c1 = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless(c != NULL);
  fail_unless( ASTNode_getType(c) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(c) == 2);

  c1 = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c1) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c1), "a") );
  fail_unless( ASTNode_getNumChildren(c1) == 0 );

  c1 = ASTNode_getChild(c, 1);
  fail_unless( ASTNode_getType       (c1) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c1), "x") );
  fail_unless( ASTNode_getNumChildren(c1) == 0 );


  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "y") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_triplebrackets)
{
  ASTNode_t *r = SBML_parseL3Formula("a[x][y][z]");
  ASTNode_t *c = NULL;
  ASTNode_t *c1 = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless(c != NULL);
  fail_unless( ASTNode_getType(c) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(c) == 2);

  c = ASTNode_getChild(c, 0);
  fail_unless(c != NULL);
  fail_unless( ASTNode_getType(c) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(c) == 2);

  c1 = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c1) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c1), "a") );
  fail_unless( ASTNode_getNumChildren(c1) == 0 );

  c1 = ASTNode_getChild(c, 1);
  fail_unless( ASTNode_getType       (c1) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c1), "x") );
  fail_unless( ASTNode_getNumChildren(c1) == 0 );


  c = ASTNode_getChild(r, 0);
  c1 = ASTNode_getChild(c, 1);
  fail_unless( ASTNode_getType       (c1) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c1), "y") );
  fail_unless( ASTNode_getNumChildren(c1) == 0 );

  c1 = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c1) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c1), "z") );
  fail_unless( ASTNode_getNumChildren(c1) == 0 );


  delete r;
}
END_TEST


START_TEST (test_parse_curlybraces_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("{}");
  //ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 0);

  delete r;
}
END_TEST


START_TEST (test_parse_curlybraces_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("{x}");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_curlybraces_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("{x, a^b}");
  ASTNode_t *c = NULL;
  ASTNode_t *c2 = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c) == AST_POWER );
  fail_unless( ASTNode_getNumChildren(c) == 2 );

  c2 = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "a") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  c2 = ASTNode_getChild(c, 1);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "b") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_curlybraces_nested)
{
  ASTNode_t *r = SBML_parseL3Formula("{{x, y, z}, {p, d, q}}");
  ASTNode_t *c = NULL;
  ASTNode_t *c2 = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType(c) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(c) == 3);

  c2 = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "x") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  c2 = ASTNode_getChild(c, 1);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "y") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  c2 = ASTNode_getChild(c, 2);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "z") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType(c) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(c) == 3);

  c2 = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "p") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  c2 = ASTNode_getChild(c, 1);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "d") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  c2 = ASTNode_getChild(c, 2);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "q") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  delete r;
}
END_TEST

START_TEST (test_parse_function_selector_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("selector()");

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'selector()' at position 10:  The 'selector' function must have at least one argument: the vector or matrix in question."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_selector_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(a)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_selector_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_selector_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(a,x,y)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 3);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 2);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "y") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_selector_4args)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(a,x,y,z)");

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'selector(a,x,y,z)' at position 17:  The 'selector' function may not have more than three arguments ('selector(a, x, y)'):  the first for the vector or matrix from which to select, the second for the index of the vector or the matrixrow of the matrix, and an optional third, which only applies to matrices, for the index of the selected matrixrow of the matrix.  Similarly, the bracketed form may have no more than two ('a[x, y]'), for the same reason."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_selector_5args)
{
  ASTNode_t *r = SBML_parseL3Formula("selector(a,x,y,z,q)");
  //ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'selector(a,x,y,z,q)' at position 19:  The 'selector' function may not have more than three arguments ('selector(a, x, y)'):  the first for the vector or matrix from which to select, the second for the index of the vector or the matrixrow of the matrix, and an optional third, which only applies to matrices, for the index of the selected matrixrow of the matrix.  Similarly, the bracketed form may have no more than two ('a[x, y]'), for the same reason."));
  safe_free(error);
}
END_TEST

START_TEST (test_parse_function_vector_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("vector()");
  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 0);
  delete r;
}
END_TEST


START_TEST (test_parse_function_vector_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("vector(a)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_vector_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("vector(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_vector_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("vector(a,x,y)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_LINEAR_ALGEBRA_VECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 3);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 2);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "y") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST

Suite *
create_suite_ArrayInfixParsing ()
{
  Suite *suite = suite_create("ArrayInfixParsing");
  TCase *tcase = tcase_create("ArrayInfixParsing");

  tcase_add_test( tcase, test_parse_brackets_0args);
  tcase_add_test( tcase, test_parse_brackets_1args);
  tcase_add_test( tcase, test_parse_brackets_2args);
  tcase_add_test( tcase, test_parse_brackets_3args);

  tcase_add_test( tcase, test_parse_doublebrackets);
  tcase_add_test( tcase, test_parse_triplebrackets);

  tcase_add_test( tcase, test_parse_curlybraces_0args);
  tcase_add_test( tcase, test_parse_curlybraces_1args);
  tcase_add_test( tcase, test_parse_curlybraces_2args);
  tcase_add_test( tcase, test_parse_curlybraces_nested);

  tcase_add_test( tcase, test_parse_function_selector_0args);
  tcase_add_test( tcase, test_parse_function_selector_1args);
  tcase_add_test( tcase, test_parse_function_selector_2args);
  tcase_add_test( tcase, test_parse_function_selector_3args);
  tcase_add_test( tcase, test_parse_function_selector_4args);
  tcase_add_test( tcase, test_parse_function_selector_5args);

  tcase_add_test( tcase, test_parse_function_vector_0args);
  tcase_add_test( tcase, test_parse_function_vector_1args);
  tcase_add_test( tcase, test_parse_function_vector_2args);
  tcase_add_test( tcase, test_parse_function_vector_3args);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
