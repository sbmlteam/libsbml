/**
 * \file    TestArrayInfixParsing.cpp
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

#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif


START_TEST (test_parse_brackets_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("a[]");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless(c != NULL);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless(c != NULL);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( ASTNode_getNumChildren(c) == 2);

  c = ASTNode_getChild(c, 0);
  fail_unless(c != NULL);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 0);

  delete r;
}
END_TEST


START_TEST (test_parse_curlybraces_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("{x}");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
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
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
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

#if (0)
START_TEST (test_parse_curlybraces_semicolons)
{
  ASTNode_t *r = SBML_parseL3Formula("{x, y, z; p, d, q}");
  ASTNode_t *c = NULL;
  ASTNode_t *c2 = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
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
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
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


START_TEST (test_parse_curlybraces_semicolons_short)
{
  ASTNode_t *r = SBML_parseL3Formula("{x; p}");
  ASTNode_t *c = NULL;
  ASTNode_t *c2 = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(c) == 1);

  c2 = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "x") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(c) == 1);

  c2 = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c2) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c2), "p") );
  fail_unless( ASTNode_getNumChildren(c2) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_curlybraces_semicolons_ragged)
{
  ASTNode_t *r = SBML_parseL3Formula("{x, y, z; p}");
  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input '{x, y, z; p}' at position 12:  All rows of matrices must have the same number of arguments:  In this matrix, the first row has 3 children, but row 2 has 1 child(ren)."));
  safe_free(error);
}
END_TEST
#endif

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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
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

#if (0)
START_TEST (test_parse_function_determinant_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("determinant()");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'determinant()' at position 13:  The 'determinant' function must have exactly one argument: the matrix in question."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_determinant_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("determinant(a)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_DETERMINANT);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_determinant_1args_1)
{
  ASTNode_t *r = SBML_parseL3Formula("det(a)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_DETERMINANT);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_determinant_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("determinant(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'determinant(a,x)' at position 16:  The 'determinant' function must have exactly one argument: the matrix in question."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_determinant_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("determinant(a,x,y)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'determinant(a,x,y)' at position 18:  The 'determinant' function must have exactly one argument: the matrix in question."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_transpose_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("transpose()");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'transpose()' at position 11:  The 'transpose' function must have exactly one argument: the vector or matrix in question."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_transpose_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("transpose(a)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_TRANSPOSE);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_transpose_1args_1)
{
  ASTNode_t *r = SBML_parseL3Formula("trans(a)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_TRANSPOSE);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_transpose_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("transpose(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'transpose(a,x)' at position 14:  The 'transpose' function must have exactly one argument: the vector or matrix in question."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_transpose_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("transpose(a,x,y)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'transpose(a,x,y)' at position 16:  The 'transpose' function must have exactly one argument: the vector or matrix in question."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_vectorproduct_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("vectorproduct()");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'vectorproduct()' at position 15:  The vector product function must have exactly two vector arguments."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_vectorproduct_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("vectorproduct(a)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'vectorproduct(a)' at position 16:  The vector product function must have exactly two vector arguments."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_vectorproduct_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("vectorproduct(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_PRODUCT);
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


START_TEST (test_parse_function_vectorproduct_2args_1)
{
  ASTNode_t *r = SBML_parseL3Formula("vectorprod(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_PRODUCT);
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


START_TEST (test_parse_function_vectorproduct_2args_2)
{
  ASTNode_t *r = SBML_parseL3Formula("cross(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_PRODUCT);
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



START_TEST (test_parse_function_vectorproduct_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("vectorproduct(a,x,y)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'vectorproduct(a,x,y)' at position 20:  The vector product function must have exactly two vector arguments."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_scalarproduct_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("scalarproduct()");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'scalarproduct()' at position 15:  The scalar product function must have exactly two vector arguments."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_scalarproduct_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("scalarproduct(a)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'scalarproduct(a)' at position 16:  The scalar product function must have exactly two vector arguments."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_scalarproduct_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("scalarproduct(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SCALAR_PRODUCT);
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


START_TEST (test_parse_function_scalarproduct_2args_1)
{
  ASTNode_t *r = SBML_parseL3Formula("scalarprod(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SCALAR_PRODUCT);
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


START_TEST (test_parse_function_scalarproduct_2args_2)
{
  ASTNode_t *r = SBML_parseL3Formula("dot(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_SCALAR_PRODUCT);
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


START_TEST (test_parse_function_scalarproduct_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("scalarproduct(a,x,y)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'scalarproduct(a,x,y)' at position 20:  The scalar product function must have exactly two vector arguments."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_outerproduct_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("outerproduct()");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'outerproduct()' at position 14:  The outer product function must have exactly two vector arguments."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_outerproduct_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("outerproduct(a)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'outerproduct(a)' at position 15:  The outer product function must have exactly two vector arguments."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_outerproduct_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("outerproduct(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_OUTER_PRODUCT);
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


START_TEST (test_parse_function_outerproduct_2args_1)
{
  ASTNode_t *r = SBML_parseL3Formula("outerprod(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_OUTER_PRODUCT);
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


START_TEST (test_parse_function_outerproduct_2args_2)
{
  ASTNode_t *r = SBML_parseL3Formula("outer(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_OUTER_PRODUCT);
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


START_TEST (test_parse_function_outerproduct_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("outerproduct(a,x,y)");
  ASTNode_t *c = NULL;

  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'outerproduct(a,x,y)' at position 19:  The outer product function must have exactly two vector arguments."));
  safe_free(error);
}
END_TEST
#endif

START_TEST (test_parse_function_vector_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("vector()");
  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 0);
  delete r;
}
END_TEST


START_TEST (test_parse_function_vector_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("vector(a)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
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
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
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

#if (0)
START_TEST (test_parse_function_matrix_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix()");
  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 0);

  delete r;
}
END_TEST


START_TEST (test_parse_function_matrix_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix(matrixrow(a))");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(c) == 1 );

  c = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_matrix_1arg_error)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix(a)");
  fail_unless(r == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'matrix(a)' at position 9:  All children of matrices must be matrix rows created with 'matrixrow()', or the entire matrix may be constructed with curly braces and semicolon-delimited lists: '{a, b; x, y}'."));
  safe_free(error);
}
END_TEST


START_TEST (test_parse_function_matrix_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix(matrixrow(a),matrixrow(x))");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(c) == 1 );

  c = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(c) == 1 );

  c = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_matrix_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("matrix(matrixrow(a),matrixrow(x),matrixrow(y))");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 3);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(c) == 1 );

  c = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(c) == 1 );

  c = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 2);
  fail_unless( ASTNode_getType(c) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( c->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(c) == 1 );

  c = ASTNode_getChild(c, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "y") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_matrixrow_0args)
{
  ASTNode_t *r = SBML_parseL3Formula("matrixrow()");
  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 0);

  delete r;
}
END_TEST


START_TEST (test_parse_function_matrixrow_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("matrixrow(a)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_matrixrow_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("matrixrow(a,x)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
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


START_TEST (test_parse_function_matrixrow_3args)
{
  ASTNode_t *r = SBML_parseL3Formula("matrixrow(a,x,y)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
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
#endif


START_TEST (test_parse_function_mean_1args)
{
  ASTNode_t *r = SBML_parseL3Formula("mean(a)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_STATISTICS_MEAN);
  fail_unless( ASTNode_getNumChildren(r) == 1);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  delete r;
}
END_TEST


START_TEST (test_parse_function_mode_2args)
{
  ASTNode_t *r = SBML_parseL3Formula("mode(a, b)");
  ASTNode_t *c = NULL;

  fail_unless(r != NULL);
  fail_unless( ASTNode_getType(r) == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( r->getExtendedType() == AST_STATISTICS_MODE);
  fail_unless( ASTNode_getNumChildren(r) == 2);

  c = ASTNode_getChild(r, 0);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "a") );
  fail_unless( ASTNode_getNumChildren(c) == 0 );

  c = ASTNode_getChild(r, 1);
  fail_unless( ASTNode_getType       (c) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(c), "b") );
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

#if (0)
  tcase_add_test( tcase, test_parse_curlybraces_semicolons);
  tcase_add_test( tcase, test_parse_curlybraces_semicolons_short);
  tcase_add_test( tcase, test_parse_curlybraces_semicolons_ragged);
#endif

  tcase_add_test( tcase, test_parse_function_selector_0args);
  tcase_add_test( tcase, test_parse_function_selector_1args);
  tcase_add_test( tcase, test_parse_function_selector_2args);
  tcase_add_test( tcase, test_parse_function_selector_3args);
  tcase_add_test( tcase, test_parse_function_selector_4args);
  tcase_add_test( tcase, test_parse_function_selector_5args);

#if (0)
  tcase_add_test( tcase, test_parse_function_determinant_0args);
  tcase_add_test( tcase, test_parse_function_determinant_1args);
  tcase_add_test( tcase, test_parse_function_determinant_1args_1);
  tcase_add_test( tcase, test_parse_function_determinant_2args);
  tcase_add_test( tcase, test_parse_function_determinant_3args);

  tcase_add_test( tcase, test_parse_function_transpose_0args);
  tcase_add_test( tcase, test_parse_function_transpose_1args);
  tcase_add_test( tcase, test_parse_function_transpose_1args_1);
  tcase_add_test( tcase, test_parse_function_transpose_2args);
  tcase_add_test( tcase, test_parse_function_transpose_3args);

  tcase_add_test( tcase, test_parse_function_vectorproduct_0args);
  tcase_add_test( tcase, test_parse_function_vectorproduct_1args);
  tcase_add_test( tcase, test_parse_function_vectorproduct_2args);
  tcase_add_test( tcase, test_parse_function_vectorproduct_2args_1);
  tcase_add_test( tcase, test_parse_function_vectorproduct_2args_2);
  tcase_add_test( tcase, test_parse_function_vectorproduct_3args);

  tcase_add_test( tcase, test_parse_function_scalarproduct_0args);
  tcase_add_test( tcase, test_parse_function_scalarproduct_1args);
  tcase_add_test( tcase, test_parse_function_scalarproduct_2args);
  tcase_add_test( tcase, test_parse_function_scalarproduct_2args_1);
  tcase_add_test( tcase, test_parse_function_scalarproduct_2args_2);
  tcase_add_test( tcase, test_parse_function_scalarproduct_3args);

  tcase_add_test( tcase, test_parse_function_outerproduct_0args);
  tcase_add_test( tcase, test_parse_function_outerproduct_1args);
  tcase_add_test( tcase, test_parse_function_outerproduct_2args);
  tcase_add_test( tcase, test_parse_function_outerproduct_2args_1);
  tcase_add_test( tcase, test_parse_function_outerproduct_2args_2);
  tcase_add_test( tcase, test_parse_function_outerproduct_3args);
#endif

  tcase_add_test( tcase, test_parse_function_vector_0args);
  tcase_add_test( tcase, test_parse_function_vector_1args);
  tcase_add_test( tcase, test_parse_function_vector_2args);
  tcase_add_test( tcase, test_parse_function_vector_3args);

#if (0)
  tcase_add_test( tcase, test_parse_function_matrix_0args);
  tcase_add_test( tcase, test_parse_function_matrix_1args);
  tcase_add_test( tcase, test_parse_function_matrix_1arg_error);
  tcase_add_test( tcase, test_parse_function_matrix_2args);
  tcase_add_test( tcase, test_parse_function_matrix_3args);

  tcase_add_test( tcase, test_parse_function_matrixrow_0args);
  tcase_add_test( tcase, test_parse_function_matrixrow_1args);
  tcase_add_test( tcase, test_parse_function_matrixrow_2args);
  tcase_add_test( tcase, test_parse_function_matrixrow_3args);
#endif
 
  tcase_add_test( tcase, test_parse_function_mean_1args);
  tcase_add_test( tcase, test_parse_function_mode_2args);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
