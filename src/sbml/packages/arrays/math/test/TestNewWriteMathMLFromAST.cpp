/**
 * \file    TestNewWriteMathMLFromASTFromAST.cpp
 * \brief   Write MathML unit tests starting from ASTNodes
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

#include <limits>
#include <iostream>
#include <cstring>
#include <cstdio>

#include <check.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>

/** @cond doxygenLibsbmlInternal */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond */


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


static ASTNode* N;
static char*    S;


void
NewWriteMathMLFromAST_setup ()
{
  N = NULL;
  S = NULL;
}


void
NewWriteMathMLFromAST_teardown ()
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


CK_CPPSTART


START_TEST (test_MathMLFromAST_vector)
{
  const char* expected = wrapMathML
  (
    "  <vector>\n"
    "    <ci> x </ci>\n"
    "    <cn> 0.1 </cn>\n"
    "  </vector>\n"
  );

  N = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  fail_unless( N->getPackageName() == "arrays");

  ASTNode * c1 = new ASTNode(AST_NAME);
  fail_unless(c1->setName("x") == LIBSBML_OPERATION_SUCCESS);

  ASTNode *c2 = new ASTNode(AST_REAL);
  fail_unless(c2->setValue(0.1) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(N->addChild(c1) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->addChild(c2) == LIBSBML_OPERATION_SUCCESS);


  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST

#if (0)
START_TEST (test_MathMLFromAST_matrix)
{
  const char* expected = wrapMathML
  (
    "  <matrix>\n"
    "    <matrixrow>\n"
    "      <apply>\n"
    "        <cos/>\n"
    "        <cn type=\"integer\"> 5 </cn>\n"
    "      </apply>\n"
    "      <ci> y </ci>\n"
    "    </matrixrow>\n"
    "    <matrixrow>\n"
    "      <cn type=\"integer\"> 2 </cn>\n"
    "      <cn type=\"integer\"> 4 </cn>\n"
    "    </matrixrow>\n"
    "  </matrix>\n"
  );


  ASTNode * y = new ASTNode(AST_NAME);
  fail_unless(y->setName("y") == LIBSBML_OPERATION_SUCCESS);

  ASTNode *int1 = new ASTNode(AST_INTEGER);
  fail_unless(int1->setValue((long)(5)) == LIBSBML_OPERATION_SUCCESS);

  ASTNode *cos = new ASTNode(AST_FUNCTION_COS);
  fail_unless(cos->addChild(int1) == LIBSBML_OPERATION_SUCCESS);

  ASTNode *row1 = new ASTNode(AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( row1->getPackageName() == "arrays");

  fail_unless(row1->addChild(cos) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(row1->addChild(y) == LIBSBML_OPERATION_SUCCESS);


  ASTNode *int2 = new ASTNode(AST_INTEGER);
  fail_unless(int2->setValue((long)(2)) == LIBSBML_OPERATION_SUCCESS);

  ASTNode *int3 = new ASTNode(AST_INTEGER);
  fail_unless(int3->setValue((long)(4)) == LIBSBML_OPERATION_SUCCESS);

  ASTNode *row2 = new ASTNode(AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( row2->getPackageName() == "arrays");

  fail_unless(row2->addChild(int2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(row2->addChild(int3) == LIBSBML_OPERATION_SUCCESS);

  N = new ASTNode(AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  fail_unless( N->getPackageName() == "arrays");

  fail_unless(N->addChild(row1) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->addChild(row2) == LIBSBML_OPERATION_SUCCESS);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST
#endif

#if (0)
START_TEST (test_MathMLFromAST_transpose)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <transpose/>\n"
    "    <ci> y </ci>\n"
    "  </apply>\n"
  );


  ASTNode * y = new ASTNode(AST_NAME);
  fail_unless(y->setName("y") == LIBSBML_OPERATION_SUCCESS);


  N = new ASTNode(AST_LINEAR_ALGEBRA_TRANSPOSE);
  fail_unless( N->getPackageName() == "arrays");

  fail_unless(N->addChild(y) == LIBSBML_OPERATION_SUCCESS);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST
#endif

#if (0)
START_TEST (test_MathMLFromAST_scalarproduct)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <scalarproduct/>\n"
    "    <ci> y </ci>\n"
    "    <ci> z </ci>\n"
    "  </apply>\n"
  );


  ASTNode * y = new ASTNode(AST_NAME);
  fail_unless(y->setName("y") == LIBSBML_OPERATION_SUCCESS);

  ASTNode * z = new ASTNode(AST_NAME);
  fail_unless(z->setName("z") == LIBSBML_OPERATION_SUCCESS);

  N = new ASTNode(AST_LINEAR_ALGEBRA_SCALAR_PRODUCT);
  fail_unless( N->getPackageName() == "arrays");

  fail_unless(N->addChild(y) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->addChild(z) == LIBSBML_OPERATION_SUCCESS);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST
#endif

START_TEST (test_MathMLFromAST_selector)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <selector/>\n"
    "    <ci> y </ci>\n"
    "    <ci> z </ci>\n"
    "    <cn type=\"integer\"> 5 </cn>\n"
    "  </apply>\n"
  );


  ASTNode * y = new ASTNode(AST_NAME);
  fail_unless(y->setName("y") == LIBSBML_OPERATION_SUCCESS);

  ASTNode * z = new ASTNode(AST_NAME);
  fail_unless(z->setName("z") == LIBSBML_OPERATION_SUCCESS);

  ASTNode *int1 = new ASTNode(AST_INTEGER);
  fail_unless(int1->setValue((long)(5)) == LIBSBML_OPERATION_SUCCESS);

  N = new ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( N->getPackageName() == "arrays");

  fail_unless(N->addChild(y) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->addChild(z) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->addChild(int1) == LIBSBML_OPERATION_SUCCESS);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFromAST_mean)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <mean/>\n"
    "    <ci> y </ci>\n"
    "    <ci> z </ci>\n"
    "    <cn type=\"integer\"> 5 </cn>\n"
    "  </apply>\n"
  );


  ASTNode * y = new ASTNode(AST_NAME);
  fail_unless(y->setName("y") == LIBSBML_OPERATION_SUCCESS);

  ASTNode * z = new ASTNode(AST_NAME);
  fail_unless(z->setName("z") == LIBSBML_OPERATION_SUCCESS);

  ASTNode *int1 = new ASTNode(AST_INTEGER);
  fail_unless(int1->setValue((long)(5)) == LIBSBML_OPERATION_SUCCESS);

  N = new ASTNode(AST_STATISTICS_MEAN);
  fail_unless( N->getPackageName() == "arrays");

  fail_unless(N->addChild(y) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->addChild(z) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->addChild(int1) == LIBSBML_OPERATION_SUCCESS);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFromAST_product)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <product/>\n"
    "    <bvar>\n"
    "      <ci> y </ci>\n"
    "    </bvar>\n"
    "    <uplimit>\n"
    "      <ci> z </ci>\n"
    "    </uplimit>\n"
    "    <ci> A </ci>\n"
    "  </apply>\n"
  );


  ASTNode * y = new ASTNode(AST_NAME);
  fail_unless(y->setName("y") == LIBSBML_OPERATION_SUCCESS);

  ASTNode * z = new ASTNode(AST_NAME);
  fail_unless(z->setName("z") == LIBSBML_OPERATION_SUCCESS);

  ASTNode * A = new ASTNode(AST_NAME);
  fail_unless(A->setName("A") == LIBSBML_OPERATION_SUCCESS);

  ASTNode * bvar = new ASTNode(AST_QUALIFIER_BVAR);
  bvar->addChild(y);

  ASTNode * up = new ASTNode(AST_QUALIFIER_UPLIMIT);
  up->addChild(z);

  N = new ASTNode(AST_SERIES_PRODUCT);
  fail_unless( N->getPackageName() == "arrays");

  fail_unless(N->addChild(bvar) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->addChild(up) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->addChild(A) == LIBSBML_OPERATION_SUCCESS);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


Suite *
create_suite_NewWriteMathMLFromAST ()
{
  Suite *suite = suite_create("NewWriteMathMLFromAST");
  TCase *tcase = tcase_create("NewWriteMathMLFromAST");

  tcase_add_checked_fixture(tcase, NewWriteMathMLFromAST_setup, NewWriteMathMLFromAST_teardown);

  tcase_add_test( tcase, test_MathMLFromAST_vector             );
#if (0)
  tcase_add_test( tcase, test_MathMLFromAST_matrix             );
  tcase_add_test( tcase, test_MathMLFromAST_transpose          );
  tcase_add_test( tcase, test_MathMLFromAST_scalarproduct      );
#endif
  tcase_add_test( tcase, test_MathMLFromAST_selector           );
  tcase_add_test( tcase, test_MathMLFromAST_mean               );
  tcase_add_test( tcase, test_MathMLFromAST_product            );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
