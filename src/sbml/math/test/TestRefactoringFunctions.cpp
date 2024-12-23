/**
 * \file    TestRefactoringFunctions.cpp
 * \brief   Test extra functions for inferring rns - refactoring function
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
/** @endcond */

CK_CPPSTART

START_TEST(test_refactor_numbers_integer)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "    <cn type=\"integer\"> 1 </cn>"
     "</math>"
    );

  fail_unless(n != NULL);
  fail_unless(n->getType() == AST_INTEGER);

  n->refactorNumbers();

  fail_unless(n->getType() == AST_REAL);
  fail_unless(util_isEqual(n->getValue(), 1.0));
}
END_TEST

START_TEST(test_refactor_numbers_real)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "    <cn> 4.0 </cn>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_REAL);

    n->refactorNumbers();

    fail_unless(n->getType() == AST_REAL);
    fail_unless(util_isEqual(n->getValue(), 4.0));
}
END_TEST

START_TEST(test_refactor_numbers_e_notation)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "    <cn type=\"e-notation\"> 4.1 <sep/> 2 </cn>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_REAL_E);

    n->refactorNumbers();

    fail_unless(n->getType() == AST_REAL);
    fail_unless(util_isEqual(n->getValue(), 410));
}
END_TEST

START_TEST(test_refactor_numbers_rational)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "    <cn type=\"rational\"> 4 <sep/> 2 </cn>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_RATIONAL);

    n->refactorNumbers();

    fail_unless(n->getType() == AST_REAL);
    fail_unless(util_isEqual(n->getValue(), 2.0));
}
END_TEST

START_TEST(test_refactor_numbers_encompass_unary_minus_1)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <minus/>"
        "      <cn> 4.0 </cn>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_MINUS);
    fail_unless(n->getNumChildren() == 1);
    
    // replace -(4) with -4
    n->encompassUnaryMinus();

    fail_unless(n->getType() == AST_REAL);
    fail_unless(util_isEqual(n->getValue(), -4.0));
}
END_TEST

START_TEST(test_refactor_numbers_encompass_unary_minus_2)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <minus/>"
        "       <apply>"
        "          <times/>"
        "          <cn> 4.0 </cn>"
        "          <ci> x </ci>"
        "       </apply>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_MINUS);
    fail_unless(n->getNumChildren() == 1);

    // replace -(2*a) with -2*a or -(2/a) with -2/a
    n->encompassUnaryMinus();

    fail_unless(n->getType() == AST_TIMES);
    fail_unless(n->getNumChildren() == 2);

    fail_unless(util_isEqual(n->getChild(0)->getValue(), -4.0));
    fail_unless(n->getChild(1)->getType() == AST_NAME);
}
END_TEST

START_TEST(test_refactor_numbers_encompass_unary_minus_3)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <minus/>"
        "       <apply>"
        "          <times/>"
        "          <ci> a </ci>"
        "          <ci> b </ci>"
        "       </apply>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_MINUS);
    fail_unless(n->getNumChildren() == 1);

    // replace -(a*b) with -1*a*b
    n->encompassUnaryMinus();

    fail_unless(n->getType() == AST_TIMES);
    fail_unless(n->getNumChildren() == 3);

    fail_unless(util_isEqual(n->getChild(0)->getValue(), -1.0));
    fail_unless(n->getChild(1)->getType() == AST_NAME);
    fail_unless(n->getChild(2)->getType() == AST_NAME);
}
END_TEST

START_TEST(test_refactor_numbers_encompass_unary_minus_4)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <minus/>"
        "       <apply>"
        "          <divide/>"
        "          <ci> a </ci>"
        "          <ci> b </ci>"
        "       </apply>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_MINUS);
    fail_unless(n->getNumChildren() == 1);

    // replace -(a/b) with -1*a/b
    n->encompassUnaryMinus();

    fail_unless(n->getType() == AST_DIVIDE);
    fail_unless(n->getNumChildren() == 2);

    fail_unless(n->getChild(0)->getType() == AST_TIMES);
    fail_unless(util_isEqual(n->getChild(0)->getChild(0)->getValue(), -1.0));
    fail_unless(n->getChild(0)->getChild(1)->getType() == AST_NAME);
    fail_unless(n->getChild(1)->getType() == AST_NAME);
}
END_TEST

START_TEST(test_refactor_create_non_binary_tree_1)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <times/>"
        "       <ci> a </ci>"
        "       <ci> b </ci>"
        "       <ci> c </ci>"
        "       <ci> d </ci>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_TIMES);
    fail_unless(n->getNumChildren() == 2);
    fail_unless(n->getChild(0)->getType() == AST_TIMES);
    fail_unless(n->getChild(1)->getType() == AST_NAME);
    fail_unless(n->getChild(0)->getChild(0)->getType() == AST_TIMES);
    fail_unless(n->getChild(0)->getChild(1)->getType() == AST_NAME);

    n->createNonBinaryTree();

    fail_unless(n->getType() == AST_TIMES);
    fail_unless(n->getNumChildren() == 4);

    fail_unless(n->getChild(0)->getType() == AST_NAME);
    fail_unless(n->getChild(1)->getType() == AST_NAME);
    fail_unless(n->getChild(2)->getType() == AST_NAME);
    fail_unless(n->getChild(3)->getType() == AST_NAME);
}
END_TEST

START_TEST(test_refactor_create_non_binary_tree_2)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <plus/>"
        "       <ci> a </ci>"
        "       <ci> b </ci>"
        "       <ci> c </ci>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_PLUS);
    fail_unless(n->getNumChildren() == 2);
    fail_unless(n->getChild(0)->getType() == AST_PLUS);
    fail_unless(n->getChild(1)->getType() == AST_NAME);
    fail_unless(n->getChild(0)->getChild(0)->getType() == AST_NAME);
    fail_unless(n->getChild(0)->getChild(1)->getType() == AST_NAME);

    n->createNonBinaryTree();

    fail_unless(n->getType() == AST_PLUS);
    fail_unless(n->getNumChildren() == 3);

    fail_unless(n->getChild(0)->getType() == AST_NAME);
    fail_unless(n->getChild(1)->getType() == AST_NAME);
    fail_unless(n->getChild(2)->getType() == AST_NAME);
}
END_TEST

START_TEST(test_refactor_reorder_arguments_1)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <plus/>"
        "      <cn> 4.3 </cn>"
        "      <cn> 2.8 </cn>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_PLUS);
    fail_unless(n->getNumChildren() == 2);
    fail_unless(formulas_equal("4.3 + 2.8", n));

    n->reorderArguments();

    fail_unless(n->getType() == AST_REAL);
    fail_unless(n->getNumChildren() == 0);
    fail_unless(util_isEqual(n->getValue(), 7.1));
    fail_unless(formulas_equal("7.1", n));

}
END_TEST

START_TEST(test_refactor_reorder_arguments_2)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <plus/>"
        "      <ci> a </ci>"
        "      <cn> 2.8 </cn>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_PLUS);
    fail_unless(n->getNumChildren() == 2);
    fail_unless(formulas_equal("a + 2.8", n));

    n->reorderArguments();

    fail_unless(n->getType() == AST_PLUS);
    fail_unless(n->getNumChildren() == 2);
    fail_unless(formulas_equal("2.8 + a", n));

}
END_TEST

START_TEST(test_refactor_reorder_arguments_3)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <times/>"
        "      <cn> 1 </cn>"
        "      <ci> a </ci>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_TIMES);
    fail_unless(n->getNumChildren() == 2);
    fail_unless(formulas_equal("1 * a", n));

    n->reorderArguments();

    fail_unless(n->getType() == AST_NAME);
    fail_unless(n->getNumChildren() == 0);
    fail_unless(formulas_equal("a", n));

}
END_TEST

START_TEST(test_refactor_reorder_arguments_4)
{
    ASTNode* n = readMathMLFromString(
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
        "   <apply>"
        "      <minus/>"
        "      <ci> a </ci>"
        "      <ci> a </ci>"
        "   </apply>"
        "</math>"
    );

    fail_unless(n != NULL);
    fail_unless(n->getType() == AST_MINUS);
    fail_unless(n->getNumChildren() == 2);
    fail_unless(formulas_equal("a - a", n));

    n->reorderArguments();

    fail_unless(n->getType() == AST_REAL);
    fail_unless(n->getNumChildren() == 0);
    fail_unless(formulas_equal("0", n));
    fail_unless(util_isEqual(n->getValue(), 0.0));

}
END_TEST

Suite *
create_suite_TestRefactoringFunctions()
{
  Suite *suite = suite_create("TestRefactoringFunctions");
  TCase *tcase = tcase_create("TestRefactoringFunctions");

  //tcase_add_test(tcase, test_refactor_numbers_integer); 
  ////tcase_add_test(tcase, test_refactor_numbers_real); 
  //tcase_add_test(tcase, test_refactor_numbers_e_notation); 
  //tcase_add_test(tcase, test_refactor_numbers_rational);
  //tcase_add_test(tcase, test_refactor_numbers_encompass_unary_minus_1);
  //tcase_add_test(tcase, test_refactor_numbers_encompass_unary_minus_2);
  //tcase_add_test(tcase, test_refactor_numbers_encompass_unary_minus_3);
  //tcase_add_test(tcase, test_refactor_numbers_encompass_unary_minus_4);
  //tcase_add_test(tcase, test_refactor_create_non_binary_tree_1);
  //tcase_add_test(tcase, test_refactor_create_non_binary_tree_2);
  tcase_add_test(tcase, test_refactor_reorder_arguments_1);
  tcase_add_test(tcase, test_refactor_reorder_arguments_2);
  tcase_add_test(tcase, test_refactor_reorder_arguments_3);
  tcase_add_test(tcase, test_refactor_reorder_arguments_4);

  // TESTED SIMPLIFY UP TO A + A

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

