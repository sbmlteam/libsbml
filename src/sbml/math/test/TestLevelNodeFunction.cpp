/**
 * \file    TestLevelNodeFunctions.cpp
 * \brief   MathML unit tests for the level of a operator within an expression
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

#include <sbml/math/L3FormulaFormatter.h>
#include <sbml/math/L3Parser.h>
#include <sbml/math/L3ParserSettings.h>
#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>

/** @cond doxygenLibsbmlInternal */

#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif
/** @endcond */

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


START_TEST(test_null)
{
#ifdef _MSC_VER
    ASTNode* node = NULL; 
    ASTNodeLevels node_levels = node->getListOfNodesWithLevel();
    fail_unless(node_levels.size() == 0);

    delete node;
    node_levels.clear();
#endif
}
END_TEST

START_TEST(test_level_1)
{

    ASTNode* node = SBML_parseL3Formula("x + y");
    ASTNodeLevels node_levels = node->getListOfNodesWithLevel();
    fail_unless(node_levels.size() == 3);
    fail_unless(node_levels[0].first == 0);
    fail_unless(formulas_equal("x + y", node_levels[0].second));
    fail_unless(node_levels[1].first == 1);
    fail_unless(formulas_equal("x", node_levels[1].second));
    fail_unless(node_levels[2].first == 1);
    fail_unless(formulas_equal("y", node_levels[2].second));


    delete node;
    node_levels.clear();
}
END_TEST

START_TEST(test_level_2)
{

    ASTNode* node = SBML_parseL3Formula("k - x - y");
    ASTNodeLevels node_levels = node->getListOfNodesWithLevel();
    fail_unless(node_levels.size() == 5);
    fail_unless(node_levels[0].first == 0);
    fail_unless(formulas_equal("k - x - y", node_levels[0].second));
    fail_unless(node_levels[1].first == 1);
    fail_unless(formulas_equal("k - x", node_levels[1].second));
    fail_unless(node_levels[2].first == 2);
    fail_unless(formulas_equal("k", node_levels[2].second));
    fail_unless(node_levels[3].first == 2);
    fail_unless(formulas_equal("x", node_levels[3].second));
    fail_unless(node_levels[4].first == 1);
    fail_unless(formulas_equal("y", node_levels[4].second));


    delete node;
    node_levels.clear();
}
END_TEST


START_TEST(test_level_3)
{
    ASTNode* node = SBML_parseL3Formula("k + v - x - y");
    ASTNodeLevels node_levels = node->getListOfNodesWithLevel();
    fail_unless(node_levels.size() == 7);
    fail_unless(node_levels[0].first == 0);
    fail_unless(formulas_equal("k + v - x - y", node_levels[0].second));
    fail_unless(node_levels[1].first == 1);
    fail_unless(formulas_equal("k + v - x", node_levels[1].second));
    fail_unless(node_levels[2].first == 2);
    fail_unless(formulas_equal("k + v", node_levels[2].second));
    fail_unless(node_levels[3].first == 3);
    fail_unless(formulas_equal("k", node_levels[3].second));
    fail_unless(node_levels[4].first == 3);
    fail_unless(formulas_equal("v", node_levels[4].second));
    fail_unless(node_levels[5].first == 2);
    fail_unless(formulas_equal("x", node_levels[5].second));
    fail_unless(node_levels[6].first == 1);
    fail_unless(formulas_equal("y", node_levels[6].second));

    delete node;
    node_levels.clear();
}
END_TEST

START_TEST(test_level_4)
{
    ASTNode* node = SBML_parseL3Formula("w + (k - x) - y");
    ASTNodeLevels node_levels = node->getListOfNodesWithLevel();
    fail_unless(node_levels.size() == 7);
    fail_unless(node_levels[0].first == 0);
    fail_unless(formulas_equal("w + (k - x) - y", node_levels[0].second));
    fail_unless(node_levels[1].first == 1);
    fail_unless(formulas_equal("w + (k - x)", node_levels[1].second));
    fail_unless(node_levels[2].first == 2);
    fail_unless(formulas_equal("k - x", node_levels[2].second));
    fail_unless(node_levels[3].first == 2);
    fail_unless(formulas_equal("w", node_levels[3].second));
    fail_unless(node_levels[4].first == 3);
    fail_unless(formulas_equal("k", node_levels[4].second));
    fail_unless(node_levels[5].first == 3);
    fail_unless(formulas_equal("x", node_levels[5].second));
    fail_unless(node_levels[6].first == 1);
    fail_unless(formulas_equal("y", node_levels[6].second));

    delete node;
    node_levels.clear();
}
END_TEST

START_TEST(test_level_5)
{
    ASTNode* node = SBML_parseL3Formula("k - x");
    ASTNodeLevels node_levels = node->getListOfNodesWithLevel();
    fail_unless(node_levels.size() == 3);
    fail_unless(node_levels[0].first == 0);
    fail_unless(formulas_equal("k - x", node_levels[0].second));
    fail_unless(node_levels[1].first == 1);
    fail_unless(formulas_equal("k", node_levels[1].second));
    fail_unless(node_levels[2].first == 1);
    fail_unless(formulas_equal("x", node_levels[2].second));

    delete node;
    node_levels.clear();
}
END_TEST

START_TEST(test_level_6)
{
    ASTNode* node = SBML_parseL3Formula("k + v - x");
    ASTNodeLevels node_levels = node->getListOfNodesWithLevel();
    fail_unless(node_levels.size() == 5);
    fail_unless(node_levels[0].first == 0);
    fail_unless(formulas_equal("k + v - x", node_levels[0].second));
    fail_unless(node_levels[1].first == 1);
    fail_unless(formulas_equal("k + v", node_levels[1].second));
    fail_unless(node_levels[2].first == 2);
    fail_unless(formulas_equal("k", node_levels[2].second));
    fail_unless(node_levels[3].first == 2);
    fail_unless(formulas_equal("v", node_levels[3].second));
    fail_unless(node_levels[4].first == 1);
    fail_unless(formulas_equal("x", node_levels[4].second));

    delete node;
    node_levels.clear();
}
END_TEST

START_TEST(test_level_7)
{
    ASTNode* node = SBML_parseL3Formula("(k + v - x - y) + (l - m)");
    ASTNodeLevels node_levels = node->getListOfNodesWithLevel();
    fail_unless(node_levels.size() == 11);
    fail_unless(node_levels[0].first == 0);
    fail_unless(formulas_equal("(k + v - x - y) + (l - m)", node_levels[0].second));
    fail_unless(node_levels[1].first == 1);
    fail_unless(formulas_equal("k + v - x - y", node_levels[1].second));
    fail_unless(node_levels[2].first == 2);
    fail_unless(formulas_equal("k + v - x", node_levels[2].second));
    fail_unless(node_levels[3].first == 3);
    fail_unless(formulas_equal("k + v", node_levels[3].second));
    fail_unless(node_levels[4].first == 1);
    fail_unless(formulas_equal("l - m", node_levels[4].second));
    fail_unless(node_levels[5].first == 4);
    fail_unless(formulas_equal("k", node_levels[5].second));
    fail_unless(node_levels[6].first == 4);
    fail_unless(formulas_equal("v", node_levels[6].second));
    fail_unless(node_levels[7].first == 3);
    fail_unless(formulas_equal("x", node_levels[7].second));
    fail_unless(node_levels[8].first == 2);
    fail_unless(formulas_equal("y", node_levels[8].second));
    fail_unless(node_levels[9].first == 2);
    fail_unless(formulas_equal("l", node_levels[9].second));
    fail_unless(node_levels[10].first == 2);
    fail_unless(formulas_equal("m", node_levels[10].second));

    delete node;
    node_levels.clear();
}
END_TEST


Suite *
create_suite_TestLevelNodeFunction()
{
  Suite *suite = suite_create("TestLevelNodeFunction");
  TCase *tcase = tcase_create("TestLevelNodeFunction");

  tcase_add_test(tcase, test_null); 
  tcase_add_test(tcase, test_level_1);
  tcase_add_test(tcase, test_level_2);
  tcase_add_test(tcase, test_level_3);
  tcase_add_test(tcase, test_level_4);
  tcase_add_test(tcase, test_level_5);
  tcase_add_test(tcase, test_level_6);
  tcase_add_test(tcase, test_level_7);

  suite_add_tcase(suite, tcase);

  return suite;
}


#if defined(__cplusplus)
CK_CPPEND
#endif
