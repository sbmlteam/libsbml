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

/** @endcond */

CK_CPPSTART

START_TEST(test_refactor_numbers)
{
  ASTNode *n = readMathMLFromString(
     "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
     "    <apply>"
     "        <plus/>"
     "    <cn type=\"integer\"> 1 </cn>"
     "    <cn> 4.0 </cn>"
     "    <cn type=\"e-notation\"> 4.1 <sep/> 2 </cn>"
      "    <cn type=\"rational\"> 4.1 <sep/> 2 </cn>"
     "    </apply>"
     "</math>"
    );
  ASTNodeLevels node_levels = n->getListOfNodesWithLevel();

  printNodeLevels(node_levels);

  fail_unless(n != NULL);
  fail_unless(n->getNumChildren() == 2);
  fail_unless(n->getChild(0)->getChild(1)->getType() == AST_RATIONAL);
  //fail_unless(n->getChild(1)->getType() == AST_REAL);
  //fail_unless(n->getChild(2)->getType() == AST_REAL_E);
  //fail_unless(n->getChild(3)->getType() == AST_RATIONAL);

  //n->refactorNumbers();

  //fail_unless(n->getChild(0)->getType() == AST_REAL);
  //fail_unless(n->getChild(1)->getType() == AST_REAL);
  //fail_unless(n->getChild(2)->getType() == AST_REAL);
  //fail_unless(n->getChild(3)->getType() == AST_REAL);
  //fail_unless(util_isEqual(n->getChild(0)->getValue(), 1.0));
  //fail_unless(util_isEqual(n->getChild(1)->getValue(), 4.0));
  //fail_unless(util_isEqual(n->getChild(2)->getValue(), 410.0));
  //fail_unless(util_isEqual(n->getChild(3)->getValue(), 2.05));
}
END_TEST



Suite *
create_suite_TestRefactoringFunctions()
{
  Suite *suite = suite_create("TestRefactoringFunctions");
  TCase *tcase = tcase_create("TestRefactoringFunctions");

  tcase_add_test(tcase, test_refactor_numbers); 


  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

