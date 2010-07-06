/**
 * \file    TestValidASTNode.cpp
 * \brief   Test the isWellFormedASTNode function
 * \author  Sarah Keating
 *
 * $Id: TestValidASTNode.cpp 7249 2008-06-26 22:48:40Z mhucka $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/math/test/TestValidASTNode.cpp $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <limits>
#include <iostream>
#include <cstdio>
#include <cstring>

#include <check.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>

/** @cond doxygen-ignored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygen-ignored */

CK_CPPSTART


START_TEST (test_ValidASTNode_Number)
{
  ASTNode *n = SBML_parseFormula("1.2");

  fail_unless( n->isWellFormedASTNode() );

  ASTNode *d = SBML_parseFormula("d");
  n->addChild(d);

  fail_unless( !(n->isWellFormedASTNode()) );

  delete n;
}
END_TEST


START_TEST (test_ValidASTNode_Name)
{
  ASTNode *n = SBML_parseFormula("c");

  fail_unless( n->isWellFormedASTNode() );

  ASTNode *d = SBML_parseFormula("d");
  n->addChild(d);

  fail_unless( !(n->isWellFormedASTNode()) );

  delete n;
}
END_TEST


START_TEST (test_ValidASTNode_unary)
{
  ASTNode *n = new ASTNode(AST_FUNCTION_ABS);

  fail_unless( !(n->isWellFormedASTNode()) );

  ASTNode *c = SBML_parseFormula("c");
  n->addChild(c);

  fail_unless( n->isWellFormedASTNode() );

  ASTNode *d = SBML_parseFormula("d");
  n->addChild(d);

  fail_unless( !(n->isWellFormedASTNode()) );

  delete n;
}
END_TEST


START_TEST (test_ValidASTNode_binary)
{
  ASTNode *n = new ASTNode(AST_DIVIDE);

  fail_unless( !(n->isWellFormedASTNode()) );

  ASTNode *c = SBML_parseFormula("c");
  n->addChild(c);

  fail_unless( !(n->isWellFormedASTNode()) );

  ASTNode *d = SBML_parseFormula("d");
  n->addChild(d);

  fail_unless( n->isWellFormedASTNode() );

  delete n;
}
END_TEST


START_TEST (test_ValidASTNode_nary)
{
  ASTNode *n = new ASTNode(AST_TIMES);

  fail_unless( !(n->isWellFormedASTNode()) );

  ASTNode *c = SBML_parseFormula("c");
  n->addChild(c);

  fail_unless( !(n->isWellFormedASTNode()) );

  ASTNode *d = SBML_parseFormula("d");
  n->addChild(d);

  fail_unless( n->isWellFormedASTNode() );

  ASTNode *e = SBML_parseFormula("e");
  n->addChild(e);

  fail_unless( n->isWellFormedASTNode() );

  delete n;
}
END_TEST


START_TEST (test_ValidASTNode_root)
{
  ASTNode *n = new ASTNode(AST_FUNCTION_ROOT);

  fail_unless( !(n->isWellFormedASTNode()) );

  ASTNode *c = SBML_parseFormula("c");
  n->addChild(c);

  fail_unless( n->isWellFormedASTNode() );

  ASTNode *d = SBML_parseFormula("3");
  n->addChild(d);

  fail_unless( n->isWellFormedASTNode() );

  ASTNode *e = SBML_parseFormula("3");
  n->addChild(e);

  fail_unless( !(n->isWellFormedASTNode()) );

  delete n;
}
END_TEST


START_TEST (test_ValidASTNode_lambda)
{
  ASTNode *n = new ASTNode(AST_LAMBDA);
  fail_unless( !(n->isWellFormedASTNode()) );

  ASTNode *c = SBML_parseFormula("c");
  n->addChild(c);

  fail_unless( n->isWellFormedASTNode() );

  ASTNode *d = SBML_parseFormula("d");
  n->addChild(d);

  fail_unless( n->isWellFormedASTNode() );

  ASTNode *e = SBML_parseFormula("e");
  n->addChild(e);

  fail_unless( n->isWellFormedASTNode() );

  delete n;
}
END_TEST


START_TEST (test_ValidASTNode_setType)
{
  ASTNode *n = new ASTNode();
  
  int i = n->setType(AST_REAL);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( n->getType() == AST_REAL);
  
  i = n->setType(AST_PLUS);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( n->getType() == AST_PLUS);
  fail_unless( n->getCharacter() == '+' );
  
  i = n->setType(AST_FUNCTION_ARCCOSH);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( n->getType() == AST_FUNCTION_ARCCOSH);
  
  i = n->setType(AST_UNKNOWN);

  fail_unless( i == LIBSBML_INVALID_ATTRIBUTE_VALUE);
  fail_unless( n->getType() == AST_UNKNOWN);
  
  delete n;
}
END_TEST


Suite *
create_suite_TestValidASTNode ()
{
  Suite *suite = suite_create("TestValidASTNode");
  TCase *tcase = tcase_create("TestValidASTNode");

  tcase_add_test( tcase, test_ValidASTNode_Number       );
  tcase_add_test( tcase, test_ValidASTNode_Name         );
  tcase_add_test( tcase, test_ValidASTNode_unary         );
  tcase_add_test( tcase, test_ValidASTNode_binary         );
  tcase_add_test( tcase, test_ValidASTNode_nary         );
  tcase_add_test( tcase, test_ValidASTNode_root        );
  tcase_add_test( tcase, test_ValidASTNode_lambda        );
  tcase_add_test( tcase, test_ValidASTNode_setType        );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
