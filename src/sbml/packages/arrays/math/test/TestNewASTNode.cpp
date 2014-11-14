/**
 * \file    TestNewNewASTNode.cpp
 * \brief   ASTNode unit tests
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

#include <sbml/common/common.h>
#include <sbml/util/List.h>

#include <sbml/math/ASTNode.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/EventAssignment.h>
#include <sbml/Model.h>
#include <sbml/SBMLDocument.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>

#include <limits.h>
#include <check.h>

#if defined(WIN32) && !defined(CYGWIN)
#include <math.h>
extern int isnan(double x); 
extern int isinf(double x); 
extern int finite(double x);
#endif


#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif



START_TEST (test_ASTNode_create)
{
  ASTNode *n = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);


  fail_unless( n->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( n->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  fail_unless( n->getCharacter() == '\0' );
  fail_unless( strcmp(n->getName(), "vector") == 0);
  fail_unless( n->getInteger  () == 0    );
  fail_unless( n->getExponent () == 0    );

  fail_unless( n->getNumChildren() == 0 );

  fail_unless( n->getParentSBMLObject() == NULL );

  fail_unless( n->getPackageName() == "arrays");

  // vector with no args is fine
  fail_unless( n->isWellFormedASTNode() == true);
  fail_unless( n->hasCorrectNumberArguments() == true);

  delete n;
}
END_TEST


START_TEST (test_ASTNode_deepCopy_1)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  ASTNode *child, *copy;


  node->addChild(new ASTNode(AST_INTEGER) );
  node->addChild(new ASTNode(AST_INTEGER) );

  node->getLeftChild()->setValue((long)(1) );
  node->getRightChild()->setValue(long (2) );

  fail_unless( node->getType() == AST_ORIGINATES_IN_PACKAGE );
  fail_unless( node->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR );
  fail_unless( node->getNumChildren() == 2        );

  child = node->getLeftChild();

  fail_unless( child->getType       () == AST_INTEGER );
  fail_unless( child->getInteger    () == 1           );
  fail_unless( child->getNumChildren() == 0           );

  child = node->getRightChild();

  fail_unless( child->getType       () == AST_INTEGER );
  fail_unless( child->getInteger    () == 2           );
  fail_unless( child->getNumChildren() == 0           );

  /** deepCopy() **/
  copy = node->deepCopy();

  fail_unless( copy != node );
  fail_unless( copy->getType() == AST_ORIGINATES_IN_PACKAGE );
  fail_unless( copy->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR );
  fail_unless( copy->getNumChildren() == 2        );

  child = copy->getLeftChild();

  fail_unless( child != node->getLeftChild() );
  fail_unless( child->getType       () == AST_INTEGER );
  fail_unless( child->getInteger    () == 1           );
  fail_unless( child->getNumChildren() == 0           );

  child = copy->getRightChild();
  fail_unless( child != node->getRightChild() );
  fail_unless( child->getType       () == AST_INTEGER );
  fail_unless( child->getInteger    () == 2           );
  fail_unless( child->getNumChildren() == 0           );

  fail_unless(node->isWellFormedASTNode() == true);
  fail_unless(node->hasCorrectNumberArguments() == true);

  delete node;
  delete copy;
}
END_TEST


START_TEST (test_ASTNode_setNewTypes_1)
{
  ASTNode *node = new ASTNode(AST_INTEGER);
  Model* m = new Model(3,1);
  node->setValue((long)(2));
  node->setId("s");
  node->setUnits("mole");
  node->setParentSBMLObject(m);

  fail_unless( node->getType() == AST_INTEGER);
  fail_unless( node->getPackageName() == "core");
  fail_unless( node->getInteger() == 2);
  fail_unless( util_isEqual(node->getMantissa(), 0));
  fail_unless( node->getExponent() == 0);
  fail_unless( util_isEqual(node->getReal(), 0));
  fail_unless( node->getDenominator() == 1);
  fail_unless( node->getNumerator() == 2);
  fail_unless( node->getId() == "s");
  fail_unless( node->getUnits() == "mole");
  fail_unless( node->getParentSBMLObject() == m);

  node->setType(AST_LOGICAL_EXISTS);

  fail_unless( node->getType() == AST_ORIGINATES_IN_PACKAGE );
  fail_unless( node->getExtendedType() == AST_LOGICAL_EXISTS );
  fail_unless( node->getPackageName() == "arrays");
  fail_unless( node->getInteger() == 0);
  fail_unless( util_isEqual(node->getMantissa(), 0));
  fail_unless( node->getExponent() == 0);
  fail_unless( util_isEqual(node->getReal(), 0));
  fail_unless( node->getDenominator() == 1);
  fail_unless( node->getNumerator() == 0);
  fail_unless( node->getId() == "s");
  fail_unless( node->getUnits() == "");
  fail_unless( node->getParentSBMLObject() == m);

  delete node;
  delete m;
}
END_TEST


START_TEST (test_ASTNode_replaceArgument)
{
  ASTNode *arg = new ASTNode();
  ASTNode *node = new ASTNode();
  ASTNode *c1 = new ASTNode();
  ASTNode *c2 = new ASTNode();
  const char * varName = "foo";

  node->setType(AST_LINEAR_ALGEBRA_SELECTOR);
  c1->setName(varName);
  c2->setName("foo2");
  node->addChild(c1);
  node->addChild(c2);

  arg->setName("rep1");

  fail_unless( !strcmp(node->getChild(0)->getName(), "foo")); 
  fail_unless( !strcmp(node->getChild(1)->getName(), "foo2")); 

  node->replaceArgument(varName, arg);

  fail_unless( !strcmp(node->getChild(0)->getName(), "rep1")); 
  fail_unless( !strcmp(node->getChild(1)->getName(), "foo2")); 

  delete arg;
  delete node;
}
END_TEST


START_TEST (test_ASTNode_removeChild)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);
  int i = 0;

  c1->setName("foo");
  c1->setName("foo2");
  node->addChild(c1);
  node->addChild(c2);

  fail_unless( node->getNumChildren() == 2); 


  i = node->removeChild(0);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 1); 

  i = node->removeChild(1);

  fail_unless( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless( node->getNumChildren() == 1); 

  i = node->removeChild(0);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 0); 

  delete node;
  delete c1;
  delete c2;
}
END_TEST


START_TEST (test_ASTNode_replaceChild)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);
  ASTNode *c3 = new ASTNode(AST_NAME);
  ASTNode *newc = new ASTNode(AST_NAME);
  int i = 0;

  c1->setName("a");
  c2->setName("b");
  c3->setName("c");
  node->addChild(c1);
  node->addChild(c2);
  node->addChild(c3);

  fail_unless( node->getNumChildren() == 3); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "b"));

  newc->setName("d");

  i = node->replaceChild(0, newc);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 3); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "d"));

  i = node->replaceChild(3, newc);

  fail_unless( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless( node->getNumChildren() == 3); 

  i = node->replaceChild(1, c1);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 3); 
  fail_unless( !strcmp(node->getChild(1)->getName(), "a"));

  delete node;
  delete c2;
}
END_TEST


START_TEST (test_ASTNode_insertChild)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);
  ASTNode *c3 = new ASTNode(AST_NAME);
  ASTNode *newc = new ASTNode(AST_NAME);
  ASTNode *newc1 = new ASTNode(AST_NAME);
  int i = 0;

  c1->setName("a");
  c2->setName("b");
  c3->setName("c");
  node->addChild(c1);
  node->addChild(c2);
  node->addChild(c3);

  fail_unless( node->getNumChildren() == 3); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "b"));
  fail_unless( !strcmp(node->getChild(2)->getName(), "c"));

  newc->setName("d");
  newc1->setName("e");

  i = node->insertChild(1, newc);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 4); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "d"));
  fail_unless( !strcmp(node->getChild(2)->getName(), "b"));
  fail_unless( !strcmp(node->getChild(3)->getName(), "c"));

  i = node->insertChild(5, newc);

  fail_unless( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless( node->getNumChildren() == 4); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "d"));
  fail_unless( !strcmp(node->getChild(2)->getName(), "b"));
  fail_unless( !strcmp(node->getChild(3)->getName(), "c"));

  i = node->insertChild(2, newc1);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 5); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "d"));
  fail_unless( !strcmp(node->getChild(2)->getName(), "e"));
  fail_unless( !strcmp(node->getChild(3)->getName(), "b"));
  fail_unless( !strcmp(node->getChild(4)->getName(), "c"));

  delete node;
}
END_TEST


START_TEST (test_ASTNode_swapChildren)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);
  ASTNode *node_1 = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  ASTNode *c1_1 = new ASTNode(AST_NAME);
  ASTNode *c2_1 = new ASTNode(AST_NAME);
  int i = 0;

  c1->setName("a");
  c2->setName("b");
  node->addChild( c1);
  node->addChild( c2);

  fail_unless( node->getNumChildren() == 2); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "b"));

  c1_1->setName("d");
  c2_1->setName("f");
  node_1->addChild(c1_1);
  node_1->addChild(c2_1);

  fail_unless( node_1->getNumChildren() == 2); 
  fail_unless( !strcmp(node_1->getChild(0)->getName(), "d"));
  fail_unless( !strcmp(node_1->getChild(1)->getName(), "f"));

  i = node->swapChildren( node_1);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 2); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "d"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "f"));

  fail_unless( node_1->getNumChildren() == 2); 
  fail_unless( !strcmp(node_1->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node_1->getChild(1)->getName(), "b"));

  delete node;
  delete node_1;
}
END_TEST


START_TEST (test_ASTNode_swapChildren_1)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);
  ASTNode *node_1 = new ASTNode(AST_TIMES);
  ASTNode *c1_1 = new ASTNode(AST_NAME);
  ASTNode *c2_1 = new ASTNode(AST_NAME);
  int i = 0;

  c1->setName("a");
  c2->setName("b");
  node->addChild( c1);
  node->addChild( c2);

  fail_unless( node->getNumChildren() == 2); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "b"));

  c1_1->setName("d");
  c2_1->setName("f");
  node_1->addChild(c1_1);
  node_1->addChild(c2_1);

  fail_unless( node_1->getNumChildren() == 2); 
  fail_unless( !strcmp(node_1->getChild(0)->getName(), "d"));
  fail_unless( !strcmp(node_1->getChild(1)->getName(), "f"));

  i = node->swapChildren( node_1);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 2); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "d"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "f"));

  fail_unless( node_1->getNumChildren() == 2); 
  fail_unless( !strcmp(node_1->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node_1->getChild(1)->getName(), "b"));

  delete node;
  delete node_1;
}
END_TEST


START_TEST (test_ASTNode_addChild1)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);
  ASTNode *c1_1 = new ASTNode(AST_NAME);
  int i = 0;

  c1->setName("a");
  c2->setName("b");
  node->addChild(c1);
  node->addChild(c2);

  fail_unless( node->getNumChildren() == 2); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "b"));

  c1_1->setName("d");

  i = node->addChild(c1_1);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 3); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "b"));
  fail_unless( !strcmp(node->getChild(2)->getName(), "d"));

  delete node;
}
END_TEST


START_TEST (test_ASTNode_prependChild1)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);
  ASTNode *c1_1 = new ASTNode(AST_NAME);
  int i = 0;

  c1->setName("a");
  c2->setName("b");
  node->addChild(c1);
  node->addChild(c2);

  fail_unless( node->getNumChildren() == 2); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "b"));

  c1_1->setName("d");

  i = node->prependChild(c1_1);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( node->getNumChildren() == 3); 
  fail_unless( !strcmp(node->getChild(0)->getName(), "d"));
  fail_unless( !strcmp(node->getChild(1)->getName(), "a"));
  fail_unless( !strcmp(node->getChild(2)->getName(), "b"));

  delete node;
}
END_TEST


START_TEST (test_ASTNode_isWellFormed_selector)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);

  // selector must have at least one argument and no more than 3
  fail_unless(node->isWellFormedASTNode() == false);
  fail_unless(node->hasCorrectNumberArguments() == false);

  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);
  ASTNode *c3 = new ASTNode(AST_NAME);
  ASTNode *c4 = new ASTNode(AST_NAME);

  c1->setName("a");
  c2->setName("b");
  c3->setName("a");
  c4->setName("b");


  node->addChild(c1);
  
  // selector must have at least one argument and no more than 3
  fail_unless(node->isWellFormedASTNode() == true);
  fail_unless(node->hasCorrectNumberArguments() == true);

  node->addChild(c2);
  
  // selector must have at least one argument and no more than 3
  fail_unless(node->isWellFormedASTNode() == true);
  fail_unless(node->hasCorrectNumberArguments() == true);

  node->addChild(c3);
  
  // selector must have at least one argument and no more than 3
  fail_unless(node->isWellFormedASTNode() == true);
  fail_unless(node->hasCorrectNumberArguments() == true);

  node->addChild(c4);
  
  // selector must have at least one argument and no more than 3
  fail_unless(node->isWellFormedASTNode() == false);
  fail_unless(node->hasCorrectNumberArguments() == false);

  delete node;
}
END_TEST

#if (0)
START_TEST (test_ASTNode_isWellFormed_determinant)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_DETERMINANT);

  // determinant should have one arg
  fail_unless(node->isWellFormedASTNode() == false);
  fail_unless(node->hasCorrectNumberArguments() == false);

  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);

  c1->setName("a");
  c2->setName("b");


  node->addChild(c1);
  
  // determinant should have one arg
  fail_unless(node->isWellFormedASTNode() == true);
  fail_unless(node->hasCorrectNumberArguments() == true);

  node->addChild(c2);
  
  // determinant should have one arg
  fail_unless(node->isWellFormedASTNode() == false);
  fail_unless(node->hasCorrectNumberArguments() == false);

  delete node;
}
END_TEST


START_TEST (test_ASTNode_isWellFormed_vectorproduct)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR_PRODUCT);

  // vectorproduct should have 2 args
  fail_unless(node->isWellFormedASTNode() == false);
  fail_unless(node->hasCorrectNumberArguments() == false);

  ASTNode *c1 = new ASTNode(AST_NAME);
  ASTNode *c2 = new ASTNode(AST_NAME);
  ASTNode *c3 = new ASTNode(AST_NAME);

  c1->setName("a");
  c2->setName("b");
  c3->setName("a");

  node->addChild(c1);
  
  // vectorproduct should have 2 args
  fail_unless(node->isWellFormedASTNode() == false);
  fail_unless(node->hasCorrectNumberArguments() == false);

  node->addChild(c2);
  
  // vectorproduct should have 2 args
  fail_unless(node->isWellFormedASTNode() == true);
  fail_unless(node->hasCorrectNumberArguments() == true);

  node->addChild(c3);
  
  // vectorproduct should have 2 args
  fail_unless(node->isWellFormedASTNode() == false);
  fail_unless(node->hasCorrectNumberArguments() == false);

  delete node;
}
END_TEST


START_TEST (test_ASTNode_isWellFormed_matrix)
{
  ASTNode *node = new ASTNode(AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);

  // matrix can be empty
  fail_unless(node->isWellFormedASTNode() == true);
  fail_unless(node->hasCorrectNumberArguments() == true);

  // matrix children must be of type matrixrow

  ASTNode *c1 = new ASTNode(AST_NAME);
  c1->setName("a");
  node->addChild(c1);

  // not a matrix row child
  fail_unless(node->isWellFormedASTNode() == false);
  fail_unless(node->hasCorrectNumberArguments() == true);


  ASTNode *mrow = new ASTNode(AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  node = new ASTNode(AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  node->addChild(mrow);
  
  // has a matrixrow child
  fail_unless(node->isWellFormedASTNode() == true);
  fail_unless(node->hasCorrectNumberArguments() == true);

  delete node;
}
END_TEST
#endif

Suite *
create_suite_NewASTNode (void) 
{ 
  Suite *suite = suite_create("ASTNode");
  TCase *tcase = tcase_create("ASTNode");


  tcase_add_test( tcase, test_ASTNode_create                  );

  tcase_add_test( tcase, test_ASTNode_deepCopy_1              );



  tcase_add_test( tcase, test_ASTNode_setNewTypes_1             );

  tcase_add_test( tcase, test_ASTNode_replaceArgument         );
  tcase_add_test( tcase, test_ASTNode_removeChild             );
  tcase_add_test( tcase, test_ASTNode_replaceChild            );
  tcase_add_test( tcase, test_ASTNode_insertChild             );
  tcase_add_test( tcase, test_ASTNode_swapChildren            );
  tcase_add_test( tcase, test_ASTNode_swapChildren_1          );
  tcase_add_test( tcase, test_ASTNode_addChild1               );
  tcase_add_test( tcase, test_ASTNode_prependChild1           );

  tcase_add_test( tcase, test_ASTNode_isWellFormed_selector           );
#if (0)
  tcase_add_test( tcase, test_ASTNode_isWellFormed_determinant        );
  tcase_add_test( tcase, test_ASTNode_isWellFormed_vectorproduct      );
  tcase_add_test( tcase, test_ASTNode_isWellFormed_matrix             );
#endif

  
  suite_add_tcase(suite, tcase);

  return suite;
}

#if defined(__cplusplus)
CK_CPPEND
#endif


