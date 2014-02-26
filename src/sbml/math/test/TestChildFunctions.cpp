/**
 * \file    TestChildFunctions.cpp
 * \brief   MathML unit tests for child manipulation functions
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>

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
TestChildFunctions_setup ()
{
  N = NULL;
  S = NULL;
}


void
TestChildFunctions_teardown ()
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


START_TEST (test_ChildFunctions_addToPiecewise_1)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <cn> 0 </cn>\n"
    "      <apply>\n"
    "        <eq/>\n"
    "        <ci> x </ci>\n"
    "        <cn> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "    <otherwise>\n"
    "      <ci> newChild </ci>\n"
    "    </otherwise>\n"
    "  </piecewise>\n"
  );

  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <cn>0</cn>"
    "    <apply> <eq/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  ASTNode * newChild = new ASTNode(AST_NAME);
  newChild->setName("newChild");
  
  ASTNode * newChild1 = new ASTNode(AST_NAME);
  newChild1->setName("newChild1");

  int i = N->addChild(newChild);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 3);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_ChildFunctions_addToPiecewise_2)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <cn> 0 </cn>\n"
    "      <apply>\n"
    "        <eq/>\n"
    "        <ci> x </ci>\n"
    "        <cn> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "    <piece>\n"
    "      <ci> newChild </ci>\n"
    "      <ci> newChild1 </ci>\n"
    "    </piece>\n"
    "  </piecewise>\n"
  );

  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <cn>0</cn>"
    "    <apply> <eq/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  ASTNode * newChild = new ASTNode(AST_NAME);
  newChild->setName("newChild");
  
  ASTNode * newChild1 = new ASTNode(AST_NAME);
  newChild1->setName("newChild1");

  int i = N->addChild(newChild);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 3);

  i = N->addChild(newChild1);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 4);  
  
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_ChildFunctions_addToPiecewise_3)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <cn> 0 </cn>\n"
    "      <true/>\n"
    "    </piece>\n"
    "  </piecewise>\n"
  );

  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <cn>0</cn>"
    "  </piece>"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 2 children */
  fail_unless(N->getNumChildren() == 1);

  ASTNode * newChild = new ASTNode(AST_CONSTANT_TRUE);

  int i = N->addChild(newChild);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 2);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_ChildFunctions_getChild)
{

  N = new ASTNode(AST_TIMES);

  ASTNode * c1 = new ASTNode(AST_NAME);
  c1->setName("c1");
  N->addChild(c1);
  ASTNode * c2 = new ASTNode(AST_NAME);
  c2->setName("c2");
  N->addChild(c2);

  /* we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  /* check we fail nicely if we try to access more children */
  ASTNode * child = N->getChild(2);

  fail_unless ( child == NULL);

  child = N->getChild(3);

  fail_unless ( child == NULL);
  
  child = N->getChild(1);

  fail_unless ( child->getType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "c2") == 0); 
}
END_TEST


START_TEST (test_ChildFunctions_getChildFromPiecewise_1)
{
  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <ci>y</ci>"
    "    <apply> <eq/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "    <otherwise>\n"
    "      <ci> x </ci>\n"
    "    </otherwise>\n"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 3 children */
  fail_unless(N->getNumChildren() == 3);

  /* check we fail nicely if we try to access more children */
  ASTNode * child = N->getChild(4);

  fail_unless ( child == NULL);

  child = N->getChild(3);

  fail_unless ( child == NULL);

  child = N->getChild(2);

  fail_unless ( child->getType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "x") == 0); 

  child = N->getChild(1);

  fail_unless ( child->getType() == AST_RELATIONAL_EQ);

  child = N->getChild(0);

  fail_unless ( child->getType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "y") == 0); 
}
END_TEST


START_TEST (test_ChildFunctions_getChildFromPiecewise_2)
{
  N = new ASTNode(AST_FUNCTION_PIECEWISE);

  ASTNode * c1 = new ASTNode(AST_NAME);
  c1->setName("y");
  N->addChild(c1);
  ASTNode * c2 = new ASTNode(AST_CONSTANT_TRUE);
  N->addChild(c2);
  ASTNode * c3 = new ASTNode(AST_NAME);
  c3->setName("x");
  N->addChild(c3);

  /* old behaviour - we should have 3 children */
  fail_unless(N->getNumChildren() == 3);

  /* check we fail nicely if we try to access more children */
  ASTNode * child = N->getChild(4);

  fail_unless ( child == NULL);

  child = N->getChild(3);

  fail_unless ( child == NULL);

  child = N->getChild(2);

  fail_unless ( child->getType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "x") == 0); 

  child = N->getChild(1);

  fail_unless ( child->getType() == AST_CONSTANT_TRUE);

  child = N->getChild(0);

  fail_unless ( child->getType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "y") == 0); 
}
END_TEST


START_TEST (test_ChildFunctions_remove)
{

  N = new ASTNode(AST_TIMES);

  ASTNode * c1 = new ASTNode(AST_NAME);
  c1->setName("c1");
  N->addChild(c1);
  ASTNode * c2 = new ASTNode(AST_NAME);
  c2->setName("c2");
  N->addChild(c2);

  /* we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  /* check we fail nicely if we try to access more children */
  int i = N->removeChild(3);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless(N->getNumChildren() == 2);

  i = N->removeChild(2);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless(N->getNumChildren() == 2);

  i = N->removeChild(0);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 1);

  ASTNode *child = N->getChild(0);

  fail_unless( child->getType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "c2") == 0); 
}
END_TEST


START_TEST (test_ChildFunctions_removeFromPiecewise_1)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <cn> 0 </cn>\n"
    "      <apply>\n"
    "        <eq/>\n"
    "        <ci> x </ci>\n"
    "        <cn> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "    <piece>\n"
    "      <ci> x </ci>\n"
    "      <apply>\n"
    "        <gt/>\n"
    "        <ci> x </ci>\n"
    "        <cn> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "  </piecewise>\n"
  );

  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <apply> <cos/> <ci>x</ci> </apply>"
    "    <apply> <lt/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "  <piece>"
    "    <cn>0</cn>"
    "    <apply> <eq/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "  <piece>"
    "    <ci>x</ci>"
    "    <apply> <gt/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 6 children */
  fail_unless(N->getNumChildren() == 6);

  int i = N->removeChild(0);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);

  /* old behaviour - we should have 5 children 
   * although the interpretation of the piecewise would be a complete mess
   */
  fail_unless(N->getNumChildren() == 5);
  
  i = N->removeChild(0);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);

  /* old behaviour - we should have 4 children 
   */
  fail_unless(N->getNumChildren() == 4);


  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );

}
END_TEST


START_TEST (test_ChildFunctions_removeFromPiecewise_2)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <cn> 0 </cn>\n"
    "      <apply>\n"
    "        <eq/>\n"
    "        <ci> x </ci>\n"
    "        <cn> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "  </piecewise>\n"
  );

  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <cn>0</cn>"
    "    <apply> <eq/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "    <otherwise>\n"
    "      <ci> x </ci>\n"
    "    </otherwise>\n"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 3 children */
  fail_unless(N->getNumChildren() == 3);

  int i = N->removeChild(2);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);

  /* old behaviour - we should have 2 children */
  fail_unless(N->getNumChildren() == 2);


  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );

}
END_TEST


START_TEST (test_ChildFunctions_removeFromPiecewise_3)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <cn> 0 </cn>\n"
    "    </piece>\n"
    "  </piecewise>\n"
  );

  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <cn>0</cn>"
    "    <apply> <eq/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  int i = N->removeChild(1);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);

  /* old behaviour - we should have 1 children 
   * although the interpretation of the piecewise would be a complete mess
   * except there was only one piece so its reasonably clean
   */
  fail_unless(N->getNumChildren() == 1);
  
  /* lets look at the bad piecewise */
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );

}
END_TEST


START_TEST (test_ChildFunctions_replace)
{

  N = new ASTNode(AST_TIMES);

  ASTNode * c1 = new ASTNode(AST_NAME);
  c1->setName("c1");
  N->addChild(c1);
  ASTNode * c2 = new ASTNode(AST_NAME);
  c2->setName("c2");
  N->addChild(c2);

  ASTNode * newChild = new ASTNode(AST_NAME);
  newChild->setName("newChild");
  
  /* we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  /* check we fail nicely if we try to access more children */
  int i = N->replaceChild(3, newChild);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless(N->getNumChildren() == 2);

  i = N->replaceChild(2, newChild);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless(N->getNumChildren() == 2);

  i = N->replaceChild(1, newChild);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 2);

  ASTNode *child = N->getChild(1);

  fail_unless( child->getType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "newChild") == 0); 
}
END_TEST


START_TEST (test_ChildFunctions_insert)
{

  N = new ASTNode(AST_TIMES);

  ASTNode * c1 = new ASTNode(AST_NAME);
  c1->setName("c1");
  N->addChild(c1);
  ASTNode * c2 = new ASTNode(AST_NAME);
  c2->setName("c2");
  N->addChild(c2);

  ASTNode * newChild = new ASTNode(AST_NAME);
  newChild->setName("newChild");
  
  ASTNode * newChild1 = new ASTNode(AST_NAME);
  newChild1->setName("newChild1");

  /* we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  /* check we fail nicely if we try to access more children */
  int i = N->insertChild(3, newChild);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless(N->getNumChildren() == 2);

  /* we can insert here because it will just go on the end */
  i = N->insertChild(2, newChild);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 3);
  fail_unless(strcmp(SBML_formulaToString(N), "c1 * c2 * newChild") == 0);

  i = N->insertChild(1, newChild1);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 4);
  fail_unless(strcmp(SBML_formulaToString(N), "c1 * newChild1 * c2 * newChild") == 0);

  ASTNode *child = N->getChild(1);

  fail_unless( child->getType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "newChild1") == 0); 
}
END_TEST


START_TEST (test_ChildFunctions_insertIntoPiecewise_1)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <cn> 0 </cn>\n"
    "      <apply>\n"
    "        <eq/>\n"
    "        <ci> x </ci>\n"
    "        <cn> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "    <otherwise>\n"
    "      <ci> newChild </ci>\n"
    "    </otherwise>\n"
    "  </piecewise>\n"
  );

  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <cn>0</cn>"
    "    <apply> <eq/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  ASTNode * newChild = new ASTNode(AST_NAME);
  newChild->setName("newChild");
  
  /* check we fail nicely if we try to access more children */
  int i = N->insertChild(3, newChild);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless(N->getNumChildren() == 2);

  /* we can insert here because it will just go on the end */
  i = N->insertChild(2, newChild);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 3);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );

}
END_TEST


START_TEST (test_ChildFunctions_insertIntoPiecewise_2)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <cn> 0 </cn>\n"
    "      <ci> newChild </ci>\n"
    "    </piece>\n"
    "    <otherwise>\n"
    "      <apply>\n"
    "        <eq/>\n"
    "        <ci> x </ci>\n"
    "        <cn> 0 </cn>\n"
    "      </apply>\n"
    "    </otherwise>\n"
    "  </piecewise>\n"
  );

  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <cn>0</cn>"
    "    <apply> <eq/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  ASTNode * newChild = new ASTNode(AST_NAME);
  newChild->setName("newChild");
  
  /* check we fail nicely if we try to access more children */
  int i = N->insertChild(3, newChild);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE);
  fail_unless(N->getNumChildren() == 2);

  i = N->insertChild(1, newChild);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 3);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_ChildFunctions_insertIntoPiecewise_3)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <ci> newChild </ci>\n"
    "      <cn> 0 </cn>\n"
    "    </piece>\n"
    "    <otherwise>\n"
    "      <apply>\n"
    "        <eq/>\n"
    "        <ci> x </ci>\n"
    "        <cn> 0 </cn>\n"
    "      </apply>\n"
    "    </otherwise>\n"
    "  </piecewise>\n"
  );

  const char* original = wrapMathML
  (
    "<piecewise>"
    "  <piece>"
    "    <cn>0</cn>"
    "    <apply> <eq/> <ci>x</ci> <cn>0</cn> </apply>"
    "  </piece>"
    "</piecewise>"
  );

  N = readMathMLFromString(original);

  /* old behaviour - we should have 2 children */
  fail_unless(N->getNumChildren() == 2);

  ASTNode * newChild = new ASTNode(AST_NAME);
  newChild->setName("newChild");
  
  /* check we fail nicely if we try to access more children */
  int i = N->prependChild(newChild);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(N->getNumChildren() == 3);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


Suite *
create_suite_TestChildFunctions ()
{
  Suite *suite = suite_create("TestChildFunctions");
  TCase *tcase = tcase_create("TestChildFunctions");

  tcase_add_checked_fixture(tcase, TestChildFunctions_setup, 
                                   TestChildFunctions_teardown);

  tcase_add_test( tcase, test_ChildFunctions_addToPiecewise_1  );
  tcase_add_test( tcase, test_ChildFunctions_addToPiecewise_2  );
  tcase_add_test( tcase, test_ChildFunctions_addToPiecewise_3  );
  tcase_add_test( tcase, test_ChildFunctions_getChild             );
  tcase_add_test( tcase, test_ChildFunctions_getChildFromPiecewise_1  );
  tcase_add_test( tcase, test_ChildFunctions_getChildFromPiecewise_2  );
  tcase_add_test( tcase, test_ChildFunctions_remove               );
  tcase_add_test( tcase, test_ChildFunctions_removeFromPiecewise_1  );
  tcase_add_test( tcase, test_ChildFunctions_removeFromPiecewise_2  );
  tcase_add_test( tcase, test_ChildFunctions_removeFromPiecewise_3  );
  tcase_add_test( tcase, test_ChildFunctions_replace               );
  tcase_add_test( tcase, test_ChildFunctions_insert               );
  tcase_add_test( tcase, test_ChildFunctions_insertIntoPiecewise_1               );
  tcase_add_test( tcase, test_ChildFunctions_insertIntoPiecewise_2               );
  tcase_add_test( tcase, test_ChildFunctions_insertIntoPiecewise_3               );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
