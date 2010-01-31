/**
 * @file    TestSBMLTransforms.cpp
 * @brief   SBMLTransforms unit tests
 * @author  Sarah Keating
 *
 * $Id: TestSBMLTransforms.cpp 10144 2009-08-29 11:26:19Z sarahkeating $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/sbml/test/TestSBMLTransforms.cpp $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
*/


#include <sbml/common/common.h>
#include <sbml/common/extern.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <sbml/SBMLTransforms.h>

#include <check.h>

#include <iostream>

LIBSBML_CPP_NAMESPACE_USE


BEGIN_C_DECLS

bool
equalDouble (double a, double b)
{
  return (fabs(a-b) < sqrt(__DBL_EPSILON__));
}

extern char *TestDataDirectory;

START_TEST (test_SBMLTransforms_replaceFD)
{
  SBMLReader        reader;
  SBMLDocument*     d;
  Model*            m;
  const ASTNode*          ast;
  FunctionDefinition*     fd;
  ListOfFunctionDefinitions * lofd;

  std::string filename(TestDataDirectory);
  filename += "multiple-functions.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"multiple-functions.xml\") returned a NULL pointer.");
  }

  m = d->getModel();

  fail_unless( m->getNumFunctionDefinitions() == 2 );

  /* one function definition */
  ast = m->getReaction(2)->getKineticLaw()->getMath();

  fail_unless (!strcmp(SBML_formulaToString(ast), "f(S1, p) * compartmentOne / t"), NULL);

  fd = m->getFunctionDefinition(0);
  SBMLTransforms::replaceFD(const_cast<ASTNode *>(ast), fd);

  fail_unless (!strcmp(SBML_formulaToString(ast), "S1 * p * compartmentOne / t"), NULL);

  /* one function definition - nested */
  ast = m->getReaction(1)->getKineticLaw()->getMath();

  fail_unless (!strcmp(SBML_formulaToString(ast), "f(f(S1, p), compartmentOne) / t"), NULL);

  SBMLTransforms::replaceFD(const_cast<ASTNode *>(ast), fd);

  fail_unless (!strcmp(SBML_formulaToString(ast), "S1 * p * compartmentOne / t"), NULL);

  /* two function definitions - nested */
  ast = m->getReaction(0)->getKineticLaw()->getMath();

  fail_unless (!strcmp(SBML_formulaToString(ast), "g(f(S1, p), compartmentOne) / t"), NULL);

  SBMLTransforms::replaceFD(const_cast<ASTNode *>(ast), fd);

  fail_unless (!strcmp(SBML_formulaToString(ast), "g(S1 * p, compartmentOne) / t"), NULL);

  fd = m->getFunctionDefinition(1);

  SBMLTransforms::replaceFD(const_cast<ASTNode *>(ast), fd);

  fail_unless (!strcmp(SBML_formulaToString(ast), "f(S1 * p, compartmentOne) / t"), NULL);

  ast = m->getReaction(0)->getKineticLaw()->getMath();
  lofd = m->getListOfFunctionDefinitions();
  SBMLTransforms::replaceFD(const_cast<ASTNode *>(ast), lofd);

  fail_unless (!strcmp(SBML_formulaToString(ast), "S1 * p * compartmentOne / t"), NULL);

  d->expandFunctionDefinitions();

  fail_unless( d->getModel()->getNumFunctionDefinitions() == 0 );
  
  ast = d->getModel()->getReaction(0)->getKineticLaw()->getMath();
  
  fail_unless (!strcmp(SBML_formulaToString(ast), "S1 * p * compartmentOne / t"), NULL);
  
  ast = d->getModel()->getReaction(1)->getKineticLaw()->getMath();
  
  fail_unless (!strcmp(SBML_formulaToString(ast), "S1 * p * compartmentOne / t"), NULL);

  ast = d->getModel()->getReaction(2)->getKineticLaw()->getMath();

  fail_unless (!strcmp(SBML_formulaToString(ast), "S1 * p * compartmentOne / t"), NULL);
}
END_TEST


START_TEST(test_SBMLTransforms_evaluateAST)
{
  double temp;
  ASTNode * node = new ASTNode();
  node->setValue((int)(2));
  
  fail_unless(SBMLTransforms::evaluateASTNode(node) == 2);

  node->setValue((double) (3.2));

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 3.2));

  node->setValue((long)(1), (long)(4));

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.25));

  node->setValue((double) (4.234), (int) (2));

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 423.4));

  node->setType(AST_NAME_AVOGADRO);

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 6.02214179e23));

  node->setType(AST_NAME_TIME);

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node->setType(AST_NAME);

  fail_unless(isnan(SBMLTransforms::evaluateASTNode(node)));

  node->setType(AST_CONSTANT_E);

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), exp(1.0)));

  node->setType(AST_CONSTANT_FALSE);

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node->setType(AST_CONSTANT_PI);

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 4.0*atan(1.0)));

  node->setType(AST_CONSTANT_TRUE);

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("2.5 + 6.1");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 8.6));

  node = SBML_parseFormula("-4.3");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), -4.3));

  node = SBML_parseFormula("9.2-4.3");

  temp = 9.2-4.3;
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("2*3");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 6));

  node = SBML_parseFormula("1/5");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.2));

  node = SBML_parseFormula("pow(2, 3)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 8));

  node = SBML_parseFormula("3^3");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 27));

  node = SBML_parseFormula("abs(-9.456)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 9.456));

  node = SBML_parseFormula("ceil(9.456)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 10));

  node = SBML_parseFormula("exp(2.0)");
  
  temp = exp(2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("floor(2.04567)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 2));

  node = SBML_parseFormula("ln(2.0)");

  temp = log(2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("log10(100.0)");

  temp = log10(100.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("sin(2.0)");

  temp = sin(2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("cos(4.1)");

  temp = cos(4.1);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("tan(0.345)");

  temp = tan(0.345);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arcsin(0.456)");

  temp = asin(0.456);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arccos(0.41)");

  temp = acos(0.41);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arctan(0.345)");

  temp = atan(0.345);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("sinh(2.0)");

  temp = sinh(2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("cosh(4.1)");

  temp = cosh(4.1);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("tanh(0.345)");

  temp = tanh(0.345);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("and(1, 0)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node = SBML_parseFormula("or(1, 0)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("not(1)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node = SBML_parseFormula("xor(1, 0)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("xor(1, 1)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node = SBML_parseFormula("eq(1, 2)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node = SBML_parseFormula("eq(1, 1)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("geq(2,1)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("geq(2,4)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node = SBML_parseFormula("geq(2,2)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("gt(2,1)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("gt(2,4)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node = SBML_parseFormula("leq(2,1)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node = SBML_parseFormula("leq(2,4)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("leq(2,2)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("lt(2,1)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node = SBML_parseFormula("lt(2,4)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));
  
  node = SBML_parseFormula("neq(2,2)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 0.0));

  node = SBML_parseFormula("neq(3,2)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 1.0));

  node = SBML_parseFormula("cot(2.0)");

  temp = 1.0/tan(2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("csc(4.1)");

  temp = 1.0/sin(4.1);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("sec(0.345)");

  temp = 1.0/cos(0.345);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("coth(2.0)");

  temp = cosh(2.0)/sinh(2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));
  
  node = SBML_parseFormula("sech(2.0)");

  temp = 1.0/cosh(2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("csch(2.0)");

  temp = 1.0/sinh(2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arccot(2.0)");

  temp = atan(1/2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arccsc(2.0)");

  temp = asin(1/2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arcsec(2.0)");

  temp = acos(1/2.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arccosh(2.0)");

  temp = log(2.0+pow(3.0, 0.5));
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arccoth(2.0)");

  temp = 0.5 * log(3.0);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arcsech(0.2)");

  temp = log(2*pow(6, 0.5)+5);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));
  
  node = SBML_parseFormula("arccsch(0.2)");

  /* temp = log(5 +pow(26, 0.5));
   * only matches to 15 dp and therefore fails !!
   */
  temp = 2.312438341272753;
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arcsinh(3.0)");

  temp = log(3.0 +pow(10.0, 0.5));
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node = SBML_parseFormula("arctanh(0.2)");

  temp = 0.5 * log(1.5);
  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), temp));

  node->setType(AST_FUNCTION_DELAY);

  fail_unless(isnan(SBMLTransforms::evaluateASTNode(node)));

  node = SBML_parseFormula("factorial(3)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 6));
  
  node->setType(AST_FUNCTION_PIECEWISE);

  fail_unless(isnan(SBMLTransforms::evaluateASTNode(node)));

  node = SBML_parseFormula("root(2, 4)");

  fail_unless(equalDouble(SBMLTransforms::evaluateASTNode(node), 2));
  
}
END_TEST


START_TEST (test_SBMLTransforms_replaceIA)
{
  SBMLReader        reader;
  SBMLDocument*     d;
  Model*            m;
  const ASTNode*          ast;
  FunctionDefinition*     fd;
  ListOfFunctionDefinitions * lofd;

  std::string filename(TestDataDirectory);
  filename += "initialAssignments.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"initialAssignments.xml\") returned a NULL pointer.");
  }

  m = d->getModel();

  fail_unless( m->getNumInitialAssignments() == 2 );
  fail_unless( !(m->getCompartment(0)->isSetSize()));
  fail_unless( m->getParameter(1)->getValue() == 2);

  d->expandInitialAssignments();

  fail_unless( d->getModel()->getNumInitialAssignments() == 0 );
  fail_unless( d->getModel()->getCompartment(0)->isSetSize());
  fail_unless( d->getModel()->getCompartment(0)->getSize() == 25.0);
  fail_unless( m->getParameter(1)->getValue() == 50);
  
}
END_TEST


START_TEST (test_SBMLTransforms_replaceIA_species)
{
  SBMLReader        reader;
  SBMLDocument*     d;
  Model*            m;
  const ASTNode*          ast;
  FunctionDefinition*     fd;
  ListOfFunctionDefinitions * lofd;

  std::string filename(TestDataDirectory);
  filename += "initialAssignments_species.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"initialAssignments_species.xml\") returned a NULL pointer.");
  }

  m = d->getModel();

  fail_unless( m->getNumInitialAssignments() == 3 );
  fail_unless( m->getParameter(1)->getValue() == 0.75);
  fail_unless( !(m->getParameter(2)->isSetValue()));
  fail_unless( m->getSpecies(2)->isSetInitialAmount());
  fail_unless( m->getSpecies(2)->getInitialAmount() == 2);

  d->expandInitialAssignments();

  fail_unless( d->getModel()->getNumInitialAssignments() == 0 );
  fail_unless( m->getParameter(1)->getValue() == 3);
  fail_unless( m->getParameter(2)->isSetValue());
  fail_unless( m->getParameter(2)->getValue() == 0.75);
  fail_unless( !(m->getSpecies(2)->isSetInitialAmount()));
  fail_unless( m->getSpecies(2)->getInitialConcentration() == 2);
  
}
END_TEST


Suite *
create_suite_SBMLTransforms (void)
{
  Suite *suite = suite_create("SBMLTransforms");
  TCase *tcase = tcase_create("SBMLTransforms");


  tcase_add_test(tcase, test_SBMLTransforms_replaceFD);
  tcase_add_test(tcase, test_SBMLTransforms_evaluateAST);
  tcase_add_test(tcase, test_SBMLTransforms_replaceIA);
  tcase_add_test(tcase, test_SBMLTransforms_replaceIA_species);


  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
