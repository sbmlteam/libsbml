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
 * Copyright 2005-2009 California Institute of Technology.
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


Suite *
create_suite_SBMLTransforms (void)
{
  Suite *suite = suite_create("SBMLTransforms");
  TCase *tcase = tcase_create("SBMLTransforms");


  tcase_add_test(tcase, test_SBMLTransforms_replaceFD);


  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
