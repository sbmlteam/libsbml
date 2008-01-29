/**
 * @file    TestReadFromFile2.cpp
 * @brief   Tests for reading MathML from files into ASTNodes.
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/


#include <sbml/common/common.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <sbml/math/ASTNode.h>



#include <string>

#include <check.h>


BEGIN_C_DECLS


extern char *TestDataDirectory;


START_TEST (test_read_MathML_2)
{
  SBMLReader         reader;
  SBMLDocument*      d;
  Model*             m;
  FunctionDefinition* fd;
  InitialAssignment* ia;
  Rule*              r;
  KineticLaw*        kl;


  std::string filename(TestDataDirectory);
  filename += "mathML_2.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"mathML_2.xml\") returned a NULL pointer.");
  }

  m = d->getModel();
  fail_unless( m != NULL, NULL );

  // check that whole model has been read in
  fail_unless( m->getNumFunctionDefinitions() == 2, NULL);
  fail_unless( m->getNumInitialAssignments() == 1, NULL);
  fail_unless( m->getNumRules() == 2, NULL );
  fail_unless( m->getNumReactions() == 1, NULL );

  //<functionDefinition id="fd">
  //  <math xmlns="http://www.w3.org/1998/Math/MathML">
  //    <lambda>
  //      <apply/>
  //    </lambda>
  //  </math>
  //</functionDefinition>
  fd = m->getFunctionDefinition(0);
  const ASTNode *fd_math = fd->getMath();

  fail_unless (fd_math->getType() == AST_LAMBDA, NULL);
  fail_unless (fd_math->getNumChildren() == 1, NULL);
  fail_unless (!strcmp(SBML_formulaToString(fd_math), "lambda()"), NULL);

  ASTNode *child = fd_math->getRightChild();
  fail_unless (child->getType() == AST_UNKNOWN, NULL);
  fail_unless (child->getNumChildren() == 0, NULL);
  fail_unless (!strcmp(SBML_formulaToString(child), ""), NULL);

  //<functionDefinition id="fd1">
  //  <math xmlns="http://www.w3.org/1998/Math/MathML">
  //    <lambda>
  //      <bvar>
  //        <ci> x </ci>
  //      </bvar>
        //<piecewise>
        //  <piece>
        //    <ci> p </ci>
        //    <apply>
        //      <leq/>
        //      <ci> x </ci>
        //      <cn type="integer"> 4 </cn>
        //    </apply>
        //  </piece>
        //</piecewise>
  //    </lambda>
  //  </math>
  //</functionDefinition>
  fd = m->getFunctionDefinition(1);
  const ASTNode *fd1_math = fd->getMath();

  fail_unless (fd1_math->getType() == AST_LAMBDA, NULL);
  fail_unless (fd1_math->getNumChildren() == 2, NULL);
  fail_unless (!strcmp(SBML_formulaToString(fd1_math), 
                          "lambda(x, piecewise(p, leq(x, 4)))"), NULL);

  ASTNode *child1 = fd1_math->getRightChild();
  fail_unless (child1->getType() == AST_FUNCTION_PIECEWISE, NULL);
  fail_unless (child1->getNumChildren() == 2, NULL);
  fail_unless (!strcmp(SBML_formulaToString(child1), 
                                    "piecewise(p, leq(x, 4)))"), NULL);

  ASTNode *c1 = child1->getChild(0);
  fail_unless (c1->getType() == AST_NAME, NULL);
  fail_unless (c1->getNumChildren() == 0, NULL);
  fail_unless (!strcmp(SBML_formulaToString(c1), "p"), NULL);

  ASTNode *c2 = child1->getChild(0);
  fail_unless (c2->getType() == AST_RELATIONAL_LEQ, NULL);
  fail_unless (c2->getNumChildren() == 2, NULL);
  fail_unless (!strcmp(SBML_formulaToString(c2), "leq(x, 4)"), NULL);


  ////<initialAssignment symbol="p1">
  //  //<math xmlns="http://www.w3.org/1998/Math/MathML">
  //  //    <piecewise>
  //  //      <piece>
  //  //          <apply><minus/><ci> x </ci></apply>
  //  //          <apply><lt/><ci> x </ci> <cn> 0 </cn></apply>
  //  //      </piece>
  //  //      <piece>
  //  //          <cn> 0 </cn>
  //  //          <apply><eq/><ci> x </ci> <cn> 0 </cn></apply>
  //  //      </piece>
  //  //    </piecewise>
  //  //</math>
  ////</initialAssignment>
  //ia = m->getInitialAssignment(0);
  //const ASTNode *ia_math = ia->getMath();

  //fail_unless (ia_math->getType() == AST_PIECEWISE, NULL);
  //fail_unless (ia_math->getNumChildren() == 0, NULL);
  //fail_unless (!strcmp(SBML_formulaToString(ia_math), ""), NULL);

  ////<algebraicRule>
  ////  <math xmlns="http://www.w3.org/1998/Math/MathML">
  ////    <true/>
  ////  </math>
  ////</algebraicRule>
  //r = m->getRule(0);
  //const ASTNode *r_math = r->getMath();

  //fail_unless (r_math->getType() == AST_CONSTANT_TRUE, NULL);
  //fail_unless (r_math->getNumChildren() == 0, NULL);
  //fail_unless (!strcmp(SBML_formulaToString(r_math), "true"), NULL);

  ////<assignmentRule variable="p2">
  ////  <math xmlns="http://www.w3.org/1998/Math/MathML">
  ////      <infinity/>
  ////  </math>
  ////</assignmentRule>
  //r = m->getRule(1);
  //const ASTNode *r1_math = r->getMath();

  //fail_unless (r1_math->getType() == AST_REAL, NULL);
  //fail_unless (r1_math->getNumChildren() == 0, NULL);
  //fail_unless (!strcmp(SBML_formulaToString(r1_math), "INF"), NULL);

  ////<kineticLaw>
  ////  <math xmlns="http://www.w3.org/1998/Math/MathML">
  ////    <apply>
  ////      <cn> 4.5 </cn>
  ////    </apply>
  ////  </math>
  ////  <listOfParameters>
  ////    <parameter id="k" value="9" units="litre"/>
  ////  </listOfParameters>
  ////</kineticLaw>
  //kl = m->getReaction(0)->getKineticLaw();
  //const ASTNode *kl_math = kl->getMath();

  //fail_unless (kl_math->getType() == AST_REAL, NULL);
  //fail_unless (kl_math->getNumChildren() == 0, NULL);
  //fail_unless (!strcmp(SBML_formulaToString(kl_math), "4.5"), NULL);


  delete d;
}
END_TEST


Suite *
create_suite_TestReadFromFile2 (void)
{ 
  Suite *suite = suite_create("test-data/mathML_2.xml");
  TCase *tcase = tcase_create("test-data/mathML_2.xml");


  tcase_add_test(tcase, test_read_MathML_2);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
