/**
 * \file    TestInitialAssignment_newSetters.c
 * \brief   InitialAssignment unit tests for new set function API
 * \author  Sarah Keating
 *
 * $Symbol: TestInitialAssignment_newSetters.c 8311 2008-10-31 01:54:18Z sarahkeating $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/sbml/test/TestInitialAssignment_newSetters.c $
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


#include <sbml/common/common.h>
#include <sbml/math/FormulaParser.h>

#include <sbml/SBase.h>
#include <sbml/InitialAssignment.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/SBMLDocument.h>

#include <check.h>



static InitialAssignment_t *E;


void
InitialAssignmentTest1_setup (void)
{
  E = InitialAssignment_create(2, 4);

  if (E == NULL)
  {
    fail("InitialAssignment_create() returned a NULL pointer.");
  }
}


void
InitialAssignmentTest1_teardown (void)
{
  InitialAssignment_free(E);
}


START_TEST (test_InitialAssignment_setSymbol1)
{
  char *id = "1e1";
  int i = InitialAssignment_setSymbol(E, id);

  fail_unless( i == LIBSBML_INVALID_ATTRIBUTE_VALUE );
  fail_unless( !InitialAssignment_isSetSymbol(E) );
}
END_TEST


START_TEST (test_InitialAssignment_setSymbol2)
{
  char *id = "e1";
  int i = InitialAssignment_setSymbol(E, id);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( !strcmp(InitialAssignment_getSymbol(E), id) );
  fail_unless( InitialAssignment_isSetSymbol(E) );

  i = InitialAssignment_setSymbol(E, NULL);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( !InitialAssignment_isSetSymbol(E) );
}
END_TEST


START_TEST (test_InitialAssignment_setMath1)
{
  ASTNode_t *math = SBML_parseFormula("2 * k");

  int i = InitialAssignment_setMath(E, math);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( InitialAssignment_getMath(E) != math );
  fail_unless( InitialAssignment_isSetMath(E) );

  i = InitialAssignment_setMath(E, NULL);
  
  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( InitialAssignment_getMath(E) == NULL );
  fail_unless( !InitialAssignment_isSetMath(E) );

  ASTNode_free(math);
}
END_TEST


START_TEST (test_InitialAssignment_setMath2)
{
  ASTNode_t *math = ASTNode_createWithType(AST_TIMES);

  int i = InitialAssignment_setMath(E, math);

  fail_unless( i == LIBSBML_INVALID_OBJECT);
  fail_unless( !InitialAssignment_isSetMath(E) );

  ASTNode_free(math);
}
END_TEST


Suite *
create_suite_InitialAssignment_newSetters (void)
{
  Suite *suite = suite_create("InitialAssignment_newSetters");
  TCase *tcase = tcase_create("InitialAssignment_newSetters");


  tcase_add_checked_fixture( tcase,
                             InitialAssignmentTest1_setup,
                             InitialAssignmentTest1_teardown );

  tcase_add_test( tcase, test_InitialAssignment_setSymbol1        );
  tcase_add_test( tcase, test_InitialAssignment_setSymbol2        );
  tcase_add_test( tcase, test_InitialAssignment_setMath1      );
  tcase_add_test( tcase, test_InitialAssignment_setMath2      );

  suite_add_tcase(suite, tcase);

  return suite;
}
