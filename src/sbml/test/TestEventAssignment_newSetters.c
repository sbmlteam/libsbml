/**
 * \file    TestEventAssignment_newSetters.c
 * \brief   EventAssignment unit tests for new set function API
 * \author  Sarah Keating
 *
 * $Variable: TestEventAssignment_newSetters.c 8311 2008-10-31 01:54:18Z sarahkeating $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/sbml/test/TestEventAssignment_newSetters.c $
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
#include <sbml/EventAssignment.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/SBMLDocument.h>

#include <check.h>



static EventAssignment_t *E;


void
EventAssignmentTest1_setup (void)
{
  E = EventAssignment_create(2, 4);

  if (E == NULL)
  {
    fail("EventAssignment_create() returned a NULL pointer.");
  }
}


void
EventAssignmentTest1_teardown (void)
{
  EventAssignment_free(E);
}


START_TEST (test_EventAssignment_setVariable1)
{
  char *id = "1e1";
  int i = EventAssignment_setVariable(E, id);

  fail_unless( i == LIBSBML_INVALID_ATTRIBUTE_VALUE );
  fail_unless( !EventAssignment_isSetVariable(E) );
}
END_TEST


START_TEST (test_EventAssignment_setVariable2)
{
  char *id = "e1";
  int i = EventAssignment_setVariable(E, id);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( !strcmp(EventAssignment_getVariable(E), id) );
  fail_unless( EventAssignment_isSetVariable(E) );

  i = EventAssignment_setVariable(E, NULL);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( !EventAssignment_isSetVariable(E) );
}
END_TEST


START_TEST (test_EventAssignment_setMath1)
{
  ASTNode_t *math = SBML_parseFormula("2 * k");

  int i = EventAssignment_setMath(E, math);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( EventAssignment_getMath(E) != math );
  fail_unless( EventAssignment_isSetMath(E) );

  i = EventAssignment_setMath(E, NULL);
  
  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( EventAssignment_getMath(E) == NULL );
  fail_unless( !EventAssignment_isSetMath(E) );

  ASTNode_free(math);
}
END_TEST


START_TEST (test_EventAssignment_setMath2)
{
  ASTNode_t *math = ASTNode_createWithType(AST_TIMES);

  int i = EventAssignment_setMath(E, math);

  fail_unless( i == LIBSBML_INVALID_OBJECT);
  fail_unless( !EventAssignment_isSetMath(E) );

  ASTNode_free(math);
}
END_TEST


Suite *
create_suite_EventAssignment_newSetters (void)
{
  Suite *suite = suite_create("EventAssignment_newSetters");
  TCase *tcase = tcase_create("EventAssignment_newSetters");


  tcase_add_checked_fixture( tcase,
                             EventAssignmentTest1_setup,
                             EventAssignmentTest1_teardown );

  tcase_add_test( tcase, test_EventAssignment_setVariable1        );
  tcase_add_test( tcase, test_EventAssignment_setVariable2        );
  tcase_add_test( tcase, test_EventAssignment_setMath1      );
  tcase_add_test( tcase, test_EventAssignment_setMath2      );

  suite_add_tcase(suite, tcase);

  return suite;
}
