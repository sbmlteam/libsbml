/**
 * \file    TestEventAssignment.c
 * \brief   SBML EventAssignment unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <check.h>

#include "common.h"
#include "FormulaParser.h"

#include "SBase.h"
#include "EventAssignment.h"


static EventAssignment_t *EA;


void
EventAssignmentTest_setup (void)
{
  EA = EventAssignment_create();

  if (EA == NULL)
  {
    fail("EventAssignment_create() returned a NULL pointer.");
  }
}


void
EventAssignmentTest_teardown (void)
{
  EventAssignment_free(EA);
}


START_TEST (test_EventAssignment_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) EA) == SBML_EVENT_ASSIGNMENT );
  fail_unless( SBase_getMetaId    ((SBase_t *) EA) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) EA) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) EA) == NULL );

  fail_unless( EventAssignment_getVariable(EA) == NULL );
  fail_unless( EventAssignment_getMath    (EA) == NULL );
}
END_TEST


START_TEST (test_EventAssignment_createWith)
{
  ASTNode_t         *math = SBML_parseFormula("0");
  EventAssignment_t *ea   = EventAssignment_createWith("k", math);


  fail_unless( SBase_getTypeCode  ((SBase_t *) ea) == SBML_EVENT_ASSIGNMENT );
  fail_unless( SBase_getMetaId    ((SBase_t *) ea) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) ea) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) ea) == NULL );

  fail_unless( EventAssignment_getMath(ea) == math );
  fail_unless( EventAssignment_isSetMath(ea) );

  fail_unless( !strcmp(EventAssignment_getVariable(ea), "k") );
  fail_unless( EventAssignment_isSetVariable(ea) );

  EventAssignment_free(ea);
}
END_TEST


START_TEST (test_EventAssignment_free_NULL)
{
  EventAssignment_free(NULL);
}
END_TEST


START_TEST (test_EventAssignment_setVariable)
{
  char *variable = "k2";


  EventAssignment_setVariable(EA, variable);

  fail_unless( !strcmp(EventAssignment_getVariable(EA), variable) );
  fail_unless( EventAssignment_isSetVariable(EA) );

  if (EventAssignment_getVariable(EA) == variable)
  {
    fail("EventAssignment_setVariable(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  EventAssignment_setVariable(EA, EventAssignment_getVariable(EA));
  fail_unless( !strcmp(EventAssignment_getVariable(EA), variable) );

  EventAssignment_setVariable(EA, NULL);
  fail_unless( !EventAssignment_isSetVariable(EA) );

  if (EventAssignment_getVariable(EA) != NULL)
  {
    fail("EventAssignment_setVariable(EA, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_EventAssignment_setMath)
{
  ASTNode_t *math = SBML_parseFormula("2 * k");


  EventAssignment_setMath(EA, math);

  fail_unless( EventAssignment_getMath(EA) == math );
  fail_unless( EventAssignment_isSetMath(EA) );

  /* Reflexive case (pathological) */
  EventAssignment_setMath(EA, (ASTNode_t *) EventAssignment_getMath(EA));
  fail_unless( EventAssignment_getMath(EA) == math );

  EventAssignment_setMath(EA, NULL);
  fail_unless( !EventAssignment_isSetMath(EA) );

  if (EventAssignment_getMath(EA) != NULL)
  {
    fail("EventAssignment_setMath(EA, NULL) did not clear ASTNode.");
  }
}
END_TEST


Suite *
create_suite_EventAssignment (void)
{
  Suite *suite = suite_create("EventAssignment");
  TCase *tcase = tcase_create("EventAssignment");


  tcase_add_checked_fixture( tcase,
                             EventAssignmentTest_setup,
                             EventAssignmentTest_teardown );

  tcase_add_test( tcase, test_EventAssignment_create      );
  tcase_add_test( tcase, test_EventAssignment_createWith  );
  tcase_add_test( tcase, test_EventAssignment_free_NULL   );
  tcase_add_test( tcase, test_EventAssignment_setVariable );
  tcase_add_test( tcase, test_EventAssignment_setMath     );

  suite_add_tcase(suite, tcase);

  return suite;
}
