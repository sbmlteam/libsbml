/**
 * Filename    : TestEvent.c
 * Description : SBML Event unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-03
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <check.h>

#include "sbml/common.h"
#include "sbml/FormulaParser.h"
#include "sbml/Event.h"


static Event_t *E;


void
EventTest_setup (void)
{
  E = Event_create();

  if (E == NULL)
  {
    fail("Event_create() returned a NULL pointer.");
  }
}


void
EventTest_teardown (void)
{
  Event_free(E);
}


START_TEST (test_Event_create)
{
  fail_unless( SBase_getTypeCode  (E) == SBML_EVENT, NULL );
  fail_unless( SBase_getMetaId    (E) == NULL, NULL );
  fail_unless( SBase_getNotes     (E) == NULL, NULL );
  fail_unless( SBase_getAnnotation(E) == NULL, NULL );

  fail_unless( Event_getId        (E) == NULL, NULL );
  fail_unless( Event_getName      (E) == NULL, NULL );
  fail_unless( Event_getTrigger   (E) == NULL, NULL );
  fail_unless( Event_getDelay     (E) == NULL, NULL );
  fail_unless( Event_getTimeUnits (E) == NULL, NULL );

  fail_unless( Event_getNumEventAssignments(E) == 0, NULL );
}
END_TEST


START_TEST (test_Event_createWith)
{
  ASTNode_t *trigger = SBML_parseFormula("leq(P1, t)");
  Event_t   *e       = Event_createWith("e1", trigger);


  fail_unless( SBase_getTypeCode  (e) == SBML_EVENT, NULL );
  fail_unless( SBase_getMetaId    (e) == NULL, NULL );
  fail_unless( SBase_getNotes     (e) == NULL, NULL );
  fail_unless( SBase_getAnnotation(e) == NULL, NULL );

  fail_unless( Event_getName      (e) == NULL, NULL );
  fail_unless( Event_getDelay     (e) == NULL, NULL );
  fail_unless( Event_getTimeUnits (e) == NULL, NULL );

  fail_unless( Event_getNumEventAssignments(e) == 0, NULL );

  fail_unless( Event_getTrigger(e) == trigger, NULL );
  fail_unless( Event_isSetTrigger(e), NULL );

  fail_unless( !strcmp(Event_getId(e), "e1"), NULL );
  fail_unless( Event_isSetId(e), NULL );

  Event_free(e);
}
END_TEST


START_TEST (test_Event_free_NULL)
{
  Event_free(NULL);
}
END_TEST


START_TEST (test_Event_setId)
{
  char *id = "e1";


  Event_setId(E, id);

  fail_unless( !strcmp(Event_getId(E), id), NULL );
  fail_unless( Event_isSetId(E), NULL );

  if (Event_getId(E) == id)
  {
    fail("Event_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Event_setId(E, Event_getId(E));
  fail_unless( !strcmp(Event_getId(E), id), NULL );

  Event_setId(E, NULL);
  fail_unless( !Event_isSetId(E), NULL );

  if (Event_getId(E) != NULL)
  {
    fail("Event_setId(E, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Event_setName)
{
  char *name = "Set k2 to zero when P1 <= t";


  Event_setName(E, name);

  fail_unless( !strcmp(Event_getName(E), name), NULL );
  fail_unless( Event_isSetName(E), NULL );

  if (Event_getName(E) == name)
  {
    fail("Event_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Event_setName(E, Event_getName(E));
  fail_unless( !strcmp(Event_getName(E), name), NULL );

  Event_setName(E, NULL);
  fail_unless( !Event_isSetName(E), NULL );

  if (Event_getName(E) != NULL)
  {
    fail("Event_setName(E, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Event_setTrigger)
{
  ASTNode_t *trigger = SBML_parseFormula("leq(P1, t)");


  Event_setTrigger(E, trigger);

  fail_unless( Event_getTrigger(E) == trigger, NULL );
  fail_unless( Event_isSetTrigger(E), NULL );

  /* Reflexive case (pathological) */
  Event_setTrigger(E, (ASTNode_t *) Event_getTrigger(E));
  fail_unless( Event_getTrigger(E) == trigger, NULL );

  Event_setTrigger(E, NULL);
  fail_unless( !Event_isSetTrigger(E), NULL );

  if (Event_getTrigger(E) != NULL)
  {
    fail("Event_setTrigger(E, NULL) did not clear ASTNode.");
  }
}
END_TEST


START_TEST (test_Event_setDelay)
{
  ASTNode_t *delay = SBML_parseFormula("t + 1");


  Event_setDelay(E, delay);

  fail_unless( Event_getDelay(E) == delay  , NULL );
  fail_unless( Event_isSetDelay(E), NULL );

  /* Reflexive case (pathological) */
  Event_setDelay(E, (ASTNode_t *) Event_getDelay(E));
  fail_unless( Event_getDelay(E) == delay, NULL );

  Event_setDelay(E, NULL);
  fail_unless( !Event_isSetDelay(E), NULL );

  if (Event_getDelay(E) != NULL)
  {
    fail("Event_setDelay(E, NULL) did not clear ASTNode.");
  }
}
END_TEST


START_TEST (test_Event_setTimeUnits)
{
  char *units = "second";


  Event_setTimeUnits(E, units);

  fail_unless( !strcmp(Event_getTimeUnits(E), units), NULL );
  fail_unless( Event_isSetTimeUnits(E), NULL );

  if (Event_getTimeUnits(E) == units)
  {
    fail("Event_setTimeUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Event_setTimeUnits(E, Event_getTimeUnits(E));
  fail_unless( !strcmp(Event_getTimeUnits(E), units), NULL );

  Event_setTimeUnits(E, NULL);
  fail_unless( !Event_isSetTimeUnits(E), NULL );

  if (Event_getTimeUnits(E) != NULL)
  {
    fail("Event_setTimeUnits(E, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Event_full)
{
  ASTNode_t         *trigger = SBML_parseFormula("leq(P1, t)");
  ASTNode_t         *math    = SBML_parseFormula("0");
  Event_t           *e       = Event_createWith("e1", trigger);
  EventAssignment_t *ea      = EventAssignment_createWith("k", math);


  Event_setName(e, "Set k2 to zero when P1 <= t");
  Event_addEventAssignment(e, ea);

  fail_unless( Event_getNumEventAssignments(e) ==  1, NULL );
  fail_unless( Event_getEventAssignment(e, 0)  == ea, NULL );

  Event_free(e);
}
END_TEST


Suite *
create_suite_Event (void)
{
  Suite *suite = suite_create("Event");
  TCase *tcase = tcase_create("Event");


  tcase_add_checked_fixture( tcase,
                             EventTest_setup,
                             EventTest_teardown );

  tcase_add_test( tcase, test_Event_create       );
  tcase_add_test( tcase, test_Event_createWith   );
  tcase_add_test( tcase, test_Event_free_NULL    );
  tcase_add_test( tcase, test_Event_setId        );
  tcase_add_test( tcase, test_Event_setName      );
  tcase_add_test( tcase, test_Event_setTrigger   );
  tcase_add_test( tcase, test_Event_setDelay     );
  tcase_add_test( tcase, test_Event_setTimeUnits );
  tcase_add_test( tcase, test_Event_full         );

  suite_add_tcase(suite, tcase);

  return suite;
}
