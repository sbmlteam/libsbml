/**
 * Filename    : TestSBase.c
 * Description : SBase unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-18
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
#include "sbml/SBase.h"


static SBase_t *S;


void
SBaseTest_setup (void)
{
  S = (SBase_t *) safe_calloc(1, sizeof(SBase_t));

  if (S == NULL)
  {
    fail("safe_calloc(1, sizeof(SBase_t)) returned a NULL pointer.");
  }

  /**
   * SBase_init() requires an SBMLTypeCode_t as its second argument.  Which
   * one doesn't really matter for the purposes of these tests, so one was
   * picked at random.
   */
  SBase_init(S, SBML_MODEL);
}


void
SBaseTest_teardown (void)
{
  SBase_clear(S);
  safe_free(S);
}


START_TEST (test_SBase_init)
{
  fail_unless(S->typecode   == SBML_MODEL, NULL);
  fail_unless(S->metaid     == NULL, NULL);
  fail_unless(S->notes      == NULL, NULL);
  fail_unless(S->annotation == NULL, NULL);

  fail_unless( !SBase_isSetMetaId    (S), NULL );
  fail_unless( !SBase_isSetNotes     (S), NULL );
  fail_unless( !SBase_isSetAnnotation(S), NULL );
}
END_TEST


START_TEST (test_SBase_clear_NULL)
{
  SBase_clear(NULL);
}
END_TEST


START_TEST (test_SBase_setMetaId)
{
  char *metaid = "x12345";


  SBase_setMetaId(S, metaid);

  fail_unless( !strcmp(S->metaid, metaid), NULL );
  fail_unless( SBase_isSetMetaId(S)      , NULL );

  if (S->metaid == metaid)
  {
    fail("SBase_setMetaId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setMetaId(S, S->metaid);
  fail_unless( !strcmp(S->metaid, metaid), NULL );

  SBase_setMetaId(S, NULL);
  fail_unless( !SBase_isSetMetaId(S), NULL );

  if (S->metaid != NULL)
  {
    fail("SBase_setMetaId(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SBase_setNotes)
{
  char *notes = "This is a test note.";


  SBase_setNotes(S, notes);

  fail_unless( !strcmp(S->notes, notes), NULL );
  fail_unless( SBase_isSetNotes(S)     , NULL );

  if (S->notes == notes)
  {
    fail("SBase_setNotes(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setNotes(S, S->notes);
  fail_unless( !strcmp(S->notes, notes), NULL );

  SBase_setNotes(S, NULL);
  fail_unless( !SBase_isSetNotes(S), NULL );

  if (S->notes != NULL)
  {
    fail("SBase_setNotes(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SBase_setAnnotation)
{
  char *annotation = "This is a test annotation.";


  SBase_setAnnotation(S, annotation);

  fail_unless( !strcmp(S->annotation, annotation), NULL );
  fail_unless( SBase_isSetAnnotation(S), NULL );

  if (S->annotation == annotation)
  {
    fail("SBase_setAnnotation(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setAnnotation(S, S->annotation);
  fail_unless( !strcmp(S->annotation, annotation), NULL );

  SBase_setAnnotation(S, NULL);
  fail_unless( !SBase_isSetAnnotation(S), NULL );

  if (S->annotation != NULL)
  {
    fail("SBase_setAnnotation(S, NULL) did not clear string.");
  }
}
END_TEST


Suite *
create_suite_SBase (void)
{
  Suite *suite = suite_create("SBase");
  TCase *tcase = tcase_create("SBase");


  tcase_add_checked_fixture(tcase, SBaseTest_setup, SBaseTest_teardown);

  tcase_add_test(tcase, test_SBase_init          );
  tcase_add_test(tcase, test_SBase_clear_NULL    );
  tcase_add_test(tcase, test_SBase_setMetaId     );
  tcase_add_test(tcase, test_SBase_setNotes      );
  tcase_add_test(tcase, test_SBase_setAnnotation );

  suite_add_tcase(suite, tcase);

  return suite;
}
