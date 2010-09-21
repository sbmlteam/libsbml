/**
 * \file    TestL3Trigger.c
 * \brief   SBML Trigger unit tests
 * \author  Sarah Keating
 *
 * $Id: TestTrigger.c 10129 2009-08-28 12:23:22Z sarahkeating $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/sbml/test/TestTrigger.c $
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


#include <sbml/common/common.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/FormulaFormatter.h>

#include <sbml/SBase.h>
#include <sbml/Trigger.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/SBMLDocument.h>

#include <check.h>


static Trigger_t *T;


void
L3TriggerTest_setup (void)
{
  T = Trigger_create(3, 1);

  if (T == NULL)
  {
    fail("Trigger_create() returned a NULL pointer.");
  }
}


void
L3TriggerTest_teardown (void)
{
  Trigger_free(T);
}


START_TEST (test_L3Trigger_create)
{
  fail_unless( SBase_getTypeCode((SBase_t *) T) == SBML_TRIGGER );
  fail_unless( SBase_getMetaId    ((SBase_t *) T) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) T) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) T) == NULL );

  fail_unless( Trigger_getMath(T) == NULL );
  fail_unless( Trigger_getInitialValue(T) == 1 );
  fail_unless( Trigger_getPersistent(T) == 1 );
  fail_unless( Trigger_isSetInitialValue(T) == 0 );
  fail_unless( Trigger_isSetPersistent(T) == 0 );

}
END_TEST


START_TEST (test_L3Trigger_setInitialValue)
{
  int i = Trigger_setInitialValue(T, 0);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
 
  fail_unless( Trigger_getInitialValue(T) == 0 );
  fail_unless( Trigger_isSetInitialValue(T) == 1 );

  i = Trigger_setInitialValue(T, 1);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
 
  fail_unless( Trigger_getInitialValue(T) == 1 );
  fail_unless( Trigger_isSetInitialValue(T) == 1 );

}
END_TEST


START_TEST (test_L3Trigger_setInitialValue1)
{
  Trigger_t *t = Trigger_create(2, 4);

  int i = Trigger_setInitialValue(t, 0);

  fail_unless( i == LIBSBML_UNEXPECTED_ATTRIBUTE );
 
  fail_unless( Trigger_getInitialValue(T) == 1 );
  fail_unless( Trigger_isSetInitialValue(T) == 0 );

  Trigger_free(t);
}
END_TEST


START_TEST (test_L3Trigger_setPersistent)
{
  int i = Trigger_setPersistent(T, 0);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
 
  fail_unless( Trigger_getPersistent(T) == 0 );
  fail_unless( Trigger_isSetPersistent(T) == 1 );

  i = Trigger_setPersistent(T, 1);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
 
  fail_unless( Trigger_getPersistent(T) == 1 );
  fail_unless( Trigger_isSetPersistent(T) == 1 );

}
END_TEST


START_TEST (test_L3Trigger_setPersistent1)
{
  Trigger_t *t = Trigger_create(2, 4);

  int i = Trigger_setPersistent(t, 0);

  fail_unless( i == LIBSBML_UNEXPECTED_ATTRIBUTE );
 
  fail_unless( Trigger_getPersistent(T) == 1 );
  fail_unless( Trigger_isSetPersistent(T) == 0 );

  Trigger_free(t);
}
END_TEST


Suite *
create_suite_L3Trigger (void)
{
  Suite *suite = suite_create("L3Trigger");
  TCase *tcase = tcase_create("L3Trigger");


  tcase_add_checked_fixture( tcase,
                             L3TriggerTest_setup,
                             L3TriggerTest_teardown );

  tcase_add_test( tcase, test_L3Trigger_create       );
  tcase_add_test( tcase, test_L3Trigger_setInitialValue      );
  tcase_add_test( tcase, test_L3Trigger_setInitialValue1     );
  tcase_add_test( tcase, test_L3Trigger_setPersistent      );
  tcase_add_test( tcase, test_L3Trigger_setPersistent1     );

  suite_add_tcase(suite, tcase);

  return suite;
}
