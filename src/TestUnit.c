/**
 * Filename    : TestUnit.c
 * Description : Unit unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-22
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
#include "sbml/Unit.h"


static Unit_t *U;


void
UnitTest_setup (void)
{
  U = Unit_create();

  if (U == NULL)
  {
    fail("Unit_create() returned a NULL pointer.");
  }
}


void
UnitTest_teardown (void)
{
  Unit_free(U);
}


START_TEST (test_Unit_create)
{
  fail_unless( SBase_getTypeCode  (U) == SBML_UNIT, NULL );
  fail_unless( SBase_getMetaId    (U) == NULL, NULL );
  fail_unless( SBase_getNotes     (U) == NULL, NULL );
  fail_unless( SBase_getAnnotation(U) == NULL, NULL );

  fail_unless( Unit_getKind      (U) == UNIT_KIND_INVALID, NULL );
  fail_unless( Unit_getExponent  (U) == 1  , NULL );
  fail_unless( Unit_getScale     (U) == 0  , NULL );
  fail_unless( Unit_getMultiplier(U) == 1.0, NULL );
  fail_unless( Unit_getOffset    (U) == 0.0, NULL );

  fail_unless( !Unit_isSetKind(U), NULL );
}
END_TEST


START_TEST (test_Unit_createWith)
{
  Unit_t *u = Unit_createWith(UNIT_KIND_SECOND, -2, 1);


  fail_unless( SBase_getTypeCode  (u) == SBML_UNIT, NULL );
  fail_unless( SBase_getMetaId    (u) == NULL, NULL );
  fail_unless( SBase_getNotes     (u) == NULL, NULL );
  fail_unless( SBase_getAnnotation(u) == NULL, NULL );

  fail_unless( Unit_getKind      (u) == UNIT_KIND_SECOND, NULL );
  fail_unless( Unit_getExponent  (u) == -2  , NULL );
  fail_unless( Unit_getScale     (u) ==  1  , NULL );
  fail_unless( Unit_getMultiplier(u) ==  1.0, NULL );
  fail_unless( Unit_getOffset    (u) ==  0.0, NULL );

  fail_unless( Unit_isSetKind(u), NULL );

  Unit_free(u);
}
END_TEST


START_TEST (test_Unit_free_NULL)
{
  Unit_free(NULL);
}
END_TEST


Suite *
create_suite_Unit (void)
{
  Suite *suite = suite_create("Unit");
  TCase *tcase = tcase_create("Unit");


  tcase_add_checked_fixture( tcase, UnitTest_setup, UnitTest_teardown );

  tcase_add_test( tcase, test_Unit_create     );
  tcase_add_test( tcase, test_Unit_createWith );
  tcase_add_test( tcase, test_Unit_free_NULL  );

  suite_add_tcase(suite, tcase);

  return suite;
}
