/**
 * Filename    : TestUnitDefinition.c
 * Description : SBML UnitDefinition unit tests
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
#include "sbml/UnitDefinition.h"


static UnitDefinition_t *UD;


void
UnitDefinitionTest_setup (void)
{
  UD = UnitDefinition_create();

  if (UD == NULL)
  {
    fail("UnitDefinition_create() returned a NULL pointer.");
  }
}


void
UnitDefinitionTest_teardown (void)
{
  UnitDefinition_free(UD);
}


START_TEST (test_UnitDefinition_create)
{
  fail_unless( SBase_getTypeCode  (UD) == SBML_UNIT_DEFINITION, NULL );
  fail_unless( SBase_getMetaId    (UD) == NULL, NULL );
  fail_unless( SBase_getNotes     (UD) == NULL, NULL );
  fail_unless( SBase_getAnnotation(UD) == NULL, NULL );

  fail_unless( UnitDefinition_getId  (UD) == NULL, NULL );
  fail_unless( UnitDefinition_getName(UD) == NULL, NULL );

  fail_unless( !UnitDefinition_isSetId  (UD), NULL );
  fail_unless( !UnitDefinition_isSetName(UD), NULL );

  fail_unless(UnitDefinition_getNumUnits(UD) == 0, NULL);
}
END_TEST


START_TEST (test_UnitDefinition_createWith)
{
  UnitDefinition_t *ud = UnitDefinition_createWith("mmls");


  fail_unless( SBase_getTypeCode  (ud) == SBML_UNIT_DEFINITION, NULL );
  fail_unless( SBase_getMetaId    (ud) == NULL, NULL );
  fail_unless( SBase_getNotes     (ud) == NULL, NULL );
  fail_unless( SBase_getAnnotation(ud) == NULL, NULL );

  fail_unless( UnitDefinition_getName(ud) == NULL, NULL );

  fail_unless( !strcmp(UnitDefinition_getId(ud), "mmls"), NULL );
  fail_unless(UnitDefinition_isSetId(ud), NULL);

  fail_unless(UnitDefinition_getNumUnits(ud) == 0, NULL);

  UnitDefinition_free(ud);
}
END_TEST


START_TEST (test_UnitDefinition_createWithName)
{
  UnitDefinition_t *ud = UnitDefinition_createWithName("mmol liter^-1 sec^-1");


  fail_unless( SBase_getTypeCode  (ud) == SBML_UNIT_DEFINITION, NULL );
  fail_unless( SBase_getMetaId    (ud) == NULL, NULL );
  fail_unless( SBase_getNotes     (ud) == NULL, NULL );
  fail_unless( SBase_getAnnotation(ud) == NULL, NULL );

  fail_unless( UnitDefinition_getId(ud) == NULL, NULL );

  fail_unless( !strcmp(UnitDefinition_getName(ud), "mmol liter^-1 sec^-1"),
               NULL );

  fail_unless(UnitDefinition_isSetName(ud), NULL);

  fail_unless(UnitDefinition_getNumUnits(ud) == 0, NULL);

  UnitDefinition_free(ud);
}
END_TEST


START_TEST (test_UnitDefinition_free_NULL)
{
  UnitDefinition_free(NULL);
}
END_TEST


START_TEST (test_UnitDefinition_setId)
{
  char *id = "mmls";


  UnitDefinition_setId(UD, id);

  fail_unless( !strcmp(UnitDefinition_getId(UD), id), NULL );
  fail_unless( UnitDefinition_isSetId(UD), NULL );

  if (UnitDefinition_getId(UD) == id)
  {
    fail("UnitDefinition_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  UnitDefinition_setId(UD, UnitDefinition_getId(UD));
  fail_unless( !strcmp(UnitDefinition_getId(UD), id), NULL );

  UnitDefinition_setId(UD, NULL);
  fail_unless( !UnitDefinition_isSetId(UD), NULL );

  if (UnitDefinition_getId(UD) != NULL)
  {
    fail("UnitDefinition_setId(R, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_UnitDefinition_setName)
{
  char *name = "mmol liter^-1 sec^-1";


  UnitDefinition_setName(UD, name);

  fail_unless( !strcmp(UnitDefinition_getName(UD), name), NULL );
  fail_unless( UnitDefinition_isSetName(UD), NULL );

  if (UnitDefinition_getName(UD) == name)
  {
    fail("UnitDefinition_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  UnitDefinition_setName(UD, UnitDefinition_getName(UD));
  fail_unless( !strcmp(UnitDefinition_getName(UD), name), NULL );

  UnitDefinition_setName(UD, NULL);
  fail_unless( !UnitDefinition_isSetName(UD), NULL );

  if (UnitDefinition_getName(UD) != NULL)
  {
    fail("UnitDefinition_setName(R, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_UnitDefinition_addUnit)
{
  UnitDefinition_addUnit(UD, Unit_create());

  fail_unless( UnitDefinition_getNumUnits(UD) == 1, NULL );
}
END_TEST


START_TEST (test_UnitDefinition_getUnit)
{
  Unit_t *mole   = Unit_create();
  Unit_t *litre  = Unit_create();
  Unit_t *second = Unit_create();


  Unit_setKind( mole  , UnitKind_forName("mole")   );
  Unit_setKind( litre , UnitKind_forName("litre")  );
  Unit_setKind( second, UnitKind_forName("second") );

  Unit_setScale   (mole  , -3);
  Unit_setExponent(litre , -1);
  Unit_setExponent(second, -1);

  UnitDefinition_addUnit( UD, mole   );
  UnitDefinition_addUnit( UD, litre  );
  UnitDefinition_addUnit( UD, second );

  fail_unless( UnitDefinition_getNumUnits(UD) == 3, NULL );

  mole   = UnitDefinition_getUnit(UD, 0);
  litre  = UnitDefinition_getUnit(UD, 1);
  second = UnitDefinition_getUnit(UD, 2);

  fail_unless( Unit_getKind(mole)   == UNIT_KIND_MOLE  , NULL );
  fail_unless( Unit_getKind(litre)  == UNIT_KIND_LITRE , NULL );
  fail_unless( Unit_getKind(second) == UNIT_KIND_SECOND, NULL );

  fail_unless( Unit_getScale(mole)      == -3, NULL );
  fail_unless( Unit_getExponent(litre)  == -1, NULL );
  fail_unless( Unit_getExponent(second) == -1, NULL );
}
END_TEST


Suite *
create_suite_UnitDefinition (void)
{
  Suite *suite = suite_create("UnitDefinition");
  TCase *tcase = tcase_create("UnitDefinition");


  tcase_add_checked_fixture( tcase,
                             UnitDefinitionTest_setup,
                             UnitDefinitionTest_teardown );

  tcase_add_test( tcase, test_UnitDefinition_create         );
  tcase_add_test( tcase, test_UnitDefinition_createWith     );
  tcase_add_test( tcase, test_UnitDefinition_createWithName );
  tcase_add_test( tcase, test_UnitDefinition_free_NULL      );
  tcase_add_test( tcase, test_UnitDefinition_setId          );
  tcase_add_test( tcase, test_UnitDefinition_setName        );
  tcase_add_test( tcase, test_UnitDefinition_addUnit        );
  tcase_add_test( tcase, test_UnitDefinition_getUnit        );

  suite_add_tcase(suite, tcase);

  return suite;
}
