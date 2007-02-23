/**
 * \file    TestUnit.c
 * \brief   Unit unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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


#include "common/common.h"

#include "SBase.h"
#include "Unit.h"

#include <check.h>


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
  fail_unless( SBase_getTypeCode  ((SBase_t *) U) == SBML_UNIT );
  fail_unless( SBase_getMetaId    ((SBase_t *) U) == NULL );
  //fail_unless( SBase_getNotes     ((SBase_t *) U) == NULL );
  //fail_unless( SBase_getAnnotation((SBase_t *) U) == NULL );

  fail_unless( Unit_getKind      (U) == UNIT_KIND_INVALID );
  fail_unless( Unit_getExponent  (U) == 1   );
  fail_unless( Unit_getScale     (U) == 0   );
  fail_unless( Unit_getMultiplier(U) == 1.0 );
  //fail_unless( Unit_getOffset    (U) == 0.0 );

  fail_unless( !Unit_isSetKind(U) );
}
END_TEST


START_TEST (test_Unit_createWith)
{
  Unit_t *u = Unit_createWith(UNIT_KIND_SECOND, -2, 1);


  fail_unless( SBase_getTypeCode  ((SBase_t *) u) == SBML_UNIT );
  fail_unless( SBase_getMetaId    ((SBase_t *) u) == NULL );
  //fail_unless( SBase_getNotes     ((SBase_t *) u) == NULL );
  //fail_unless( SBase_getAnnotation((SBase_t *) u) == NULL );

  fail_unless( Unit_getKind      (u) == UNIT_KIND_SECOND );
  fail_unless( Unit_getExponent  (u) == -2   );
  fail_unless( Unit_getScale     (u) ==  1   );
  fail_unless( Unit_getMultiplier(u) ==  1.0 );
  //fail_unless( Unit_getOffset    (u) ==  0.0 );

  fail_unless( Unit_isSetKind(u) );

  Unit_free(u);
}
END_TEST


START_TEST (test_Unit_free_NULL)
{
  Unit_free(NULL);
}
END_TEST


START_TEST (test_Unit_isXXX)
{
  fail_unless( !Unit_isSetKind(U) );

  Unit_setKind(U, UNIT_KIND_AMPERE);
  fail_unless( Unit_isAmpere(U) );

  Unit_setKind(U, UNIT_KIND_BECQUEREL);
  fail_unless( Unit_isBecquerel(U) );

  Unit_setKind(U, UNIT_KIND_CANDELA);
  fail_unless( Unit_isCandela(U) );

  //Unit_setKind(U, UNIT_KIND_CELSIUS);
  //fail_unless( Unit_isCelsius(U) );

  Unit_setKind(U, UNIT_KIND_COULOMB);
  fail_unless( Unit_isCoulomb(U) );

  Unit_setKind(U, UNIT_KIND_DIMENSIONLESS);
  fail_unless( Unit_isDimensionless(U) );

  Unit_setKind(U, UNIT_KIND_FARAD);
  fail_unless( Unit_isFarad(U) );

  Unit_setKind(U, UNIT_KIND_GRAM);
  fail_unless( Unit_isGram(U) );

  Unit_setKind(U, UNIT_KIND_GRAY);
  fail_unless( Unit_isGray(U) );

  Unit_setKind(U, UNIT_KIND_HENRY);
  fail_unless( Unit_isHenry(U) );

  Unit_setKind(U, UNIT_KIND_HERTZ);
  fail_unless( Unit_isHertz(U) );

  Unit_setKind(U, UNIT_KIND_ITEM);
  fail_unless( Unit_isItem(U) );

  Unit_setKind(U, UNIT_KIND_JOULE);
  fail_unless( Unit_isJoule(U) );

  Unit_setKind(U, UNIT_KIND_KATAL);
  fail_unless( Unit_isKatal(U) );

  Unit_setKind(U, UNIT_KIND_KELVIN);
  fail_unless( Unit_isKelvin(U) );

  Unit_setKind(U, UNIT_KIND_KILOGRAM);
  fail_unless( Unit_isKilogram(U) );

  Unit_setKind(U, UNIT_KIND_LITRE);
  fail_unless( Unit_isLitre(U) );

  Unit_setKind(U, UNIT_KIND_LUMEN);
  fail_unless( Unit_isLumen(U) );

  Unit_setKind(U, UNIT_KIND_LUX);
  fail_unless( Unit_isLux(U) );

  Unit_setKind(U, UNIT_KIND_METRE);
  fail_unless( Unit_isMetre(U) );

  Unit_setKind(U, UNIT_KIND_MOLE);
  fail_unless( Unit_isMole(U) );

  Unit_setKind(U, UNIT_KIND_NEWTON);
  fail_unless( Unit_isNewton(U) );

  Unit_setKind(U, UNIT_KIND_OHM);
  fail_unless( Unit_isOhm(U) );

  Unit_setKind(U, UNIT_KIND_PASCAL);
  fail_unless( Unit_isPascal(U) );

  Unit_setKind(U, UNIT_KIND_RADIAN);
  fail_unless( Unit_isRadian(U) );

  Unit_setKind(U, UNIT_KIND_SECOND);
  fail_unless( Unit_isSecond(U) );

  Unit_setKind(U, UNIT_KIND_SIEMENS);
  fail_unless( Unit_isSiemens(U) );

  Unit_setKind(U, UNIT_KIND_SIEVERT);
  fail_unless( Unit_isSievert(U) );

  Unit_setKind(U, UNIT_KIND_STERADIAN);
  fail_unless( Unit_isSteradian(U) );

  Unit_setKind(U, UNIT_KIND_TESLA);
  fail_unless( Unit_isTesla(U) );

  Unit_setKind(U, UNIT_KIND_VOLT);
  fail_unless( Unit_isVolt(U) );

  Unit_setKind(U, UNIT_KIND_WATT);
  fail_unless( Unit_isWatt(U) );

  Unit_setKind(U, UNIT_KIND_WEBER);
  fail_unless( Unit_isWeber(U) );
}
END_TEST


START_TEST (test_Unit_isBuiltIn)
{
  fail_unless( Unit_isBuiltIn( "substance") );
  fail_unless( Unit_isBuiltIn( "volume"   ) );
  fail_unless( Unit_isBuiltIn( "area"     ) );
  fail_unless( Unit_isBuiltIn( "length"   ) );
  fail_unless( Unit_isBuiltIn( "time"     ) );

  fail_unless( !Unit_isBuiltIn( NULL     ) );
  fail_unless( !Unit_isBuiltIn( ""       ) );
  fail_unless( !Unit_isBuiltIn( "volt"   ) );
  fail_unless( !Unit_isBuiltIn( "foobar" ) );
}
END_TEST


START_TEST (test_Unit_set_get)
{
  Unit_t *u = Unit_create();


  fail_unless( Unit_getKind      (u) == UNIT_KIND_INVALID );
  fail_unless( Unit_getExponent  (u) == 1   );
  fail_unless( Unit_getScale     (u) == 0   );
  fail_unless( Unit_getMultiplier(u) == 1.0 );
  fail_unless( !Unit_isSetKind(u) );

  Unit_setKind(u, UNIT_KIND_WATT);
  fail_unless( Unit_getKind      (u) == UNIT_KIND_WATT );

  Unit_setExponent(u, 3);
  fail_unless( Unit_getExponent  (u) == 3   );

  Unit_setScale(u, 4);
  fail_unless( Unit_getScale     (u) == 4  );

  Unit_setMultiplier(u, 3.2);
  fail_unless( Unit_getMultiplier(u) == 3.2 );

  Unit_free(u);
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
  tcase_add_test( tcase, test_Unit_isXXX      );
  tcase_add_test( tcase, test_Unit_isBuiltIn  );
  tcase_add_test( tcase, test_Unit_set_get    );

  suite_add_tcase(suite, tcase);

  return suite;
}
