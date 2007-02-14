/**
 * \file    TestUnitKind.h
 * \brief   UnitKind enumeration unit tests
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
#include "UnitKind.h"

#include <check.h>


START_TEST (test_UnitKind_equals)
{
  fail_unless( UnitKind_equals( UNIT_KIND_AMPERE , UNIT_KIND_AMPERE  ), NULL );
  fail_unless( UnitKind_equals( UNIT_KIND_INVALID, UNIT_KIND_INVALID ), NULL );
  fail_unless( UnitKind_equals( UNIT_KIND_LITER  , UNIT_KIND_LITER   ), NULL );
  fail_unless( UnitKind_equals( UNIT_KIND_LITRE  , UNIT_KIND_LITRE   ), NULL );
  fail_unless( UnitKind_equals( UNIT_KIND_METER  , UNIT_KIND_METER   ), NULL );
  fail_unless( UnitKind_equals( UNIT_KIND_METRE  , UNIT_KIND_METRE   ), NULL );

  fail_unless( UnitKind_equals(UNIT_KIND_LITER, UNIT_KIND_LITRE), NULL );
  fail_unless( UnitKind_equals(UNIT_KIND_LITRE, UNIT_KIND_LITER), NULL );

  fail_unless( UnitKind_equals(UNIT_KIND_METER, UNIT_KIND_METRE), NULL );
  fail_unless( UnitKind_equals(UNIT_KIND_METRE, UNIT_KIND_METER), NULL );

  fail_unless( !UnitKind_equals(UNIT_KIND_AMPERE, UNIT_KIND_WEBER), NULL );
}
END_TEST


START_TEST (test_UnitKind_forName)
{
  fail_unless( UnitKind_forName("ampere")    == UNIT_KIND_AMPERE   , NULL );
  fail_unless( UnitKind_forName("becquerel") == UNIT_KIND_BECQUEREL, NULL );
  fail_unless( UnitKind_forName("candela")   == UNIT_KIND_CANDELA  , NULL );
  fail_unless( UnitKind_forName("Celsius")   == UNIT_KIND_CELSIUS  , NULL );
  fail_unless( UnitKind_forName("coulomb")   == UNIT_KIND_COULOMB  , NULL );

  fail_unless( UnitKind_forName("dimensionless") == UNIT_KIND_DIMENSIONLESS,
               NULL);

  fail_unless( UnitKind_forName("farad")     == UNIT_KIND_FARAD    , NULL );
  fail_unless( UnitKind_forName("gram")      == UNIT_KIND_GRAM     , NULL );
  fail_unless( UnitKind_forName("gray")      == UNIT_KIND_GRAY     , NULL );
  fail_unless( UnitKind_forName("henry")     == UNIT_KIND_HENRY    , NULL );
  fail_unless( UnitKind_forName("hertz")     == UNIT_KIND_HERTZ    , NULL );
  fail_unless( UnitKind_forName("item")      == UNIT_KIND_ITEM     , NULL );
  fail_unless( UnitKind_forName("joule")     == UNIT_KIND_JOULE    , NULL );
  fail_unless( UnitKind_forName("katal")     == UNIT_KIND_KATAL    , NULL );
  fail_unless( UnitKind_forName("kelvin")    == UNIT_KIND_KELVIN   , NULL );
  fail_unless( UnitKind_forName("kilogram")  == UNIT_KIND_KILOGRAM , NULL );
  fail_unless( UnitKind_forName("liter")     == UNIT_KIND_LITER    , NULL );
  fail_unless( UnitKind_forName("litre")     == UNIT_KIND_LITRE    , NULL );
  fail_unless( UnitKind_forName("lumen")     == UNIT_KIND_LUMEN    , NULL );
  fail_unless( UnitKind_forName("lux")       == UNIT_KIND_LUX      , NULL );
  fail_unless( UnitKind_forName("meter")     == UNIT_KIND_METER    , NULL );
  fail_unless( UnitKind_forName("metre")     == UNIT_KIND_METRE    , NULL );
  fail_unless( UnitKind_forName("mole")      == UNIT_KIND_MOLE     , NULL );
  fail_unless( UnitKind_forName("newton")    == UNIT_KIND_NEWTON   , NULL );
  fail_unless( UnitKind_forName("ohm")       == UNIT_KIND_OHM      , NULL );
  fail_unless( UnitKind_forName("pascal")    == UNIT_KIND_PASCAL   , NULL );
  fail_unless( UnitKind_forName("radian")    == UNIT_KIND_RADIAN   , NULL );
  fail_unless( UnitKind_forName("second")    == UNIT_KIND_SECOND   , NULL );
  fail_unless( UnitKind_forName("siemens")   == UNIT_KIND_SIEMENS  , NULL );
  fail_unless( UnitKind_forName("sievert")   == UNIT_KIND_SIEVERT  , NULL );
  fail_unless( UnitKind_forName("steradian") == UNIT_KIND_STERADIAN, NULL );
  fail_unless( UnitKind_forName("tesla")     == UNIT_KIND_TESLA    , NULL );
  fail_unless( UnitKind_forName("volt")      == UNIT_KIND_VOLT     , NULL );
  fail_unless( UnitKind_forName("watt")      == UNIT_KIND_WATT     , NULL );
  fail_unless( UnitKind_forName("weber")     == UNIT_KIND_WEBER    , NULL );

  fail_unless( UnitKind_forName(NULL)     == UNIT_KIND_INVALID, NULL );
  fail_unless( UnitKind_forName("")       == UNIT_KIND_INVALID, NULL );
  fail_unless( UnitKind_forName("foobar") == UNIT_KIND_INVALID, NULL );
}
END_TEST


START_TEST (test_UnitKind_toString)
{
  const char* s;


  s = UnitKind_toString(UNIT_KIND_AMPERE);
  fail_unless(!strcmp(s, "ampere"), NULL);

  s = UnitKind_toString(UNIT_KIND_BECQUEREL);
  fail_unless(!strcmp(s, "becquerel"), NULL);

  s = UnitKind_toString(UNIT_KIND_CANDELA);
  fail_unless(!strcmp(s, "candela"), NULL);

  s = UnitKind_toString(UNIT_KIND_CELSIUS);
  fail_unless(!strcmp(s, "Celsius"), NULL);

  s = UnitKind_toString(UNIT_KIND_COULOMB);
  fail_unless(!strcmp(s, "coulomb"), NULL);

  s = UnitKind_toString(UNIT_KIND_DIMENSIONLESS);
  fail_unless(!strcmp(s, "dimensionless"), NULL);

  s = UnitKind_toString(UNIT_KIND_FARAD);
  fail_unless(!strcmp(s, "farad"), NULL);

  s = UnitKind_toString(UNIT_KIND_GRAM);
  fail_unless(!strcmp(s, "gram"), NULL);

  s = UnitKind_toString(UNIT_KIND_GRAY);
  fail_unless(!strcmp(s, "gray"), NULL);

  s = UnitKind_toString(UNIT_KIND_HENRY);
  fail_unless(!strcmp(s, "henry"), NULL);

  s = UnitKind_toString(UNIT_KIND_HERTZ);
  fail_unless(!strcmp(s, "hertz"), NULL);

  s = UnitKind_toString(UNIT_KIND_ITEM);
  fail_unless(!strcmp(s, "item"), NULL);

  s = UnitKind_toString(UNIT_KIND_JOULE);
  fail_unless(!strcmp(s, "joule"), NULL);

  s = UnitKind_toString(UNIT_KIND_KATAL);
  fail_unless(!strcmp(s, "katal"), NULL);

  s = UnitKind_toString(UNIT_KIND_KELVIN);
  fail_unless(!strcmp(s, "kelvin"), NULL);

  s = UnitKind_toString(UNIT_KIND_KILOGRAM);
  fail_unless(!strcmp(s, "kilogram"), NULL);

  s = UnitKind_toString(UNIT_KIND_LITER);
  fail_unless(!strcmp(s, "liter"), NULL);

  s = UnitKind_toString(UNIT_KIND_LITRE);
  fail_unless(!strcmp(s, "litre"), NULL);

  s = UnitKind_toString(UNIT_KIND_LUMEN);
  fail_unless(!strcmp(s, "lumen"), NULL);

  s = UnitKind_toString(UNIT_KIND_LUX);
  fail_unless(!strcmp(s, "lux"), NULL);

  s = UnitKind_toString(UNIT_KIND_METER);
  fail_unless(!strcmp(s, "meter"), NULL);

  s = UnitKind_toString(UNIT_KIND_METRE);
  fail_unless(!strcmp(s, "metre"), NULL);

  s = UnitKind_toString(UNIT_KIND_MOLE);
  fail_unless(!strcmp(s, "mole"), NULL);

  s = UnitKind_toString(UNIT_KIND_NEWTON);
  fail_unless(!strcmp(s, "newton"), NULL);

  s = UnitKind_toString(UNIT_KIND_OHM);
  fail_unless(!strcmp(s, "ohm"), NULL);

  s = UnitKind_toString(UNIT_KIND_PASCAL);
  fail_unless(!strcmp(s, "pascal"), NULL);

  s = UnitKind_toString(UNIT_KIND_RADIAN);
  fail_unless(!strcmp(s, "radian"), NULL);

  s = UnitKind_toString(UNIT_KIND_SECOND);
  fail_unless(!strcmp(s, "second"), NULL);

  s = UnitKind_toString(UNIT_KIND_SIEMENS);
  fail_unless(!strcmp(s, "siemens"), NULL);

  s = UnitKind_toString(UNIT_KIND_SIEVERT);
  fail_unless(!strcmp(s, "sievert"), NULL);

  s = UnitKind_toString(UNIT_KIND_STERADIAN);
  fail_unless(!strcmp(s, "steradian"), NULL);

  s = UnitKind_toString(UNIT_KIND_TESLA);
  fail_unless(!strcmp(s, "tesla"), NULL);

  s = UnitKind_toString(UNIT_KIND_VOLT);
  fail_unless(!strcmp(s, "volt"), NULL);

  s = UnitKind_toString(UNIT_KIND_WATT);
  fail_unless(!strcmp(s, "watt"), NULL);

  s = UnitKind_toString(UNIT_KIND_WEBER);
  fail_unless(!strcmp(s, "weber"), NULL);


  s = UnitKind_toString(UNIT_KIND_INVALID);
  fail_unless(!strcmp(s, "(Invalid UnitKind)"), NULL );

  s = UnitKind_toString(-1);
  fail_unless(!strcmp(s, "(Invalid UnitKind)"), NULL );

  s = UnitKind_toString(UNIT_KIND_INVALID + 1);
  fail_unless(!strcmp(s, "(Invalid UnitKind)"), NULL );
}
END_TEST


START_TEST (test_UnitKind_isValidUnitKindString)
{
  fail_unless( !UnitKind_isValidUnitKindString("fun-foam-unit for kids!"),
               NULL );

  fail_unless( UnitKind_isValidUnitKindString("litre"), NULL );
}
END_TEST


Suite *
create_suite_UnitKind (void) 
{ 
  Suite *suite = suite_create("UnitKind");
  TCase *tcase = tcase_create("UnitKind");


  tcase_add_test( tcase, test_UnitKind_equals   );
  tcase_add_test( tcase, test_UnitKind_forName  );
  tcase_add_test( tcase, test_UnitKind_toString );
  tcase_add_test( tcase, test_UnitKind_isValidUnitKindString );

  suite_add_tcase(suite, tcase);

  return suite;
}
