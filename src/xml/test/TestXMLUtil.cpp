/**
 * \file    TestXMLUtil.cpp
 * \brief   XMLUtil unit tests
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
 *   Stefan Hoops
 */


#include <iostream>
#include <check.h>

#include "common/common.h"


#ifdef USE_EXPAT
#  include "ExpatFormatter.h"
#else
#  include <xercesc/util/XMLString.hpp>
#  include <xercesc/util/PlatformUtils.hpp>
#endif  // USE_EXPAT


#include "sbml/SBMLUnicodeConstants.h"
#include "SAX2AttributesMock.h"
#include "XMLUtil.h"


BEGIN_C_DECLS


void
XMLUtilTest_setup (void)
{
  try
  {
    XML_PLATFORM_UTILS_INIT();
  }
  catch (...)
  {
    fail("XMLPlatformUtils::Initialize() threw an Exception.");
  }
}


void
XMLUtilTest_teardown (void)
{
#ifndef USE_EXPAT
  XMLPlatformUtils::Terminate();
#endif  // !USE_EXPAT
}


START_TEST (test_scanAttr_bool)
{
  bool f  = false;
  bool r  = true;
  bool bc = false;

  SAX2AttributesMock attrs1(3);
  SAX2AttributesMock attrs2(1);


  attrs1.add( "fast"             , "true"  );
  attrs1.add( "reversible"       , "false" );
  attrs1.add( "boundaryCondition", "1"     );
  attrs2.add( "fast"             , "0"     );

  fail_unless( XMLUtil::scanAttr(attrs1, ATTR_FAST, &f) == true, NULL );
  fail_unless( f == true, NULL );

  fail_unless( XMLUtil::scanAttr(attrs1, ATTR_REVERSIBLE, &r) == true, NULL );
  fail_unless( r == false, NULL );

  fail_unless( XMLUtil::scanAttr(attrs1, ATTR_BOUNDARY_CONDITION, &bc) == true,
               NULL );
  fail_unless( bc == true, NULL );

  fail_unless( XMLUtil::scanAttr(attrs2, ATTR_FAST, &f) == true , NULL );
  fail_unless( f == false, NULL );
}
END_TEST


START_TEST (test_scanAttr_bool_bogus)
{
  bool f = false;
  bool r = false;
  SAX2AttributesMock attrs(2);


  attrs.add( "fast"      , "foobar"  );
  attrs.add( "reversible", "truebar" );

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_FAST, &f) == false, NULL );
  fail_unless( f == false, NULL );

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_REVERSIBLE, &r) == false, NULL );
  fail_unless( r == false, NULL );
}
END_TEST


START_TEST (test_scanAttr_bool_empty)
{
  bool f = false;
  SAX2AttributesMock attrs(1);


  attrs.add("fast", "");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_FAST, &f) == false, NULL );
  fail_unless( f == false, NULL );
}
END_TEST


START_TEST (test_scanAttr_bool_formatted)
{
  bool f = false;
  bool r = true;
  SAX2AttributesMock attrs(2);


  attrs.add( "fast"      , "trUE"  );
  attrs.add( "reversible", "fAlSe" );

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_FAST, &f) == true, NULL );
  fail_unless( f == true, NULL );

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_REVERSIBLE, &r) == true, NULL );
  fail_unless( r == false, NULL );
}
END_TEST


START_TEST (test_scanAttr_bool_notFound)
{
  bool f = false;
  SAX2AttributesMock attrs(1);


  attrs.add("foobar", "true");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_FAST, &f) == false, NULL );
  fail_unless( f == false, NULL );
}
END_TEST


START_TEST (test_scanAttr_bool_ws)
{
  bool f  = false;
  bool r  = true;
  bool bc = false;

  SAX2AttributesMock attrs1(3);
  SAX2AttributesMock attrs2(1);


  attrs1.add( "fast"             , " true"   );
  attrs1.add( "reversible"       , "\tfalse" );
  attrs1.add( "boundaryCondition", "1  "     );
  attrs2.add( "fast"             , " 0 "     );

  fail_unless( XMLUtil::scanAttr(attrs1, ATTR_FAST, &f) == true, NULL );
  fail_unless( f == true, NULL );

  fail_unless( XMLUtil::scanAttr(attrs1, ATTR_REVERSIBLE, &r) == true, NULL );
  fail_unless( r == false, NULL );

  fail_unless( XMLUtil::scanAttr(attrs1, ATTR_BOUNDARY_CONDITION, &bc) == true,
               NULL );
  fail_unless( bc == true, NULL );

  fail_unless( XMLUtil::scanAttr(attrs2, ATTR_FAST, &f) == true , NULL );
  fail_unless( f == false, NULL );
}
END_TEST


START_TEST (test_scanAttr_int)
{
  int charge = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("charge", "2");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_CHARGE, &charge) == true, NULL );
  fail_unless( charge == 2, NULL );
}
END_TEST


START_TEST (test_scanAttr_int_bogus)
{
  int charge = 1;
  int scale  = 2;
  SAX2AttributesMock attrs(2);


  attrs.add( "charge", "foobar"    );
  attrs.add( "scale" , "123foobar" );


  fail_unless( XMLUtil::scanAttr(attrs, ATTR_CHARGE, &charge) == false, NULL );
  fail_unless( charge == 1, NULL );

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_SCALE, &scale) == false, NULL );
  fail_unless( scale == 2, NULL );
}
END_TEST


START_TEST (test_scanAttr_int_empty)
{
  int charge = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("charge", "");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_CHARGE, &charge) == false, NULL );
  fail_unless( charge == 1, NULL );
}
END_TEST


START_TEST (test_scanAttr_int_formatted)
{
  int charge = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("charge", "-3");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_CHARGE, &charge) == true, NULL );
  fail_unless( charge == -3, NULL );
}
END_TEST


START_TEST (test_scanAttr_int_notFound)
{
  int charge = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("foobar", "1");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_CHARGE, &charge) == false, NULL );
  fail_unless( charge == 1, NULL );
}
END_TEST


START_TEST (test_scanAttr_int_ws)
{
  int charge = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("charge", "  2 ");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_CHARGE, &charge) == true, NULL );
  fail_unless( charge == 2, NULL );
}
END_TEST


START_TEST (test_scanAttr_unsigned_int)
{
  unsigned int level = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("level", "2");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_LEVEL, &level) == true, NULL );
  fail_unless( level == 2, NULL );
}
END_TEST


START_TEST (test_scanAttr_unsigned_int_bogus)
{
  int stoichiometry = 1;
  int denominator   = 2;
  SAX2AttributesMock attrs(2);


  attrs.add( "stoichiometry", "foobar"    );
  attrs.add( "denominator"  , "123foobar" );


  fail_unless( XMLUtil::scanAttr(attrs,
                                 ATTR_STOICHIOMETRY,
                                 &stoichiometry) == false, NULL );
  fail_unless( stoichiometry == 1, NULL );

  fail_unless( XMLUtil::scanAttr(attrs,
                                 ATTR_DENOMINATOR,
                                 &denominator) == false, NULL );
  fail_unless( denominator == 2, NULL );
}
END_TEST


START_TEST (test_scanAttr_unsigned_int_negative)
{
  unsigned int level = 1;
  SAX2AttributesMock attrs(2);


  attrs.add("level", "-2");


  fail_unless( XMLUtil::scanAttr(attrs, ATTR_LEVEL, &level) == false, NULL );
  fail_unless( level == 1, NULL );
}
END_TEST


START_TEST (test_scanAttr_unsigned_int_empty)
{
  unsigned int version = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("version", "");

  fail_unless( XMLUtil::scanAttr(attrs,
                                 ATTR_VERSION,
                                 &version) == false, NULL );
  fail_unless( version == 1, NULL );
}
END_TEST


START_TEST (test_scanAttr_unsigned_int_formatted)
{
  unsigned int denominator = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("denominator", "+3");

  fail_unless( XMLUtil::scanAttr(attrs,
                                 ATTR_DENOMINATOR,
                                 &denominator) == true, NULL );
  fail_unless( denominator == 3, NULL );
}
END_TEST


START_TEST (test_scanAttr_unsigned_int_notFound)
{
  unsigned int level = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("foobar", "1");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_LEVEL, &level) == false, NULL );
  fail_unless( level == 1, NULL );
}
END_TEST


START_TEST (test_scanAttr_unsigned_int_ws)
{
  int version = 1;
  SAX2AttributesMock attrs(1);


  attrs.add("version", "  2 ");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VERSION, &version) == true, NULL );
  fail_unless( version == 2, NULL );
}
END_TEST


START_TEST (test_scanAttr_double)
{
  double volume = 1.41;
  SAX2AttributesMock attrs(1);


  attrs.add("volume", "3.14");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VOLUME, &volume) == true, NULL );
  fail_unless( volume == 3.14, NULL );
}
END_TEST


START_TEST (test_scanAttr_double_bogus)
{
  double value  = 2.72;
  double volume = 1.41;
  SAX2AttributesMock attrs(2);


  attrs.add( "value" , "foobar"     );
  attrs.add( "volume", "1.23foobar" );

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VALUE, &value) == false, NULL );
  fail_unless( value == 2.72, NULL );

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VOLUME, &volume) == false, NULL );
  fail_unless( volume == 1.41, NULL );
}
END_TEST


START_TEST (test_scanAttr_double_empty)
{
  double volume = 1.41;
  SAX2AttributesMock attrs(1);


  attrs.add("volume", "");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VOLUME, &volume) == false, NULL );
  fail_unless( volume == 1.41, NULL );
}
END_TEST


START_TEST (test_scanAttr_double_formatted)
{
  double value  = 2.72;
  double volume = 1.41;
  SAX2AttributesMock attrs(2);


  attrs.add("value" , "+4.666666667000000e-06");
  attrs.add("volume", "-1.000000000000000e+00");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VALUE, &value) == true, NULL );
  fail_unless( value == 4.666666667000000e-06, NULL );

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VOLUME, &volume) == true, NULL );
  fail_unless( volume == -1.0, NULL );
}
END_TEST


START_TEST (test_scanAttr_double_inf)
{
  double value  = 2.72;
  double volume = 1.41;
  SAX2AttributesMock attrs(2);


  attrs.add("value" , "+Inf");
  attrs.add("volume", "-Inf");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VALUE, &value) == true, NULL );
  fail_unless( util_isInf(value) == 1, NULL );

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VOLUME, &volume) == true, NULL );
  fail_unless( util_isInf(volume) == -1, NULL );
}
END_TEST


START_TEST (test_scanAttr_double_nan)
{
  double volume = 2.72;
  SAX2AttributesMock attrs(1);


  attrs.add("volume", "NaN");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VOLUME, &volume) == true, NULL );
  fail_unless( volume != volume, NULL );
}
END_TEST


START_TEST (test_scanAttr_double_negZero)
{
  double volume = 2.72;
  SAX2AttributesMock attrs(1);


  attrs.add("volume", "-0");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VOLUME, &volume) == true, NULL );
  fail_unless( util_isNegZero(volume), NULL );
}
END_TEST


START_TEST (test_scanAttr_double_notFound)
{
  double volume = 1.41;
  SAX2AttributesMock attrs(1);


  attrs.add("foobar", "3.14");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VOLUME, &volume) == false, NULL );
  fail_unless( volume == 1.41, NULL );
}
END_TEST


START_TEST (test_scanAttr_double_ws)
{
  double volume = 1.41;
  SAX2AttributesMock attrs(1);


  attrs.add("volume", " 2.72  ");

  fail_unless( XMLUtil::scanAttr(attrs, ATTR_VOLUME, &volume) == true, NULL );
  fail_unless( volume == 2.72, NULL );
}
END_TEST


START_TEST (test_scanAttrCStr_byIndex)
{
  char*        name  = NULL;
  unsigned int index = 0;
  SAX2AttributesMock attrs(2);


  attrs.add("name", "Pauline");
  attrs.add("name", "Ben");

  fail_unless( XMLUtil::scanAttrCStr(attrs, index, &name) == true, NULL );
  fail_unless( !strcmp(name, "Pauline"), NULL );

  safe_free(name);

  index = 1;
  fail_unless( XMLUtil::scanAttrCStr(attrs, index, &name) == true, NULL );
  fail_unless( !strcmp(name, "Ben"), NULL );

  safe_free(name);

  name  = NULL;
  index = 2;
  fail_unless( XMLUtil::scanAttrCStr(attrs, index, &name) == false , NULL );
  fail_unless( name == NULL, NULL );
}
END_TEST


START_TEST (test_scanAttrCStr)
{
  char* name = NULL;
  SAX2AttributesMock attrs(1);


  attrs.add("name", "Pauline");

  fail_unless( XMLUtil::scanAttrCStr(attrs, ATTR_NAME, &name) == true, NULL );
  fail_unless( !strcmp(name, "Pauline"), NULL );

  safe_free(name);
}
END_TEST


START_TEST (test_scanAttrCStr_notFound)
{
  char* name = NULL;
  SAX2AttributesMock attrs(1);


  fail_unless( XMLUtil::scanAttrCStr(attrs, ATTR_NAME, &name) == false, NULL );
  fail_unless( name == NULL, NULL );
}
END_TEST


Suite *
create_suite_XMLUtil (void)
{
  Suite *suite = suite_create("XMLUtil");
  TCase *tcase = tcase_create("XMLUtil");


  tcase_add_checked_fixture(tcase, XMLUtilTest_setup, XMLUtilTest_teardown);

  tcase_add_test( tcase, test_scanAttr_bool           );
  tcase_add_test( tcase, test_scanAttr_bool_bogus     );
  tcase_add_test( tcase, test_scanAttr_bool_empty     );
  tcase_add_test( tcase, test_scanAttr_bool_formatted );
  tcase_add_test( tcase, test_scanAttr_bool_notFound  );
  tcase_add_test( tcase, test_scanAttr_bool_ws        );

  tcase_add_test( tcase, test_scanAttr_int           );
  tcase_add_test( tcase, test_scanAttr_int_bogus     );
  tcase_add_test( tcase, test_scanAttr_int_empty     );
  tcase_add_test( tcase, test_scanAttr_int_formatted );
  tcase_add_test( tcase, test_scanAttr_int_notFound  );
  tcase_add_test( tcase, test_scanAttr_int_ws        );

  tcase_add_test( tcase, test_scanAttr_unsigned_int           );
  tcase_add_test( tcase, test_scanAttr_unsigned_int_bogus     );
  tcase_add_test( tcase, test_scanAttr_unsigned_int_negative  );
  tcase_add_test( tcase, test_scanAttr_unsigned_int_empty     );
  tcase_add_test( tcase, test_scanAttr_unsigned_int_formatted );
  tcase_add_test( tcase, test_scanAttr_unsigned_int_ws         );

  tcase_add_test( tcase, test_scanAttr_double           );
  tcase_add_test( tcase, test_scanAttr_double_bogus     );
  tcase_add_test( tcase, test_scanAttr_double_empty     );
  tcase_add_test( tcase, test_scanAttr_double_formatted );
  tcase_add_test( tcase, test_scanAttr_double_inf       );
  tcase_add_test( tcase, test_scanAttr_double_nan       );
  tcase_add_test( tcase, test_scanAttr_double_negZero   );
  tcase_add_test( tcase, test_scanAttr_double_notFound  );
  tcase_add_test( tcase, test_scanAttr_double_ws        );

  tcase_add_test( tcase, test_scanAttrCStr_byIndex   );
  tcase_add_test( tcase, test_scanAttrCStr           );
  tcase_add_test( tcase, test_scanAttrCStr_notFound  );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
