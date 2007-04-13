/**
 * \file    TestXMLAttributes.cpp
 * \brief   TestXMLAttributes unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */

#include <limits>

#include <check.h>
#include <XMLAttributes.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


CK_CPPSTART



START_TEST (test_XMLAttributes_add_get)
{
  XMLAttributes attrs;

  fail_unless( attrs.getLength() == 0 );
  fail_unless( attrs.isEmpty()        );

  attrs.add("xmlns", "http://foo.org/");
  fail_unless( attrs.getLength() == 1     );
  fail_unless( attrs.isEmpty()   == false );

  attrs.add("foo", "bar");
  fail_unless( attrs.getLength() == 2     );
  fail_unless( attrs.isEmpty()   == false );

  fail_unless( attrs.getIndex("xmlns") ==  0 );
  fail_unless( attrs.getIndex("foo"  ) ==  1 );
  fail_unless( attrs.getIndex("bar"  ) == -1 );

  fail_unless( attrs.getValue("xmlns") == "http://foo.org/" );
  fail_unless( attrs.getValue("foo"  ) == "bar"             );
  fail_unless( attrs.getValue("bar"  ) == ""                );
}
END_TEST


START_TEST (test_XMLAttributes_readInto_bool)
{
  XMLAttributes attrs;
  bool          value;

  attrs.add("bool0", "0"       );
  attrs.add("bool1", "1"       );
  attrs.add("bool2", "false"   );
  attrs.add("bool3", "true"    );
  attrs.add("bool4", " 0 "     );
  attrs.add("bool5", " 1 "     );
  attrs.add("bool6", " false " );
  attrs.add("bool7", " true "  );
  attrs.add("bool8", " fool "  );
  attrs.add("empty", ""        );

  value = true;

  fail_unless( attrs.readInto("bool0", value) == true );
  fail_unless( value == false );

  fail_unless( attrs.readInto("bool1", value) == true );
  fail_unless( value == true );

  fail_unless( attrs.readInto("bool2", value) == true );
  fail_unless( value == false );

  fail_unless( attrs.readInto("bool3", value) == true );
  fail_unless( value == true );

  fail_unless( attrs.readInto("bool4", value) == true );
  fail_unless( value == false );

  fail_unless( attrs.readInto("bool5", value) == true );
  fail_unless( value == true );

  fail_unless( attrs.readInto("bool6", value) == true );
  fail_unless( value == false );

  fail_unless( attrs.readInto("bool7", value) == true );
  fail_unless( value == true );

  fail_unless( attrs.readInto("bool8", value) == false );
  fail_unless( value == true );

  fail_unless( attrs.readInto("empty", value) == false );
  fail_unless( value == true );

  fail_unless( attrs.readInto("bar", value)   == false );
  fail_unless( value == true );
}
END_TEST


START_TEST (test_XMLAttributes_readInto_double)
{
  XMLAttributes attrs;
  double        value;

  attrs.add("double0", "0"       );
  attrs.add("double1", "1."      );
  attrs.add("double2", " 3.14"   );
  attrs.add("double3", "-2.72"   );
  attrs.add("double4", "6.022e23");
  attrs.add("double5", "-0"      );
  attrs.add("double6", "INF"     );
  attrs.add("double7", "-INF"    );
  attrs.add("double8", "NaN "    );
  attrs.add("double9", "3,14"    );
  attrs.add("empty"  , ""        );

  value = 42.0;

  fail_unless( attrs.readInto("double0", value) == true );
  fail_unless( value == 0.0 );

  fail_unless( attrs.readInto("double1", value) == true );
  fail_unless( value == 1.0 );

  fail_unless( attrs.readInto("double2", value) == true );
  fail_unless( value == 3.14 );

  fail_unless( attrs.readInto("double3", value) == true );
  fail_unless( value == -2.72 );

  fail_unless( attrs.readInto("double4", value) == true );
  fail_unless( value == 6.022e23);

  fail_unless( attrs.readInto("double5", value) == true );
  fail_unless( value == 0 );

  fail_unless( attrs.readInto("double6", value) == true      );
  fail_unless( value ==  numeric_limits<double>::infinity()  );

  fail_unless( attrs.readInto("double7", value) == true      );
  fail_unless( value == - numeric_limits<double>::infinity() );

  fail_unless( attrs.readInto("double8", value) == true );
  fail_unless( value != value );

  value = 42.0;

  fail_unless( attrs.readInto("double9", value)  == false );
  fail_unless( value == 42.0 );

  fail_unless( attrs.readInto("empty", value)    == false );
  fail_unless( value == 42.0 );

  fail_unless( attrs.readInto("bar", value)      == false );
  fail_unless( value == 42.0 );
}
END_TEST


START_TEST (test_XMLAttributes_readInto_long)
{
  XMLAttributes attrs;
  long          value;

  attrs.add("long0", " 0"   );
  attrs.add("long1", " 1"   );
  attrs.add("long2", " 2"   );
  attrs.add("long3", "-3"   );
  attrs.add("long4", "+4"   );
  attrs.add("long5", "3.14" );
  attrs.add("long6", "foo" );
  attrs.add("empty", ""    );

  value = 42;

  fail_unless( attrs.readInto("long0", value) == true );
  fail_unless( value == 0 );

  fail_unless( attrs.readInto("long1", value) == true );
  fail_unless( value == 1 );

  fail_unless( attrs.readInto("long2", value) == true );
  fail_unless( value == 2 );

  fail_unless( attrs.readInto("long3", value) == true );
  fail_unless( value == -3 );

  fail_unless( attrs.readInto("long4", value) == true );
  fail_unless( value == 4 );

  value = 42;

  fail_unless( attrs.readInto("long5", value) == false );
  fail_unless( value == 42 );

  fail_unless( attrs.readInto("long6", value) == false );
  fail_unless( value == 42 );

  fail_unless( attrs.readInto("empty", value) == false );
  fail_unless( value == 42 );

  fail_unless( attrs.readInto("bar", value)   == false );
  fail_unless( value == 42 );
}
END_TEST


Suite *
create_suite_XMLAttributes (void)
{
  Suite *suite = suite_create("XMLAttributes");
  TCase *tcase = tcase_create("XMLAttributes");

 
  tcase_add_test( tcase, test_XMLAttributes_add_get         );
  tcase_add_test( tcase, test_XMLAttributes_readInto_bool   );
  tcase_add_test( tcase, test_XMLAttributes_readInto_double );
  tcase_add_test( tcase, test_XMLAttributes_readInto_long   );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
