/**
 * \file    TestStringMap.c
 * \brief   List unit tests
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


#include <check.h>

#include "common/common.h"
#include "StringMap.h"


static StringMap_t *SM;


void
StringMapTest_setup (void)
{
  SM = StringMap_create();

  if (SM == NULL)
  {
    fail("StringMap_create() returned a NULL pointer.");
  }
}


void
StringMapTest_teardown (void)
{
  StringMap_free(SM);
}


START_TEST (test_StringMap_basics)
{
  fail_unless( StringMap_size(SM)       == 0    );
  fail_unless( StringMap_get(SM, "key") == NULL );

  StringMap_put(SM, "key", "value");

  fail_unless( !strcmp(StringMap_get(SM, "key"), "value") );

  fail_unless(StringMap_size(SM) == 1);
}
END_TEST


START_TEST (test_StringMap_duplicate)
{
  StringMap_put(SM, "foo", "bar");
  StringMap_put(SM, "foo", "baz");

  fail_unless(!strcmp( StringMap_get(SM, "foo"), "baz") );

  fail_unless(StringMap_size(SM) == 1);
}
END_TEST


START_TEST (test_StringMap_grow)
{
  StringMap_put( SM, "1" , "one"   );
  StringMap_put( SM, "2" , "two"   );
  StringMap_put( SM, "3" , "three" );
  StringMap_put( SM, "4" , "four"  );
  StringMap_put( SM, "5" , "five"  );
  StringMap_put( SM, "6" , "six"   );
  StringMap_put( SM, "7" , "seven" );
  StringMap_put( SM, "8" , "eight" );
  StringMap_put( SM, "9" , "nine"  );
  StringMap_put( SM, "10", "ten"   );

  fail_unless( StringMap_size(SM)      == 10   ); 
  fail_unless( SM->capacity            == 10   ); 
  fail_unless( StringMap_get(SM, "11") == NULL );

  StringMap_put(SM, "11", "eleven");

  fail_unless( StringMap_size(SM) ==  11 ); 
  fail_unless( SM->capacity       == 100 ); 

  fail_unless( !strcmp(StringMap_get(SM,  "1"), "one"   ) );
  fail_unless( !strcmp(StringMap_get(SM, "10"), "ten"   ) );
  fail_unless( !strcmp(StringMap_get(SM, "11"), "eleven") );
}
END_TEST


START_TEST (test_StringMap_remove)
{
  fail_unless( !StringMap_exists(SM, "gnip") );

  StringMap_put(SM, "gnip", "gnop");
  fail_unless( StringMap_exists(SM, "gnip") );

  StringMap_remove(SM, "gnip");
  fail_unless( !StringMap_exists(SM, "gnip") );
  fail_unless( StringMap_size(SM) == 0       );
}
END_TEST


START_TEST (test_StringMap_nullValue)
{
   fail_unless( !StringMap_exists(SM, "null")         );
   fail_unless( StringMap_get    (SM, "null") == NULL );

   StringMap_put(SM, "null");

   fail_unless( StringMap_exists(SM, "null")         );
   fail_unless( StringMap_get   (SM, "null") == NULL );
   fail_unless( StringMap_size  (SM)         == 1    );
}
END_TEST


Suite *
create_suite_StringMap (void)
{
  Suite *suite = suite_create("StringMap");
  TCase *tcase = tcase_create("StringMap");


  tcase_add_checked_fixture(tcase, StringMapTest_setup, StringMapTest_teardown);

  tcase_add_test( tcase, test_StringMap_basics    );
  tcase_add_test( tcase, test_StringMap_duplicate );
  tcase_add_test( tcase, test_StringMap_grow      );
  tcase_add_test( tcase, test_StringMap_remove    );
  tcase_add_test( tcase, test_StringMap_nullValue );

  suite_add_tcase(suite, tcase);

  return suite;
}
