/**
 * Filename    : TestUtil.h
 * Description : utilility functions unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-12-06
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common.h"


START_TEST (test_util_strcmp_insensitive)
{
  fail_unless( strcmp_insensitive("foobar", "foobar") == 0, NULL );
  fail_unless( strcmp_insensitive("foobar", "FooBar") == 0, NULL );

  fail_unless( strcmp_insensitive("foobar", "FooBaz") < 0, NULL );
  fail_unless( strcmp_insensitive("foobar", "FooBaZ") < 0, NULL );

  fail_unless( strcmp_insensitive("foobar", "FooBab") > 0, NULL );
  fail_unless( strcmp_insensitive("foobar", "FooBaB") > 0, NULL );

  fail_unless( strcmp_insensitive("", "")  == 0, NULL );

  fail_unless( strcmp_insensitive("", "a") < 0, NULL );
  fail_unless( strcmp_insensitive("a", "") > 0, NULL );
}
END_TEST


START_TEST (test_util_safe_strcat)
{
  char *p, *q, *r, *s;


  fail_unless( !strcmp( p = safe_strcat( "foo", "bar" ), "foobar" ), NULL );
  fail_unless( !strcmp( q = safe_strcat( "foo", ""    ), "foo"    ), NULL );
  fail_unless( !strcmp( r = safe_strcat( ""   , "bar" ), "bar"    ), NULL );
  fail_unless( !strcmp( s = safe_strcat( ""   , ""    ), ""       ), NULL );

  safe_free(p);
  safe_free(q);
  safe_free(r);
  safe_free(s);
}
END_TEST


START_TEST (test_util_NaN)
{
  double d = util_NaN();


  fail_unless( d != d, "util_NaN() did not return NaN.");
}
END_TEST


START_TEST (test_util_NegInf)
{
  double d = util_NegInf();


  if ( finite(d) || isnan(d) || d >= 0)
  {
    fail("util_NegInf() did not return -Inf.");
  }
}
END_TEST


START_TEST (test_util_PosInf)
{
  double d = util_PosInf();


  if ( finite(d) || isnan(d) || d <= 0)
  {
    fail("util_PosInf() did not return +Inf.");
  }
}
END_TEST


START_TEST (test_util_NegZero)
{
  double d = util_NegZero();


  fail_unless(d == 0, "util_NegZero() did not even return a zero!");
  fail_unless( util_isNegZero(d), NULL );
}
END_TEST


START_TEST (test_util_isInf)
{
  fail_unless( util_isInf( util_PosInf()  ) ==  1, NULL );
  fail_unless( util_isInf( util_NegInf()  ) == -1, NULL );
  fail_unless( util_isInf( util_NaN()     ) ==  0, NULL );
  fail_unless( util_isInf( util_NegZero() ) ==  0, NULL );

  fail_unless( util_isInf(0.0) == 0, NULL );
  fail_unless( util_isInf(1.2) == 0, NULL );
}
END_TEST


Suite *
create_suite_util (void) 
{ 
  Suite *suite = suite_create("util");
  TCase *tcase = tcase_create("util");


  tcase_add_test( tcase, test_util_strcmp_insensitive );
  tcase_add_test( tcase, test_util_safe_strcat        );
  tcase_add_test( tcase, test_util_NaN                );
  tcase_add_test( tcase, test_util_NegInf             );
  tcase_add_test( tcase, test_util_PosInf             );
  tcase_add_test( tcase, test_util_NegZero            );
  tcase_add_test( tcase, test_util_isInf              );

  suite_add_tcase(suite, tcase);

  return suite;
}
