/**
 * \file    TestUtil.h
 * \brief   utilility functions unit tests
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
#include <locale.h>

#include "common/common.h"


START_TEST (test_c_locale_snprintf)
{
  char s[32];


  setlocale(LC_ALL, "de_DE");

  fail_unless( snprintf(s, sizeof(s), "%3.2f", 3.14) == 4 );
  fail_unless( !strcmp(s, "3,14")                         );
  
  fail_unless( c_locale_snprintf(s, sizeof(s), "%3.2f", 3.14) == 4 );
  fail_unless( !strcmp(s, "3.14")                                  );

  setlocale(LC_ALL, "C");
}
END_TEST


/**
 * The nature of vsnprintf makes it difficult to unit test using the
 * 'check' framework.  For now, since c_local_snprintf is written in terms
 * of c_locale_vsnprintf, if the "sn" test succeeds, we assume "vsn" is
 * correct.
 *
START_TEST (test_c_locale_vsnprintf)
{
}
END_TEST
*/


START_TEST (test_c_locale_strtod)
{
  const char *de = "2,72";
  const char *en = "2.72";
  char *endptr;


  setlocale(LC_ALL, "de_DE");

  endptr = NULL;
  fail_unless( strtod(de, &endptr) == 2.72 );
  fail_unless( (endptr - de)       == 4    );
  fail_unless( errno != ERANGE             );

  endptr = NULL;
  fail_unless( c_locale_strtod(en, &endptr) == 2.72 );
  fail_unless( (endptr - en)                == 4    );
  fail_unless( errno != ERANGE                      );

  setlocale(LC_ALL, "C");
}
END_TEST


START_TEST (test_util_file_exists)
{
  fail_unless(  util_file_exists("TestUtil.c")      );
  fail_unless( !util_file_exists("NonexistentFile") );
}
END_TEST


START_TEST (test_util_strcmp_insensitive)
{
  fail_unless( strcmp_insensitive("foobar", "foobar") == 0 );
  fail_unless( strcmp_insensitive("foobar", "FooBar") == 0 );

  fail_unless( strcmp_insensitive("foobar", "FooBaz") < 0 );
  fail_unless( strcmp_insensitive("foobar", "FooBaZ") < 0 );

  fail_unless( strcmp_insensitive("foobar", "FooBab") > 0 );
  fail_unless( strcmp_insensitive("foobar", "FooBaB") > 0 );

  fail_unless( strcmp_insensitive("", "")  == 0 );

  fail_unless( strcmp_insensitive("", "a") < 0 );
  fail_unless( strcmp_insensitive("a", "") > 0 );
}
END_TEST


START_TEST (test_util_safe_strcat)
{
  char *p, *q, *r, *s;


  fail_unless( !strcmp( p = safe_strcat( "foo", "bar" ), "foobar" ) );
  fail_unless( !strcmp( q = safe_strcat( "foo", ""    ), "foo"    ) );
  fail_unless( !strcmp( r = safe_strcat( ""   , "bar" ), "bar"    ) );
  fail_unless( !strcmp( s = safe_strcat( ""   , ""    ), ""       ) );

  safe_free(p);
  safe_free(q);
  safe_free(r);
  safe_free(s);
}
END_TEST


START_TEST (test_util_trim)
{
  char *p, *q, *r, *s, *t, *u, *v, *w, *x, *y, *z;


  fail_unless( !strcmp( p = util_trim("p"  ), "p") );
  fail_unless( !strcmp( q = util_trim("q " ), "q") );
  fail_unless( !strcmp( r = util_trim(" r" ), "r") );
  fail_unless( !strcmp( s = util_trim(" s "), "s") );

  fail_unless( !strcmp( t = util_trim("foo"  ), "foo") );
  fail_unless( !strcmp( u = util_trim("foo " ), "foo") );
  fail_unless( !strcmp( v = util_trim(" bar" ), "bar") );
  fail_unless( !strcmp( w = util_trim(" bar "), "bar") );

  fail_unless( !strcmp( x = util_trim(" foo bar " ), "foo bar") );

  fail_unless( !strcmp( y = util_trim(" "), "") );
  fail_unless( !strcmp( z = util_trim("" ), "") );


  fail_unless( util_trim((char *) NULL) == NULL );

  safe_free(p);
  safe_free(q);
  safe_free(r);
  safe_free(s);
  safe_free(t);
  safe_free(u);
  safe_free(v);
  safe_free(w);
  safe_free(x);
  safe_free(y);
  safe_free(z);
}
END_TEST


START_TEST (test_util_trim_in_place)
{
  char *s = safe_malloc(100);

  strcpy(s, "p");
  fail_unless( !strcmp( util_trim_in_place(s), "p") );

  strcpy(s, "q ");
  fail_unless( !strcmp( util_trim_in_place(s), "q") );

  strcpy(s, " r");
  fail_unless( !strcmp( util_trim_in_place(s), "r") );

  strcpy(s, " s ");
  fail_unless( !strcmp( util_trim_in_place(s), "s") );

  strcpy(s, "foo");
  fail_unless( !strcmp( util_trim_in_place(s), "foo") );

  strcpy(s, "foo ");
  fail_unless( !strcmp( util_trim_in_place(s), "foo") );

  strcpy(s, " bar");
  fail_unless( !strcmp( util_trim_in_place(s), "bar") );

  strcpy(s, " bar ");
  fail_unless( !strcmp( util_trim_in_place(s), "bar") );

  strcpy(s, " foo bar ");
  fail_unless( !strcmp( util_trim_in_place(s), "foo bar") );

  strcpy(s, " ");
  fail_unless( !strcmp( util_trim_in_place(s), "") );

  strcpy(s, "");
  fail_unless( !strcmp( util_trim_in_place(s), "") );

  fail_unless( util_trim_in_place((char *) NULL) == NULL );

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
  fail_unless( util_isNegZero(d) );
}
END_TEST


START_TEST (test_util_isInf)
{
  fail_unless( util_isInf( util_PosInf()  ) ==  1 );
  fail_unless( util_isInf( util_NegInf()  ) == -1 );
  fail_unless( util_isInf( util_NaN()     ) ==  0 );
  fail_unless( util_isInf( util_NegZero() ) ==  0 );

  fail_unless( util_isInf(0.0) == 0 );
  fail_unless( util_isInf(1.2) == 0 );
}
END_TEST


Suite *
create_suite_util (void) 
{ 
  Suite *suite = suite_create("util");
  TCase *tcase = tcase_create("util");


  tcase_add_test( tcase, test_c_locale_snprintf       );
  tcase_add_test( tcase, test_c_locale_strtod         );
  tcase_add_test( tcase, test_util_file_exists        );
  tcase_add_test( tcase, test_util_strcmp_insensitive );
  tcase_add_test( tcase, test_util_safe_strcat        );
  tcase_add_test( tcase, test_util_trim               );
  tcase_add_test( tcase, test_util_trim_in_place      );
  tcase_add_test( tcase, test_util_NaN                );
  tcase_add_test( tcase, test_util_NegInf             );
  tcase_add_test( tcase, test_util_PosInf             );
  tcase_add_test( tcase, test_util_NegZero            );
  tcase_add_test( tcase, test_util_isInf              );

  suite_add_tcase(suite, tcase);

  return suite;
}
