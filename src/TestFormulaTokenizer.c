/**
 * Filename    : TestFormulaTokenizer.c
 * Description : FormulaTokenizer unit tests
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-02
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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

#include "sbml/common.h"
#include "sbml/FormulaTokenizer.h"


START_TEST (test_FormulaTokenizer_create)
{
  const char         *formula = "1 + two * 3";
  FormulaTokenizer_t *ft      = FormulaTokenizer_create(formula);


  fail_unless( !strcmp(ft->formula, formula), NULL );
  fail_unless( ft->pos == 0, NULL );

  FormulaTokenizer_free(ft);
}
END_TEST


START_TEST (test_FormulaTokenizer_free_NULL)
{
  FormulaTokenizer_free(NULL);
}
END_TEST


START_TEST (test_Token_create)
{
  Token_t *t = Token_create();


  fail_unless( t->type          == TT_UNKNOWN, NULL );
  fail_unless( t->value.ch      == '\0'      , NULL );
  fail_unless( t->value.name    == NULL      , NULL );
  fail_unless( t->value.integer == 0         , NULL );
  fail_unless( t->value.real    == 0.0       , NULL );
  fail_unless( t->exponent      == 0         , NULL );

  Token_free(t);
}
END_TEST


START_TEST (test_Token_free)
{
  Token_t *t = Token_create();


  t->type       = TT_NAME;
  t->value.name = safe_strdup("foo");

  Token_free(t);
}
END_TEST


START_TEST (test_Token_free_NULL)
{
  Token_free(NULL);
}
END_TEST


START_TEST (test_FormulaTokenizer_empty)
{
  FormulaTokenizer_t *ft = FormulaTokenizer_create("");
  Token_t            *t;


  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_END, NULL );
  fail_unless( t->value.ch == '\0'  , NULL );
  Token_free(t);

  /**
   * Subsequent calls to FormulaTokenizer_nextToken() should continue to
   * return TT_END.
   */
  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_END, NULL );
  fail_unless( t->value.ch == '\0'  , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_END, NULL );
  fail_unless( t->value.ch == '\0'  , NULL );
  Token_free(t);

  FormulaTokenizer_free(ft);
}
END_TEST


START_TEST (test_FormulaTokenizer_whitespace)
{
  FormulaTokenizer_t *ft = FormulaTokenizer_create(" \t \n ");
  Token_t            *t;


  t = FormulaTokenizer_nextToken(ft);

  fail_unless( t->type     == TT_END, NULL );
  fail_unless( t->value.ch == '\0'  , NULL );

  Token_free(t);
  FormulaTokenizer_free(ft);
}
END_TEST


START_TEST (test_FormulaTokenizer_operators)
{
  FormulaTokenizer_t *ft = FormulaTokenizer_create("+-*/^(),");
  Token_t            *t;


  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_PLUS, NULL );
  fail_unless( t->value.ch == '+'    , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_MINUS, NULL );
  fail_unless( t->value.ch == '-'     , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_TIMES, NULL );
  fail_unless( t->value.ch == '*'     , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_DIVIDE, NULL );
  fail_unless( t->value.ch == '/'      , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_POWER, NULL );
  fail_unless( t->value.ch == '^'     , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_LPAREN, NULL );
  fail_unless( t->value.ch == '('      , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_RPAREN, NULL );
  fail_unless( t->value.ch == ')'      , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_COMMA, NULL );
  fail_unless( t->value.ch == ','     , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless(t->type      == TT_END, NULL );
  fail_unless( t->value.ch == '\0'  , NULL );
  Token_free(t);

  FormulaTokenizer_free(ft);
}
END_TEST


START_TEST (test_FormulaTokenizer_names)
{
  FormulaTokenizer_t *ft = FormulaTokenizer_create("foobar Foo2Bar _Foo_Bar");
  Token_t            *t;


  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type == TT_NAME              , NULL );
  fail_unless( !strcmp(t->value.name, "foobar"), NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type == TT_NAME               , NULL );
  fail_unless( !strcmp(t->value.name, "Foo2Bar"), NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type == TT_NAME                , NULL );
  fail_unless( !strcmp(t->value.name, "_Foo_Bar"), NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless(t->type == TT_END, NULL);
  Token_free(t);

  FormulaTokenizer_free(ft);
}
END_TEST


START_TEST (test_FormulaTokenizer_numbers)
{
  FormulaTokenizer_t *ft = FormulaTokenizer_create("123 3.14 .007 6.7 5.");
  Token_t            *t;


  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type          == TT_INTEGER, NULL );
  fail_unless( t->value.integer == 123       , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL, NULL );
  fail_unless( t->value.real == 3.14   , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL, NULL );
  fail_unless( t->value.real == .007   , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL, NULL );
  fail_unless( t->value.real == 6.7    , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL, NULL );
  fail_unless( t->value.real == 5.0    , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless(t->type == TT_END, NULL);
  Token_free(t);

  FormulaTokenizer_free(ft);
}
END_TEST


START_TEST (test_FormulaTokenizer_numbers_nan_inf)
{
  FormulaTokenizer_t *ft = FormulaTokenizer_create("NaN Inf");
  Token_t            *t;


  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL      , NULL );
  fail_unless( t->value.real != t->value.real, NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type                   == TT_REAL, NULL );
  fail_unless( util_isInf(t->value.real) == 1, NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless(t->type == TT_END, NULL);
  Token_free(t);

  FormulaTokenizer_free(ft);
}
END_TEST


START_TEST (test_FormulaTokenizer_numbers_exp)
{
  const char         *formula = "12.3e1 .314E1 7e-3 .067e2 5E0 2e+12 3e 4";
  FormulaTokenizer_t *ft      = FormulaTokenizer_create(formula);
  Token_t            *t;


  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL_E, NULL );
  fail_unless( t->value.real == 12.3, NULL );
  fail_unless( t->exponent   == 1   , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL_E, NULL );
  fail_unless( t->value.real == .314, NULL );
  fail_unless( t->exponent   == 1   , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL_E, NULL );
  fail_unless( t->value.real ==  7, NULL );
  fail_unless( t->exponent   == -3, NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL_E, NULL );
  fail_unless( t->value.real == .067, NULL );
  fail_unless( t->exponent   == 2   , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL_E, NULL );
  fail_unless( t->value.real == 5.0, NULL );
  fail_unless( t->exponent   == 0  , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL_E, NULL );
  fail_unless( t->value.real == 2 , NULL );
  fail_unless( t->exponent   == 12, NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type       == TT_REAL_E, NULL );
  fail_unless( t->value.real == 3.0, NULL );
  fail_unless( t->exponent   == 0  , NULL );
  Token_free(t);

  /**
   * Gobble-up the '4'.  This last token is here as a test to ensure the
   * previous token is not interpreted as 3e4.
   */
  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type          == TT_INTEGER, NULL );
  fail_unless( t->value.integer == 4, NULL );
  Token_free(t);


  t = FormulaTokenizer_nextToken(ft);
  fail_unless(t->type == TT_END, NULL);
  Token_free(t);

  FormulaTokenizer_free(ft);
}
END_TEST


START_TEST (test_FormulaTokenizer_unknown)
{
  FormulaTokenizer_t *ft = FormulaTokenizer_create("bbornstein@acm.org");
  Token_t            *t;


  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type == TT_NAME, NULL );
  fail_unless( !strcmp(t->value.name, "bbornstein"), NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_UNKNOWN, NULL );
  fail_unless( t->value.ch == '@'       , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type == TT_NAME, NULL );
  fail_unless( !strcmp(t->value.name, "acm"), NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type     == TT_UNKNOWN, NULL );
  fail_unless( t->value.ch == '.'       , NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless( t->type == TT_NAME, NULL );
  fail_unless( !strcmp(t->value.name, "org"), NULL );
  Token_free(t);

  t = FormulaTokenizer_nextToken(ft);
  fail_unless(t->type == TT_END, NULL);
  Token_free(t);

  FormulaTokenizer_free(ft);
}
END_TEST


Suite *
create_suite_FormulaTokenizer (void) 
{ 
  Suite *suite = suite_create("FormulaTokenizer");
  TCase *tcase = tcase_create("FormulaTokenizer");
 

  tcase_add_test( tcase, test_FormulaTokenizer_create    );
  tcase_add_test( tcase, test_FormulaTokenizer_free_NULL );
  tcase_add_test( tcase, test_Token_create               );
  tcase_add_test( tcase, test_Token_free                 );
  tcase_add_test( tcase, test_Token_free_NULL            );

  tcase_add_test( tcase, test_FormulaTokenizer_empty           );
  tcase_add_test( tcase, test_FormulaTokenizer_whitespace      );
  tcase_add_test( tcase, test_FormulaTokenizer_operators       );
  tcase_add_test( tcase, test_FormulaTokenizer_names           );
  tcase_add_test( tcase, test_FormulaTokenizer_numbers         );
  tcase_add_test( tcase, test_FormulaTokenizer_numbers_exp     );
  tcase_add_test( tcase, test_FormulaTokenizer_numbers_nan_inf );
  tcase_add_test( tcase, test_FormulaTokenizer_unknown         );

  suite_add_tcase(suite, tcase);

  return suite;
}
