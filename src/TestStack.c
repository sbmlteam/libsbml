/**
 * Filename    : TestStack.c
 * Description : Stack unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-21
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
#include "sbml/Stack.h"


static Stack_t *S;


void
StackTest_setup (void)
{
  S = Stack_create(10);

  if (S == NULL)
  {
    fail("Stack_create(10) returned a NULL pointer.");
  }
}


void
StackTest_teardown (void)
{
  Stack_free(S);
}


START_TEST (test_Stack_create)
{
  fail_unless(Stack_size(S)     ==  0, NULL);
  fail_unless(Stack_capacity(S) == 10, NULL);
}
END_TEST


START_TEST (test_Stack_free_NULL)
{
  Stack_free(NULL);
}
END_TEST


START_TEST (test_Stack_push)
{
  Stack_push(S, "foo");

  fail_unless( Stack_size(S)     ==  1       , NULL );
  fail_unless( Stack_capacity(S) == 10       , NULL );
  fail_unless( !strcmp( Stack_peek(S), "foo"), NULL );
}
END_TEST


START_TEST (test_Stack_pop)
{
  char *item = NULL;


  Stack_push(S, "foo");
  
  item = (char *) Stack_pop(S);

  fail_unless( Stack_size(S)     ==  0, NULL );
  fail_unless( Stack_capacity(S) == 10, NULL );
  fail_unless( !strcmp(item, "foo")   , NULL );
}
END_TEST


START_TEST (test_Stack_popN)
{
  Stack_push(S, "foo");
  Stack_push(S, "bar");
  Stack_push(S, "baz");
  Stack_push(S, "bop");

  fail_unless( Stack_size(S) ==  4, NULL );

  fail_unless( Stack_popN(S, 0) == NULL, NULL );
  fail_unless( Stack_size(S) == 4, NULL );

  fail_unless( !strcmp(Stack_popN(S, 3), "bar"), NULL );
  fail_unless( Stack_size(S) == 1, NULL );

  fail_unless( !strcmp(Stack_popN(S, 1), "foo"), NULL );
  fail_unless( Stack_size(S) == 0, NULL );

  fail_unless( Stack_popN(S, 0) == NULL, NULL );
  fail_unless( Stack_size(S) == 0, NULL );
}
END_TEST


START_TEST (test_Stack_peek)
{
  char *item = NULL;


  Stack_push(S, "foo");

  item = (char *) Stack_peek(S);

  fail_unless( Stack_size(S)     ==  1, NULL );
  fail_unless( Stack_capacity(S) == 10, NULL );
  fail_unless( !strcmp(item, "foo")   , NULL );
}
END_TEST


START_TEST (test_Stack_peekAt)
{
  char *item = NULL;


  Stack_push(S, "foo");
  Stack_push(S, "bar");
  Stack_push(S, "baz");

  fail_unless( Stack_size(S)     ==  3, NULL );
  fail_unless( Stack_capacity(S) == 10, NULL );

  item = (char *) Stack_peekAt(S, 0);
  fail_unless( !strcmp(item, "baz"), NULL );

  item = (char *) Stack_peekAt(S, 1);
  fail_unless( !strcmp(item, "bar"), NULL );

  item = (char *) Stack_peekAt(S, 2);
  fail_unless( !strcmp(item, "foo"), NULL );

  fail_unless( Stack_peekAt(S, -1) == NULL, NULL );
  fail_unless( Stack_peekAt(S,  3) == NULL, NULL );
}
END_TEST


START_TEST (test_Stack_size)
{
  fail_unless( Stack_size(S)     ==  0, NULL );
  fail_unless( Stack_capacity(S) == 10, NULL );

  Stack_push(S, "foo");

  fail_unless( Stack_size(S)     ==  1, NULL );
  fail_unless( Stack_capacity(S) == 10, NULL );
}
END_TEST


START_TEST (test_Stack_capacity)
{
  fail_unless(Stack_capacity(S) == 10, NULL);
}
END_TEST


START_TEST (test_Stack_grow)
{
  int  i;


  for (i = 0; i < 10; i++)
  {
    Stack_push(S, "foo");
  }

  fail_unless( Stack_size(S)     == 10, NULL );
  fail_unless( Stack_capacity(S) == 10, NULL );
  fail_unless( !strcmp(Stack_peek(S), "foo"), NULL );

  Stack_push(S, "bar");

  fail_unless( Stack_size(S)     == 11      , NULL );
  fail_unless( Stack_capacity(S) == 20      , NULL );
  fail_unless( !strcmp(Stack_peek(S), "bar"), NULL );
}
END_TEST


START_TEST (test_Stack_find)
{
  char *s1 = "foo";
  char *s2 = "bar";
  char *s3 = "baz";
  char *s4 = "bop";


  Stack_push(S, s1);
  Stack_push(S, s2);
  Stack_push(S, s3);

  fail_unless( Stack_find(S, s1) ==  2, NULL );
  fail_unless( Stack_find(S, s2) ==  1, NULL );
  fail_unless( Stack_find(S, s3) ==  0, NULL );
  fail_unless( Stack_find(S, s4) == -1, NULL );
}
END_TEST


Suite *
create_suite_Stack (void)
{
  Suite *suite = suite_create("Stack");
  TCase *tcase = tcase_create("Stack");

  tcase_add_checked_fixture(tcase, StackTest_setup, StackTest_teardown);

  tcase_add_test( tcase, test_Stack_create    );
  tcase_add_test( tcase, test_Stack_free_NULL );
  tcase_add_test( tcase, test_Stack_push      );
  tcase_add_test( tcase, test_Stack_pop       );
  tcase_add_test( tcase, test_Stack_popN      );
  tcase_add_test( tcase, test_Stack_peek      );
  tcase_add_test( tcase, test_Stack_peekAt    );
  tcase_add_test( tcase, test_Stack_size      );
  tcase_add_test( tcase, test_Stack_capacity  );
  tcase_add_test( tcase, test_Stack_grow      );
  tcase_add_test( tcase, test_Stack_find      );

  suite_add_tcase(suite, tcase);

  return suite;
}
