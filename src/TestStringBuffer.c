/**
 * Filename    : TestStringBuffer.c
 * Description : StringBuffer unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-06-21
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
#include "sbml/StringBuffer.h"


static StringBuffer_t *SB;


void
StringBufferTest_setup (void)
{
  SB = StringBuffer_create(10);

  if (SB == NULL)
  {
    fail("StringBuffer_create(10) returned a NULL pointer.");
  }
}


void
StringBufferTest_teardown (void)
{
  StringBuffer_free(SB);
}


START_TEST (test_StringBuffer_create)
{
  fail_unless(StringBuffer_length(SB)   ==  0, NULL);
  fail_unless(StringBuffer_capacity(SB) == 10, NULL);
}
END_TEST


START_TEST (test_StringBuffer_free_NULL)
{
  StringBuffer_free(NULL);
}
END_TEST


START_TEST (test_StringBuffer_append)
{
  char *s, *t;


  StringBuffer_append(SB, "foo");

  fail_unless( StringBuffer_length(SB)   ==  3, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  s = StringBuffer_toString(SB);
  fail_unless( !strcmp(s, "foo"), NULL );

  StringBuffer_append(SB, "bar");

  fail_unless( StringBuffer_length(SB)   ==  6, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  t = StringBuffer_toString(SB);
  fail_unless( !strcmp(t, "foobar"), NULL );

  safe_free(s);
  safe_free(t);
}
END_TEST


START_TEST (test_StringBuffer_appendChar)
{
  char *s, *t, *u;


  StringBuffer_appendChar(SB, '*');

  fail_unless( StringBuffer_length(SB)   ==  1, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  s = StringBuffer_toString(SB);
  fail_unless( !strcmp(s, "*"), NULL );

  StringBuffer_append(SB, "foo");

  fail_unless( StringBuffer_length(SB)   ==  4, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  t = StringBuffer_toString(SB);
  fail_unless( !strcmp(t, "*foo"), NULL );

  StringBuffer_appendChar(SB, '*');

  fail_unless( StringBuffer_length(SB)   ==  5, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  u = StringBuffer_toString(SB);
  fail_unless( !strcmp(u, "*foo*"), NULL );

  safe_free(s);
  safe_free(t);
  safe_free(u);
}
END_TEST


START_TEST (test_StringBuffer_appendInt)
{
  char *s, *t;


  StringBuffer_appendInt(SB, 1);

  fail_unless( StringBuffer_length(SB)   ==  1, NULL );
  fail_unless( StringBuffer_capacity(SB) == 20, NULL );

  s = StringBuffer_toString(SB);
  fail_unless( !strcmp(s, "1"), NULL );

  StringBuffer_appendChar(SB, ' ');
  StringBuffer_appendInt(SB, 23);

  fail_unless( StringBuffer_length(SB)   ==  4, NULL );
  fail_unless( StringBuffer_capacity(SB) == 20, NULL );

  t = StringBuffer_toString(SB);

  fail_unless( !strcmp(t, "1 23"), NULL );

  safe_free(s);
  safe_free(t);
}
END_TEST


START_TEST (test_StringBuffer_appendReal)
{
  char *s, *t;


  StringBuffer_appendReal(SB, 1.2);

  fail_unless( StringBuffer_length(SB)   ==  3, NULL );
  fail_unless( StringBuffer_capacity(SB) == 20, NULL );

  s = StringBuffer_toString(SB);
  fail_unless( !strcmp(s, "1.2"), NULL );

  StringBuffer_appendChar(SB, ' ');
  StringBuffer_appendReal(SB, 3);

  fail_unless( StringBuffer_length(SB)   ==  5, NULL );
  fail_unless( StringBuffer_capacity(SB) == 20, NULL );

  t = StringBuffer_toString(SB);
  fail_unless( !strcmp(t, "1.2 3"), NULL );

  safe_free(s);
  safe_free(t);
}
END_TEST


START_TEST (test_StringBuffer_grow)
{
  char *s;


  StringBuffer_append(SB, "foobar");

  fail_unless( StringBuffer_length(SB)   ==  6, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  StringBuffer_grow(SB, 10);

  fail_unless( StringBuffer_length(SB)   ==  6, NULL );
  fail_unless( StringBuffer_capacity(SB) == 20, NULL );

  s = StringBuffer_toString(SB);
  fail_unless( !strcmp(s, "foobar"), NULL );

  safe_free(s);
}
END_TEST


START_TEST (test_StringBuffer_append_grow)
{
  char *s, *t;


  StringBuffer_append(SB, "fooooooooo");

  fail_unless( StringBuffer_length(SB)   == 10, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  s = StringBuffer_toString(SB);
  fail_unless( !strcmp(s, "fooooooooo"), NULL );

  StringBuffer_append(SB, "bar");

  fail_unless( StringBuffer_length(SB)   == 13, NULL );
  fail_unless( StringBuffer_capacity(SB) == 20, NULL );

  t = StringBuffer_toString(SB);
  fail_unless( !strcmp(t, "fooooooooobar"), NULL );

  safe_free(s);
  safe_free(t);
}
END_TEST


START_TEST (test_StringBuffer_appendChar_grow)
{
  char *s, *t;


  StringBuffer_append(SB, "fooooooooo");

  fail_unless( StringBuffer_length(SB)   == 10, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  s = StringBuffer_toString(SB);
  fail_unless( !strcmp(s, "fooooooooo"), NULL );

  StringBuffer_appendChar(SB, '!');

  fail_unless( StringBuffer_length(SB)   == 11, NULL );
  fail_unless( StringBuffer_capacity(SB) == 20, NULL );

  t = StringBuffer_toString(SB);
  fail_unless( !strcmp(t, "fooooooooo!"), NULL );

  safe_free(s);
  safe_free(t);
}
END_TEST


START_TEST (test_StringBuffer_reset)
{
  StringBuffer_append(SB, "foo");

  fail_unless( StringBuffer_length(SB)   ==  3, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  StringBuffer_reset(SB);

  fail_unless( StringBuffer_length(SB)   ==  0, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  StringBuffer_append(SB, "foobarfoobar");

  fail_unless( StringBuffer_length(SB)   == 12, NULL );
  fail_unless( StringBuffer_capacity(SB) == 20, NULL );

  StringBuffer_reset(SB);

  fail_unless( StringBuffer_length(SB)   ==  0, NULL );
  fail_unless( StringBuffer_capacity(SB) == 20, NULL );
}
END_TEST


START_TEST (test_StringBuffer_toString)
{
  char *s;


  StringBuffer_append(SB, "foo");

  fail_unless( StringBuffer_length(SB)   ==  3, NULL );
  fail_unless( StringBuffer_capacity(SB) == 10, NULL );

  s = StringBuffer_toString(SB);
  fail_unless( !strcmp(s, "foo"), NULL );

  s[0] = 'b';

  if ( !strcmp(SB->buffer, "boo") )
  {
    fail("StringBuffer_toString() did not make a copy of its internal buffer.");
  }

  safe_free(s);
}
END_TEST


Suite *
create_suite_StringBuffer (void)
{
  Suite *suite = suite_create("StringBuffer");
  TCase *tcase = tcase_create("StringBuffer");

  tcase_add_checked_fixture( tcase,
                             StringBufferTest_setup,
                             StringBufferTest_teardown );

  tcase_add_test( tcase, test_StringBuffer_create          );
  tcase_add_test( tcase, test_StringBuffer_free_NULL       );
  tcase_add_test( tcase, test_StringBuffer_append          );
  tcase_add_test( tcase, test_StringBuffer_appendChar      );
  tcase_add_test( tcase, test_StringBuffer_appendInt       );
  tcase_add_test( tcase, test_StringBuffer_appendReal      );
  tcase_add_test( tcase, test_StringBuffer_grow            );
  tcase_add_test( tcase, test_StringBuffer_append_grow     );
  tcase_add_test( tcase, test_StringBuffer_appendChar_grow );
  tcase_add_test( tcase, test_StringBuffer_reset           );
  tcase_add_test( tcase, test_StringBuffer_toString        );

  suite_add_tcase(suite, tcase);

  return suite;
}
