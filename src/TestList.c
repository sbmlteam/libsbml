/**
 * Filename    : TestList.c
 * Description : List unit tests
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-20
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <check.h>

#include "sbml/common.h"
#include "sbml/List.h"


static List_t *L;


void
ListTest_setup (void)
{
  L = List_create();

  if (L == NULL)
  {
    fail("List_create() returned a NULL pointer.");
  }
}


void
ListTest_teardown (void)
{
  List_free(L);
}


START_TEST (test_List_create)
{
  fail_unless(List_size(L) == 0, NULL);

  /*
  fail_unless(L->head == NULL, NULL);
  fail_unless(L->tail == NULL, NULL);
  */
}
END_TEST


START_TEST (test_ListNode_create)
{
  char       *s    = "foo";
  ListNode_t *node = ListNode_create(s);


  /*
  fail_unless(node->item == s   , NULL);
  fail_unless(node->next == NULL, NULL);
  */

  ListNode_free(node);
}
END_TEST


START_TEST (test_List_free_NULL)
{
  List_free(NULL);
}
END_TEST


START_TEST (test_List_add_1)
{
  List_add(L, "foo");

  fail_unless(List_size(L) == 1, NULL);

  /*
  fail_unless( !strcmp(L->head->item, "foo"), NULL );

  fail_unless(L->head       == L->tail, NULL);
  fail_unless(L->head->next == NULL   , NULL);
  */
}
END_TEST


START_TEST (test_List_add_2)
{
  List_add(L, "foo");
  List_add(L, "bar");

  fail_unless(List_size(L) == 2, NULL);

  /*
  fail_unless( !strcmp(L->head->item      , "foo"), NULL );
  fail_unless( !strcmp(L->head->next->item, "bar"), NULL );

  fail_unless(L->head->next == L->tail, NULL);
  fail_unless(L->tail->next == NULL   , NULL);
  */
}
END_TEST


int
myPredicate (const void *item)
{
  const char *s = (char *) item;


  return s[1] == 'a';
}


START_TEST (test_List_countIf)
{
  const char *foo = "foo";
  const char *bar = "bar";
  const char *baz = "baz";
  const char *bop = "bop";

  List_add(L, (void *) foo);
  List_add(L, (void *) bop);

  fail_unless( List_countIf(L, myPredicate) == 0 , NULL );

  List_add(L, (void *) foo);
  List_add(L, (void *) bar);
  List_add(L, (void *) baz);
  List_add(L, (void *) bop);

  fail_unless( List_countIf(L, myPredicate) == 2 , NULL );

  List_add(L, (void *) baz);

  fail_unless( List_countIf(L, myPredicate) == 3 , NULL );
}
END_TEST


START_TEST (test_List_findIf)
{
  List_t *list;

  const char *foo = "foo";
  const char *bar = "bar";
  const char *baz = "baz";
  const char *bop = "bop";


  List_add(L, (void *) foo);
  List_add(L, (void *) bop);

  list = List_findIf(L, myPredicate);

  fail_unless( list            != NULL, NULL );
  fail_unless( List_size(list) == 0   , NULL );

  List_free(list);

  List_add(L, (void *) foo);
  List_add(L, (void *) bar);
  List_add(L, (void *) baz);
  List_add(L, (void *) bop);

  list = List_findIf(L, myPredicate);

  fail_unless( list              != NULL, NULL );
  fail_unless( List_size(list)   == 2   , NULL );
  fail_unless( List_get(list, 0) == bar , NULL );
  fail_unless( List_get(list, 1) == baz , NULL );

  List_free(list);

  List_add(L, (void *) baz);

  list = List_findIf(L, myPredicate);

  fail_unless( list              != NULL, NULL );
  fail_unless( List_size(list)   == 3   , NULL );
  fail_unless( List_get(list, 0) == bar , NULL );
  fail_unless( List_get(list, 1) == baz , NULL );
  fail_unless( List_get(list, 2) == baz , NULL );

  List_free(list);
}
END_TEST


int
myStrCmp (const void *s1, const void *s2)
{
  return strcmp((const char *) s1, (const char *) s2);
}


START_TEST (test_List_find)
{
  const char *foo = "foo";
  const char *bar = "bar";
  const char *baz = "baz";
  const char *bop = "bop";

  List_add(L, (void *) foo);
  List_add(L, (void *) bar);
  List_add(L, (void *) baz);

  fail_unless( List_find(L, (void *) foo, myStrCmp) == foo , NULL );
  fail_unless( List_find(L, (void *) bar, myStrCmp) == bar , NULL );
  fail_unless( List_find(L, (void *) baz, myStrCmp) == baz , NULL );
  fail_unless( List_find(L, (void *) bop, myStrCmp) == NULL, NULL );
}
END_TEST


START_TEST (test_List_get)
{
  List_add(L, "foo");
  List_add(L, "bar");

  fail_unless(List_size(L) == 2, NULL);
 
  fail_unless( !strcmp(List_get(L, 0), "foo"), NULL );
  fail_unless( !strcmp(List_get(L, 1), "bar"), NULL );

  fail_unless(List_get(L, -1) == NULL, NULL);
  fail_unless(List_get(L,  2) == NULL, NULL);
}
END_TEST


START_TEST (test_List_prepend_1)
{
  List_prepend(L, "foo");

  fail_unless(List_size(L) == 1, NULL);

  /*
  fail_unless( !strcmp(L->head->item, "foo"), NULL );

  fail_unless(L->head       == L->tail, NULL);
  fail_unless(L->head->next == NULL   , NULL);
  */
}
END_TEST


START_TEST (test_List_prepend_2)
{
  List_prepend(L, "foo");
  List_prepend(L, "bar");

  fail_unless(List_size(L) == 2, NULL);

  /*
  fail_unless( !strcmp(L->head->item      , "bar"), NULL );
  fail_unless( !strcmp(L->head->next->item, "foo"), NULL );

  fail_unless(L->head->next == L->tail, NULL);
  fail_unless(L->tail->next == NULL   , NULL);
  */
}
END_TEST


START_TEST (test_List_remove_1)
{
  List_add(L, "foo");

  fail_unless( !strcmp(List_remove(L, 0), "foo"), NULL );

  fail_unless(List_size(L) == 0, NULL);

  /*
  fail_unless(L->head == NULL, NULL);
  fail_unless(L->tail == NULL, NULL);
  */
}
END_TEST


START_TEST (test_List_remove_2)
{
  List_add(L, "foo");
  List_add(L, "bar");

  fail_unless( !strcmp(List_remove(L, 1), "bar"), NULL );

  fail_unless(List_size(L) == 1, NULL);

  fail_unless( !strcmp( List_get(L, 0), "foo" ), NULL );

  /*
  fail_unless(L->head       == L->tail, NULL);
  fail_unless(L->head->next == NULL   , NULL);
  */
}
END_TEST


START_TEST (test_List_remove_3)
{
  List_add(L, "foo");
  List_add(L, "bar");
  List_add(L, "baz");

  fail_unless( !strcmp( List_remove(L, 1), "bar" ), NULL );

  fail_unless(List_size(L) == 2, NULL);

  fail_unless( !strcmp( List_get(L, 0), "foo" ), NULL );
  fail_unless( !strcmp( List_get(L, 1), "baz" ), NULL );

  /*
  fail_unless(L->head       != L->tail, NULL);
  fail_unless(L->tail->next == NULL   , NULL);
  */
}
END_TEST


START_TEST (test_List_remove_4)
{
  List_add(L, "foo");
  List_add(L, "bar");
  List_add(L, "baz");

  fail_unless( !strcmp( List_remove(L, 2), "baz" ), NULL );

  fail_unless(List_size(L) == 2, NULL);

  fail_unless( !strcmp( List_get(L, 0), "foo" ), NULL );
  fail_unless( !strcmp( List_get(L, 1), "bar" ), NULL );

  /*
  fail_unless(L->head       != L->tail, NULL);
  fail_unless(L->tail->next == NULL   , NULL);
  */
}
END_TEST


START_TEST (test_List_freeItems)
{
  List_add(L, safe_strdup("foo"));
  List_add(L, safe_strdup("bar"));
  List_add(L, safe_strdup("baz"));

  fail_unless(List_size(L) == 3, NULL);

  List_freeItems(L, safe_free, void);

  fail_unless(List_size(L) == 0   , NULL);

  /*
  fail_unless(L->head      == NULL, NULL);
  fail_unless(L->tail      == NULL, NULL);
  */
}
END_TEST


Suite *
create_suite_List (void)
{
  Suite *suite = suite_create("List");
  TCase *tcase = tcase_create("List");


  tcase_add_checked_fixture(tcase, ListTest_setup, ListTest_teardown);

  tcase_add_test( tcase, test_List_create     );
  tcase_add_test( tcase, test_List_free_NULL  );
  tcase_add_test( tcase, test_ListNode_create );
  tcase_add_test( tcase, test_List_add_1      );
  tcase_add_test( tcase, test_List_add_2      );
  tcase_add_test( tcase, test_List_get        );
  tcase_add_test( tcase, test_List_countIf    );
  tcase_add_test( tcase, test_List_findIf     );
  tcase_add_test( tcase, test_List_find       );
  tcase_add_test( tcase, test_List_prepend_1  );
  tcase_add_test( tcase, test_List_prepend_2  );
  tcase_add_test( tcase, test_List_remove_1   );
  tcase_add_test( tcase, test_List_remove_2   );
  tcase_add_test( tcase, test_List_remove_3   );
  tcase_add_test( tcase, test_List_remove_4   );
  tcase_add_test( tcase, test_List_freeItems  );

  suite_add_tcase(suite, tcase);

  return suite;
}
