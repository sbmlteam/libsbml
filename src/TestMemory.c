/**
 * Filename    : TestMemory.h
 * Description : memory functions unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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


/**
 * Tests in this suite are somewhat verbose.  Since most of the tests are
 * of memory tracing facilities, each piece of memory needs to be
 * allocated, tracked and freed for each test.
 */


/**
 * This is really a dummy test, without it, if TRACE_MEMORY was not defined,
 * the test suite would be empty.
 */
START_TEST (test_memory_safe_malloc)
{
  void *p = safe_malloc(1024);


  if (p == NULL)
  {
    fail("safe_malloc() should not return NULL");
  }

  safe_free(p);
}
END_TEST


#ifdef TRACE_MEMORY


START_TEST (test_memory_MemTrace_MemInfoList_create)
{
  MemInfoList_t *list = MemTrace_MemInfoList_create();


  fail_unless( list->head == NULL, NULL );
  fail_unless( list->tail == NULL, NULL );
  fail_unless( list->size ==    0, NULL );

  MemTrace_MemInfoList_free(list);
}
END_TEST


START_TEST (test_memory_MemTrace_MemInfoNode_create)
{
  MemInfoNode_t *node;
  int           line;


  node = MemTrace_MemInfoNode_create((int *) 42, 4, "Foo.c", line = __LINE__);

  fail_unless( node->address == (int *) 42, NULL );
  fail_unless( node->size    ==          4, NULL );
  fail_unless( node->line    == line, NULL );
  fail_unless( node->next    == NULL, NULL );

  fail_unless( !strcmp(node->filename, "Foo.c"), NULL );

  free(node);
}
END_TEST


START_TEST (test_memory_MemTrace_MemInfoList_append_1)
{
  MemInfoList_t *list = MemTrace_MemInfoList_create();
  MemInfoNode_t *node = NULL;
  int           size  = sizeof(MemInfoList_t); 


  node = MemTrace_MemInfoNode_create(&list, size, __FILE__, __LINE__);
  MemTrace_MemInfoList_append(list, node);

  fail_unless( list->head       == node, NULL );
  fail_unless( list->tail       == node, NULL );
  fail_unless( list->head->next == NULL, NULL );
  fail_unless( list->tail->next == NULL, NULL ); 
  fail_unless( list->size       ==    1, NULL );

  MemTrace_MemInfoList_free(list);
}
END_TEST


START_TEST (test_memory_MemTrace_MemInfoList_append_2)
{
  MemInfoList_t *list  = MemTrace_MemInfoList_create();
  MemInfoNode_t *node1 = NULL;
  MemInfoNode_t *node2 = NULL;
  int           size   = sizeof(MemInfoList_t); 


  node1 = MemTrace_MemInfoNode_create(&list, size, __FILE__, __LINE__);
  node2 = MemTrace_MemInfoNode_create(&list, size, __FILE__, __LINE__);

  MemTrace_MemInfoList_append(list, node1);
  MemTrace_MemInfoList_append(list, node2);

  fail_unless( list->head       == node1, NULL );
  fail_unless( list->tail       == node2, NULL );
  fail_unless( list->head->next == node2, NULL );
  fail_unless( list->tail->next ==  NULL, NULL ); 
  fail_unless( list->size       ==     2, NULL );

  MemTrace_MemInfoList_free(list);
}
END_TEST


START_TEST (test_memory_MemTrace_MemInfoList_get)
{
  MemInfoList_t *list  = MemTrace_MemInfoList_create();
  MemInfoNode_t *node1 = NULL;
  MemInfoNode_t *node2 = NULL;
  MemInfoNode_t *node3 = NULL;
  MemInfoNode_t *node4 = NULL;


  node1 = MemTrace_MemInfoNode_create( (int *) 41, 4, __FILE__, __LINE__);
  node2 = MemTrace_MemInfoNode_create( (int *) 42, 4, __FILE__, __LINE__);
  node3 = MemTrace_MemInfoNode_create( (int *) 43, 4, __FILE__, __LINE__);
  node4 = MemTrace_MemInfoNode_create( (int *) 44, 4, __FILE__, __LINE__);

  MemTrace_MemInfoList_append(list, node1);
  MemTrace_MemInfoList_append(list, node2);
  MemTrace_MemInfoList_append(list, node3);
  MemTrace_MemInfoList_append(list, node4);

  fail_unless( MemTrace_MemInfoList_get(list, (int *) 40) ==  NULL, NULL );
  fail_unless( MemTrace_MemInfoList_get(list, (int *) 41) == node1, NULL );
  fail_unless( MemTrace_MemInfoList_get(list, (int *) 42) == node2, NULL );
  fail_unless( MemTrace_MemInfoList_get(list, (int *) 43) == node3, NULL );
  fail_unless( MemTrace_MemInfoList_get(list, (int *) 44) == node4, NULL );

  MemTrace_MemInfoList_free(list);
}
END_TEST


START_TEST (test_memory_MemTrace_MemInfoList_remove)
{
  MemInfoList_t *list    = MemTrace_MemInfoList_create();
  MemInfoNode_t *node1   = NULL;
  MemInfoNode_t *node2   = NULL;
  MemInfoNode_t *node3   = NULL;
  MemInfoNode_t *node4   = NULL;
  MemInfoNode_t *removed = NULL;


  node1 = MemTrace_MemInfoNode_create( (int *) 41, 4, __FILE__, __LINE__);
  node2 = MemTrace_MemInfoNode_create( (int *) 42, 4, __FILE__, __LINE__);
  node3 = MemTrace_MemInfoNode_create( (int *) 43, 4, __FILE__, __LINE__);
  node4 = MemTrace_MemInfoNode_create( (int *) 44, 4, __FILE__, __LINE__);

  MemTrace_MemInfoList_append(list, node1);
  MemTrace_MemInfoList_append(list, node2);
  MemTrace_MemInfoList_append(list, node3);
  MemTrace_MemInfoList_append(list, node4);
  fail_unless( list->size == 4, NULL );

  /* Not Found */
  fail_unless( MemTrace_MemInfoList_remove(list, (int *) 40) == NULL, NULL );
  fail_unless( list->size == 4, NULL );

  /* Remove middle */
  removed = MemTrace_MemInfoList_remove(list, (int *) 43);
  fail_unless( removed    == node3, NULL );
  fail_unless( list->head == node1, NULL );
  fail_unless( list->tail == node4, NULL );
  fail_unless( list->size ==     3, NULL );

  /* Remove first */
  removed = MemTrace_MemInfoList_remove(list, (int *) 41);
  fail_unless( removed    == node1, NULL );
  fail_unless( list->head == node2, NULL );
  fail_unless( list->tail == node4, NULL );
  fail_unless( list->size ==     2, NULL );

  /* Remove last */
  removed = MemTrace_MemInfoList_remove(list, (int *) 44);
  fail_unless( removed    == node4, NULL );
  fail_unless( list->head == node2, NULL );
  fail_unless( list->tail == node2, NULL );
  fail_unless( list->size ==     1, NULL );

  MemTrace_MemInfoList_free(list);
}
END_TEST


START_TEST (test_memory_MemTrace_init)
{
  MemTrace_init();

  fail_unless( MemTrace_getNumAllocs()         == 0, NULL );
  fail_unless( MemTrace_getNumLeaks()          == 0, NULL );
  fail_unless( MemTrace_getNumFrees()          == 0, NULL );
  fail_unless( MemTrace_getNumUnmatchedFrees() == 0, NULL );
}
END_TEST


START_TEST (test_memory_MemTrace_getAllocs)
{
  void *p = safe_malloc(1024);
  void *q = safe_malloc(1024);


  fail_unless( MemTrace_getNumAllocs() == 2, NULL );
  fail_unless( MemTrace_getNumLeaks()  == 2, NULL );
  fail_unless( MemTrace_getNumFrees()  == 0, NULL );

  safe_free(p);

  fail_unless( MemTrace_getNumAllocs() == 2, NULL );
  fail_unless( MemTrace_getNumLeaks()  == 1, NULL );
  fail_unless( MemTrace_getNumFrees()  == 1, NULL );

  safe_free(q);

  fail_unless( MemTrace_getNumAllocs() == 2, NULL );
  fail_unless( MemTrace_getNumLeaks()  == 0, NULL );
  fail_unless( MemTrace_getNumFrees()  == 2, NULL );
}
END_TEST


START_TEST (test_memory_MemTrace_getNumUnmatchedFrees)
{
  void *p = malloc(1024);
  void *q = malloc(1024);


  fail_unless( MemTrace_getNumFrees()          == 2, NULL );
  fail_unless( MemTrace_getNumUnmatchedFrees() == 0, NULL );

  safe_free(p);

  fail_unless( MemTrace_getNumFrees()          == 3, NULL );
  fail_unless( MemTrace_getNumUnmatchedFrees() == 1, NULL );

  safe_free(q);

  fail_unless( MemTrace_getNumFrees()          == 4, NULL );
  fail_unless( MemTrace_getNumUnmatchedFrees() == 2, NULL );
}
END_TEST


START_TEST (test_memory_MemTrace_reset)
{
  MemTrace_reset();

  fail_unless( MemTrace_getNumAllocs()         == 0, NULL );
  fail_unless( MemTrace_getNumLeaks()          == 0, NULL );
  fail_unless( MemTrace_getNumFrees()          == 0, NULL );
  fail_unless( MemTrace_getNumUnmatchedFrees() == 0, NULL );
}
END_TEST


#endif  /** TRACE_MEMORY **/


Suite *
create_suite_memory (void) 
{ 
  Suite *suite = suite_create("memory");
  TCase *tcase = tcase_create("memory");


  tcase_add_test( tcase, test_memory_safe_malloc );

#ifdef TRACE_MEMORY

  tcase_add_test( tcase, test_memory_MemTrace_MemInfoList_create   );
  tcase_add_test( tcase, test_memory_MemTrace_MemInfoNode_create   );
  tcase_add_test( tcase, test_memory_MemTrace_MemInfoList_append_1 );
  tcase_add_test( tcase, test_memory_MemTrace_MemInfoList_append_2 );
  tcase_add_test( tcase, test_memory_MemTrace_MemInfoList_get      );
  tcase_add_test( tcase, test_memory_MemTrace_MemInfoList_remove   );
  tcase_add_test( tcase, test_memory_MemTrace_init                 );
  tcase_add_test( tcase, test_memory_MemTrace_getAllocs            );
  tcase_add_test( tcase, test_memory_MemTrace_getNumUnmatchedFrees );
  tcase_add_test( tcase, test_memory_MemTrace_reset                );

#endif

  suite_add_tcase(suite, tcase);

  return suite;
}
