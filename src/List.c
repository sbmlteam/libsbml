/**
 * Filename    : List.c
 * Description : Generic (void *) List for C structs on the heap
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
#include "sbml/List.h"


#ifdef DEBUG

/**
 * Reports a message to stderr if n is out of range in either List_get or
 * List_remove.
 */
#define REPORT_LIST_RANGE_ERROR(function, n, size)                \
  fprintf( stderr,                                                \
           "List.c: %s(List_t *list, n = %d): "                   \
           "Warning: n must be less-than List_size(list) (%d).\n" \
           "Returning NULL.\n",                                   \
           function,                                              \
           n,                                                     \
           size )

#else

#define REPORT_LIST_RANGE_ERROR(function, n, size)

#endif


/**
 * Creates a new List and returns a pointer to it.
 */
List_t *
List_create (void)
{
  List_t *list;


  list       = (List_t *) safe_calloc( 1, sizeof(List_t) );
  list->size = 0;

  return list;
}


/**
 * Frees the given List.
 *
 * This function does not free List items.  It frees only the List_t
 * structure and its constituent ListNode_t structures (if any).
 *
 * Presumably, you either i) have pointers to the individual list items
 * elsewhere in your program and you want to keep them around for awhile
 * longer or ii) the list has no items (List_size(list) == 0).  If neither
 * are true, try List_freeItems() instead.
 */
void
List_free (List_t *list)
{
  ListNode_t *node;
  ListNode_t *temp;


  if (list == NULL) return;

  node = list->head;

  while (node != NULL)
  {
    temp = node;
    node = node->next;

    safe_free(temp);
  }

  safe_free(list);
}


/**
 * Adds item to this List.
 */
void
List_add (List_t *list, void *item)
{
  ListNode_t *node;


  node       = (ListNode_t *) safe_calloc( 1, sizeof(ListNode_t) );
  node->item = item;
  node->next = NULL;

  if (list->head == NULL)
  {
    list->head = node;
    list->tail = node;
  }
  else
  {
    list->tail->next = node;
    list->tail       = node;
  }

  list->size++;
}


/**
 * Returns the nth item in this List.  If n > List_size(list) returns
 * NULL.
 */
void *
List_get (List_t *list, unsigned int n)
{
  unsigned int  size = list->size;
  ListNode_t   *node = list->head;


  if (n >= size)
  {
    REPORT_LIST_RANGE_ERROR("List_get", n, size);
    return NULL;
  }

  /**
   * Special case to retreive last item in the list without a full list
   * traversal.
   */
  if (n == (size - 1))
  {
    node = list->tail;
  }
  else
  {
    /* Point node to the nth item. */
    while (n-- > 0)
    {
      node = node->next;
    }
  }

  return node->item;
}


/**
 * Removes the nth item from this List and returns a pointer to it.  If n >
 * List_size(list) returns NULL.
 */
void *
List_remove (List_t *list, unsigned int n)
{
  void       *item;
  ListNode_t *prev;
  ListNode_t *temp;
  ListNode_t *next;


  if (n >= list->size)
  {
    REPORT_LIST_RANGE_ERROR("List_remove", n, list->size);
    return NULL;
  }

  /**
   * temp = node to be removed
   * prev = node before temp (or NULL if temp == list->head)
   * next = node after  temp (or NULL if temp == list->tail)
   */
  prev = NULL;
  temp = list->head;
  next = temp->next;

  /**
   * Point temp to nth item.
   */
  while (n-- > 0)
  {
    prev = temp;
    temp = temp->next;
    next = temp->next;
  }

  /**
   * If the first item in the list is being removed, only list->head needs
   * to be updated to remove temp.  Otherwise, prev->next must "forget"
   * about temp and point to next instead.
   */
  if (list->head == temp)
  {
    list->head = next;
  }
  else
  {
    prev->next = next;
  }

  /**
   * Regardless of the restructuring above, if the last item in the list
   * has been removed, update list->tail.
   */
  if (list->tail == temp)
  {
    list->tail = prev;
  }

  item = temp->item;
  safe_free(temp);

  list->size--;

  return item;
}


/**
 * Returns the number of elements in this List.
 */
unsigned int
List_size (const List_t *list)
{
  return list->size;
}
