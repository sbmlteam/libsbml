/**
 * \file    ListOf.h
 * \author  Wraps List and inherits from SBase
 * \author  SBML Team <sbml-team@caltech.edu>
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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


#ifndef ListOf_h
#define ListOf_h


#include "common/extern.h"
#include "util/List.h"


#ifdef __cplusplus


#include "SBMLTypeCodes.h"
#include "SBase.h"


class SBMLVisitor;


class ListOf : public SBase
{
public:

  /**
   * Creates a new ListOf.
   */
  LIBSBML_EXTERN
  ListOf ();

  /**
   * Destroys the given ListOf and its constituent items.
   */
  LIBSBML_EXTERN
  virtual ~ListOf ();


  /**
   * Accepts the given SBMLVisitor.
   */
  LIBSBML_EXTERN
  void accept (SBMLVisitor& v, SBMLTypeCode_t type) const;

  /**
   * Adds item to the end of this List.
   */
  LIBSBML_EXTERN
  void append (SBase* item);

  /**
   * @return the number of items in this List for which predicate(item)
   * returns true.
   *
   * The typedef for ListItemPredicate is:
   *
   *   int (*ListItemPredicate) (const void *item);
   *
   * where a return value of non-zero represents true and zero represents
   * false.
   */
  LIBSBML_EXTERN
  unsigned int countIf (ListItemPredicate predicate) const;

  /**
   * @return the first occurrence of item1 in this List or NULL if item was
   * not found.  ListItemComparator is a pointer to a function used to find
   * item.  The typedef for ListItemComparator is:
   *
   *   int (*ListItemComparator) (const void *item1, const void *item2);
   *
   * The return value semantics are the same as for strcmp:
   *
   *   -1    item1 <  item2,
   *    0    item1 == item 2
   *    1    item1 >  item2
   */
  LIBSBML_EXTERN
  void* find (const void *item1, ListItemComparator comparator) const;

  /**
   * Removes and deletes each item in this List.
   */
  void
  freeItems ();

  /**
   * @return the nth item in this List.  If n > ListOf.getNumItems()
   * returns 0.
   */
  LIBSBML_EXTERN
  SBase* get (unsigned int n) const;

  /**
   * @return the number of items in this List.
   */
  LIBSBML_EXTERN
  unsigned int getNumItems () const;

  /**
   * Adds item to the beginning of this ListOf.
   */
  LIBSBML_EXTERN
  void prepend (SBase* item);

  /**
   * Removes the nth item from this List and returns a pointer to it.  If n
   * > ListOf.getNumItems() returns 0.
   */
  LIBSBML_EXTERN
  SBase* remove (unsigned int n);


protected:

  List items;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new ListOf and returns a pointer to it.
 */
LIBSBML_EXTERN
ListOf_t *
ListOf_create (void);

/**
 * Frees the given ListOf and its constituent items.
 *
 * This function assumes each item in the list is derived from SBase.
 */
LIBSBML_EXTERN
void
ListOf_free (ListOf_t *lo);

/**
 * Adds item to the end of this List.
 */
LIBSBML_EXTERN
void
ListOf_append (ListOf_t *lo, void *item);

/**
 * @return the number of items in this List for which predicate(item)
 * returns true.
 *
 * The typedef for ListItemPredicate is:
 *
 *   int (*ListItemPredicate) (const void *item);
 *
 * where a return value of non-zero represents true and zero represents
 * false.
 */
unsigned int
ListOf_countIf (const ListOf_t *lo, ListItemPredicate predicate);

/**
 * @return the first occurrence of item1 in this List or NULL if item was
 * not found.  ListItemComparator is a pointer to a function used to find
 * item.  The typedef for ListItemComparator is:
 *
 *   int (*ListItemComparator) (const void *item1, const void *item2);
 *
 * The return value semantics are the same as for strcmp:
 *
 *   -1    item1 <  item2,
 *    0    item1 == item 2
 *    1    item1 >  item2
 */
void *
ListOf_find ( const ListOf_t    *lo,
              const void        *item1,
              ListItemComparator comparator );

/**
 * Returns the nth item in this List.  If n > ListOf_getNumItems(list)
 * returns NULL.
 */
LIBSBML_EXTERN
void *
ListOf_get (const ListOf_t *lo, unsigned int n);

/**
 * Returns the number of items in this List.
 */
LIBSBML_EXTERN
unsigned int
ListOf_getNumItems (const ListOf_t *lo);

/**
 * Adds item to the beginning of this ListOf.
 */
LIBSBML_EXTERN
void
ListOf_prepend (ListOf_t *lo, void *item);

/**
 * Removes the nth item from this List and returns a pointer to it.  If n >
 * ListOf_getNumItems(list) returns NULL.
 */
LIBSBML_EXTERN
void *
ListOf_remove (ListOf_t *lo, unsigned int n);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* ListOf_h */
