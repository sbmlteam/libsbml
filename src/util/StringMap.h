/**
 * \file    StringMap.h
 * \brief   Generic (void *) StringMap
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#ifndef StringMap_h
#define StringMap_h


#include <sbml/common/extern.h>
#include <sbml/util/List.h>


BEGIN_C_DECLS


typedef struct item
{
  char *key;
  void *value;
} StringMapItem_t;


typedef struct
{
  unsigned int     size;
  unsigned int     capacity;
  List_t         **itemLists;
} StringMap_t;


/**
 * Creates a new StringMap and returns a pointer to it.
 */
LIBSBML_EXTERN
StringMap_t *
StringMap_create (void);

/**
 * Creates a new StringMapItem and returns a pointer to it.
 */
StringMapItem_t *
StringMapItem_create (const char *key, void *value);

/**
 * Returns nonzero iff key exists.
 */
LIBSBML_EXTERN
int
StringMap_exists (StringMap_t *map, const char *key);

/**
 * Frees the given StringMap.  Does not free the values.
 */
LIBSBML_EXTERN
void
StringMap_free (StringMap_t *map);


/**
 * Returns the value of the item corresponding to the given key.
 * If no such item exists, returns NULL.
 */
LIBSBML_EXTERN
void *
StringMap_get (const StringMap_t *map, const char *key);

/**
 * @return the number of items the Stack is capable of holding before
 * it will (automatically) double its storage capacity.
 */
LIBSBML_EXTERN
unsigned int
StringMap_capacity (const StringMap_t *map);

/**
 * Associates the specified value with the specified key.
 */
LIBSBML_EXTERN
void
StringMap_put (StringMap_t *map, const char *key, void *value);

/**
 * Removes the specified key.  Does nothing if the key does not exist.
 */
LIBSBML_EXTERN
void
StringMap_remove (StringMap_t *map, const char *key);

/**
 * Returns the number of elements in this StringMap.
 */
LIBSBML_EXTERN
unsigned int
StringMap_size (const StringMap_t *map);


END_C_DECLS


#endif  /** StringMap_h **/
