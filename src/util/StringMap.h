/**
 * \file    StringMap.h
 * \brief   Generic (void *) StringMap
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and
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
 *     Ben Bornstein and Ben Kovitz
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


#ifndef StringMap_h
#define StringMap_h


#include "../common/extern.h"
#include "List.h"


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
