/**
 * Filename    : StringMap.c
 * Description : Generic (void *) StringMap for C structs on the heap
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-20
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2004 California Institute of Technology and
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common.h"
#include "sbml/StringMap.h"

#define INITIAL_CAPACITY 10

typedef struct
{
  enum {
    FI_FOUND_KEY,       // found the key we were looking for: index is
                        // the index of that key
    FI_FOUND_EMPTY_SLOT, // didn't find the key, but we found an empty slot
                        // where it could go: index is the index of that
                        // empty slot
    FI_FOUND_NOTHING    // found neither the key nor an empty slot: index
                        // is undefined
  } status;
  unsigned int index;   // see above
} FoundIndex_t;

unsigned long StringMap_hashFunction(const unsigned char *str);


/**
 * Creates a new StringMap and returns a pointer to it.
 */
LIBSBML_EXTERN
StringMap_t *
StringMap_create (void)
{
  StringMap_t *sm = (StringMap_t *) safe_malloc(sizeof(StringMap_t));
  sm->items = 
    (StringMapItem_t *) safe_calloc(INITIAL_CAPACITY, sizeof(StringMapItem_t));
  sm->size     = 0;
  sm->capacity = INITIAL_CAPACITY;
  return sm;
}


/**
 * Creates a new StringMapItem and returns a pointer to it.
 */
StringMapItem_t *
StringMapItem_create (const char *key, void *value)
{
  return 0;
}


/**
 * Finds a key in the hash.
 */
FoundIndex_t
StringMap_findKey (const StringMap_t *map, const char *key)
{
  unsigned int index = StringMap_hashFunction(key) % map->capacity;
  FoundIndex_t result;
  int count;
  const char *itemKey;

  for (count = 0; count < map->capacity; count++)
  {
    itemKey = map->items[index].key;
    if (itemKey == NULL)
    {
      result.status = FI_FOUND_EMPTY_SLOT;
      result.index = index;
      return result;
    }

    if (!strcmp(itemKey, key)) {
      result.status = FI_FOUND_KEY;
      result.index = index;
      return result;
    }

    if (++index >= map->capacity)
    {
      index = 0;  // wrap around
    }
  }

  // if we got here, then the key is not in the hash

  result.status = FI_FOUND_NOTHING;
  return result;
}


/**
 * Frees the StringMap.  Does not free the values.
 */
LIBSBML_EXTERN
void
StringMap_free (StringMap_t *map)
{
  unsigned int n;


  if (map == NULL) return;

  for (n = 0; n < map->capacity; n++)
  {
    safe_free(map->items[n].key);
  }

  safe_free(map->items);
  safe_free(map);
}


/**
 * Returns the value of the item corresponding to the given key.
 * If no such item exists, returns NULL.
 */
LIBSBML_EXTERN
void *
StringMap_get (const StringMap_t *map, const char *key)
{
  FoundIndex_t found = StringMap_findKey(map, key);

  switch (found.status)
  {
    case FI_FOUND_KEY:
      return map->items[found.index].value;

    default:
      return NULL;
  }
}


/**
 * Grows the capacity of the StringMap.
 */
void
StringMap_grow (StringMap_t *map)
{
  StringMapItem_t *oldItems = map->items;
  unsigned int oldCapacity = map->capacity;
  unsigned int index;

  map->capacity *= 10;
  map->items = (StringMapItem_t *) safe_calloc(
    map->capacity, sizeof(StringMapItem_t));

  /* rehash all the old items into the new, larger hash */

  for (index = 0; index < oldCapacity; index++)
  {
    if (oldItems[index].key != NULL)
    {
      FoundIndex_t found = StringMap_findKey(map, oldItems[index].key);
      // assert(found.status == FI_FOUND_EMPTY_SLOT);

      map->items[found.index].key   = oldItems[index].key; // not strdup()!
      map->items[found.index].value = oldItems[index].value;
    }
  }

  safe_free(oldItems);
}


/**
 * Associates the specified value with the specified key.  If the key already
 * exists, it gets the new value.
 */
LIBSBML_EXTERN
void
StringMap_put (StringMap_t *map, const char *key, void *value)
{
  unsigned int n;
  FoundIndex_t found;


  found = StringMap_findKey(map, key);
  switch (found.status)
  {
    case FI_FOUND_NOTHING:
      StringMap_grow(map); // this invalidates found.index
      found = StringMap_findKey(map, key);
      // assert(found.status == FI_FOUND_EMPTY_SLOT);
      // fall through

    case FI_FOUND_EMPTY_SLOT:
      map->size++;
      map->items[found.index].key   = safe_strdup(key); 
      map->items[found.index].value = value;
      break;

    case FI_FOUND_KEY:
      safe_free(map->items[found.index].key);
      map->items[found.index].key   = safe_strdup(key); 
      map->items[found.index].value = value;
      break;
  }
}


/**
 * Returns the number of elements in this StringMap.
 */
LIBSBML_EXTERN
unsigned int
StringMap_size (const StringMap_t *map)
{
  return map->size;
}


unsigned long
StringMap_hashFunction(const unsigned char *str)
{
  unsigned long hash = 5381;
  int c;

  while (c = *str++)
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

  return hash;
}
