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
#include "sbml/list.h"

#define INITIAL_CAPACITY 10

unsigned int StringMap_hashFunction (const char *str);
unsigned int StringMap_getHashIndex(StringMap_t *map, const char *key);
StringMapItem_t *StringMap_findItemInList (List_t *items, const char *key);
StringMapItem_t *StringMap_findItem (StringMap_t *map, const char *key);

/**
 * Creates a new StringMap and returns a pointer to it.
 */
LIBSBML_EXTERN
StringMap_t *
StringMap_create(void)
{
  StringMap_t *sm = (StringMap_t *) safe_malloc(sizeof(StringMap_t));
  sm->itemLists = 
    (List_t **) safe_calloc(INITIAL_CAPACITY, sizeof(List_t *));
  sm->size     = 0;
  sm->capacity = INITIAL_CAPACITY;
  return sm;
}


/**
 * Creates a new StringMapItem_t and returns a pointer to it.
 */
StringMapItem_t *
StringMapItem_create(const char *key, void *value)
{
  StringMapItem_t *result =
      (StringMapItem_t *) safe_malloc(sizeof(StringMapItem_t));
  result->key = key;
  result->value = value;

  return result;
}

/**
 * Frees a StringMapItem_t including its key.
 */
void
StringMapItem_free(StringMapItem_t *item)
{
   if (item)
   {
      safe_free(item->key);
   }
   safe_free(item);
}


/**
 * Returns nonzero iff key exists.
 */
LIBSBML_EXTERN
int
StringMap_exists(StringMap_t *map, const char *key)
{
  return StringMap_findItem(map, key) != NULL;
}


/**
 * Finds the StringMapItem_t with given key, or NULL if not found.
 */
StringMapItem_t *
StringMap_findItem(StringMap_t *map, const char *key)
{
  unsigned int index = StringMap_getHashIndex(map, key);
  List_t *list;


  list = map->itemLists[index];
  if (list)
  {
    return (StringMapItem_t *)StringMap_findItemInList(list, key);
  }

  return NULL;
}


/**
 * Returns the index of key in list.  If key is not in list, returns -1.
 */

int
StringMap_findIndexOfItemInList(List_t *list, const char *key)
{
  int i;


  for (i = 0; i < List_size(list); i++)
  {
    StringMapItem_t *item = (StringMapItem_t *)List_get(list, i);
    if (!strcmp(item->key, key))
    {
      return i;
    }
  }

  return -1;
}


/**
 * Finds a StringMapItem_t with given key in a List_t.  Returns NULL if not
 * found.
 */
StringMapItem_t *
StringMap_findItemInList(List_t *items, const char *key)
{
  unsigned int i;


  for (i = 0; i < List_size(items); i++)
  {
    StringMapItem_t *item = (StringMapItem_t *) List_get(items, i);
    if (!strcmp(item->key, key))
    {
      return item;
    }
  }

  return NULL;
}


/**
 * Frees the StringMap.  Does not free the values.
 */
LIBSBML_EXTERN
void
StringMap_free(StringMap_t *map)
{
  unsigned int n, i;


  if (map == NULL) return;

  for (n = 0; n < map->capacity; n++)
  {
    List_t *items = map->itemLists[n];
    if (items)
    {
      for (i = 0; i < List_size(items); i++)
      {
        StringMapItem_t *item = (StringMapItem_t *) List_get(items, i);
        if (item)
        {
          safe_free(item->key);
          safe_free(item);
        }
      }
      List_free(items);
    }
  }

  safe_free(map->itemLists);
  safe_free(map);
}


/**
 * Returns the value of the item corresponding to the given key.
 * If no such item exists, returns NULL.  Note that an item with
 * a NULL value also returns NULL.
 */
LIBSBML_EXTERN
void *
StringMap_get(const StringMap_t *map, const char *key)
{
  StringMapItem_t *item = StringMap_findItem(map, key);


  if (item)
  {
    return item->value;
  }

  return NULL;
}


/**
 * Returns the index of key in map.  The index is the index of the hash
 * bucket; this index can produce hash collisions.
 */
unsigned int
StringMap_getHashIndex(StringMap_t *map, const char *key)
{
  return StringMap_hashFunction(key) % map->capacity;
}

/**
 * Grows the capacity of the StringMap.
 */
void
StringMap_grow(StringMap_t *map)
{
  List_t **oldLists = map->itemLists;
  unsigned int oldCapacity = map->capacity;
  unsigned int i, j;


  map->capacity *= 10;
  map->itemLists = (List_t **) safe_calloc(map->capacity, sizeof(List_t));

  /* rehash all the old items into the new, larger hash */

  for (i = 0; i < oldCapacity; i++) /* for each linked list */
  {
    List_t *oldList = oldLists[i];

    if (oldList != NULL)
    {
      for (j = 0; j < List_size(oldList); j++) {
        StringMapItem_t *oldItem = (StringMapItem_t *) List_get(oldList, j);
        StringMapItem_t *newItem = 
            StringMapItem_create(oldItem->key, oldItem->value);
        unsigned int index = StringMap_getHashIndex(map, oldItem->key);
        List_t *itemList = map->itemLists[index];
        
        if (itemList == NULL)
        {
          itemList = map->itemLists[index] = List_create();
        }
        List_add(itemList, newItem);

        safe_free(oldItem);
      }

      List_free(oldList);
    }
  }

  safe_free(oldLists);
}


unsigned int
StringMap_hashFunction(const char *str_)
{
  const unsigned char *str = str_;
  unsigned int hash = 5381;
  int c;

  while (c = *str++)
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

  return hash;
}


/**
 * Associates the specified value with the specified key.  If the key already
 * exists, it gets the new value.
 */
LIBSBML_EXTERN
void
StringMap_put(StringMap_t *map, const char *key, void *value)
{
  unsigned int index;
  List_t *list;


  if (map->size >= map->capacity)
  {
    StringMap_grow(map);
  }
  index = StringMap_getHashIndex(map, key);

  if (map->itemLists[index] == NULL)
  {
    map->itemLists[index] = List_create();
  }
  list = map->itemLists[index];
  
  StringMapItem_t *item = StringMap_findItemInList(list, key);

  if (item == NULL)
  {
    StringMapItem_t *item = StringMapItem_create(safe_strdup(key), value);
    List_add(list, item);
    map->size++;
  }
  else
  {
    safe_free(item->key);
    item->key = safe_strdup(key);
    item->value = value;
  }
}


/**
 * Removes the specified key.  Does nothing if the key does not exist.
 */
LIBSBML_EXTERN
void
StringMap_remove(StringMap_t *map, const char *key)
{
  unsigned int index = StringMap_getHashIndex(map, key);
  List_t *list;


  list = map->itemLists[index];
  if (list)
  {
    int itemIndex = StringMap_findIndexOfItemInList(list, key);
    if (itemIndex >= 0)
    {
      StringMapItem_t *item = (StringMapItem_t *) List_get(list, itemIndex);
      safe_free(item->key);
      safe_free(item);
      List_remove(list, itemIndex);
      map->size--;
    }
  }
}


/**
 * Returns the number of elements in this StringMap.
 */
LIBSBML_EXTERN
unsigned int
StringMap_size(const StringMap_t *map)
{
  return map->size;
}
