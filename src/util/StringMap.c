/**
 * \file    StringMap.c
 * \brief   Generic (void *) StringMap for C structs on the heap
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


#include <sbml/common/common.h>

#include "List.h"
#include "StringMap.h"


#define INITIAL_CAPACITY 10


unsigned int
StringMap_hashFunction (const char *str);

unsigned int
StringMap_getHashIndex (const StringMap_t *map, const char *key);

StringMapItem_t *
StringMap_findItemInList (List_t *items, const char *key);

StringMapItem_t *
StringMap_findItem (const StringMap_t *map, const char *key);


/**
 * Creates a new StringMap and returns a pointer to it.
 */
LIBSBML_EXTERN
StringMap_t *
StringMap_create (void)
{
  StringMap_t *sm;


  sm            = (StringMap_t *) safe_malloc( sizeof(StringMap_t) );
  sm->itemLists = (List_t **) safe_calloc(INITIAL_CAPACITY, sizeof(List_t *));
  sm->size      = 0;
  sm->capacity  = INITIAL_CAPACITY;

  return sm;
}


/**
 * Creates a new StringMapItem_t and returns a pointer to it.
 */
StringMapItem_t *
StringMapItem_create (const char *key, void *value)
{
  StringMapItem_t *item;


  item        = (StringMapItem_t *) safe_malloc( sizeof(StringMapItem_t) );
  item->key   = safe_strdup(key);
  item->value = value;

  return item;
}


/**
 * Frees a StringMapItem_t including its key.
 */
void
StringMapItem_free (StringMapItem_t *item)
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
StringMap_exists (StringMap_t *map, const char *key)
{
  return StringMap_findItem(map, key) != NULL;
}


/**
 * Finds the StringMapItem_t with given key, or NULL if not found.
 */
StringMapItem_t *
StringMap_findItem (const StringMap_t *map, const char *key)
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
StringMap_findIndexOfItemInList (List_t *list, const char *key)
{
  StringMapItem_t *item;
  unsigned int     i;
  unsigned int     size = List_size(list);


  for (i = 0; i < size; i++)
  {
    item = (StringMapItem_t *) List_get(list, i);
    if ( !strcmp(item->key, key) )
    {
      return (int) i;
    }
  }

  return -1;
}


/**
 * Finds a StringMapItem_t with given key in a List_t.  Returns NULL if not
 * found.
 */
StringMapItem_t *
StringMap_findItemInList (List_t *items, const char *key)
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
StringMap_free (StringMap_t *map)
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
StringMap_get (const StringMap_t *map, const char *key)
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
StringMap_getHashIndex (const StringMap_t *map, const char *key)
{
  return StringMap_hashFunction(key) % map->capacity;
}


/**
 * Grows the capacity of the StringMap.
 */
void
StringMap_grow (StringMap_t *map)
{
  List_t **oldLists = map->itemLists;
  unsigned int oldCapacity = map->capacity;
  unsigned int i, j;


  map->capacity *= 10;
  map->itemLists = (List_t **) safe_calloc(map->capacity, sizeof(List_t*));

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

        StringMapItem_free(oldItem);
      }

      List_free(oldList);
    }
  }

  safe_free(oldLists);
}


unsigned int
StringMap_hashFunction (const char *str)
{
  unsigned char *s    = (unsigned char *) str;
  unsigned int   hash = 5381;
  unsigned char  c    = *s;


  while (c != '\0')
  {
    hash = ((hash << 5) + hash) + c;
    c    = *(s++);
  }

  return hash;
}


/**
 * Associates the specified value with the specified key.  If the key already
 * exists, it gets the new value.
 */
LIBSBML_EXTERN
void
StringMap_put (StringMap_t *map, const char *key, void *value)
{
  unsigned int     index;
  List_t          *list;
  StringMapItem_t *item;


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
  item = StringMap_findItemInList(list, key);

  if (item == NULL)
  {
    item = StringMapItem_create(key, value);
    List_add(list, item);
    map->size++;
  }
  else
  {
    safe_free(item->key);
    item->key   = safe_strdup(key);
    item->value = value;
  }
}


/**
 * Removes the specified key.  Does nothing if the key does not exist.
 */
LIBSBML_EXTERN
void
StringMap_remove (StringMap_t *map, const char *key)
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
StringMap_size (const StringMap_t *map)
{
  return map->size;
}
