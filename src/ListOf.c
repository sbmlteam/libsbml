/**
 * Filename    : ListOf.c
 * Description : Wraps List_t and "inherits" from SBase
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-04-28
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


#include "sbml/ListOf.h"
#include "sbml/SBMLTypes.h"


void ListOf_freeItems (ListOf_t *lo);


/**
 * Creates a new ListOf and returns a pointer to it.
 */
LIBSBML_EXTERN
ListOf_t *
ListOf_create (void)
{
  ListOf_t *lo = (ListOf_t *) safe_calloc(1, sizeof(ListOf_t));


  SBase_init((SBase_t *) lo, SBML_LIST_OF);
  lo->items = List_create();

  return lo;
}


/**
 * Frees the given ListOf and its constituent items.
 *
 * This function assumes each item in the list is derived from SBase.
 */
LIBSBML_EXTERN
void
ListOf_free (ListOf_t *lo)
{
  if (lo == NULL) return;


  SBase_clear( (SBase_t *) lo );

  ListOf_freeItems(lo);
  List_free(lo->items);

  safe_free(lo);
}


/**
 * Removes and Frees each item in this List.
 *
 * This function assumes each item in the list is derived from SBase.
 */
void
ListOf_freeItems (ListOf_t *lo)
{
  SBase_t      *item;
  unsigned int  size = List_size(lo->items);



  while (size--)
  {
    item = (SBase_t *) List_remove(lo->items, 0);

    switch (item->typecode)
    {
      case SBML_COMPARTMENT:
        Compartment_free( (Compartment_t *) item );
        break;

      case SBML_DOCUMENT:
        SBMLDocument_free( (SBMLDocument_t *) item );
        break;

      case SBML_EVENT:
        Event_free( (Event_t *) item );
        break;

      case SBML_EVENT_ASSIGNMENT:
        EventAssignment_free( (EventAssignment_t *) item );
        break;

      case SBML_FUNCTION_DEFINITION:
        FunctionDefinition_free( (FunctionDefinition_t *) item  );
        break;

      case SBML_KINETIC_LAW:
        KineticLaw_free( (KineticLaw_t *) item );
        break;

      case SBML_LIST_OF:
        ListOf_free( (ListOf_t *) item );
        break;

      case SBML_MODEL:
        Model_free( (Model_t *) item );
        break;

      case SBML_PARAMETER:
        Parameter_free( (Parameter_t *) item );
        break;

      case SBML_REACTION:
        Reaction_free( (Reaction_t *) item );
        break;

      case SBML_SPECIES:
        Species_free( (Species_t *) item );
        break;

      case SBML_SPECIES_REFERENCE:
        SpeciesReference_free( (SpeciesReference_t *) item );
        break;

      case SBML_MODIFIER_SPECIES_REFERENCE:
        ModifierSpeciesReference_free( (ModifierSpeciesReference_t *) item );
        break;

      case SBML_UNIT_DEFINITION:
        UnitDefinition_free( (UnitDefinition_t *) item );
        break;

      case SBML_UNIT:
        Unit_free( (Unit_t *) item );
        break;

      case SBML_ALGEBRAIC_RULE:
        AlgebraicRule_free( (AlgebraicRule_t *) item );
        break;

      case SBML_ASSIGNMENT_RULE:
        AssignmentRule_free( (AssignmentRule_t *) item );
        break;

      case SBML_RATE_RULE:
        RateRule_free( (RateRule_t *) item );
        break;

      case SBML_SPECIES_CONCENTRATION_RULE:
        SpeciesConcentrationRule_free( (SpeciesConcentrationRule_t *) item );
        break;

      case SBML_COMPARTMENT_VOLUME_RULE:
        CompartmentVolumeRule_free( (CompartmentVolumeRule_t *) item  );
        break;

      case SBML_PARAMETER_RULE:
        ParameterRule_free( (ParameterRule_t *) item );
        break;

      default:
        safe_free( (void *) item );
        break;
    }
  }
}


/**
 * Adds item to the end of this List.
 */
LIBSBML_EXTERN
void
ListOf_append (ListOf_t *lo, void *item)
{
  List_add(lo->items, item);
}


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
ListOf_countIf (const ListOf_t *lo, ListItemPredicate predicate)
{
  return List_countIf(lo->items, predicate);
}


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
ListOf_find ( const ListOf_t     *lo,
              const void         *item1,
              ListItemComparator comparator )
{
  return List_find(lo->items, item1, comparator);
}


/**
 * Returns the nth item in this List.  If n > ListOf_getNumItems(list)
 * returns NULL.
 */
LIBSBML_EXTERN
void *
ListOf_get (const ListOf_t *lo, unsigned int n)
{
  return List_get(lo->items, n);
}


/**
 * Returns the number of items in this List.
 */
LIBSBML_EXTERN
unsigned int
ListOf_getNumItems (const ListOf_t *lo)
{
  return List_size(lo->items);
}


/**
 * Adds item to the beginning of this ListOf.
 */
LIBSBML_EXTERN
void
ListOf_prepend (ListOf_t *lo, void *item)
{
  List_prepend(lo->items, item);
}


/**
 * Removes the nth item from this List and returns a pointer to it.  If n >
 * ListOf_getNumItems(list) returns NULL.
 */
LIBSBML_EXTERN
void *
ListOf_remove (ListOf_t *lo, unsigned int n)
{
  return List_remove(lo->items, n);
}
