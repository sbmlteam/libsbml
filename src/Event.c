/**
 * Filename    : Event.c
 * Description : SBML Event
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-03
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


#include "sbml/common.h"
#include "sbml/Event.h"


/**
 * Creates a new Event and returns a pointer to it.
 */
LIBSBML_EXTERN
Event_t *
Event_create (void)
{
  Event_t *e;


  e = (Event_t *) safe_calloc(1, sizeof(Event_t));
  SBase_init((SBase_t *) e, SBML_EVENT);

  e->eventAssignment = ListOf_create();

  return e;
}


/**
 * Creates a new Event with the given id and trigger and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   e = Event_create();
 *   Event_setId(e, id); Event_setTrigger(e, trigger);
 */
LIBSBML_EXTERN
Event_t *
Event_createWith (const char *sid, ASTNode_t *trigger)
{
  Event_t *e = Event_create();


  Event_setId     ( e, sid     );
  Event_setTrigger( e, trigger );

  return e;
}


/**
 * Frees the given Event.
 */
LIBSBML_EXTERN
void
Event_free (Event_t *e)
{
  if (e == NULL) return;


  SBase_clear((SBase_t *) e);

  ListOf_free(e->eventAssignment);

  safe_free(e->id);
  safe_free(e->name);
  safe_free(e->timeUnits);

  ASTNode_free(e->trigger);
  ASTNode_free(e->delay);

  safe_free(e);
}


/**
 * @return the id of this Event.
 */
LIBSBML_EXTERN
const char *
Event_getId (const Event_t *e)
{
  return e->id;
}


/**
 * @return the name of this Event.
 */
LIBSBML_EXTERN
const char *
Event_getName (const Event_t *e)
{
  return e->name;
}


/**
 * @return the trigger of this Event.
 */
LIBSBML_EXTERN
const ASTNode_t *
Event_getTrigger (const Event_t *e)
{
  return e->trigger;
}


/**
 * @return the delay of this Event.
 */
LIBSBML_EXTERN
const ASTNode_t *
Event_getDelay (const Event_t *e)
{
  return e->delay;
}


/**
 * @return the timeUnits of this Event
 */
LIBSBML_EXTERN
const char *
Event_getTimeUnits (const Event_t *e)
{
  return e->timeUnits;
}


/**
 * @return 1 if the id of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetId (const Event_t *e)
{
  return (e->id != NULL);
}


/**
 * @return 1 if the name of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetName (const Event_t *e)
{
  return (e->name != NULL);
}

/**
 * @return 1 if the trigger of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetTrigger (const Event_t *e)
{
  return (e->trigger != NULL);
}


/**
 * @return 1 if the delay of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetDelay (const Event_t *e)
{
  return (e->delay != NULL);
}


/**
 * @return 1 if the timeUnits of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetTimeUnits (const Event_t *e)
{
  return (e->timeUnits != NULL);
}


/**
 * Sets the id of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event_setId (Event_t *e, const char *sid)
{
  if (e->id == sid) return;


  if (e->id != NULL)
  {
    safe_free(e->id);
  }

  e->id = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the name of this Event to a copy of string.
 */
LIBSBML_EXTERN
void
Event_setName (Event_t *e, const char *string)
{
  if (e->name == string) return;


  if (e->name != NULL)
  {
    safe_free(e->name);
  }

  e->name = (string == NULL) ? NULL : safe_strdup(string);
}


/**
 * Sets the trigger of this Event to the given ASTNode.
 *
 * The node <b>is not copied</b> and this Event <b>takes ownership</b> of
 * it; i.e. subsequent calls to this function or a call to Event_free()
 * will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
Event_setTrigger (Event_t *e, ASTNode_t *math)
{
  if (e->trigger == math) return;


  if (e->trigger != NULL)
  {
    ASTNode_free(e->trigger);
  }

  e->trigger = math;
}


/**
 * Sets the delay of this Event to the given ASTNode.
 *
 * The node <b>is not copied</b> and this Event <b>takes ownership</b> of
 * it; i.e. subsequent calls to this function or a call to Event_free()
 * will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
Event_setDelay (Event_t *e, ASTNode_t *math)
{
  if (e->delay == math) return;


  if (e->delay != NULL)
  {
    ASTNode_free(e->delay);
  }

  e->delay = math;
}


/**
 * Sets the timeUnits of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event_setTimeUnits (Event_t *e, const char *sid)
{
  if (e->timeUnits == sid) return;


  if (e->timeUnits != NULL)
  {
    safe_free(e->timeUnits);
  }

  e->timeUnits = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Unsets the id of this Event.  This is equivalent to:
 * safe_free(e->id); e->id = NULL;
 */
LIBSBML_EXTERN
void
Event_unsetId (Event_t *e)
{
  safe_free(e->id);
  e->id = NULL;
}


/**
 * Unsets the name of this Event.  This is equivalent to:
 * safe_free(e->name); e->name = NULL;
 */
LIBSBML_EXTERN
void
Event_unsetName (Event_t *e)
{
  safe_free(e->name);
  e->name = NULL;
}


/**
 * Unsets the delay of this Event.  This is equivalent to:
 * ASTNode_free(e->delay); e->delay = NULL;
 */
LIBSBML_EXTERN
void
Event_unsetDelay (Event_t *e)
{
  ASTNode_free(e->delay);
  e->delay = NULL;
}


/**
 * Unsets the timeUnits of this Event.  This is equivalent to:
 * safe_free(e->timeUnits); e->timeUnits = NULL;
 */
LIBSBML_EXTERN
void
Event_unsetTimeUnits (Event_t *e)
{
  safe_free(e->timeUnits);
  e->timeUnits = NULL;
}


/**
 * Appends the given EventAssignment to this Event.
 */
LIBSBML_EXTERN
void
Event_addEventAssignment (Event_t *e, EventAssignment_t *ea)
{
  ListOf_append(e->eventAssignment, ea);
}


/**
 * @return the list of EventAssignments for this Event.
 */
LIBSBML_EXTERN
ListOf_t *
Event_getListOfEventAssignments (const Event_t *e)
{
  return e->eventAssignment;
}


/**
 * @return the nth EventAssignment of this Event.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_getEventAssignment (const Event_t *e, unsigned int n)
{
  return (EventAssignment_t *) ListOf_get(e->eventAssignment, n);
}


/**
 * @return the number of EventAssignments in this Event.
 */
LIBSBML_EXTERN
unsigned int
Event_getNumEventAssignments (const Event_t *e)
{
  return ListOf_getNumItems(e->eventAssignment);
}


/**
 * The EventIdCmp function compares the string sid to e->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than e->id.
 * Returns -1 if either sid or e->id is NULL.
 */
LIBSBML_EXTERN
int
EventIdCmp (const char *sid, const Event_t *e)
{
  int result = -1;


  if (sid != NULL && e->id != NULL)
  {
    result = strcmp(sid, e->id);
  }

  return result;
}
