/**
 * Filename    : Event.cpp
 * Description : SBML Event
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/FormulaParser.h"

#include "sbml/Event.h"
#include "sbml/Event.hpp"


/**
 * Creates a new Event, optionally with its id, trigger and delay
 * attribute set.  Trigger and delay may be specified as infix formula
 * strings.
 */
LIBSBML_EXTERN
Event::Event (   const std::string&  id
               , const std::string&  trigger_
               , const std::string&  delay_   ) :
    SBase   ()
  , id      ( id   )
  , trigger ( NULL )
  , delay   ( NULL )
{
  init(SBML_EVENT);

  if ( !trigger_.empty() )
  {
    setTrigger( (ASTNode*) SBML_parseFormula( trigger_.c_str() ) );
  }

  if ( !delay_.empty() )
  {
    setDelay( (ASTNode*) SBML_parseFormula( delay_.c_str() ) );
  }
}


/**
 * Creates a new Event with an id and trigger and (optionally) delay
 * attributes set.
 */
LIBSBML_EXTERN
Event::Event (   const std::string&  id
               , ASTNode*            trigger
               , ASTNode*            delay   ) :
    SBase   ()
  , id      ( id      )
  , trigger ( trigger )
  , delay   ( delay   )
{
  init(SBML_EVENT);
}


/**
 * Destroys this Event.
 */
LIBSBML_EXTERN
Event::~Event ()
{
  delete trigger;
  delete delay;
}


/**
 * @return the id of this Event.
 */
LIBSBML_EXTERN
const std::string&
Event::getId () const
{
  return id;
}


/**
 * @return the name of this Event.
 */
LIBSBML_EXTERN
const std::string&
Event::getName () const
{
  return name;
}


/**
 * @return the trigger of this Event.
 */
LIBSBML_EXTERN
const ASTNode*
Event::getTrigger () const
{
  return trigger;
}


/**
 * @return the delay of this Event.
 */
LIBSBML_EXTERN
const ASTNode*
Event::getDelay () const
{
  return delay;
}


/**
 * @return the timeUnits of this Event
 */
LIBSBML_EXTERN
const std::string&
Event::getTimeUnits () const
{
  return timeUnits;
}


/**
 * @return true if the id of this Event has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Event::isSetId () const
{
  return ! id.empty();
}


/**
 * @return true if the name of this Event has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Event::isSetName () const
{
  return ! name.empty();
}


/**
 * @return true if the trigger of this Event has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Event::isSetTrigger () const
{
  return (trigger != NULL);
}


/**
 * @return true if the delay of this Event has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Event::isSetDelay () const
{
  return (delay != NULL);
}


/**
 * @return true if the timeUnits of this Event has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Event::isSetTimeUnits () const
{
  return ! timeUnits.empty();
}


/**
 * Sets the id of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event::setId (const std::string& sid)
{
  id = sid;
}


/**
 * Sets the name of this Event to a copy of string.
 */
LIBSBML_EXTERN
void
Event::setName (const std::string& string)
{
  name = string;
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
Event::setTrigger (ASTNode* math)
{
  if (trigger == math) return;



  delete trigger;
  trigger = math;
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
Event::setDelay (ASTNode* math)
{
  if (delay == math) return;


  delete delay;
  delay = math;
}


/**
 * Sets the timeUnits of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event::setTimeUnits (const std::string& sid)
{
  timeUnits = sid;
}


/**
 * Unsets the id of this Event.
 */
LIBSBML_EXTERN
void
Event::unsetId ()
{
  id.erase();

}


/**
 * Unsets the name of this Event.
 */
LIBSBML_EXTERN
void
Event::unsetName ()
{
  name.erase();
}


/**
 * Unsets the delay of this Event.
 */
LIBSBML_EXTERN
void
Event::unsetDelay ()
{
  delete delay;
  delay = NULL;
}


/**
 * Unsets the timeUnits of this Event.
 */
LIBSBML_EXTERN
void
Event::unsetTimeUnits ()
{
  timeUnits.erase();
}


/**
 * Appends the given EventAssignment to this Event.
 */
LIBSBML_EXTERN
void
Event::addEventAssignment (EventAssignment& ea)
{
  eventAssignment.append(&ea);
}


/**
 * @return the list of EventAssignments for this Event.
 */
LIBSBML_EXTERN
ListOf&
Event::getListOfEventAssignments ()
{
  return eventAssignment;
}


/**
 * @return the nth EventAssignment of this Event.
 */
LIBSBML_EXTERN
EventAssignment*
Event::getEventAssignment (unsigned int n) const
{
  return static_cast<EventAssignment*>( eventAssignment.get(n) );
}


/**
 * @return the number of EventAssignments in this Event.
 */
LIBSBML_EXTERN
unsigned int
Event::getNumEventAssignments () const
{
  return eventAssignment.getNumItems();
}




/**
 * Creates a new Event and returns a pointer to it.
 */
LIBSBML_EXTERN
Event_t *
Event_create (void)
{
  return new(std::nothrow) Event;
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
  ASTNode* x = static_cast<ASTNode*>(trigger);
  return new(std::nothrow) Event(sid ? sid : "", x);
}


/**
 * Frees the given Event.
 */
LIBSBML_EXTERN
void
Event_free (Event_t *e)
{
  delete static_cast<Event*>(e);
}


/**
 * @return the id of this Event.
 */
LIBSBML_EXTERN
const char *
Event_getId (const Event_t *e)
{
  const Event* x = static_cast<const Event*>(e);


  return x->isSetId() ? x->getId().c_str() : NULL;
}


/**
 * @return the name of this Event.
 */
LIBSBML_EXTERN
const char *
Event_getName (const Event_t *e)
{
  const Event* x = static_cast<const Event*>(e);


  return x->isSetName() ? x->getName().c_str() : NULL;
}


/**
 * @return the trigger of this Event.
 */
LIBSBML_EXTERN
const ASTNode_t *
Event_getTrigger (const Event_t *e)
{
  return static_cast<const Event*>(e)->getTrigger();
}


/**
 * @return the delay of this Event.
 */
LIBSBML_EXTERN
const ASTNode_t *
Event_getDelay (const Event_t *e)
{
  return static_cast<const Event*>(e)->getDelay();
}


/**
 * @return the timeUnits of this Event
 */
LIBSBML_EXTERN
const char *
Event_getTimeUnits (const Event_t *e)
{
  const Event* x = static_cast<const Event*>(e);


  return x->isSetTimeUnits() ? x->getTimeUnits().c_str() : NULL;
}


/**
 * @return 1 if the id of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetId (const Event_t *e)
{
  return (int) static_cast<const Event*>(e)->isSetId();
}


/**
 * @return 1 if the name of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetName (const Event_t *e)
{
  return (int) static_cast<const Event*>(e)->isSetName();
}


/**
 * @return 1 if the trigger of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetTrigger (const Event_t *e)
{
  return (int) static_cast<const Event*>(e)->isSetTrigger();
}


/**
 * @return 1 if the delay of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetDelay (const Event_t *e)
{
  return (int) static_cast<const Event*>(e)->isSetDelay();
}


/**
 * @return 1 if the timeUnits of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetTimeUnits (const Event_t *e)
{
  return (int) static_cast<const Event*>(e)->isSetTimeUnits();
}


/**
 * Sets the id of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event_setId (Event_t *e, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Event*>(e)->unsetId();
  }
  else
  {
    static_cast<Event*>(e)->setId(sid);
  }
}


/**
 * Sets the name of this Event to a copy of string.
 */
LIBSBML_EXTERN
void
Event_setName (Event_t *e, const char *string)
{
  if (string == NULL)
  {
    static_cast<Event*>(e)->unsetName();
  }
  else
  {
    static_cast<Event*>(e)->setName(string);
  }
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
  static_cast<Event*>(e)->setTrigger( static_cast<ASTNode*>(math) );
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
  static_cast<Event*>(e)->setDelay( static_cast<ASTNode*>(math) );
}


/**
 * Sets the timeUnits of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event_setTimeUnits (Event_t *e, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Event*>(e)->unsetTimeUnits();
  }
  else
  {
    static_cast<Event*>(e)->setTimeUnits(sid);
  }
}


/**
 * Unsets the id of this Event.  This is equivalent to:
 * safe_free(e->id); e->id = NULL;
 */
LIBSBML_EXTERN
void
Event_unsetId (Event_t *e)
{
  static_cast<Event*>(e)->unsetId();
}


/**
 * Unsets the name of this Event.  This is equivalent to:
 * safe_free(e->name); e->name = NULL;
 */
LIBSBML_EXTERN
void
Event_unsetName (Event_t *e)
{
  static_cast<Event*>(e)->unsetName();
}


/**
 * Unsets the delay of this Event.  This is equivalent to:
 * ASTNode_free(e->delay); e->delay = NULL;
 */
LIBSBML_EXTERN
void
Event_unsetDelay (Event_t *e)
{
  static_cast<Event*>(e)->unsetDelay();
}


/**
 * Unsets the timeUnits of this Event.  This is equivalent to:
 * safe_free(e->timeUnits); e->timeUnits = NULL;
 */
LIBSBML_EXTERN
void
Event_unsetTimeUnits (Event_t *e)
{
  static_cast<Event*>(e)->unsetTimeUnits();
}


/**
 * Appends the given EventAssignment to this Event.
 */
LIBSBML_EXTERN
void
Event_addEventAssignment (Event_t *e, EventAssignment_t *ea)
{
  if (ea != NULL)
  {
    EventAssignment* x = static_cast<EventAssignment*>(ea);

  
    static_cast<Event*>(e)->addEventAssignment(*x);
  }
}


/**
 * @return the list of EventAssignments for this Event.
 */
LIBSBML_EXTERN
ListOf_t *
Event_getListOfEventAssignments (Event_t *e)
{
  return (ListOf_t *) &
  static_cast<Event*>(e)->getListOfEventAssignments();
}


/**
 * @return the nth EventAssignment of this Event.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_getEventAssignment (const Event_t *e, unsigned int n)
{
  return static_cast<const Event*>(e)->getEventAssignment(n);
  
}


/**
 * @return the number of EventAssignments in this Event.
 */
LIBSBML_EXTERN
unsigned int
Event_getNumEventAssignments (const Event_t *e)
{
  return static_cast<const Event*>(e)->getNumEventAssignments();
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


  if (sid != NULL && Event_isSetId(e))
  {
    result = strcmp(sid, Event_getId(e));
  }

  return result;
}
