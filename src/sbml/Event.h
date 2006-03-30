/**
 * \file    Event.h
 * \brief   SBML Event
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
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


#ifndef Event_h
#define Event_h


#include "common/extern.h"


#ifdef __cplusplus


#include <string>

#include "SBase.h"
#include "ListOf.h"
#include "EventAssignment.h"


class ASTNode;
class SBMLVisitor;


class LIBSBML_EXTERN Event : public SBase
{
public:

  /**
   * Creates a new Event, optionally with its id, trigger and delay
   * attribute set.  Trigger and delay may be specified as infix formula
   * strings.
   */
  Event (  const std::string&  id      = ""
         , const std::string&  trigger = ""
         , const std::string&  delay   = "" );

  /**
   * Creates a new Event with an id and trigger and (optionally) delay
   * attributes set.
   */
  Event (  const std::string&  id
         , ASTNode*            trigger
         , ASTNode*            delay   = 0 );

  /**
   * Copies this Event.
   */
  Event (const Event& rhs);

  /**
   * Destroys this Event.
   */
  virtual ~Event ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next Event
   * (if available).
   */
  bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Event.
   */
  virtual SBase* clone () const;


  /**
   * @return the trigger of this Event.
   */
  const ASTNode* getTrigger () const;

  /**
   * @return the delay of this Event.
   */
  const ASTNode* getDelay () const;

  /**
   * @return the timeUnits of this Event.
   */
  const std::string& getTimeUnits () const;


  /**
   * @return true if the trigger of this Event has been set, false
   * otherwise.
   */
  bool isSetTrigger () const;

  /**
   * @return true if the delay of this Event has been set, false otherwise.
   */
  bool isSetDelay () const;

  /**
   * @return true if the timeUnits of this Event has been set, false
   * otherwise.
   */
  bool isSetTimeUnits () const;


  /**
   * Sets the trigger of this Event to a copy of the given ASTNode.
   */
  void setTrigger (const ASTNode* math);

  /**
   * Sets the delay of this Event to a copy of the given ASTNode.
   *
   * The node <b>is not copied</b> and this Event <b>takes ownership</b> of
   * it; i.e. subsequent calls to this function or a call to Event_free()
   * will free the ASTNode (and any child nodes).
   */
  void setDelay (const ASTNode* math);

  /**
   * Sets the timeUnits of this Event to a copy of sid.
   */
  void setTimeUnits (const std::string& sid);


  /**
   * Unsets the delay of this Event.
   */
  void unsetDelay ();

  /**
   * Unsets the timeUnits of this Event.
   */
  void unsetTimeUnits ();

  /**
   * Appends a copy of the given EventAssignment to this Event.
   */
  void addEventAssignment (const EventAssignment* ea);


  /**
   * @return the list of EventAssignments for this Event.
   */
  const ListOfEventAssignments* getListOfEventAssignments () const;

  /**
   * @return the list of EventAssignments for this Event.
   */
  ListOfEventAssignments* getListOfEventAssignments ();


  /**
   * @return the nth EventAssignment of this Event.
   */
  const EventAssignment* getEventAssignment (unsigned int n) const;

  /**
   * @return the nth EventAssignment of this Event.
   */
  EventAssignment* getEventAssignment (unsigned int n);

  /**
   * @return the EventAssignment for the given variable, or NULL if no such
   * EventAssignment exits.
   */
  const EventAssignment* getEventAssignment (const std::string& variable) const;

  /**
   * @return the EventAssignment for the given variable, or NULL if no such
   * EventAssignment exits.
   */
  EventAssignment* getEventAssignment (const std::string& variable);


  /**
   * @return the number of EventAssignments in this Event.
   */
  unsigned int getNumEventAssignments () const;


  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const;

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream);


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream);


  ASTNode*                mTrigger;
  ASTNode*                mDelay;
  std::string             mTimeUnits;
  ListOfEventAssignments  mEventAssignments;
};



class LIBSBML_EXTERN ListOfEvents : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfEvents.
   */
  virtual SBase* clone () const;

  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const;


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new Event and returns a pointer to it.
 */
LIBSBML_EXTERN
Event_t *
Event_create (void);

/**
 * Creates a new Event with the given id and trigger and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   e = Event_create();
 *   Event_setId(e, id); Event_setTrigger(e, trigger);
 */
LIBSBML_EXTERN
Event_t *
Event_createWith (const char *sid, ASTNode_t *trigger);

/**
 * Frees the given Event.
 */
LIBSBML_EXTERN
void
Event_free (Event_t *e);

/**
 * @return a (deep) copy of this Event.
 */
LIBSBML_EXTERN
Event_t *
Event_clone (const Event_t *e);


/**
 * @return the id of this Event.
 */
LIBSBML_EXTERN
const char *
Event_getId (const Event_t *e);

/**
 * @return the name of this Event.
 */
LIBSBML_EXTERN
const char *
Event_getName (const Event_t *e);

/**
 * @return the trigger of this Event.
 */
LIBSBML_EXTERN
const ASTNode_t *
Event_getTrigger (const Event_t *e);

/**
 * @return the delay of this Event.
 */
LIBSBML_EXTERN
const ASTNode_t *
Event_getDelay (const Event_t *e);

/**
 * @return the timeUnits of this Event
 */
LIBSBML_EXTERN
const char *
Event_getTimeUnits (const Event_t *e);


/**
 * @return 1 if the id of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetId (const Event_t *e);

/**
 * @return 1 if the name of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetName (const Event_t *e);

/**
 * @return 1 if the trigger of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetTrigger (const Event_t *e);

/**
 * @return 1 if the delay of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetDelay (const Event_t *e);

/**
 * @return 1 if the timeUnits of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetTimeUnits (const Event_t *e);


/**
 * Sets the id of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event_setId (Event_t *e, const char *sid);

/**
 * Sets the name of this Event to a copy of name.
 */
LIBSBML_EXTERN
void
Event_setName (Event_t *e, const char *name);

/**
 * Sets the trigger of this Event to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Event_setTrigger (Event_t *e, ASTNode_t *math);

/**
 * Sets the delay of this Event to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Event_setDelay (Event_t *e, const ASTNode_t *math);

/**
 * Sets the timeUnits of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event_setTimeUnits (Event_t *e, const char *sid);


/**
 * Unsets the id of this Event.
 */
LIBSBML_EXTERN
void
Event_unsetId (Event_t *e);

/**
 * Unsets the name of this Event.
 */
LIBSBML_EXTERN
void
Event_unsetName (Event_t *e);

/**
 * Unsets the delay of this Event.
 */
LIBSBML_EXTERN
void
Event_unsetDelay (Event_t *e);

/**
 * Unsets the timeUnits of this Event.
 */
LIBSBML_EXTERN
void
Event_unsetTimeUnits (Event_t *e);


/**
 * Appends a copy of the given EventAssignment to this Event.
 */
LIBSBML_EXTERN
void
Event_addEventAssignment (Event_t *e, const EventAssignment_t *ea);

/**
 * @return the list of EventAssignments for this Event.
 */
LIBSBML_EXTERN
ListOf_t *
Event_getListOfEventAssignments (Event_t *e);

/**
 * @return the nth EventAssignment of this Event.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_getEventAssignment (Event_t *e, unsigned int n);


/**
 * @return the EventAssignment for the given variable, or NULL if no such
 * EventAssignment exits.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_getEventAssignmentByVar (Event_t *e, const char *variable);

/**
 * @return the number of EventAssignments in this Event.
 */
LIBSBML_EXTERN
unsigned int
Event_getNumEventAssignments (const Event_t *e);


/**
 * The EventIdCmp function compares the string sid to e->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than e->id.
 * Returns -1 if either sid or e->id is NULL.
 */
LIBSBML_EXTERN
int
EventIdCmp (const char *sid, const Event_t *e);


END_C_DECLS


#endif  /* !SWIG   */
#endif  /* Event_h */
