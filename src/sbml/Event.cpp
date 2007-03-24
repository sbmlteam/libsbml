/**
 * \file    Event.cpp
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


#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBML.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/EventAssignment.h>
#include <sbml/Event.h>


using namespace std;


/**
 * Creates a new Event, optionally with its id attribute set. 
 */
Event::Event (const string& id) :
   SBase    ( id, "", -1 )
 , mTrigger ( 0          )
 , mDelay   ( 0          )
{
}


/**
 * Copies this Event.
 */
Event::Event (const Event& rhs) :
   SBase            ( rhs                   )
 , mTrigger         ( 0                     )
 , mDelay           ( 0                     )
 , mTimeUnits       ( rhs.mTimeUnits        )
 , mEventAssignments( rhs.mEventAssignments )
{
  if (rhs.mTrigger) mTrigger = new Trigger(*rhs.getTrigger());
  if (rhs.mDelay) mDelay = new Delay(*rhs.getDelay());
}
 

/**
 * Destroys this Event.
 */
Event::~Event ()
{
  delete mTrigger;
  delete mDelay;
}


/**
 * Assignment operator
 */
Event& Event::operator=(const Event& rhs)
{
  this->SBase::operator =(rhs);
 
  mTimeUnits        = rhs.mTimeUnits        ;
  mEventAssignments = rhs.mEventAssignments ;

  if (rhs.mTrigger) mTrigger = new Trigger(*rhs.getTrigger());
  if (rhs.mDelay) mDelay = new Delay(*rhs.getDelay());

  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next Event
 * (if available).
 */
bool
Event::accept (SBMLVisitor& v) const
{
  bool result = v.visit(*this);

  if (mTrigger) mTrigger->accept(v);
  
  if (mDelay) mDelay->accept(v);

  mEventAssignments.accept(v);

  return result;
}


/**
 * @return a (deep) copy of this Event.
 */
SBase*
Event::clone () const
{
  return new Event(*this);
}


/**
 * @return the trigger of this Event.
 */
const Trigger*
Event::getTrigger () const
{
  return mTrigger;
}


/**
 * @return the delay of this Event.
 */
const Delay*
Event::getDelay () const
{
  return mDelay;
}


/**
 * @return the timeUnits of this Event
 */
const string&
Event::getTimeUnits () const
{
  return mTimeUnits;
}


/**
 * @return true if the trigger of this Event has been set, false otherwise.
 */
bool
Event::isSetTrigger () const
{
  return (mTrigger != 0);
}


/**
 * @return true if the delay of this Event has been set, false otherwise.
 */
bool
Event::isSetDelay () const
{
  return (mDelay != 0);
}


/**
 * @return true if the timeUnits of this Event has been set, false
 * otherwise.
 */
bool
Event::isSetTimeUnits () const
{
  return (mTimeUnits.empty() == false);
}


/**
 * Sets the trigger of this Event to a copy of the given Trigger.
 */
void
Event::setTrigger (const Trigger* trigger)
{
  delete mTrigger;
  mTrigger = (trigger != 0) ? static_cast<Trigger*>( trigger->clone() ) : 0;

  if (mTrigger) mTrigger->setSBMLDocument(mSBML);
}


/**
 * Sets the delay of this Event to a copy of the given Delay.
 */
void
Event::setDelay (const Delay* delay)
{
  delete mDelay;
  mDelay = (delay != 0) ? static_cast<Delay*>( delay->clone() ) : 0;

  if (mDelay) mDelay->setSBMLDocument(mSBML);
}


/**
 * Sets the timeUnits of this Event to a copy of sid.
 */
void
Event::setTimeUnits (const string& sid)
{
  mTimeUnits = sid;
}


/**
 * Unsets the delay of this Event.
 */
void
Event::unsetDelay ()
{
  delete mDelay;
  mDelay = 0;
}


/**
 * Unsets the timeUnits of this Event.
 */
void
Event::unsetTimeUnits ()
{
  mTimeUnits.erase();
}


/**
 * Appends a copy of the given EventAssignment to this Event.
 */
void
Event::addEventAssignment (const EventAssignment* ea)
{
  mEventAssignments.append(ea);
}


/**
 * Creates a new EventAssignment, adds it to this Event's list of event
 * assignments and returns it.
 */
EventAssignment*
Event::createEventAssignment ()
{
  EventAssignment* ea = new EventAssignment;
  mEventAssignments.appendAndOwn(ea);

  return ea;
}


/**
 * @return the list of EventAssignments for this Event.
 */
const ListOfEventAssignments*
Event::getListOfEventAssignments () const
{
  return &mEventAssignments;
}


/**
 * @return the list of EventAssignments for this Event.
 */
ListOfEventAssignments*
Event::getListOfEventAssignments ()
{
  return &mEventAssignments;
}


/**
 * @return the nth EventAssignment of this Event.
 */
const EventAssignment*
Event::getEventAssignment (unsigned int n) const
{
  return static_cast<const EventAssignment*>( mEventAssignments.get(n) );
}


/**
 * @return the nth EventAssignment of this Event.
 */
EventAssignment*
Event::getEventAssignment (unsigned int n)
{
  return static_cast<EventAssignment*>( mEventAssignments.get(n) );
}


/**
 * @return the EventAssignment for the given variable, or NULL if no such
 * EventAssignment exits.
 */
const EventAssignment*
Event::getEventAssignment (const string& variable) const
{
  return
    static_cast<const EventAssignment*>( mEventAssignments.get(variable) );
}


/**
 * @return the EventAssignment for the given variable, or NULL if no such
 * EventAssignment exits.
 */
EventAssignment*
Event::getEventAssignment (const string& variable)
{
  return static_cast<EventAssignment*>( mEventAssignments.get(variable) );
}


/**
 * @return the number of EventAssignments in this Event.
 */
unsigned int
Event::getNumEventAssignments () const
{
  return mEventAssignments.size();
}


/**
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Event::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
  mEventAssignments.setSBMLDocument(d);
  mTrigger->setSBMLDocument(d);
  if (mDelay) mDelay->setSBMLDocument(d);
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Event::getTypeCode () const
{
  return SBML_EVENT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
Event::getElementName () const
{
  static const string name = "event";
  return name;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
Event::createObject (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();
  if (name == "listOfEventAssignments") 
  {
    if (mEventAssignments.size() != 0)
    {
      mSBML->getErrorLog()->logError(10103);
    }
    return &mEventAssignments;
  }
  else if (name == "trigger")
  {
    delete mTrigger;

    mTrigger = new Trigger();
    return mTrigger;
  }
  else if (name == "delay")
  {
    if (mDelay)
    {
      mSBML->getErrorLog()->logError(10103);
    }
    delete mDelay;

    mDelay = new Delay();
    return mDelay;
  }
  else
  {
    return 0;
  }
}


/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
Event::readOtherXML (XMLInputStream& stream)
{
  bool          read  = false;
  const string& name  = stream.peek().getName();



  if (name == "annotation")
  {
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    mCVTerms = new List();
    parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = deleteRDFAnnotation(mAnnotation);
    read = true;
  }
  else if (name == "notes")
  {
    /* if notes already exists then it is an error 
     * if annotation already exists then ordering is wrong
     */
    if (mNotes || mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }

    delete mNotes;
    mNotes = new XMLNode(stream);
    checkXHTML(mNotes);
    read = true;
  }

  return read;
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Event::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // id: SId  { use="optional" }  (L2v1, L2v2)
  //
  attributes.readInto("id", mId);
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  attributes.readInto("name", mName);

  //
  // timeUnits: SId  { use="optional" }  (L2v1, L2v2)
  // removed in l2v3
  //
  attributes.readInto("timeUnits", mTimeUnits);


  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Event::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // id: SId  { use="optional" }  (L2v1, L2v2)
  //
  stream.writeAttribute("id", mId);

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  stream.writeAttribute("name", mName);

  if (version != 3)
  {
    //
    // timeUnits: SId  { use="optional" }  (L2v1, L2v2)
    // removed in l2v3
    //
    stream.writeAttribute("timeUnits", mTimeUnits);
  }


  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    SBML::writeSBOTerm(stream, mSBOTerm);
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Event::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  mTrigger->write(stream);

  if (mDelay)
  {
    mDelay->write(stream);
  }

  if ( getNumEventAssignments() > 0 ) mEventAssignments.write(stream);
}




/**
 * @return a (deep) copy of this ListOfEvents.
 */
SBase*
ListOfEvents::clone () const
{
  return new ListOfEvents(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfEvents::getItemTypeCode () const
{
  return SBML_EVENT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
ListOfEvents::getElementName () const
{
  static const string name = "listOfEvents";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfEvents::getElementPosition () const
{
  return 12;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfEvents::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "event")
  {
    object = new Event();
    mItems.push_back(object);
  }

  return object;
}




/**
 * Creates a new Event and returns a pointer to it.
 */
LIBSBML_EXTERN
Event_t *
Event_create (void)
{
  return new(nothrow) Event;
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
Event_createWith (const char *sid)
{
  return new(nothrow) Event(sid ? sid : "");
}


/**
 * Frees the given Event.
 */
LIBSBML_EXTERN
void
Event_free (Event_t *e)
{
  delete e;
}


/**
 * @return a (deep) copy of this Event.
 */
LIBSBML_EXTERN
Event_t *
Event_clone (const Event_t *e)
{
  return static_cast<Event_t*>( e->clone() );
}


/**
 * @return the id of this Event.
 */
LIBSBML_EXTERN
const char *
Event_getId (const Event_t *e)
{
  return e->isSetId() ? e->getId().c_str() : NULL;
}


/**
 * @return the name of this Event.
 */
LIBSBML_EXTERN
const char *
Event_getName (const Event_t *e)
{
  return e->isSetName() ? e->getName().c_str() : NULL;
}


/**
 * @return the trigger of this Event.
 */
LIBSBML_EXTERN
const Trigger_t *
Event_getTrigger (const Event_t *e)
{
  return e->getTrigger();
}


/**
 * @return the delay of this Event.
 */
LIBSBML_EXTERN
const Delay_t *
Event_getDelay (const Event_t *e)
{
  return e->getDelay();
}


/**
 * @return the timeUnits of this Event
 */
LIBSBML_EXTERN
const char *
Event_getTimeUnits (const Event_t *e)
{
  return e->isSetTimeUnits() ? e->getTimeUnits().c_str() : NULL;
}


/**
 * @return 1 if the id of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetId (const Event_t *e)
{
  return static_cast<int>( e->isSetId() );
}


/**
 * @return 1 if the name of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetName (const Event_t *e)
{
  return static_cast<int>( e->isSetName() );
}


/**
 * @return 1 if the trigger of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetTrigger (const Event_t *e)
{
  return static_cast<int>( e->isSetTrigger() );
}


/**
 * @return 1 if the delay of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetDelay (const Event_t *e)
{
  return static_cast<int>( e->isSetDelay() );
}


/**
 * @return 1 if the timeUnits of this Event has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetTimeUnits (const Event_t *e)
{
  return static_cast<int>( e->isSetTimeUnits() );
}


/**
 * Sets the id of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event_setId (Event_t *e, const char *sid)
{
  (sid == NULL) ? e->unsetId() : e->setId(sid);
}


/**
 * Sets the name of this Event to a copy of name.
 */
LIBSBML_EXTERN
void
Event_setName (Event_t *e, const char *name)
{
  (name == NULL) ? e->unsetName() : e->setName(name);
}


/**
 * Sets the trigger of this Event to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Event_setTrigger (Event_t *e, const Trigger_t *math)
{
  e->setTrigger(math);
}


/**
 * Sets the delay of this Event to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Event_setDelay (Event_t *e, const Delay_t *math)
{
  e->setDelay(math);
}


/**
 * Sets the timeUnits of this Event to a copy of sid.
 */
LIBSBML_EXTERN
void
Event_setTimeUnits (Event_t *e, const char *sid)
{
  (sid == NULL) ? e->unsetTimeUnits() : e->setTimeUnits(sid);
}


/**
 * Unsets the id of this Event.
 */
LIBSBML_EXTERN
void
Event_unsetId (Event_t *e)
{
  e->unsetId();
}


/**
 * Unsets the name of this Event.
 */
LIBSBML_EXTERN
void
Event_unsetName (Event_t *e)
{
  e->unsetName();
}


/**
 * Unsets the delay of this Event.
 */
LIBSBML_EXTERN
void
Event_unsetDelay (Event_t *e)
{
  e->unsetDelay();
}


/**
 * Unsets the timeUnits of this Event.
 */
LIBSBML_EXTERN
void
Event_unsetTimeUnits (Event_t *e)
{
  e->unsetTimeUnits();
}


/**
 * Appends a copy of the given EventAssignment to this Event.
 */
LIBSBML_EXTERN
void
Event_addEventAssignment (Event_t *e, const EventAssignment_t *ea)
{
  if (ea != NULL) e->addEventAssignment(ea);
}


/**
 * Creates a new EventAssignment, adds it to this Event's list of event
 * assignments and returns it.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_createEventAssignment (Event_t *e)
{
  return e->createEventAssignment();
}


/**
 * @return the list of EventAssignments for this Event.
 */
LIBSBML_EXTERN
ListOf_t *
Event_getListOfEventAssignments (Event_t *e)
{
  return e->getListOfEventAssignments();
}


/**
 * @return the nth EventAssignment of this Event.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_getEventAssignment (Event_t *e, unsigned int n)
{
  return e->getEventAssignment(n);
}


/**
 * @return the EventAssignment for the given variable, or NULL if no such
 * EventAssignment exits.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_getEventAssignmentByVar (Event_t *e, const char *variable)
{
  return (variable != NULL) ? e->getEventAssignment(variable) : NULL;
}


/**
 * @return the number of EventAssignments in this Event.
 */
LIBSBML_EXTERN
unsigned int
Event_getNumEventAssignments (const Event_t *e)
{
  return e->getNumEventAssignments();
}
