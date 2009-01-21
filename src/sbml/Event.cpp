/**
 * @file    Event.cpp
 * @brief   Implementations of Event and ListOfEvents.
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/EventAssignment.h>
#include <sbml/Event.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new Event, optionally with its id and name attributes set. 
 */
Event::Event (const std::string& id, const std::string& name) :
   SBase    ( id, name   )
 , mTrigger ( 0          )
 , mDelay   ( 0          )
 , mUseValuesFromTriggerTime ( true )
{
  mInternalIdOnly = false;
}


/** @cond doxygen-libsbml-internal */
Event::Event (unsigned int level, unsigned int version,
                          XMLNamespaces *xmlns) :
   SBase ("", "", -1)
 , mTrigger ( 0          )
 , mDelay   ( 0          )
 , mUseValuesFromTriggerTime ( true )
{
  mInternalIdOnly = false;
  mObjectLevel = level;
  mObjectVersion = version;
  if (xmlns) setNamespaces(xmlns);;
}
/** @endcond doxygen-libsbml-internal */

                          
/*
 * Destroys this Event.
 */
Event::~Event ()
{
  delete mTrigger;
  delete mDelay;
}


/*
 * Copy constructor. Creates a copy of this Event.
 */
Event::Event (const Event& orig) :
   SBase            ( orig                   )
 , mTrigger         ( 0                      )
 , mDelay           ( 0                      )
 , mTimeUnits       ( orig.mTimeUnits        )
 , mUseValuesFromTriggerTime ( orig.mUseValuesFromTriggerTime )
 , mInternalIdOnly  ( orig.mInternalIdOnly   )
 , mEventAssignments( orig.mEventAssignments )
{
  if (orig.mTrigger) mTrigger = new Trigger(*orig.getTrigger());
  if (orig.mDelay) mDelay = new Delay(*orig.getDelay());
}
 

/*
 * Assignment operator
 */
Event& Event::operator=(const Event& rhs)
{
  this->SBase::operator =(rhs);
 
  mTimeUnits        = rhs.mTimeUnits        ;
  mUseValuesFromTriggerTime = rhs.mUseValuesFromTriggerTime;
  mInternalIdOnly   = rhs.mInternalIdOnly   ;
  mEventAssignments = rhs.mEventAssignments ;

  if (rhs.mTrigger) mTrigger = new Trigger(*rhs.getTrigger());
  if (rhs.mDelay) mDelay = new Delay(*rhs.getDelay());

  return *this;
}


/*
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


/*
 * @return a (deep) copy of this Event.
 */
Event*
Event::clone () const
{
  return new Event(*this);
}


/*
 * @return the trigger of this Event.
 */
const Trigger*
Event::getTrigger () const
{
  return mTrigger;
}


/*
 * @return the trigger of this Event.
 */
Trigger*
Event::getTrigger ()
{
  return mTrigger;
}


/*
 * @return the delay of this Event.
 */
const Delay*
Event::getDelay () const
{
  return mDelay;
}


/*
 * @return the delay of this Event.
 */
Delay*
Event::getDelay ()
{
  return mDelay;
}


/*
 * @return the timeUnits of this Event
 *
 * @warning Definitions of Event in SBML Level 2 Versions 1 and 2
 * included the additional attribute called "timeUnits", but it was
 * removed in SBML Level 2 Version 3.  LibSBML supports this attribute
 * for compatibility with previous versions of SBML Level 2, but its use
 * is discouraged since models in Level 2 Versions 3 cannot contain it.
 */
const string&
Event::getTimeUnits () const
{
  return mTimeUnits;
}


/*
 * Returns the value of the "useValuesFromTriggerTime" attribute of this Event.
 */
bool 
Event::getUseValuesFromTriggerTime () const
{
  return mUseValuesFromTriggerTime;
}

  
/*
 * @return true if the trigger of this Event has been set, false otherwise.
 */
bool
Event::isSetTrigger () const
{
  return (mTrigger != 0);
}


/*
 * @return true if the delay of this Event has been set, false otherwise.
 */
bool
Event::isSetDelay () const
{
  return (mDelay != 0);
}


/*
 * @return true if the timeUnits of this Event has been set, false
 * otherwise.
 *
 * @warning Definitions of Event in SBML Level 2 Versions 1 and 2
 * included the additional attribute called "timeUnits", but it was
 * removed in SBML Level 2 Version 3.  LibSBML supports this attribute
 * for compatibility with previous versions of SBML Level 2, but its use
 * is discouraged since models in Level 2 Version 3 cannot contain it.
 */
bool
Event::isSetTimeUnits () const
{
  return (mTimeUnits.empty() == false);
}


/*
 * Sets the trigger of this Event to a copy of the given Trigger.
 */
void
Event::setTrigger (const Trigger* trigger)
{
  if(mTrigger == trigger) return;

  delete mTrigger;
  mTrigger = (trigger != 0) ? static_cast<Trigger*>( trigger->clone() ) : 0;

  if (mTrigger) mTrigger->setSBMLDocument(mSBML);
  if (mTrigger) mTrigger->setParentSBMLObject(this);
}


/*
 * Sets the delay of this Event to a copy of the given Delay.
 */
void
Event::setDelay (const Delay* delay)
{
  if(mDelay == delay) return;

  delete mDelay;
  mDelay = (delay != 0) ? static_cast<Delay*>( delay->clone() ) : 0;

  if (mDelay) mDelay->setSBMLDocument(mSBML);
  if (mDelay) mDelay->setParentSBMLObject(this);
}


/*
 * Sets the timeUnits of this Event to a copy of sid.
 *
 * @warning Definitions of Event in SBML Level 2 Versions 1 and 2
 * included the additional attribute called "timeUnits", but it was
 * removed in SBML Level 2 Version 3.  LibSBML supports this attribute
 * for compatibility with previous versions of SBML Level 2, but its use
 * is discouraged since models in Level 2 Version 3 cannot contain it.
 */
void
Event::setTimeUnits (const std::string& sid)
{
  mTimeUnits = sid;
}


/*
 * Sets the "useValuesFromTriggerTime" attribute of this Event to a @p value.
 */
void 
Event::setUseValuesFromTriggerTime (bool value)
{
  mUseValuesFromTriggerTime = value;
}


/*
 * Unsets the delay of this Event.
 */
void
Event::unsetDelay ()
{
  delete mDelay;
  mDelay = 0;
}


/*
 * Unsets the timeUnits of this Event.
 *
 * @warning Definitions of Event in SBML Level 2 Versions 1 and 2
 * included the additional attribute called "timeUnits", but it was
 * removed in SBML Level 2 Version 3.  LibSBML supports this attribute
 * for compatibility with previous versions of SBML Level 2, but its use
 * is discouraged since models in Level 2 Version 3 cannot contain it.
 */
void
Event::unsetTimeUnits ()
{
  mTimeUnits.erase();
}


/*
 * Appends a copy of the given EventAssignment to this Event.
 */
void
Event::addEventAssignment (const EventAssignment* ea)
{
  /* if the ListOf is empty it doesnt know its parent */
  if (mEventAssignments.size() == 0)
  {
    mEventAssignments.setSBMLDocument(this->getSBMLDocument());
    mEventAssignments.setParentSBMLObject(this);
  }

  mEventAssignments.append(ea);
}


/*
 * Creates a new EventAssignment, adds it to this Event's list of event
 * assignments and returns it.
 */
EventAssignment*
Event::createEventAssignment ()
{
  EventAssignment* ea = new EventAssignment;
  
  /* if the ListOf is empty it doesnt know its parent */
  if (mEventAssignments.size() == 0)
  {
    mEventAssignments.setSBMLDocument(this->getSBMLDocument());
    mEventAssignments.setParentSBMLObject(this);
  }
  
  mEventAssignments.appendAndOwn(ea);

  return ea;
}


/*
 * @return the list of EventAssignments for this Event.
 */
const ListOfEventAssignments*
Event::getListOfEventAssignments () const
{
  return &mEventAssignments;
}


/*
 * @return the list of EventAssignments for this Event.
 */
ListOfEventAssignments*
Event::getListOfEventAssignments ()
{
  return &mEventAssignments;
}


/*
 * @return the nth EventAssignment of this Event.
 */
const EventAssignment*
Event::getEventAssignment (unsigned int n) const
{
  return static_cast<const EventAssignment*>( mEventAssignments.get(n) );
}


/*
 * @return the nth EventAssignment of this Event.
 */
EventAssignment*
Event::getEventAssignment (unsigned int n)
{
  return static_cast<EventAssignment*>( mEventAssignments.get(n) );
}


/*
 * @return the EventAssignment for the given variable, or NULL if no such
 * EventAssignment exits.
 */
const EventAssignment*
Event::getEventAssignment (const std::string& variable) const
{
  return
    static_cast<const EventAssignment*>( mEventAssignments.get(variable) );
}


/*
 * @return the EventAssignment for the given variable, or NULL if no such
 * EventAssignment exits.
 */
EventAssignment*
Event::getEventAssignment (const std::string& variable)
{
  return static_cast<EventAssignment*>( mEventAssignments.get(variable) );
}


/*
 * @return the number of EventAssignments in this Event.
 */
unsigned int
Event::getNumEventAssignments () const
{
  return mEventAssignments.size();
}

/** @cond doxygen-libsbml-internal */

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Event::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
  mEventAssignments.setSBMLDocument(d);
  if (mTrigger) mTrigger->setSBMLDocument(d);
  if (mDelay) mDelay->setSBMLDocument(d);
}


/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
Event::setParentSBMLObject (SBase* sb)
{
  mParentSBMLObject = sb;
}
/** @endcond doxygen-libsbml-internal */


/*
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


/*
 * @return the name of this element ie "event".
 */
const string&
Event::getElementName () const
{
  static const string name = "event";
  return name;
}


/** @cond doxygen-libsbml-internal */

/*
 * sets the mInternalIdOnly flag
 */
void 
Event::setInternalIdOnly()
{
  mInternalIdOnly = true;
}

/*
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
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <listOfEventAssignments> elements is permitted "
	       "in a single <event> element.");
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
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <delay> element is permitted in a single "
	       "<event> element.");
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
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

  if (level < 2)
  {
    logError(NotSchemaConformant, getLevel(), getVersion(),
	      "Event is not a valid component for this level/version.");
    return;
  }
  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("id");

  if (level == 2 && version < 3)
  {
    expectedAttributes.push_back("timeUnits");
  }

  if (!(level == 2 && version == 1))
  {
    expectedAttributes.push_back("sboTerm");
  }

  if (!(level == 2 && version < 4))
  {
    expectedAttributes.push_back("useValuesFromTriggerTime");
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      logUnknownAttribute(name, level, version, "<event>");
    }
  }

  //
  // id: SId  { use="optional" }  (L2v1 ->)
  //
  bool assigned = attributes.readInto("id", mId, getErrorLog(), false);
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<event>");
  }
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v1 ->)
  //
  attributes.readInto("name", mName);

  //
  // timeUnits: SId  { use="optional" }  (L2v1, L2v2)
  // removed in l2v3
  //
  if (level == 2 && version < 3)
  {
    attributes.readInto("timeUnits", mTimeUnits);
    SBase::checkUnitSyntax();
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
  //
  if (!(level == 2 && version == 1)) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());

  //
  // useValuesFromTriggerTime: bool {use="optional" default="true"} (L2V4 ->)
  //
  if (!(level == 2 && version < 4))
    attributes.readInto("useValuesFromTriggerTime", mUseValuesFromTriggerTime);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
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

  /* invalid level/version */
  if (level < 2)
  {
    return;
  }

  //
  // id: SId  { use="optional" }  (L2v1 ->)
  //
  if (!mInternalIdOnly)
    stream.writeAttribute("id", mId);

  //
  // name: string  { use="optional" }  (L2v1->)
  //
  stream.writeAttribute("name", mName);

  if (level == 2 && version < 3)
  {
    //
    // timeUnits: SId  { use="optional" }  (L2v1, L2v2)
    // removed in l2v3
    //
    stream.writeAttribute("timeUnits", mTimeUnits);
  }


  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
  //
  if (!(level == 2 && version == 1)) 
    SBO::writeTerm(stream, mSBOTerm);

  //
  // useValuesFromTriggerTime: bool {use="optional" default="true"} (L2V4 ->)
  //
  if (!(level == 2 && version < 4))
  {
    if (!mUseValuesFromTriggerTime)
      stream.writeAttribute("useValuesFromTriggerTime", mUseValuesFromTriggerTime);
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Event::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (mTrigger)
  {
    mTrigger->write(stream);
  }

  if (mDelay)
  {
    mDelay->write(stream);
  }

  if ( getNumEventAssignments() > 0 ) mEventAssignments.write(stream);
}
/** @endcond doxygen-libsbml-internal */


/*
 * @return a (deep) copy of this ListOfEvents.
 */
ListOfEvents*
ListOfEvents::clone () const
{
  return new ListOfEvents(*this);
}


/*
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfEvents::getItemTypeCode () const
{
  return SBML_EVENT;
}


/*
 * @return the name of this element ie "listOfEvents".
 */
const string&
ListOfEvents::getElementName () const
{
  static const string name = "listOfEvents";
  return name;
}


/* return nth item in list */
Event *
ListOfEvents::get(unsigned int n)
{
  return static_cast<Event*>(ListOf::get(n));
}


/* return nth item in list */
const Event *
ListOfEvents::get(unsigned int n) const
{
  return static_cast<const Event*>(ListOf::get(n));
}


/* return item by id */
Event*
ListOfEvents::get (const std::string& sid)
{
  return static_cast<Event*>(ListOf::get(sid));
}


/* return item by id */
const Event*
ListOfEvents::get (const std::string& sid) const
{
  return static_cast<const Event*>(ListOf::get(sid));
}


/* Removes the nth item from this list */
Event*
ListOfEvents::remove (unsigned int n)
{
   return static_cast<Event*>(ListOf::remove(n));
}


/* Removes item in this list by id */
Event*
ListOfEvents::remove (const std::string& sid)
{
   return static_cast<Event*>(ListOf::remove(sid));
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfEvents::getElementPosition () const
{
  return 12;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
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
/** @endcond doxygen-libsbml-internal */



/** @cond doxygen-c-only */


/**
 * Creates a new, empty Event_t structure and returns a pointer to it.
 * 
 * @return the newly-created Event_t structure.
 */
LIBSBML_EXTERN
Event_t *
Event_create (void)
{
  return new(nothrow) Event;
}


/**
 * Creates a new Event with a specific identifier and name and returns
 * a pointer to it.
 *
 * @param sid a string to be assigned as the identifier of the Event
 * @param name a string to be assigned as the name of the Event
 *
 * @return the newly-created Event_t structure.
 */
LIBSBML_EXTERN
Event_t *
Event_createWith (const char *sid, const char *name)
{
  return new(nothrow) Event(sid ? sid : "", name ? name : "");
}


/** @cond doxygen-libsbml-internal */
/**
 * Creates a new Event_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * Event
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Event
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this Event
 *
 * @return a pointer to the newly created Event_t structure.
 *
 * @note Once a Event has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Event.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
Event_t *
Event_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version, XMLNamespaces_t *xmlns)
{
  return new(nothrow) Event(level, version, xmlns);
}
/** @endcond doxygen-libsbml-internal */


/**
 * Frees the given Event_t structure.
 *
 * @param e the Event_t structure to free.
 */
LIBSBML_EXTERN
void
Event_free (Event_t *e)
{
  delete e;
}


/**
 * Returns a copy of the given Event_t structure.
 *
 * @param e the Event_t structure to copy.
 * 
 * @return a (deep) copy of the Event_t.
 */
LIBSBML_EXTERN
Event_t *
Event_clone (const Event_t *e)
{
  return static_cast<Event_t*>( e->clone() );
}


/**
 * Returns a list of XMLNamespaces_t associated with this Event_t
 * structure.
 *
 * @param e the Event_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
Event_getNamespaces(Event_t *e)
{
  return e->getNamespaces();
}


/**
 * Takes an Event_t structure and returns its identifier.
 *
 * @param p the Event_t structure whose identifier is sought
 * 
 * @return the identifier of this Event_t, as a pointer to a string.
 */
LIBSBML_EXTERN
const char *
Event_getId (const Event_t *e)
{
  return e->isSetId() ? e->getId().c_str() : NULL;
}


/**
 * Takes a Event_t structure and returns its name.
 *
 * @param p the Event_t whose name is sought.

 * @return the name of this Event_t, as a pointer to a string.
 */
LIBSBML_EXTERN
const char *
Event_getName (const Event_t *e)
{
  return e->isSetName() ? e->getName().c_str() : NULL;
}


/**
 * Takes an Event_t structure and returns its Trigger_t structure.
 *
 * @param e the Event_t structure whose trigger definition is sought.
 * 
 * @return the Trigger_t of this Event.
 */
LIBSBML_EXTERN
Trigger_t *
Event_getTrigger (Event_t *e)
{
  return e->getTrigger();
}


/**
 * Takes an Event_t structure and returns its Delay_t structure.
 *
 * @param e the Event_t structure whose delay definition is sought.
 * 
 * @return the Delay_t of this Event.
 */
LIBSBML_EXTERN
Delay_t *
Event_getDelay (Event_t *e)
{
  return e->getDelay();
}


/**
 * Takes an Event_t structure and returns the value of its "timeUnits"
 * attribute.
 *
 * @param e the Event_t structure whose "timeUnits" value is sought
 * 
 * @return the timeUnits of this Event
 *
 * @warning Definitions of Event in SBML Level 2 Versions 1 and 2
 * included the additional attribute called "timeUnits", but it was
 * removed in SBML Level 2 Version 3.  LibSBML supports this attribute
 * for compatibility with previous versions of SBML Level 2, but its use
 * is discouraged since models in Level 2 Versions 3 and 4 cannot contain it.
 */
LIBSBML_EXTERN
const char *
Event_getTimeUnits (const Event_t *e)
{
  return e->isSetTimeUnits() ? e->getTimeUnits().c_str() : NULL;
}


/**
 * Takes an Event_t structure and returns the value of its "useValuesFromTriggerTime"
 * attribute.
 *
 * @param e the Event_t structure whose "useValuesFromTriggerTime" value is sought
 * 
 * @return the useValuesFromTriggerTime of this Event
 */
LIBSBML_EXTERN
int
Event_getUseValuesFromTriggerTime (const Event_t *e)
{
  return static_cast<int> (e->getUseValuesFromTriggerTime());
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Event_t structure's identifier has been set.
 *
 * @param e the Event_t structure to query
 * 
 * @return @c non-zero (true) if the "id" attribute of the given
 * Event_t structure has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetId (const Event_t *e)
{
  return static_cast<int>( e->isSetId() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Event_t structure's name has been set.
 *
 * @param e the Event_t structure to query
 * 
 * @return @c non-zero (true) if the "name" attribute of the given
 * Event_t structure has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetName (const Event_t *e)
{
  return static_cast<int>( e->isSetName() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Event_t structure's trigger has been set.
 *
 * @param e the Event_t structure to query
 * 
 * @return @c non-zero (true) if a Trigger_t structure has been assigned to
 * the given Event_t structure, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetTrigger (const Event_t *e)
{
  return static_cast<int>( e->isSetTrigger() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Event_t structure's delay has been set.
 *
 * @param e the Event_t structure to query
 * 
 * @return @c non-zero (true) if a Delay_t structure has been assigned to
 * the given Event_t structure, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetDelay (const Event_t *e)
{
  return static_cast<int>( e->isSetDelay() );
}


/**
 * Predicate returning @c true or @c false depending on whether the given
 * Event_t structure's "timeUnits" attribute has been set.
 *
 * @param e the Event_t structure to query
 * 
 * @return @c non-zero (true) if a value for the "timeUnits" attribute has
 * been assigned in the given Event_t structure, zero (false) otherwise.
 *
 * @warning Definitions of Event in SBML Level 2 Versions 1 and 2
 * included the additional attribute called "timeUnits", but it was
 * removed in SBML Level 2 Version 3.  LibSBML supports this attribute
 * for compatibility with previous versions of SBML Level 2, but its use
 * is discouraged since models in Level 2 Versions 3 and 4 cannot contain it.
 */
LIBSBML_EXTERN
int
Event_isSetTimeUnits (const Event_t *e)
{
  return static_cast<int>( e->isSetTimeUnits() );
}


/**
 * Assigns the identifier of an Event_t structure.
 *
 * This makes a copy of the string passed in the param @p sid.
 *
 * @param e the Event_t structure to set.
 * @param sid the string to use as the identifier.
 */
LIBSBML_EXTERN
void
Event_setId (Event_t *e, const char *sid)
{
  (sid == NULL) ? e->unsetId() : e->setId(sid);
}


/**
 * Sets the name of this Event to a copy of @p name.
 *
 * @param e the Event_t structure to set
 * @param name the name to assign to this Event_t's "name" attribute.
 */
LIBSBML_EXTERN
void
Event_setName (Event_t *e, const char *name)
{
  (name == NULL) ? e->unsetName() : e->setName(name);
}


/**
 * Sets the trigger of this Event to a copy of the given Trigger.
 *
 * @param e the Event_t structure to set
 * @param trigger the Trigger_t structure to use.
 */
LIBSBML_EXTERN
void
Event_setTrigger (Event_t *e, const Trigger_t *trigger)
{
  e->setTrigger(trigger);
}


/**
 * Sets the delay of this Event to a copy of the given Delay.
 * 
 * @param e the Event_t structure to set
 * @param delay the Delay_t structure to use.
 */
LIBSBML_EXTERN
void
Event_setDelay (Event_t *e, const Delay_t *delay)
{
  e->setDelay(delay);
}


/**
 * Sets the "timeUnits" attribute of this Event to a copy of @p sid.
 * 
 * @param e the Event_t structure to set
 * @param sid the identifier of the units to use as the value of the
 * "timeUnits" attribute
 *
 * @warning Definitions of Event in SBML Level 2 Versions 1 and 2
 * included the additional attribute called "timeUnits", but it was
 * removed in SBML Level 2 Version 3.  LibSBML supports this attribute
 * for compatibility with previous versions of SBML Level 2, but its use
 * is discouraged since models in Level 2 Versions 3 and 4 cannot contain it.
 */
LIBSBML_EXTERN
void
Event_setTimeUnits (Event_t *e, const char *sid)
{
  (sid == NULL) ? e->unsetTimeUnits() : e->setTimeUnits(sid);
}


/**
 * Sets the "useValuesFromTriggerTime" attribute of this Event to a @p value.
 * 
 * @param e the Event_t structure to set
 * @param value the value of the "useValuesFromTriggerTime" attribute
 */
LIBSBML_EXTERN
void
Event_setUseValuesFromTriggerTime (Event_t *e, int value)
{
  e->setUseValuesFromTriggerTime( static_cast<bool>(value) );
}


/**
 * Unsets the "id" attribute of this Event_t structure.
 *
 * @param e the Event_t structure to unset
 */
LIBSBML_EXTERN
void
Event_unsetId (Event_t *e)
{
  e->unsetId();
}


/**
 * Unsets the "name" attribute of this Event_t structure.
 *
 * @param e the Event_t structure to unset
 */
LIBSBML_EXTERN
void
Event_unsetName (Event_t *e)
{
  e->unsetName();
}


/**
 * Unsets the delay of this Event.
 *
 * @param e the Event_t structure to unset
 */
LIBSBML_EXTERN
void
Event_unsetDelay (Event_t *e)
{
  e->unsetDelay();
}


/**
 * Unsets the "timeUnits" attribute of this Event.
 *
 * @param e the Event_t structure to unset
 *
 * @warning Definitions of Event in SBML Level 2 Versions 1 and 2
 * included the additional attribute called "timeUnits", but it was
 * removed in SBML Level 2 Version 3.  LibSBML supports this attribute
 * for compatibility with previous versions of SBML Level 2, but its use
 * is discouraged since models in Level 2 Versions 3 and 4 cannot contain it.
 */
LIBSBML_EXTERN
void
Event_unsetTimeUnits (Event_t *e)
{
  e->unsetTimeUnits();
}


/**
 * Appends a copy of the given EventAssignment_t structure to this Event_t
 * structure.
 *
 * @param e the Event_t structure to which the event assignment should be
 * added
 *
 * @param ea an EventAssignment_t structure to add
 */
LIBSBML_EXTERN
void
Event_addEventAssignment (Event_t *e, const EventAssignment_t *ea)
{
  if (ea != NULL) e->addEventAssignment(ea);
}


/**
 * Creates a new, empty EventAssignment_t structure, adds it to this
 * Event's list of event assignments, and returns the EventAssignment_t.
 *
 * @param e the Event_t structure to which the event assignment should be
 * added
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_createEventAssignment (Event_t *e)
{
  return e->createEventAssignment();
}


/**
 * Get the list of EventAssignment_t structures from this Event_t
 * structure.
 *
 * @param e the Event_t structure to use.
 *
 * @return the list of EventAssignments for this Event.
 */
LIBSBML_EXTERN
ListOf_t *
Event_getListOfEventAssignments (Event_t *e)
{
  return e->getListOfEventAssignments();
}


/**
 * Return a specific EventAssignment_t structure of this Event_t.
 *
 * @param e the Event_t structure to use
 *
 * @param n an integer, the index of the EventAssignment_t structure to return
 * 
 * @return the nth EventAssignment_t of this Event.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_getEventAssignment (Event_t *e, unsigned int n)
{
  return e->getEventAssignment(n);
}


/**
 * Return the event assignment indicated by the given @p variable.
 *
 * @param e the Event_t structure to use
 *
 * @param variable a string, the identifier of the variable whose
 * EventAssignment_t is being sought.
 *
 * @return the EventAssignment_t for the given variable, or NULL if no such
 * EventAssignment_t exits.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_getEventAssignmentByVar (Event_t *e, const char *variable)
{
  return (variable != NULL) ? e->getEventAssignment(variable) : NULL;
}


/**
 * Returns the number of EventAssignment_t objects attached to this
 * Event.
 *
 * @param e the Event_t structure to use
 * 
 * @return the number of EventAssignment_t structures in this Event.
 */
LIBSBML_EXTERN
unsigned int
Event_getNumEventAssignments (const Event_t *e)
{
  return e->getNumEventAssignments();
}


/** @endcond doxygen-c-only */
