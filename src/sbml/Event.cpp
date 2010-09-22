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
 * Copyright 2005-2010 California Institute of Technology.
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

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

Event::Event (unsigned int level, unsigned int version) :
   SBase ( level, version )
 , mId                       ( ""   )
 , mName                     ( ""   )
 , mTrigger                  ( 0    )
 , mDelay                    ( 0    )
 , mPriority                 ( 0    )
 , mUseValuesFromTriggerTime ( true )
 , mIsSetUseValuesFromTriggerTime ( false )
{
  mInternalIdOnly = false;
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


Event::Event (SBMLNamespaces * sbmlns) :
   SBase                     ( sbmlns )
 , mId                       ( ""   )
 , mName                     ( ""   )
 , mTrigger                  ( 0    )
 , mDelay                    ( 0    )
 , mPriority                 ( 0    )
 , mUseValuesFromTriggerTime ( true )
 , mIsSetUseValuesFromTriggerTime (false )
{
  mInternalIdOnly = false;

  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


/** @cond doxygen-libsbml-internal */

/* constructor for validators */
Event::Event() :
  SBase ()
{
}

/** @endcond */
                          
/*
 * Destroys this Event.
 */
Event::~Event ()
{
  delete mTrigger;
  delete mDelay;
  delete mPriority;
}


/*
 * Copy constructor. Creates a copy of this Event.
 */
Event::Event (const Event& orig) :
   SBase                     ( orig                           )
 , mId                       ( orig.mId                       )  
 , mName                     ( orig.mName                     )
 , mTrigger                  ( 0                              )
 , mDelay                    ( 0                              )
 , mPriority                 ( 0    )
 , mTimeUnits                ( orig.mTimeUnits                )
 , mUseValuesFromTriggerTime ( orig.mUseValuesFromTriggerTime )
 , mIsSetUseValuesFromTriggerTime ( orig.mUseValuesFromTriggerTime )
 , mInternalIdOnly           ( orig.mInternalIdOnly           )
 , mEventAssignments         ( orig.mEventAssignments         )
{
  /* since a unit definition has children we need to re-establish the
   * parentage of these children
   */
  if (orig.getNumEventAssignments() > 0)
  {
    mEventAssignments.setParentSBMLObject(this);
  }
  if (orig.mTrigger) 
  {
    mTrigger = new Trigger(*orig.getTrigger());
    mTrigger->setParentSBMLObject(this);
  }
  if (orig.mDelay) 
  {
    mDelay = new Delay(*orig.getDelay());
    mDelay->setParentSBMLObject(this);
  }
  if (orig.mPriority) 
  {
    mPriority = new Priority(*orig.getPriority());
    mPriority->setParentSBMLObject(this);
  }
}
 

/*
 * Assignment operator
 */
Event& Event::operator=(const Event& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
   
    mId = rhs.mId;
    mName = rhs.mName;
    mTimeUnits        = rhs.mTimeUnits        ;
    mUseValuesFromTriggerTime = rhs.mUseValuesFromTriggerTime;
    mIsSetUseValuesFromTriggerTime = rhs.mUseValuesFromTriggerTime;
    mInternalIdOnly   = rhs.mInternalIdOnly   ;
    mEventAssignments = rhs.mEventAssignments ;

    if (rhs.getNumEventAssignments() > 0)
    {
      mEventAssignments.setParentSBMLObject(this);
    }
   
    delete mTrigger;
    if (rhs.mTrigger) 
    {
      mTrigger = new Trigger(*rhs.getTrigger());
      mTrigger->setParentSBMLObject(this);
    }
    else
    {
      mTrigger = 0;
    }

    delete mDelay;
    if (rhs.mDelay) 
    {
      mDelay = new Delay(*rhs.getDelay());
      mDelay->setParentSBMLObject(this);
    }
    else
    {
      mDelay = 0;
    }

    delete mPriority;
    if (rhs.mPriority) 
    {
      mPriority = new Priority(*rhs.getPriority());
      mPriority->setParentSBMLObject(this);
    }
    else
    {
      mPriority = 0;
    }
  }

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

  if (mPriority) mPriority->accept(v);

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
 * @return the id of this SBML object.
 */
const string&
Event::getId () const
{
  return mId;
}


/*
 * @return the name of this SBML object.
 */
const string&
Event::getName () const
{
  return (getLevel() == 1) ? mId : mName;
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
 * @return the delay of this Event.
 */
const Priority*
Event::getPriority () const
{
  return mPriority;
}


/*
 * @return the delay of this Event.
 */
Priority*
Event::getPriority ()
{
  return mPriority;
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
 * @return true if the id of this SBML object has been set, false
 * otherwise.
 */
bool
Event::isSetId () const
{
  return (mId.empty() == false);
}


/*
 * @return true if the name of this SBML object has been set, false
 * otherwise.
 */
bool
Event::isSetName () const
{
  return (getLevel() == 1) ? (mId.empty() == false) : 
                            (mName.empty() == false);
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
 * @return true if the priority of this Event has been set, false otherwise.
 */
bool
Event::isSetPriority () const
{
  return (mPriority != 0);
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
 * @return true if the mUseValuesFromTriggerTime of this Event has been set, false otherwise.
 */
bool
Event::isSetUseValuesFromTriggerTime () const
{
  return mIsSetUseValuesFromTriggerTime;
}


/*
 * Sets the id of this SBML object to a copy of sid.
 */
int
Event::setId (const std::string& sid)
{
  /* since the setId function has been used as an
   * alias for setName we cant require it to only
   * be used on a L2 model
   */
/*  if (getLevel() == 1)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
*/
  if (!(SyntaxChecker::isValidSBMLSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mId = sid;
    mInternalIdOnly = false;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the name of this SBML object to a copy of name.
 */
int
Event::setName (const std::string& name)
{
  /* if this is setting an L2 name the type is string
   * whereas if it is setting an L1 name its type is SId
   */
  if (getLevel() == 1)
  {
    if (!(SyntaxChecker::isValidSBMLSId(name)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mId = name;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the trigger of this Event to a copy of the given Trigger.
 */
int
Event::setTrigger (const Trigger* trigger)
{
  if (mTrigger == trigger)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (trigger == NULL)
  {
    delete mTrigger;
    mTrigger = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (getLevel() != trigger->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != trigger->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else
  {
    delete mTrigger;
    mTrigger = (trigger != 0) ? static_cast<Trigger*>( trigger->clone() ) : 0;

    if (mTrigger) mTrigger->setSBMLDocument(mSBML);
    if (mTrigger) mTrigger->setParentSBMLObject(this);
    
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the delay of this Event to a copy of the given Delay.
 */
int
Event::setDelay (const Delay* delay)
{
  if (mDelay == delay) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (delay == NULL)
  {
    delete mDelay;
    mDelay = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (getLevel() != delay->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != delay->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else
  {
    delete mDelay;
    mDelay = (delay != 0) ? static_cast<Delay*>( delay->clone() ) : 0;

    if (mDelay) mDelay->setSBMLDocument(mSBML);
    if (mDelay) mDelay->setParentSBMLObject(this);
    
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the prioirty of this Event to a copy of the given Priority.
 */
int
Event::setPriority (const Priority* priority)
{
  if (getLevel() < 3)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else if (mPriority == priority) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (priority == NULL)
  {
    delete mPriority;
    mPriority = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (getLevel() != priority->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != priority->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else
  {
    delete mPriority;
    mPriority = (priority != 0) ? static_cast<Priority*>( priority->clone() ) : 0;

    if (mPriority) mPriority->setSBMLDocument(mSBML);
    if (mPriority) mPriority->setParentSBMLObject(this);
    
    return LIBSBML_OPERATION_SUCCESS;
  }
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
int
Event::setTimeUnits (const std::string& sid)
{
  if (getLevel() == 2 && getVersion() > 2)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else if (!(SyntaxChecker::isValidSBMLSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mTimeUnits = sid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the "useValuesFromTriggerTime" attribute of this Event to a @p value.
 */
int 
Event::setUseValuesFromTriggerTime (bool value)
{
  if (getLevel() == 2 && getVersion() < 4)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mUseValuesFromTriggerTime = value;
    mIsSetUseValuesFromTriggerTime = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the id of this SBML object.
 */
int
Event::unsetId ()
{
  mId.erase();

  if (mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the name of this SBML object.
 */
int
Event::unsetName ()
{
  if (getLevel() == 1) 
  {
    mId.erase();
  }
  else 
  {
    mName.erase();
  }

  if (getLevel() == 1 && mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (mName.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the delay of this Event.
 */
int
Event::unsetDelay ()
{
  delete mDelay;
  mDelay = 0;

  if (mDelay == NULL) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the priority of this Event.
 */
int
Event::unsetPriority ()
{
  delete mPriority;
  mPriority = 0;

  if (mPriority == NULL) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
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
int
Event::unsetTimeUnits ()
{
  if (getLevel() == 2 && getVersion() > 2)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  mTimeUnits.erase();
  
  if (mTimeUnits.empty()) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Appends a copy of the given EventAssignment to this Event.
 */
int
Event::addEventAssignment (const EventAssignment* ea)
{
  if (ea == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(ea->hasRequiredAttributes()) || !(ea->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ea->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ea->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getEventAssignment(ea->getVariable()) != NULL)
  {
    // an eventAssignment for this variable already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    /* if the ListOf is empty it doesnt know its parent */
    if (mEventAssignments.size() == 0)
    {
      mEventAssignments.setSBMLDocument(this->getSBMLDocument());
      mEventAssignments.setParentSBMLObject(this);
    }

    mEventAssignments.append(ea);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new EventAssignment, adds it to this Event's list of event
 * assignments and returns it.
 */
EventAssignment*
Event::createEventAssignment ()
{
  EventAssignment* ea = 0;

  try
  {
    ea = new EventAssignment(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }

  
  /* if the ListOf is empty it doesnt know its parent */
  if (mEventAssignments.size() == 0)
  {
    mEventAssignments.setSBMLDocument(this->getSBMLDocument());
    mEventAssignments.setParentSBMLObject(this);
  }
  
  if (ea) mEventAssignments.appendAndOwn(ea);

  return ea;
}


/*
 * Creates a new Trigger, adds it to this Event
 * and returns it.
 */
Trigger*
Event::createTrigger ()
{
  delete mTrigger;
  mTrigger = 0;
  
  try
  {
    mTrigger = new Trigger(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }

  if (mTrigger)
  {
    mTrigger->setSBMLDocument(mSBML);
    mTrigger->setParentSBMLObject(this);
  }

  return mTrigger;
}


/*
 * Creates a new Delay, adds it to this Event
 * and returns it.
 */
Delay*
Event::createDelay ()
{
  delete mDelay;
  mDelay = 0;
  
  try
  {
    mDelay = new Delay(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }

  if (mDelay)
  {
    mDelay->setSBMLDocument(mSBML);
    mDelay->setParentSBMLObject(this);
  }

  return mDelay;
}


/*
 * Creates a new Priority, adds it to this Event
 * and returns it.
 */
Priority*
Event::createPriority ()
{
  delete mPriority;
  mPriority = 0;
  
  try
  {
    mPriority = new Priority(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
  }

  if (mPriority)
  {
    mPriority->setSBMLDocument(mSBML);
    mPriority->setParentSBMLObject(this);
  }

  return mPriority;
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


/**
 * Removes the nth EventAssignment object from this Event object and
 * returns a pointer to it.
 */
EventAssignment* 
Event::removeEventAssignment (unsigned int n)
{
  return mEventAssignments.remove(n);  
}


/**
 * Removes the EventAssignment object with the given "variable" attribute 
 * from this Event object and returns a pointer to it.
 */
EventAssignment* 
Event::removeEventAssignment (const std::string& variable)
{
  return mEventAssignments.remove(variable);  
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
/** @endcond */


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


bool 
Event::hasRequiredAttributes() const
{
  bool allPresent = true;

  /* required attributes for event: useValuesFromtriggerTime (L3 ->) 
   * only required if there is a delay present*/

  if (getLevel() > 2 && isSetDelay())
  {
    if(!isSetUseValuesFromTriggerTime())
      allPresent = false;
  }

  return allPresent;
}


bool 
Event::hasRequiredElements() const
{
  bool allPresent = true;

  /* required attributes for event: trigger; 
   * listOfEventAssignments (not L3)
  */

  if (!isSetTrigger())
    allPresent = false;

  if (getLevel() < 3 && getNumEventAssignments() == 0)
    allPresent = false;

  return allPresent;
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
      if (getLevel() < 3)
        logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <listOfEventAssignments> elements is permitted "
	       "in a single <event> element.");
      else
        logError(OneListOfEventAssignmentsPerEvent, getLevel(), getVersion());
    }
    return &mEventAssignments;
  }
  else if (name == "trigger")
  {
    if (mTrigger)
    {
      if (getLevel() < 3)
        logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <trigger> elements is permitted "
	       "in a single <event> element.");
      else
        logError(MissingTriggerInEvent, getLevel(), getVersion());
    }

    delete mTrigger;

    try
    {
      mTrigger = new Trigger(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      mTrigger = new Trigger(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      mTrigger = new Trigger(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    return mTrigger;
  }
  else if (name == "delay")
  {
    if (mDelay)
    {
      if (getLevel() < 3)
        logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <delay> element is permitted in a single "
	       "<event> element.");
      else
        logError(OnlyOneDelayPerEvent, getLevel(), getVersion());
    }
    delete mDelay;

    try
    {
      mDelay = new Delay(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      mDelay = new Delay(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      mDelay = new Delay(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    return mDelay;
  }
  else if (name == "priority")
  {
    if (mPriority)
    {
      if (getLevel() < 3)
        logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Priority is not a valid component for this level/version.");
      else 
      {
        logError(OnlyOnePriorityPerEvent, getLevel(), getVersion());
      }
      
    }
    delete mPriority;

    try
    {
      mPriority = new Priority(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      mPriority = new Priority(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      mPriority = new Priority(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    return mPriority;
  }
  else
  {
    return 0;
  }
}
/** @endcond */


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

  const unsigned int level   = getLevel  ();
  switch (level)
  {
  case 1:
    logError(NotSchemaConformant, getLevel(), getVersion(),
	      "Event is not a valid component for this level/version.");
    return;
    break;
  case 2:
    readL2Attributes(attributes);
    break;
  case 3:
  default:
    readL3Attributes(attributes);
    break;
  }
}

/** @cond doxygen-libsbml-internal */

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Event::readL2Attributes (const XMLAttributes& attributes)
{
  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("id");

  if (version < 3)
  {
    expectedAttributes.push_back("timeUnits");
  }

  if (version > 1)
  {
    expectedAttributes.push_back("sboTerm");
  }

  if (version == 4)
  {
    expectedAttributes.push_back("useValuesFromTriggerTime");
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    std::string prefix = attributes.getPrefix(i);
    // only check attributes in the sbml namespace   
    if (prefix.empty() || prefix == "sbml")
    {
      if (std::find(begin, end, name) == end)
      {
        logUnknownAttribute(name, level, version, "<event>");
      }
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
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // name: string  { use="optional" }  (L2v1 ->)
  //
  attributes.readInto("name", mName);

  //
  // timeUnits: SId  { use="optional" }  (L2v1, L2v2)
  // removed in l2v3
  //
  if (version < 3)
  {
    assigned = attributes.readInto("timeUnits", mTimeUnits);
    if (assigned && mTimeUnits.size() == 0)
    {
      logEmptyString("timeUnits", level, version, "<event>");
    }
    if (!SyntaxChecker::isValidUnitSId(mTimeUnits))
    {
      logError(InvalidUnitIdSyntax);
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
  //
  if (version > 1) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);

  //
  // useValuesFromTriggerTime: bool {use="optional" default="true"} (L2V4 ->)
  // useValuesFromTriggerTime: bool {use="optional" } (L3 ->)
  //
  if (version  == 4)
  {
    attributes.readInto("useValuesFromTriggerTime", mUseValuesFromTriggerTime);
  }
}

/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Event::readL3Attributes (const XMLAttributes& attributes)
{
  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("id");
  expectedAttributes.push_back("sboTerm");
  expectedAttributes.push_back("useValuesFromTriggerTime");

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    std::string prefix = attributes.getPrefix(i);
    // only check attributes in the sbml namespace   
    if (prefix.empty() || prefix == "sbml")
    {
      if (std::find(begin, end, name) == end)
      {
        logUnknownAttribute(name, level, version, "<event>");
      }
    }
  }

  //
  // id: SId  { use="optional" }  (L2v1 ->)
  //
  bool assigned = attributes.readInto("id", mId);
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<event>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // name: string  { use="optional" }  (L2v1 ->)
  //
  attributes.readInto("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
  //
  mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);

  //
  // useValuesFromTriggerTime: bool {use="required" } (L3 ->)
  //
  mIsSetUseValuesFromTriggerTime = attributes.readInto(
      "useValuesFromTriggerTime", mUseValuesFromTriggerTime, 
       getErrorLog());
 
  if (!mIsSetUseValuesFromTriggerTime)
  {
    logError(AllowedAttributesOnEvent, level, version);
  }

}

/** @endcond */


/** @endcond */


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
  // useValuesFromTriggerTime: bool {use="required"} (L3 ->)
  //
  if (level == 2 && version == 4)
  {
    if (!mUseValuesFromTriggerTime)
      stream.writeAttribute("useValuesFromTriggerTime", 
                            mUseValuesFromTriggerTime);
  }
  else if (level > 2)
  {
    stream.writeAttribute("useValuesFromTriggerTime", 
                          mUseValuesFromTriggerTime);
  }
}
/** @endcond */


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

  if (mPriority)
  {
    mPriority->write(stream);
  }

  if ( getNumEventAssignments() > 0 ) mEventAssignments.write(stream);
}
/** @endcond */


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


/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqE : public unary_function<SBase*, bool>
{
  const string& id;

  IdEqE (const string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <Event *> (sb)->getId() == id; }
};


/* return item by id */
Event*
ListOfEvents::get (const std::string& sid)
{
  return const_cast<Event*>( 
    static_cast<const ListOfEvents&>(*this).get(sid) );
}


/* return item by id */
const Event*
ListOfEvents::get (const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqE(sid) );
  return (result == mItems.end()) ? 0 : static_cast <Event*> (*result);
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
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqE(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Event*> (item);
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
/** @endcond */


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
    try
    {
      object = new Event(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      object = new Event(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      object = new Event(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    
    if (object) mItems.push_back(object);
  }

  return object;
}
/** @endcond */



/** @cond doxygen-c-only */


/**
 * Creates a new Event_t structure using the given SBML @p level
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Event
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Event
 *
 * @return a pointer to the newly created Event_t structure.
 *
 * @note Once a Event has been added to an SBMLDocument, the @p
 * level and @p version for the document @em override those used to create
 * the Event.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version  determine whether it is valid to
 * assign a particular value to an attribute, or whether it is valid to add
 * an object to an existing SBMLDocument.
 */
LIBSBML_EXTERN
Event_t *
Event_create (unsigned int level, unsigned int version)
{
  try
  {
    Event* obj = new Event(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new Event_t structure using the given
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this Event
 *
 * @return a pointer to the newly created Event_t structure.
 *
 * @note Once a Event has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the Event.  Despite this, the ability to supply the values at creation time
 * is an important aid to creating valid SBML.  Knowledge of the intended SBML
 * Level and Version determine whether it is valid to assign a particular value
 * to an attribute, or whether it is valid to add an object to an existing
 * SBMLDocument.
 */
LIBSBML_EXTERN
Event_t *
Event_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    Event* obj = new Event(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


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
 * Takes an Event_t structure and returns its Priority_t structure.
 *
 * @param e the Event_t structure whose delay definition is sought.
 * 
 * @return the Priority_t of this Event.
 */
LIBSBML_EXTERN
Priority_t *
Event_getPriority (Event_t *e)
{
  return e->getPriority();
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
 * Event_t structure's priority has been set.
 *
 * @param e the Event_t structure to query
 * 
 * @return @c non-zero (true) if a Priority_t structure has been assigned to
 * the given Event_t structure, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetPriority (const Event_t *e)
{
  return static_cast<int>( e->isSetPriority() );
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
 * Predicate returning @c true or @c false depending on whether the given
 * Event_t structure's useValuesFromTriggerTime attribute has been set.
 *
 * @param e the Event_t structure to query
 * 
 * @return @c non-zero (true) if the "useValuesFromTriggerTime" attribute of the given
 * Event_t structure has been set, zero (false) otherwise.
 */
LIBSBML_EXTERN
int
Event_isSetUseValuesFromTriggerTime (const Event_t *e)
{
  return static_cast<int>( e->isSetUseValuesFromTriggerTime() );
}


/**
 * Assigns the identifier of an Event_t structure.
 *
 * This makes a copy of the string passed in the param @p sid.
 *
 * @param e the Event_t structure to set.
 * @param sid the string to use as the identifier.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with an id of NULL is equivalent to
 * unsetting the "id" attribute.
 */
LIBSBML_EXTERN
int
Event_setId (Event_t *e, const char *sid)
{
  return (sid == NULL) ? e->unsetId() : e->setId(sid);
}


/**
 * Sets the name of this Event to a copy of @p name.
 *
 * @param e the Event_t structure to set
 * @param name the name to assign to this Event_t's "name" attribute.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with the name set to NULL is equivalent to
 * unsetting the "name" attribute.
 */
LIBSBML_EXTERN
int
Event_setName (Event_t *e, const char *name)
{
  return (name == NULL) ? e->unsetName() : e->setName(name);
}


/**
 * Sets the trigger of this Event to a copy of the given Trigger.
 *
 * @param e the Event_t structure to set
 * @param trigger the Trigger_t structure to use.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 */
LIBSBML_EXTERN
int
Event_setTrigger (Event_t *e, const Trigger_t *trigger)
{
  return e->setTrigger(trigger);
}


/**
 * Sets the delay of this Event to a copy of the given Delay.
 * 
 * @param e the Event_t structure to set
 * @param delay the Delay_t structure to use.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 */
LIBSBML_EXTERN
int
Event_setDelay (Event_t *e, const Delay_t *delay)
{
  return e->setDelay(delay);
}


/**
 * Sets the priority of this Event to a copy of the given Priority.
 * 
 * @param e the Event_t structure to set
 * @param priority the Priority_t structure to use.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 */
LIBSBML_EXTERN
int
Event_setPriority (Event_t *e, const Priority_t *priority)
{
  return e->setPriority(priority);
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
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 *
 * @note Using this function with an id of NULL is equivalent to
 * unsetting the "timeUnits" attribute.
 */
LIBSBML_EXTERN
int
Event_setTimeUnits (Event_t *e, const char *sid)
{
  return (sid == NULL) ? e->unsetTimeUnits() : e->setTimeUnits(sid);
}


/**
 * Sets the "useValuesFromTriggerTime" attribute of this Event to a @p value.
 * 
 * @param e the Event_t structure to set
 * @param value the value of the "useValuesFromTriggerTime" attribute
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 */
LIBSBML_EXTERN
int
Event_setUseValuesFromTriggerTime (Event_t *e, int value)
{
  return e->setUseValuesFromTriggerTime( static_cast<bool>(value) );
}


/**
 * Unsets the "id" attribute of this Event_t structure.
 *
 * @param e the Event_t structure to unset
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Event_unsetId (Event_t *e)
{
  return e->unsetId();
}


/**
 * Unsets the "name" attribute of this Event_t structure.
 *
 * @param e the Event_t structure to unset
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Event_unsetName (Event_t *e)
{
  return e->unsetName();
}


/**
 * Unsets the delay of this Event.
 *
 * @param e the Event_t structure to unset
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Event_unsetDelay (Event_t *e)
{
  return e->unsetDelay();
}


/**
 * Unsets the priority of this Event.
 *
 * @param e the Event_t structure to unset
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Event_unsetPriority (Event_t *e)
{
  return e->unsetPriority();
}


/**
 * Unsets the "timeUnits" attribute of this Event.
 *
 * @param e the Event_t structure to unset
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 *
 * @warning Definitions of Event in SBML Level 2 Versions 1 and 2
 * included the additional attribute called "timeUnits", but it was
 * removed in SBML Level 2 Version 3.  LibSBML supports this attribute
 * for compatibility with previous versions of SBML Level 2, but its use
 * is discouraged since models in Level 2 Versions 3 and 4 cannot contain it.
 */
LIBSBML_EXTERN
int
Event_unsetTimeUnits (Event_t *e)
{
  return e->unsetTimeUnits();
}


/**
  * Predicate returning @c true or @c false depending on whether
  * all the required attributes for this Event object
  * have been set.
  *
  * @note The required attributes for a Event object are:
  * @li useValuesfromTriggerTime ( L3 onwards )
  */
LIBSBML_EXTERN
int
Event_hasRequiredAttributes (Event_t *e)
{
  return static_cast <int> (e->hasRequiredAttributes());
}



/**
  * Predicate returning @c true or @c false depending on whether
  * all the required elements for this Event object
  * have been set.
  *
  * @note The required elements for a Event object are:
  * @li trigger
  * @li listOfEventAssignments (requirement removed in L3)
  */
LIBSBML_EXTERN
int
Event_hasRequiredElements (Event_t *e)
{
  return static_cast <int> (e->hasRequiredElements() );
}



/**
 * Appends a copy of the given EventAssignment_t structure to this Event_t
 * structure.
 *
 * @param e the Event_t structure to which the event assignment should be
 * added
 *
 * @param ea an EventAssignment_t structure to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_DUPLICATE_OBJECT_ID
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Event_addEventAssignment (Event_t *e, const EventAssignment_t *ea)
{
  return e->addEventAssignment(ea);
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
 * Creates a new, empty Trigger_t structure, adds it to this
 * Event, and returns the Trigger_t.
 *
 * @param e the Event_t structure to which the trigger should be
 * added
 */
LIBSBML_EXTERN
Trigger_t *
Event_createTrigger (Event_t *e)
{
  return e->createTrigger();
}


/**
 * Creates a new, empty Delay_t structure, adds it to this
 * Event, and returns the Delay_t.
 *
 * @param e the Event_t structure to which the delay should be
 * added
 */
LIBSBML_EXTERN
Delay_t *
Event_createDelay (Event_t *e)
{
  return e->createDelay();
}


/**
 * Creates a new, empty Priority_t structure, adds it to this
 * Event, and returns the Priority_t.
 *
 * @param e the Event_t structure to which the priority should be
 * added
 */
LIBSBML_EXTERN
Priority_t *
Event_createPriority (Event_t *e)
{
  return e->createPriority();
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


/**
 * Removes the nth EventAssignment_t object from this Event_t object and
 * returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param m the Event_t structure
 * @param n the integer index of the EventAssignment_t sought
 *
 * @return the EventAssignment_t object removed.  As mentioned above, 
 * the caller owns the returned item. NULL is returned if the given index 
 * is out of range.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_removeEventAssignment (Event_t *e, unsigned int n)
{
  if (!e) return 0;
  return e->removeEventAssignment(n);
}


/**
 * Removes the EventAssignment_t object with the given "variable" attribute
 * from this Event_t object and returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param m the Event_t structure
 * @param sid the string of the "variable" attribute of the EventAssignment_t sought
 *
 * @return the EventAssignment_t object removed.  As mentioned above, the 
 * caller owns the returned object. NULL is returned if no EventAssignment_t
 * object with the "variable" attribute exists in this Event_t object.
 */
LIBSBML_EXTERN
EventAssignment_t *
Event_removeEventAssignmentByVar (Event_t *e, const char *variable)
{
  if (!e) return 0;
  return e->removeEventAssignment(variable);
}


/**
 * @return item in this ListOfEvent with the given id or NULL if no such
 * item exists.
 */
LIBSBML_EXTERN
Event_t *
ListOfEvents_getById (ListOf_t *lo, const char *sid)
{
  return (sid != NULL) ? 
    static_cast <ListOfEvents *> (lo)->get(sid) : NULL;
}


/**
 * Removes item in this ListOf items with the given id or NULL if no such
 * item exists.  The caller owns the returned item and is responsible for
 * deleting it.
 */
LIBSBML_EXTERN
Event_t *
ListOfEvents_removeById (ListOf_t *lo, const char *sid)
{
  return (sid != NULL) ? 
    static_cast <ListOfEvents *> (lo)->remove(sid) : NULL;
}

/** @endcond */

LIBSBML_CPP_NAMESPACE_END

