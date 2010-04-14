/**
 * @file    SBase.cpp
 * @brief   Implementation of SBase, the base object of all SBML objects.
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

#include <sstream>

#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLErrorLog.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>

#include <sbml/util/util.h>

#include <sbml/annotation/RDFAnnotation.h>

#include <sbml/KineticLaw.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/ListOf.h>
#include <sbml/SBase.h>

#ifdef USE_LAYOUT
 #include <sbml/layout/LineSegment.h>
#endif

/** @cond doxygen-ignored */

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/** @endcond doxygen-ignored */

/**
 * elements permitted on the body element of xhtml
 */

/** @cond doxygen-libsbml-internal */

SBMLConstructorException::SBMLConstructorException() :
      std::invalid_argument("Level/version/namespaces combination is invalid")
{
}


/** @endcond doxygen-libsbml-internal */

/** @cond doxygen-libsbml-internal */
/*
 * Only subclasses may create SBase objects.
 */
SBase::SBase (const std::string& id, const std::string& name, int sbo) :
   mNotes     ( 0 )
 , mAnnotation( 0 )
 , mSBML      ( 0 )
 , mSBMLNamespaces (0)
 , mSBOTerm   ( sbo )
 , mLine      ( 0 )
 , mColumn    ( 0 )
 , mParentSBMLObject (0)
 , mCVTerms   ( 0 )
 , mHistory   ( 0 )
 , mHasBeenDeleted (false)
 , mEmptyString ("")
{
  mSBMLNamespaces = new SBMLNamespaces();
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Creates a new SBase object with the given sboTerm.
 * Only subclasses may create SBase objects.
 */
SBase::SBase (unsigned int level, unsigned int version) :
   mNotes     ( 0 )
 , mAnnotation( 0 )
 , mSBML      ( 0 )
 , mSBMLNamespaces (0)
 , mSBOTerm   ( -1 )
 , mLine      ( 0 )
 , mColumn    ( 0 )
 , mParentSBMLObject (0)
 , mCVTerms   ( 0 )
 , mHistory   ( 0 )
 , mHasBeenDeleted (false)
 , mEmptyString ("")
{
  mSBMLNamespaces = new SBMLNamespaces(level, version);
}
/*
 * Creates a new SBase object with the given SBMLNamespaces.
 * Only subclasses may create SBase objects.
 */
SBase::SBase (SBMLNamespaces *sbmlns) :
   mNotes     ( 0 )
 , mAnnotation( 0 )
 , mSBML      ( 0 )
 , mSBMLNamespaces (0)
 , mSBOTerm   ( -1 )
 , mLine      ( 0 )
 , mColumn    ( 0 )
 , mParentSBMLObject (0)
 , mCVTerms   ( 0 )
 , mHistory   ( 0 )
 , mHasBeenDeleted (false)
 , mEmptyString ("")
{
  if (!sbmlns) throw SBMLConstructorException();
  mSBMLNamespaces = sbmlns->clone();
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Copy constructor. Creates a copy of this SBase object.
 */
SBase::SBase(const SBase& orig)
{
  this->mMetaId = orig.mMetaId;

  if(orig.mNotes) 
    this->mNotes = new XMLNode(*const_cast<SBase&>(orig).getNotes());
  else
    this->mNotes = 0;
  
  if(orig.mAnnotation) 
    this->mAnnotation = new XMLNode(*const_cast<SBase&>(orig).mAnnotation);
  else
    this->mAnnotation = 0;
  
  this->mSBML       = NULL;
  this->mSBOTerm    = orig.mSBOTerm;
  this->mLine       = orig.mLine;
  this->mColumn     = orig.mColumn;
  this->mParentSBMLObject = NULL;

  if(orig.mSBMLNamespaces)
    this->mSBMLNamespaces = 
    new SBMLNamespaces(*const_cast<SBase&>(orig).mSBMLNamespaces);
  else
    this->mSBMLNamespaces = 0;

  if(orig.mCVTerms)
  {
    this->mCVTerms  = new List();
    unsigned int i,iMax = orig.mCVTerms->getSize();
    for(i = 0; i < iMax; ++i)
    {
      this->mCVTerms
        ->add(static_cast<CVTerm*>(orig.mCVTerms->get(i))->clone());
    }
  }
  else
  {
    this->mCVTerms = 0;
  }

  if (orig.mHistory)
  {
    this->mHistory = orig.mHistory->clone();
  }
  else
  {
    this->mHistory = 0;
  }

  this->mHasBeenDeleted = false;

}
/** @endcond doxygen-libsbml-internal */


/*
 * Destroy this SBase object.
 */
SBase::~SBase ()
{
  if (mNotes)       delete mNotes;
  if (mAnnotation)  delete mAnnotation;
  if (mSBMLNamespaces)  delete mSBMLNamespaces;
  if (mCVTerms)
  {  
    unsigned int size = mCVTerms->getSize();
    while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
    delete mCVTerms;
  }
  if (mHistory) delete mHistory;
  mHasBeenDeleted = true;
}

/*
 * Assignment operator
 */
SBase& SBase::operator=(const SBase& orig)
{
  if(&orig!=this)
  {
    this->mMetaId = orig.mMetaId;

    delete this->mNotes;

    if(orig.mNotes) 
      this->mNotes = new XMLNode(*const_cast<SBase&>(orig).getNotes());
    else
      this->mNotes = 0;

    delete this->mAnnotation;

    if(orig.mAnnotation) 
      this->mAnnotation = new XMLNode(*const_cast<SBase&>(orig).mAnnotation);
    else
      this->mAnnotation = 0;

    this->mSBML       = orig.mSBML;
    this->mSBOTerm    = orig.mSBOTerm;
    this->mLine       = orig.mLine;
    this->mColumn     = orig.mColumn;
    this->mParentSBMLObject = orig.mParentSBMLObject;

    delete this->mSBMLNamespaces;

    if(orig.mSBMLNamespaces)
      this->mSBMLNamespaces = 
      new SBMLNamespaces(*const_cast<SBase&>(orig).mSBMLNamespaces);
    else
      this->mSBMLNamespaces = 0;


    if(this->mCVTerms)
    {  
      unsigned int size = this->mCVTerms->getSize();
      while (size--) delete static_cast<CVTerm*>( this->mCVTerms->remove(0) );
      delete this->mCVTerms;
    }

    if(orig.mCVTerms)
    {
      this->mCVTerms  = new List();
      unsigned int i,iMax = orig.mCVTerms->getSize();
      for(i = 0; i < iMax; ++i)
      {
        this->mCVTerms
          ->add(static_cast<CVTerm*>(orig.mCVTerms->get(i))->clone());
      }
    }
    else
    {
      this->mCVTerms = 0;
    }

    delete this->mHistory;
    if (orig.mHistory)
    {
      this->mHistory = orig.mHistory->clone();
    }
    else
    {
      this->mHistory = 0;
    }

    this->mHasBeenDeleted = orig.mHasBeenDeleted;
  }

  return *this;
}


/*
 * @return the metaid of this SBML object.
 */
const string&
SBase::getMetaId () const
{
  return mMetaId;
}


/*
 * @return the metaid of this SBML object.
 */
string&
SBase::getMetaId ()
{
  return mMetaId;
}


/** @cond doxygen-libsbml-internal */

/*
 * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
 *
 * @return the id of this SBML object.
 */
const string&
SBase::getId () const
{
  SBMLTypeCode_t type = this->getTypeCode();

  switch(type)
  {
  case SBML_COMPARTMENT:
    return static_cast <const Compartment *> (this)->getId();
    break;

  case SBML_COMPARTMENT_TYPE:
    return static_cast <const CompartmentType *> (this)->getId();
    break;

  case SBML_EVENT:
    return static_cast <const Event *> (this)->getId();
    break;

  case SBML_EVENT_ASSIGNMENT:
    return static_cast <const EventAssignment *> (this)->getVariable();
    break;

  case SBML_FUNCTION_DEFINITION:
    return static_cast <const FunctionDefinition *> (this)->getId();
    break;

  case SBML_INITIAL_ASSIGNMENT:
    return static_cast <const InitialAssignment *> (this)->getSymbol();
    break;

  case SBML_MODEL:
    return static_cast <const Model *> (this)->getId();
    break;

  case SBML_PARAMETER:
    return static_cast <const Parameter *> (this)->getId();
    break;

  case SBML_LOCAL_PARAMETER:
    return static_cast <const LocalParameter *> (this)->getId();
    break;

  case SBML_REACTION:
    return static_cast <const Reaction *> (this)->getId();
    break;

  case SBML_SPECIES:
    return static_cast <const Species *> (this)->getId();
    break;

  case SBML_SPECIES_REFERENCE:
    return static_cast <const SpeciesReference *> (this)->getId();
    break;

  case SBML_SPECIES_TYPE:
    return static_cast <const SpeciesType *> (this)->getId();
    break;

  case SBML_MODIFIER_SPECIES_REFERENCE:
    return static_cast <const ModifierSpeciesReference *> (this)->getId();
    break;

  case SBML_UNIT_DEFINITION:
    return static_cast <const UnitDefinition *> (this)->getId();
    break;

  case SBML_ASSIGNMENT_RULE:
    return static_cast <const AssignmentRule *> (this)->getVariable();
    break;

  case SBML_RATE_RULE:
    return static_cast <const RateRule *> (this)->getVariable();
    break;

#ifdef USE_LAYOUT

  case SBML_LAYOUT_BOUNDINGBOX:
    return static_cast <const BoundingBox *> (this)->getId();
    break;

  case SBML_LAYOUT_GRAPHICALOBJECT:
    return static_cast <const GraphicalObject *> (this)->getId();
    break;

  case SBML_LAYOUT_LAYOUT:
    return static_cast <const Layout *> (this)->getId();
    break;

  case SBML_LAYOUT_LINESEGMENT:
    return static_cast <const LineSegment *> (this)->getId();
    break;
#endif  /* USE_LAYOUT */

  default:
    return mEmptyString;
    break;
  }

  return mEmptyString;
}



/*
 * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
 *
 * @return the name of this SBML object.
 */
const string&
SBase::getName () const
{
  SBMLTypeCode_t type = this->getTypeCode();

  if (getLevel() == 1)
  {
    switch(type)
    {
      case SBML_COMPARTMENT:
        return static_cast <const Compartment *> (this)->getId();
        break;

      case SBML_MODEL:
        return static_cast <const Model *> (this)->getId();
        break;

      case SBML_PARAMETER:
        return static_cast <const Parameter *> (this)->getId();
        break;

      case SBML_LOCAL_PARAMETER:
        return static_cast <const LocalParameter *> (this)->getId();
        break;

      case SBML_REACTION:
        return static_cast <const Reaction *> (this)->getId();
        break;

      case SBML_SPECIES:
        return static_cast <const Species *> (this)->getId();
        break;

      case SBML_UNIT_DEFINITION:
        return static_cast <const UnitDefinition *> (this)->getId();
        break;

      case SBML_ASSIGNMENT_RULE:
        return static_cast <const AssignmentRule *> (this)->getVariable();
        break;

      case SBML_RATE_RULE:
        return static_cast <const RateRule *> (this)->getVariable();
        break;

    #ifdef USE_LAYOUT

      case SBML_LAYOUT_BOUNDINGBOX:
        return static_cast <const BoundingBox *> (this)->getId();
        break;

      case SBML_LAYOUT_GRAPHICALOBJECT:
        return static_cast <const GraphicalObject *> (this)->getId();
        break;

      case SBML_LAYOUT_LAYOUT:
        return static_cast <const Layout *> (this)->getId();
        break;

      case SBML_LAYOUT_LINESEGMENT:
        return static_cast <const LineSegment *> (this)->getId();
        break;
    #endif  /* USE_LAYOUT */

      default:
        return mEmptyString;
        break;
    }

    return mEmptyString;
  }
  else
  {
    switch(type)
    {
    case SBML_COMPARTMENT:
      return static_cast <const Compartment *> (this)->getName();
      break;

    case SBML_COMPARTMENT_TYPE:
      return static_cast <const CompartmentType *> (this)->getName();
      break;

    case SBML_EVENT:
      return static_cast <const Event *> (this)->getName();
      break;

    case SBML_FUNCTION_DEFINITION:
      return static_cast <const FunctionDefinition *> (this)->getName();
      break;

    case SBML_MODEL:
      return static_cast <const Model *> (this)->getName();
      break;

    case SBML_PARAMETER:
      return static_cast <const Parameter *> (this)->getName();
      break;

    case SBML_LOCAL_PARAMETER:
      return static_cast <const LocalParameter *> (this)->getName();
      break;

    case SBML_REACTION:
      return static_cast <const Reaction *> (this)->getName();
      break;

    case SBML_SPECIES:
      return static_cast <const Species *> (this)->getName();
      break;

    case SBML_SPECIES_REFERENCE:
      return static_cast <const SpeciesReference *> (this)->getName();
      break;

    case SBML_SPECIES_TYPE:
      return static_cast <const SpeciesType *> (this)->getName();
      break;

    case SBML_MODIFIER_SPECIES_REFERENCE:
      return static_cast <const ModifierSpeciesReference *> (this)->getName();
      break;

    case SBML_UNIT_DEFINITION:
      return static_cast <const UnitDefinition *> (this)->getName();
      break;

    default:
      return mEmptyString;
      break;
    }

    return mEmptyString;
  }
}

/** @endcond doxygen-libsbml-internal */

/*
 * @return the notes of this SBML object.
 */
XMLNode*
SBase::getNotes()
{
  return mNotes;
}


/*
 * @return the notes of this SBML object by string.
 */
std::string
SBase::getNotesString() 
{
  return XMLNode::convertXMLNodeToString(mNotes);
}


/*
 * @return the annotation of this SBML object.
 */
XMLNode* 
SBase::getAnnotation ()
{
  syncAnnotation();

  return mAnnotation;
}


/*
 * @return the annotation of this SBML object by string.
 */
std::string
SBase::getAnnotationString ()
{
  return XMLNode::convertXMLNodeToString(getAnnotation());
}


/*
 * @return the Namespaces associated with this SBML object
 */
XMLNamespaces*
SBase::getNamespaces() const
{
  if (mSBML)
    return mSBML->getSBMLNamespaces()->getNamespaces();
  else
    return mSBMLNamespaces->getNamespaces();
}


/*
 * @return the parent SBMLDocument of this SBML object.
 */
const SBMLDocument*
SBase::getSBMLDocument () const
{
  if (mSBML != NULL)
  {
    // if the doc object has been deleted the pointer is 
    // still valid but points to nothing
    try 
    {
      if (mSBML->getHasBeenDeleted())
      {
        return NULL;
      }
      else
      {
        return mSBML;
      }
    }
    catch ( ... )
    {
      return NULL;
    }
  }
  return mSBML;
}

/*
 * @return the parent SBMLDocument of this SBML object.
 */
SBMLDocument*
SBase::getSBMLDocument ()
{
  if (mSBML != NULL)
  {
    // if the doc object has been deleted the pointer is 
    // still valid but points to nothing
    try 
    {
      if (mSBML->getHasBeenDeleted())
      {
        return NULL;
      }
      else
      {
        return mSBML;
      }
    }
    catch ( ... )
    {
      return NULL;
    }
  }
  return mSBML;
}
SBase*
SBase::getParentSBMLObject ()
{
  if (mParentSBMLObject != NULL)
  {
    // if the parent object has been deleted the pointer is 
    // still valid but points to nothing
    try 
    {
      if (mParentSBMLObject->getHasBeenDeleted())
      {
        return NULL;
      }
      else
      {
        return mParentSBMLObject;
      }
    }
    catch ( ... )
    {
      return NULL;
    }
  }
  
  return mParentSBMLObject;
}

/*
 * @return the sboTerm as an integer.  If not set,
 * sboTerm will be -1. 
 */
int
SBase::getSBOTerm () const
{
  return mSBOTerm;
}


/*
 * @return the sboTerm as a string.  If not set,
 * return an empty string.
 */
std::string
SBase::getSBOTermID () const
{
  return SBO::intToString(mSBOTerm);
}


/*
 * @return the line number of this SBML object.
 */
unsigned int
SBase::getLine () const
{
  return mLine;
}


/*
 * @return the column number of this SBML object.
 */
unsigned int
SBase::getColumn () const
{
  return mColumn;
}


ModelHistory* 
SBase::getModelHistory() const
{
  return mHistory;
}

ModelHistory* 
SBase::getModelHistory()
{
  return mHistory;
}

/*
 * @return true if the metaid of this SBML object has been set, false
 * otherwise.
 */
bool
SBase::isSetMetaId () const
{
  return (mMetaId.empty() == false);
}


/** @cond doxygen-libsbml-internal */

/*
 * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
 *
 * @return true if the id of this SBML object has been set, false
 * otherwise.
 */
bool
SBase::isSetId () const
{
  return (getId().empty() == false);
}


/*
 * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
 *
 * @return true if the name of this SBML object has been set, false
 * otherwise.
 */
bool
SBase::isSetName () const
{
  return (getName().empty() == false);
}


/** @endcond doxygen-libsbml-internal */


/*
 * @return true if the notes of this SBML object has been set, false
 * otherwise.
 */
bool
SBase::isSetNotes () const
{
  return (mNotes != 0);
}


/*
 * @return true if the annotation of this SBML object has been set,
 * false otherwise.
 */
bool
SBase::isSetAnnotation () const
{
  const_cast <SBase *> (this)->syncAnnotation();
  return (mAnnotation != 0);
}


/*
 * @return true if the sboTerm has been set, false
 * otherwise.
 */
bool
SBase::isSetSBOTerm () const
{
  return (mSBOTerm != -1);
}


bool
SBase::isSetModelHistory()
{
  return (mHistory != 0);
}

/*
 * Sets the metaid field of the given SBML object to a copy of metaid.
 */
int
SBase::setMetaId (const std::string& metaid)
{
  if (getLevel() == 1)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else if (metaid.empty())
  {
    mMetaId.erase();
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!(SyntaxChecker::isValidXMLID(metaid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mMetaId = metaid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/** @cond doxygen-libsbml-internal */

/*
 * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
 *
 * Sets the id of this SBML object to a copy of sid.
 */
int
SBase::setId (const std::string& sid)
{
  SBMLTypeCode_t type = this->getTypeCode();

  switch(type)
  {
  case SBML_COMPARTMENT:
    return static_cast <Compartment *> (this)->setId(sid);
    break;

  case SBML_COMPARTMENT_TYPE:
    return static_cast <CompartmentType *> (this)->setId(sid);
    break;

  case SBML_EVENT:
    return static_cast <Event *> (this)->setId(sid);
    break;

  case SBML_EVENT_ASSIGNMENT:
    return static_cast <EventAssignment *> (this)->setVariable(sid);
    break;

  case SBML_FUNCTION_DEFINITION:
    return static_cast <FunctionDefinition *> (this)->setId(sid);
    break;

  case SBML_INITIAL_ASSIGNMENT:
    return static_cast <InitialAssignment *> (this)->setSymbol(sid);
    break;

  case SBML_MODEL:
    return static_cast <Model *> (this)->setId(sid);
    break;

  case SBML_PARAMETER:
    return static_cast <Parameter *> (this)->setId(sid);
    break;

  case SBML_LOCAL_PARAMETER:
    return static_cast <LocalParameter *> (this)->setId(sid);
    break;

  case SBML_REACTION:
    return static_cast <Reaction *> (this)->setId(sid);
    break;

  case SBML_SPECIES:
    return static_cast <Species *> (this)->setId(sid);
    break;

  case SBML_SPECIES_REFERENCE:
    return static_cast <SpeciesReference *> (this)->setId(sid);
    break;

  case SBML_SPECIES_TYPE:
    return static_cast <SpeciesType *> (this)->setId(sid);
    break;

  case SBML_MODIFIER_SPECIES_REFERENCE:
    return static_cast <ModifierSpeciesReference *> (this)->setId(sid);
    break;

  case SBML_UNIT_DEFINITION:
    return static_cast <UnitDefinition *> (this)->setId(sid);
    break;

  case SBML_ASSIGNMENT_RULE:
    return static_cast <AssignmentRule *> (this)->setVariable(sid);
    break;

  case SBML_RATE_RULE:
    return static_cast <RateRule *> (this)->setVariable(sid);
    break;

#ifdef USE_LAYOUT

  case SBML_LAYOUT_BOUNDINGBOX:
    return static_cast <BoundingBox *> (this)->setId(sid);
    break;

  case SBML_LAYOUT_GRAPHICALOBJECT:
    return static_cast <GraphicalObject *> (this)->setId(sid);
    break;

  case SBML_LAYOUT_LAYOUT:
    return static_cast <Layout *> (this)->setId(sid);
    break;

  case SBML_LAYOUT_LINESEGMENT:
    return static_cast <LineSegment *> (this)->setId(sid);
    break;
#endif  /* USE_LAYOUT */

  default:
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
    break;
  }
}


/*
 * NOTE: THIS IS FOR BACKWARD COMPATABILITY REASONS
 * Sets the name of this SBML object to a copy of name.
 */
int
SBase::setName (const std::string& name)
{
  SBMLTypeCode_t type = this->getTypeCode();

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
      switch(type)
      {
      case SBML_COMPARTMENT:
        return static_cast <Compartment *> (this)->setId(name);
        break;

      case SBML_MODEL:
        return static_cast <Model *> (this)->setId(name);
        break;

      case SBML_PARAMETER:
        return static_cast <Parameter *> (this)->setId(name);
        break;

      case SBML_LOCAL_PARAMETER:
        return static_cast <LocalParameter *> (this)->setId(name);
        break;

      case SBML_REACTION:
        return static_cast <Reaction *> (this)->setId(name);
        break;

      case SBML_SPECIES:
        return static_cast <Species *> (this)->setId(name);
        break;

      case SBML_SPECIES_REFERENCE:
        return static_cast <SpeciesReference *> (this)->setId(name);
        break;

      case SBML_UNIT_DEFINITION:
        return static_cast <UnitDefinition *> (this)->setId(name);
        break;

      case SBML_ASSIGNMENT_RULE:
        return static_cast <AssignmentRule *> (this)->setVariable(name);
        break;

      case SBML_RATE_RULE:
        return static_cast <RateRule *> (this)->setVariable(name);
        break;

    #ifdef USE_LAYOUT

      case SBML_LAYOUT_BOUNDINGBOX:
        static_cast <BoundingBox *> (this)->setId(name);
        return LIBSBML_OPERATION_SUCCESS;
        break;

      case SBML_LAYOUT_GRAPHICALOBJECT:
        static_cast <GraphicalObject *> (this)->setId(name);
        return LIBSBML_OPERATION_SUCCESS;
        break;

      case SBML_LAYOUT_LAYOUT:
        static_cast <Layout *> (this)->setId(name);
        return LIBSBML_OPERATION_SUCCESS;
        break;

      case SBML_LAYOUT_LINESEGMENT:
        static_cast <LineSegment *> (this)->setId(name);
        return LIBSBML_OPERATION_SUCCESS;
        break;
    #endif  /* USE_LAYOUT */

      default:
        return LIBSBML_UNEXPECTED_ATTRIBUTE;
        break;
      }
    }
  }
  else
  {
    switch(type)
    {
    case SBML_COMPARTMENT:
      return static_cast <Compartment *> (this)->setName(name);
      break;

    case SBML_COMPARTMENT_TYPE:
      return static_cast <CompartmentType *> (this)->setName(name);
      break;

    case SBML_EVENT:
      return static_cast <Event *> (this)->setName(name);
      break;

    case SBML_FUNCTION_DEFINITION:
      return static_cast <FunctionDefinition *> (this)->setName(name);
      break;

    case SBML_MODEL:
      return static_cast <Model *> (this)->setName(name);
      break;

    case SBML_PARAMETER:
      return static_cast <Parameter *> (this)->setName(name);
      break;

    case SBML_LOCAL_PARAMETER:
      return static_cast <LocalParameter *> (this)->setName(name);
      break;

    case SBML_REACTION:
      return static_cast <Reaction *> (this)->setName(name);
      break;

    case SBML_SPECIES:
      return static_cast <Species *> (this)->setName(name);
      break;

    case SBML_SPECIES_REFERENCE:
      return static_cast <SpeciesReference *> (this)->setName(name);
      break;

    case SBML_SPECIES_TYPE:
      return static_cast <SpeciesType *> (this)->setName(name);
      break;

    case SBML_MODIFIER_SPECIES_REFERENCE:
      return static_cast <ModifierSpeciesReference *> (this)->setName(name);
      break;

    case SBML_UNIT_DEFINITION:
      return static_cast <UnitDefinition *> (this)->setName(name);
      break;

    default:
      return LIBSBML_UNEXPECTED_ATTRIBUTE;
      break;
    }
  }
}


/** @endcond doxygen-libsbml-internal */


/*
 * Sets the annotation of this SBML object to a copy of annotation.
 */
int 
SBase::setAnnotation (const XMLNode* annotation)
{
  //
  // (*NOTICE*) 
  //
  // syncAnnotation() must not be invoked in this function.
  // 
  // 

  if (annotation == NULL)
  {
    delete mAnnotation;
    mAnnotation = 0;
  }
  //else if (!(math->isWellFormedASTNode()))
  //{
  //  return LIBSBML_INVALID_OBJECT;
  //}
  else if (mAnnotation != annotation)
  { 
    delete mAnnotation;
    // check for annotation tags and add if necessary
    const string&  name = annotation->getName();
    if (name != "annotation")
    {
      XMLToken ann_t = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
      mAnnotation = new XMLNode(ann_t);

      // The root node of the given XMLNode tree can be an empty XMLNode 
      // (i.e. neither start, end, nor text XMLNode) if the given annotation was 
      // converted from an XML string whose top level elements are neither 
      // "html" nor "body" and not enclosed with <annotation>..</annotation> tags
      // (e.g. <foo xmlns:foo="...">..</foo><bar xmlns:bar="...">..</bar> ) 
      if (!annotation->isStart() && !annotation->isEnd() && !annotation->isText()) 
      {
        for (unsigned int i=0; i < annotation->getNumChildren(); i++)
        {
          mAnnotation->addChild(annotation->getChild(i));
        }
      }
      else
      {
        mAnnotation->addChild(*annotation);
      }
    }
    else
    {
      mAnnotation = annotation->clone();
    }
  }

  //
  // delete existing mCVTerms
  //
  // existing CVTerms (if any) needs to be deleted at any rate, otherwise
  // unsetAnnotation() ( setAnnotation(NULL) ) doesn't work as expected.
  // (These functions must clear all elements in an annotation.)
  //
  if (mCVTerms)
  {
    // delete existing mCVTerms (if any)
    unsigned int size = mCVTerms->getSize();
    while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
    delete mCVTerms;
    mCVTerms = NULL;
  }


  if(mAnnotation && RDFAnnotationParser::hasCVTermRDFAnnotation(mAnnotation))
  {
    // parse mAnnotation (if any) and set mCVTerms 
    mCVTerms = new List();
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
  }
  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Sets the annotation (by string) of this SBML object to a copy of annotation.
 */
int
SBase::setAnnotation (const std::string& annotation)
{
  int success = LIBSBML_OPERATION_FAILED;

  //
  // (*NOTICE*) 
  //
  // syncAnnotation() must not be invoked in this function.
  // 
  // 

  if(annotation.empty()) 
  {
    unsetAnnotation();
    return LIBSBML_OPERATION_SUCCESS;
  }

  XMLNode* annt_xmln;

  // you might not have a document !!
  if (getSBMLDocument())
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns); 
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }

  if(annt_xmln)
  {
    success = setAnnotation(annt_xmln);
    delete annt_xmln;
  }
  return success;
}


/*
 * Appends annotation to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
int 
SBase::appendAnnotation (const XMLNode* annotation)
{
  int success = LIBSBML_OPERATION_FAILED;

  //
  // (*NOTICE*)
  //
  // syncAnnotation() doesn't need to be invoked in this function because
  // existing mCVTerm objects are properly merged in the following code.
  //

  if(!annotation) return LIBSBML_OPERATION_SUCCESS;

  XMLNode* new_annotation = NULL;
  const string&  name = annotation->getName();

  // check for annotation tags and add if necessary 
  if (name != "annotation")
  {
    XMLToken ann_t = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
    new_annotation = new XMLNode(ann_t);
    new_annotation->addChild(*annotation);
  }
  else
  {
    new_annotation = annotation->clone();
  }

  // parse new_annotation and add mCVTerms (if any) 
  if (RDFAnnotationParser::hasCVTermRDFAnnotation(new_annotation))
  {
    RDFAnnotationParser::parseRDFAnnotation(new_annotation,mCVTerms);
  }

  // delete RDFAnnotation (CVTerm and ModelHistory) from new_annotation 
//  XMLNode* tmp_annotation = RDFAnnotationParser::deleteRDFAnnotation(new_annotation);
//  delete new_annotation;
//  new_annotation = tmp_annotation;

  if (mAnnotation != 0)
  {
    // if mAnnotation is just <annotation/> need to tell
    // it to no longer be an end
    if (mAnnotation->isEnd())
    {
      mAnnotation->unsetEnd();
    }

    for(unsigned int i=0; i < new_annotation->getNumChildren(); i++)
    {
      if (new_annotation->getChild(i).getName() == "RDF")
      {
        if (RDFAnnotationParser::hasRDFAnnotation(mAnnotation))
        {
          unsigned int n = 0;
          while(n < mAnnotation->getNumChildren())
          {
            if (mAnnotation->getChild(n).getName() == "RDF")
            {
              break;
            }
            n++;
          }
          success = mAnnotation->getChild(n).addChild(
                                    new_annotation->getChild(i).getChild(0));
        }
        else
        {
          success = mAnnotation->addChild(new_annotation->getChild(i));
        }
      }
      else
      {
        success = mAnnotation->addChild(new_annotation->getChild(i));
      }
    }
  }
  else
  {
    success = setAnnotation(new_annotation);
  }

  delete new_annotation;

  return success;
}

/*
 * Appends annotation (by string) to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
int
SBase::appendAnnotation (const std::string& annotation)
{
  //
  // (*NOTICE*)
  //
  // syncAnnotation() doesn't need to be invoked in this function because
  // existing mCVTerm objects are properly merged in the following code.
  //

  int success = LIBSBML_OPERATION_FAILED;
  XMLNode* annt_xmln;
  if (getSBMLDocument())
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }
  
  if(annt_xmln)
  {
    success = appendAnnotation(annt_xmln);
    delete annt_xmln;
  }

  return success;
}



/*
 * Sets the notes of this SBML object to a copy of notes.
 */
int 
SBase::setNotes(const XMLNode* notes)
{
  if (mNotes == notes) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (notes == NULL)
  {
    delete mNotes;
    mNotes = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }

  delete mNotes;
  const string&  name = notes->getName();

  /* check for notes tags and add if necessary */

  if (name == "notes")
  {
    mNotes = static_cast<XMLNode*>( notes->clone() );
  }
  else
  {
    XMLToken notes_t = XMLToken(XMLTriple("notes", "", ""), 
                                XMLAttributes());
    mNotes = new XMLNode(notes_t);
  
    // The root node of the given XMLNode tree can be an empty XMLNode 
    // (i.e. neither start, end, nor text XMLNode) if the given notes was 
    // converted from an XML string whose top level elements are neither 
    // "html" nor "body" and not enclosed with <notes>..</notes> tag 
    // (e.g. <p ...>..</p><br/>).
    if (!notes->isStart() && !notes->isEnd() && !notes->isText() ) 
    {
      for (unsigned int i=0; i < notes->getNumChildren(); i++)
      {
        if (mNotes->addChild(notes->getChild(i)) < 0)
        {
          return LIBSBML_OPERATION_FAILED;
        }
      }
    }
    else
    {
      if (mNotes->addChild(*notes) < 0)
        return LIBSBML_OPERATION_FAILED;
    }
  }
  
  // in L2v2 and beyond the XHTML content of notes is restricted
  // but I need the notes tag to use the function
  // so I havent tested it until now
  if (getLevel() > 2 
    || (getLevel() == 2 && getVersion() > 1))
  {
    if (!SyntaxChecker::hasExpectedXHTMLSyntax(mNotes, getSBMLNamespaces()))
    {
      delete mNotes;
      mNotes = 0;
      return LIBSBML_INVALID_OBJECT;
    }
  }

  return LIBSBML_OPERATION_SUCCESS;

}

/*
 * Sets the notes (by std::string) of this SBML object to a copy of notes.
 */
int
SBase::setNotes(const std::string& notes)
{
  int success = LIBSBML_OPERATION_FAILED;
  if (notes.empty())
  {
    success = unsetNotes();
  }
  else
  {
    XMLNode* notes_xmln;

    // you might not have a document !!
    if (getSBMLDocument())
    {
      XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
      notes_xmln = XMLNode::convertStringToXMLNode(notes,xmlns); 
    }
    else
    {
      notes_xmln = XMLNode::convertStringToXMLNode(notes);
    }

    if(notes_xmln)
    {
      success = setNotes(notes_xmln);
      delete notes_xmln;
    }
  }
  return success;
}


/*
 * Appends notes to the existing notes.
 * This allows other notes to be preserved whilst
 * adding additional information.
 */
int 
SBase::appendNotes(const XMLNode* notes)
{
  int success = LIBSBML_OPERATION_FAILED;
  if(!notes) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  const string&  name = notes->getName();

  // The content of notes in SBML can consist only of the following 
  // possibilities:
  //
  //  1. A complete XHTML document (minus the XML and DOCTYPE 
  //     declarations), that is, XHTML content beginning with the 
  //     html tag.
  //     (_NotesType is _ANotesHTML.)
  //
  //  2. The body element from an XHTML document.
  //     (_NotesType is _ANotesBody.) 
  //
  //  3. Any XHTML content that would be permitted within a body 
  //     element, each one must declare the XML namespace separately.
  //     (_NotesType is _ANotesAny.) 
  //

  typedef enum { _ANotesHTML, _ANotesBody, _ANotesAny } _NotesType;

  _NotesType addedNotesType = _ANotesAny; 
  XMLNode   addedNotes;

  //------------------------------------------------------------
  //
  // STEP1 : identifies the type of the given notes
  //
  //------------------------------------------------------------

  if (name == "notes")
  {
    /* check for notes tags on the added notes and strip if present and
       the notes tag has "html" or "body" element */

    if (notes->getNumChildren() > 0)  
    { 
      // notes->getChild(0) must be "html", "body", or any XHTML
      // element that would be permitted within a "body" element 
      // (e.g. <p>..</p>,  <br>..</br> and so forth).

      const string& cname = notes->getChild(0).getName();

      if (cname == "html")
      {
        addedNotes = notes->getChild(0);
        addedNotesType = _ANotesHTML;
      }
      else if (cname == "body") 
      {
        addedNotes = notes->getChild(0);
        addedNotesType = _ANotesBody;
      }
      else
      {
        // the notes tag must NOT be stripped if notes->getChild(0) node 
        // is neither "html" nor "body" element because the children of 
        // the addedNotes will be added to the curNotes later if the node 
        // is neither "html" nor "body".
        addedNotes = *notes;
        addedNotesType = _ANotesAny;
      }
    }
    else
    {
      // the given notes is empty 
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    // if the XMLNode argument notes has been created from a string and 
    // it is a set of subelements there may be a single empty node
    // as parent - leaving this in doesnt affect the writing out of notes
    // but messes up the check for correct syntax
    if (!notes->isStart() && !notes->isEnd() && !notes->isText() ) 
    {
      if (notes->getNumChildren() > 0)
      { 
        addedNotes = *notes;
        addedNotesType = _ANotesAny;
      }
      else
      {
        // the given notes is empty 
        return LIBSBML_OPERATION_SUCCESS;
      }
    }
    else
    {
      if (name == "html")
      {
        addedNotes = *notes;
        addedNotesType = _ANotesHTML;
      }
      else if (name == "body")
      {
        addedNotes = *notes;
        addedNotesType = _ANotesBody;
      }
      else
      {
        // The given notes node needs to be added to a parent node
        // if the node is neither "html" nor "body" element because the 
        // children of addedNotes will be added to the curNotes later if the 
        // node is neither "html" nor "body" (i.e. any XHTML element that 
        // would be permitted within a "body" element)
        addedNotes.addChild(*notes);
        addedNotesType = _ANotesAny;
      }
    }
  }

  //
  // checks the addedNotes of "html" if the html tag contains "head" and 
  // "body" tags which must be located in this order.
  //
  if (addedNotesType == _ANotesHTML)
  {
    if ((addedNotes.getNumChildren() != 2) ||
        ( (addedNotes.getChild(0).getName() != "head") ||
          (addedNotes.getChild(1).getName() != "body")
        )
       )
    {
      return LIBSBML_INVALID_OBJECT;
    }
  }

  // check whether notes is valid xhtml
  if (getLevel() > 2 
    || (getLevel() == 2 && getVersion() > 1))
  {
    XMLNode tmpNotes(XMLTriple("notes","",""), XMLAttributes());

    if (addedNotesType == _ANotesAny)
    {
      for (unsigned int i=0; i < addedNotes.getNumChildren(); i++)
      {
        tmpNotes.addChild(addedNotes.getChild(i));
      }
    }
    else
    {
      tmpNotes.addChild(addedNotes);
    }

    if (!SyntaxChecker::hasExpectedXHTMLSyntax(&tmpNotes, getSBMLNamespaces()))
    {
      return LIBSBML_INVALID_OBJECT;
    }
  }


  if ( mNotes )
  {
    //------------------------------------------------------------
    //
    //  STEP2: identifies the type of the existing notes 
    //
    //------------------------------------------------------------

    _NotesType curNotesType   = _ANotesAny; 
    XMLNode&  curNotes = *mNotes;

    // curNotes.getChild(0) must be "html", "body", or any XHTML
    // element that would be permitted within a "body" element .

    const string& cname = curNotes.getChild(0).getName();
  
    if (cname == "html")
    {
      XMLNode& curHTML = curNotes.getChild(0);
      //
      // checks the curHTML if the html tag contains "head" and "body" tags
      // which must be located in this order, otherwise nothing will be done.
      //
      if ((curHTML.getNumChildren() != 2) ||
          ( (curHTML.getChild(0).getName() != "head") ||
            (curHTML.getChild(1).getName() != "body")
          )
         )
      {
        return LIBSBML_INVALID_OBJECT;
      }
      curNotesType = _ANotesHTML;
    }
    else if (cname == "body") 
    {
      curNotesType = _ANotesBody;
    }
    else
    {
      curNotesType = _ANotesAny;
    }
  
    /*
     * BUT we also have the issue of the rules relating to notes
     * contents and where to add them ie we cannot add a second body element
     * etc...
     */

    //------------------------------------------------------------
    //
    //  STEP3: appends the given notes to the current notes
    //
    //------------------------------------------------------------
  
    unsigned int i;
  
    if (curNotesType == _ANotesHTML)
    {
      XMLNode& curHTML = curNotes.getChild(0); 
      XMLNode& curBody = curHTML.getChild(1);
      
      if (addedNotesType == _ANotesHTML)
      {
        // adds the given html tag to the current html tag
  
        XMLNode& addedBody = addedNotes.getChild(1);   
  
        for (i=0; i < addedBody.getNumChildren(); i++)
        {
          if (curBody.addChild(addedBody.getChild(i)) < 0 )
            return LIBSBML_OPERATION_FAILED;          
        }
      }
      else if ((addedNotesType == _ANotesBody) || (addedNotesType == _ANotesAny))
      {
        // adds the given body or other tag (permitted in the body) to the current 
        // html tag
  
        for (i=0; i < addedNotes.getNumChildren(); i++)
        {
          if (curBody.addChild(addedNotes.getChild(i)) < 0 )
            return LIBSBML_OPERATION_FAILED;
        }
      }
      success = LIBSBML_OPERATION_SUCCESS;
    }
    else if (curNotesType == _ANotesBody)
    {
      if (addedNotesType == _ANotesHTML)
      {
        // adds the given html tag to the current body tag
  
        XMLNode  addedHTML(addedNotes);
        XMLNode& addedBody = addedHTML.getChild(1);
        XMLNode& curBody   = curNotes.getChild(0);
  
        for (i=0; i < curBody.getNumChildren(); i++)
        {
          addedBody.insertChild(i,curBody.getChild(i));
        }
        
        curNotes.removeChildren();
        if (curNotes.addChild(addedHTML) < 0)
          return LIBSBML_OPERATION_FAILED;
      }
      else if ((addedNotesType == _ANotesBody) || (addedNotesType == _ANotesAny))
      {
        // adds the given body or other tag (permitted in the body) to the current 
        // body tag
  
        XMLNode& curBody = curNotes.getChild(0);
  
        for (i=0; i < addedNotes.getNumChildren(); i++)
        {
          if (curBody.addChild(addedNotes.getChild(i)) < 0)
            return LIBSBML_OPERATION_FAILED;
        }
      }
      success = LIBSBML_OPERATION_SUCCESS;
    }
    else if (curNotesType == _ANotesAny)
    {
      if (addedNotesType == _ANotesHTML)
      {
        // adds the given html tag to the current any tag permitted in the body.
  
        XMLNode  addedHTML(addedNotes);
        XMLNode& addedBody = addedHTML.getChild(1);
  
        for (i=0; i < curNotes.getNumChildren(); i++)
        {
          addedBody.insertChild(i,curNotes.getChild(i));
        }
  
        curNotes.removeChildren();
        if (curNotes.addChild(addedHTML) < 0)
          return LIBSBML_OPERATION_FAILED;
      }
      else if (addedNotesType == _ANotesBody)
      {
        // adds the given body tag to the current any tag permitted in the body.
  
        XMLNode addedBody(addedNotes);
  
        for (i=0; i < curNotes.getNumChildren(); i++)
        {
          addedBody.insertChild(i,curNotes.getChild(i));
        }
  
        curNotes.removeChildren();
        if (curNotes.addChild(addedBody) < 0)
          return LIBSBML_OPERATION_FAILED;
      }
      else if (addedNotesType == _ANotesAny)
      {
        // adds the given any tag permitted in the boy to that of the current 
        // any tag.
  
        for (i=0; i < addedNotes.getNumChildren(); i++)
        {
          if (curNotes.addChild(addedNotes.getChild(i)) < 0)
            return LIBSBML_OPERATION_FAILED;
        }
      }
      success = LIBSBML_OPERATION_SUCCESS;
    }
  }
  else // if (mNotes == NULL)
  {
    // setNotes accepts XMLNode with/without top level notes tags.
    success = setNotes(notes);
  }

  return success;
}

/*
 * Appends notes (by string) to the existing notes.
 * This allows other notes to be preserved whilst
 * adding additional information.
 */
int
SBase::appendNotes(const std::string& notes)
{
  int success = LIBSBML_OPERATION_FAILED;
  if (notes.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  XMLNode* notes_xmln = XMLNode::convertStringToXMLNode(notes);
  if(notes_xmln)
  {
    success = appendNotes(notes_xmln);
    delete notes_xmln;
  }
  return success;
}

int
SBase::setModelHistory(ModelHistory * history)
{
  /* ModelHistory is only allowed on Model in L2
   * but on any element in L3
   */
  if (getLevel() < 3 && getTypeCode() != SBML_MODEL)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  if (mHistory == history) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (history == NULL)
  {
    delete mHistory;
    mHistory = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (!(history->hasRequiredAttributes()))
  {
    delete mHistory;
    mHistory = 0;
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    delete mHistory;
    mHistory = static_cast<ModelHistory*>( history->clone() );
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/** @cond doxygen-libsbml-internal */

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
SBase::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
}


/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
SBase::setParentSBMLObject (SBase* sb)
{
  mParentSBMLObject = sb;
}
/** @endcond doxygen-libsbml-internal */

SBase* 
SBase::getAncestorOfType(SBMLTypeCode_t type)
{
  if (type == SBML_DOCUMENT)
    return getSBMLDocument();

  SBase *child = this;
  SBase *parent = getParentSBMLObject();

  while (parent != NULL && parent->getTypeCode() != SBML_DOCUMENT)
  {
    if (parent->getTypeCode() == type)
      return parent;
    else
    {
      child = parent;
      parent = child->getParentSBMLObject();
    }
  }

  // if we get here we havent found an ancestor of this type
  return NULL;

}


/*
 * Sets the sboTerm field to value.
 */
int
SBase::setSBOTerm (int value)
{
  if (getLevel() < 2 || (getLevel() == 2 && getVersion() < 2))
  {
    mSBOTerm = -1;
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    if ( !SBO::checkTerm(value) )
    {
      mSBOTerm = -1;
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    mSBOTerm = value;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Sets the sboTerm field to value converted from the given string.
 */
int
SBase::setSBOTerm (const std::string &sboid)
{
  return setSBOTerm(SBO::stringToInt(sboid));
}


/*
 * Sets the namespaces relevant of this SBML object.
 *
 * @param xmlns the namespaces to set
 */
int 
SBase::setNamespaces(XMLNamespaces* xmlns)
{
  if (xmlns == NULL)
  {
    mSBMLNamespaces->setNamespaces(NULL);
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    mSBMLNamespaces->setNamespaces(xmlns);
    return LIBSBML_OPERATION_SUCCESS;
  }
}



/*
 * Unsets the metaid of this SBML object.
 */
int
SBase::unsetMetaId ()
{
  /* only in L2 onwards */
  if (getLevel() < 2)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  mMetaId.erase();

  if (mMetaId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


///*
// * Unsets the id of this SBML object.
// */
//int
//SBase::unsetId ()
//{
//  mId.erase();
//
//  if (mId.empty())
//  {
//    return LIBSBML_OPERATION_SUCCESS;
//  }
//  else
//  {
//    return LIBSBML_OPERATION_FAILED;
//  }
//}
//
//
///*
// * Unsets the name of this SBML object.
// */
//int
//SBase::unsetName ()
//{
//  if (getLevel() == 1) 
//  {
//    mId.erase();
//  }
//  else 
//  {
//    mName.erase();
//  }
//
//  if (getLevel() == 1 && mId.empty())
//  {
//    return LIBSBML_OPERATION_SUCCESS;
//  }
//  else if (mName.empty())
//  {
//    return LIBSBML_OPERATION_SUCCESS;
//  }
//  else
//  {
//    return LIBSBML_OPERATION_FAILED;
//  }
//}
//
//
/*
 * Unsets the notes of this SBML object.
 */
int
SBase::unsetNotes ()
{
  delete mNotes;
  mNotes = 0;
  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Unsets the annotation of this SBML object.
 */
int
SBase::unsetAnnotation ()
{
  XMLNode* empty = NULL;
  return setAnnotation(empty);
}


/*
 * Unsets the sboTerm of this SBML object.
 */
int
SBase::unsetSBOTerm ()
{
  if (getLevel() < 2 || (getLevel() == 2 && getVersion() < 2))
  {
    mSBOTerm = -1;
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mSBOTerm = -1;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Adds a copy of the given CVTerm to this SBML object.
 */
int
SBase::addCVTerm(CVTerm * term)
{
  unsigned int added = 0;
  // shouldnt add a CVTerm to an object with no metaid 
  if (!isSetMetaId())
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  if (term == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!term->hasRequiredAttributes())
  {
    return LIBSBML_INVALID_OBJECT;
  }
  
  /* clone the term to be added so that I can adjust 
   * which resources are actually added
   */
  CVTerm * copyTerm = term->clone();

  if (mCVTerms == NULL)
  {
    mCVTerms = new List();
    mCVTerms->add((void *) term->clone());
  }
  else
  {
    /* check whether the resources are already listed */
    QualifierType_t type = copyTerm->getQualifierType();
    if (type == BIOLOGICAL_QUALIFIER)
    {
      BiolQualifierType_t biolQual = BQB_UNKNOWN;
      int length = copyTerm->getResources()->getLength();
      for (int p = length-1; p > -1; p--)
      {
        biolQual = getResourceBiologicalQualifier(copyTerm->getResources()->getValue(p));
        if (biolQual != BQB_UNKNOWN)
        {
          /* resource is already present 
           * - dont want to add again; 
           * so delete it from set to be added
           */
          copyTerm->removeResource(copyTerm->getResources()->getValue(p));
        }
      }
    }
    else if (type == MODEL_QUALIFIER)
    {
      ModelQualifierType_t modelQual = BQM_UNKNOWN;
      int length = copyTerm->getResources()->getLength();
      for (int p = length-1; p > -1; p--)
      {
        modelQual = getResourceModelQualifier(copyTerm->getResources()->getValue(p));
        if (modelQual != BQM_UNKNOWN)
        {
          /* resource is already present 
           * - dont want to add again; 
           * so delete it from set to be added
           */
          copyTerm->removeResource(copyTerm->getResources()->getValue(p));
        }
      }
    }

    /* if the qualifier of the term being added is already present
     * add to the list of resources for that qualifier
     */
    if (type == BIOLOGICAL_QUALIFIER)
    {
      BiolQualifierType_t biol = copyTerm->getBiologicalQualifierType();
      
      for (unsigned int n = 0; n < mCVTerms->getSize() && added == 0; n++)
      {
        if (biol == static_cast <CVTerm *>(mCVTerms->get(n))->getBiologicalQualifierType())
        {
          for (int r = 0; r < copyTerm->getResources()->getLength(); r++)
          {
            static_cast <CVTerm *>(mCVTerms->get(n))->addResource(
                copyTerm->getResources()->getValue(r));
          }
          added = 1;
        }
      }
    }
    else if (type == MODEL_QUALIFIER)
    {
      ModelQualifierType_t model = copyTerm->getModelQualifierType();
      
      for (unsigned int n = 0; n < mCVTerms->getSize() && added == 0; n++)
      {
        if (model == static_cast <CVTerm *>(mCVTerms->get(n))->getModelQualifierType())
        {
          for (int r = 0; r < copyTerm->getResources()->getLength(); r++)
          {
            static_cast <CVTerm *>(mCVTerms->get(n))->addResource(
                copyTerm->getResources()->getValue(r));
          }
          added = 1;
        }
      }
    }
    if (added == 0 && copyTerm->getResources()->getLength() > 0)
    {
      /* no matching copyTerms already in list */
      mCVTerms->add((void *) copyTerm->clone());
    }

  }

  delete copyTerm;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * @return the list of CVTerms for this SBML object.
 */
List*
SBase::getCVTerms()
{
  return mCVTerms;
}


/*
 * @return the list of CVTerms for this SBML object.
 */
List*
SBase::getCVTerms() const
{
  return mCVTerms;
}

/*
 * Returns the number of CVTerm objects in the annotations of this SBML
 * object.
 * 
 * @return the number of CVTerms for this SBML object.
 */
unsigned int 
SBase::getNumCVTerms()
{
  if (mCVTerms)
  {
    return mCVTerms->getSize();
  }
  else
  {
    return 0;
  }
}


/*
 * Returns the nth CVTerm in the list of CVTerms of this SBML
 * object.
 * 
 * @param n unsigned int the index of the CVTerm to retrieve
 *
 * @return the nth CVTerm in the list of CVTerms for this SBML object.
 */
CVTerm* 
SBase::getCVTerm(unsigned int n)
{
  return (mCVTerms) ? static_cast <CVTerm*> (mCVTerms->get(n)) : 0;
}


/*
 * Clears the list of CVTerms of this SBML
 * object.
 */
int 
SBase::unsetCVTerms()
{
  if (mCVTerms)
  {  
    unsigned int size = mCVTerms->getSize();
    while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
    delete mCVTerms;
  }
  mCVTerms = 0;
  
  if (mCVTerms)
    return LIBSBML_OPERATION_FAILED;
  else
    return LIBSBML_OPERATION_SUCCESS;
}


int 
SBase::unsetModelHistory()
{
  delete mHistory;
  mHistory = 0;

  /* ModelHistory is only allowed on Model in L2
   * but on any element in L3
   */
  if (getLevel() < 3 && getTypeCode() != SBML_MODEL)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  if (mHistory)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Returns the BiologicalQualifier associated with this resource,
 * an empty string if the resource does not exist.
 *
 * @param resource string representing the resource; e.g.,
 * "http://www.geneontology.org/#GO:0005892"
 *
 * @return the BiolQualifierType_t associated with the resource
 */
BiolQualifierType_t 
SBase::getResourceBiologicalQualifier(std::string resource)
{
  if (mCVTerms)
  {
    for (unsigned int n = 0; n < mCVTerms->getSize(); n++)
    {
      // does this term have a biological qualifier
      if (static_cast <CVTerm *>(mCVTerms->get(n))->getQualifierType() 
                                                              == BIOLOGICAL_QUALIFIER)
      {
        // check whether given resource is present
        for (int r = 0; 
          r < static_cast <CVTerm *>(mCVTerms->get(n))->getResources()->getLength(); r++)
        {
          if (resource == 
            static_cast <CVTerm *>(mCVTerms->get(n))->getResources()->getValue(r))
          {
            return static_cast <CVTerm *>(mCVTerms->get(n))->getBiologicalQualifierType();
          }
        }
      }
    }
  }

  return BQB_UNKNOWN;
}

/*
 * Returns the ModelQualifier associated with this resource,
 * an empty string if the resource does not exist.
 *
 * @param resource string representing the resource; e.g.,
 * "http://www.geneontology.org/#GO:0005892"
 *
 * @return the ModelQualifierType_t associated with the resource
 */
ModelQualifierType_t 
SBase::getResourceModelQualifier(std::string resource)
{
  if (mCVTerms)
  {
    for (unsigned int n = 0; n < mCVTerms->getSize(); n++)
    {
      // does this term have a biological qualifier
      if (static_cast <CVTerm *>(mCVTerms->get(n))->getQualifierType() 
                                                              == MODEL_QUALIFIER)
      {
        // check whether given resource is present
        for (int r = 0; 
          r < static_cast <CVTerm *>(mCVTerms->get(n))->getResources()->getLength(); r++)
        {
          if (resource == 
            static_cast <CVTerm *>(mCVTerms->get(n))->getResources()->getValue(r))
          {
            return static_cast <CVTerm *>(mCVTerms->get(n))->getModelQualifierType();
          }
        }
      }
    }
  }

  return BQM_UNKNOWN;
}


/*
 * @return the parent Model of this SBML object.
 */
const Model*
SBase::getModel () const
{
  return (mSBML != 0) ? mSBML->getModel() : 0;
}


/*
 * @return the SBML level of this SBML object.
 */
unsigned int
SBase::getLevel () const
{
  if (mSBML)
    return mSBML->mLevel;
  else if (mSBMLNamespaces != 0)
    return mSBMLNamespaces->getLevel();
  else
    return SBMLDocument::getDefaultLevel();
}


/*
 * @return the SBML version of this SBML object.
 */
unsigned int
SBase::getVersion () const
{
  if (mSBML)
    return mSBML->mVersion;
  else if (mSBMLNamespaces != 0)
    return mSBMLNamespaces->getVersion();
  else
    return SBMLDocument::getDefaultVersion();
}


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * This method MAY return the typecode of this SBML object or it MAY
 * return SBML_UNKNOWN.  That is, subclasses of SBase are not required to
 * implement this method to return a typecode.  This method is meant
 * primarily for the LibSBML C interface where class and subclass
 * information is not readily available.
 *
 * @see getElementName()
 */
SBMLTypeCode_t
SBase::getTypeCode () const
{
  return SBML_UNKNOWN;
}

bool 
SBase::hasValidLevelVersionNamespaceCombination()
{
  bool valid = true;
  bool sbmlDeclared = false;
  std::string declaredURI("");
  unsigned int index = 0;
  unsigned int version = getVersion();
  XMLNamespaces *xmlns = getNamespaces();
  if (xmlns)
  {
    // 
    // checks defined SBML XMLNamespace
    // returns false if different SBML XMLNamespaces 
    // (e.g. SBML_XMLNS_L2V1 and SBML_XMLNS_L2V3) are defined.
    //
    int numNS = 0;

    if (xmlns->hasURI(SBML_XMLNS_L3V1))
    {
      ++numNS;
      declaredURI.assign(SBML_XMLNS_L3V1);
    }

    if (xmlns->hasURI(SBML_XMLNS_L2V4))
    {
      ++numNS;
      declaredURI.assign(SBML_XMLNS_L2V4);
    }

    if (xmlns->hasURI(SBML_XMLNS_L2V3))
    {
      // checks different SBML XMLNamespaces
      if (numNS > 0) return false;
      ++numNS;
      declaredURI.assign(SBML_XMLNS_L2V3);
    }

    if (xmlns->hasURI(SBML_XMLNS_L2V2))
    {
      // checks different SBML XMLNamespaces
      if (numNS > 0) return false;
      ++numNS;
      declaredURI.assign(SBML_XMLNS_L2V2);
    }

    if (xmlns->hasURI(SBML_XMLNS_L2V1))
    {
      // checks different SBML XMLNamespaces
      if (numNS > 0) return false;
      ++numNS;
      declaredURI.assign(SBML_XMLNS_L2V1);
    }

    if (xmlns->hasURI(SBML_XMLNS_L1))
    {
      // checks different SBML XMLNamespaces
      if (numNS > 0) return false;
      ++numNS;
      declaredURI.assign(SBML_XMLNS_L1);
    }

    // checks if the SBML Namespace is explicitly defined.
    for (int i=0; i < xmlns->getLength(); i++)
    {
      if (!xmlns->getPrefix(i).empty() && 
                      xmlns->getURI(i) == declaredURI)
      {
        sbmlDeclared = true;
        break;
      }
    }

  }

  SBMLTypeCode_t typecode = getTypeCode();

  switch (getLevel())
  {
    case 1:
      // some components didnt exist in level 1
      if ( typecode == SBML_COMPARTMENT_TYPE
        || typecode == SBML_CONSTRAINT
        || typecode == SBML_EVENT
        || typecode == SBML_EVENT_ASSIGNMENT
        || typecode == SBML_FUNCTION_DEFINITION
        || typecode == SBML_INITIAL_ASSIGNMENT
        || typecode == SBML_SPECIES_TYPE
        || typecode == SBML_MODIFIER_SPECIES_REFERENCE
        || typecode == SBML_TRIGGER
        || typecode == SBML_DELAY
        || typecode == SBML_STOICHIOMETRY_MATH)
        valid = false;
     switch (version)
      {
        case 1:
        case 2:
          // the namespaces contains the sbml namespaces
          // check it is the correct ns for the level/version
          if (sbmlDeclared)
          {
            if (declaredURI != string(SBML_XMLNS_L1))
            {
              valid = false;
            }
          }
          break;
        default:
          valid = false;
          break;
        }
      break;
    case 2:
      //if (typecode == SBML_LOCAL_PARAMETER)
      //  valid = false;
      switch (version)
      {
        case 1:
          // some components didnt exist in l2v1
          if ( typecode == SBML_COMPARTMENT_TYPE
            || typecode == SBML_CONSTRAINT
            || typecode == SBML_INITIAL_ASSIGNMENT
            || typecode == SBML_SPECIES_TYPE)
            valid = false;
         // the namespaces contains the sbml namespaces
          // check it is the correct ns for the level/version
          if (sbmlDeclared)
          {
            if (declaredURI != string(SBML_XMLNS_L2V1))
            {
              valid = false;
            }
          }
          break;
        case 2:
          // the namespaces contains the sbml namespaces
          // check it is the correct ns for the level/version
          if (sbmlDeclared)
          {
            if (declaredURI != string(SBML_XMLNS_L2V2))
            {
              valid = false;
            }
          }
          break;
        case 3:
          // the namespaces contains the sbml namespaces
          // check it is the correct ns for the level/version
          if (sbmlDeclared)
          {
            if (declaredURI != string(SBML_XMLNS_L2V3))
            {
              valid = false;
            }
          }
          break;
        case 4:
          // the namespaces contains the sbml namespaces
          // check it is the correct ns for the level/version
          if (sbmlDeclared)
          {
            if (declaredURI != string(SBML_XMLNS_L2V4))
            {
              valid = false;
            }
          }
          break;
        default:
          valid = false;
          break;
        }
      break;
    case 3:
      // some components no longer exist in level 3
      if ( typecode == SBML_COMPARTMENT_TYPE
        || typecode == SBML_SPECIES_TYPE
        || typecode == SBML_STOICHIOMETRY_MATH)
        valid = false;
      switch (version)
      {
        case 1:
         // the namespaces contains the sbml namespaces
          // check it is the correct ns for the level/version
          if (sbmlDeclared)
          {
            if (declaredURI != string(SBML_XMLNS_L3V1))
            {
              valid = false;
            }
          }
          break;
        default:
          valid = false;
          break;
      }
      break;
    default:
      valid = false;
      break;
  }

  return valid;
}

/** @cond doxygen-libsbml-internal */

/* sets the SBMLnamespaces - internal use only*/
void 
SBase::setSBMLNamespaces(SBMLNamespaces * sbmlns)
{
  delete mSBMLNamespaces;
  if (sbmlns)
    mSBMLNamespaces = sbmlns->clone();
  else
    mSBMLNamespaces = 0;
}

/* gets the SBMLnamespaces - internal use only*/
SBMLNamespaces *
SBase::getSBMLNamespaces() const
{
  if (mSBML)
    return mSBML->mSBMLNamespaces;
  else if (mSBMLNamespaces != 0)
    return mSBMLNamespaces;
  else
    return new SBMLNamespaces();
}

/** @endcond doxygen-libsbml-internal */



/*
 * @return the partial SBML that describes this SBML object.
 */
char*
SBase::toSBML ()
{
  ostringstream    os;
  XMLOutputStream  stream(os, "UTF-8", false);

  write(stream);

  return safe_strdup( os.str().c_str() );
}


/** @cond doxygen-libsbml-internal */
/*
 * Reads (initializes) this SBML object by reading from XMLInputStream.
 */
void
SBase::read (XMLInputStream& stream)
{
  if ( !stream.peek().isStart() ) return;

  const XMLToken  element  = stream.next();
  int             position =  0;

  setSBaseFields( element );
  readAttributes( element.getAttributes() );

  /* if we are reading a document pass the
   * SBML Namespace information to the input stream object
   * thus the MathML reader can find out what level/version
   * of SBML it is parsing
   */
  if (element.getName() == "sbml")
  {
    stream.setSBMLNamespaces(this->getSBMLNamespaces());
  }
  else
  {
    //
    // checks if the given default namespace (if any) is a valid
    // SBML namespace
    //
    checkDefaultNamespace(mSBMLNamespaces->getNamespaces(), element.getName());
  }

  if ( element.isEnd() ) return;

  while ( stream.isGood() )
  {
    stream.skipText();
    const XMLToken& next = stream.peek();

    // Re-check stream.isGood() because stream.peek() could hit something.
    if ( !stream.isGood() ) break;

    if ( next.isEndFor(element) )
    {
      stream.next();
      break;
    }
    else if ( next.isStart() )
    {
      SBase * object = createObject(stream);

      if (object)
      {
        checkOrderAndLogError(object, position);
        position = object->getElementPosition();

        object->setSBMLDocument(mSBML);
        object->setParentSBMLObject(static_cast <SBase*>(this));

        object->read(stream);

        if ( !stream.isGood() ) break;

        if (object->getTypeCode() == SBML_SPECIES_REFERENCE 
            && object->getLevel() == 2)
        {
          SpeciesReference* spr = static_cast <SpeciesReference *> (object);
          //
          // initL2Stoichiometry() must be invoked before sortMath().
          //
          spr->initL2Stoichiometry();
          spr->sortMath();
        }
        checkListOfPopulated(object);
      }
      else if ( !( readOtherXML(stream)
                   || readAnnotation(stream)
                   || readNotes(stream) ))
      {
        logUnknownElement(next.getName(), getLevel(), getVersion());
        stream.skipPastEnd( stream.next() );
      }
    }
    else
    {
      stream.skipPastEnd( stream.next() );
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Writes (serializes) this SBML object by writing it to XMLOutputStream.
 */
void
SBase::write (XMLOutputStream& stream) const
{
  stream.startElement( getElementName() );

  writeAttributes( stream );
  writeElements  ( stream );

  stream.endElement( getElementName() );
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SBase::writeElements (XMLOutputStream& stream) const
{
  if ( mNotes      ) stream << *mNotes;

  /*
   * NOTE: CVTerms on a model have already been dealt with
   */

  const_cast <SBase *> (this)->syncAnnotation();
  if (mAnnotation) stream << *mAnnotation;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to create, store, and then
 * return an SBML object corresponding to the next XMLToken in the
 * XMLInputStream.
 *
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SBase::createObject (XMLInputStream&)
{
  return 0;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
SBase::readOtherXML (XMLInputStream&)
{
  return false;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return true if read an <annotation> element from the stream
 */
bool
SBase::readAnnotation (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();

  if (name == "annotation" 
    || (getLevel() == 1 && getVersion() == 1 && name == "annotations"))
  {
//    XMLNode* new_annotation = NULL;
    // If this is a level 1 document then annotations are not allowed on
    // the sbml container
    if (getLevel() == 1 && getTypeCode() == SBML_DOCUMENT)
    {
      logError(AnnotationNotesNotAllowedLevel1);
    }


    // If an annotation already exists, log it as an error and replace
    // the content of the existing annotation with the new one.

    if (mAnnotation)
    {
      if (getLevel() < 3) 
      {
        logError(NotSchemaConformant, getLevel(), getVersion(),
	        "Only one <annotation> element is permitted inside a "
	        "particular containing element.");
      }
      else
      {
        logError(MultipleAnnotations, getLevel(), getVersion());
      }
    }

    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    if(mCVTerms)
    {
      unsigned int size = mCVTerms->getSize();
      while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
      delete mCVTerms; 
    }
    mCVTerms = new List();
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
//    new_annotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
//    delete mAnnotation;
//    mAnnotation = new_annotation;
    return true;
  }

  return false;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return true if read a <notes> element from the stream
 */
bool
SBase::readNotes (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();

  if (name == "notes")
  {
    // If this is a level 1 document then notes are not allowed on
    // the sbml container
    if (getLevel() == 1 && getTypeCode() == SBML_DOCUMENT)
    {
      logError(AnnotationNotesNotAllowedLevel1);
    }

    // If a notes element already exists, then it is an error.
    // If an annotation element already exists, then the ordering is wrong.
    // In either case, replace existing content with the new notes read.

    if (mNotes)
    {
      if (getLevel() < 3)
      {
        logError(NotSchemaConformant, getLevel(), getVersion(),
                "Only one <notes> element is permitted inside a "
	        "particualr containing element.");
      }
      else
      {
        logError(OnlyOneNotesElementAllowed, getLevel(), getVersion());
      }
    }
    else if (mAnnotation)
    {
      logError(NotSchemaConformant, getLevel(), getVersion(),
               "Incorrect ordering of <annotation> and <notes> elements -- "
	       "<notes> must come before <annotation> due to the way that "
	       "the XML Schema for SBML is defined.");
    }

    delete mNotes;
    mNotes = new XMLNode(stream);

    //
    // checks if the given default namespace (if any) is a valid
    // SBML namespace
    //
    const XMLNamespaces &xmlns = mNotes->getNamespaces();
    checkDefaultNamespace(&xmlns,"notes");

    if (getSBMLDocument() != NULL)
    {
      if (getSBMLDocument()->getNumErrors() == 0)
      {
        checkXHTML(mNotes);
      }
    }
    return true;
  }

  return false;
}

bool
SBase::getHasBeenDeleted()
{
  return mHasBeenDeleted;
}

/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
SBase::getElementPosition () const
{
  return -1;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBMLErrorLog used to log errors during while reading and
 * validating SBML.
 */
SBMLErrorLog*
SBase::getErrorLog ()
{
  return (mSBML != 0) ? mSBML->getErrorLog() : 0;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Helper to log a common type of error.
 */
void
SBase::logUnknownAttribute( string attribute,
                            const unsigned int level,
                            const unsigned int version,
                            string element )
{
  ostringstream msg;

  msg << "Attribute '" << attribute << "' is not part of the "
      << "definition of an SBML Level " << level
      << " Version " << version << " " << element << " element.";
   
  if (level < 3)
  {
    getErrorLog()->logError(NotSchemaConformant,
			    level, version, msg.str());
  }
  else
  {
    if (element == "<listOfFunctionDefinitions>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfFuncs, level,
        version, msg.str());
    }
    else if (element == "<listOfUnitDefinitions>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfUnitDefs, level,
        version, msg.str());
    }
    else if (element == "<listOfCompartments>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfComps, level,
        version, msg.str());
    }
    else if (element == "<listOfSpecies>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfSpecies, level,
        version, msg.str());
    }
    else if (element == "<listOfParameters>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfParams, level,
        version, msg.str());
    }
    else if (element == "<listOfInitialAssignments>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfInitAssign, level,
        version, msg.str());
    }
    else if (element == "<listOfRules>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfRules, level,
        version, msg.str());
    }
    else if (element == "<listOfConstraints>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfConstraints, level,
        version, msg.str());
    }
    else if (element == "<listOfReactions>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfReactions, level,
        version, msg.str());
    }
    else if (element == "<listOfEvents>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfEvents, level,
        version, msg.str());
    }
    else if (element == "<model>")
    {
      getErrorLog()->logError(AllowedAttributesOnModel, level,
        version, msg.str());
    }
    else if (element == "<listOfUnits>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfUnits, level,
        version, msg.str());
    }
    else if (element == "<unitDefinition>")
    {
      getErrorLog()->logError(AllowedAttributesOnUnitDefinition, level,
        version, msg.str());
    }
    else if (element == "<unit>")
    {
      getErrorLog()->logError(AllowedAttributesOnUnit, level,
        version, msg.str());
    }
    else if (element == "<functionDefinition>")
    {
      getErrorLog()->logError(AllowedAttributesOnFunc, level,
        version, msg.str());
    }
    else if (element == "<compartment>")
    {
      getErrorLog()->logError(AllowedAttributesOnCompartment, level,
        version, msg.str());
    }
    else if (element == "<species>")
    {
      getErrorLog()->logError(AllowedAttributesOnSpecies, level,
        version, msg.str());
    }
    else if (element == "<parameter>")
    {
      getErrorLog()->logError(AllowedAttributesOnParameter, level,
        version, msg.str());
    }
    else if (element == "<initialAssignment>")
    {
      getErrorLog()->logError(AllowedAttributesOnInitialAssign, level,
        version, msg.str());
    }
    else if (element == "<assignmentRule>")
    {
      getErrorLog()->logError(AllowedAttributesOnAssignRule, level,
        version, msg.str());
    }
    else if (element == "<rateRule>")
    {
      getErrorLog()->logError(AllowedAttributesOnRateRule, level,
        version, msg.str());
    }
    else if (element == "<algebraicRule>")
    {
      getErrorLog()->logError(AllowedAttributesOnAlgRule, level,
        version, msg.str());
    }
    else if (element == "<constraint>")
    {
      getErrorLog()->logError(AllowedAttributesOnConstraint, level,
        version, msg.str());
    }
    else if (element == "<reaction>")
    {
      getErrorLog()->logError(AllowedAttributesOnReaction, level,
        version, msg.str());
    }
    else if (element == "<listOfReactants>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfSpeciesRef, level,
        version, msg.str());
    }
    else if (element == "<listOfProducts>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfSpeciesRef, level,
        version, msg.str());
    }
    else if (element == "<listOfModifiers>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfMods, level,
        version, msg.str());
    }
    else if (element == "<speciesReference>")
    {
      getErrorLog()->logError(AllowedAttributesOnSpeciesReference, level,
        version, msg.str());
    }
    else if (element == "<modifierSpeciesReference>")
    {
      getErrorLog()->logError(AllowedAttributesOnModifier, level,
        version, msg.str());
    }
    else if (element == "<listOfLocalParameters>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfLocalParam, level,
        version, msg.str());
    }
    else if (element == "<kineticLaw>")
    {
      getErrorLog()->logError(eeeFIX_ME, level,
        version, msg.str());
    }
    else if (element == "<localParameter>")
    {
      getErrorLog()->logError(AllowedAttributesOnLocalParameter, level,
        version, msg.str());
    }
    else if (element == "<event>")
    {
      getErrorLog()->logError(AllowedAttributesOnEvent, level,
        version, msg.str());
    }
    else if (element == "<listOfEventAssignments>")
    {
      getErrorLog()->logError(AllowedAttributesOnListOfEventAssign, level,
        version, msg.str());
    }
    else if (element == "<trigger>")
    {
      getErrorLog()->logError(AllowedAttributesOnTrigger, level,
        version, msg.str());
    }
    else if (element == "<delay>")
    {
      getErrorLog()->logError(AllowedAttributesOnDelay, level,
        version, msg.str());
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Helper to log a common type of error.
 */
void
SBase::logUnknownElement( string element,
			  const unsigned int level,
			  const unsigned int version )
{
  bool logged = false;
  ostringstream msg;

  if (level > 2 && getTypeCode() == SBML_LIST_OF)
  {
    SBMLTypeCode_t tc = static_cast<ListOf*>(this)->getItemTypeCode();
    msg << "Element '" << element << "' is not part of the definition of "
      << this->getElementName() << ".";
    switch (tc)
    {
    case SBML_UNIT:
      getErrorLog()->logError(OnlyUnitsInListOfUnits, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_FUNCTION_DEFINITION:
    
      getErrorLog()->logError(OnlyFuncDefsInListOfFuncDefs, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_UNIT_DEFINITION:
    
      getErrorLog()->logError(OnlyUnitDefsInListOfUnitDefs, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_COMPARTMENT:
    
      getErrorLog()->logError(OnlyCompartmentsInListOfCompartments, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_SPECIES:
    
      getErrorLog()->logError(OnlySpeciesInListOfSpecies, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_PARAMETER:
    
      getErrorLog()->logError(OnlyParametersInListOfParameters, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_INITIAL_ASSIGNMENT:
    
      getErrorLog()->logError(OnlyInitAssignsInListOfInitAssigns, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_CONSTRAINT:
    
      getErrorLog()->logError(OnlyConstraintsInListOfConstraints, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_RULE:
    
      getErrorLog()->logError(OnlyRulesInListOfRules, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_REACTION:
    
      getErrorLog()->logError(OnlyReactionsInListOfReactions, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_EVENT:
    
      getErrorLog()->logError(OnlyEventsInListOfEvents, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_LOCAL_PARAMETER:
    
      getErrorLog()->logError(OnlyLocalParamsInListOfLocalParams, 
                                level, version, msg.str());
      logged = true;
      break;
    case SBML_EVENT_ASSIGNMENT:
    
      getErrorLog()->logError(OnlyEventAssignInListOfEventAssign, 
                                level, version, msg.str());
      logged = true;
      break;
    }
  }

  if (!logged)
  {
    ostringstream msg;

    msg << "Element '" << element << "' is not part of the definition of "
        << "SBML Level " << level << " Version " << version << ".";
        
    getErrorLog()->logError(UnrecognizedElement,
			    level, version, msg.str());
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Helper to log a common type of error.
 */
void
SBase::logEmptyString( string attribute,
                       const unsigned int level,
                       const unsigned int version,
                       string element )
                       
{
  ostringstream msg;

  msg << "Attribute '" << attribute << "' on an "
    << element << " must not be an empty string.";
      
  getErrorLog()->logError(NotSchemaConformant,
			  level, version, msg.str());
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Convenience method for easily logging problems from within method
 * implementations.
 *
 * This is essentially a short form of getErrorLog()->logError(...)
 */
void
SBase::logError (  unsigned int       id
                 , const unsigned int level
                 , const unsigned int version
                 , const std::string& details )
{
  if ( SBase::getErrorLog() ) 
    getErrorLog()->logError(id, getLevel(), getVersion(), details);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SBase::readAttributes (const XMLAttributes& attributes)
{
  const_cast<XMLAttributes&>(attributes).setErrorLog(getErrorLog());

  bool assigned = attributes.readInto("metaid", mMetaId);

  if (assigned && mMetaId.empty())
  {
    logEmptyString("metaid", getLevel(), getVersion(), 
      SBMLTypeCode_toString(getTypeCode()));
  }

  if (isSetMetaId())
  {
    if (!SyntaxChecker::isValidXMLID(mMetaId))
      logError(InvalidMetaidSyntax, getLevel(), getVersion());
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SBase::writeAttributes (XMLOutputStream& stream) const
{
  if (getTypeCode() == SBML_DOCUMENT)
  {
    if (this->getNamespaces()) stream << *(this->getNamespaces());
  }
  if ( getLevel() > 1 && !mMetaId.empty() )
  {
    stream.writeAttribute("metaid", mMetaId);
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Synchronizes the annotation of this SBML object. 
 */
void
SBase::syncAnnotation ()
{
  bool hasRDF = false;
  bool hasAdditionalRDF = false;

  //
  // (*NOTICE*) 
  //
  // syncAnnotation() must not exit here even if the CVTerm objects (mCVTerms) 
  // in this SBase object is null or empty (0 CVTerm object).
  // The reason is that syncAnnotation updates the mAnnotation (an XMLNode object 
  // of annotation element in this SBase object) as follows:
  //
  //   (1) removes XMLNode elements corresponding to CVTerm and/or ModelHistory 
  //       objects from the mAnnotation, and
  //   (2) converts the current mCVTerms object (and mHistory objcect in 
  //       Model::syncAnnotation()) into corresponding temporary XMLNode objects,
  //       and
  //   (3) merges the temporary XMLNode objects into the mAnnotation 
  //
  // For example, an SBase object with null or empty mCVTerms needs to be updated
  // if mAnnotation contains XMLNode objects corresponding to CVTerm. 
  // (This can happen when unsetCVTerms() function invoked after addCVTerm() function.)
  //

  // determine status of existing annotation before doing anything
  if (mAnnotation)
  {
    hasRDF = RDFAnnotationParser::hasRDFAnnotation(mAnnotation);
    hasAdditionalRDF = 
      RDFAnnotationParser::hasAdditionalRDFAnnotation(mAnnotation);
  }

 // if (this->getTypeCode() != SBML_MODEL)
 // {
  if(mAnnotation && hasRDF)
    {
      XMLNode* new_annotation = 
        RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
      if(!new_annotation)
      {
         XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
         new_annotation = new XMLNode(ann_token);
         new_annotation->addChild(*mAnnotation);
      }
      *mAnnotation = *new_annotation;
      delete new_annotation;
    }
 // }

  XMLNode * cvTerms = RDFAnnotationParser::parseCVTerms(this);

  if (cvTerms)
  {
    if (!mAnnotation)
    {
      mAnnotation = cvTerms;
    }
    else
    {
      if (mAnnotation->isEnd())
      {
        mAnnotation->unsetEnd();
      }
      if (hasAdditionalRDF)
      {
        //need to insert the CVTerms into existing RDF
        unsigned int n = 0;
        while (n < mAnnotation->getNumChildren())
        {
          if (mAnnotation->getChild(n).getName() == "RDF")
          {
            mAnnotation->getChild(n).insertChild(0, 
              cvTerms->getChild(0).getChild(0));
            break;
          }
          n++;
        }
      }
      else
      {
        mAnnotation->addChild(cvTerms->getChild(0));
      }
      delete cvTerms;
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Checks that SBML element has been read in the proper order.  If object
 * is not in the expected position, an error is logged.
 */
void
SBase::checkOrderAndLogError (SBase* object, int expected)
{
  int actual = object->getElementPosition();

  if (actual != -1 && actual < expected)
  {
    SBMLErrorCode_t error = IncorrectOrderInModel;

    if (object->getTypeCode() == SBML_LIST_OF)
    {
      SBMLTypeCode_t tc = static_cast<ListOf*>(object)->getItemTypeCode();

      if (tc == SBML_SPECIES_REFERENCE || tc == SBML_MODIFIER_SPECIES_REFERENCE)
      {
        error = IncorrectOrderInReaction;
      }
    }
    else if (object->getTypeCode() == SBML_TRIGGER)
    {
      error = IncorrectOrderInEvent;
    }

    logError(error, getLevel(), getVersion());
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
  * Checks that an SBML ListOf element has been populated.  
  * If a listOf element has been declared with no elements, 
  * an error is logged.
  */
void 
SBase::checkListOfPopulated(SBase* object)
{
  if (object->getTypeCode() == SBML_LIST_OF)
  {
    // Check that the list has at least one element.
    if (static_cast <ListOf*> (object)->size() == 0)
    {
      SBMLTypeCode_t tc = static_cast<ListOf*>(object)->getItemTypeCode();
      SBMLErrorCode_t error = EmptyListElement;

      // By default, the error will be the EmptyListElement error, unless
      // we have a special case for which SBML has a separate error code.
      switch (tc)
      {
      case SBML_UNIT:
        if (object->getLevel() < 3)
          error = EmptyListOfUnits;
        else
          error = EmptyUnitListElement;
        break;

      case SBML_SPECIES_REFERENCE:
      case SBML_MODIFIER_SPECIES_REFERENCE:
        error = EmptyListInReaction;
        break;

      case SBML_PARAMETER:
      case SBML_LOCAL_PARAMETER:
        // If listOfParameters is inside a KineticLaw, we have a separate code.
        if (this->getTypeCode() == SBML_KINETIC_LAW)
        {
          error = EmptyListInKineticLaw;
        }
        break;

      default:;
      }

      logError(error, getLevel(), getVersion());
    }
  }
  else if (object->getTypeCode() == SBML_KINETIC_LAW)
  {
    /* 
     * if nothing has been set in the kineticLaw we assume its is empty
     */
    if (static_cast <KineticLaw *> (object)->isSetMath()           == 0  &&
        static_cast <KineticLaw *> (object)->isSetFormula()        == 0  &&
        static_cast <KineticLaw *> (object)->isSetTimeUnits()      == 0  &&
        static_cast <KineticLaw *> (object)->isSetSubstanceUnits() == 0  &&
        static_cast <KineticLaw *> (object)->isSetSBOTerm()        == 0  &&
        static_cast <KineticLaw *> (object)->getNumParameters()    == 0)
    {
      logError(EmptyListInReaction, getLevel(), getVersion());
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */

void 
SBase::checkDefaultNamespace(const XMLNamespaces* xmlns, const std::string& elementName)
{
  //
  // checks if the given default namespace (if any) is a valid
  // SBML namespace
  //
  if (xmlns && xmlns->getLength() > 0)
  {
    unsigned int level   = getLevel();
    unsigned int version = getVersion();
    const std::string currentURI = SBMLNamespaces::getSBMLNamespaceURI(level,version); 
    const std::string defaultURI = xmlns->getURI();
    if (!defaultURI.empty() && currentURI != defaultURI)
    {
      static ostringstream errMsg;
      errMsg.str("");
      errMsg << "xmlns=\"" << defaultURI << "\" in <" << elementName
             << "> element is an invalid namespace." << endl;
      
      logError(NotSchemaConformant, level, version, errMsg.str());
    }
  }
}

/**
  * Checks the annotation does not declare an sbml namespace.
  * If the annotation declares an sbml namespace an error is logged.
  */
void
SBase::checkAnnotation()
{
  unsigned int nNodes = 0;
  unsigned int match = 0;
  int n = 0;
  std::vector<std::string> prefixes;
  prefixes.clear();

  if (!mAnnotation) return;

  //
  // checks if the given default namespace (if any) is a valid
  // SBML namespace
  //
  const XMLNamespaces &xmlns = mAnnotation->getNamespaces();
  checkDefaultNamespace(&xmlns,"annotation");

  while (nNodes < mAnnotation->getNumChildren())
  {
    XMLNode topLevel = mAnnotation->getChild(nNodes);

    std::string prefix = topLevel.getPrefix();

    // cannot be other toplevel element with this prefix
    if (!prefix.empty())
    {
      if (find(prefixes.begin(), prefixes.end(), prefix) 
                                               != prefixes.end())
      {
        logError(DuplicateAnnotationNamespaces);
      }
      prefixes.push_back(prefix);
    }

    match = 0;
    n = 0;

    bool implicitNSdecl = false;
   // must have a namespace
    if (topLevel.getNamespaces().getLength() == 0)
    {
      // not on actual element - is it explicit ??
      if( mSBML->getNamespaces() != NULL)
      /* check for implicit declaration */
      {
        for (n = 0; n < mSBML->getNamespaces()->getLength(); n++)
        {
          if (!strcmp(mSBML->getNamespaces()->getPrefix(n).c_str(), 
                        prefix.c_str()))
          {
            implicitNSdecl = true;
            break;
          }
        }
     }


      if (!implicitNSdecl)
      {
        logError(MissingAnnotationNamespace);
      }
    }
    // cannot declare sbml namespace
    while(!match && n < topLevel.getNamespaces().getLength())
    {
      match += !strcmp(topLevel.getNamespaces().getURI(n).c_str(), 
                                          "http://www.sbml.org/sbml/level1");
      match += !strcmp(topLevel.getNamespaces().getURI(n).c_str(), 
                                          "http://www.sbml.org/sbml/level2");
      match += !strcmp(topLevel.getNamespaces().getURI(n).c_str(), 
                                "http://www.sbml.org/sbml/level2/version2");
      match += !strcmp(topLevel.getNamespaces().getURI(n).c_str(), 
                                "http://www.sbml.org/sbml/level2/version3");
      n++;
    }
    if (match > 0)
    {
      logError(SBMLNamespaceInAnnotation);
      break;
    }

    if (implicitNSdecl && prefix.empty())
    {
      logError(MissingAnnotationNamespace);
      logError(SBMLNamespaceInAnnotation);   
    }
    nNodes++;
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Checks that the XHTML is valid.
 * If the xhtml does not conform to the specification of valid xhtml within
 * an sbml document, an error is logged.
 */
void
SBase::checkXHTML(const XMLNode * xhtml)
{
  if (!xhtml) return;

  const string&  name = xhtml->getName();
  unsigned int i, errorNS, errorXML, errorDOC, errorELEM;

  if (name == "notes")
  {
    errorNS   = NotesNotInXHTMLNamespace;
    errorXML  = NotesContainsXMLDecl;
    errorDOC  = NotesContainsDOCTYPE;
    errorELEM = InvalidNotesContent;
  }
  else if (name == "message")
  {
    errorNS   = ConstraintNotInXHTMLNamespace;
    errorXML  = ConstraintContainsXMLDecl;
    errorDOC  = ConstraintContainsDOCTYPE;
    errorELEM = InvalidConstraintContent;
  }
  else                                  // We shouldn't ever get to this point.
  {
    logError(UnknownError);
    return;
  }

  /*
  * errors relating to a misplaced XML or DOCTYPE declaration 
  * will also cause a parser error.
  * since parsing will terminate at this error, then if it has occurred
  * it will be in the XML currently being checked and so a more
  * informative message can be added
  */
  for (i = 0; i < getErrorLog()->getNumErrors(); i++)
  {
    if (getErrorLog()->getError(i)->getErrorId() == BadXMLDeclLocation)
    {
      logError(errorXML);
    }
    if (getErrorLog()->getError(i)->getErrorId() == BadlyFormedXML)
    {
      logError(errorDOC);
    }
  }

  XMLNamespaces* toplevelNS = (mSBML) ? mSBML->getNamespaces() : 0;

  /*
  * namespace declaration is variable
  * if a whole html tag has been used
  * or a whole body tag then namespace can be implicitly declared
  */
  unsigned int children = xhtml->getNumChildren();

  if (children > 1)
  {
    for (i=0; i < children; i++)
    {
      if (SyntaxChecker::isAllowedElement(xhtml->getChild(i)))
      {
        if (!SyntaxChecker::hasDeclaredNS(xhtml->getChild(i),
                                                  toplevelNS))
        {
          logError(errorNS);
        }
      }
      else
      {
        logError(errorELEM);
      }
    }
  }
  else
  {
    /* only one element which can be html or body with either implicit/explicit
    * namespace declaration
    * OR could be one of the listed elements.
    */

    const string& top_name = xhtml->getChild(0).getName();

    if (top_name != "html" && top_name != "body"
      && !SyntaxChecker::isAllowedElement(xhtml->getChild(0)))
    {
      logError(errorELEM);
    }
    else
    {
      if (!SyntaxChecker::hasDeclaredNS(xhtml->getChild(0), toplevelNS))
      {
        logError(errorNS);
      }
      if (top_name == "html" 
        && !SyntaxChecker::isCorrectHTMLNode(xhtml->getChild(0)))
      {
        logError(errorELEM);
      }
    }
  }
}

/** @endcond doxygen-libsbml-internal */
/** @cond doxygen-libsbml-internal */
/* default for components that have no required attributes */
bool
SBase::hasRequiredAttributes() const
{
  return true;
}

/* default for components that have no required elements */
bool
SBase::hasRequiredElements() const
{
  return true;
}

void
SBase::removeDuplicateAnnotations()
{
  bool resetNecessary = false;
  XMLNamespaces xmlns = XMLNamespaces();
  xmlns.add("http://www.sbml.org/libsbml/annotation", "");
  XMLTriple triple = XMLTriple("duplicateTopLevelElements",
    "http://www.sbml.org/libsbml/annotation", "");
  XMLAttributes att = XMLAttributes();
  XMLToken token = XMLToken(triple, att, xmlns);
  XMLNode * newNode = NULL;
  if (isSetAnnotation())
  { 
    //make a copy to work with
    XMLNode * newAnnotation = mAnnotation->clone();

    unsigned int numChildren = newAnnotation->getNumChildren();
    if (numChildren == 1)
      return;

    bool duplicate = false;
    for (unsigned int i = 0; i < numChildren; i++)
    {
      duplicate = false;
      std::string name = newAnnotation->getChild(i).getName();
      for (unsigned int j = numChildren-1; j > i; j--)
      {
        if (name == newAnnotation->getChild(j).getName())
        {
          resetNecessary = true;
          duplicate = true;
          if (!newNode)
          {
            // need to  create the new node
            newNode = new XMLNode(token);
          }
          newNode->addChild(static_cast <XMLNode> 
                            (*(newAnnotation->removeChild(j))));
        }
      }
      if (duplicate)
        newNode->addChild(static_cast <XMLNode>
                          (*(newAnnotation->removeChild(i))));
      numChildren = newAnnotation->getNumChildren();
    }
    if (resetNecessary)
    {
      newAnnotation->addChild(*(newNode));
      setAnnotation(newAnnotation);
    }
  }


}


/** @endcond doxygen-libsbml-internal */

/** @cond doxygen-libsbml-internal */
/*
 * Stores the location (line and column) and any XML namespaces (for
 * roundtripping) declared on this SBML (XML) element.
 */
void
SBase::setSBaseFields (const XMLToken& element)
{
  mLine   = element.getLine  ();
  mColumn = element.getColumn();

  if (element.getNamespaces().getLength() > 0)
  {
    XMLNamespaces tmpxmlns(element.getNamespaces());
    setNamespaces(&tmpxmlns);
  }
  else
  {
    setNamespaces(NULL);
  }
}
/** @endcond doxygen-libsbml-internal */



/** @cond doxygen-c-only */

/**
 * Adds a copy of the given CVTerm to this SBML object.
 *
 * @param sb the object to add the CVTerm to
 * @param term the CVTerm_t to assign
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 * @li LIBSBML_INVALID_OBJECT
 *
 * @note The annotation constructed from a CVTerm uses the metaid
 * of the object to identify it.  Adding a CVTerm to an object
 * where the 'metaId' attribute has not been set will fail with the
 * return value LIBSBML_UNEXPECTED_ATTRIBUTE.
 */
LIBSBML_EXTERN
int 
SBase_addCVTerm(SBase_t *sb, CVTerm_t *term)
{
  return sb->addCVTerm(term);
}


/**
 * Returns a list of CVTerm objects in the annotations of this SBML
 * object.
 *
 * @param sb the object to getCVTerms from
 * 
 * @return the list of CVTerms for this SBML object.
 */
LIBSBML_EXTERN
List_t* 
SBase_getCVTerms(SBase_t *sb)
{
  return sb->getCVTerms();
}


/**
 * Returns the number of CVTerm objects in the annotations of this SBML
 * object.
 *
 * @param sb the object to getCVTerms from
 * 
 * @return the number of CVTerms for this SBML object.
 */
LIBSBML_EXTERN
unsigned int 
SBase_getNumCVTerms(SBase_t *sb)
{
  return sb->getNumCVTerms();
}

/**
 * Returns the nth CVTerm in the list of CVTerms of this SBML
 * object.
 *
 * @param sb the object to getCVTerms from
 * @param n unsigned int the index of the CVTerm to retrieve
 *
 * @return the nth CVTerm in the list of CVTerms for this SBML object.
 */
LIBSBML_EXTERN
CVTerm_t* 
SBase_getCVTerm(SBase_t *sb, unsigned int n)
{
  return static_cast <CVTerm_t *> (sb->getCVTerm(n));
}

/**
 * Clears the list of CVTerms of this SBML
 * object.
 *
 * @param sb the object to clear CVTerms from
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
SBase_unsetCVTerms(SBase_t *sb)
{
  return sb->unsetCVTerms();
}


/**
 * Returns the ModelHistory of the given SBase_t structure.
 *
 * @return the ModelHistory of the given SBase_t structure.
 * 
 * @param m the SBase_t structure
 */
LIBSBML_EXTERN
ModelHistory_t * 
SBase_getModelHistory(SBase_t *sb)
{
  return sb->getModelHistory();
}

/**
 * Predicate for testing whether the ModelHistory of a given SBase_t structure has
 * been assigned.
 * 
 * @param m the SBase_t structure
 * 
 * @return nonzero if the ModelHistory of this SBase_t structure has
 * been set, zero (0) otherwise.
 */LIBSBML_EXTERN
int 
SBase_isSetModelHistory(SBase_t *sb)
{
  return static_cast<int>( sb->isSetModelHistory() );
}


/**
 * Set the ModelHistory of the given SBase_t structure.
 * 
 * @param m the SBase_t structure
 * @param history the ModelHistory_t structure
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int 
SBase_setModelHistory(SBase_t *sb, ModelHistory_t *history)
{
  return sb->setModelHistory(history);
}

/**
 * Unsets the ModelHistory of the given SBase_t structure.
 * 
 * @param m the SBase_t structure
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
SBase_unsetModelHistory(SBase_t *sb)
{
  return sb->unsetModelHistory();
}


/**
 * Returns the BiologicalQualifier associated with this resource,
 * BQB_UNKNOWN if the resource does not exist.
 *
 * @param sb the object to query
 * @param resource string representing the resource; e.g.,
 * "http://www.geneontology.org/#GO:0005892"
 *
 * @return the BiolQualifierType_t associated with the resource
 */
LIBSBML_EXTERN
BiolQualifierType_t 
SBase_getResourceBiologicalQualifier(SBase_t *sb, const char * resource)
{
  return sb->getResourceBiologicalQualifier(resource);
}


/**
 * Returns the ModelQualifier associated with this resource,
 * BQM_UNKNOWN if the resource does not exist.
 *
 * @param sb the object to query
 * @param resource string representing the resource; e.g.,
 * "http://www.geneontology.org/#GO:0005892"
 *
 * @return the ModelQualifierType_t associated with the resource
 */
LIBSBML_EXTERN
ModelQualifierType_t 
SBase_getResourceModelQualifier(SBase_t *sb, const char * resource)
{ 
  return sb->getResourceModelQualifier(resource);
}




/**
 * Returns the value of the "metaid" attribute of the given SBase_t
 * structure.
 *
 * @param sb the SBase_t structure
 * 
 * @return the value of the "metaid" attribute of @p sb
 */
LIBSBML_EXTERN
const char *
SBase_getMetaId (SBase_t *sb)
{
  return sb->isSetMetaId() ? sb->getMetaId().c_str() : NULL;
}


///**
// * Returns the value of the "id" attribute of the given SBase_t
// * structure.
// *
// * @param sb the SBase_t structure
// * 
// * @return the value of the "id" attribute of @p sb
// */
//LIBSBML_EXTERN
//const char *
//SBase_getId (const SBase_t *sb)
//{
//  return sb->isSetId() ? sb->getId().c_str() : NULL;
//}
//
//
///**
// * Returns the value of the "name" attribute of the given SBase_t
// * structure.
// *
// * @param sb the SBase_t structure
// * 
// * @return the value of the "name" attribute of @p sb
// */
//LIBSBML_EXTERN
//const char *
//SBase_getName (const SBase_t *sb)
//{
//  return sb->isSetName() ? sb->getName().c_str() : NULL;
//}


/**
 * Returns the parent SBMLDocument_t structure of the given SBase_t
 * structure.
 *
 * @param sb the SBase_t structure
 * 
 * @return the parent SBMLDocument of this SBML object.
 */
LIBSBML_EXTERN
const SBMLDocument_t *
SBase_getSBMLDocument (SBase_t *sb)
{
  return sb->getSBMLDocument();
}


/**
 * Returns the parent SBase_t structure of the given SBase_t
 * structure.
 *
 * @param sb the SBase_t structure
 * 
 * @return the parent SBase  of this SBML object.
 */
LIBSBML_EXTERN
const SBase_t *
SBase_getParentSBMLObject (SBase_t *sb)
{
  return sb->getParentSBMLObject();
}


/**
 * Returns the ancestor SBase_t structure of the given SBase_t
 * structure that corresponds to the given type.
 *
 * This function allows any object to determine its exact 
 * location/function within a model. For example a 
 * StoichiometryMath object has ancestors of type SpeciesReference,
 * ListOf(Products/Reactants), Reaction, ListOfReactions and Model; 
 * any of which can be accessed via this function.
 *
 * @param sb the SBase_t structure
 * @param type the SBMLTypeCode_t of the structure to be returned
 * 
 * @return the ancestor SBase_t structure of this SBML object with
 * the corresponding SBMLTypeCode_t, NULL if there is no ancestor of
 * this type.
 */
LIBSBML_EXTERN
const SBase_t *
SBase_getAncestorOfType (SBase_t *sb, SBMLTypeCode_t type)
{
  return sb->getAncestorOfType(type);
}


/**
 * Returns the integer portion of the value of the "sboTerm" attribute of
 * the given SBase_t structure.
 *
 * In SBML Level 2 Versions 2 and 3, the data type of the attribute is a
 * string of the form SBO:NNNNNNN, where NNNNNNN is a seven digit integer
 * number; libSBML simplifies the representation by only storing the
 * NNNNNNN integer portion.  Thus, in libSBML, the "sboTerm" attribute on
 * SBase_t has data type @c int, and SBO identifiers are stored simply as
 * integers.  SBO terms are a type of optional annotation, and each
 * different class of SBML object derived from SBase_t imposes its own
 * requirements about the values permitted for "sboTerm".  Please consult
 * the SBML Level 2 Version 4 specification for more information about
 * the use of SBO and the "sboTerm" attribute.
 *
 * @param sb the SBase_t structure
 * 
 * @return the value of the "sboTerm" attribute as an integer, or @c -1
 * if the value is not set.
 */
LIBSBML_EXTERN
int
SBase_getSBOTerm (const SBase_t *sb)
{
  return sb->getSBOTerm();
}


/**
 * Returns the string representation of the "sboTerm" attribute of
 * this object.
 *
 * In SBML Level 2 Versions 2, 3 and 4, the data type of the attribute is a
 * string of the form SBO:NNNNNNN, where NNNNNNN is a seven digit integer
 * number; libSBML simplifies the representation by only storing the
 * NNNNNNN integer portion.  Thus, in libSBML, the "sboTerm" attribute on
 * SBase has data type @c int, and SBO identifiers are stored simply as
 * integers.  This function recreates the string representation from the
 * stored value.  SBO terms are a type of optional annotation, and each
 * different class of SBML object derived from SBase imposes its own
 * requirements about the values permitted for "sboTerm".  Please consult
 * the SBML Level 2 Version 4 specification for more information about
 * the use of SBO and the "sboTerm" attribute.
 *
 * @return the value of the "sboTerm" attribute as a string of the form
 * SBO:NNNNNNN, or NULL if the value is not set.
 */
LIBSBML_EXTERN
char*
SBase_getSBOTermID (const SBase_t *sb)
{
  return sb->isSetSBOTerm() ? safe_strdup(sb->getSBOTermID().c_str()) : NULL;
}


/**
 * Returns the SBML Level of the overall SBML document.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return the SBML level of the given object.
 * 
 * @see getVersion()
 */
LIBSBML_EXTERN
unsigned int
SBase_getLevel (const SBase_t *sb)
{
  return sb->getLevel();
}


/**
 * Returns the Version within the SBML Level of the overall SBML document.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return the SBML version of the given object.
 *
 * @see getLevel()
 */
LIBSBML_EXTERN
unsigned int
SBase_getVersion (const SBase_t *sb)
{
  return sb->getVersion();
}


/**
 * Returns the notes from given SBML object.
 *
 * @param sb the given SBML object.
 *
 * @return the XMLNode_t structure representing the notes from this object.
 */
LIBSBML_EXTERN
XMLNode_t *
SBase_getNotes (SBase_t *sb)
{
  return sb->getNotes();
}


/**
 * Returns the notes string from given SBML object.
 * The string is owned by the caller and should be freed
 * (with free()) when no longer needed.  
 *
 * @param sb the given SBML object.
 *
 * @return the string (char*) representing the notes from this object.
 */
LIBSBML_EXTERN
char*
SBase_getNotesString (SBase_t *sb)
{
  return sb->isSetNotes() ? safe_strdup(sb->getNotesString().c_str()) : NULL;
}


/**
 * Returns the annotation from given SBML object.
 *
 * @param sb the given SBML object.
 *
 * @return the XMLNode_t structure representing the annotation from this object.
 */
LIBSBML_EXTERN
XMLNode_t *
SBase_getAnnotation (SBase_t *sb)
{
  return sb->getAnnotation();
}


/**
 * Returns the annotation string from given SBML object.
 * The string is owned by the caller and should be freed
 * (with free()) when no longer needed.
 *
 * @param sb the given SBML object.
 *
 * @return the string (char*) representing the annotation from this object.
 */
LIBSBML_EXTERN
char*
SBase_getAnnotationString (SBase_t *sb)
{
  return sb->isSetAnnotation() ? safe_strdup(sb->getAnnotationString().c_str()) : NULL;
}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "metaid" attribute has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "metaid" attribute of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetMetaId (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetMetaId() );
}


///**
// * Predicate returning nonzero true or false depending on whether the given
// * structure's "id" attribute has been set.
// *
// * @param sb the SBase_t structure to query
// * 
// * @return nonzero (for true) if the "id" attribute of this SBML object
// * has been set, zero (for false) otherwise.
// */
//LIBSBML_EXTERN
//int
//SBase_isSetId (const SBase_t *sb)
//{
//  return static_cast<int>( sb->isSetId() );
//}
//
//
///**
// * Predicate returning nonzero true or false depending on whether the given
// * structure's "name" attribute has been set.
// *
// * @param sb the SBase_t structure to query
// * 
// * @return nonzero (for true) if the "name" attribute of this SBML object
// * has been set, zero (for false) otherwise.
// */
//LIBSBML_EXTERN
//int
//SBase_isSetName (const SBase_t *sb)
//{
//  return static_cast<int>( sb->isSetName() );
//}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "notes" subelement has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "notes" subelement of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetNotes (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetNotes() );
}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "annotation" subelement has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "annotation" subelement of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetAnnotation (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetAnnotation() );
}


/**
 * Predicate returning nonzero true or false depending on whether the given
 * structure's "sboTerm" attribute has been set.
 *
 * @param sb the SBase_t structure to query
 * 
 * @return nonzero (for true) if the "sboTerm" attribute of this SBML object
 * has been set, zero (for false) otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetSBOTerm (const SBase_t *sb)
{
  return static_cast<int>( sb->isSetSBOTerm() );
}


/**
 * Sets the value of the "metaid" attribute of the given object.
 *
 * The string @p metaid is copied.  The value of @p metaid must be an
 * identifier conforming to the syntax defined by the XML 1.0 data type
 * ID.  Among other things, this type requires that a value is unique
 * among all the values of type XML ID in an SBMLDocument.  Although SBML
 * only uses XML ID for the "metaid" attribute, callers should be careful
 * if they use XML ID's in XML portions of a model that are not defined
 * by SBML, such as in the application-specific content of the
 * "annotation" subelement.
 *
 * @param sb the SBase_t structure
 *
 * @param metaid the identifier string to use as the value of the
 * "metaid" attribute
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 *
 * @note Using this function with the metaid set to NULL is equivalent to
 * unsetting the "metaid" attribute.
 */
LIBSBML_EXTERN
int
SBase_setMetaId (SBase_t *sb, const char *metaid)
{
  return (metaid == NULL) ? sb->unsetMetaId() : sb->setMetaId(metaid);
}


///**
// * Sets the value of the "id" attribute of this SBML object.
// *
// * The string @p sid is copied.  Note that SBML has strict requirements
// * for the syntax of identifiers.  The following is summary of the
// * definition of the SBML identifier type @c SId (here expressed in an
// * extended form of BNF notation):
// * @code
// *   letter ::= 'a'..'z','A'..'Z'
// *   digit  ::= '0'..'9'
// *   idChar ::= letter | digit | '_'
// *   SId    ::= ( letter | '_' ) idChar*
// * @endcode
// * The equality of SBML identifiers is determined by an exact character
// * sequence match; i.e., comparisons must be performed in a
// * case-sensitive manner.  In addition, there are a few conditions for
// * the uniqueness of identifiers in an SBML model.  Please consult the
// * SBML specifications for the exact formulations.
// *
// * @param sb the SBase_t structure
// *
// * @param sid the string to use as the identifier of this object
// *
// * @return integer value indicating success/failure of the
// * function.  @if clike The value is drawn from the
// * enumeration #OperationReturnValues_t. @endif The possible values
// * returned by this function are:
// *
// * @li LIBSBML_OPERATION_SUCCESS
// * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
// *
// * @note Using this function with an id of NULL is equivalent to
// * unsetting the "id" attribute.
// */
//LIBSBML_EXTERN
//int
//SBase_setId (SBase_t *sb, const char *sid)
//{
//  return (sid == NULL) ? sb->unsetId() : sb->setId(sid);
//}
//
//
///**
// * Sets the value of the "name" attribute of this SBML object.
// *
// * The string in @p name is copied.
// *
// * @param sb the SBase_t structure
// *
// * @param name the new name for the object
// *
// * @return integer value indicating success/failure of the
// * function.  @if clike The value is drawn from the
// * enumeration #OperationReturnValues_t. @endif The possible values
// * returned by this function are:
// *
// * @li LIBSBML_OPERATION_SUCCESS
// * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
// *
// * @note Using this function with the name set to NULL is equivalent to
// * unsetting the "name" attribute.
// */
//LIBSBML_EXTERN
//int
//SBase_setName (SBase_t *sb, const char *name)
//{
//  return (name == NULL) ? sb->unsetName() : sb->setName(name);
//}


/**
 * Sets the value of the "sboTerm" attribute.
 *
 * In SBML Level 2 Versions 2, 3 and 4, the data type of the SBML "sboTerm"
 * attribute is a string of the form SBO:NNNNNNN, where NNNNNNN is a seven
 * digit integer number; libSBML simplifies the representation by only
 * storing the NNNNNNN integer portion.  Thus, in libSBML, the "sboTerm"
 * attribute on SBase_t has data type @c int, and SBO identifiers are
 * stored simply as integers.  SBO terms are a type of optional annotation,
 * and each different class of SBML object derived from SBase_t imposes its
 * own requirements about the values permitted for "sboTerm".  Please
 * consult the SBML Level 2 Version 4 specification for more information
 * about the use of SBO and the "sboTerm" attribute.
 *
 * @param sb the SBase_t structure
 *
 * @param value the NNNNNNN integer portion of the SBO identifier
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 */
LIBSBML_EXTERN
int
SBase_setSBOTerm (SBase_t *sb, int value)
{
  return sb->setSBOTerm(value);
}


/*
 * Sets the value of the "sboTerm" attribute by string.
 *
 * In SBML Level 2 Versions 2, 3 and 4, the data type of the SBML "sboTerm"
 * attribute is a string of the form SBO:NNNNNNN, where NNNNNNN is a
 * seven digit integer number; libSBML simplifies the representation by
 * only storing the NNNNNNN integer portion converted from the given string.
 * Thus, in libSBML, the "sboTerm" attribute on SBase has data type @c int,
 * and SBO identifiers are stored simply as integers.  SBO terms are a type
 * of optional annotation, and each different class of SBML object derived
 * from SBase imposes its own requirements about the values permitted for
 * "sboTerm".  Please consult the SBML Level 2 Version 4 specification for
 * more information about the use of SBO and the "sboTerm" attribute.
 *
 * @param sb the SBase_t structure
 *
 * @param value the SBO identifier string of the form SBO:NNNNNNN 
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 */
LIBSBML_EXTERN
int
SBase_setSBOTermID (SBase_t *sb, const char* sboid)
{
  return sb->setSBOTerm(sboid);
}


/**
 * Sets the namespaces relevant of this SBML object.
 *
 * @param sb the SBase_t structure
 *
 * @param xmlns the namespaces to set
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
SBase_setNamespaces (SBase_t *sb, XMLNamespaces_t *xmlns)
{
  return sb->setNamespaces(xmlns);
}


/**
 * Sets the notes for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param notes the XMLNode_t structure respresenting the notes.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 */
LIBSBML_EXTERN
int
SBase_setNotes (SBase_t *sb, XMLNode_t *notes)
{
  return sb->setNotes(notes);
}


/**
 * Sets the notes for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param notes the string (const char*) respresenting the notes.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
SBase_setNotesString (SBase_t *sb, char *notes)
{
  if(notes == NULL)
  {
    return sb->unsetNotes();
  }
  else
  {
    return sb->setNotes(notes);
  }
}


/**
 * Appends the notes for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param notes the XMLNode_t structure respresenting the notes.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
SBase_appendNotes (SBase_t *sb, XMLNode_t *notes)
{
  return sb->appendNotes(notes);
}


/**
 * Appends the notes for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param notes the string (const char*) respresenting the notes.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_OBJECT
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
SBase_appendNotesString (SBase_t *sb, char *notes)
{
  return sb->appendNotes(notes);
}


/**
 * Sets the annotation for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param annotation the XMLNode_t structure respresenting the annotation.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
SBase_setAnnotation (SBase_t *sb, XMLNode_t *annotation)
{
  return sb->setAnnotation(annotation);
}


/**
 * Sets the annotation for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param annotation the string (const char*) respresenting the annotation.
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
SBase_setAnnotationString (SBase_t *sb, char *annotation)
{
  if(annotation == NULL)
  {
    return sb->unsetAnnotation();
  }
  else
  {
    return sb->setAnnotation(annotation);
  }
}


/**
 * Appends the annotation for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param annotation the XMLNode_t structure respresenting the annotation.
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
SBase_appendAnnotation (SBase_t *sb, XMLNode_t *annotation)
{
  return sb->appendAnnotation(annotation);
}


/**
 * Appends the annotation for the given SBML object.
 *
 * @param sb the given SBML object.
 * @param annotation the string (const char*) respresenting the annotation.
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
SBase_appendAnnotationString (SBase_t *sb, char *annotation)
{
  return sb->appendAnnotation(annotation);
}


/**
 * Unsets the "metaid" attribute of the given object.
 *
 * @param sb the SBase_t structure
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
SBase_unsetMetaId (SBase_t *sb)
{
  return sb->unsetMetaId();
}


///**
// * Unsets the "id" attribute of the given object.
// *
// * @param sb the SBase_t structure
// *
// * @return integer value indicating success/failure of the
// * function.  @if clike The value is drawn from the
// * enumeration #OperationReturnValues_t. @endif The possible values
// * returned by this function are:
// *
// * @li LIBSBML_OPERATION_SUCCESS
// * @li LIBSBML_OPERATION_FAILED
// */
//LIBSBML_EXTERN
//int
//SBase_unsetId (SBase_t *sb)
//{
//  return sb->unsetId();
//}
//
//
///**
// * Unsets the "name" attribute of the given object.
// *
// * @param sb the SBase_t structure
// *
// * @return integer value indicating success/failure of the
// * function.  @if clike The value is drawn from the
// * enumeration #OperationReturnValues_t. @endif The possible values
// * returned by this function are:
// *
// * @li LIBSBML_OPERATION_SUCCESS
// * @li LIBSBML_OPERATION_FAILED
// */
//LIBSBML_EXTERN
//int
//SBase_unsetName (SBase_t *sb)
//{
//  return sb->unsetName();
//}


/**
 * Unsets the "notes" subelement of the given object.
 *
 * @param sb the SBase_t structure
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
SBase_unsetNotes (SBase_t *sb)
{
  return sb->unsetNotes();
}


/**
 * Unsets the "annotation" subelement of the given object.
 *
 * @param sb the SBase_t structure
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
SBase_unsetAnnotation (SBase_t *sb)
{
  return sb->unsetAnnotation();
}


/**
 * Unsets the "sboTerm" attribute of the given object.
 *
 * @param sb the SBase_t structure
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
SBase_unsetSBOTerm (SBase_t *sb)
{
  return sb->unsetSBOTerm();
}


/**
 * Returns the Model_t structure in which the given instance is located.
 *
 * @param sb the SBase_t structure
 * 
 * @return the parent Model_t strucdture of the given object.
 */
LIBSBML_EXTERN
const Model_t *
SBase_getModel (const SBase_t *sb)
{
  return sb->getModel();
}


/**
 * Returns the libSBML type code for the given structure.
 *
 * This method MAY return the typecode of this SBML object or it MAY
 * return SBML_UNKNOWN.  That is, subclasses of SBase are not required to
 * implement this method to return a typecode.  This method is meant
 * primarily for the LibSBML C interface where class and subclass
 * information is not readily available.
 *
 * @param sb the SBase_t structure
 *
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
LIBSBML_EXTERN
SBMLTypeCode_t
SBase_getTypeCode (const SBase_t *sb)
{
  return sb->getTypeCode();
}


/**
 * Returns the XML element name of the given structure.
 *
 * This is overridden by subclasses to return a string appropriate to the
 * SBML component.  For example, Model defines it as returning "model",
 * CompartmentType defines it as returning "compartmentType", etc.
 *
 * @param sb the SBase_t structure
 */
LIBSBML_EXTERN
const char *
SBase_getElementName (const SBase_t *sb)
{
  return sb->getElementName().empty() ? NULL : sb->getElementName().c_str();
}


/**
 * Returns the line number on which the given object first appears in the
 * XML representation of the SBML document.
 *
 * @param sb the SBase_t structure
 * 
 * @return the line number of the given structure
 *
 * @see getColumn().
 */
LIBSBML_EXTERN
unsigned int
SBase_getLine (const SBase_t *sb)
{
  return sb->getLine();
}


/**
 * Returns the column number on which the given object first appears in the
 * XML representation of the SBML document.
 *
 * @param sb the SBase_t structure
 * 
 * @return the column number of this SBML object.
 * 
 * @see getLine().
 */
LIBSBML_EXTERN
unsigned int
SBase_getColumn (const SBase_t *sb)
{
  return sb->getColumn();
}


/**
  * Predicate returning nonzero true or false depending on whether the
  * object's level/version and namespace values correspond to a valid
  * SBML specification.
  *
  * The valid combinations of SBML Level, Version and Namespace as of this release
  * of libSBML are the following: 
  * <ul>
  * <li> Level&nbsp;1 Version&nbsp;2 "http://www.sbml.org/sbml/level1"
  * <li> Level&nbsp;2 Version&nbsp;1 "http://www.sbml.org/sbml/level2"
  * <li> Level&nbsp;2 Version&nbsp;2 "http://www.sbml.org/sbml/level2/version2"
  * <li> Level&nbsp;2 Version&nbsp;3 "http://www.sbml.org/sbml/level2/version3"
  * <li> Level&nbsp;2 Version&nbsp;4 "http://www.sbml.org/sbml/level2/version4"
  * </ul>
  *
  * @param sb the SBase_t structure
  *
  * @return nonzero (true) if the level, version and namespace values of this 
  * SBML object correspond to a valid set of values, zero (false) otherwise.
  */
LIBSBML_EXTERN
int
SBase_hasValidLevelVersionNamespaceCombination(SBase_t *sb)
{
  return static_cast <int> (sb->hasValidLevelVersionNamespaceCombination());
}




/** @endcond doxygen-c-only */

LIBSBML_CPP_NAMESPACE_END
