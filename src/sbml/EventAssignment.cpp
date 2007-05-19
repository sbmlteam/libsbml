/**
 * @file    EventAssignment.cpp
 * @brief   Implementation of EventAssignment.
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
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
#include <sbml/Model.h>
#include <sbml/EventAssignment.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new EventAssignment with its variable and math attributes
 * set.
 */
EventAssignment::EventAssignment (const std::string& variable, const ASTNode* math)
 :
   SBase   ( variable, "", -1 )
 , mMath   ( 0        )
{
  if (math) mMath = math->deepCopy();
}


/**
 * Destroys this EventAssignment.
 */
EventAssignment::~EventAssignment ()
{
  delete mMath;
}


/**
 * Copy constructor. Creates a copy of this EventAssignment.
 */
EventAssignment::EventAssignment (const EventAssignment& orig) :
   SBase   ( orig )
 , mMath   ( 0    )
{
  if (orig.mMath) mMath = orig.mMath->deepCopy();
}


/**
 * Assignment operator
 */
EventAssignment& EventAssignment::operator=(const EventAssignment& rhs)
{
  this->SBase::operator =(rhs);
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Event's next
 * EventAssignment (if available).
 */
bool
EventAssignment::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this EventAssignment.
 */
SBase*
EventAssignment::clone () const
{
  return new EventAssignment(*this);
}


/**
 * @return the variable of this EventAssignment.
 */
const string&
EventAssignment::getVariable () const
{
  return getId();
}


/**
 * @return the math of this EventAssignment.
 */
const ASTNode*
EventAssignment::getMath () const
{
  return mMath;
}


/**
 * @return true if the variable of this EventAssignment has been set, false
 * otherwise.
 */
bool
EventAssignment::isSetVariable () const
{
  return isSetId();
}


/**
 * @return true if the math of this EventAssignment has been set, false
 * otherwise.
 */
bool
EventAssignment::isSetMath () const
{
  return (mMath != 0);
}


/**
 * Sets the variable of this EventAssignment to a copy of sid.
 */
void
EventAssignment::setVariable (const std::string& sid)
{
  setId(sid);
}


/**
 * Sets the math of this EventAssignment to a copy of the given ASTNode.
 */
void
EventAssignment::setMath (const ASTNode* math)
{
  if (mMath == math) return;


  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
EventAssignment::getTypeCode () const
{
  return SBML_EVENT_ASSIGNMENT;
}


/**
 * @return the name of this element ie "eventAssignment".
 */
const string&
EventAssignment::getElementName () const
{
  static const string name = "eventAssignment";
  return name;
}


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
EventAssignment::writeElements (XMLOutputStream& stream) const
{
  if (mMath) writeMathML(mMath, stream);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
EventAssignment::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();


  if (name == "math")
  {
    /* check for MathML namespace 
     * this may be explicitly declared here
     * or implicitly declared on the whole document
     */
    const XMLToken elem = stream.peek();
    unsigned int match = 0;
    int n;
    if (elem.getNamespaces().getLength() != 0)
    {
      for (n = 0; n < elem.getNamespaces().getLength(); n++)
      {
        if (!strcmp(elem.getNamespaces().getURI(n).c_str(), "http://www.w3.org/1998/Math/MathML"))
        {
          match = 1;
          break;
        }
      }
    }
    if (match == 0)
    {
      if( mSBML->getNamespaces() != NULL)
      /* check for implicit declaration */
      {
        for (n = 0; n < mSBML->getNamespaces()->getLength(); n++)
        {
          if (!strcmp(mSBML->getNamespaces()->getURI(n).c_str(), 
                                                     "http://www.w3.org/1998/Math/MathML"))
          {
            match = 1;
            break;
          }
        }
      }
    }
    if (match == 0)
    {
      mSBML->getErrorLog()->logError(10201);
    }

    delete mMath;
    mMath = readMathML(stream);
    read  = true;
  }
  else if (name == "annotation")
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
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
EventAssignment::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // variable: SId  { use="required" }  (L2v1, L2v2)
  //
  attributes.readInto("variable", mId);
  SBase::checkIdSyntax();


  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
EventAssignment::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // variable: SId  { use="required" }  (L2v1, L2v2)
  //
  stream.writeAttribute("variable", mId);


  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond doxygen-libsbml-internal */


/**
 * @return a (deep) copy of this ListOfEventAssignments.
 */
SBase*
ListOfEventAssignments::clone () const
{
  return new ListOfEventAssignments(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfEventAssignments::getItemTypeCode () const
{
  return SBML_EVENT_ASSIGNMENT;
}


/**
 * @return the name of this element ie "listOfEventAssignments".
 */
const string&
ListOfEventAssignments::getElementName () const
{
  static const string name = "listOfEventAssignments";
  return name;
}


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its
 * siblings or -1 (default) to indicate the position is not significant.
 */
int
ListOfEventAssignments::getElementPosition () const
{
  return 3;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfEventAssignments::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "eventAssignment")
  {
    object = new EventAssignment();
    mItems.push_back(object);
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */



/** @cond doxygen-c-only */


/**
 * Creates a new, empty EventAssignment_t structure and returns a pointer
 * to it.
 * 
 * @return the freshly-created EventAssignment_t structure
 *
 * @note It is worth emphasizing that valid EventAssignment definitions
 * must have a value for the "variable".  If no variable is provided at the
 * time of an EventAssignment_t's creation, the value is left as the empty
 * string.  Callers are cautioned to set the value using
 * EventAssignment_setVariable() soon after invoking this constructor.
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_create (void)
{
  return new(nothrow) EventAssignment;
}


/**
 * Creates a new EventAssignment_t with the given values for the "variable"
 * attribute and "math" subelement, and returns a pointer to it.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   eventassign = EventAssignment_create();
 *   EventAssignment_setId(eventassign, variable);
 *   EventAssignment_setMath(eventassign, math);
 * @endcode
 * 
 * @return the freshly-created EventAssignment_t structure
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_createWithVarAndMath (const char *variable, ASTNode_t* math)
{
  return new(nothrow) EventAssignment(variable ? variable : "", math);
}


/**
 * Frees the given EventAssignment_t structure.
 *
 * @param ea the EventAssignment_t to be freed.
 */
LIBSBML_EXTERN
void
EventAssignment_free (EventAssignment_t *ea)
{
  delete ea;
}


/**
 * Creates a (deep) copy of the given EventAssignment_t structure.
 *
 * @param ea the EventAssignment_t to be copied
 * 
 * @return a (deep) copy of @p ea.
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_clone (const EventAssignment_t *ea)
{
  return static_cast<EventAssignment*>( ea->clone() );
}


/**
 * Gets the value of the "variable" attribute of this EventAssignment_t
 * structure.
 *
 * @param ea the EventAssignment_t structure to query.
 *
 * @return the identifier stored in the "variable" attribute of @p ea.
 */
LIBSBML_EXTERN
const char *
EventAssignment_getVariable (const EventAssignment_t *ea)
{
  return ea->isSetVariable() ? ea->getVariable().c_str() : NULL;
}


/**
 * Gets the mathematical formula stored in the given EventAssignment_t
 * structure.
 *
 * @param ea the EventAssignment_t structure to query.
 *
 * @return the ASTNode tree stored in @p ea.
 */
LIBSBML_EXTERN
const ASTNode_t *
EventAssignment_getMath (const EventAssignment_t *ea)
{
  return ea->getMath();
}


/**
 * Predicate for testing whether the attribute "variable" of the
 * given EventAssignment_t structure has been set.
 *
 * @param ea the EventAssignment_t structure to query.
 * 
 * @return nonzero (for true) if the "variable" attribute of @p ea
 * has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
EventAssignment_isSetVariable (const EventAssignment_t *ea)
{
  return static_cast<int>( ea->isSetVariable() );
}


/**
 * Predicate for testing whether the attribute "variable" of the
 * given EventAssignment_t structure has been set.
 *
 * @param ea the EventAssignment_t structure to query.
 * 
 * @return nonzero (for true) if the "variable" attribute of @p ea
 * has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
EventAssignment_isSetMath (const EventAssignment_t *ea)
{
  return static_cast<int>( ea->isSetMath() );
}


/**
 * Sets the attribute "variable" of the given EventAssignment_t structure
 * to a copy of the given identifier string.
 *
 * @param ea the EventAssignment_t to set.
 * @param sid the identifier of a Compartment, Species or (global)
 * Parameter defined in this model.
 */
LIBSBML_EXTERN
void
EventAssignment_setVariable (EventAssignment_t *ea, const char *sid)
{
  ea->setVariable(sid ? sid : "");
}


/**
 * Sets the "math" subelement content of the given EventAssignment_t
 * structure to the given ASTNode.
 *
 * The given @p math ASTNode is copied.
 *
 * @param ea the EventAssignment_t to set.
 * @param math the ASTNode to copy into @p ea
 */
LIBSBML_EXTERN
void
EventAssignment_setMath (EventAssignment_t *ea, const ASTNode_t *math)
{
  ea->setMath(math);
}


/** @endcond doxygen-c-only */
