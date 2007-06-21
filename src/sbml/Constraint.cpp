/**
 * @file    Constraint.cpp
 * @brief   Implementations of Constraint and ListOfConstraints.
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

#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/Constraint.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Constraint
 * set.
 */
Constraint::Constraint (const ASTNode* math) :
   SBase   ( -1 )
 , mMath   (  0 )
 , mMessage(  0 )
{
  if (math) mMath = math->deepCopy();
}


/**
 * Destroys this Constraint.
 */
Constraint::~Constraint ()
{
}


/**
 * Copy constructor. Creates a copy of this Constraint.
 */
Constraint::Constraint (const Constraint& orig) :
   SBase   ( orig )
 , mMath   ( 0   )
 , mMessage( 0   )
{
  if (orig.mMath)    mMath    = orig.mMath->deepCopy();
  if (orig.mMessage) mMessage = new XMLNode(*orig.mMessage);
}


/**
 * Assignment operator
 */
Constraint& Constraint::operator=(const Constraint& rhs)
{
  this->SBase::operator =(rhs);

  if (rhs.mMath)    mMath    = rhs.mMath->deepCopy();
  if (rhs.mMessage) mMessage = new XMLNode(*rhs.mMessage);

  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * Constraint (if available).
 */
bool
Constraint::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this Constraint.
 */
SBase*
Constraint::clone () const
{
  return new Constraint(*this);
}


/**
 * @return the message for this Constraint.
 */
const XMLNode*
Constraint::getMessage () const
{
  return mMessage;
}


/**
 * @return the math for this Constraint.
 */
const ASTNode*
Constraint::getMath () const
{
  return mMath;
}


/**
 * @return true if the message of this Constraint has been set,
 * false otherwise.
 */
bool
Constraint::isSetMessage () const
{
  return (mMessage != 0);
}


/**
 * @return true if the math for this Constraint has been set,
 * false otherwise.
 */
bool
Constraint::isSetMath () const
{
  return (mMath != 0);
}


/**
 * Sets the message of this Constraint to a copy of xhtml.
 */
void
Constraint::setMessage (const XMLNode* xhtml)
{
  if (mMessage == xhtml) return;


  delete mMessage;
  mMessage = (xhtml != 0) ? new XMLNode(*xhtml) : 0;

}


/**
 * Sets the math of this Constraint to a copy of the given
 * ASTNode.
 */
void
Constraint::setMath (const ASTNode* math)
{
  if (mMath == math) return;


  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;
}


/**
 * Unsets the message of this Constraint.
 */
void 
Constraint::unsetMessage ()
{
  delete mMessage;
  mMessage = 0;
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Constraint::getTypeCode () const
{
  return SBML_CONSTRAINT;
}


/**
 * @return the name of this element ie "constraint".
 */
const string&
Constraint::getElementName () const
{
  static const string name = "constraint";
  return name;
}


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
Constraint::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  if (name == "math")
  {
    // if this is level 1 there shouldnt be any math!!!
    if (getLevel() == 1) 
    {
      logError(SBMLError::NotSchemaConformant,
              "SBML Level 1 does not support MathML");
      delete mMath;
      return false;
    }

    // If there's a <message>, it's supposed to show up first

    if (mMessage) logError(SBMLError::IncorrectOrderInConstraint);

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
      logError(SBMLError::InvalidMathElement);
    }

    delete mMath;
  
    mMath = readMathML(stream);
    read  = true;
  }
  else if (name == "message")
  {
    delete mMessage;

    mMessage = new XMLNode(stream);
    checkXHTML(mMessage);
    read     = true;
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
Constraint::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

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
Constraint::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Constraint::writeElements (XMLOutputStream& stream) const
{
  if (mMath) writeMathML(mMath, stream);
}
/** @endcond doxygen-libsbml-internal */


/**
 * @return a (deep) copy of this ListOfConstraints.
 */
SBase*
ListOfConstraints::clone () const
{
  return new ListOfConstraints(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfConstraints::getItemTypeCode () const
{
  return SBML_CONSTRAINT;
}


/**
 * @return the name of this element ie "listOfConstraints".
 */
const string&
ListOfConstraints::getElementName () const
{
  static const string name = "listOfConstraints";
  return name;
}


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfConstraints::getElementPosition () const
{
  return 10;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfConstraints::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "constraint")
  {
    object = new Constraint();
    mItems.push_back(object);
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */



/** @cond doxygen-c-only */


/**
 * Creates a new, empty Constraint_t structure and returns a pointer to it.
 *
 * @return a pointer to a Constraint_t structure
 */
LIBSBML_EXTERN
Constraint_t *
Constraint_create ()
{
  return new(nothrow) Constraint;
}


/**
 * Creates a new Constraint_t structure with the math set
 * and returns a pointer to it.
 *
 * @param math ASTNode_t structure representing the math.
 *
 * @return a pointer to a Constraint_t structure
 */
LIBSBML_EXTERN
Constraint_t *
Constraint_createWithMath (ASTNode_t * math)
{
  return new(nothrow) Constraint(math);
}


/**
 * Frees the given Constraint_t structure.
 */
LIBSBML_EXTERN
void
Constraint_free (Constraint_t *c)
{
  delete c;
}


/**
 * Creates and returns a deep copy of the given Constraint_t structure.
 *
 * @param c the Constraint_t structure to copy
 * 
 * @return a (deep) copy of Constraint_t.
 */
LIBSBML_EXTERN
Constraint_t *
Constraint_clone (const Constraint_t *c)
{
  return static_cast<Constraint*>( c->clone() );
}


/**
 * Get the message, if any, associated with this Constraint
 *
 * @param c the Constraint_t structure 
 * 
 * @return the message for this Constraint, as an XMLNode.
 */
LIBSBML_EXTERN
const XMLNode_t *
Constraint_getMessage (const Constraint_t *c)
{
  return c->getMessage();
}


/**
 * Get the mathematical expression of this Constraint
 *
 * @param c the Constraint_t structure 
 * 
 * @return the math for this Constraint, as an ASTNode.
 */
LIBSBML_EXTERN
const ASTNode_t *
Constraint_getMath (const Constraint_t *c)
{
  return c->getMath();
}


/**
 * Predicate returning @c true or @c false depending on whether a
 * message has been defined for this Constraint.
 *
 * @param c the Constraint_t structure 
 * 
 * @return a nonzero integer if the "message" subelement for this
 * Constraint has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Constraint_isSetMessage (const Constraint_t *c)
{
  return static_cast<int>( c->isSetMessage() );
}


/**
 * Predicate returning @c true or @c false depending on whether a
 * mathematical formula has been defined for this Constraint.
 *
 * @param c the Constraint_t structure 
 * 
 * @return a nonzero integer if the "math" subelement for this Constraint
 * has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Constraint_isSetMath (const Constraint_t *c)
{
  return static_cast<int>( c->isSetMath() );
}


/**
 * Sets the message of this Constraint.
 *
 * @param c the Constraint_t structure
 *
 * @param xhtml an XML tree containing XHTML content.
 */
LIBSBML_EXTERN
void
Constraint_setMessage (Constraint_t *c, const XMLNode_t *xhtml)
{
  c->setMessage(xhtml);
}


/**
 * Sets the mathematical expression of this Constraint.
 *
 * @param c the Constraint_t structure
 *
 * @param math an ASTNode expression to be assigned as the "math"
 * subelement of this Constraint
 */
LIBSBML_EXTERN
void
Constraint_setMath (Constraint_t *c, const ASTNode_t *math)
{
  c->setMath(math);
}


/**
 * Unsets the "message" subelement of this Constraint.
 *
 * @param c the Constraint_t structure
 */
LIBSBML_EXTERN
void 
Constraint_unsetMessage (Constraint_t *c)
{
  c->unsetMessage();
}


/** @endcond doxygen-c-only */
