/**
 * @file    Trigger.cpp
 * @brief   Implementation of Trigger.
 * @author  Sarah Keating
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

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/Parameter.h>
#include <sbml/Trigger.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Trigger, optionally with its formula set.
 */
Trigger::Trigger (   const ASTNode* math ) :
   SBase		  (  -1 )
 , mMath      ( 0              )
{
  if (math) mMath = math->deepCopy();
}


/**
 * Destroys this Trigger.
 */
Trigger::~Trigger ()
{
  delete mMath;
}


/**
 * Copy constructor. Creates a copy of this Trigger.
 */
Trigger::Trigger (const Trigger& orig) :
   SBase          ( orig )
 , mMath          ( 0    )
{
  if (orig.mMath) mMath = orig.mMath->deepCopy();
}


/**
 * Assignment operator.
 */
Trigger& Trigger::operator=(const Trigger& rhs)
{
  this->SBase::operator =(rhs);
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 */
bool
Trigger::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this Trigger.
 */
SBase*
Trigger::clone () const
{
  return new Trigger(*this);
}


/**
 * @return the math of this Trigger.
 */
const ASTNode*
Trigger::getMath () const
{
  return mMath;
}


/**
 * @return true if the math (or equivalently the formula) of this
 * Trigger has been set, false otherwise.
 */
bool
Trigger::isSetMath () const
{
  return (mMath != 0);
}



/**
 * Sets the math of this Trigger to a copy of the given ASTNode.
 */
void
Trigger::setMath (const ASTNode* math)
{
  if (mMath == math) return;


  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;

}


/**
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Trigger::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Trigger::getTypeCode () const
{
  return SBML_TRIGGER;
}


/**
 * @return the name of this element ie "trigger".
 */
const string&
Trigger::getElementName () const
{
  static const string name = "trigger";
  return name;
}


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
Trigger::getElementPosition () const
{
  return 0;
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
Trigger::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  if (name == "math")
  {
    // if this is level 1 there shouldnt be any math!!!
    if (getLevel() == 1) 
    {
      logError(SBMLError::NotSchemaConformant, getLevel(), getVersion(),
              "SBML Level 1 does not support MathML");
      delete mMath;
      return false;
    }

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
Trigger::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
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
Trigger::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
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
Trigger::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getLevel() == 2 && isSetMath() ) writeMathML(getMath(), stream);
}
/** @endcond doxygen-libsbml-internal */



/** @cond doxygen-c-only */

/**
 * Creates a new Trigger and returns a pointer to it.
 */
LIBSBML_EXTERN
Trigger_t *
Trigger_create (void)
{
  return new(nothrow) Trigger;
}


/**
 * Creates a new Trigger_t structure with the given math.
 *
 * This is functionally equivalent to
 * @code
 *   Trigger_t *t = Trigger_create();
 *   Trigger_setMath(math);
 * @endcode.
 *
 * @param math an ASTNode_t structure representing the mathematical 
 * formula for the trigger expressions.
 *
 * @return the newly constructed Trigger_t structure.
 */
LIBSBML_EXTERN
Trigger_t *
Trigger_createWithMath (const ASTNode_t *math)
{
  return new(nothrow) Trigger(math);
}


/**
 * Frees the given Trigger.
 */
LIBSBML_EXTERN
void
Trigger_free (Trigger_t *t)
{
  delete t;
}


/**
 * @return a (deep) copy of this Trigger.
 */
LIBSBML_EXTERN
SBase_t *
Trigger_clone (const Trigger_t *t)
{
  return t->clone();
}


/**
 * @return the math of this Trigger.
 */
LIBSBML_EXTERN
const ASTNode_t *
Trigger_getMath (const Trigger_t *t)
{
  return t->getMath();
}


/**
 * @return true (non-zero) if the math (or equivalently the formula) of
 * this Trigger has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Trigger_isSetMath (const Trigger_t *t)
{
  return static_cast<int>( t->isSetMath() );
}


/**
 * Sets the math of this Trigger to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Trigger_setMath (Trigger_t *t, const ASTNode_t *math)
{
  t->setMath(math);
}


/** @endcond doxygen-c-only */
