/**
 * @file    Delay.cpp
 * @brief   Implementation of Delay.
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
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/Parameter.h>
#include <sbml/Delay.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new Delay, optionally with the given "math".
 */
Delay::Delay (   const ASTNode* math ) :
   SBase		  (  -1 )
 , mMath      ( 0              )
{
  if (math) mMath = math->deepCopy();
}


/*
 * Destroys this Delay.
 */
Delay::~Delay ()
{
  delete mMath;
}


/*
 * Copy constructor. Creates a copy of this Delay.
 */
Delay::Delay (const Delay& orig) :
   SBase          ( orig                 )
 , mMath          ( 0                   )
{
  if (orig.mMath) mMath = orig.mMath->deepCopy();
}


/*
 * Assignment operator
 */
Delay& Delay::operator=(const Delay& rhs)
{
  this->SBase::operator =(rhs);
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
Delay::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/*
 * @return a (deep) copy of this Delay.
 */
SBase*
Delay::clone () const
{
  return new Delay(*this);
}



/*
 * @return the math of this Delay.
 */
const ASTNode*
Delay::getMath () const
{
  return mMath;
}


/*
 * @return true if the math (or equivalently the formula) of this
 * Delay has been set, false otherwise.
 */
bool
Delay::isSetMath () const
{
  return (mMath != 0);
}


/*
 * Sets the math of this Delay to a copy of the given ASTNode.
 */
void
Delay::setMath (const ASTNode* math)
{
  if (mMath == math) return;


  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;
}


/*
  * Calculates and returns a UnitDefinition that expresses the units
  * returned by the math expression of this InitialAssignment.
  */
UnitDefinition * 
Delay::getDerivedUnitDefinition()
{
  if (!getSBMLDocument()->getModel()->isPopulatedListFormulaUnitsData())
  {
    getSBMLDocument()->getModel()->populateListFormulaUnitsData();
  }

  return getSBMLDocument()->getModel()
    ->getFormulaUnitsData(getId(), SBML_EVENT)
    ->getUnitDefinition();
}

/*
 * Predicate returning @c true or @c false depending on whether 
 * the math expression of this InitialAssignment contains
 * parameters/numbers with undeclared units that cannot be ignored.
 */
bool 
Delay::containsUndeclaredUnits()
{
  if (!getSBMLDocument()->getModel()->isPopulatedListFormulaUnitsData())
  {
    getSBMLDocument()->getModel()->populateListFormulaUnitsData();
  }

  return (getSBMLDocument()->getModel()
    ->getFormulaUnitsData(getId(), SBML_EVENT)
    ->getContainsUndeclaredUnits());
}


/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Delay::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
}


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Delay::getTypeCode () const
{
  return SBML_DELAY;
}


/*
 * @return the name of this element ie "delay".
 */
const string&
Delay::getElementName () const
{
  static const string name = "delay";
  return name;
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
Delay::getElementPosition () const
{
  return 1;
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
Delay::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  if (name == "math")
  {
    // if this is level 1 there shouldnt be any math!!!
    if (getLevel() == 1) 
    {
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "SBML Level 1 does not support MathML.");
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
      logError(InvalidMathElement);
    }
    delete mMath;
    mMath = readMathML(stream);
    read  = true;
  }

  return read;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Delay::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  if (level == 2)
  {
    if (version > 1)
      expectedAttributes.push_back("metaid");
    if (version > 2)
      expectedAttributes.push_back("sboTerm");
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      logUnknownAttribute(name, level, version, "<delay>");
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Delay::writeAttributes (XMLOutputStream& stream) const
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
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Delay::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getLevel() == 2 && isSetMath() ) writeMathML(getMath(), stream);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new, empty Delay_t structure.
 *
 * @return the new Delay_t structure.
 */
LIBSBML_EXTERN
Delay_t *
Delay_create (void)
{
  return new(nothrow) Delay;
}


/**
 * Creates a new Delay_t structure with the given math.
 *
 * This is functionally equivalent to
 * @code
 *   Delay_t *t = Delay_create();
 *   Delay_setMath(math);
 * @endcode.
 *
 * @param math an ASTNode_t structure representing the mathematical 
 * formula for the delay expressions.
 *
 * @return the newly constructed Delay_t structure.
 */
LIBSBML_EXTERN
Delay_t *
Delay_createWithMath (const ASTNode_t *math)
{
  return new(nothrow) Delay(math);
}


/**
 * Frees the given Delay_t structure.
 *
 * @param t the Delay_t structure to free.
 */
LIBSBML_EXTERN
void
Delay_free (Delay_t *t)
{
  delete t;
}


/**
 * Creates and returns a deep copy of the given Delay_t structure.
 *
 * @param t the Delay_t structure to copy. 
 *
 * @return a (deep) copy of the given Delay_t structure @p t.
 */
LIBSBML_EXTERN
SBase_t *
Delay_clone (const Delay_t *t)
{
  return t->clone();
}


/**
 * Get the mathematical formula for a Delay_t structure and return it as
 * as an ASTNode structure.
 *
 * @param t the Delay_t structure to query.
 * 
 * @return an ASTNode_t structure representing the expression tree.
 */
LIBSBML_EXTERN
const ASTNode_t *
Delay_getMath (const Delay_t *t)
{
  return t->getMath();
}


/**
 * Predicate to test whether the formula for the given Delay_t structure
 * has been set.
 *
 * @param t the Delay_t structure to query
 *
 * @return @c true if the formula (meaning the @c math subelement) of
 * this Delay has been set, @c false otherwise.
 */
LIBSBML_EXTERN
int
Delay_isSetMath (const Delay_t *t)
{
  return static_cast<int>( t->isSetMath() );
}


/**
 * Sets the delay expression of the given Delay_t instance to a copy of the
 * given ASTNode_t structure.
 *
 * @param t the Delay_t structure to set.
 * @param math an ASTNode representing a formula tree.
 */
LIBSBML_EXTERN
void
Delay_setMath (Delay_t *t, const ASTNode_t *math)
{
  t->setMath(math);
}

/**
  * Calculates and returns a UnitDefinition_t that expresses the units
  * returned by the math expression of this Delay_t.
  *
  * @return a UnitDefinition_t that expresses the units of the math 
  * expression of this Delay_t.
  *
  * @note The units are calculated by applying the mathematics 
  * from the expression to the units of the <ci> elements used 
  * within the expression. Where there are parameters/numbers
  * with undeclared units the UnitDefinition_t returned by this
  * function may not accurately represent the units of the expression.
  * 
  * @see Delay_containsUndeclaredUnits()
  */
LIBSBML_EXTERN
UnitDefinition_t * 
Delay_getDerivedUnitDefinition(Delay_t *d)
{
  return d->getDerivedUnitDefinition();
}


/**
  * Predicate returning @c true or @c false depending on whether 
  * the math expression of this Delay_t contains
  * parameters/numbers with undeclared units.
  * 
  * @return @c true if the math expression of this Delay_t
  * includes parameters/numbers 
  * with undeclared units, @c false otherwise.
  *
  * @note a return value of @c true indicates that the UnitDefinition_t
  * returned by the getDerivedUnitDefinition function may not 
  * accurately represent the units of the expression.
  *
  * @see Delay_getDerivedUnitDefinition()
  */
LIBSBML_EXTERN
int 
Delay_containsUndeclaredUnits(Delay_t *d)
{
  return static_cast<int>(d->containsUndeclaredUnits());
}


/** @endcond doxygen-c-only */
