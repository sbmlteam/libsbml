/**
 * @file    StoichiometryMath.cpp
 * @brief   Implementation of StoichiometryMath.
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
#include <sbml/StoichiometryMath.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new StoichiometryMath, optionally with its math set.
 */
StoichiometryMath::StoichiometryMath (   const ASTNode* math ) :
   SBase		  (  -1 )
 , mMath      ( 0              )
{
  if (math) mMath = math->deepCopy();
}


/*
 * Destroys this StoichiometryMath.
 */
StoichiometryMath::~StoichiometryMath ()
{
  delete mMath;
}


/*
 * Copy constructor. Creates a copy of this StoichiometryMath.
 */
StoichiometryMath::StoichiometryMath (const StoichiometryMath& rhs) :
   SBase          ( rhs )
 , mMath          ( 0    )
{
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
}


/*
 * Assignment operator.
 */
StoichiometryMath& StoichiometryMath::operator=(const StoichiometryMath& rhs)
{
  this->SBase::operator =(rhs);
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
StoichiometryMath::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/*
 * @return a (deep) copy of this StoichiometryMath.
 */
SBase*
StoichiometryMath::clone () const
{
  return new StoichiometryMath(*this);
}


/*
 * @return the math of this StoichiometryMath.
 */
const ASTNode*
StoichiometryMath::getMath () const
{
  return mMath;
}


/*
 * @return true if the math (or equivalently the formula) of this
 * StoichiometryMath has been set, false otherwise.
 */
bool
StoichiometryMath::isSetMath () const
{
  return (mMath != 0);
}



/*
 * Sets the math of this StoichiometryMath to a copy of the given ASTNode.
 */
void
StoichiometryMath::setMath (const ASTNode* math)
{
  if (mMath == math) return;


  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;

}


/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
StoichiometryMath::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
}


/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
StoichiometryMath::setParentSBMLObject (SBase* sb)
{
  mParentSBMLObject = sb;
}


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
StoichiometryMath::getTypeCode () const
{
  return SBML_STOICHIOMETRY_MATH;
}


/*
 * @return the name of this element ie "stoichiometryMath".
 */
const string&
StoichiometryMath::getElementName () const
{
  static const string name = "stoichiometryMath";
  return name;
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
StoichiometryMath::getElementPosition () const
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
StoichiometryMath::readOtherXML (XMLInputStream& stream)
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
StoichiometryMath::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);
  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("metaid");
  if (level == 2 && version == 3)
  {
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
      logUnknownAttribute(name, level, version, "<stoichiometryMath>");
    }
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 3) 
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
StoichiometryMath::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3)
  //
  if (level == 2 && version == 3) 
    SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond doxygen-libsbml-internal */


/*
  * Calculates and returns a UnitDefinition that expresses the units
  * returned by the math expression of this StoichiometryMath.
  */
UnitDefinition * 
StoichiometryMath::getDerivedUnitDefinition()
{
  if (!isSetMath())
    return NULL;
  if (!getSBMLDocument()->getModel()->isPopulatedListFormulaUnitsData())
  {
    getSBMLDocument()->getModel()->populateListFormulaUnitsData();
  }

  if (getSBMLDocument()->getModel()
    ->getFormulaUnitsData(getId(), getTypeCode()))
  {
    return getSBMLDocument()->getModel()
      ->getFormulaUnitsData(getId(), getTypeCode())
      ->getUnitDefinition();
  }
  else
  {
    return NULL;
  }
}

/*
 * Predicate returning @c true or @c false depending on whether 
 * the math expression of this StoichiometryMath contains
 * parameters/numbers with undeclared units that cannot be ignored.
 */
bool 
StoichiometryMath::containsUndeclaredUnits()
{
  if (!isSetMath())
    return false;
  if (!getSBMLDocument()->getModel()->isPopulatedListFormulaUnitsData())
  {
    getSBMLDocument()->getModel()->populateListFormulaUnitsData();
  }

  if (getSBMLDocument()->getModel()
    ->getFormulaUnitsData(getId(), getTypeCode()))
  {
    return getSBMLDocument()->getModel()
      ->getFormulaUnitsData(getId(), getTypeCode())
      ->getContainsUndeclaredUnits();
  }
  else
  {
    return false;
  }
}


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
StoichiometryMath::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getLevel() == 2 && isSetMath() ) writeMathML(getMath(), stream);
}
/** @endcond doxygen-libsbml-internal */




/**
 * Creates a new StoichiometryMath and returns a pointer to it.
 */
LIBSBML_EXTERN
StoichiometryMath_t *
StoichiometryMath_create (void)
{
  return new(nothrow) StoichiometryMath;
}


/**
 * Creates a new StoichiometryMath_t structure with the given math.
 *
 * This is functionally equivalent to
 * @code
 *   StoichiometryMath_t *t = StoichiometryMath_create();
 *   StoichiometryMath_setMath(math);
 * @endcode.
 *
 * @param math an ASTNode_t structure representing the mathematical 
 * formula for the stoichiometryMath expressions.
 *
 * @return the newly constructed StoichiometryMath_t structure.
 */
LIBSBML_EXTERN
StoichiometryMath_t *
StoichiometryMath_createWithMath (const ASTNode_t *math)
{
  return new(nothrow) StoichiometryMath(math);
}


/**
 * Frees the given StoichiometryMath.
 */
LIBSBML_EXTERN
void
StoichiometryMath_free (StoichiometryMath_t *stoichMath)
{
  delete stoichMath;
}


/**
 * @return a (deep) copy of this StoichiometryMath.
 */
LIBSBML_EXTERN
SBase_t *
StoichiometryMath_clone (const StoichiometryMath_t *stoichMath)
{
  return stoichMath->clone();
}


/**
 * @return the stoichMath of this StoichiometryMath.
 */
LIBSBML_EXTERN
const ASTNode_t *
StoichiometryMath_getMath (const StoichiometryMath_t *stoichMath)
{
  return stoichMath->getMath();
}


/**
 * @return true (non-zero) if the stoichMath (or equivalently the formula) of
 * this StoichiometryMath has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
StoichiometryMath_isSetMath (const StoichiometryMath_t *stoichMath)
{
  return static_cast<int>( stoichMath->isSetMath() );
}


/**
 * Sets the math of this StoichiometryMath to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
StoichiometryMath_setMath (StoichiometryMath_t *stoichMath, const ASTNode_t *math)
{
  stoichMath->setMath(math);
}


/**
 * Calculates and returns a UnitDefinition that expresses the units
 * returned by the math expression in this StoichiometryMath.
 *
 * The units are calculated based on the mathematical expression in the
 * StoichiometryMath and the model quantities referenced by
 * <code>&lt;ci&gt;</code> elements used within that expression.  The
 * getDerivedUnitDefinition() method returns the calculated units.
 * 
 * @warning Note that it is possible the "math" expression in the
 * StoichiometryMath contains pure numbers or parameters with undeclared
 * units.  In those cases, it is not possible to calculate the units of
 * the overall expression without making assumptions.  LibSBML does not
 * make assumptions about the units, and getDerivedUnitDefinition() only
 * returns the units as far as it is able to determine them.  For
 * example, in an expression <em>X + Y</em>, if <em>X</em> has
 * unambiguously-defined units and <em>Y</em> does not, it will return
 * the units of <em>X</em>.  <strong>It is important that callers also
 * invoke the method</strong> containsUndeclaredUnits() <strong>to
 * determine whether this situation holds</strong>.  Callers may wish to
 * take suitable actions in those scenarios.
 *
 * @param stoichMath the StoichiometryMath structure to check
 *
 * @return A UnitDefinition that expresses the units of the math 
 *
 * @see containsUndeclaredUnits()
 */
LIBSBML_EXTERN
UnitDefinition_t * 
StoichiometryMath_getDerivedUnitDefinition(StoichiometryMath_t *stoichMath)
{
  return static_cast<StoichiometryMath*>(stoichMath)->getDerivedUnitDefinition();
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * the math expression of this StoichiometryMath contains
 * parameters/numbers with undeclared units.
 *
 * @param stoichMath the StoichiometryMath structure to check
 * 
 * @return @c true if the math expression of this StoichiometryMath
 * includes parameters/numbers 
 * with undeclared units, @c false otherwise.
 *
 * @note A return value of @c true indicates that the UnitDefinition
 * returned by getDerivedUnitDefinition() may not accurately represent
 * the units of the expression.
 *
 * @see StoichiometryMath_getDerivedUnitDefinition()
 */
LIBSBML_EXTERN
int 
StoichiometryMath_containsUndeclaredUnits(StoichiometryMath_t *stoichMath)
{
  return static_cast<int>(static_cast<StoichiometryMath*>(stoichMath)
                                         ->containsUndeclaredUnits());
}


