/**
 * @file    Delay.cpp
 * @brief   SBML Delay
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
#include <sbml/Model.h>
#include <sbml/Parameter.h>
#include <sbml/Delay.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Delay, optionally with its formula, timeUnits and/or
 * substanceUnits set.
 */
Delay::Delay (   const string& formula ) :
   SBase		  (  -1 )
 , mFormula       ( formula        )
 , mMath          ( 0              )
{
}


/**
 * Destroys this Delay.
 */
Delay::~Delay ()
{
  delete mMath;
}


/**
 * Copy constructor. Creates a copy of this Delay.
 */
Delay::Delay (const Delay& rhs) :
   SBase          ( rhs                 )
 , mFormula       ( rhs.mFormula        )
 , mMath          ( 0                   )
{
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
}


/**
 * Assignment operator
 */
Delay& Delay::operator=(const Delay& rhs)
{
  this->SBase::operator =(rhs);
  mFormula = rhs.mFormula;
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 */
bool
Delay::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this Delay.
 */
SBase*
Delay::clone () const
{
  return new Delay(*this);
}


/**
 * @return the formula of this Delay.
 */
const string&
Delay::getFormula () const
{
  if (mFormula.empty() == true && mMath != 0)
  {
    char* s  = SBML_formulaToString(mMath);
    mFormula = s;

    free(s);
  }

  return mFormula;
}


/**
 * @return the math of this Delay.
 */
const ASTNode*
Delay::getMath () const
{
  if (mMath == 0 && mFormula.empty() == false)
  {
    mMath = SBML_parseFormula( mFormula.c_str() );
  }

  return mMath;
}


/**
 * @return true if the formula (or equivalently the math) of this
 * Delay has been set, false otherwise.
 */
bool
Delay::isSetFormula () const
{
  return (mFormula.empty() == false) || (mMath != 0);
}


/**
 * @return true if the math (or equivalently the formula) of this
 * Delay has been set, false otherwise.
 */
bool
Delay::isSetMath () const
{
  return isSetFormula();
}


/**
 * Sets the formula of this Delay to a copy of formula.
 */
void
Delay::setFormula (const string& formula)
{
  mFormula = formula;

  if (mMath)
  {
    delete mMath;
    mMath = 0;
  }
}


/**
 * Sets the math of this Delay to a copy of the given ASTNode.
 */
void
Delay::setMath (const ASTNode* math)
{
  if (mMath == math) return;


  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;

  mFormula.erase();
}


/**
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Delay::setSBMLDocument (SBMLDocument* d)
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
Delay::getTypeCode () const
{
  return SBML_DELAY;
}


/**
 * @return the name of this element ie "delay".
 */
const string&
Delay::getElementName () const
{
  static const string name = "delay";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
Delay::getElementPosition () const
{
  return 1;
}


/**
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
Delay::readAttributes (const XMLAttributes& attributes)
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


/**
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


/**
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
 * Creates a new Delay_t structure with the given formula.
 *
 * This is functionally equivalent to
 * @code
 *   Delay_t *t = Delay_create();
 *   Delay_set_Formula(formula);
 * @endcode.
 *
 * @param formula a string, the mathematical formula for the delay
 * expressions.
 *
 * @return the newly constructed Delay_t structure.
 */
LIBSBML_EXTERN
Delay_t *
Delay_createWith (const char *formula)
{
  string f  = formula ? formula : "";

  return new(nothrow) Delay(f);
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
 * as a string.
 *
 * @param t the Delay_t structure to query.
 * 
 * @return a string representing the formula of this delay expression.
 */
LIBSBML_EXTERN
const char *
Delay_getFormula (const Delay_t *t)
{
  return t->isSetFormula() ? t->getFormula().c_str() : NULL;
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
 * This is identical to the function Delay_isSetMath().  It is provided in
 * order to mirror the parallel between getFormula() and getMath().
 *
 * @param t the Delay_t structure to query
 *
 * @return @c true if the formula (meaning the @c math subelement) of
 * this Delay has been set, @c false otherwise.
 */
LIBSBML_EXTERN
int
Delay_isSetFormula (const Delay_t *t)
{
  return static_cast<int>( t->isSetFormula() );
}


/**
 * Predicate to test whether the formula for the given Delay_t structure
 * has been set.
 *
 * This is identical to the function Delay_isSetFormula().  It is provided
 * in order to mirror the parallel between getFormula() and getMath().
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
 * Sets the delay expression of the given Delay_t structure @p t to the
 * given string @p formula.
 *
 * The given @p formula string is copied.
 *
 * @param t the Delay_t structure to set.
 * @param formula the mathematical expression to use.
 */
LIBSBML_EXTERN
void
Delay_setFormula (Delay_t *t, const char *formula)
{
  t->setFormula(formula ? formula : "");
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


/** @endcond doxygen-c-only */
