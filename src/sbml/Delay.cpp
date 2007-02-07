/**
 * \file    Delay.cpp
 * \brief   SBML Delay
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
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

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include "SBML.h"
#include "SBMLVisitor.h"
#include "SBMLDocument.h"
#include "Model.h"
#include "Parameter.h"
#include "Delay.h"


using namespace std;


/**
 * Creates a new Delay, optionally with its formula, timeUnits and/or
 * substanceUnits set.
 */
Delay::Delay (   const string& formula ) :
   mFormula       ( formula        )
 , mMath          ( 0              )
 , mSBOTerm       ( -1             )
{
}


/**
 * Copies this Delay.
 */
Delay::Delay (const Delay& rhs) :
   SBase          ( rhs                 )
 , mFormula       ( rhs.mFormula        )
 , mMath          ( 0                   )
 , mSBOTerm       ( rhs.mSBOTerm        )
{
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
}


/**
 * Destroys this Delay.
 */
Delay::~Delay ()
{
  delete mMath;
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
 * @return the sboTerm of this Delay as an integer.  If not set,
 * sboTerm will be -1.  Use SBML::sboTermToString() to convert the
 * sboTerm to a zero-padded, seven digit string.
 */
int
Delay::getSBOTerm () const
{
  return mSBOTerm;
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
 * @return true if the sboTerm of this Delay has been set, false
 * otherwise.
 */
bool
Delay::isSetSBOTerm () const
{
  return (mSBOTerm != -1);
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
 * Sets the sboTerm field of this Delay to value.
 */
void
Delay::setSBOTerm (int sboTerm)
{
  mSBOTerm = sboTerm;
}


/**
 * Unsets the sboTerm of this Delay.
 */
void
Delay::unsetSBOTerm ()
{
  mSBOTerm = -1;
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
 * Subclasses should override this method to return XML element name of
 * this SBML object.
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
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    mCVTerms = new List();
    parseRDFAnnotation(mAnnotation, mCVTerms);
    checkAnnotation();
    read = true;
  }
  else if (name == "notes")
  {
    delete mNotes;
    mNotes = new XMLNode(stream);
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

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 2) mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());
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

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 2) SBML::writeSBOTerm(stream, mSBOTerm);
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





/**
 * Creates a new Delay and returns a pointer to it.
 */
LIBSBML_EXTERN
Delay_t *
Delay_create (void)
{
  return new(nothrow) Delay;
}


/**
 * Creates a new Delay with the given formula, timeUnits and
 * substanceUnits and returns a pointer to it.  This convenience function
 * is functionally equivalent to:
 *
 *   Delay_t *t = Delay_create();
 *   Delay_setFormula(t, formula);
 *   Delay_setTimeUnits(t, timeUnits);
 *   ...;
 */
LIBSBML_EXTERN
Delay_t *
Delay_createWith ( const char *formula)
{
  string f  = formula        ? formula        : "";

  return new(nothrow) Delay(f);
}


/**
 * Frees the given Delay.
 */
LIBSBML_EXTERN
void
Delay_free (Delay_t *t)
{
  delete t;
}


/**
 * @return a (deep) copy of this Delay.
 */
LIBSBML_EXTERN
SBase_t *
Delay_clone (const Delay_t *t)
{
  return t->clone();
}


/**
 * @return the formula of this Delay.
 */
LIBSBML_EXTERN
const char *
Delay_getFormula (const Delay_t *t)
{
  return t->isSetFormula() ? t->getFormula().c_str() : NULL;
}


/**
 * @return the math of this Delay.
 */
LIBSBML_EXTERN
const ASTNode_t *
Delay_getMath (const Delay_t *t)
{
  return t->getMath();
}


/**
 * @return the sboTerm of this Delay as an integer.  If not set,
 * sboTerm will be -1.  Use SBML_sboTermToString() to convert the sboTerm
 * to a zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
Delay_getSBOTerm (const Delay_t *t)
{
  return t->getSBOTerm();
}


/**
 * @return true (non-zero) if the formula (or equivalently the math) of
 * this Delay has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Delay_isSetFormula (const Delay_t *t)
{
  return static_cast<int>( t->isSetFormula() );
}


/**
 * @return true (non-zero) if the math (or equivalently the formula) of
 * this Delay has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Delay_isSetMath (const Delay_t *t)
{
  return static_cast<int>( t->isSetMath() );
}


/**
 * @return true (non-zero) if the substanceUnits of this Delay has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Delay_isSetSBOTerm (const Delay_t *t)
{
  return static_cast<int>( t->isSetSBOTerm() );
}


/**
 * Sets the formula of this Delay to a copy of formula.
 */
LIBSBML_EXTERN
void
Delay_setFormula (Delay_t *t, const char *formula)
{
  t->setFormula(formula ? formula : "");
}


/**
 * Sets the math of this Delay to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Delay_setMath (Delay_t *t, const ASTNode_t *math)
{
  t->setMath(math);
}


/**
 * Sets the sboTerm field of this Delay to value.
 */
LIBSBML_EXTERN
void
Delay_setSBOTerm (Delay_t *t, int sboTerm)
{
  t->setSBOTerm(sboTerm);
}


/**
 * Unsets the sboTerm of this Delay.
 */
LIBSBML_EXTERN
void
Delay_unsetSBOTerm (Delay_t *t)
{
  t->unsetSBOTerm();
}
