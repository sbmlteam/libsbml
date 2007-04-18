/**
 * @file    Trigger.cpp
 * @brief   SBML Trigger
 * @author  Sarah Keating
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

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/Parameter.h>
#include <sbml/Trigger.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


/**
 * Creates a new Trigger, optionally with its formula set.
 */
Trigger::Trigger (   const string& formula ) :
   SBase ( -1)
 , mFormula       ( formula        )
 , mMath          ( 0              )
{
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
Trigger::Trigger (const Trigger& rhs) :
   SBase          ( rhs                 )
 , mFormula       ( rhs.mFormula        )
 , mMath          ( 0                   )
{
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
}


/**
 * Assignment operator.
 */
Trigger& Trigger::operator=(const Trigger& rhs)
{
  this->SBase::operator =(rhs);
  mFormula  = rhs.mFormula;
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
 * @return the formula of this Trigger.
 */
const string&
Trigger::getFormula () const
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
 * @return the math of this Trigger.
 */
const ASTNode*
Trigger::getMath () const
{
  if (mMath == 0 && mFormula.empty() == false)
  {
    mMath = SBML_parseFormula( mFormula.c_str() );
  }

  return mMath;
}


/**
 * @return true if the formula (or equivalently the math) of this
 * Trigger has been set, false otherwise.
 */
bool
Trigger::isSetFormula () const
{
  return (mFormula.empty() == false) || (mMath != 0);
}


/**
 * @return true if the math (or equivalently the formula) of this
 * Trigger has been set, false otherwise.
 */
bool
Trigger::isSetMath () const
{
  return isSetFormula();
}


/**
 * Sets the formula of this Trigger to a copy of formula.
 */
void
Trigger::setFormula (const string& formula)
{
  mFormula = formula;

  if (mMath)
  {
    delete mMath;
    mMath = 0;
  }
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

  mFormula.erase();
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


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
Trigger::getElementPosition () const
{
  return 0;
}


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
 * Creates a new Trigger with the given formula and 
 * returns a pointer to it.  This convenience function
 * is functionally equivalent to:
 *
 *   Trigger_t *t = Trigger_create();
 *   Trigger_setFormula(t, formula);
 */
LIBSBML_EXTERN
Trigger_t *
Trigger_createWith ( const char *formula)
{
  string f  = formula        ? formula        : "";

  return new(nothrow) Trigger(f);
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
 * @return the formula of this Trigger.
 */
LIBSBML_EXTERN
const char *
Trigger_getFormula (const Trigger_t *t)
{
  return t->isSetFormula() ? t->getFormula().c_str() : NULL;
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
 * @return true (non-zero) if the formula (or equivalently the math) of
 * this Trigger has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Trigger_isSetFormula (const Trigger_t *t)
{
  return static_cast<int>( t->isSetFormula() );
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
 * Sets the formula of this Trigger to a copy of formula.
 */
LIBSBML_EXTERN
void
Trigger_setFormula (Trigger_t *t, const char *formula)
{
  t->setFormula(formula ? formula : "");
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
