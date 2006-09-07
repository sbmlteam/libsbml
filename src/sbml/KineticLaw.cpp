/**
 * \file    KineticLaw.cpp
 * \brief   SBML KineticLaw
 * \author  Ben Bornstein
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
#include "KineticLaw.h"


using namespace std;


/**
 * Creates a new KineticLaw, optionally with its formula, timeUnits and/or
 * substanceUnits set.
 */
KineticLaw::KineticLaw (   const string& formula
                         , const string& timeUnits
                         , const string& substanceUnits ) :
   mFormula       ( formula        )
 , mMath          ( 0              )
 , mTimeUnits     ( timeUnits      )
 , mSubstanceUnits( substanceUnits )
 , mSBOTerm       ( -1             )
{
}


/**
 * Copies this KineticLaw.
 */
KineticLaw::KineticLaw (const KineticLaw& rhs) :
   SBase          ( rhs                 )
 , mFormula       ( rhs.mFormula        )
 , mMath          ( 0                   )
 , mTimeUnits     ( rhs.mTimeUnits      )
 , mSubstanceUnits( rhs.mSubstanceUnits )
 , mSBOTerm       ( rhs.mSBOTerm        )
{
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
}


/**
 * Destroys this KineticLaw.
 */
KineticLaw::~KineticLaw ()
{
  delete mMath;
}


/**
 * Accepts the given SBMLVisitor.
 */
bool
KineticLaw::accept (SBMLVisitor& v) const
{
  v.visit(*this);
  mParameters.accept(v);
  v.leave(*this);

  return true;
}


/**
 * @return a (deep) copy of this KineticLaw.
 */
SBase*
KineticLaw::clone () const
{
  return new KineticLaw(*this);
}


/**
 * @return the formula of this KineticLaw.
 */
const string&
KineticLaw::getFormula () const
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
 * @return the math of this KineticLaw.
 */
const ASTNode*
KineticLaw::getMath () const
{
  if (mMath == 0 && mFormula.empty() == false)
  {
    mMath = SBML_parseFormula( mFormula.c_str() );
  }

  return mMath;
}


/**
 * @return the list of Parameters for this KineticLaw.
 */
const ListOfParameters*
KineticLaw::getListOfParameters () const
{
  return &mParameters;
}


/**
 * @return the list of Parameters for this KineticLaw.
 */
ListOfParameters*
KineticLaw::getListOfParameters ()
{
  return &mParameters;
}


/**
 * @return the timeUnits of this KineticLaw.
 */
const string&
KineticLaw::getTimeUnits () const
{
  return mTimeUnits;
}


/**
 * @return the substanceUnits of this KineticLaw.
 */
const string&
KineticLaw::getSubstanceUnits () const
{
  return mSubstanceUnits;
}


/**
 * @return the sboTerm of this KineticLaw as an integer.  If not set,
 * sboTerm will be -1.  Use SBML::sboTermToString() to convert the
 * sboTerm to a zero-padded, seven digit string.
 */
int
KineticLaw::getSBOTerm () const
{
  return mSBOTerm;
}


/**
 * @return true if the formula (or equivalently the math) of this
 * KineticLaw has been set, false otherwise.
 */
bool
KineticLaw::isSetFormula () const
{
  return (mFormula.empty() == false) || (mMath != 0);
}


/**
 * @return true if the math (or equivalently the formula) of this
 * KineticLaw has been set, false otherwise.
 */
bool
KineticLaw::isSetMath () const
{
  return isSetFormula();
}


/**
 * @return true if the timeUnits of this KineticLaw has been set, false
 * otherwise.
 */
bool
KineticLaw::isSetTimeUnits () const
{
  return (mTimeUnits.empty() == false);
}


/**
 * @return true if the substanceUnits of this KineticLaw has been set,
 * false otherwise.
 */
bool
KineticLaw::isSetSubstanceUnits () const
{
  return (mSubstanceUnits.empty() == false);
}


/**
 * @return true if the sboTerm of this KineticLaw has been set, false
 * otherwise.
 */
bool
KineticLaw::isSetSBOTerm () const
{
  return (mSBOTerm != -1);
}


/**
 * Sets the formula of this KineticLaw to a copy of formula.
 */
void
KineticLaw::setFormula (const string& formula)
{
  mFormula = formula;

  if (mMath)
  {
    delete mMath;
    mMath = 0;
  }
}


/**
 * Sets the math of this KineticLaw to a copy of the given ASTNode.
 */
void
KineticLaw::setMath (const ASTNode* math)
{
  if (mMath == math) return;


  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;

  mFormula.erase();
}


/**
 * Sets the timeUnits of this KineticLaw to a copy of sid.
 */
void
KineticLaw::setTimeUnits (const string& sid)
{
  mTimeUnits = sid;
}


/**
 * Sets the substanceUnits of this KineticLaw to a copy of sid.
 */
void
KineticLaw::setSubstanceUnits (const string& sid)
{
  mSubstanceUnits = sid;
}


/**
 * Sets the sboTerm field of this KineticLaw to value.
 */
void
KineticLaw::setSBOTerm (int sboTerm)
{
  mSBOTerm = sboTerm;
}


/**
 * Adds a copy of the given Parameter to this KineticLaw.
 */
void
KineticLaw::addParameter (const Parameter* p)
{
  mParameters.append(p);
}


/**
 * Creates a new Parameter, adds it to this KineticLaw's list of
 * parameters and returns it.
 */
Parameter*
KineticLaw::createParameter ()
{
  Parameter* p = new Parameter();
  mParameters.appendAndOwn(p);

  return p;
}


/**
 * @return the nth Parameter of this KineticLaw.
 */
const Parameter*
KineticLaw::getParameter (unsigned int n) const
{
  return static_cast<const Parameter*>( mParameters.get(n) );
}


/**
 * @return the nth Parameter of this KineticLaw.
 */
Parameter*
KineticLaw::getParameter (unsigned int n)
{
  return static_cast<Parameter*>( mParameters.get(n) );
}


/**
 * @return the Parameter in this kineticLaw with the given id or NULL if
 * no such Parameter exists.
 */
const Parameter*
KineticLaw::getParameter (const string& sid) const
{
  return static_cast<const Parameter*>( mParameters.get(sid) );
}


/**
 * @return the Parameter in this kineticLaw with the given id or NULL if
 * no such Parameter exists.
 */
Parameter*
KineticLaw::getParameter (const string& sid)
{
  return static_cast<Parameter*>( mParameters.get(sid) );
}


/**
 * @return the number of Parameters in this KineticLaw.
 */
unsigned int
KineticLaw::getNumParameters () const
{
  return mParameters.size();
}


/**
 * Unsets the timeUnits of this KineticLaw.
 */
void
KineticLaw::unsetTimeUnits ()
{
  mTimeUnits.erase();
}


/**
 * Unsets the substanceUnits of this KineticLaw.
 */
void
KineticLaw::unsetSubstanceUnits ()
{
  mSubstanceUnits.erase();
}


/**
 * Unsets the sboTerm of this KineticLaw.
 */
void
KineticLaw::unsetSBOTerm ()
{
  mSBOTerm = -1;
}


/**
 * Sets the parent SBMLDocument of this SBML object.
 */
void
KineticLaw::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
  mParameters.setSBMLDocument(d);
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
KineticLaw::getTypeCode () const
{
  return SBML_KINETIC_LAW;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
KineticLaw::getElementName () const
{
  static const string name = "kineticLaw";
  return name;
}


/**
  * returns expected position of Kinetic Law in the reaction
  */
int
KineticLaw::getElementPosition(unsigned int reactionNo) const
{
  int position = 1;
  /**
   * the expected position of each element depends on the type
   * and any lists ahead
   */

  const Reaction * r = this->getSBMLDocument()->getModel()->getReaction(reactionNo-1);

  if (r->getNumReactants() != 0)
    position++;
  
  if (r->getNumProducts() != 0)
    position++;

  if (r->getNumModifiers() != 0)
    position++;

  return position;

}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
KineticLaw::createObject (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();
  return (name == "listOfParameters") ? &mParameters : 0;
}


/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
KineticLaw::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();


  if (name == "math")
  {
    delete mMath;
    mMath = readMathML(stream);
    read  = true;
  }

  return read;
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
KineticLaw::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  if (level == 1) attributes.readInto("formula", mFormula);

  //
  // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("timeUnits", mTimeUnits);

  //
  // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("substanceUnits", mSubstanceUnits);

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
KineticLaw::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  if (getLevel() == 1) stream.writeAttribute("formula", getFormula());

  //
  // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  stream.writeAttribute("timeUnits", mTimeUnits);

  //
  // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  //
  stream.writeAttribute("substanceUnits", mSubstanceUnits);

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
KineticLaw::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if ( getLevel() == 2 && isSetMath() ) writeMathML(getMath(), stream);
  if ( getNumParameters() > 0 ) mParameters.write(stream);
}





/**
 * Creates a new KineticLaw and returns a pointer to it.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_create (void)
{
  return new(nothrow) KineticLaw;
}


/**
 * Creates a new KineticLaw with the given formula, timeUnits and
 * substanceUnits and returns a pointer to it.  This convenience function
 * is functionally equivalent to:
 *
 *   KineticLaw_t *kl = KineticLaw_create();
 *   KineticLaw_setFormula(kl, formula);
 *   KineticLaw_setTimeUnits(kl, timeUnits);
 *   ...;
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWith ( const char *formula,
                        const char *timeUnits,
                        const char *substanceUnits )
{
  string f  = formula        ? formula        : "";
  string tu = timeUnits      ? timeUnits      : "";
  string su = substanceUnits ? substanceUnits : "";

  return new(nothrow) KineticLaw(f, tu, su);
}


/**
 * Frees the given KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_free (KineticLaw_t *kl)
{
  delete kl;
}


/**
 * @return a (deep) copy of this KineticLaw.
 */
LIBSBML_EXTERN
SBase_t *
KineticLaw_clone (const KineticLaw_t *kl)
{
  return kl->clone();
}


/**
 * @return the formula of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getFormula (const KineticLaw_t *kl)
{
  return kl->isSetFormula() ? kl->getFormula().c_str() : NULL;
}


/**
 * @return the math of this KineticLaw.
 */
LIBSBML_EXTERN
const ASTNode_t *
KineticLaw_getMath (const KineticLaw_t *kl)
{
  return kl->getMath();
}


/**
 * @return the list of Parameters for this KineticLaw.
 */
LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfParameters (KineticLaw_t *kl)
{
  return kl->getListOfParameters();
}


/**
 * @return the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getTimeUnits (const KineticLaw_t *kl)
{
  return kl->isSetTimeUnits() ? kl->getTimeUnits().c_str() : NULL;
}


/**
 * @return the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getSubstanceUnits (const KineticLaw_t *kl)
{
  return kl->isSetSubstanceUnits() ? kl->getSubstanceUnits().c_str() : NULL;
}


/**
 * @return the sboTerm of this KineticLaw as an integer.  If not set,
 * sboTerm will be -1.  Use SBML_sboTermToString() to convert the sboTerm
 * to a zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
KineticLaw_getSBOTerm (const KineticLaw_t *kl)
{
  return kl->getSBOTerm();
}


/**
 * @return true (non-zero) if the formula (or equivalently the math) of
 * this KineticLaw has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetFormula (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetFormula() );
}


/**
 * @return true (non-zero) if the math (or equivalently the formula) of
 * this KineticLaw has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetMath (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetMath() );
}


/**
 * @return true (non-zero) if the timeUnits of this KineticLaw has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetTimeUnits (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetTimeUnits() );
}


/**
 * @return true (non-zero) if the substanceUnits of this KineticLaw has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSubstanceUnits (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetSubstanceUnits() );
}


/**
 * @return true (non-zero) if the substanceUnits of this KineticLaw has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSBOTerm (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetSBOTerm() );
}


/**
 * Sets the formula of this KineticLaw to a copy of formula.
 */
LIBSBML_EXTERN
void
KineticLaw_setFormula (KineticLaw_t *kl, const char *formula)
{
  kl->setFormula(formula ? formula : "");
}


/**
 * Sets the math of this KineticLaw to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
KineticLaw_setMath (KineticLaw_t *kl, const ASTNode_t *math)
{
  kl->setMath(math);
}


/**
 * Sets the timeUnits of this KineticLaw to a copy of sid.
 */
LIBSBML_EXTERN
void
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sid)
{
  (sid == NULL) ? kl->unsetTimeUnits() : kl->setTimeUnits(sid);
}


/**
 * Sets the substanceUnits of this KineticLaw to a copy of sid.
 */
LIBSBML_EXTERN
void
KineticLaw_setSubstanceUnits (KineticLaw_t *kl, const char *sid)
{
  (sid == NULL) ? kl->unsetSubstanceUnits() : kl->setSubstanceUnits(sid);
}


/**
 * Sets the sboTerm field of this KineticLaw to value.
 */
LIBSBML_EXTERN
void
KineticLaw_setSBOTerm (KineticLaw_t *kl, int sboTerm)
{
  kl->setSBOTerm(sboTerm);
}


/**
 * Adds a copy of the given Parameter to this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_addParameter (KineticLaw_t *kl, const Parameter_t *p)
{
  if (p != NULL) kl->addParameter(p);
}


/**
 * Creates a new Parameter, adds it to this KineticLaw's list of
 * parameters and returns it.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_createParameter (KineticLaw_t *kl)
{
  return kl->createParameter();
}


/**
 * @return the nth Parameter of this KineticLaw.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (KineticLaw_t *kl, unsigned int n)
{
  return kl->getParameter(n);
}


/**
 * @return the Parameter in this KineticLaw with the given id or NULL if no
 * such Parameter exists.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameterById (KineticLaw_t *kl, const char *sid)
{
  return (sid != NULL) ? kl->getParameter(sid) : 0;
}


/**
 * @return the number of Parameters in this KineticLaw.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl)
{
  return kl->getNumParameters();
}


/**
 * Unsets the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetTimeUnits (KineticLaw_t *kl)
{
  kl->unsetTimeUnits();
}


/**
 * Unsets the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetSubstanceUnits (KineticLaw_t *kl)
{
  kl->unsetSubstanceUnits();
}


/**
 * Unsets the sboTerm of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetSBOTerm (KineticLaw_t *kl)
{
  kl->unsetSBOTerm();
}
