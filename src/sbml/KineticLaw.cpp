/**
 * @file    KineticLaw.cpp
 * @brief   Implementation of KineticLaw.
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
#include <sbml/KineticLaw.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new KineticLaw, optionally with its formula, timeUnits and/or
 * substanceUnits set.
 */
KineticLaw::KineticLaw (   const std::string& formula
                         , const std::string& timeUnits
                         , const std::string& substanceUnits ) :
   SBase          ( -1             )
 , mFormula       ( formula        )
 , mMath          ( 0              )
 , mTimeUnits     ( timeUnits      )
 , mSubstanceUnits( substanceUnits )
{
}


/*
 * Creates a new KineticLaw, optionally with its math, timeUnits and/or
 * substanceUnits set.
 */
KineticLaw::KineticLaw (   const ASTNode* math
                         , const std::string& timeUnits
                         , const std::string& substanceUnits ) :
   SBase          ( -1             )
 , mMath          ( 0              )
 , mTimeUnits     ( timeUnits      )
 , mSubstanceUnits( substanceUnits )
{
  if (math) mMath = math->deepCopy();
}


/*
 * Destroys this KineticLaw.
 */
KineticLaw::~KineticLaw ()
{
  delete mMath;
}


/*
 * Copy constructor. Creates a copy of this KineticLaw.
 */
KineticLaw::KineticLaw (const KineticLaw& orig) :
   SBase          ( orig                 )
 , mFormula       ( orig.mFormula        )
 , mMath          ( 0                    )
 , mParameters    ( orig.mParameters     )
 , mTimeUnits     ( orig.mTimeUnits      )
 , mSubstanceUnits( orig.mSubstanceUnits )
{
  if (orig.mMath) mMath = orig.mMath->deepCopy();
}


/*
 * Assignment operator
 */
KineticLaw& KineticLaw::operator=(const KineticLaw& rhs)
{
  this->SBase::operator =(rhs);
  mFormula        = rhs.mFormula        ;
  mTimeUnits      = rhs.mTimeUnits      ;
  mSubstanceUnits = rhs.mSubstanceUnits ;
  mParameters     = rhs.mParameters     ;
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
  return *this;
}


/*
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


/*
 * @return a (deep) copy of this KineticLaw.
 */
SBase*
KineticLaw::clone () const
{
  return new KineticLaw(*this);
}


/*
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


/*
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


/*
 * @return the timeUnits of this KineticLaw.
 */
const string&
KineticLaw::getTimeUnits () const
{
  return mTimeUnits;
}


/*
 * @return the substanceUnits of this KineticLaw.
 */
const string&
KineticLaw::getSubstanceUnits () const
{
  return mSubstanceUnits;
}


/*
 * @return true if the formula (or equivalently the math) of this
 * KineticLaw has been set, false otherwise.
 */
bool
KineticLaw::isSetFormula () const
{
  return (mFormula.empty() == false) || (mMath != 0);
}


/*
 * @return true if the math (or equivalently the formula) of this
 * KineticLaw has been set, false otherwise.
 */
bool
KineticLaw::isSetMath () const
{
  return isSetFormula();
}


/*
 * @return true if the timeUnits of this KineticLaw has been set, false
 * otherwise.
 */
bool
KineticLaw::isSetTimeUnits () const
{
  return (mTimeUnits.empty() == false);
}


/*
 * @return true if the substanceUnits of this KineticLaw has been set,
 * false otherwise.
 */
bool
KineticLaw::isSetSubstanceUnits () const
{
  return (mSubstanceUnits.empty() == false);
}


/*
 * Sets the formula of this KineticLaw to a copy of formula.
 */
void
KineticLaw::setFormula (const std::string& formula)
{
  mFormula = formula;

  if (mMath)
  {
    delete mMath;
    mMath = 0;
  }
}


/*
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


/*
 * Sets the timeUnits of this KineticLaw to a copy of sid.
 */
void
KineticLaw::setTimeUnits (const std::string& sid)
{
  mTimeUnits = sid;
}


/*
 * Sets the substanceUnits of this KineticLaw to a copy of sid.
 */
void
KineticLaw::setSubstanceUnits (const std::string& sid)
{
  mSubstanceUnits = sid;
}


/*
 * Unsets the timeUnits of this KineticLaw.
 */
void
KineticLaw::unsetTimeUnits ()
{
  mTimeUnits.erase();
}


/*
 * Unsets the substanceUnits of this KineticLaw.
 */
void
KineticLaw::unsetSubstanceUnits ()
{
  mSubstanceUnits.erase();
}


/*
 * Adds a copy of the given Parameter to this KineticLaw.
 */
void
KineticLaw::addParameter (const Parameter* p)
{
  mParameters.append(p);
}


/*
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


/*
 * @return the list of Parameters for this KineticLaw.
 */
const ListOfParameters*
KineticLaw::getListOfParameters () const
{
  return &mParameters;
}


/*
 * @return the list of Parameters for this KineticLaw.
 */
ListOfParameters*
KineticLaw::getListOfParameters ()
{
  return &mParameters;
}


/*
 * @return the nth Parameter of this KineticLaw.
 */
const Parameter*
KineticLaw::getParameter (unsigned int n) const
{
  return static_cast<const Parameter*>( mParameters.get(n) );
}


/*
 * @return the nth Parameter of this KineticLaw.
 */
Parameter*
KineticLaw::getParameter (unsigned int n)
{
  return static_cast<Parameter*>( mParameters.get(n) );
}


/*
 * @return the Parameter in this kineticLaw with the given id or NULL if
 * no such Parameter exists.
 */
const Parameter*
KineticLaw::getParameter (const std::string& sid) const
{
  return static_cast<const Parameter*>( mParameters.get(sid) );
}


/*
 * @return the Parameter in this kineticLaw with the given id or NULL if
 * no such Parameter exists.
 */
Parameter*
KineticLaw::getParameter (const std::string& sid)
{
  return static_cast<Parameter*>( mParameters.get(sid) );
}


/*
 * @return the number of Parameters in this KineticLaw.
 */
unsigned int
KineticLaw::getNumParameters () const
{
  return mParameters.size();
}

/*
  * Calculates and returns a UnitDefinition that expresses the units
  * returned by the math expression of this KineticLaw.
  */
UnitDefinition * 
KineticLaw::getCalculatedUnitDefinition()
{
  if (!getSBMLDocument()->getModel()->isPopulatedListFormulaUnitsData())
  {
    getSBMLDocument()->getModel()->populateListFormulaUnitsData();
  }

  return getSBMLDocument()->getModel()
    ->getFormulaUnitsData(getId(), getTypeCode())
    ->getUnitDefinition();
}

/*
 * Predicate returning @c true or @c false depending on whether 
 * the math expression of this KineticLaw contains
 * parameters/numbers with undeclared units that cannot be ignored.
 */
bool 
KineticLaw::containsUndeclaredUnits()
{
  if (!getSBMLDocument()->getModel()->isPopulatedListFormulaUnitsData())
  {
    getSBMLDocument()->getModel()->populateListFormulaUnitsData();
  }

  return (getSBMLDocument()->getModel()
    ->getFormulaUnitsData(getId(), getTypeCode())
    ->getContainsUndeclaredUnits());
}



/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
KineticLaw::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
  mParameters.setSBMLDocument(d);
}


/*
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


/*
 * @return the name of this element ie "kineticLaw".
 */
const string&
KineticLaw::getElementName () const
{
  static const string name = "kineticLaw";
  return name;
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
KineticLaw::getElementPosition () const
{
  return 4;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
KineticLaw::createObject (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();

  if (name == "listOfParameters")
  {
    if (mParameters.size() != 0)
    {
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <listOfParameters> elements is permitted "
	       "in a given <kineticLaw> element.");
    }
    return &mParameters;
  }
  
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
KineticLaw::readOtherXML (XMLInputStream& stream)
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

    if (getNumParameters() > 0) logError(IncorrectOrderInKineticLaw);

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
KineticLaw::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  if (level == 1)
  {
    expectedAttributes.push_back("formula");
    expectedAttributes.push_back("timeUnits");
    expectedAttributes.push_back("substanceUnits");
  }
  else
  {
    expectedAttributes.push_back("metaid");

    if (version == 1)
    {
      expectedAttributes.push_back("timeUnits");
      expectedAttributes.push_back("substanceUnits");
    }

    if (version != 1)
    {
      expectedAttributes.push_back("sboTerm");
    }
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      logUnknownAttribute(name, level, version, "<kineticLaw>");
    }
  }

  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  if (level == 1) attributes.readInto("formula", mFormula, getErrorLog(), true);

  
  //
  // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  // removed in l2v3
  //
  attributes.readInto("timeUnits", mTimeUnits);

  //
  // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1, L2v2)
  // removed in l2v3
  //
  attributes.readInto("substanceUnits", mSubstanceUnits);

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
KineticLaw::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // formula: string  { use="required" }  (L1v1, L1v2)
  //
  if (level == 1) stream.writeAttribute("formula", getFormula());

  if (level == 1  || version == 1)
  {
    //
    // timeUnits  { use="optional" }  (L1v1, L1v2, L2v1)
    // removed in l2v2
    //
    stream.writeAttribute("timeUnits", mTimeUnits);

    //
    // substanceUnits  { use="optional" }  (L1v1, L1v2, L2v1)
    // removed in l2v2
    //
    stream.writeAttribute("substanceUnits", mSubstanceUnits);
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */



/**
 * Creates a new, empty KineticLaw_t structure and returns a pointer to it.
 *
 * Note that in SBML Level 2, if a KineticLaw_t structure is present in a
 * Reaction_t object, the KineticLaw_t structure must contain a non-empty
 * "math" subelement.  Although this method allows the creation of empty
 * KineticLaw_t structures, callers should make sure to assign a
 * mathematical expression for the rate using (for example)
 * KineticLaw_setMath().
 *
 * @return pointer to newly created KineticLaw_t structure.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_create (void)
{
  return new(nothrow) KineticLaw;
}


/**
 * Creates a new KineticLaw_t structure with the given formula and returns a pointer
 * to it.
 *
 * See the description of Reaction for important information about the
 * interpretation of reaction rate expressions, and on particular about the
 * units of the expressions.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   KineticLaw_t *kl = KineticLaw_create();
 *   KineticLaw_setFormula(kl, formula);
 * @endcode
 *
 * @param formula a mathematical expression in text-string form
 * representing the rate of the reaction.
 * 
 * @param timeUnits the identifier of the units of time for this
 * KineticLaw_t structure.
 * 
 * @param substanceUnits the identifier of the units of substance for this
 * KineticLaw_t structure.
 *
 * @return pointer to newly created KineticLaw_t structure.
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this constructor) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  See KineticLaw_createWithMath for a
 * version that takes an ASTNode_t structure.
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWithFormula ( const char *formula)
{
  string f  = formula        ? formula        : "";

  return new(nothrow) KineticLaw(f);
}


/**
 * Creates a new KineticLaw_t structure with the given mathematical
 * expression and returns a pointer to it.
 *
 * See the description of Reaction for important information about the
 * interpretation of reaction rate expressions, and on particular about the
 * units of the expressions.
 *
 * This convenience function is functionally equivalent to:
 * @code
 *   KineticLaw_t *kl = KineticLaw_create();
 *   KineticLaw_setMath(kl, math);
 * @endcode
 *
 * @param math an ASTNode structure representing a mathematical expression
 * for the rate of the reaction.
 *
 * @return pointer to newly created KineticLaw_t structure.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWithMath (ASTNode_t *math)
{
  return new(nothrow) KineticLaw(math);
}


/**
 * Frees the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure
 */
LIBSBML_EXTERN
void
KineticLaw_free (KineticLaw_t *kl)
{
  delete kl;
}


/**
 * Returns a deep copy of the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return a (deep) copy of this KineticLaw_t structure.
 */
LIBSBML_EXTERN
SBase_t *
KineticLaw_clone (const KineticLaw_t *kl)
{
  return kl->clone();
}


/**
 * Gets the mathematical expression of this KineticLaw_t structure as a
 * formula in text-string form.
 *
 * This is fundamentally equivalent to KineticLaw_getMath().  It is
 * provided principally for compatibility with SBML Level 1.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the formula of this KineticLaw_t structure.
 *
 * @see KineticLaw_getMath().
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this constructor) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  See KineticLaw_createWithMath for a
 * version that takes an ASTNode_t structure.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getFormula (const KineticLaw_t *kl)
{
  return kl->isSetFormula() ? kl->getFormula().c_str() : NULL;
}


/**
 * Gets the mathematical expression of this KineticLaw_t structure as an
 * ASTNode_t structure.
 *
 * This is fundamentally equivalent to KineticLaw_getFormula().  The latter
 * is provided principally for compatibility with SBML Level 1, which
 * represented mathematical formulas in text-string form.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the formula in the form of an ASTNode_t structure
 *
 * @see KineticLaw_getFormula().
 */
LIBSBML_EXTERN
const ASTNode_t *
KineticLaw_getMath (const KineticLaw_t *kl)
{
  return kl->getMath();
}


/**
 * Gets the value of the "timeUnits" attribute of the given
 * KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the "timeUnits" attribute value
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getTimeUnits (const KineticLaw_t *kl)
{
  return kl->isSetTimeUnits() ? kl->getTimeUnits().c_str() : NULL;
}


/**
 * Gets the value of the "substanceUnits" attribute of the given
 * KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the "substanceUnits" attribute value
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getSubstanceUnits (const KineticLaw_t *kl)
{
  return kl->isSetSubstanceUnits() ? kl->getSubstanceUnits().c_str() : NULL;
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "formula" attribute of the given KineticLaw_t structure is
 * set.
 *
 * This is fundamentally equivalent to KineticLaw_isSetMath().  It is
 * provided principally for compatibility with SBML Level 1.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return nonzero (meaning true) if the formula (or equivalently the
 * "math" subelement) of the given KineticLaw_t structure has been set,
 * zero (meaning false) otherwise.
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this constructor) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  See KineticLaw_createWithMath for a
 * version that takes an ASTNode_t structure.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetFormula (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetFormula() );
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "math" subelement of the given KineticLaw_t structure is
 * set.
 *
 * This is fundamentally equivalent to KineticLaw_isSetFormula().  The
 * latter provided principally for compatibility with SBML Level 1, which
 * represented mathematical formulas in text-string form.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return nonzero (meaning true) if the "math" subelement of the given
 * KineticLaw_t structure has been set, zero (meaning false) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetMath (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetMath() );
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "timeUnits" attribute of the given KineticLaw_t structure is
 * set.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return nonzero (meaning true) if the "timeUnits" attribute of the given
 * KineticLaw_t structure has been set, zero (meaning false) otherwise.
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetTimeUnits (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetTimeUnits() );
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "timeUnits" attribute of the given KineticLaw_t structure is
 * set.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return nonzero (meaning true) if the "timeUnits" attribute of the given
 * KineticLaw_t structure has been set, zero (meaning false) otherwise.
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSubstanceUnits (const KineticLaw_t *kl)
{
  return static_cast<int>( kl->isSetSubstanceUnits() );
}


/**
 * Sets the formula of the given KineticLaw_t structure.
 *
 * This is fundamentally equivalent to KineticLaw_setMath().  It is
 * provided principally for compatibility with SBML Level 1.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param formula the mathematical expression, in text-string form.
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this constructor) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  See KineticLaw_createWithMath for a
 * version that takes an ASTNode_t structure.
 */
LIBSBML_EXTERN
void
KineticLaw_setFormula (KineticLaw_t *kl, const char *formula)
{
  kl->setFormula(formula ? formula : "");
}


/**
 * Sets the formula of the given KineticLaw_t structure.
 *
 * This is fundamentally equivalent to KineticLaw_setFormula().  The latter
 * provided principally for compatibility with SBML Level 1, which
 * represented mathematical formulas in text-string form.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param math an ASTNode_t structure representing the mathematical formula
 */
LIBSBML_EXTERN
void
KineticLaw_setMath (KineticLaw_t *kl, const ASTNode_t *math)
{
  kl->setMath(math);
}


/**
 * Sets the "timeUnits" attribute of the given KineticLaw_t structure.
 *
 * The identifier string @p sid is copied.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param sid the identifier of the units
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
void
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sid)
{
  (sid == NULL) ? kl->unsetTimeUnits() : kl->setTimeUnits(sid);
}


/**
 * Sets the "substanceUnits" attribute of the given KineticLaw_t structure.
 *
 * The identifier string @p sid is copied.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param sid the identifier of the units
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
void
KineticLaw_setSubstanceUnits (KineticLaw_t *kl, const char *sid)
{
  (sid == NULL) ? kl->unsetSubstanceUnits() : kl->setSubstanceUnits(sid);
}


/**
 * Unsets the "timeUnits" attribute of the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetTimeUnits (KineticLaw_t *kl)
{
  kl->unsetTimeUnits();
}


/**
 * Unsets the "substanceUnits" attribute of the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @warning In SBML Level 2 Version 2, the "timeUnits" and "substanceUnits"
 * attributes were removed.  For compatibility with new versions of SBML,
 * users are cautioned to avoid these attributes.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetSubstanceUnits (KineticLaw_t *kl)
{
  kl->unsetSubstanceUnits();
}


/**
 * Adds a copy of the given Parameter_t structure to the list of local
 * parameters in the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param p a pointer to a Parameter_t structure
 */
LIBSBML_EXTERN
void
KineticLaw_addParameter (KineticLaw_t *kl, const Parameter_t *p)
{
  if (p != NULL) kl->addParameter(p);
}


/**
 * Creates a new Parameter_t structure, adds it to the given KineticLaw_t
 * structures's list of local parameters, and returns a pointer to the
 * Parameter_t created.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @return a pointer to a Parameter_t structure
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_createParameter (KineticLaw_t *kl)
{
  return kl->createParameter();
}


/**
 * Get the list of local parameters defined for the given KineticLaw_t
 * structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return a list of Parameters
 */
LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfParameters (KineticLaw_t *kl)
{
  return kl->getListOfParameters();
}


/**
 * Get the nth parameter in the list of local parameters in the
 * given KineticLaw_t structure.
 *
 * Callers should first find out how many parameters are in the list by
 * calling KineticLaw_getNumParameters().
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param n the index of the Parameter_t structure sought
 * 
 * @return a pointer to the Parameter_t structure
 *
 * @see KineticLaw_getNumParameters().
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (KineticLaw_t *kl, unsigned int n)
{
  return kl->getParameter(n);
}


/**
 * Get a parameter with identifier "id" out of the list of local
 * parameters defined for the given KineticLaw_t structure.
 *
 * @param kl the KineticLaw_t structure.
 *
 * @param sid the identifier of the Parameter_t structure sought
 * 
 * @return the Parameter_t structure with the given id, or NULL if no such
 * Parameter_t exists in the given KineticLaw_t structure.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameterById (KineticLaw_t *kl, const char *sid)
{
  return (sid != NULL) ? kl->getParameter(sid) : 0;
}


/**
 * Get the number of local parameters defined in the given KineticLaw_t
 * structure.
 *
 * @param kl the KineticLaw_t structure.
 * 
 * @return the number of Parameter_t structures in the given KineticLaw_t
 * structure.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl)
{
  return kl->getNumParameters();
}

/**
  * Calculates and returns a UnitDefinition_t that expresses the units
  * returned by the math expression of this KineticLaw_t.
  *
  * @return a UnitDefinition_t that expresses the units of the math 
  * expression of this KineticLaw_t.
  *
  * @note The units are calculated by applying the mathematics 
  * from the expression to the units of the <ci> elements used 
  * within the expression. Where there are parameters/numbers
  * with undeclared units the UnitDefinition_t returned by this
  * function may not accurately represent the units of the expression.
  * 
  * @see KineticLaw_containsUndeclaredUnits()
  */
LIBSBML_EXTERN
UnitDefinition_t * 
KineticLaw_getCalculatedUnitDefinition(KineticLaw_t *kl)
{
  return kl->getCalculatedUnitDefinition();
}


/**
  * Predicate returning @c true or @c false depending on whether 
  * the math expression of this KineticLaw_t contains
  * parameters/numbers with undeclared units.
  * 
  * @return @c true if the math expression of this KineticLaw_t
  * includes parameters/numbers 
  * with undeclared units, @c false otherwise.
  *
  * @note a return value of @c true indicates that the UnitDefinition_t
  * returned by the getCalculatedUnitDefinition function may not 
  * accurately represent the units of the expression.
  *
  * @see KineticLaw_getCalculatedUnitDefinition()
  */
LIBSBML_EXTERN
int 
KineticLaw_containsUndeclaredUnits(KineticLaw_t *kl)
{
  return static_cast<int>(kl->containsUndeclaredUnits());
}


/** @endcond doxygen-c-only */
