/**
 * \file    Rule.cpp
 * \brief   SBML Rule
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


#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLNamespaces.h>

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include "SBML.h"
#include "SBMLTypeCodes.h"
#include "SBMLVisitor.h"
#include "SBMLDocument.h"
#include "Model.h"
#include "Rule.h"


using namespace std;


/**
 * Only subclasses may create Rules.
 */
Rule::Rule (SBMLTypeCode_t type, const string& variable, const string& formula)
 :
   SBase   ( variable )
 , mFormula( formula  )
 , mMath   (  0       )
 , mSBOTerm( -1       )
 , mType   ( type     )
 , mL1Type ( SBML_UNKNOWN )
{
}


/**
 * Only subclasses may create Rules.
 */
Rule::Rule (SBMLTypeCode_t type, const string& variable, const ASTNode* math)
 :
   SBase   ( variable         )
 , mMath   ( 0                )
 , mSBOTerm( -1               )
 , mType   ( type             )
 , mL1Type ( SBML_UNKNOWN     )
{
  if (math) mMath = math->deepCopy();
}


/**
 * Copies this Rule.
 */
Rule::Rule (const Rule& rhs) :
   SBase   ( rhs          )
 , mFormula( rhs.mFormula )
 , mMath   ( 0            )
 , mSBOTerm( rhs.mSBOTerm )
 , mUnits  ( rhs.mUnits   )
 , mType   ( rhs.mType    )
 , mL1Type ( rhs.mL1Type  )
{
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
}


/**
 * Destroys this Rule.
 */
Rule::~Rule ()
{
  delete mMath;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next Rule
 * (if available).
 */
bool
Rule::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this Rule.
 */
SBase*
Rule::clone () const
{
  return new Rule(*this);
}


/**
 * @return the formula for this Rule.
 */
const string&
Rule::getFormula () const
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
 * @return the math for this Rule.
 */
const ASTNode*
Rule::getMath () const
{
  if (mMath == 0 && mFormula.empty() == false)
  {
    mMath = SBML_parseFormula( mFormula.c_str() );
  }

  return mMath;
}


/**
 * @return the sboTerm of this Rule as an integer.  If not set, sboTerm
 * will be -1.  Use SBML::sboTermToString() to convert the sboTerm to a
 * zero-padded, seven digit string.
 */
int
Rule::getSBOTerm () const
{
  return mSBOTerm;
}


/**
 * @return the type of this Rule, either RULE_TYPE_RATE or
 * RULE_TYPE_SCALAR.
 */
RuleType_t
Rule::getType () const
{
  if (mType == SBML_ASSIGNMENT_RULE) return RULE_TYPE_SCALAR;
  if (mType == SBML_RATE_RULE)       return RULE_TYPE_RATE;
  return RULE_TYPE_INVALID;
}


/**
 * @return the variable for this Rule.
 */
const string&
Rule::getVariable () const
{
  return getId();
}


/**
 * @return the units for this Rule (L1 ParameterRules only).
 */
const string&
Rule::getUnits () const
{
  return mUnits;
}


/**
 * @return true if the formula (or equivalently the math) for this Rule has
 * been set, false otherwise.
 */
bool
Rule::isSetFormula () const
{
  return (mFormula.empty() == false) || (mMath != 0);
}


/**
 * @return true if the math (or equivalently the formula) for this Rule has
 * been set, false otherwise.
 */
bool
Rule::isSetMath () const
{
  return isSetFormula();
}


/**
 * @return true if the sboTerm of this Rule has been set, false
 * otherwise.
 */
bool
Rule::isSetSBOTerm () const
{
  return (mSBOTerm != -1);
}


/**
 * @return true if the variable of this Rule has been set, false
 * otherwise.
 */
bool
Rule::isSetVariable () const
{
  return isSetId();
}


/**
 * @return true if the units for this Rule has been set, false otherwise
 * (L1 ParameterRules only).
 */
bool
Rule::isSetUnits () const
{
  return (mUnits.empty() == false);
}


/**
 * Sets the formula of this Rule to a copy of string.
 */
void
Rule::setFormula (const string& formula)
{
  mFormula = formula;

  if (mMath)
  {
    delete mMath;
    mMath = 0;
  }
}


/**
 * Sets the math of this Rule to a copy of the given ASTNode.
 */
void
Rule::setMath (const ASTNode* math)
{
  if (mMath == math) return;


  delete mMath;
  mMath = (math != 0) ? math->deepCopy() : 0;

  mFormula.erase();
}


/**
 * Sets the sboTerm field of this Rule to value.
 */
void
Rule::setSBOTerm (int sboTerm)
{
  mSBOTerm = sboTerm;
}


/**
 * Sets the variable of this RateRule to a copy of sid.
 */
void
Rule::setVariable (const string& sid)
{
  setId(sid);
}


/**
 * Sets the units for this Rule to a copy of sname (L1 ParameterRules
 * only).
 */
void
Rule::setUnits (const string& sname)
{
  mUnits = sname;
}


/**
 * Unsets the sboTerm of this Rule.
 */
void
Rule::unsetSBOTerm ()
{
  mSBOTerm = -1;
}


/**
 * Unsets the units for this Rule (L1 ParameterRules only).
 */
void
Rule::unsetUnits ()
{
  mUnits.erase();
}


/**
 * @return true if this Rule is an AlgebraicRule, false otherwise.
 */
bool
Rule::isAlgebraic () const
{
  return (mType == SBML_ALGEBRAIC_RULE);
}


/**
 * @return true if this Rule is an AssignmentRule, false otherwise.
 */
bool
Rule::isAssignment () const
{
  return (mType == SBML_ASSIGNMENT_RULE);
}


/**
 * @return true if this Rule is a CompartmentVolumeRule, false otherwise.
 */
bool
Rule::isCompartmentVolume () const
{
  if (mL1Type == SBML_COMPARTMENT_VOLUME_RULE)
  {
    return true;
  }
  else
  {
    const Model* model = getModel();
    return (model == 0) ? false : model->getCompartment( getVariable() ) != 0;
  }
}


/**
 * @return true if this Rule is a ParameterRule, false otherwise.
 */
bool
Rule::isParameter () const
{
  if (mL1Type == SBML_PARAMETER_RULE)
  {
    return true;
  }
  else
  {
    const Model* model = getModel();
    return (model == 0) ? false : model->getParameter( getVariable() ) != 0;
  }
}


/**
 * @return true if this Rule is a RateRule (L2) or has type="rate" (L1),
 * false otherwise.
 */
bool
Rule::isRate () const
{
  return (mType == SBML_RATE_RULE);
}


/**
 * @return true if this Rule is an AssignmentRule (L2) has type="scalar"
 * (L1), false otherwise.
 */
bool
Rule::isScalar () const
{
  return (mType == SBML_ASSIGNMENT_RULE);
}


/**
 * @return true if this Rule is a SpeciesConcentrationRule, false
 * otherwise.
 */
bool
Rule::isSpeciesConcentration () const
{
  if (mL1Type == SBML_SPECIES_CONCENTRATION_RULE)
  {
    return true;
  }
  else
  {
    const Model* model = getModel();
    return (model == 0) ? false : model->getSpecies( getVariable() ) != 0;
  }
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Rule::getTypeCode () const
{
  return mType;
}


/**
 * @return the SBML Level 1 typecode for this Rule or SBML_UNKNOWN
 * (default).
 */
SBMLTypeCode_t
Rule::getL1TypeCode () const
{
  return mL1Type;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
Rule::getElementName () const
{
  static const string algebraic   = "algebraicRule";
  static const string specie      = "specieConcentrationRule";
  static const string species     = "speciesConcentrationRule";
  static const string compartment = "compartmentVolumeRule";
  static const string parameter   = "parameterRule";
  static const string assignment  = "assignmentRule";
  static const string rate        = "rateRule";
  static const string unknown     = "unknownRule";

  if ( isAlgebraic() )
  {
    return algebraic;
  }
  else if (getLevel() == 1)
  {
    if ( isSpeciesConcentration() )
    {
      return (getVersion() == 2) ? species : specie;
    }
    else if ( isCompartmentVolume() )
    {
      return compartment;
    }
    else if ( isParameter() )
    {
      return parameter;
    }
  }
  else
  {
    if ( isAssignment() )
    {
      return assignment;
    }
    else if ( isRate() )
    {
      return rate;
    }
  }

  return unknown;
}


/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
Rule::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();


  if (name == "math")
  {
    delete mMath;

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
Rule::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();


  if (level == 1)
  {
    //
    // formula: string  { use="required" }  (L1v1, L1v2)
    //
    attributes.readInto("formula", mFormula);

    //
    // type { use="optional" default="scalar" }  (L1v1, L1v2)
    //
    // This attribute is handled by ListOfRules::createObject();
    //

    if ( isSpeciesConcentration() )
    {
      //
      // specie : SName   { use="required" }  (L1v1)
      // species: SName   { use="required" }  (L1v2)
      //
      const string s = (level == 1 && version == 1) ? "specie" : "species";
      attributes.readInto(s , mId);
    }
    else if ( isCompartmentVolume() )
    {
      //
      // compartment: SName  { use="required" }  (L1v1, L1v2)
      //
      attributes.readInto("compartment", mId);
    }
    else if ( isParameter() )
    {
      //
      // name: SName  { use="required" } (L1v1, L1v2)
      //
      attributes.readInto("name", mId);

      //
      // units  { use="optional" }  (L1v1, L1v2);
      //
      attributes.readInto("units", mUnits);

    }
  }
  else if (level == 2)
  {
    //
    // variable: SId  { use="required" }  (L2v1, L2v2)
    //
    attributes.readInto("variable", mId);

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2)
    //
    if ((version == 2 || version == 3)) 
      mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());
  }
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Rule::writeAttributes (XMLOutputStream& stream) const
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();


  if (level == 1)
  {
    //
    // formula: string  { use="required" }  (L1v1, L1v2)
    //
    stream.writeAttribute("formula", getFormula());

    //
    // type { use="optional" default="scalar" }  (L1v1, L1v2)
    //
    if (getType() == RULE_TYPE_RATE)
    {
      const string rate = "rate";
      stream.writeAttribute("type", rate);
    }

    //
    // specie : SName   { use="required" }  (L1v1)
    // species: SName   { use="required" }  (L1v2)
    //
    if ( isSpeciesConcentration() )
    {
      const string species = (version == 1) ? "specie" : "species";
      stream.writeAttribute(species, mId);
    }

    //
    // compartment: SName  { use="required" }  (L1v1, L1v2)
    //
    else if ( isCompartmentVolume() )
    {
      stream.writeAttribute("compartment", mId);
    }

    else if ( isParameter() )
    {
      //
      // name: SName  { use="required" } (L1v1, L1v2)
      //
      stream.writeAttribute("name", mId);

      //
      // units  { use="optional" }  (L1v1, L1v2);
      //
      stream.writeAttribute("units", mUnits);
    }
  }
  else if (level == 2)
  {
    //
    // variable: SId  { use="required" }  (L2v1, L2v2)
    //
    stream.writeAttribute("variable", mId);

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2)
    //
    if ((version == 2 || version == 3)) SBML::writeSBOTerm(stream, mSBOTerm);
  }
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Rule::writeElements (XMLOutputStream& stream) const
{
  if ( getLevel() == 2 && isSetMath() ) writeMathML(getMath(), stream);
}


/**
 * Sets the SBML Level 1 typecode for this Rule.
 */
void
Rule::setL1TypeCode (SBMLTypeCode_t type)
{
  mL1Type = type;
}




/**
 * Creates a new AlgebraicRule and optionally sets its formula.
 */
AlgebraicRule::AlgebraicRule (const string& formula) :
  Rule(SBML_ALGEBRAIC_RULE, "", formula)
{
}

/**
 * Creates a new AlgebraicRule and optionally sets its formula.
 */
AlgebraicRule::AlgebraicRule (const ASTNode* math) :
  Rule(SBML_ALGEBRAIC_RULE, "", math)
{
}


/**
 * Destroys this AlgebraicRule.
 */
AlgebraicRule::~AlgebraicRule ()
{
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next Rule
 * (if available).
 */
bool
AlgebraicRule::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}




/**
 * Creates a new AssignmentRule and optionally sets its variable and
 * formula.
 */
AssignmentRule::AssignmentRule (const string& variable, const string& formula)
  : Rule(SBML_ASSIGNMENT_RULE, variable, formula)
{
}


/**
 * Creates a new AssignmentRule and optionally sets its variable and
 * formula.
 */
AssignmentRule::AssignmentRule (const string& variable, const ASTNode* math)
  : Rule(SBML_ASSIGNMENT_RULE, variable, math)
{
}


/**
 * Destroys this AssignmentRule.
 */
AssignmentRule::~AssignmentRule ()
{
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next Rule
 * (if available).
 */
bool
AssignmentRule::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}




/**
 * Creates a new RateRule and optionally sets its variable and formula.
 */
RateRule::RateRule (const string& variable, const string& formula) :
  Rule(SBML_RATE_RULE, variable, formula)
{
}


/**
 * Creates a new RateRule and optionally sets its variable and formula.
 */
RateRule::RateRule (const string& variable, const ASTNode* math) :
  Rule(SBML_RATE_RULE, variable, math)
{
}


/**
 * Destroys this RateRule.
 */
RateRule::~RateRule ()
{
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next Rule
 * (if available).
 */
bool
RateRule::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}




/**
 * @return a (deep) copy of this ListOfRules.
 */
SBase*
ListOfRules::clone () const
{
  return new ListOfRules(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfRules::getItemTypeCode () const
{
  return SBML_RULE;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
ListOfRules::getElementName () const
{
  static const string name = "listOfRules";
  return name;
}


/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfRules::getElementPosition () const
{
  return 9;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfRules::createObject (XMLInputStream& stream)
{
  const unsigned int level  = getLevel();
  const string&      name   = stream.peek().getName();
  Rule*              object = 0;


  if (name == "algebraicRule")
  {
    object = new AlgebraicRule();
  }
  else if (level == 1)
  {
    string type = "scalar";
    stream.peek().getAttributes().readInto("type", type);

    if (type == "scalar")
    {
      object = new AssignmentRule();
    }
    else if (type == "rate")
    {
      object = new RateRule();
    }

    if (object)
    {
      if ( name == "speciesConcentrationRule" ||
           name == "specieConcentrationRule" )
      {
        object->setL1TypeCode(SBML_SPECIES_CONCENTRATION_RULE);
      }
      else if (name == "compartmentVolumeRule")
      {
        object->setL1TypeCode(SBML_COMPARTMENT_VOLUME_RULE);
      }
      else if (name == "parameterRule")
      {
        object->setL1TypeCode(SBML_PARAMETER_RULE);
      }
      else
      {
        delete object;
        object = 0;
      }
    }
  }
  else
  {
    if (name == "assignmentRule")
    {
      object = new AssignmentRule();
    }
    else if (name == "rateRule")
    {
      object = new RateRule();
    }
  }

  if (object) mItems.push_back(object);

  return object;
}




/**
 * Creates a new AlgebraicRule and returns a pointer to it.
 */
LIBSBML_EXTERN
Rule_t *
Rule_createAlgebraic ()
{
  return new(nothrow) AlgebraicRule;
}


/**
 * Creates a new AssignmentRule and returns a pointer to it
 */
LIBSBML_EXTERN
Rule_t *
Rule_createAssignment ()
{
  return new(nothrow) AssignmentRule;
}


/**
 * Creates a new RateRule and returns a pointer to it
 */
LIBSBML_EXTERN
Rule_t *
Rule_createRate ()
{
  return new(nothrow) RateRule;
}


/**
 * Destroys this Rule.
 */
LIBSBML_EXTERN
void
Rule_free (Rule_t *r)
{
  delete r;
}


/**
 * @return a (deep) copy of this Rule.
 */
LIBSBML_EXTERN
Rule_t *
Rule_clone (const Rule_t *r)
{
  return static_cast<Rule*>( r->clone() );
}


/**
 * @return the formula for this Rule.
 */
LIBSBML_EXTERN
const char *
Rule_getFormula (const Rule_t *r)
{
  return r->isSetFormula() ? r->getFormula().c_str() : NULL;
}


/**
 * @return the math for this Rule.
 */
LIBSBML_EXTERN
const ASTNode_t *
Rule_getMath (const Rule_t *r)
{
  return r->getMath();
}


/**
 * @return the sboTerm of this Rule as an integer.  If not set, sboTerm
 * will be -1.  Use SBML_sboTermToString() to convert the sboTerm to a
 * zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
Rule_getSBOTerm (const Rule_t *r)
{
  return r->getSBOTerm();
}


/**
 * @return the type of this Rule, either RULE_TYPE_RATE or
 * RULE_TYPE_SCALAR.
 */
LIBSBML_EXTERN
RuleType_t
Rule_getType (const Rule_t *r)
{
  return r->getType();
}


/**
 * @return the variable for this Rule.
 */
LIBSBML_EXTERN
const char *
Rule_getVariable (const Rule_t *r)
{
  return r->isSetVariable() ? r->getVariable().c_str() : NULL;
}


/**
 * @return the units for this Rule (L1 ParameterRules only).
 */
LIBSBML_EXTERN
const char *
Rule_getUnits (const Rule_t *r)
{
  return r->isSetUnits() ? r->getUnits().c_str() : NULL;
}


/**
 * @return true (non-zero) if the formula (or equivalently the math) for
 * this Rule has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetFormula (const Rule_t *r)
{
  return static_cast<int>( r->isSetFormula() );
}


/**
 * @return true (non-zero) if the math (or equivalently the formula) for
 * this Rule has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetMath (const Rule_t *r)
{
  return static_cast<int>( r->isSetMath() );
}


/**
 * @return true (non-zero) if the sboTerm of this Rule has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetSBOTerm (const Rule_t *r)
{
  return static_cast<int>( r->isSetSBOTerm() );
}


/**
 * @return true (non-zero) if the variable of this Rule has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetVariable (const Rule_t *r)
{
  return static_cast<int>( r->isSetVariable() );
}


/**
 * @return true (non-zero) if the units for this Rule has been set, false
 * (0) otherwise (L1 ParameterRules only).
 */
LIBSBML_EXTERN
int
Rule_isSetUnits (const Rule_t *r)
{
  return static_cast<int>( r->isSetUnits() );
}


/**
 * Sets the formula of this Rule to a copy of string.
 */
LIBSBML_EXTERN
void
Rule_setFormula (Rule_t *r, const char *formula)
{
  (formula == NULL) ? r->setMath(0) : r->setFormula(formula);
}


/**
 * Sets the math of this Rule to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Rule_setMath (Rule_t *r, const ASTNode_t *math)
{
  r->setMath(math);
}


/**
 * Sets the sboTerm field of this Rule to value.
 */
LIBSBML_EXTERN
void
Rule_setSBOTerm (Rule_t *r, int sboTerm)
{
  r->setSBOTerm(sboTerm);
}


/**
 * Sets the variable of this RateRule to a copy of sid.
 */
LIBSBML_EXTERN
void
Rule_setVariable (Rule_t *r, const char *sid)
{
  (sid == NULL) ? r->setVariable("") : r->setVariable(sid);
}


/**
 * Sets the units for this Rule to a copy of sname (L1 ParameterRules
 * only).
 */
LIBSBML_EXTERN
void
Rule_setUnits (Rule_t *r, const char *sname)
{
  (sname == NULL) ? r->unsetUnits() : r->setUnits(sname);
}


/**
 * Unsets the sboTerm of this Rule.
 */
LIBSBML_EXTERN
void
Rule_unsetSBOTerm (Rule_t *r)
{
  r->unsetSBOTerm();
}


/**
 * Unsets the units for this Rule (L1 ParameterRules only).
 */
LIBSBML_EXTERN
void
Rule_unsetUnits (Rule_t *r)
{
  r->unsetUnits();
}


/**
 * @return true (non-zero) if this Rule is an AlgebraicRule, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Rule_isAlgebraic (const Rule_t *r)
{
  return static_cast<int>( r->isAlgebraic() );
}


/**
 * @return true (non-zero) if this Rule is an AssignmentRule, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Rule_isAssignment (const Rule_t *r)
{
  return static_cast<int>( r->isAssignment() );
}


/**
 * This method attempts to lookup the Rule's variable in the Model's list
 * of Compartments.
 *
 * @return true (non-zero) if this Rule is a CompartmentVolumeRule, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isCompartmentVolume (const Rule_t *r)
{
  return static_cast<int>( r->isCompartmentVolume() );
}


/**
 * This method attempts to lookup the Rule's variable in the Model's list
 * of Parameters.
 *
 * @return true (non-zero) if this Rule is a ParameterRule, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Rule_isParameter (const Rule_t *r)
{
  return static_cast<int>( r->isParameter() );
}


/**
 * @return true (non-zero) if this Rule is a RateRule (L2) or has
 * type="rate" (L1), false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isRate (const Rule_t *r)
{
  return static_cast<int>( r->isRate() );
}


/**
 * @return true (non-zero) if this Rule is an AssignmentRule (L2) has
 * type="scalar" (L1), false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isScalar (const Rule_t *r)
{
  return static_cast<int>( r->isScalar() );
}


/**
 * This method attempts to lookup the Rule's variable in the Model's list
 * of Species.
 *
 * @return true (non-zero) if this Rule is a SpeciesConcentrationRule, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSpeciesConcentration (const Rule_t *r)
{
  return static_cast<int>( r->isSpeciesConcentration() );
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 */
LIBSBML_EXTERN
SBMLTypeCode_t
Rule_getTypeCode (const Rule_t *r)
{
  return r->getTypeCode();
}


/**
 * @return the SBML Level 1 typecode for this Rule or SBML_UNKNOWN
 * (default).
 */
LIBSBML_EXTERN
SBMLTypeCode_t
Rule_getL1TypeCode (const Rule_t *r)
{
  return r->getL1TypeCode();
}
