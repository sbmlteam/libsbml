/**
 * @file    Rule.cpp
 * @brief   Implementations of Rule, ListOfRules, AlgebraicRule, AssignmentRule
 *          and RateRule.
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
#include <sbml/xml/XMLNamespaces.h>

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBO.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/Rule.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */
/**
 * Only subclasses may create Rules.
 */
Rule::Rule (SBMLTypeCode_t type, const std::string& variable, const std::string& formula)
 :
   SBase   ( variable , "", -1)
 , mFormula( formula  )
 , mMath   (  0       )
 , mType   ( type     )
 , mL1Type ( SBML_UNKNOWN )
{
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Only subclasses may create Rules.
 */
Rule::Rule (SBMLTypeCode_t type, const std::string& variable, const ASTNode* math)
 :
   SBase   ( variable  , "", -1       )
 , mMath   ( 0                )
 , mType   ( type             )
 , mL1Type ( SBML_UNKNOWN     )
{
  if (math) mMath = math->deepCopy();
}
/** @endcond doxygen-libsbml-internal */


/**
 * Destroys this Rule.
 */
Rule::~Rule ()
{
  delete mMath;
}


/**
 * Copy constructor. Creates a copy of this Rule.
 */
Rule::Rule (const Rule& orig) :
   SBase   ( orig          )
 , mFormula( orig.mFormula )
 , mMath   ( 0            )
 , mUnits  ( orig.mUnits   )
 , mType   ( orig.mType    )
 , mL1Type ( orig.mL1Type  )
{
  if (orig.mMath) mMath = orig.mMath->deepCopy();
}


/**
 * Assignment operator.
 */
Rule& Rule::operator=(const Rule& rhs)
{
  this->SBase::operator =(rhs);
  mFormula = rhs.mFormula ;
  mUnits   = rhs.mUnits   ;
  mType    = rhs.mType    ;
  mL1Type  = rhs.mL1Type  ;
  if (rhs.mMath) mMath = rhs.mMath->deepCopy();
  return *this;
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
Rule::setFormula (const std::string& formula)
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
 * Sets the variable of this Rule to a copy of sid.
 */
void
Rule::setVariable (const std::string& sid)
{
  setId(sid);
}


/**
 * Sets the units for this Rule to a copy of sname (L1 ParameterRules
 * only).
 */
void
Rule::setUnits (const std::string& sname)
{
  mUnits = sname;
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
 * @return the name of this element eg "algebraicRule".
 
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


/** @cond doxygen-libsbml-internal */
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
      logError(SBMLError::InvalidMathElement);
    }

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
      SBase::checkIdSyntax();
    }
    else if ( isCompartmentVolume() )
    {
      //
      // compartment: SName  { use="required" }  (L1v1, L1v2)
      //
      attributes.readInto("compartment", mId);
      SBase::checkIdSyntax();
    }
    else if ( isParameter() )
    {
      //
      // name: SName  { use="required" } (L1v1, L1v2)
      //
      attributes.readInto("name", mId);
      SBase::checkIdSyntax();

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
    SBase::checkIdSyntax();

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2)
    //
    if ((version == 2 || version == 3)) 
      mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
//    if ((version == 2 || version == 3)) SBO::writeTerm(stream, mSBOTerm);
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Sets the SBML Level 1 typecode for this Rule.
 */
void
Rule::setL1TypeCode (SBMLTypeCode_t type)
{
  mL1Type = type;
}
/** @endcond doxygen-libsbml-internal */



/**
 * Creates a new AlgebraicRule and optionally sets its formula.
 */
AlgebraicRule::AlgebraicRule (const std::string& formula) :
  Rule(SBML_ALGEBRAIC_RULE, "", formula)
{
}

/**
 * Creates a new AlgebraicRule and optionally sets its math.
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
AssignmentRule::AssignmentRule (const std::string& variable, const std::string& formula)
  : Rule(SBML_ASSIGNMENT_RULE, variable, formula)
{
}


/**
 * Creates a new AssignmentRule and optionally sets its variable and
 * math.
 */
AssignmentRule::AssignmentRule (const std::string& variable, const ASTNode* math)
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
RateRule::RateRule (const std::string& variable, const std::string& formula) :
  Rule(SBML_RATE_RULE, variable, formula)
{
}


/**
 * Creates a new RateRule and optionally sets its variable and math.
 */
RateRule::RateRule (const std::string& variable, const ASTNode* math) :
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
 * @return the name of this element ie "listOfRules".
 */
const string&
ListOfRules::getElementName () const
{
  static const string name = "listOfRules";
  return name;
}


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfRules::getElementPosition () const
{
  return 9;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


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
 * Creates a new AlgebraicRule with the given formula 
 * and returns a pointer to it.
 *
 * @return pointer to the newly created Algebraic Rule_t structure.
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this one) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  
 */
LIBSBML_EXTERN
Rule_t *
Rule_createAlgebraicWithFormula (const char *formula)
{
  return new(nothrow) AlgebraicRule(formula);
}


/**
 * Creates a new AssignmentRule with the given formula 
 * and returns a pointer to it.
 *
 * @param variable string representing the variable to be assigned by
 * the Rule_t structure.
 * @param formula string representing the formula of the Rule_t structure.
 *
 * @return pointer to the newly created Assignment Rule_t structure.
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this one) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  
 */
LIBSBML_EXTERN
Rule_t *
Rule_createAssignmentWithVariableAndFormula (const char * variable, 
                                             const char *formula)
{
  return new(nothrow) AssignmentRule(variable, formula);
}


/**
 * Creates a new RateRule with the given formula 
 * and returns a pointer to it.
 *
 * @param variable string representing the variable to be assigned by
 * the Rule_t structure.
 * @param formula string representing the formula of the Rule_t structure.
 *
 * @return pointer to the newly created Rate Rule_t structure.
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this one) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  
 */
LIBSBML_EXTERN
Rule_t *
Rule_createRateWithVariableAndFormula (const char * variable, 
                                       const char *formula)
{
  return new(nothrow) RateRule(variable, formula);
}


/**
 * Creates a new AlgebraicRule with the given math 
 * and returns a pointer to it.
 *
 * @return pointer to the newly created Algebraic Rule_t structure.
 */
LIBSBML_EXTERN
Rule_t *
Rule_createAlgebraicWithMath (ASTNode *math)
{
  return new(nothrow) AlgebraicRule(math);
}


/**
 * Creates a new AssignmentRule with the given math 
 * and returns a pointer to it.
 *
 * @param variable string representing the variable to be assigned by
 * the Rule_t structure.
 * @param math ASTNode_t structure representing the math of the rule.
 *
 * @return pointer to the newly created Assignment Rule_t structure.
 */
LIBSBML_EXTERN
Rule_t *
Rule_createAssignmentWithVariableAndMath (const char * variable, 
                                             ASTNode *math)
{
  return new(nothrow) AssignmentRule(variable, math);
}


/**
 * Creates a new RateRule with the given math 
 * and returns a pointer to it.
 *
 * @param variable string representing the variable to be assigned by
 * the Rule_t structure.
 * @param math ASTNode_t structure representing the math of the rule.
 *
 * @return pointer to the newly created Rate Rule_t structure.
 */
LIBSBML_EXTERN
Rule_t *
Rule_createRateWithVariableAndMath (const char * variable, 
                                       ASTNode *math)
{
  return new(nothrow) RateRule (variable, math);
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
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this one) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.  
 *
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
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this one) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.
 *
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
 *
 * @note SBML Level 1 uses a text-string format for mathematical formulas.
 * SBML Level 2 uses MathML, an XML format for representing mathematical
 * expressions.  LibSBML provides an Abstract Syntax Tree API for working
 * with mathematical expressions; this API is more powerful than working
 * with formulas directly in text form, and ASTs can be translated into
 * either MathML or the text-string syntax.  The libSBML methods that
 * accept text-string formulas directly (such as this one) are
 * provided for SBML Level 1 compatibility, but developers are encouraged
 * to use the AST mechanisms.
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

LIBSBML_EXTERN
void
Rule_setL1TypeCode (Rule_t *r, SBMLTypeCode_t L1Type)
{
  r->setL1TypeCode(L1Type);
}



/** @endcond doxygen-c-only */

