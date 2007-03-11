/**
 * \file    Rule.h
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


#ifndef Rule_h
#define Rule_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


BEGIN_C_DECLS

typedef enum
{
    RULE_TYPE_RATE
  , RULE_TYPE_SCALAR
  , RULE_TYPE_INVALID
} RuleType_t;

END_C_DECLS


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class ASTNode;
class ListOfRules;
class SBMLVisitor;


class LIBSBML_EXTERN Rule : public SBase
{
public:

  /**
   * Copies this Rule.
   */
  Rule (const Rule& rhs);

  /**
   * Destroys this Rule.
   */
  virtual ~Rule ();

  /**
   * Assignment operator
   */
  Rule& operator=(const Rule& orig);

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next Rule
   * (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Rule.
   */
  virtual SBase* clone () const;


  /**
   * @return the formula for this Rule.
   */
  const std::string& getFormula () const;

  /**
   * @return the math for this Rule.
   */
  const ASTNode* getMath () const;

  /**
   * @return the type of this Rule, either RULE_TYPE_RATE or
   * RULE_TYPE_SCALAR.
   */
  RuleType_t getType () const;

  /**
   * @return the variable for this Rule.
   */
  const std::string& getVariable () const;

  /**
   * @return the units for this Rule (L1 ParameterRules only).
   */
  const std::string& getUnits () const;



  /**
   * @return true if the formula (or equivalently the math) for this Rule
   * has been set, false otherwise.
   */
  bool isSetFormula () const;

  /**
   * @return true if the formula (or equivalently the math) for this Rule
   * has been set, false otherwise.
   */
  bool isSetMath () const;

  /**
   * @return true if the variable of this Rule has been set, false
   * otherwise.
   */
  bool isSetVariable () const;

  /**
   * @return true if the units for this Rule has been set, false otherwise
   * (L1 ParameterRules only).
   */
  bool isSetUnits () const;


  /**
   * Sets the formula of this Rule to a copy of string.
   */
  void setFormula (const std::string& formula);

  /**
   * Sets the math of this Rule to a copy of the given ASTNode.
   */
  void setMath (const ASTNode* math);

  /**
   * Sets the variable of this RateRule to a copy of sid.
   */
  void setVariable (const std::string& sid);

  /**
   * Sets the units for this Rule to a copy of sname (L1 ParameterRules
   * only).
   */
  void setUnits (const std::string& sname);


  /**
   * Unsets the units for this Rule (L1 ParameterRules only).
   */
  void unsetUnits ();


  /**
   * @return true if this Rule is an AlgebraicRule, false otherwise.
   */
  bool isAlgebraic () const;

  /**
   * @return true if this Rule is an AssignmentRule, false otherwise.
   */
  bool isAssignment () const;

  /**
   * @return true if this Rule is a CompartmentVolumeRule, false otherwise.
   */
  bool isCompartmentVolume () const;

  /**
   * @return true if this Rule is a ParameterRule, false otherwise.
   */
  bool isParameter () const;

  /**
   * @return true if this Rule is a RateRule (L2) or has type="rate" (L1),
   * false otherwise.
   */
  bool isRate () const;

  /**
   * @return true if this Rule is an AssignmentRule (L2) has type="scalar"
   * (L1), false otherwise.
   */
  bool isScalar () const;

  /**
   * @return true if this Rule is a SpeciesConcentrationRule, false
   * otherwise.
   */
  bool isSpeciesConcentration () const;


  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * @return the SBML Level 1 typecode for this Rule or SBML_UNKNOWN
   * (default).
   */
  SBMLTypeCode_t getL1TypeCode () const;


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const;

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


protected:

  /**
   * Only subclasses may create Rules.
   */
  Rule (  SBMLTypeCode_t      type
        , const std::string&  variable
        , const std::string&  formula );

  /**
   * Only subclasses may create Rules.
   */
  Rule (  SBMLTypeCode_t      type
        , const std::string&  variable
        , const ASTNode*      math );


  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;

  /**
   * Sets the SBML Level 1 typecode for this Rule.
   */
  void setL1TypeCode (SBMLTypeCode_t type);


  mutable std::string  mFormula;
  mutable ASTNode*     mMath;
  std::string          mUnits;

  SBMLTypeCode_t mType;
  SBMLTypeCode_t mL1Type;


  friend class ListOfRules;
};



class LIBSBML_EXTERN AlgebraicRule : public Rule
{
public:

  /**
   * Creates a new AlgebraicRule and optionally sets its formula.
   */
  AlgebraicRule (const std::string& formula = "");

  /**
   * Creates a new AlgebraicRule and optionally sets its formula.
   */
  AlgebraicRule (const ASTNode* math);

  /**
   * Destroys this AlgebraicRule.
   */
  virtual ~AlgebraicRule ();

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next Rule
   * (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
};



class LIBSBML_EXTERN AssignmentRule : public Rule
{
public:

  /**
   * Creates a new AssignmentRule and optionally sets its variable and
   * formula.
   */
  AssignmentRule (  const std::string& variable = ""
                  , const std::string& formula  = "" );

  /**
   * Creates a new AssignmentRule and optionally sets its variable and
   * formula.
   */
  AssignmentRule (const std::string& variable, const ASTNode* math);

  /**
   * Destroys this AssignmentRule.
   */
  virtual ~AssignmentRule ();

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next Rule
   * (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
};



class LIBSBML_EXTERN RateRule : public Rule
{
public:

  /**
   * Creates a new RateRule and optionally sets its variable and formula.
   */
  RateRule (const std::string& variable = "", const std::string& formula = "");


  /**
   * Creates a new RateRule and optionally sets its variable and formula.
   */
  RateRule (const std::string& variable, const ASTNode* math);

  /**
   * Destroys this RateRule.
   */
  virtual ~RateRule ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next Rule
   * (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
};



class LIBSBML_EXTERN ListOfRules : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfRules.
   */
  virtual SBase* clone () const;

  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const;

  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/**
 * Creates a new AlgebraicRule and returns a pointer to it.
 */
LIBSBML_EXTERN
Rule_t *
Rule_createAlgebraic ();


/**
 * Creates a new AssignmentRule and returns a pointer to it
 */
LIBSBML_EXTERN
Rule_t *
Rule_createAssignment ();


/**
 * Creates a new RateRule and returns a pointer to it
 */
LIBSBML_EXTERN
Rule_t *
Rule_createRate ();


/**
 * Destroys this Rule.
 */
LIBSBML_EXTERN
void
Rule_free (Rule_t *r);


/**
 * @return a (deep) copy of this Rule.
 */
LIBSBML_EXTERN
Rule_t *
Rule_clone (const Rule_t *r);


/**
 * @return the formula for this Rule.
 */
LIBSBML_EXTERN
const char *
Rule_getFormula (const Rule_t *r);


/**
 * @return the math for this Rule.
 */
LIBSBML_EXTERN
const ASTNode_t *
Rule_getMath (const Rule_t *r);


/**
 * @return the type of this Rule, either RULE_TYPE_RATE or
 * RULE_TYPE_SCALAR.
 */
LIBSBML_EXTERN
RuleType_t
Rule_getType (const Rule_t *r);


/**
 * @return the variable for this Rule.
 */
LIBSBML_EXTERN
const char *
Rule_getVariable (const Rule_t *r);


/**
 * @return the units for this Rule (L1 ParameterRules only).
 */
LIBSBML_EXTERN
const char *
Rule_getUnits (const Rule_t *r);


/**
 * @return true (non-zero) if the formula (or equivalently the math) for
 * this Rule has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetFormula (const Rule_t *r);


/**
 * @return true (non-zero) if the math (or equivalently the formula) for
 * this Rule has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetMath (const Rule_t *r);


/**
 * @return true (non-zero) if the variable of this Rule has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSetVariable (const Rule_t *r);


/**
 * @return true (non-zero) if the units for this Rule has been set, false
 * (0) otherwise (L1 ParameterRules only).
 */
LIBSBML_EXTERN
int
Rule_isSetUnits (const Rule_t *r);


/**
 * Sets the formula of this Rule to a copy of string.
 */
LIBSBML_EXTERN
void
Rule_setFormula (Rule_t *r, const char *formula);


/**
 * Sets the math of this Rule to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Rule_setMath (Rule_t *r, const ASTNode_t *math);


/**
 * Sets the variable of this RateRule to a copy of sid.
 */
LIBSBML_EXTERN
void
Rule_setVariable (Rule_t *r, const char *sid);


/**
 * Sets the units for this Rule to a copy of sname (L1 ParameterRules
 * only).
 */
LIBSBML_EXTERN
void
Rule_setUnits (Rule_t *r, const char *sname);


/**
 * Unsets the units for this Rule (L1 ParameterRules only).
 */
LIBSBML_EXTERN
void
Rule_unsetUnits (Rule_t *r);


/**
 * @return true (non-zero) if this Rule is an AlgebraicRule, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Rule_isAlgebraic (const Rule_t *r);


/**
 * @return true (non-zero) if this Rule is an AssignmentRule, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Rule_isAssignment (const Rule_t *r);


/**
 * This method attempts to lookup the Rule's variable in the Model's list
 * of Compartments.
 *
 * @return true (non-zero) if this Rule is a CompartmentVolumeRule, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isCompartmentVolume (const Rule_t *r);


/**
 * This method attempts to lookup the Rule's variable in the Model's list
 * of Parameters.
 *
 * @return true (non-zero) if this Rule is a ParameterRule, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Rule_isParameter (const Rule_t *r);


/**
 * @return true (non-zero) if this Rule is a RateRule (L2) or has
 * type="rate" (L1), false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isRate (const Rule_t *r);


/**
 * @return true (non-zero) if this Rule is an AssignmentRule (L2) has
 * type="scalar" (L1), false (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isScalar (const Rule_t *r);


/**
 * This method attempts to lookup the Rule's variable in the Model's list
 * of Species.
 *
 * @return true (non-zero) if this Rule is a SpeciesConcentrationRule, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Rule_isSpeciesConcentration (const Rule_t *r);


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 */
LIBSBML_EXTERN
SBMLTypeCode_t
Rule_getTypeCode (const Rule_t *r);

/**
 * @return the SBML Level 1 typecode for this Rule or SBML_UNKNOWN
 * (default).
 */
LIBSBML_EXTERN
SBMLTypeCode_t
Rule_getL1TypeCode (const Rule_t *r);


END_C_DECLS


#endif  /* !SWIG  */
#endif  /* Rule_h */
