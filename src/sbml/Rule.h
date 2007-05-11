/**
 * @file    Rule.h
 * @brief   SBML Rule
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
   * Destroys this Rule.
   */
  virtual ~Rule ();

  /**
   * Copy constructor. Creates a copy of this Rule.
   */
  Rule (const Rule& rhs);

  /**
   * Assignment operator.
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
   * Sets the variable of this Rule to a copy of sid.
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
   * @return the type of this Rule, either RULE_TYPE_RATE or
   * RULE_TYPE_SCALAR.
   */
  RuleType_t getType () const;

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
   * @return the name of this element eg "algebraicRule".
   
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /** @endcond doxygen-libsbml-internal */


protected:
  /** @cond doxygen-libsbml-internal */

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

  /** @endcond doxygen-libsbml-internal */
};



class LIBSBML_EXTERN AlgebraicRule : public Rule
{
public:

  /**
   * Creates a new AlgebraicRule and optionally sets its formula.
   */
  AlgebraicRule (const std::string& formula = "");

  /**
   * Creates a new AlgebraicRule and optionally sets its math.
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
   * math.
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
   * Creates a new RateRule and optionally sets its variable and math.
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
   * Returns the libSBML type code for this %SBML object.
   * 
   * @return the SBMLTypeCode_t of this object or SBML_UNKNOWN (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const { return SBML_LIST_OF; };


  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
 * @return the name of this element ie "listOfRules".
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */

  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;

  /** @endcond doxygen-libsbml-internal */


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  /** @endcond doxygen-libsbml-internal */
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBSBML_EXTERN
Rule_t *
Rule_createAlgebraic ();


LIBSBML_EXTERN
Rule_t *
Rule_createAssignment ();


LIBSBML_EXTERN
Rule_t *
Rule_createRate ();


LIBSBML_EXTERN
Rule_t *
Rule_createAlgebraicWithFormula (const char *formula);


LIBSBML_EXTERN
Rule_t *
Rule_createAssignmentWithVariableAndFormula (const char *formula,
                                             const char * variable);


LIBSBML_EXTERN
Rule_t *
Rule_createRateWithVariableAndFormula (const char * variable, 
                                       const char *formula);


LIBSBML_EXTERN
Rule_t *
Rule_createAlgebraicWithMath (ASTNode_t *math);


LIBSBML_EXTERN
Rule_t *
Rule_createAssignmentWithVariableAndMath (const char * variable, 
                                          ASTNode_t *math);


LIBSBML_EXTERN
Rule_t *
Rule_createRateWithVariableAndMath (const char * variable, 
                                    ASTNode_t *math);


LIBSBML_EXTERN
void
Rule_free (Rule_t *r);


LIBSBML_EXTERN
Rule_t *
Rule_clone (const Rule_t *r);


LIBSBML_EXTERN
const char *
Rule_getFormula (const Rule_t *r);


LIBSBML_EXTERN
const ASTNode_t *
Rule_getMath (const Rule_t *r);


LIBSBML_EXTERN
RuleType_t
Rule_getType (const Rule_t *r);


LIBSBML_EXTERN
const char *
Rule_getVariable (const Rule_t *r);


LIBSBML_EXTERN
const char *
Rule_getUnits (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSetFormula (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSetMath (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSetVariable (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSetUnits (const Rule_t *r);


LIBSBML_EXTERN
void
Rule_setFormula (Rule_t *r, const char *formula);


LIBSBML_EXTERN
void
Rule_setMath (Rule_t *r, const ASTNode_t *math);


LIBSBML_EXTERN
void
Rule_setVariable (Rule_t *r, const char *sid);


LIBSBML_EXTERN
void
Rule_setUnits (Rule_t *r, const char *sname);


LIBSBML_EXTERN
void
Rule_unsetUnits (Rule_t *r);


LIBSBML_EXTERN
int
Rule_isAlgebraic (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isAssignment (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isCompartmentVolume (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isParameter (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isRate (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isScalar (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSpeciesConcentration (const Rule_t *r);


LIBSBML_EXTERN
SBMLTypeCode_t
Rule_getTypeCode (const Rule_t *r);


LIBSBML_EXTERN
SBMLTypeCode_t
Rule_getL1TypeCode (const Rule_t *r);


END_C_DECLS


#endif  /* !SWIG  */
#endif  /* Rule_h */
