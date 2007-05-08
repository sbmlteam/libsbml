/**
 * @file    KineticLaw.h
 * @brief   SBML KineticLaw
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

#ifndef KineticLaw_h
#define KineticLaw_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/Parameter.h>


class ASTNode;
class Parameter;
class SBMLVisitor;


class LIBSBML_EXTERN KineticLaw : public SBase
{
public:

  /**
   * Creates a new KineticLaw, optionally with its formula, timeUnits
   * and/or substanceUnits set.
   *
   * @param formula string representing the kinetic law.
   * @param timeUnits string representing the units of time for this KineticLaw.
   * @param substanceUnits string representing the units of substance for this KineticLaw.
   * 
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  KineticLaw (   const std::string& formula        = ""
               , const std::string& timeUnits      = ""
               , const std::string& substanceUnits = "" );

  /**
   * Creates a new KineticLaw, optionally with its math, timeUnits
   * and/or substanceUnits set.
   *
   * @param math ASTNode representing the kinetic law.
   * @param timeUnits string representing the units of time for this KineticLaw.
   * @param substanceUnits string representing the units of substance for this KineticLaw.
   * 
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  KineticLaw (   ASTNode*      math
               , const std::string& timeUnits      = ""
               , const std::string& substanceUnits = "" );

  /**
   * Destroys this KineticLaw.
   */
  virtual ~KineticLaw ();

  /**
   * Copy constructor; creates a copy of this KineticLaw.
   */
  KineticLaw (const KineticLaw& rhs);

  /**
   * Assignment operator
   */
  KineticLaw& operator=(const KineticLaw& orig);

  /**
   * Accepts the given SBMLVisitor for this instance of KineticLaw.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * Creates and returns a deep copy of this KineticLaw.
   *
   * @return a (deep) copy of this KineticLaw.
   */
  virtual SBase* clone () const;


  /**
   * Get the mathematical formula for the kineticlaw and return it as
   * as a string.
   * 
   * @return a string representing the formula of this Kineticlaw.
   */
  const std::string& getFormula () const;

  /**
   * Get the mathematical formula for the kineticlaw and return it
   * as an AST.
   * 
   * @return the math of this Kineticlaw.
   */
  const ASTNode* getMath () const;

  /**
   * Return the timeUnits of this KineticLaw.
   *
   * @return the timeUnits of this KineticLaw.
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  const std::string& getTimeUnits () const;

  /**
   * Return the substanceUnits of this KineticLaw.
   *
   * @return the substanceUnits of this KineticLaw.
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  const std::string& getSubstanceUnits () const;

  /**
   * Predicate to test whether the formula for this kineticlaw has been set.
   *
   * This is identical to the method isSetMath().  It is provided
   * in order to mirror the parallel between getFormula() and getMath().
   *
   * @return @c true if the formula (meaning the @c math subelement) of
   * this Kineticlaw has been set, @c false otherwise.
   */  bool isSetFormula () const;

  /**
   * Predicate to test whether the math for this kineticlaw has been set.
   *
   * This is identical to the method isSetFormula().  It is provided
   * in order to mirror the parallel between getFormula() and getMath().
   * 
   * @return @c true if the formula (meaning the @c math subelement) of
   * this Kineticlaw has been set, @c false otherwise.
   */
  bool isSetMath () const;

 /**
   * Predicate to test whether the timeUnits for this kineticlaw has been set.
   *
   * @return @c true if the timeUnits of this KineticLaw has been set, @c false
   * otherwise.
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  bool isSetTimeUnits () const;

  /**
   * Predicate to test whether the substanceUnits for this kineticlaw has been set.
   *
   * @return @c true if the substanceUnits of this KineticLaw has been set, @c false
   * otherwise.
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  bool isSetSubstanceUnits () const;

  /**
   * Sets the mathematical expression of this Kineticlaw instance to the given string
   * @p formula.
   *
   * The given @p formula string is copied.
   *
   * @param formula the mathematical expression to use.
   */
  void setFormula (const std::string& formula);

  /**
   * Sets the mathematical expression of this Kineticlaw instance to a copy of the given
   * ASTNode.
   *
   * @param math an ASTNode representing a formula tree.
   */
  void setMath (const ASTNode* math);

  /**
   * Sets the timeUnits of this KineticLaw to a copy of sid.
   *
   * @param sid string representing the time units.
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  void setTimeUnits (const std::string& sid);

  /**
   * Sets the substanceUnits of this KineticLaw to a copy of sid.
   *
   * @param sid string representing the time units.
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  void setSubstanceUnits (const std::string& sid);

  /**
   * Unsets the timeUnits of this KineticLaw.
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  void unsetTimeUnits ();

  /**
   * Unsets the substanceUnits of this KineticLaw.
   *
   * @note starting from #SBML Level 2 Version 2 timeUnits and substanceUnits have
   * been removed.
   */
  void unsetSubstanceUnits ();

  /**
   * Adds a copy of the given Parameter to this KineticLaw.
   */
  void addParameter (const Parameter* p);

  /**
   * Creates a new Parameter, adds it to this KineticLaw's list of
   * parameters and returns it.
   */
  Parameter* createParameter ();

  /**
   * @return the list of Parameters for this KineticLaw.
   */
  const ListOfParameters* getListOfParameters () const;

  /**
   * @return the list of Parameters for this KineticLaw.
   */
  ListOfParameters* getListOfParameters ();

  /**
   * @return the nth Parameter of this KineticLaw.
   */
  const Parameter* getParameter (unsigned int n) const;

  /**
   * @return the nth Parameter of this KineticLaw.
   */
  Parameter* getParameter (unsigned int n);

  /**
   * @return the Parameter in this kineticLaw with the given id or NULL if
   * no such Parameter exists.
   */
  const Parameter* getParameter (const std::string& sid) const;

  /**
   * @return the Parameter in this kineticLaw with the given id or NULL if
   * no such Parameter exists.
   */
  Parameter* getParameter (const std::string& sid);

  /**
   * @return the number of Parameters in this KineticLaw.
   */
  unsigned int getNumParameters () const;


  /**
   * Sets the parent SBMLDocument of this SBML object.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * @return the name of this element ie "kineticLaw".
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */

  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;


  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /** @endcond doxygen-libsbml-internal */


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

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


  mutable std::string  mFormula;
  mutable ASTNode*     mMath;

  ListOfParameters  mParameters;
  std::string       mTimeUnits;
  std::string       mSubstanceUnits;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/**
 * Creates a new KineticLaw and returns a pointer to it.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_create (void);

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
                        const char *substanceUnits );

LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWithMathAndUnits ( ASTNode_t *math,
                            const char *timeUnits,
                            const char *substanceUnits );

LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWithMath ( ASTNode_t *math);

/**
 * Frees the given KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_free (KineticLaw_t *kl);

/**
 * @return a (deep) copy of this KineticLaw.
 */
LIBSBML_EXTERN
SBase_t *
KineticLaw_clone (const KineticLaw_t *kl);


/**
 * @return the formula of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getFormula (const KineticLaw_t *kl);

/**
 * @return the math of this KineticLaw.
 */
LIBSBML_EXTERN
const ASTNode_t *
KineticLaw_getMath (const KineticLaw_t *kl);

/**
 * @return the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getTimeUnits (const KineticLaw_t *kl);

/**
 * @return the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getSubstanceUnits (const KineticLaw_t *kl);


/**
 * @return true (non-zero) if the formula (or equivalently the math) of
 * this KineticLaw has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetFormula (const KineticLaw_t *kl);

/**
 * @return true (non-zero) if the math (or equivalently the formula) of
 * this KineticLaw has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetMath (const KineticLaw_t *kl);

/**
 * @return true (non-zero) if the timeUnits of this KineticLaw has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetTimeUnits (const KineticLaw_t *kl);

/**
 * @return true (non-zero) if the substanceUnits of this KineticLaw has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSubstanceUnits (const KineticLaw_t *kl);

/**
 * Sets the formula of this KineticLaw to a copy of formula.
 */
LIBSBML_EXTERN
void
KineticLaw_setFormula (KineticLaw_t *kl, const char *formula);

/**
 * Sets the math of this KineticLaw to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
KineticLaw_setMath (KineticLaw_t *kl, const ASTNode_t *math);


/**
 * Sets the timeUnits of this KineticLaw to a copy of sid.
 */
LIBSBML_EXTERN
void
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sid);

/**
 * Sets the substanceUnits of this KineticLaw to a copy of sid.
 */
LIBSBML_EXTERN
void
KineticLaw_setSubstanceUnits (KineticLaw_t *kl, const char *sid);

/**
 * Unsets the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetTimeUnits (KineticLaw_t *kl);

/**
 * Unsets the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetSubstanceUnits (KineticLaw_t *kl);

/**
 * Adds a copy of the given Parameter to this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_addParameter (KineticLaw_t *kl, const Parameter_t *p);

/**
 * Creates a new Parameter, adds it to this KineticLaw's list of
 * parameters and returns it.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_createParameter (KineticLaw_t *kl);

/**
 * @return the list of Parameters for this KineticLaw.
 */
LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfParameters (KineticLaw_t *kl);

/**
 * @return the nth Parameter of this KineticLaw.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (KineticLaw_t *kl, unsigned int n);

/**
 * @return the Parameter in this kineticLaw with the given id or NULL if no
 * such Parameter exists.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameterById (KineticLaw_t *kl, const char *sid);

/**
 * @return the number of Parameters in this KineticLaw.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl);

END_C_DECLS


#endif  /* !SWIG */
#endif  /* KineticLaw_h */
