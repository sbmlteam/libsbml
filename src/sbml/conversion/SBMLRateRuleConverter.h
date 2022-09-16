/**
 * @file    SBMLRateRuleConverter.h
 * @brief   Definition of SBMLRateRuleConverter, a converter from raterule to reaction
 * @author  Sarah Keating
 * @author  Alessandro Felder
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class SBMLRateRuleConverter
 * @sbmlbrief{core} Converter that sorts SBML rules and assignments.
 *
 * @htmlinclude libsbml-facility-only-warning.html
 *
 * ADD DESCRIPTION
 *
 * @section SBMLRateRuleConverter-usage Configuration and use of SBMLRateRuleConverter
 *
 * SBMLRateRuleConverter is enabled by creating a ConversionProperties object
 * with the option @c "inferReactions", and passing this properties object to
 * SBMLDocument::convert(@if java ConversionProperties@endif).  This
 * converter offers no other options.
 *
 * Implementation is based on the algorithm described in Fages et al, Theoretical Computer Science, 2015.
 *
 * @copydetails doc_section_using_sbml_converters
 */


#ifndef SBMLRateRuleConverter_h
#define SBMLRateRuleConverter_h

#include <sbml/SBMLNamespaces.h>
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/math/ASTNode.h>

typedef enum
{
  POSITIVE_DERIVATIVE
  , NEGATIVE_DERIVATIVE
} DerivativeSign_t;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN
typedef std::vector< std::pair< std::string, ASTNode*> > pairODEs;

class LIBSBML_EXTERN SBMLRateRuleConverter : public SBMLConverter
{
public:

  /** @cond doxygenLibsbmlInternal */
  /**
   * Register with the ConversionRegistry.
   */
  static void init();

  /** @endcond */


  /**
   * Creates a new SBMLLevelVersionConverter object.
   */
  SBMLRateRuleConverter();


  /**
   * Copy constructor; creates a copy of an SBMLLevelVersionConverter
   * object.
   *
   * @param obj the SBMLLevelVersionConverter object to copy.
   */
  SBMLRateRuleConverter(const SBMLRateRuleConverter& obj);


  /**
  * Assignment operator for SBMLInferUnitsConverter.
  *
  * @param rhs the object whose values are used as the basis of the
  * assignment.
  */
  SBMLRateRuleConverter& operator=(const SBMLRateRuleConverter& rhs);


  /**
   * Creates and returns a deep copy of this SBMLLevelVersionConverter
   * object.
   *
   * @return a (deep) copy of this converter.
   */
  virtual SBMLRateRuleConverter* clone() const;


  /**
   * Destroy this SBMLRateRuleConverter object.
   */
  virtual ~SBMLRateRuleConverter ();


  /**
   * Returns @c true if this converter object's properties match the given
   * properties.
   *
   * A typical use of this method involves creating a ConversionProperties
   * object, setting the options desired, and then calling this method on
   * an SBMLLevelVersionConverter object to find out if the object's
   * property values match the given ones.  This method is also used by
   * SBMLConverterRegistry::getConverterFor(@if java ConversionProperties@endif)
   * to search across all registered converters for one matching particular
   * properties.
   *
   * @param props the properties to match.
   *
   * @return @c true if this converter's properties match, @c false
   * otherwise.
   */
  virtual bool matchesProperties(const ConversionProperties &props) const;


  /**
   * Perform the conversion.
   *
   * This method causes the converter to do the actual conversion work,
   * that is, to convert the SBMLDocument object set by
   * SBMLConverter::setDocument(@if java SBMLDocument@endif) and
   * with the configuration options set by
   * SBMLConverter::setProperties(@if java ConversionProperties@endif).
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_CONV_INVALID_SRC_DOCUMENT, OperationReturnValues_t}
   */
  virtual int convert();


  /**
   * Returns the default properties of this converter.
   *
   * A given converter exposes one or more properties that can be adjusted
   * in order to influence the behavior of the converter.  This method
   * returns the @em default property settings for this converter.  It is
   * meant to be called in order to discover all the settings for the
   * converter object.
   *
   * @return the ConversionProperties object describing the default properties
   * for this converter.
   */
  virtual ConversionProperties getDefaultProperties() const;

  // helper functions whilst creating code
  typedef std::pair<std::string, std::string > pairString;

  typedef std::vector<std::pair<ASTNode*, std::vector<double> > > setCoeff;
  typedef std::vector<std::pair<ASTNode*, std::vector<double> > >::iterator setCoeffIt;
  typedef std::vector< std::vector<double> > setRnCoeffs;
  typedef std::vector< std::pair< std::string, ASTNode*> >::iterator odeIt;

  pairODEs getOde() { return mODEs; };

  setCoeff getCoeff() { return mCoefficients; };
  setRnCoeffs getReactants() { return mReactants; };
  setRnCoeffs getProducts() { return mProducts; };
  setRnCoeffs getModifiers() { return mModifiers; };

  std::vector< std::vector<bool> > getPosDer() { return mPosDerivative; };

  std::vector< std::vector<bool> > getNegDer() { return mNegDerivative; };
  bool getMathNotSupportedFlag() const;


private:
  /** @cond doxygenLibsbmlInternal */

  bool checkDocumentValidity();

  bool isDocumentAppropriate(OperationReturnValues_t& returnVal);

  void populateODEinfo();

  void populateReactionCoefficients();


  // functions for populateODEinfo()

  void addODEPair(std::string id, Model* model);

  void addToTerms(ASTNode* node);

  void createTerms(ASTNode* node);

  bool determineCoefficient(ASTNode* ode, unsigned int termN, double& coeff);

  std::vector<double> populateCoefficientVector(unsigned int termN);

  unsigned int locateTerm(ASTNode* node);

  bool determineDerivativeSign(std::string variable, ASTNode* term, bool& posDeriv);

  std::vector<bool> populateDerivativeVector(unsigned int termN);

  bool isPositive(const ASTNode* node, bool& posDeriv);

  // functions for Reaction Coefficients
  void createInitialValues();
  void analyseCoefficient(std::vector<double> coeffs, unsigned int index);
  void analysePosDerivative(std::vector<double> coeffs, unsigned int index);
  void analyseNegDerivative(std::vector<double> coeffs, unsigned int index);

  // functions to reconstruct model
  void reconstructModel();
  void dealWithSpecies();
  void createReactions();
  void removeRules();


  // member variables populated during analysis
  pairODEs mODEs;
  std::vector<ASTNode*> mTerms;
  setCoeff mCoefficients;
  std::vector< std::vector<bool> > mPosDerivative;
  std::vector< std::vector<bool> > mNegDerivative;
  DerivativeSign_t mDerivSign;
  bool mMathNotSupported;
  setRnCoeffs mProducts;
  setRnCoeffs mReactants;
  setRnCoeffs mModifiers;

  /** @endcond */

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* SBMLRateRuleConverter_h */

