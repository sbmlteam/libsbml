
/**
 * @file    SBMLRateRuleConverter.cpp
 * @brief   Implementation of SBMLRateRuleConverter, a converter from raterule to reaction
 * @author  Sarah Keating
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
 */


#include <sbml/conversion/SBMLRateRuleConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/L3Parser.h>
#include <sbml/AlgebraicRule.h>
#include <sbml/AssignmentRule.h>
#include <sbml/RateRule.h>
#include <sbml/InitialAssignment.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/ModifierSpeciesReference.h>
#include <sbml/conversion/ExpressionAnalyser.h>
#include <sbml/math/L3FormulaFormatter.h>
#ifdef __cplusplus

#include <algorithm>
#include <string>
#include <vector>
#include <map>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN



/** @cond doxygenLibsbmlInternal */
void SBMLRateRuleConverter::init()
{
  SBMLRateRuleConverter converter;
  SBMLConverterRegistry::getInstance().addConverter(&converter);
}
/** @endcond */

SBMLRateRuleConverter::SBMLRateRuleConverter() 
  : SBMLConverter("SBML RateRule Converter")
  , mODEs ()
  , mTerms ()
  , mCoefficients ()
  , mPosDerivative ()
  , mNegDerivative ()
  , mDerivSign (POSITIVE_DERIVATIVE)
  , mMathNotSupported (false)
  , mProducts ()
  , mReactants ()
  , mModifiers ()
{
}

SBMLRateRuleConverter::SBMLRateRuleConverter(const SBMLRateRuleConverter& orig) :
  SBMLConverter(orig)
  , mODEs(orig.mODEs)
  , mTerms(orig.mTerms)
  , mCoefficients(orig.mCoefficients)
  , mPosDerivative(orig.mPosDerivative)
  , mNegDerivative(orig.mNegDerivative)
  , mDerivSign(POSITIVE_DERIVATIVE)
  , mMathNotSupported(orig.mMathNotSupported)
  , mProducts(orig.mProducts)
  , mReactants(orig.mReactants)
  , mModifiers(orig.mModifiers)
{
}

/*
* Assignment operator for SBMLLevelVersionConverter.
*/
SBMLRateRuleConverter&
SBMLRateRuleConverter::operator=(const SBMLRateRuleConverter& rhs)
{
  if (&rhs != this)
  {
    this->SBMLConverter::operator =(rhs);
    //, mODEs(NULL)
    //  , mTerms(NULL)
    //  , mCoefficients(NULL)
      mPosDerivative = rhs.mPosDerivative;
    mNegDerivative = rhs.mNegDerivative;
    mDerivSign = rhs.mDerivSign;
    mMathNotSupported = rhs.mMathNotSupported;
    mProducts = rhs.mProducts;
    mReactants = rhs.mReactants;
    mModifiers = rhs.mModifiers;

  }

  return *this;
}

SBMLRateRuleConverter* 
SBMLRateRuleConverter::clone() const
{
  return new SBMLRateRuleConverter(*this);
}

/*
 * Destroy this object.
 */
SBMLRateRuleConverter::~SBMLRateRuleConverter ()
{
  for (std::vector<std::pair<std::string, ASTNode*> >::iterator it = mODEs.begin(); it != mODEs.end(); ++it)
  {
    if (it->second != NULL)
    {
      delete it->second;
      it->second = NULL;
    }
  }
  mODEs.clear();

  for (std::vector<ASTNode*>::iterator it = mTerms.begin(); it != mTerms.end(); ++it)
  {
    delete *it;
  }
  mTerms.clear();

  for (std::vector<std::pair<ASTNode*, std::vector<double> > >::iterator it = mCoefficients.begin(); it != mCoefficients.end(); ++it)
  {
    // the ASTNode* is the pointer to the term which has been destroyed above
    //if (it->first != NULL)
    //{
    //  delete it->first;
    //  it->first = NULL;
    //}
    it->second.clear();
  }
  mCoefficients.clear();
  mPosDerivative.clear();
  mNegDerivative.clear();
  mProducts.clear();
  mReactants.clear();
  mModifiers.clear();
}


ConversionProperties
SBMLRateRuleConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  static bool init = false;

  if (init) 
  {
    return prop;
  }
  else
  {
    prop.addOption("inferReactions", true,
                 "Infer reactions from rateRules in the model");
    init = true;
    return prop;
  }
}

bool 
SBMLRateRuleConverter::matchesProperties(const ConversionProperties &props) const
{
  if (!props.hasOption("inferReactions"))
    return false;
  return true;
}


int 
SBMLRateRuleConverter::convert()
{
  // if we cannot do the conversion - dont try
  OperationReturnValues_t returnValue;
  if (!isDocumentAppropriate(returnValue))
  {
    return returnValue;
  }

  // Fages algo 3.6 Steps 1-2
  populateODEinfo();

  if (getMathNotSupportedFlag() == true)
  {
    return LIBSBML_OPERATION_FAILED;
  }

  // Fages algo 3.6 Step 3-4a-d
  populateReactionCoefficients();

  // Fages algo 3.6 Step 4e
  reconstructModel();

  return LIBSBML_OPERATION_SUCCESS;
  
}

/** @cond doxygenIgnored */

bool
SBMLRateRuleConverter::checkDocumentValidity()
{
  bool valid = true;
  /* check consistency of model */
  /* since this function will write to the error log we should
  * clear anything in the log first
  */
  mDocument->getErrorLog()->clearLog();
  unsigned char origValidators = mDocument->getApplicableValidators();

  mDocument->setApplicableValidators(AllChecksON);

  mDocument->checkConsistency();


  /* replace original consistency checks */
  mDocument->setApplicableValidators(origValidators);

  if (mDocument->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) != 0)
  {
    valid = false;
  }

  return valid;
}
/** @endcond */

/** @cond doxygenIgnored */

bool
SBMLRateRuleConverter::isDocumentAppropriate(OperationReturnValues_t& returnValue)
{
  // there are a number of scenarios that the converter will not handle

  // 1. document is null or model is null
  if (mDocument == NULL)
  {
    returnValue = LIBSBML_INVALID_OBJECT;
    return false;
  }
  Model* mModel = mDocument->getModel();
  if (mModel == NULL)
  {
    returnValue = LIBSBML_INVALID_OBJECT;
    return false;
  }

  // 2. there are no rate rules/already reactions/multiple compartments
  if (mModel->getNumRules() == 0)
  {
    returnValue = LIBSBML_OPERATION_SUCCESS;
    return false;
  }
  else
  {
    bool rateRule = false;
    unsigned int n = 0;
    while (!rateRule && n < mModel->getNumRules())
    {
      if (mModel->getRule(n)->getType() == RULE_TYPE_RATE)
      {
        rateRule = true;
      }
      n++;
    }
    if (!rateRule)
    {
      returnValue = LIBSBML_OPERATION_SUCCESS;
      return false;
    }
  }

  if (mModel->getNumReactions() > 0)
  {
    returnValue = LIBSBML_OPERATION_SUCCESS;
    return false;
  }

  if (mModel->getNumCompartments() > 1)
  {
    returnValue = LIBSBML_OPERATION_SUCCESS;
    return false;
  }

  // 3. the document is invalid
  if (checkDocumentValidity() == false)
  {
    returnValue = LIBSBML_CONV_INVALID_SRC_DOCUMENT;
    return false;
  }

  return true;
}
/** @endcond */


/** @cond doxygenIgnored */

void 
SBMLRateRuleConverter::addODEPair(std::string id, Model* model)
{
  ASTNode * zeroNode = SBML_parseL3Formula("0");
  Rule* rr = model->getRateRuleByVariable(id);
  if (rr!= NULL && rr->getType() == RULE_TYPE_RATE)
  {
    ASTNode * math;
    if (rr->isSetMath())
    {
      math = rr->getMath()->deepCopy();
      // TO DO return boolean to check this worked
    }
    else
    {
      math = zeroNode->deepCopy();
    }
    mODEs.push_back(std::make_pair(id, math));
  }
  delete zeroNode;
}

unsigned int
SBMLRateRuleConverter::locateTerm(ASTNode * node)
{
  unsigned int index = 0;
  for (std::vector<ASTNode*>::iterator it = mTerms.begin(); it != mTerms.end(); ++it)
  {
    if (node->exactlyEqual(**it))
    {
      break;
    }
    else
    {
      index++;
    }
  }
  return index;
}

bool
SBMLRateRuleConverter::determineCoefficient(ASTNode* ode, unsigned int termN, double& coeff)
{
  bool found = false;
  coeff = 0;

  ASTNode* term1 = ode->deepCopy();
  // we have decomposed nodes so that if it is times the 
  // first child should be a number
  // take it out of the term
  // it will be used as a coefficient
  if (term1->getType() == AST_TIMES && term1->getNumChildren() > 0)
  {
    if (term1->getChild(0)->isNumber())
    {
      coeff = term1->getChild(0)->getValue();
      term1->removeChild(0, true);
    }
    else
    {
      coeff = 1;
    }
  }
  else if (ode->getType() == AST_PLUS)
  {
    unsigned int i = 0;
    while (!found && i < ode->getNumChildren())
    {
      found = determineCoefficient(ode->getChild(i), termN, coeff);
      i++;
    }
  }
  else if (ode->getType() == AST_MINUS)
  {
    if (ode->getNumChildren() == 1)
    {
      found = determineCoefficient(ode->getChild(0), termN, coeff);
      if (found)
      {
        if (util_isEqual(coeff, 0.0))
        {
          coeff = -1.0;
        }
        else
        {
          coeff *= -1.0;
        }
      }

    }
    else
    {
      found = determineCoefficient(ode->getChild(0), termN, coeff);
      if (!found)
      {
        found = determineCoefficient(ode->getChild(1), termN, coeff);
        if (found)
        {
          if (util_isEqual(coeff, 0.0))
          {
            coeff = -1.0;
          }
          else
          {
            coeff *= -1.0;
          }
        }
      }
    }
  }
  else
  {
    coeff = 1.0;
  }

  if (!found)
  {
    unsigned int index = locateTerm(term1);

    if (index == termN)
    {
      found = true;
    }
    else
    {
      coeff = 0.0;
    }
  }
  delete term1;
  return found;
}


std::vector<double>
SBMLRateRuleConverter::populateCoefficientVector(unsigned int termN)
{
  std::vector<double> coeffs;
  for (unsigned int n = 0; n < mODEs.size(); ++n)
  {
    ASTNode* ode = mODEs.at(n).second;

    double coeff;
    
    determineCoefficient(ode, termN, coeff);

    coeffs.push_back(coeff);
  }
  return coeffs;
}

bool
SBMLRateRuleConverter::determineDerivativeSign(std::string variable, ASTNode* term, bool& posDeriv)
{
  // we need to know whether (d(term[termN])/dvariable) > 0
  // we already know that term is non-decomposable - so it will not be a top-level +/-
  // and any possible +/- have been expanded
  // but the derivative will have an explicit +/- number if it encountered variable

  bool found = false;
  posDeriv = false;
  bool signDetermined = false;

  // if variable is not in term that derivative not > 0
  List* names;
  names = term->getListOfNodes((ASTNodePredicate)ASTNode_isName);
  ListIterator it = names->begin();
  while (!found && it != names->end())
  {
    if (strcmp(variable.c_str(),((ASTNode *)(*it))->getName()) == 0)
    {
      found = true;
    }
    ++it;
  }

  if (!found)
  {
    posDeriv = false;
    signDetermined = true;
  }
  else
  {
    ASTNode *deriv = term->derivative(variable);
    signDetermined = isPositive(deriv, posDeriv);
    if (!signDetermined)
    {
      // TO DO log an error
      //getDocument()->getErrorLog()->add()
    }
    delete deriv;
  }
  delete names;
  return signDetermined;
}

bool
SBMLRateRuleConverter::isPositive(const ASTNode* node, bool& posDeriv)
{
  bool signDetermined = false;

  // node will be refactored so should be able to detect sign from first child
  ASTNodeType_t type = node->getType();

  if (type == AST_REAL)
  {
    if (util_isEqual(node->getValue(), 0.0))
    {
      posDeriv = false;
    }
    else if (node->getValue() > 0)
    {
      if (mDerivSign == POSITIVE_DERIVATIVE) posDeriv = true;
    }
    else
    {
      if (mDerivSign == NEGATIVE_DERIVATIVE) posDeriv = true;
    }
    signDetermined = true;
  }
  else if (type == AST_NAME)
  {
    // variable first always consider >0
    if (mDerivSign == POSITIVE_DERIVATIVE) posDeriv = true;
    else if (mDerivSign == NEGATIVE_DERIVATIVE) posDeriv = false;
    signDetermined = true;

  }

  unsigned int n = 0;
  while (!signDetermined && n < node->getNumChildren())
  {
    signDetermined = isPositive(node->getChild(n), posDeriv);
    n++;
  }

  return signDetermined;
}


std::vector<bool>
SBMLRateRuleConverter::populateDerivativeVector(unsigned int termN)
{
  std::vector<bool> derivatives;
  for (unsigned int n = 0; n < mODEs.size(); ++n)
  {
    std::string variable = mODEs.at(n).first;

    bool posDeriv;

    bool determined = determineDerivativeSign(variable, mTerms.at(termN), posDeriv);
    if (!determined)
    {
      mMathNotSupported = true;
    }

    derivatives.push_back(posDeriv);
  }

  return derivatives;
}


void
SBMLRateRuleConverter::createTerms(ASTNode* node)
{
  if (node->getType() == AST_PLUS || node->getType() == AST_MINUS)
  {
    for (unsigned int i = 0; i < node->getNumChildren(); i++)
    {
      createTerms(node->getChild(i));
    }
  }
  else
  {
    addToTerms(node);
  }

}
void 
SBMLRateRuleConverter::addToTerms(ASTNode* node)
{
  if (node == NULL)
  {
    mMathNotSupported = true;
    return;
  }
  ASTNode* term = node->deepCopy();
  // we have decomposed nodes so that if it is times the 
  // first child should be a number
  // take it out of the term
  // it will be used as a coefficient
  if (term->getType() == AST_TIMES && term->getNumChildren() > 0)
  {
    if (term->getChild(0)->isNumber())
    {
      term->removeChild(0, true);
    }
    // if we are just left with * 1 child remove times
    if (term->getNumChildren() == 1)
    {
      ASTNode* child = term->getChild(0)->deepCopy();
      term = child;
      // if term is +/- then go back to create term and do not process further
      if (term->getType() == AST_PLUS || term->getType() == AST_MINUS)
      {
        createTerms(term);
        delete term;
        return;
      }
    }
  }
  else if (term->isNumber())
  {
    delete term;
    return;
  }

  if (mTerms.size() == 0)
  {
    mTerms.push_back(term);
  }
  else
  {
    bool equivalent = false;
    std::vector<ASTNode*>::iterator it = mTerms.begin();
    while (!equivalent && it != mTerms.end())
    {
      equivalent = term->exactlyEqual(**it);
      it++;
    }
    if (!equivalent)
    {
      mTerms.push_back(term);
    }
    else
    {
      delete term;
    }
  }
}
void 
SBMLRateRuleConverter::populateODEinfo()
{
  Model* model = mDocument->getModel();

  // Fages algo 3.6 create set O
  // create pairs of variables and their corresponding ODE
  // eg. ODEs[0] = [S1, -k1*S1]
  //     ODES[1] = [S2, k1*S1]
  //     ODES[2] = [S3, k2*S3]
  //
  for (unsigned int n = 0; n < model->getNumSpecies(); n++)
  {
    Species *s = model->getSpecies(n);
    if (s->getConstant() == false)
    {
      addODEPair(s->getId(), model);
    }
  }
  for (unsigned int n = 0; n < model->getNumParameters(); n++)
  {
    Parameter *p = model->getParameter(n);
    if (p->getConstant() == false)
    {
      addODEPair(p->getId(), model);
    }
  }

  //for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  //{
  //  cout << mODEs[odeIndex].first << ": " << SBML_formulaToL3String(mODEs[odeIndex].second) << endl;
  //}
  // implement Algo 3.1 here (hidden variables!)
  // check for hidden variables, and add an appropriate ODE if a hidden variable is found
  ExpressionAnalyser *ea = new ExpressionAnalyser(model, mODEs);


  List hiddenSpecies;
  ea->detectHiddenSpecies(&hiddenSpecies);
  // add all hidden species to the model
  for (unsigned int hs=0; hs < hiddenSpecies.getSize(); hs++)
  {
      Parameter* hidden = (Parameter*) hiddenSpecies.get(hs);
      addODEPair(hidden->getId(), model);
  }
  //cout << "After\n";
  //for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  //{
  //  cout << mODEs[odeIndex].first << ": " << SBML_formulaToL3String(mODEs[odeIndex].second) << endl;
  //}

  // Fages algo 3.6 Step 1
  //create set of non decomposable terms used in ODES
  // catch any repeats so a term is only present once but may appear in
  // multiple ODEs; numerical multipliers are ignored
  // ODES above results in terms
  //     mTerms[0] = k1*S1
  //     mTerms[1] = k2*S3

  for (unsigned int n = 0; n < mODEs.size(); n++)
  {
    ASTNode* node = mODEs.at(n).second;
    node->decompose();
    // Fages algo 3.6 Step 2
    createTerms(node);
  }
  //for (unsigned int n = 0; n < mTerms.size(); n++)
  //{
  //  ASTNode* node = mTerms.at(n);
  //  cout << "Term " << n << ": " << SBML_formulaToL3String(node) << endl;
  //}

  // cooefficients
  // these are a set of numerical coefficients of each term as it occurs in each ODE
  // vector < pair < ASTNode*, vector<double> >
  //
  //  mCoefficients[0] = [mTerms[0], [-1, 1, 0]] - coeff of k1*S1 in -k1*S1, k1*S1, k2*S3
  //  mCoefficients[1] = [mTerms[1], [0, 0, 1]] -  coeff of k2*S3 in -k1*S1, k1*S1, k2*S3

  // posDerivative/negDerivative
  // vector < vector<bool> >
  // these are vector of booleans for each term's derivative wrt each variable
  // posDerivative - true if the derivative of positive term will always be > 0
  // negDerivative - true if the derivative of negative term will always be > 0
  //
  // in the example
  // posDerivative = [[true, false, false], [false, false, true]]
  // corresponding to 
  // d(mTerms[0])/dODEs[0].first > 0 ie d(k1S1)/dS1 = k1 > 0 (true
  // d(mTerms[0])/dODEs[1].first > 0 ie d(k1S1)/dS2 = 0 > 0 (false
  // d(mTerms[0])/dODEs[2].first > 0 ie d(k1S1)/dS3 = 0 > 0 (false
  // d(mTerms[1])/dODEs[0].first > 0 ie d(k2S3)/dS1 = 0 > 0 (false
  // d(mTerms[1])/dODEs[1].first > 0 ie d(k2S3)/dS2 = 0 > 0 (false
  // d(mTerms[1])/dODEs[2].first > 0 ie d(k2S3)/dS3 = k2 > 0 (true
  //
  // negDerivative = [[false, false, false], [false, false, false]]
  // corresponding to 
  // d(-1*mTerms[0])/dODEs[0].first > 0 ie d(-k1S1)/dS1 = -k1 > 0 (false
  // d(-1*mTerms[0])/dODEs[1].first > 0 ie d(-k1S1)/dS2 = 0 > 0 (false
  // d(-1*mTerms[0])/dODEs[2].first > 0 ie d(-k1S1)/dS3 = 0 > 0 (false
  // d(-1*mTerms[1])/dODEs[0].first > 0 ie d(-k2S3)/dS1 = 0 > 0 (false
  // d(-1*mTerms[1])/dODEs[1].first > 0 ie d(-k2S3)/dS2 = 0 > 0 (false
  // d(-1*mTerms[1])/dODEs[2].first > 0 ie d(-k2S3)/dS3 = -k2 > 0 (false
  //
  // NOTE: variable values are considered positive
  for (unsigned int n = 0; n < mTerms.size(); n++)
  {
    ASTNode* node = mTerms.at(n);
    std::vector<double> coeffVector = populateCoefficientVector(n);
    mCoefficients.push_back(std::make_pair(node, coeffVector));
    mDerivSign = POSITIVE_DERIVATIVE;
    std::vector<bool> posDerVector = populateDerivativeVector(n);
    mPosDerivative.push_back(posDerVector);
    mDerivSign = NEGATIVE_DERIVATIVE;
    std::vector<bool> negDerVector = populateDerivativeVector(n);
    mNegDerivative.push_back(negDerVector);
  }

}

bool 
SBMLRateRuleConverter::getMathNotSupportedFlag() const
{
  return mMathNotSupported;
}

void
SBMLRateRuleConverter::populateReactionCoefficients()
{
  // Fages algo 3.6 Step 4a
  createInitialValues();
  unsigned int i = 0;
  for (setCoeffIt it = mCoefficients.begin(); it != mCoefficients.end(); ++it)
  {
    // Fages algo 3.6 Step 4b
    analyseCoefficient(it->second, i);
    // Fages algo 3.6 Step 4c
    analysePosDerivative(it->second, i);
    // Fages algo 3.6 Step 4d
    analyseNegDerivative(it->second, i);
    i++;
  }
}

void
SBMLRateRuleConverter::analyseCoefficient(std::vector<double> coeffs, unsigned int index)
{
  for (unsigned int i = 0; i < coeffs.size(); ++i)
  {
    double coeff = coeffs.at(i);
    if (coeff < 0)
    {
      mReactants[index][i] = (-1 * coeff);
    }
    else if (coeff > 0)
    {
      mProducts[index][i] = coeff;
    }
  }
}


void
SBMLRateRuleConverter::analysePosDerivative(std::vector<double> coeffs, unsigned int index)
{
  for (unsigned int i = 0; i < coeffs.size(); ++i)
  {
    if (util_isEqual(mReactants[index][i], 0.0) && mPosDerivative[index][i])
    {
      mReactants[index][i] = 1.0;
      mProducts[index][i] += 1;
    }
  }

}


void
SBMLRateRuleConverter::analyseNegDerivative(std::vector<double> coeffs, unsigned int index)
{
  for (unsigned int i = 0; i < coeffs.size(); ++i)
  {
    if (mNegDerivative[index][i])
    {
      mModifiers[index][i] = 1.0;
    }
  }

}
void
SBMLRateRuleConverter::createInitialValues()
{
  std::vector < double> coeffs(mODEs.size(), 0.0);
  for (unsigned int i = 0; i < mTerms.size(); ++i)
  {
    mReactants.push_back(coeffs);
    mProducts.push_back(coeffs);
    mModifiers.push_back(coeffs);
  }
}

void
SBMLRateRuleConverter::reconstructModel()
{
  dealWithSpecies();
  createReactions();
  removeRules();
}

void
SBMLRateRuleConverter::dealWithSpecies()
{
  // loop through ODE pairs and check that any variables are listed as Species
  for (odeIt it = mODEs.begin(); it != mODEs.end(); ++it)
  {
    const std::string variable = it->first;
    // already a species
    if (mDocument->getModel()->getSpecies(variable) != NULL)
    {
      continue;
    }
    else
    {
      // must be a parameter
      Parameter *p = mDocument->getModel()->removeParameter(variable);
      Species *newSpecies = mDocument->getModel()->createSpecies();
      newSpecies->setId(variable);
      newSpecies->setInitialAmount(p->getValue());
      newSpecies->setHasOnlySubstanceUnits(true);
      newSpecies->setBoundaryCondition(false);
      newSpecies->setConstant(false);
      if (mDocument->getModel()->getCompartment(0) == NULL)
      {
        Compartment * newCompartment = mDocument->getModel()->createCompartment();
        newCompartment->setId("addedCompartment");
        newCompartment->setConstant(true);
        newCompartment->setSize(1.0);
        newSpecies->setCompartment("addedCompartment");
      }
      else
      {
        newSpecies->setCompartment(mDocument->getModel()->getCompartment(0)->getId());
      }
      delete p;
    }
  }
}
void
SBMLRateRuleConverter::createReactions()
{
  unsigned int i = 0;
  char number[4];
  for (setCoeffIt it = mCoefficients.begin(); it != mCoefficients.end(); ++it)
  {
    Reaction *r = mDocument->getModel()->createReaction();
    r->setReversible(false);
    r->setFast(false);
    int id = mDocument->getModel()->getNumReactions();
    sprintf(number, "%u", id);
    const std::string reactionId = "J" + string(number);
    r->setId(reactionId);
    bool itemAdded = false;
    for (unsigned int j = 0; j < mODEs.size(); ++j)
    {
      double stoichiometry = 1.0;
      if (mReactants[i][j] > 0)
      {
        stoichiometry = mReactants[i][j];
        SpeciesReference *sr = r->createReactant();
        sr->setSpecies(mODEs[j].first);
        sr->setStoichiometry(stoichiometry);
        sr->setConstant(true);
        itemAdded = true;
      }
      if (mProducts[i][j] > 0)
      {
        stoichiometry = mProducts[i][j];
        SpeciesReference *sr = r->createProduct();
        sr->setSpecies(mODEs[j].first);
        sr->setStoichiometry(stoichiometry);
        sr->setConstant(true);
        itemAdded = true;
      }
      if (mModifiers[i][j] > 0 && r->getModifier(mODEs[j].first) == NULL)
      {
        ModifierSpeciesReference *sr = r->createModifier();
        sr->setSpecies(mODEs[j].first);
        itemAdded = true;
      }
    }
    if (itemAdded && !r->isSetKineticLaw())
    {
      KineticLaw *kl = r->createKineticLaw();
      kl->setMath(it->first);
    }
      
    // check whetherkinetic law uses a species not listed as p/r/m
    if (r->isSetKineticLaw())
    { 
      List* names = r->getKineticLaw()->getMath()->getListOfNodes((ASTNodePredicate)ASTNode_isName);
      ListIterator it = names->begin();
      while (it != names->end())
      {
        ASTNode* node = (ASTNode*)*it;
        std::string n = node->getName();
        if (mDocument->getModel()->getSpecies(n) != NULL && r->getReactant(n) == NULL && 
          r->getProduct(n) == NULL && r->getModifier(n) == NULL)
        {
          ModifierSpeciesReference *sr = r->createModifier();
          sr->setSpecies(n);
        }
        it++;
      }
    }

    if (!itemAdded)
    {
      delete (mDocument->getModel()->removeReaction(id));
    }
    i++;
  }

}
void
SBMLRateRuleConverter::removeRules()
{
  for (unsigned int j = 0; j < mODEs.size(); ++j)
  {
    Rule *rr = mDocument->getModel()->removeRuleByVariable(mODEs.at(j).first);
    if (rr != NULL)
    {
      delete rr;
    }
  }
}


/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


