
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
  , mODEs (NULL)
  , mTerms (NULL)
  , mCoefficients (NULL)
  , mPosDerivative (NULL)
  , mNegDerivative (NULL)
  , mDerivSign (POSITIVE_DERIVATIVE)
  , mMathNotSupported (false)
  , mProducts (NULL)
  , mReactants (NULL)
  , mModifiers (NULL)
{
}

SBMLRateRuleConverter::SBMLRateRuleConverter(const SBMLRateRuleConverter& orig) :
  SBMLConverter(orig)
  , mODEs(NULL)
  , mTerms(NULL)
  , mCoefficients(NULL)
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
    if (strcmp(variable.c_str(),((ASTNode *)(*it))->getName()))
    {
      found = true;
    }
    ++it;
  }

  if (!found)
  {
    posDeriv = false;
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
  double number = 0;
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

  for (int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  {
    cout << mODEs[odeIndex].first << ": " << SBML_formulaToL3String(mODEs[odeIndex].second) << endl;
  }
  // implement Algo 3.1 here (hidden variables!)
  // check for hidden variables, and add an appropriate ODE if a hidden variable is found
  List hiddenSpecies;
  for(int odeIndex=0; odeIndex < mODEs.size(); odeIndex++)
  {
      std::pair<std::string, ASTNode*> ode = mODEs.at(odeIndex);
      ASTNode* odeRHS = ode.second;
      odeRHS->reduceToBinary();
 //      Step 1: iterative, in-place replacement of any -x+y terms with y-x terms
      reorderMinusXPlusYIteratively(odeRHS, model);

      // Step 2 TODO
      List* operators = odeRHS->getListOfNodes((ASTNodePredicate)ASTNode_isOperator);
      ListIterator it = operators->begin();
      while (it != operators->end())
      {
          ASTNode* currentNode = (ASTNode*)*it;
          if (isKMinusXMinusY(currentNode, model))
          {
            currentNode->printMath();
              // (a) introduce z=k-x-y with dz/dt = -dx/dt-dy/dt (add to list of additional ODEs to add at the end)
              // TODO
              // (b) replace in ALL ODEs (not just current) k-x-y with z (interior loop over mODEs again?)
              // (c) replace in ALL ODEs (not just current) k+v-x-y with v+z
              // (d) replace in ALL ODEs (not just current) k-x+w-y with w+z
          }
          it++;
      }
      addHiddenVariablesForKMinusX(odeRHS, &hiddenSpecies, operators, model);
      // Step 3
      //it = operators->begin();
      //while (it != operators->end())
      //{
      //    ASTNode* currentNode = (ASTNode*)*it;
      //    if (isKMinusX(currentNode, model))
      //    {
      //        // remember x name for later, before we replace the current node
      //        std::string xName = std::string(currentNode->getRightChild()->getName());

      //        // (a)
      //        // introduce z=k-x
      //        Parameter* zParam = model->createParameter();
      //        const std::string zName = "z" + std::to_string(model->getNumParameters());
      //        zParam->setId(zName);
      //        zParam->setConstant(false);
      //        hiddenSpecies.add(zParam);

      //        // replace k - x with z in current ODE
      //        ASTNode* z = new ASTNode(ASTNodeType_t::AST_NAME);
      //        z->setName(zName.c_str());
      //        std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
      //        ASTNode* currentParent = currentParentAndIndex.first;
      //        int index = currentParentAndIndex.second;
      //        if (currentParent != NULL)
      //        {
      //          currentParent->replaceChild(index, z, true);
      //          // intentionally, don't delete z as it's now owned by currentParent!
      //        }

      //        // add raterule defining dz/dt = -dxdt
      //        ASTNode* dxdt = odeRHS->deepCopy();
      //        RateRule* raterule = model->createRateRule();
      //        raterule->setVariable(zName);
      //        ASTNode* math = new ASTNode(ASTNodeType_t::AST_TIMES);
      //        ASTNode* minus1 = new ASTNode(ASTNodeType_t::AST_REAL);
      //        minus1->setValue(-1.0);
      //        math->addChild(minus1);
      //        math->addChild(dxdt);
      //        raterule->setMath(math);
      //        delete math; //its children dxdt and minus1 deleted as part of this.

      //        // TODO
      //        // (b) replace in ALL ODEs (not just current) k-x with z
      //        // (c) replace in ALL ODEs (not just current) k+v-x with v+z
      //    }
      //    it++;
      //}
  }

  // add all hidden species to the model
  for (int hs=0; hs < hiddenSpecies.getSize(); hs++)
  {
      Parameter* hidden = (Parameter*) hiddenSpecies.get(hs);
      addODEPair(hidden->getId(), model);
  }
  cout << "After\n";
  for (int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  {
    cout << mODEs[odeIndex].first << ": " << SBML_formulaToL3String(mODEs[odeIndex].second) << endl;
  }

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

  for (unsigned int n = 0; n < mTerms.size(); n++)
  {
    ASTNode* node = mTerms.at(n);
    cout << "Term " << n << ": " << SBML_formulaToL3String(node) << endl;
  }
}

std::string
SBMLRateRuleConverter::parameterKminusXAlreadyCreated(pairString kx)
{
  std::string renamed = "";
  bool match = false;
  unsigned int i = 0;
  while (!match && i < kMinusX.size())
  {
    if (kMinusX.at(i).first == kx)
    {
      renamed = kMinusX.at(i).second;
      match = true;
    }
    i++;
  }
  return renamed;
}
bool
SBMLRateRuleConverter::addHiddenVariablesForKMinusX(ASTNode* odeRHS, List* hiddenSpecies, 
                                                    List* operators, Model* model)
{
  ListIterator it = operators->begin();
  while (it != operators->end())
  {
    ASTNode* currentNode = (ASTNode*)*it;
    pairString KX = isKMinusX(currentNode, model);
    if (KX.first.empty() == false)
    {
      std::string zName = parameterKminusXAlreadyCreated(KX);
      if (!zName.empty())
      {

        // replace k - x with z in current ODE
        ASTNode* z = new ASTNode(ASTNodeType_t::AST_NAME);
        z->setName(zName.c_str());
        std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
        ASTNode* currentParent = currentParentAndIndex.first;
        int index = currentParentAndIndex.second;
        if (currentParent != NULL)
        {
          currentParent->replaceChild(index, z, true);
          // intentionally, don't delete z as it's now owned by currentParent!
        }

      }
      else
      {
        const std::string zName = "z" + std::to_string(model->getNumParameters());
        kMinusX.push_back(make_pair(KX, zName));

        // replace k - x with z in current ODE
        ASTNode* z = new ASTNode(ASTNodeType_t::AST_NAME);
        z->setName(zName.c_str());
        std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
        ASTNode* currentParent = currentParentAndIndex.first;
        int index = currentParentAndIndex.second;
        if (currentParent != NULL)
        {
          currentParent->replaceChild(index, z, true);
          // intentionally, don't delete z as it's now owned by currentParent!
        }
        // introduce z=k-x
        Parameter* zParam = model->createParameter();
        zParam->setId(zName);
        zParam->setConstant(false);
        hiddenSpecies->add(zParam);

        // add raterule defining dz/dt = -dxdt
        ASTNode* dxdt = odeRHS->deepCopy();
        RateRule* raterule = model->createRateRule();
        raterule->setVariable(zName);
        ASTNode* math = new ASTNode(ASTNodeType_t::AST_TIMES);
        ASTNode* minus1 = new ASTNode(ASTNodeType_t::AST_REAL);
        minus1->setValue(-1.0);
        math->addChild(minus1);
        math->addChild(dxdt);
        raterule->setMath(math);
        delete math; //its children dxdt and minus1 deleted as part of this.
      }
      //  variableAdded = true;
      //}

                   // (c) replace in ALL ODEs (not just current) k+v-x with v+z
    }
    it++;
  }
  return true;
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

// helper function to determine whether a node is the "+" in a -x+y expression, where x,y are variable species.
bool SBMLRateRuleConverter::isMinusXPlusY(ASTNode* node, Model* model)
{
    //if node is not binary, it's not -x+y
    if (node->getNumChildren() != 2)
        return false;

    // if node is not a plus, it's not -x+y
    if (node->getType() != ASTNodeType_t::AST_PLUS)
        return false;

    // if left child is not a minus, it's not -x+y
    ASTNode* leftChild = node->getLeftChild();
    if (leftChild->getType() != ASTNodeType_t::AST_MINUS)
        return false;

    // if right child is not a variable species, it's not -x+y
    ASTNode* rightChild = node->getRightChild();
    if (!isVariableSpeciesOrParameter(rightChild, model))
        return false;

    // if we get to this point, the only thing left to check is 
    // whether the ->left->right grandchild (the x in -x+y) is a variable species.
    ASTNode* grandChild = leftChild->getRightChild();
    return isVariableSpeciesOrParameter(grandChild, model);
}

// helper function to determine whether a node is the second "-" in a k-x-y expression, where x,y are variable species and k is a numerical constant or a parameter
bool SBMLRateRuleConverter::isKMinusXMinusY(ASTNode* node, Model* model)
{
    // if node is not binary, it's not k-x-y
    if (node->getNumChildren() != 2)
        return false;

    // if node is not a minus, it's not the second "-" in k-x-y
    if (node->getType() != ASTNodeType_t::AST_MINUS)
        return false;

    // if right child is not a variable, it's not k-x-y
    ASTNode* rightChild = node->getRightChild();
    if(!isVariableSpeciesOrParameter(rightChild, model))
        return false;

    // if left child is not a minus, it's not k-x-y
    ASTNode* leftChild = node->getLeftChild();
    if (leftChild->getType() != ASTNodeType_t::AST_MINUS)
        return false;

    // if left child is not binary, it's not k-x-y
    if (leftChild->getNumChildren() != 2)
        return false;

    // if we've gotten this far, what's left to check are the children of the left child (k and x)
    ASTNode* k = leftChild->getLeftChild();
    ASTNode* x = leftChild->getRightChild();
    return isNumericalConstantOrConstantParameter(k, model) && isVariableSpeciesOrParameter(x, model);
}

// check whether a node is the minus sign in a k-x expression
pairString SBMLRateRuleConverter::isKMinusX(ASTNode* node, Model* model)
{
    // if node is not binary, it's not k-x
    if (node->getNumChildren() != 2)
      return make_pair("", "");

    // if node is not a minus, it's not "-" in k-x
    if (node->getType() != ASTNodeType_t::AST_MINUS)
      return make_pair("", "");

    // if right child is not a variable species, it's not k-x
    ASTNode* rightChild = node->getRightChild();
    if (!isVariableSpeciesOrParameter(rightChild, model))
      return make_pair("", "");

    // if left child is not a numerical constant or a parameter, it's not k-x - else it is!
    ASTNode* leftChild = node->getLeftChild();
    if (isNumericalConstantOrConstantParameter(leftChild, model))
      return make_pair(leftChild->getName(), rightChild->getName());
    else
      return make_pair("", "");
}

bool SBMLRateRuleConverter::isVariableSpeciesOrParameter(ASTNode* node, Model* model)
{
    if (!node->isName()) // some nodes, like * operators, don't seem to have a name in the first place
        return false;
    Species* species = model->getSpecies(node->getName());
    Parameter* parameter = model->getParameter(node->getName()); // some species in rate rules may be defined as variable parameters
    bool isVariableSpeciesOrParameter = (species != NULL && !species->getConstant());
    bool isVariableParameter = (parameter!=NULL && !parameter->getConstant());
    return isVariableSpeciesOrParameter || isVariableParameter;
}

bool SBMLRateRuleConverter::isNumericalConstantOrConstantParameter(ASTNode* node, Model* model)
{
    if (!node->isName()) // some nodes, like * operators, don't seem to have a name in the first place
        return false;
    Parameter* parameter = model->getParameter(node->getName());
    bool isConstantParameter = (parameter != NULL) && (parameter->getConstant());
    bool isNumericalConstant = node->isNumber() && node->isConstant();
    return isNumericalConstant || isConstantParameter;
}

void SBMLRateRuleConverter::reorderMinusXPlusYIteratively(ASTNode* odeRHS, Model* model)
{
    int numMinusXPlusY = INT16_MAX;
    while (numMinusXPlusY != 0)
    {
        // since check is centred on a node that is + or -, we can iterate over operator nodes in tree
        numMinusXPlusY = 0;
        List* operators = odeRHS->getListOfNodes((ASTNodePredicate)ASTNode_isOperator);
        ListIterator it = operators->begin();
        while (it != operators->end())
        {
            ASTNode* currentNode = (ASTNode*)*it;
            if (isMinusXPlusY(currentNode, model))
            {
                //Swap -x+y node for y-x node
                ASTNode* yMinusX = new ASTNode(ASTNodeType_t::AST_MINUS);
                std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
                ASTNode* currentParent = currentParentAndIndex.first;
                int index = currentParentAndIndex.second;
                currentParent->replaceChild(index, yMinusX, true);
                numMinusXPlusY++;
            }
            it++;
        }
        delete operators;
    }
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
  for (setCoeffIt it = mCoefficients.begin(); it != mCoefficients.end(); ++it)
  {
    Reaction *r = mDocument->getModel()->createReaction();
    r->setReversible(false);
    r->setFast(false);
    int id = mDocument->getModel()->getNumReactions();
    const std::string reactionId = "J" + std::to_string(id);
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
        
      cout << r->getNumProducts() << " prods\n";
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


std::pair<ASTNode*, int> SBMLRateRuleConverter::getParentNode(ASTNode* child, ASTNode* root)
{
    for (int i = 0; i < root->getNumChildren(); i++)
    {
        if (root->getChild(i) == child)
        {
            return std::pair<ASTNode*, int>(root, i);
        }
    }
    for (int i = 0; i < root->getNumChildren(); i++)
    {
        return getParentNode(child, root->getChild(i));
    }
    return std::pair<ASTNode*, int>(NULL, NAN);
}

/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


