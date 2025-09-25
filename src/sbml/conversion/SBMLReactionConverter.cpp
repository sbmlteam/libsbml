/**
 * @file    SBMLReactionConverter.cpp
 * @brief   Implementation of SBMLReactionConverter, a converter changing reactions into rate rules
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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


#include <sbml/conversion/SBMLReactionConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SyntaxChecker.h>
#include <sbml/Model.h>
#include <sbml/AssignmentRule.h>
#include <sbml/RateRule.h>
#include <sbml/conversion/ExpressionAnalyser.h>
#include <sbml/conversion/ConversionProperties.h>

#ifdef __cplusplus

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN


/** @cond doxygenLibsbmlInternal */
void SBMLReactionConverter::init()
{
  SBMLReactionConverter converter;
  SBMLConverterRegistry::getInstance().addConverter(&converter);
}
/** @endcond */


SBMLReactionConverter::SBMLReactionConverter() 
  : SBMLConverter("SBML Reaction Converter")
{
  mReactionsToRemove.clear();
  mRateRulesMap.clear();
}


SBMLReactionConverter::SBMLReactionConverter(const SBMLReactionConverter& orig) 
  : SBMLConverter(orig)
  , mReactionsToRemove (orig.mReactionsToRemove)
  , mRateRulesMap      (orig.mRateRulesMap)
{
}


SBMLReactionConverter&
SBMLReactionConverter::operator=(const SBMLReactionConverter& rhs)
{
	if (&rhs != this)
	{
		this->SBMLConverter::operator =(rhs);
	}

	return *this;
}


/*
 * Destroy this object.
 */
SBMLReactionConverter::~SBMLReactionConverter ()
{
}


SBMLReactionConverter* 
SBMLReactionConverter::clone() const
{
  return new SBMLReactionConverter(*this);
}


ConversionProperties
SBMLReactionConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  static bool init = false;

  if (init) 
  {
    return prop;
  }
  else
  {
    prop.addOption("replaceReactions", true,
                   "Replace reactions with rateRules");
    prop.addOption("rateRuleVariablesShouldBeParameters", false,
                    "make any species into parameters");
    init = true;
    return prop;
  }
}


bool 
SBMLReactionConverter::matchesProperties(const ConversionProperties &props) const
{
  if (!props.hasOption("replaceReactions"))
    return false;
  return true;
}

bool
SBMLReactionConverter::getRateRuleVariablesShouldBeParameters() const
{
    bool value = false;
    if (getProperties() == NULL || getProperties()->hasOption("rateRuleVariablesShouldBeParameters") == false)
    {
        return value;
    }
    else
    {
        value = getProperties()->getBoolValue("rateRuleVariablesShouldBeParameters");
    }
    return value;
}

int 
SBMLReactionConverter::setDocument(const SBMLDocument* doc)
{
  if (SBMLConverter::setDocument(doc) == LIBSBML_OPERATION_SUCCESS)
  {
    if (mDocument != NULL)
    {
      mOriginalModel = mDocument->getModel()->clone();
      return LIBSBML_OPERATION_SUCCESS;
    }
    else
    {
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


int 
SBMLReactionConverter::setDocument(SBMLDocument* doc)
{
  if (SBMLConverter::setDocument(doc) == LIBSBML_OPERATION_SUCCESS)
  {
    if (mDocument != NULL)
    {
      mOriginalModel = mDocument->getModel()->clone();
      return LIBSBML_OPERATION_SUCCESS;
    }
    else
    {
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


int 
SBMLReactionConverter::convert()
{
  if (mDocument == NULL) return LIBSBML_INVALID_OBJECT;
  if (mOriginalModel == NULL) return LIBSBML_INVALID_OBJECT;

  /// validate doc - and abort if invalid
  if (isDocumentValid() == false) return LIBSBML_CONV_INVALID_SRC_DOCUMENT;

  // if we have no reactions we are done
  if (mOriginalModel->getNumReactions() == 0)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  bool success = false;

  mReactionsToRemove.clear();

  mRateRulesMap.clear();
  
  // deal with any local parameters that are about to get lost
  ConversionProperties props;
  props.addOption("promoteLocalParameters", true,
                 "Promotes all Local Parameters to Global ones");

  // convert
  int parameterReplaced = mDocument->convert(props);
  if (parameterReplaced != LIBSBML_OPERATION_SUCCESS)
  {
      return parameterReplaced;
  }
   
  // replace any initial assignments with their values
  // this is needed to ensure that the any stoichiometry assigned this way is replaced by value
  ConversionProperties props_ia;
  props_ia.addOption("expandInitialAssignments", true,
      "Expand initial assignments in the model");

  // convert
  int initialAssignmentReplaced = mDocument->convert(props_ia);
  if (initialAssignmentReplaced != LIBSBML_OPERATION_SUCCESS)
  {
    return initialAssignmentReplaced;
  }

  Model * model = mDocument->getModel();

  for (unsigned int react = 0; react < model->getNumReactions(); react++)
  {
    Reaction * rn = model->getReaction(react);
    bool rnReplaced = true;

    // if there is no kineticLaw math skip this reaction
    if (rn->isSetKineticLaw() == false ||
      rn->getKineticLaw()->isSetMath() == false)
    {
      // remove the reaction - since reactants/products may get rules attached
      mReactionsToRemove.append(rn->getId());
      continue;
    }

    for (unsigned int prod = 0; prod < rn->getNumProducts(); prod++)
    {
      const std::string speciesId = rn->getProduct(prod)->getSpecies();
      ASTNode * math = createRateRuleMathForSpecies(speciesId, rn, false);
      if (math != NULL)
      {
          mRateRulesMap.push_back(make_pair(speciesId, math));
      }
      else
      {
        rnReplaced = false;
      }
    }

    for (unsigned int reactant = 0; reactant < rn->getNumReactants(); reactant++)
    {
      const std::string speciesId = rn->getReactant(reactant)->getSpecies();
      ASTNode * math = createRateRuleMathForSpecies(speciesId, rn, true);
      if (math != NULL)
      {
          mRateRulesMap.push_back(make_pair(speciesId, math));
      }
      else
      {
        rnReplaced = false;
      }
    }

    if (rnReplaced == true)
    {
      // add the reaction id to a list to be removed
      mReactionsToRemove.append(rn->getId());
    }

  }

  if (mReactionsToRemove.size() == mOriginalModel->getNumReactions())
  {
    success = replaceReactions();
  }

  if (success && getRateRuleVariablesShouldBeParameters() == true)
  {
      success = createParametersForRateRuleVariables();
  }

  if (success) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    Model * model1 = mDocument->getModel();
    // failed - restore original model
    *model1 = *(mOriginalModel->clone());
    return LIBSBML_OPERATION_FAILED;
  }
}

bool
SBMLReactionConverter::createParametersForRateRuleVariables()
{
    bool created = false;
    Model* model = mDocument->getModel();
    unsigned int numParams = model->getNumParameters();
    unsigned int numSpecies = model->getNumSpecies();
    unsigned int numTotal = numParams + numSpecies;
    // make any species that are now variables into parameters
    for (RuleMap::iterator it = mRateRulesMap.begin(); it != mRateRulesMap.end(); ++it)
    {
        const std::string& id = (*it).first;
        Species* s = model->getSpecies(id);
        if (s != NULL)
        {
            // convert to parameter
            Parameter* p = model->createParameter();
            p->setId(s->getId());
            p->setValue(s->getInitialAmount());
            p->setUnits(s->getUnits());
            p->setConstant(false);
            // remove the species
            model->removeSpecies(s->getId());
        }
    }
    // check we have succeeded
    if (model->getNumParameters() >= numParams && model->getNumSpecies() <= numSpecies &&
        (model->getNumParameters() + model->getNumSpecies()) == numTotal)
    {
        created = true;
    }

    return created;
}


ASTNode *
SBMLReactionConverter::createRateRuleMathForSpecies(const std::string &spId, 
                                               Reaction *rn, bool isReactant)
{
  ASTNode * math = NULL;
  Species* species = mOriginalModel->getSpecies(spId);
  Compartment* compartment = mOriginalModel->getCompartment(species->getCompartment());

  // need to work out stoichiometry, return null if there is none
  ASTNode* stoich = determineStoichiometryNode(isReactant, rn, spId);
  if (stoich == NULL) return NULL;
  
  ASTNode* kineticLawMath = rn->getKineticLaw()->getMath()->deepCopy();
  ASTNode* conc_per_time = NULL;
  bool useCompSize = useCompartmentSize(species, compartment, kineticLawMath);
  if (useCompSize == false)
  {
    conc_per_time = kineticLawMath;
  }
  else
  {
    conc_per_time = new ASTNode(AST_DIVIDE);
    conc_per_time->addChild(kineticLawMath);
    ASTNode * compMath = new ASTNode(AST_NAME);
    compMath->setName(compartment->getId().c_str());
    conc_per_time->addChild(compMath);
  }

  // now we need to multiply the stoichiometry by the rate
  // but we do not need to if the stoichiometry is one
  if (stoich->getType() == AST_REAL && util_isEqual(stoich->getValue(), 1.0))
  {
      delete stoich;
      stoich = NULL;
      return conc_per_time;
  }
  else
  {
      math = new ASTNode(AST_TIMES);
      math->addChild(stoich);
      math->addChild(conc_per_time);
  }

  math = replaceMathWithAssignedVariables(math);

  return math;
}


ASTNode*
SBMLReactionConverter::determineStoichiometryNode(bool isReactant, Reaction* rn,
                                                  const std::string& spId)
{
    ASTNode* stoich = NULL;
    ASTNode* tempNode = NULL; 
    SpeciesReference* sr = isReactant ? rn->getReactant(spId) : rn->getProduct(spId);
    
    if (sr == NULL) return NULL;

  if (sr->isSetStoichiometry() == true)
  {
    double st = sr->getStoichiometry();
    tempNode = new ASTNode(AST_REAL);
    tempNode->setValue(st);
  }
  else
  {
      // set by rule; initial assignment in level 3 or stoichiometry math
    if (sr->isSetId() == true)
    {
      std::string id = sr->getId();
      if (mOriginalModel->getInitialAssignment(id) != NULL)
      {
          // should have been dealt with by removing initial assignments
        tempNode = mOriginalModel->getInitialAssignment(id)->isSetMath() ?
          mOriginalModel->getInitialAssignment(id)->getMath()->deepCopy() : NULL;
      }
      else if (mOriginalModel->getAssignmentRule(id) != NULL)
      {
          // if set by assignment then we are assuming that its a variable stoichiometry
          // create a math node that represents the variable used
          tempNode = new ASTNode(AST_NAME);
          tempNode->setName(id.c_str());

          // but also need a parameter for this variable
          Parameter* p = mDocument->getModel()->createParameter();
          p->setId(id);
          p->setConstant(false);
      }
    }
    else if (sr->isSetStoichiometryMath() == true)
    {
      if (sr->getStoichiometryMath()->isSetMath() == true)
      {
        tempNode = sr->getStoichiometryMath()->getMath()->deepCopy();
      }
    }
  }

  if (tempNode == NULL)
  {
    tempNode = new ASTNode(AST_REAL);
    tempNode->setValue(1.0);
  }

  if (isReactant == true)
  {
    stoich = new ASTNode(AST_MINUS);
    stoich->addChild(tempNode->deepCopy());
  }
  else
  {
      stoich = tempNode->deepCopy();
  }

  delete tempNode;

  return stoich;
}

int
SBMLReactionConverter::createNewRateRule(const std::string &spId, ASTNode *math)
{
  int success = LIBSBML_OPERATION_SUCCESS;
  // if the species is a boundaryConsition we dont create a raterule
  if (mOriginalModel->getSpecies(spId)->getBoundaryCondition() == true)
  {
    return success;
  }

  Model * model = mDocument->getModel();

  if (model->getRateRule(spId) == NULL)
  {
    // create a rate rule for this variable
    RateRule * rr = model->createRateRule();
    
    success = rr->setVariable(spId);

    if (success == LIBSBML_OPERATION_SUCCESS)
    {
      success = rr->setMath(math);
    }
  }
  else
  {
    // we already have a rate rule (species may occur in more than 1 reaction)
    RateRule* rr = model->getRateRule(spId);
    ASTNode * rr_math = const_cast<ASTNode*>(rr->getMath());
    ASTNode * new_math = new ASTNode(AST_PLUS);
    success = new_math->addChild(rr_math->deepCopy());
    if (success == LIBSBML_OPERATION_SUCCESS)
    {
      success = new_math->addChild(math->deepCopy());
    }
    if (success == LIBSBML_OPERATION_SUCCESS)
    {
      success = rr->setMath(new_math);
      delete new_math;
    }
  }

  return success;
}

bool 
SBMLReactionConverter::useCompartmentSize(Species* species, Compartment* compartment, ASTNode* kineticLaw)
{
    bool useCompartmentSize = true;
    if (species->getHasOnlySubstanceUnits() == true)
    {
        useCompartmentSize = false;
    }
    else if (compartment->getSpatialDimensionsAsDouble() == 0.0)
    {
        useCompartmentSize = false;
    }
    else if (util_isEqual(compartment->getSize(), 1.0) &&   
             compartment->getConstant() == true &&
             notUsedInKineticLaw(compartment->getId(), kineticLaw))
    {
        useCompartmentSize = false;
    }
    
    return useCompartmentSize;
}

bool SBMLReactionConverter::notUsedInKineticLaw(const std::string& compartment, ASTNode* kineticLaw)
{
    if (kineticLaw == NULL)
    {
        return false;
    }
    
    if (mathContainsId(kineticLaw, compartment))
    {
        return false;
    }

    return true;
}

//ASTNode* 
//SBMLReactionConverter::replaceMathWithAssignedVariables(ASTNode* original)
//{
//    // there may be bits of the math that are actually assigned with an assignment rule
//    // and therefore should be replaced
//    // e.g. math equals 2 * k1 * A + k2 * B
//    // in a model with an assignment rule k3 = 2 * k1 * A
//    // so the math could become k3 + k2 * B
//    unsigned int numAssignmentRules = 0;
//    IdList assignmentRulesVariables = getListAssignmentRuleVariables(numAssignmentRules);
//    if (numAssignmentRules == 0)
//    {
//        return original;
//    }
//    ExpressionAnalyser analyser;
//    ASTNode* newMath = original->deepCopy();
//    for (unsigned int i = 0; i < numAssignmentRules; i++)
//    {
//        AssignmentRule* ar = mOriginalModel->getAssignmentRule(assignmentRulesVariables.at(i));
//        if (ar != NULL && ar->isSetMath() == true)
//        {
//            ASTNode* arMath = ar->getMath()->deepCopy();
//            ASTNode* variable = new ASTNode(AST_NAME);
//            variable->setName(ar->getVariable().c_str());
//            //cout << "assignment rule " << i << ": " << SBML_formulaToL3String(arMath) << " variable " 
//            //    << SBML_formulaToL3String(variable) << " original " << SBML_formulaToL3String(newMath) << endl;
//
//            analyser.replaceExpressionInNodeWithNode(newMath, arMath, variable);
//            //cout << "afterwards assignment rule " << i << ": " << SBML_formulaToL3String(arMath) << " variable "
//            //    << SBML_formulaToL3String(variable) << " original " << SBML_formulaToL3String(newMath) << endl;
//        }
//    }
//    return newMath;
//}
//
//
//IdList
//SBMLReactionConverter::getListAssignmentRuleVariables(unsigned int &numAssignmentRules)
//{
//    IdList assignmentRuleVariables;
//    unsigned int numRules = mOriginalModel->getNumRules();
//    for (unsigned int i = 0; i < numRules; i++)
//    {
//        Rule* r = mOriginalModel->getRule(i);
//        if (r != NULL && r->isAssignment())
//        {
//            numAssignmentRules++;
//            assignmentRuleVariables.append(r->getVariable());
//        }
//    }
//    return assignmentRuleVariables;
//}

bool
SBMLReactionConverter::replaceReactions()
{
  bool replaced = false;
  int success = LIBSBML_OPERATION_SUCCESS;

  // create the rateRules
  RuleMapIter it;
  for (it = mRateRulesMap.begin(); 
    success == LIBSBML_OPERATION_SUCCESS && it != mRateRulesMap.end(); ++it)
  {
    success = createNewRateRule((*it).first, (*it).second);
  }

  // deallocate memory
  for (it = mRateRulesMap.begin(); it != mRateRulesMap.end(); ++it)
  {
    delete (*it).second;
  }

  if (success != LIBSBML_OPERATION_SUCCESS)
  {
    return replaced;
  }

  Model * model = mDocument->getModel();
  // remove the reactions
  for (unsigned int i = 0; i < mReactionsToRemove.size(); i++)
  {
    delete model->removeReaction(mReactionsToRemove.at((int)i));
  }

  // check we have succeeded
  if (model->getNumReactions() == 0)  replaced = true;

  return replaced;
}


bool
SBMLReactionConverter::isDocumentValid()
{
  bool valid = true;

  unsigned char origValidators = mDocument->getApplicableValidators();
  mDocument->setApplicableValidators(AllChecksON);
  
  // set the flag to ignore flattening when validating
  mDocument->checkConsistency();

  unsigned int errors =  mDocument->getErrorLog()
                      ->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
  
  // reset validators
  mDocument->setApplicableValidators(origValidators);

  if (errors > 0)
  {
    valid = false;
  }

  return valid;
}

/** @cond doxygenCOnly */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


