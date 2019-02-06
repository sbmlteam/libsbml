/**
 * @file    SBMLRateOfConverter.cpp
 * @brief   Implementation of SBMLRateOfConverter, a converter changing 'rateOf' into functionDefinition
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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


#include <sbml/conversion/SBMLRateOfConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SyntaxChecker.h>
#include <sbml/Model.h>
#include <sbml/AssignmentRule.h>
#include <sbml/RateRule.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/L3FormulaFormatter.h>
#include <sbml/math/L3Parser.h>

#ifdef __cplusplus

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN


/** @cond doxygenLibsbmlInternal */
void SBMLRateOfConverter::init()
{
  SBMLRateOfConverter converter;
  SBMLConverterRegistry::getInstance().addConverter(&converter);
}
/** @endcond */


SBMLRateOfConverter::SBMLRateOfConverter() 
  : SBMLConverter("SBML Rate Of Converter")
  , mOriginalModel (NULL)
{
}


SBMLRateOfConverter::SBMLRateOfConverter(const SBMLRateOfConverter& orig) 
  : SBMLConverter(orig)
  , mOriginalModel     (orig.mOriginalModel)
{
}


  
/*
 * Destroy this object.
 */
SBMLRateOfConverter::~SBMLRateOfConverter ()
{
  if (mOriginalModel != NULL)
    delete mOriginalModel;
  mRateOfMath.clear();
}


SBMLRateOfConverter* 
SBMLRateOfConverter::clone() const
{
  return new SBMLRateOfConverter(*this);
}


ConversionProperties
SBMLRateOfConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  static bool init = false;

  if (init) 
  {
    return prop;
  }
  else
  {
    prop.addOption("replaceRateOf", true,
                   "Replace rateOf with functionDefinition");
    prop.addOption("toFunction", true,
                   "create FunctionDefinition");
    init = true;
    return prop;
  }
}


bool 
SBMLRateOfConverter::matchesProperties(const ConversionProperties &props) const
{
  if (!props.hasOption("replaceRateOf"))
    return false;
  return true;
}


int 
SBMLRateOfConverter::setDocument(const SBMLDocument* doc)
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
SBMLRateOfConverter::setDocument(SBMLDocument* doc)
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


bool 
SBMLRateOfConverter::getToFunctionDefinition()
{
  if (getProperties() == NULL)
  {
    return true;
  }
  else if (getProperties()->hasOption("toFunction") == false)
  {
    return true;
  }
  else
  {
    return getProperties()->getBoolValue("toFunction");
  }
}



int 
SBMLRateOfConverter::convert()
{
  if (mDocument == NULL) return LIBSBML_INVALID_OBJECT;
  if (mOriginalModel == NULL) return LIBSBML_INVALID_OBJECT;

  /// validate doc - and abort if invalid
  if (isDocumentValid() == false) return LIBSBML_CONV_INVALID_SRC_DOCUMENT;

  bool success = false;

  if (getToFunctionDefinition() == true )
  {
    if (!isCSymbolRateOfUsed())
    {
      return LIBSBML_OPERATION_SUCCESS;
    }
    else
    {
      for (std::vector<ASTNode*>::iterator it = mRateOfMath.begin(); 
                                           it != mRateOfMath.end(); ++it)
      {
        ((ASTNode*)(*it))->setType(AST_FUNCTION);
        ((ASTNode*)(*it))->setName("rateOf");
        ((ASTNode*)(*it))->setDefinitionURL("");
      }

      addRateOfFunctionDefinition();

      success = true;
    }
  }
  else
  {
    if (hasFunctionDefinitionForRateOf() && !isFDRateOfUsed())
    {
      return LIBSBML_OPERATION_SUCCESS;
    }
    else
    {
      for (std::vector<ASTNode*>::iterator it = mRateOfMath.begin(); 
                                           it != mRateOfMath.end(); ++it)
      {
        ((ASTNode*)(*it))->setType(AST_FUNCTION_RATE_OF);
        ((ASTNode*)(*it))->setName("rateOf");
      }

      removeRateOfFunctionDefinition();

      success = true;
    }
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


/** @cond doxygenLibsbmlInternal */
bool
SBMLRateOfConverter::isDocumentValid()
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
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool
SBMLRateOfConverter::isCSymbolRateOfUsed()
{
  bool used = false;
  unsigned int i;
  ASTNode * math;

  mRateOfMath.clear();

  // go through any math elements and find if they contain rate of
  for (i = 0; i < mDocument->getModel()->getNumInitialAssignments(); i++)
  {
    if (mDocument->getModel()->getInitialAssignment(i)->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getInitialAssignment(i)->getMath());
      if (usesCSymbolRateOf(math))
      {
        used = true;
      }
    }
  }
  for (i = 0; i < mDocument->getModel()->getNumRules(); i++)
  {
    if (mDocument->getModel()->getRule(i)->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getRule(i)->getMath());
      if (usesCSymbolRateOf(math))
      {
        used = true;
      }
    }
  }
  for (i = 0; i < mDocument->getModel()->getNumConstraints(); i++)
  {
    if (mDocument->getModel()->getConstraint(i)->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getConstraint(i)->getMath());
      if (usesCSymbolRateOf(math))
      {
        used = true;
      }
    }
  }
  for (i = 0; i < mDocument->getModel()->getNumReactions(); i++)
  {
    if (mDocument->getModel()->getReaction(i)->isSetKineticLaw() &&
      mDocument->getModel()->getReaction(i)->getKineticLaw()->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getReaction(i)->getKineticLaw()->getMath());
      if (usesCSymbolRateOf(math))
      {
        used = true;
      }
    }
  }
  for (i = 0; i < mDocument->getModel()->getNumEvents(); i++)
  {
    if (mDocument->getModel()->getEvent(i)->isSetTrigger() &&
      mDocument->getModel()->getEvent(i)->getTrigger()->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getEvent(i)->getTrigger()->getMath());
      if (usesCSymbolRateOf(math))
      {
        used = true;
      }
    }
    if (mDocument->getModel()->getEvent(i)->isSetDelay() &&
      mDocument->getModel()->getEvent(i)->getDelay()->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getEvent(i)->getDelay()->getMath());
      if (usesCSymbolRateOf(math))
      {
        used = true;
      }
    }
    if (mDocument->getModel()->getEvent(i)->isSetPriority() &&
      mDocument->getModel()->getEvent(i)->getPriority()->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getEvent(i)->getPriority()->getMath());
      if (usesCSymbolRateOf(math))
      {
        used = true;
      }
    }
    for (unsigned int j = 0; j < mDocument->getModel()->getEvent(i)->getNumEventAssignments(); j++)
    {
      if (mDocument->getModel()->getEvent(i)->getEventAssignment(j)->isSetMath())
      {
        math = (ASTNode*)(mDocument->getModel()->getEvent(i)->getEventAssignment(j)->getMath());
        if (usesCSymbolRateOf(math))
        {
          used = true;
        }
      }
    }
  }

  return used;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
SBMLRateOfConverter::isCSymbolRateOf(ASTNode* math)
{
  bool used = false;

  if (math->getType() == AST_FUNCTION_RATE_OF) 
  {
    used = true;
    mRateOfMath.push_back(math);
  }

  return used;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
SBMLRateOfConverter::usesCSymbolRateOf(ASTNode* math)
{
  bool logged = isCSymbolRateOf(math);

  for (unsigned int i = 0; i < math->getNumChildren(); i++)
  {
    if (usesCSymbolRateOf(math->getChild(i)))
    { 
      logged = true;
    }
  }

  return logged;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
SBMLRateOfConverter::isFDRateOfUsed()
{
  bool used = false;
  unsigned int i;
  ASTNode * math;

  mRateOfMath.clear();

  // go through any math elements and find if they contain rate of
  for (i = 0; i < mDocument->getModel()->getNumInitialAssignments(); i++)
  {
    if (mDocument->getModel()->getInitialAssignment(i)->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getInitialAssignment(i)->getMath());
      if (usesFDRateOf(math))
      {
        used = true;
      }
    }
  }
  for (i = 0; i < mDocument->getModel()->getNumRules(); i++)
  {
    if (mDocument->getModel()->getRule(i)->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getRule(i)->getMath());
      if (usesFDRateOf(math))
      {
        used = true;
      }
    }
  }
  for (i = 0; i < mDocument->getModel()->getNumConstraints(); i++)
  {
    if (mDocument->getModel()->getConstraint(i)->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getConstraint(i)->getMath());
      if (usesFDRateOf(math))
      {
        used = true;
      }
    }
  }
  for (i = 0; i < mDocument->getModel()->getNumReactions(); i++)
  {
    if (mDocument->getModel()->getReaction(i)->isSetKineticLaw() &&
      mDocument->getModel()->getReaction(i)->getKineticLaw()->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getReaction(i)->getKineticLaw()->getMath());
      if (usesFDRateOf(math))
      {
        used = true;
      }
    }
  }
  for (i = 0; i < mDocument->getModel()->getNumEvents(); i++)
  {
    if (mDocument->getModel()->getEvent(i)->isSetTrigger() &&
      mDocument->getModel()->getEvent(i)->getTrigger()->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getEvent(i)->getTrigger()->getMath());
      if (usesFDRateOf(math))
      {
        used = true;
      }
    }
    if (mDocument->getModel()->getEvent(i)->isSetDelay() &&
      mDocument->getModel()->getEvent(i)->getDelay()->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getEvent(i)->getDelay()->getMath());
      if (usesFDRateOf(math))
      {
        used = true;
      }
    }
    if (mDocument->getModel()->getEvent(i)->isSetPriority() &&
      mDocument->getModel()->getEvent(i)->getPriority()->isSetMath())
    {
      math = (ASTNode*)(mDocument->getModel()->getEvent(i)->getPriority()->getMath());
      if (usesFDRateOf(math))
      {
        used = true;
      }
    }
    for (unsigned int j = 0; j < mDocument->getModel()->getEvent(i)->getNumEventAssignments(); j++)
    {
      if (mDocument->getModel()->getEvent(i)->getEventAssignment(j)->isSetMath())
      {
        math = (ASTNode*)(mDocument->getModel()->getEvent(i)->getEventAssignment(j)->getMath());
        if (usesFDRateOf(math))
        {
          used = true;
        }
      }
    }
  }

  return used;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
SBMLRateOfConverter::isFDRateOf(ASTNode* math)
{
  bool used = false;

  if (math->getType() == AST_FUNCTION && !strcmp(math->getName(), "rateOf")) 
  {
    used = true;
    mRateOfMath.push_back(math);
  }

  return used;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
SBMLRateOfConverter::usesFDRateOf(ASTNode* math)
{
  bool logged = isFDRateOf(math);

  for (unsigned int i = 0; i < math->getNumChildren(); i++)
  {
    if (usesFDRateOf(math->getChild(i)))
    { 
      logged = true;
    }
  }

  return logged;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool
SBMLRateOfConverter::hasFunctionDefinitionForRateOf()
{
  bool functionFound = true;

  FunctionDefinition *fd = mDocument->getModel()->getFunctionDefinition("rateOf");

  if (fd != NULL)
  {
    char * body = SBML_formulaToL3String(fd->getBody());
    if (fd->getMath()->getNumBvars() != 1)
    {
      functionFound = false; 
    }
    else if (strcmp(body, "NaN"))
    {
      functionFound = false; 
    }
    else if (!fd->isSetAnnotation())
    {
      functionFound = false; 
    }
    else
    {
      XMLNode * annot = fd->getAnnotation();
      if (annot->getNumChildren() != 1)
      {
        functionFound = false;
      }
      else
      {
        XMLNode child = annot->getChild(0);
        if (child.getAttributesLength() != 1)
        {
          functionFound = false;
        }
      }
    }
    safe_free(body);
  }
  else
  {
    functionFound = false;
  }

  return functionFound;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
SBMLRateOfConverter::addRateOfFunctionDefinition()
{
  FunctionDefinition * fd = mDocument->getModel()->createFunctionDefinition();
  fd->setId("rateOf");
  std::string math = "lambda(x, notanumber)";
  ASTNode* node = SBML_parseL3Formula(math.c_str());
  fd->setMath(node);
  delete node;

  
  // set the annotation
  // <symbols xmlns="http://sbml.org/annotations/symbols" 
  //          definition="http://en.wikipedia.org/wiki/Derivative"/>
  XMLTriple triple = XMLTriple("symbols", 
    "",
    "");

  XMLAttributes att = XMLAttributes();
  att.add("xmlns", "http://sbml.org/annotations/symbols");
  att.add("definition", "http://en.wikipedia.org/wiki/Derivative");
 
  XMLToken token = XMLToken(triple, att);
  const XMLNode *annot = new XMLNode(token);

  fd->setAnnotation(annot);
  delete annot;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
SBMLRateOfConverter::removeRateOfFunctionDefinition()
{
  FunctionDefinition * fd = mDocument->getModel()->removeFunctionDefinition("rateOf");
  if (mDocument->getModel()->getNumFunctionDefinitions() == 0)
  {
    mDocument->getModel()->getListOfFunctionDefinitions()->setExplicitlyListed(false);
  }
  delete fd;
}
/** @endcond */

/** @cond doxygenCOnly */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


