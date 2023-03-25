/**
 * @file    AnnotationToDistribConverter.cpp
 * @brief   Implementation of a first flattening converter.
 * @author  Sarah M Keating
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
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->
 */


#include <sbml/packages/distrib/util/AnnotationToDistribConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/util/MathFilter.h>
#include <sbml/Model.h>

#ifdef USE_COMP
#include <sbml/packages/comp/extension/CompSBMLDocumentPlugin.h>
#endif

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
 * SBML Converter stuff below
 */

void AnnotationToDistribConverter::init()
{
  //'addConverter' adds a clone, not the original.
  AnnotationToDistribConverter dac;
  SBMLConverterRegistry::getInstance().addConverter(&dac);
}
/** @endcond */


AnnotationToDistribConverter::AnnotationToDistribConverter() 
  : SBMLConverter("SBML Distributions Annotations Converter")
  , mKeepFunctions()
{
}


AnnotationToDistribConverter::AnnotationToDistribConverter
                         (const AnnotationToDistribConverter& orig) :
SBMLConverter(orig)
, mKeepFunctions()
{
}

AnnotationToDistribConverter* 
AnnotationToDistribConverter::clone() const
{
  return new AnnotationToDistribConverter(*this);
}


/*
 * Destroy this object.
 */
AnnotationToDistribConverter::~AnnotationToDistribConverter ()
{
}


ConversionProperties
AnnotationToDistribConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  prop.addOption("convert distrib annotations", true, "convert distrib annotations");
  return prop;
}


bool 
AnnotationToDistribConverter::matchesProperties
                        (const ConversionProperties &props) const
{
  if (!props.hasOption("convert distrib annotations"))
    return false;
  return true;
}

int 
AnnotationToDistribConverter::convert()
{  

  int result = performConversion();
  return result;

}

/** @cond doxygenLibsbmlInternal */
int 
AnnotationToDistribConverter::performConversion()
{
  Model* model = mDocument->getModel();
  bool converted = convertModel(model);

#ifdef USE_COMP
  CompSBMLDocumentPlugin* csdp = static_cast<CompSBMLDocumentPlugin*>(mDocument->getPlugin("comp"));
  if (csdp != NULL)
  {
    for (unsigned int md = 0; md < csdp->getNumModelDefinitions(); md++)
    {
      ModelDefinition* modeldef = csdp->getModelDefinition(md);
      if (convertModel(modeldef))
      {
        converted = true;
      }
    }
  }
#endif
  if (!converted)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  int result = LIBSBML_OPERATION_SUCCESS;
  if (mDocument->getLevel() < 3) {
    if (!mDocument->setLevelAndVersion(3, 1, false))
    {
      return LIBSBML_OPERATION_FAILED;
    }
  }

  DistribExtension de;
  string uri = de.getURI(mDocument->getLevel(), mDocument->getVersion(), 1);
  result = mDocument->enablePackage(uri, "distrib", true);
  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    return result;
  }

  return mDocument->setPackageRequired("distrib", true);
}


bool 
AnnotationToDistribConverter::convertModel(Model* model)
{  
  if (model == NULL) 
  {
    return false;
  }
  map<string, ASTNodeType_t> replacements;
  mKeepFunctions.clear();
  for (unsigned int fd = 0; fd < model->getNumFunctionDefinitions(); fd++)
  {
    FunctionDefinition* funcdef = model->getFunctionDefinition(fd);
    XMLNode* annot = funcdef->getAnnotation();
    if (annot==NULL || annot->getNumChildren() == 0)
    {
      continue;
    }
    XMLNode distrib = annot->getChild(0);
    if (distrib.getName() != "distribution")
    {
      continue;
    }
    XMLAttributes attrs = distrib.getAttributes();
    //For some reason, the XMLAttributes object ignores the 'xmlns' attribute, so this doesn't work:
    //int defindex = attrs.getIndex("definition", "http://sbml.org/annotations/distribution");
    int defindex = attrs.getIndex("definition");
    if (defindex == -1)
    {
      continue;
    }
    string value = attrs.getValue(defindex);
    if (value == "http://en.wikipedia.org/wiki/Normal_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_NORMAL;
    }
    else if (value == "http://en.wikipedia.org/wiki/Truncated_normal_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_NORMAL;
    }
    else if (value == "http://en.wikipedia.org/wiki/Uniform_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_UNIFORM;
    }
    else if (value == "http://en.wikipedia.org/wiki/Uniform_distribution_(continuous)")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_UNIFORM;
    }
    else if (value == "http://en.wikipedia.org/wiki/Exponential_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_EXPONENTIAL;
    }
    else if (value == "http://en.wikipedia.org/wiki/Gamma_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_GAMMA;
    }
    else if (value == "http://en.wikipedia.org/wiki/Poisson_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_POISSON;
    }
    else if (value == "http://en.wikipedia.org/wiki/Log-normal_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_LOGNORMAL;
    }
    else if (value == "http://en.wikipedia.org/wiki/Chi-squared_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_CHISQUARE;
    }
    else if (value == "http://en.wikipedia.org/wiki/Laplace_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_LAPLACE;
    }
    else if (value == "http://en.wikipedia.org/wiki/Cauchy_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_CAUCHY;
    }
    else if (value == "http://en.wikipedia.org/wiki/Rayleigh_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_RAYLEIGH;
    }
    else if (value == "http://en.wikipedia.org/wiki/Binomial_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_BINOMIAL;
    }
    else if (value == "http://en.wikipedia.org/wiki/Bernoulli_distribution")
    {
      replacements[funcdef->getId()] = AST_DISTRIB_FUNCTION_BERNOULLI;
    }
  }

  if (replacements.empty())
  {
    return false;
  }
  MathFilter *mfilter = new MathFilter();

  List* allelements = model->getAllElements(mfilter);
  for (unsigned int e = 0; e < allelements->getSize(); e++)
  {
    SBase* element = static_cast<SBase*>(allelements->get(e));
    for (map<string, ASTNodeType_t>::iterator rep = replacements.begin(); rep != replacements.end(); rep++)
    {
      adjustMath(element, rep->first, rep->second);
    }
  }
  delete mfilter;
  delete allelements;

  for (map<string, ASTNodeType_t>::iterator rep = replacements.begin(); rep != replacements.end(); rep++)
  {
    if (mKeepFunctions.find(rep->first) == mKeepFunctions.end())
    {
      model->removeFunctionDefinition(rep->first);
    }
  }

  if (model->getNumFunctionDefinitions() == 0)
  {
    model->getListOfFunctionDefinitions()->setExplicitlyListed(false);
  }
  return true;
}
/** @endcond */

void
AnnotationToDistribConverter::adjustMath(SBase* element, const string& function, ASTNodeType_t type)
{
  const ASTNode* astn = element->getMath();
  if (astn == NULL) {
    return;
  }
  ASTNode newastn(*astn);
  if (replaceAnnotatedFunctionWith(&newastn, function, type))
  {
    element->setMath(&newastn);
  }
}

bool 
AnnotationToDistribConverter::replaceAnnotatedFunctionWith(ASTNode * astn, const std::string & function, ASTNodeType_t type)
{
  bool replaced = false;
  if (astn->getType() == AST_FUNCTION && function == astn->getName())
  {
    if (astn->setType(type) != LIBSBML_OPERATION_SUCCESS || !astn->hasCorrectNumberArguments())
    {
      astn->setType(AST_FUNCTION);
      mKeepFunctions.insert(astn->getName());
    }
    else
    {
      replaced = true;
    }
  }
  for (unsigned int c = 0; c < astn->getNumChildren(); c++)
  {
    if (replaceAnnotatedFunctionWith(astn->getChild(c), function, type))
    {
      replaced = true;
    }
  }
  return replaced;
}



/** @cond doxygenIgnored */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


