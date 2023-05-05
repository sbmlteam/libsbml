/**
 * @file    DistribToAnnotationConverter.cpp
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


#include <sbml/packages/distrib/util/DistribToAnnotationConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/extension/DistribASTPlugin.h>
#include <sbml/util/MathFilter.h>
#include <sbml/math/L3Parser.h>
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

void DistribToAnnotationConverter::init()
{
  //'addConverter' adds a clone, not the original.
  DistribToAnnotationConverter dac;
  SBMLConverterRegistry::getInstance().addConverter(&dac);
}
/** @endcond */


DistribToAnnotationConverter::DistribToAnnotationConverter() 
  : SBMLConverter("SBML Distributions Annotations Converter")
  , mCreatedFunctions()
{
}


DistribToAnnotationConverter::DistribToAnnotationConverter
                         (const DistribToAnnotationConverter& orig) 
  : SBMLConverter(orig)
  , mCreatedFunctions()
{
}

DistribToAnnotationConverter* 
DistribToAnnotationConverter::clone() const
{
  return new DistribToAnnotationConverter(*this);
}


/*
 * Destroy this object.
 */
DistribToAnnotationConverter::~DistribToAnnotationConverter ()
{
}


ConversionProperties
DistribToAnnotationConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  prop.addOption("convert distrib to annotations", true, "convert distrib to annotations");
  prop.addOption("writeMeans", false, "Created functions return means of distributions instead of NaN");
  return prop;
}


bool 
DistribToAnnotationConverter::matchesProperties
                        (const ConversionProperties &props) const
{
  if (!props.hasOption("convert distrib to annotations"))
    return false;
  return true;
}

int 
DistribToAnnotationConverter::convert()
{  

  int result = performConversion();
  return result;

}

bool
DistribToAnnotationConverter::getWriteMeans()
{
  if (getProperties() == NULL)
  {
    return false;
  }
  else if (getProperties()->hasOption("writeMeans") == false)
  {
    return false;
  }
  else
  {
    return getProperties()->getBoolValue("writeMeans");
  }
}
/** @cond doxygenLibsbmlInternal */
int 
DistribToAnnotationConverter::performConversion()
{
  if (mDocument == NULL) {
    return LIBSBML_OPERATION_SUCCESS;
  }
  Model* model = mDocument->getModel();
  bool fail = convertModel(model);
  if (fail) {
    return LIBSBML_OPERATION_FAILED;
  }

#ifdef USE_COMP
  CompSBMLDocumentPlugin* csdp = static_cast<CompSBMLDocumentPlugin*>(mDocument->getPlugin("comp"));
  if (csdp != NULL)
  {
    for (unsigned int md = 0; md < csdp->getNumModelDefinitions(); md++)
    {
      ModelDefinition* modeldef = csdp->getModelDefinition(md);
      fail = fail || (convertModel(modeldef));
    }
  }
#endif
  if (!fail)
  {
    mDocument->disablePackage(DistribExtension::getXmlnsL3V1V1(), "distrib");
    return LIBSBML_OPERATION_SUCCESS;
  }
  return LIBSBML_OPERATION_FAILED;
}


string DistribToAnnotationConverter::getUnusedIDFor(ASTNodeType_t type, Model* model)
{
  if (mCreatedFunctions.find(type) != mCreatedFunctions.end()) {
    return mCreatedFunctions.find(type)->second;
  }
  DistribASTPlugin dastp;
  vector<unsigned int> nallowed = dastp.getNumAllowedChildren(type);
  string idbase = dastp.getConstCharFor(type);
  SBase* duplicate = model->getElementBySId(idbase);
  if (duplicate == NULL) {
    return idbase;
  }
  int count = 0;
  stringstream newid;
  while (duplicate != NULL) {
    count++;
    newid << idbase << count;
    duplicate = model->getElementBySId(newid.str());
  }
  return newid.str();
}


bool
DistribToAnnotationConverter::replaceDistribWithFunctionCalls(ASTNode* astn, Model* model)
{
  if (astn == NULL) {
    return false;
  }
  string newid;
  ASTNodeType_t type = astn->getType();
  switch (type) {
  case AST_DISTRIB_FUNCTION_NORMAL:
  case AST_DISTRIB_FUNCTION_UNIFORM:
  case AST_DISTRIB_FUNCTION_BERNOULLI:
  case AST_DISTRIB_FUNCTION_BINOMIAL:
  case AST_DISTRIB_FUNCTION_CAUCHY:
  case AST_DISTRIB_FUNCTION_CHISQUARE:
  case AST_DISTRIB_FUNCTION_EXPONENTIAL:
  case AST_DISTRIB_FUNCTION_GAMMA:
  case AST_DISTRIB_FUNCTION_LAPLACE:
  case AST_DISTRIB_FUNCTION_LOGNORMAL:
  case AST_DISTRIB_FUNCTION_POISSON:
  case AST_DISTRIB_FUNCTION_RAYLEIGH:
    newid = getUnusedIDFor(type, model);
    astn->setType(AST_FUNCTION);
    astn->setName(newid.c_str());
    astn->setDefinitionURL("");
    if (addFunctionDefinitionWith(model, newid, type, astn->getNumChildren())) {
      return true;
    }
    break;
  default:
    break;
  }
  for (unsigned int c = 0; c < astn->getNumChildren(); c++) {
    if (replaceDistribWithFunctionCalls(astn->getChild(c), model)) {
      return true;
    }
  }
  return false;
}

string DistribToAnnotationConverter::getWikipediaURLFor(ASTNodeType_t type)
{
  switch (type) {
  case AST_DISTRIB_FUNCTION_NORMAL:
    return "http://en.wikipedia.org/wiki/Normal_distribution";
    break;
  case AST_DISTRIB_FUNCTION_UNIFORM:
    return "http://en.wikipedia.org/wiki/Uniform_distribution_(continuous)";
  case AST_DISTRIB_FUNCTION_BERNOULLI:
    return "http://en.wikipedia.org/wiki/Bernoulli_distribution";
    break;
  case AST_DISTRIB_FUNCTION_BINOMIAL:
    return "http://en.wikipedia.org/wiki/Binomial_distribution";
    break;
  case AST_DISTRIB_FUNCTION_CAUCHY:
    return "http://en.wikipedia.org/wiki/Cauchy_distribution";
    break;
  case AST_DISTRIB_FUNCTION_CHISQUARE:
    return "http://en.wikipedia.org/wiki/Chi-squared_distribution";
    break;
  case AST_DISTRIB_FUNCTION_EXPONENTIAL:
    return "http://en.wikipedia.org/wiki/Exponential_distribution";
    break;
  case AST_DISTRIB_FUNCTION_GAMMA:
    return "http://en.wikipedia.org/wiki/Gamma_distribution";
    break;
  case AST_DISTRIB_FUNCTION_LAPLACE:
    return "http://en.wikipedia.org/wiki/Laplace_distribution";
    break;
  case AST_DISTRIB_FUNCTION_LOGNORMAL:
    return "http://en.wikipedia.org/wiki/Log-normal_distribution";
    break;
  case AST_DISTRIB_FUNCTION_POISSON:
    return "http://en.wikipedia.org/wiki/Poisson_distribution";
    break;
  case AST_DISTRIB_FUNCTION_RAYLEIGH:
    return "http://en.wikipedia.org/wiki/Rayleigh_distribution";
    break;
  }
  return "";
}

bool
DistribToAnnotationConverter::addFunctionDefinitionWith(Model* model, const string& id, ASTNodeType_t type, unsigned int nargs)
{
  DistribASTPlugin dastp;
  vector<unsigned int> nallowed = dastp.getNumAllowedChildren(type);
  if (nallowed.size() == 0) {
    return true;
  }
  if (nargs != nallowed[0]) {
    //Cannot create an annotation version
    return true;
  }
  if (mCreatedFunctions.find(type) != mCreatedFunctions.end()) {
    return false;
  }
  string args = "";
  string ret = "NaN";
  switch (type) {
  case AST_DISTRIB_FUNCTION_NORMAL:
    args = "mean, stdev";
    ret = "mean";
    break;
  case AST_DISTRIB_FUNCTION_LOGNORMAL:
    args = "mean, stdev";
    ret = "exp(mean + stdev^2/2)";
    break;
  case AST_DISTRIB_FUNCTION_UNIFORM:
    args = "min, max";
    ret = "(min+max)/2";
    break;
  case AST_DISTRIB_FUNCTION_BERNOULLI:
    args = "prob";
    ret = "prob";
    break;
  case AST_DISTRIB_FUNCTION_BINOMIAL:
    args = "nTrials, probabilityOfSuccess";
    ret = "nTrials * probabilityOfSuccess";
    break;
  case AST_DISTRIB_FUNCTION_CAUCHY:
    args = "location, scale";
    ret = "NaN";
    break;
  case AST_DISTRIB_FUNCTION_LAPLACE:
    args = "location, scale";
    ret = "location";
    break;
  case AST_DISTRIB_FUNCTION_CHISQUARE:
    args = "degreesOfFreedom";
    ret = "degreesOfFreedom";
    break;
  case AST_DISTRIB_FUNCTION_EXPONENTIAL:
    args = "rate";
    ret = "rate^-1";
    break;
  case AST_DISTRIB_FUNCTION_POISSON:
    args = "rate";
    ret = "rate";
    break;
  case AST_DISTRIB_FUNCTION_GAMMA:
    args = "shape, scale";
    ret = "shape*scale";
    break;
  case AST_DISTRIB_FUNCTION_RAYLEIGH:
    args = "scale";
    ret = "scale*sqrt(pi/2)";
    break;
  }

  if (!getWriteMeans()) {
    ret = "NaN";
  }

  FunctionDefinition* fd = model->createFunctionDefinition();
  fd->setId(id);
  string lstr = "lambda(" + args + ", " + ret + ")";
  ASTNode* lambda = SBML_parseL3Formula(lstr.c_str());
  fd->setMath(lambda);
  string wikipedia = getWikipediaURLFor(type);
  string annot = "<annotation> <distribution xmlns=\"http://sbml.org/annotations/distribution\" definition=\"" + getWikipediaURLFor(type) + "\" /> </annotation> ";
  fd->setAnnotation(annot);
  mCreatedFunctions.insert(make_pair(type, id));
  return false;
}

bool 
DistribToAnnotationConverter::convertModel(Model* model)
{  
  if (model == NULL) 
  {
    return false;
  }
  Model backup(*model);
  bool ret = false;
  mCreatedFunctions.clear();
  set<ASTNodeType_t> used_distrib_types;
  MathFilter *filter = new MathFilter();
  List* allelements = model->getAllElements(filter);
  for (ListIterator it = allelements->begin(); it != allelements->end(); ++it)
  {
    SBase* obj = static_cast<SBase*>(*it);
    ASTNode* astn = const_cast<ASTNode*>(obj->getMath());
    if (replaceDistribWithFunctionCalls(astn, model)) {
      *model = backup;
      return true;
    }
  }
  return false;

}
/** @endcond */



/** @cond doxygenIgnored */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


