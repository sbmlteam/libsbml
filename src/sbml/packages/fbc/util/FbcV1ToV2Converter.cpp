/**
 * @file    FbcV1ToV2Converter.cpp
 * @brief   Implementation of a fbc v1 to fbc v2 converter.
 * @author  Frank T. Bergmann
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
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->
 */

#include <sbml/packages/fbc/util/FbcV1ToV2Converter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/conversion/ConversionProperties.h>

#include <sbml/common/sbmlfwd.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLReader.h>
#include <sbml/packages/fbc/common/FbcExtensionTypes.h>
#include <sbml/math/FormulaParser.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>
#include <sstream>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
 * SBML Converter stuff below
 */
 void FbcV1ToV2Converter::init()
{
  SBMLConverterRegistry::getInstance().addConverter(new FbcV1ToV2Converter());
}
/** @endcond */

FbcV1ToV2Converter::FbcV1ToV2Converter()
  : SBMLConverter("SBML FBC v1 to FBC v2 Converter")
{
}

FbcV1ToV2Converter::FbcV1ToV2Converter(const FbcV1ToV2Converter& orig) :
SBMLConverter(orig)
{
}

FbcV1ToV2Converter*
FbcV1ToV2Converter::clone() const
{
  return new FbcV1ToV2Converter(*this);
}

/*
 * Destroy this object.
 */
FbcV1ToV2Converter::~FbcV1ToV2Converter()
{
}

ConversionProperties
FbcV1ToV2Converter::getDefaultProperties() const
{
  static ConversionProperties prop;
  prop.addOption("convert fbc v1 to fbc v2", true, "convert fbc v1 to fbc v2");
  prop.addOption("strict", true, "should the model be a strict one (i.e.: all non-specified bounds will be filled)");
  return prop;
}

bool
FbcV1ToV2Converter::matchesProperties(const ConversionProperties &props) const
{
  if (!props.hasOption("convert fbc v1 to fbc v2"))
    return false;
  return true;
}

bool FbcV1ToV2Converter::getStrict()
{
  if (mProps == NULL || !mProps->hasOption("strict")) return true;
  return mProps->getBoolValue("strict");
}

Parameter* getDefaultParameter(Model* model, const std::string& name, double value, Parameter* param)
{
  if (param != NULL) return param;

  Parameter* defaultBound = model->getParameter(name);
  string newId(name);

  if (defaultBound != NULL)
  {
    // need to find a different name, as it already exists;
    int count = 0;
    while (model->getParameter(newId) != NULL)
    {
      stringstream str; str << newId << "_" << ++count;
      newId = str.str();
    }
  }

  defaultBound = model->createParameter();
  defaultBound->setId(newId);
  defaultBound->setConstant(true);
  defaultBound->setSBOTerm(626);
  defaultBound->setValue(value);

  return defaultBound;
}

void updateFluxBounds(Model* model, FbcModelPlugin* mplug)
{
  if (model == NULL || mplug == NULL)
    return;

  if (mplug->getNumFluxBounds() == 0)
    return;

  for (unsigned int i = 0; i < mplug->getNumFluxBounds(); ++i)
  {
    FluxBound* current = mplug->getFluxBound(i);
    FluxBoundOperation_t currentType = current->getFluxBoundOperation();
    if (currentType == FLUXBOUND_OPERATION_UNKNOWN)
      continue;
    if (!current->isSetReaction()) continue;
    Reaction* reaction = model->getReaction(current->getReaction());
    if (reaction == NULL) continue;

    FbcReactionPlugin* rplug = dynamic_cast<FbcReactionPlugin*>(reaction->getPlugin("fbc"));
    if (rplug == NULL) continue;
    rplug->setElementNamespace(FbcExtension::getXmlnsL3V1V2());

    stringstream str; str << "fb_" << reaction->getId()
      << "_" << FluxBoundOperation_toString(current->getFluxBoundOperation());

    Parameter* param = model->getParameter(str.str());
    if (param == NULL)
    {
      param = model->createParameter();
      param->setId(str.str());
      param->setConstant(true);
      param->setSBOTerm(625);
      param->setValue(current->getValue());
    }

    if (currentType == FLUXBOUND_OPERATION_EQUAL ||
      currentType == FLUXBOUND_OPERATION_GREATER ||
      currentType == FLUXBOUND_OPERATION_GREATER_EQUAL)
    {
      rplug->setLowerFluxBound(param->getId());
    }

    if (currentType == FLUXBOUND_OPERATION_EQUAL ||
      currentType == FLUXBOUND_OPERATION_LESS ||
      currentType == FLUXBOUND_OPERATION_LESS_EQUAL)
    {
      rplug->setUpperFluxBound(param->getId());
    }
  }

  // remove the bounds
  mplug->getListOfFluxBounds()->clear();

  if (!mplug->getStrict())
    return;

  // add extra bounds if we need to
  Parameter* defaultLowerBound = NULL;
  Parameter* defaultUpperBound = NULL;
  Parameter* defaultzero = NULL;

  for (unsigned int i = 0; i < model->getNumReactions(); ++i)
  {
    Reaction* reaction = model->getReaction(i);
    if (reaction == NULL) continue;

    FbcReactionPlugin* rplug = dynamic_cast<FbcReactionPlugin*>(reaction->getPlugin("fbc"));
    if (rplug == NULL) continue;
    rplug->setElementNamespace(FbcExtension::getXmlnsL3V1V2());

    if (!rplug->isSetLowerFluxBound())
    {
      if (reaction->getReversible())
      {
        defaultLowerBound =
          getDefaultParameter(model,
          "fb_default_lower",
          -std::numeric_limits<double>::infinity(),
          defaultLowerBound);

        rplug->setLowerFluxBound(defaultLowerBound->getId());
      }
      else
      {
        defaultzero =
          getDefaultParameter(model,
          "fb_default_zero",
          0,
          defaultzero);

        rplug->setLowerFluxBound(defaultzero->getId());
      }
    }

    if (!rplug->isSetUpperFluxBound())
    {
      defaultUpperBound =
        getDefaultParameter(model,
        "default_upper",
        std::numeric_limits<double>::infinity(),
        defaultUpperBound);

      rplug->setUpperFluxBound(defaultUpperBound->getId());
    }
  }
}

void convertReactionsToV2(Model* model, FbcModelPlugin* mplug)
{
  if (model == NULL || mplug == NULL)
    return;

  updateFluxBounds(model, mplug);

  if (mplug->getNumGeneAssociations() == 0)
    return;

  for (int i = 0; i < mplug->getNumGeneAssociations(); ++i)
  {
    GeneAssociation* ga = mplug->getGeneAssociation((unsigned int)i);
    if (ga == NULL || !ga->isSetReaction() || !ga->isSetAssociation())
      continue;

    Reaction* reaction = model->getReaction(ga->getReaction());
    if (reaction == NULL) continue;
    FbcReactionPlugin* rplug = dynamic_cast<FbcReactionPlugin*>(reaction->getPlugin("fbc"));
    if (rplug == NULL) continue;
    rplug->setElementNamespace(FbcExtension::getXmlnsL3V1V2());

    string infix = ga->getAssociation()->toInfix();

    GeneProductAssociation* newGa = rplug->createGeneProductAssociation();
    newGa->setAssociation(infix);
  }

  mplug->getListOfGeneAssociations()->clear();
}

int
FbcV1ToV2Converter::convert()
{
  int result = LIBSBML_OPERATION_FAILED;

  if (mDocument == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  Model* mModel = mDocument->getModel();
  if (mModel == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  FbcModelPlugin *plugin =
    static_cast<FbcModelPlugin*>(mModel->getPlugin("fbc"));

  // if we have don't have a fbc model we cannot do the conversion
  if (plugin == NULL || mDocument->getLevel() != 3)
  {
    return LIBSBML_OPERATION_FAILED;
  }

  // if we are already at version 2, there is nothing to do
  if (plugin->getPackageVersion() == 2)
    return LIBSBML_OPERATION_SUCCESS;

  // collect information

  // update namespace
  plugin->setElementNamespace(FbcExtension::getXmlnsL3V1V2());

/*  FbcModelPlugin* mplug = dynamic_cast<FbcModelPlugin*>(mModel->getPlugin("fbc"));
  if (mplug == NULL)
  {
	  return LIBSBML_OPERATION_FAILED;
  }
  mplug->setElementNamespace(FbcExtension::getXmlnsL3V1V2());
 */
  // the model will be strict, as in v1 nothing else was allowed
  plugin->setStrict(getStrict());

  convertReactionsToV2(mModel, plugin);

  mDocument->getSBMLNamespaces()->removeNamespace(FbcExtension::getXmlnsL3V1V1());
  mDocument->getSBMLNamespaces()->addPackageNamespace("fbc", 2);

  result = LIBSBML_OPERATION_SUCCESS;
  return result;
}

/** @cond doxygenIgnored */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
