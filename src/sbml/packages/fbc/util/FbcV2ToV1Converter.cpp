/**
 * @file    FbcV2ToV1Converter.cpp
 * @brief   Implementation of a fbc v2 to fbc v1 converter.
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

#include <sbml/packages/fbc/util/FbcV2ToV1Converter.h>
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

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
 * SBML Converter stuff below
 */
 void FbcV2ToV1Converter::init()
{
  SBMLConverterRegistry::getInstance().addConverter(new FbcV2ToV1Converter());
}
/** @endcond */

FbcV2ToV1Converter::FbcV2ToV1Converter()
  : SBMLConverter("SBML FBC v2 to FBC v1 Converter")
{
}

FbcV2ToV1Converter::FbcV2ToV1Converter(const FbcV2ToV1Converter& orig) :
SBMLConverter(orig)
{
}

FbcV2ToV1Converter*
FbcV2ToV1Converter::clone() const
{
  return new FbcV2ToV1Converter(*this);
}

/*
 * Destroy this object.
 */
FbcV2ToV1Converter::~FbcV2ToV1Converter()
{
}

ConversionProperties
FbcV2ToV1Converter::getDefaultProperties() const
{
  static ConversionProperties prop;
  prop.addOption("convert fbc v2 to fbc v1", true, "convert fbc v2 to fbc v1");
  return prop;
}

bool
FbcV2ToV1Converter::matchesProperties(const ConversionProperties &props) const
{
  if (!props.hasOption("convert fbc v2 to fbc v1"))
    return false;
  return true;
}

void convertReactionsToV1(Model* model, FbcModelPlugin* mplug)
{
  if (model == NULL || mplug == NULL) return;

  for (unsigned int i = 0; i < model->getNumReactions(); ++i)
  {
    Reaction* current = model->getReaction(i);
    if (current == NULL) continue;

    FbcReactionPlugin* rplug = dynamic_cast<FbcReactionPlugin*>(current->getPlugin("fbc"));
    if (rplug == NULL) continue;

    if (rplug->isSetGeneProductAssociation() && 
      rplug->getGeneProductAssociation()->isSetAssociation())
    {      
      string infix = rplug->getGeneProductAssociation()->getAssociation()->toInfix();
      GeneAssociation* ga = mplug->createGeneAssociation();
      ga->setReaction(current->getId());
      ga->setAssociation(Association::parseInfixAssociation(infix));
    }

    if (rplug->isSetLowerFluxBound())
    {
      Parameter* lower = model->getParameter(rplug->getLowerFluxBound());
      if (lower != NULL)
      {
        FluxBound* fb = mplug->createFluxBound();
        fb->setReaction(current->getId());
        fb->setOperation(FLUXBOUND_OPERATION_GREATER_EQUAL);
        fb->setValue(lower->getValue());
      }
    }

    if (rplug->isSetUpperFluxBound())
    {
      Parameter* upper = model->getParameter(rplug->getUpperFluxBound());
      if (upper != NULL)
      {
        FluxBound* fb = mplug->createFluxBound();
        fb->setReaction(current->getId());
        fb->setOperation(FLUXBOUND_OPERATION_LESS_EQUAL);
        fb->setValue(upper->getValue());
      }
    }

    rplug->unsetLowerFluxBound();
    rplug->unsetUpperFluxBound();
    rplug->unsetGeneProductAssociation();
  }

  mplug->getListOfGeneProducts()->clear();

}

int
FbcV2ToV1Converter::convert()
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
    static_cast<FbcModelPlugin*>(mDocument->getModel()->getPlugin("fbc"));

  // if we have don't have a fbc model we cannot do the conversion
  if (plugin == NULL || mDocument->getLevel() != 3)
  {
    return LIBSBML_OPERATION_FAILED;
  }

  // if we already are at version 1 there is nothing to do
  if (plugin->getPackageVersion() == 1)
    return LIBSBML_OPERATION_SUCCESS;

  // collect information
  plugin->setElementNamespace(FbcExtension::getXmlnsL3V1V1());

  // delete exisiting v1 fluxbounds / gene association
  plugin->getListOfGeneAssociations()->clear();
  plugin->getListOfFluxBounds()->clear();

  // unset strict flag
  plugin->unsetStrict();

  convertReactionsToV1(mModel, plugin);

  mDocument->getSBMLNamespaces()->removeNamespace(FbcExtension::getXmlnsL3V1V2());
  mDocument->getSBMLNamespaces()->addPackageNamespace("fbc", 1);


  result = LIBSBML_OPERATION_SUCCESS;
  return result;
}

/** @cond doxygenIgnored */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
