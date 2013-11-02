/**
 * @file    CobraToFbcConverter.cpp
 * @brief   Implementation of a cobra 2 fbc converter.
 * @author  Frank T. Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
 * ---------------------------------------------------------------------- -->
 */


#include <sbml/packages/fbc/util/CobraToFbcConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/conversion/ConversionProperties.h>

#include <sbml/common/sbmlfwd.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLReader.h>
#include <sbml/packages/fbc/common/FbcExtensionTypes.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
 * SBML Converter stuff below
 */
void CobraToFbcConverter::init()
{
  SBMLConverterRegistry::getInstance().addConverter(new CobraToFbcConverter());
}
/** @endcond */

CobraToFbcConverter::CobraToFbcConverter() : SBMLConverter()
{

}


CobraToFbcConverter::CobraToFbcConverter(const CobraToFbcConverter& orig) :
  SBMLConverter(orig)
{
}

SBMLConverter* 
  CobraToFbcConverter::clone() const
{
  return new CobraToFbcConverter(*this);
}


ConversionProperties
  CobraToFbcConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  prop.addOption("convert cobra", true, "convert cobra sbml to fbc");
  return prop;
}


bool 
  CobraToFbcConverter::matchesProperties(const ConversionProperties &props) const
{
  if (&props == NULL || !props.hasOption("convert cobra"))
    return false;
  return true;
}

int 
  CobraToFbcConverter::convert()
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
    (FbcModelPlugin*)(mDocument->getModel()->getPlugin("fbc"));

  // if we have a fbc model we are done already
  if (plugin != NULL || mDocument->getLevel() == 3)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  
  std::map<const string, int> chargeMap;
  std::map<const string, string> formulaMap;
  Model* model = mDocument->getModel();

  for (unsigned int i = 0; i < model->getNumSpecies();++i)
  {
    Species* current = model->getSpecies(i);
    if (current->isSetCharge())
    {
      chargeMap[current->getId()] = current->getCharge();
      // need to unset the charge here, as it the call will 
      // not work once this is an L3 model
      current->unsetCharge();
    }
    if (current->isSetNotes())
    {
      string notes = current->getNotesString();
      std::transform(notes.begin(), notes.end(), notes.begin(), ::toupper);
      size_t pos = notes.find("FORMULA:");
      if (pos != string::npos)
      {
        size_t end = notes.find("</", pos+9);
        if (end != string::npos)
        {
          string formula = notes.substr(pos + 9, end-(pos+9));
          if (formula[0] != '<' &&  formula[0] != '/' )
          {
            size_t pos = formula.find_first_not_of(" \n\t\r");
            if (pos != std::string::npos)
            formulaMap[current->getId()] = formula;
          }
        }
      }
    }
  }


  mDocument->setConversionValidators(AllChecksON & UnitsCheckOFF);

  ConversionProperties prop(new SBMLNamespaces(3,1));
  prop.addOption("strict", false, "should validity be preserved");
  prop.addOption("ignorePackages", true, "convert even if packages are used");
  prop.addOption("setLevelAndVersion", true, "convert the document to the given level and version");
  int conversionResult = mDocument->convert(prop);
  if (conversionResult != LIBSBML_OPERATION_SUCCESS)
    return conversionResult;

  
  mDocument->enablePackage("http://www.sbml.org/sbml/level3/version1/fbc/version1", "fbc",true);
  mDocument->setPkgRequired("fbc", false);

  FbcModelPlugin *fbcPlugin = (FbcModelPlugin*)model->getPlugin("fbc");

  Objective* objective = fbcPlugin->createObjective();
  objective->setId("obj");
  objective->setType("maximize");
  fbcPlugin->setActiveObjectiveId("obj");

  for (unsigned int i = 0; i < model->getNumReactions(); ++i)
  {
    Reaction* reaction = model->getReaction(i);
    const std::string rID = reaction->getId();
    KineticLaw* kineticLaw = reaction->getKineticLaw();
    if (kineticLaw == NULL) continue;
    double LB = kineticLaw->getLocalParameter("LOWER_BOUND") != NULL ? kineticLaw->getLocalParameter("LOWER_BOUND")->getValue() : -std::numeric_limits<double>::infinity();
    double UB = kineticLaw->getLocalParameter("UPPER_BOUND") != NULL ? kineticLaw->getLocalParameter("UPPER_BOUND")->getValue() :  std::numeric_limits<double>::infinity();
    double OBJ = kineticLaw->getLocalParameter("OBJECTIVE_COEFFICIENT") != NULL ? kineticLaw->getLocalParameter("OBJECTIVE_COEFFICIENT")->getValue() : 0;
    reaction->unsetKineticLaw();

    if (LB == UB)
    {
      FluxBound* fluxBound = fbcPlugin->createFluxBound();
      fluxBound->setReaction(rID);
      fluxBound->setValue(LB);
      fluxBound->setOperation("equal");
    }
    else
    {
      if (LB > -std::numeric_limits<double>::infinity())
      {
        FluxBound *fluxBound = fbcPlugin->createFluxBound();
        fluxBound->setReaction(rID);
        fluxBound->setValue(LB);
        fluxBound->setOperation("greaterEqual");
      }
      if (UB < std::numeric_limits<double>::infinity())
      {
        FluxBound *fluxBound = fbcPlugin->createFluxBound();
        fluxBound->setReaction(rID);
        fluxBound->setValue(UB);
        fluxBound->setOperation("lessEqual");
      }
    }
    if (OBJ != 0)
    {
      FluxObjective* fluxObjective = objective->createFluxObjective();
      fluxObjective->setReaction(rID);
      fluxObjective->setCoefficient(OBJ);
    }


    if (reaction->isSetNotes())
    {
      string notes = reaction->getNotesString();
      std::transform(notes.begin(), notes.end(), notes.begin(), ::toupper);
      size_t pos = notes.find("ASSOCIATION:");
      if (pos != string::npos)
      {
        size_t end = notes.find("</", pos+12);
        if (end != string::npos)
        {
          string geneAssociation = notes.substr(pos + 12, end-(pos+12));
          Association* association = Association::parseInfixAssociation(geneAssociation);
          if (association != NULL)
          {
            GeneAssociation* ga = fbcPlugin->createGeneAssociation();
            stringstream temp; temp  << "ga_" << (fbcPlugin->getNumGeneAssociations());
            ga->setId(temp.str());
            ga->setReaction(rID);
            ga->setAssociation(association);
            delete association;
          }
        }
      }
    }

  }
  
  // remove objective if we never had an active one
  if (objective->getNumFluxObjectives() == 0)
  {
    objective = fbcPlugin->removeObjective("obj");
    delete objective;
    fbcPlugin->unsetActiveObjectiveId();
  }
  

  model->setTimeUnits("dimensionless");

  while (model->getNumUnitDefinitions() > 0)
  {
    model->removeUnitDefinition(0);
  }

  for (unsigned int i = 0; i < model->getNumCompartments(); ++i)
    model->getCompartment(i)->unsetUnits();

  for (unsigned int i = 0; i < model->getNumSpecies(); ++i)
  {
    Species *current =  model->getSpecies(i);
    current->unsetUnits();
    FbcSpeciesPlugin* splugin = static_cast<FbcSpeciesPlugin*>(current->getPlugin("fbc"));
    int charge = chargeMap[current->getId()];
    if (charge != 0)
      splugin->setCharge(charge);
    splugin->setChemicalFormula(formulaMap[current->getId()]);

  }
  result = LIBSBML_OPERATION_SUCCESS;
  return result;
}


/** @cond doxygenCOnly */


/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


