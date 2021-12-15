/**
 * @file:   MultiExtension.cpp
 * @brief:  Implementation of the MultiExtension class
 * @author: SBMLTeam
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


#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPlugin.h>


#include <sbml/packages/multi/extension/MultiExtension.h>
#include <sbml/packages/multi/extension/MultiModelPlugin.h>
#include <sbml/packages/multi/extension/MultiCompartmentPlugin.h>
#include <sbml/packages/multi/extension/MultiSpeciesPlugin.h>
#include <sbml/packages/multi/extension/MultiSimpleSpeciesReferencePlugin.h>
#include <sbml/packages/multi/extension/MultiSpeciesReferencePlugin.h>
#include <sbml/packages/multi/extension/MultiSBMLDocumentPlugin.h>
#include <sbml/packages/multi/validator/MultiSBMLErrorTable.h>
#include <sbml/packages/multi/extension/MultiASTPlugin.h>
#include <sbml/packages/multi/extension/MultiListOfReactionsPlugin.h>


#ifdef __cplusplus


#include <iostream>


LIBSBML_CPP_NAMESPACE_BEGIN


/*---------------------------------------------------------------
 *
 * This block is global initialization code which should be automatically
 * executed before invoking main() block.
 *
 */

/*------------------ (START) ----------------------------------*/

/*
 * Returns the package name of this extension.
 */
const std::string&
MultiExtension::getPackageName ()
{
  static const std::string pkgName = "multi";
  return pkgName;
}


/*
 * Returns the default SBML Level this extension.
 */
unsigned int
MultiExtension::getDefaultLevel ()
{
  return 3;
}


/*
 * Returns the default SBML Version this extension.
 */
unsigned int
MultiExtension::getDefaultVersion ()
{
  return 1;
}


/*
 * Returns the default SBML version this extension.
 */
unsigned int
MultiExtension::getDefaultPackageVersion ()
{
  return 1;
}


/*
 * XML namespaces of package.
 */
const std::string&
MultiExtension::getXmlnsL3V1V1 ()
{
  static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/multi/version1";
  return xmlns;
}


/*
 * Adds this MultiExtension object to the SBMLExtensionRegistry class.
 * MultiExtension::init function is automatically invoked when this
 * object is instantiated
 */
static SBMLExtensionRegister<MultiExtension> multiExtensionRegistry;


static
const char * SBML_MULTI_TYPECODE_STRINGS[] = 
{
    "PossibleSpeciesFeatureValue"
  , "SpeciesFeatureValue"
  , "CompartmentReference"
  , "SpeciesTypeInstance"
  , "InSpeciesTypeBond"
  , "OutwardBindingSite"
  , "SpeciesFeatureType"
  , "SpeciesTypeComponentIndex"
  , "SpeciesFeature"
  , "SpeciesTypeComponentMapInProduct"
  , "MultiSpeciesType"
  , "BindingSiteSpeciesType"
  , "IntraSpeciesReaction"
  , "SubListOfSpeciesFeatures"
};


/*
 * Instantiate SBMLExtensionNamespaces<MultiExtension>
 * (MultiPkgNamespaces) for DLL.
 */
template class LIBSBML_EXTERN  SBMLExtensionNamespaces<MultiExtension>;


/*------------------ (END) ----------------------------------*/

/*
 * Constructor
 */
MultiExtension::MultiExtension()
{
}


/*
 * Copy constructor
 */
MultiExtension::MultiExtension(const MultiExtension& orig) :
   SBMLExtension(orig)
{
}


/*
 * Assignment operator
 */
MultiExtension&
MultiExtension::operator=(const MultiExtension& rhs)
 {
  if (&rhs != this)
  {
    SBMLExtension::operator=(rhs);
  }
  return *this;
}


/*
 * Clone
 */
MultiExtension*
MultiExtension::clone () const
 {
  return new MultiExtension(*this);
}


/*
 * Destructor
 */
MultiExtension::~MultiExtension()
 {
}


/*
 * Returns the name of this package
 */
const std::string&
MultiExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns the URI (namespace) of the package
 */
const std::string&
MultiExtension::getURI(unsigned int sbmlLevel,
                                  unsigned int sbmlVersion,
                                  unsigned int pkgVersion) const
{
  if (sbmlLevel == 3)
  {
    if (sbmlVersion == 1 || sbmlVersion == 2)
    {
      if (pkgVersion == 1)
      {
        return getXmlnsL3V1V1();
      }
    }
  }

  static std::string empty = "";

  return empty;
}


/*
 * Returns the SBML level with the given URI of this package.
 */
unsigned int
MultiExtension::getLevel(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 3;
  }

  return 0;
}


/*
 * Returns the SBML version with the given URI of this package.
 */
unsigned int
MultiExtension::getVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns the package version with the given URI of this package.
 */
unsigned int
MultiExtension::getPackageVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns an SBMLExtensionNamespaces<MultiExtension> object 
 */
SBMLNamespaces*
MultiExtension::getSBMLExtensionNamespaces(const std::string& uri) const
{
  MultiPkgNamespaces* pkgns = NULL;
  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new MultiPkgNamespaces(3, 1, 1);
  }

  return pkgns;
}


/*
 * This method takes a type code from the Multi package and returns a string representing 
 */
const char*
MultiExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE;
  int max = SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Multi Type)";
  }

  return SBML_MULTI_TYPECODE_STRINGS[typeCode - min];
}


/** @cond doxygenLibsbmlInternal */
/*
 * Initialization function of multi extension module which is automatically invoked
 * by SBMLExtensionRegister class before main() function invoked. 
 */
void
MultiExtension::init()
{
  //----------------------------------------------------------------
  //
  // 1. Check if the multi package has already been registered
  //
  //----------------------------------------------------------------

  if (SBMLExtensionRegistry::getInstance().isRegistered(getPackageName()))
  {
    // do nothing
     return;
  }

  //----------------------------------------------------------------
  //
  // 2. Creates an SBMLExtension derived object
  //
  //----------------------------------------------------------------

  MultiExtension multiExtension;

  //----------------------------------------------------------------
  //
  // 3. Creates the SBasePlugins required by this package
  //
  //----------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint modelExtPoint("core", SBML_MODEL);
  SBaseExtensionPoint compartmentExtPoint("core", SBML_COMPARTMENT);
  SBaseExtensionPoint speciesExtPoint("core", SBML_SPECIES);
  SBaseExtensionPoint simplespeciesreferenceExtPoint("core", SBML_MODIFIER_SPECIES_REFERENCE);
  SBaseExtensionPoint speciesreferenceExtPoint("core", SBML_SPECIES_REFERENCE);
  SBaseExtensionPoint listOfReactionsExtPoint("core", SBML_LIST_OF, "listOfReactions", true);

  SBasePluginCreator<MultiSBMLDocumentPlugin, MultiExtension> sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<MultiModelPlugin, MultiExtension> modelPluginCreator(modelExtPoint, packageURIs);
  SBasePluginCreator<MultiCompartmentPlugin, MultiExtension> compartmentPluginCreator(compartmentExtPoint, packageURIs);
  SBasePluginCreator<MultiSpeciesPlugin, MultiExtension> speciesPluginCreator(speciesExtPoint, packageURIs);
  SBasePluginCreator<MultiSimpleSpeciesReferencePlugin, MultiExtension> simplespeciesreferencePluginCreator(simplespeciesreferenceExtPoint, packageURIs);
  SBasePluginCreator<MultiSpeciesReferencePlugin, MultiExtension> speciesreferencePluginCreator(speciesreferenceExtPoint, packageURIs);
  SBasePluginCreator<MultiListOfReactionsPlugin, MultiExtension> listOfReactionsPluginCreator(listOfReactionsExtPoint, packageURIs);

  //----------------------------------------------------------------
  //
  // 4. Adds the creator objects to the extension
  //
  //----------------------------------------------------------------

  multiExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  multiExtension.addSBasePluginCreator(&modelPluginCreator);
  multiExtension.addSBasePluginCreator(&compartmentPluginCreator);
  multiExtension.addSBasePluginCreator(&speciesPluginCreator);
  multiExtension.addSBasePluginCreator(&simplespeciesreferencePluginCreator);
  multiExtension.addSBasePluginCreator(&speciesreferencePluginCreator);
  multiExtension.addSBasePluginCreator(&listOfReactionsPluginCreator);
  MultiASTPlugin multi(getXmlnsL3V1V1());
  multiExtension.setASTBasePlugin(&multi);

  //----------------------------------------------------------------
  //
  // 5. Register the object with the registry
  //
  //----------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&multiExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] MultiExtension::init() failed." << std::endl;
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Return error table entry. 
 */
packageErrorTableEntry
MultiExtension::getErrorTable(unsigned int index) const
{
  return multiErrorTable[index];
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Return error table index for this id. 
 */
unsigned int
MultiExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize = sizeof(multiErrorTable)/sizeof(multiErrorTable[0]);
  unsigned int index = 0;

  for(unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == multiErrorTable[i].code)
    {
      index = i;
      break;
    }

  }

  return index;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Return error offset. 
 */
unsigned int
MultiExtension::getErrorIdOffset() const
{
  return 7000000;
}
/** @endcond */




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


