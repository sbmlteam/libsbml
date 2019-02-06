/**
 * @file    FbcExtension.cpp
 * @brief   Implementation of FbcExtension, the core module of fbc package.
 * @author  Akiya Jouraku
 * @author  Frank Bergmann
 *
 *<!---------------------------------------------------------------------------
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
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */


#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/packages/fbc/extension/FbcSBMLDocumentPlugin.h>
#include <sbml/conversion/SBMLConverterRegistry.h>

#include <sbml/packages/fbc/util/CobraToFbcConverter.h>
#include <sbml/packages/fbc/util/FbcToCobraConverter.h>
#include <sbml/packages/fbc/util/FbcV1ToV2Converter.h>
#include <sbml/packages/fbc/util/FbcV2ToV1Converter.h>

#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/extension/FbcModelPlugin.h>
#include <sbml/packages/fbc/extension/FbcSpeciesPlugin.h>
#include <sbml/packages/fbc/extension/FbcReactionPlugin.h>
#include <sbml/packages/fbc/extension/FbcSBMLDocumentPlugin.h>
#include <sbml/packages/fbc/validator/FbcSBMLErrorTable.h>


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
FbcExtension::getPackageName ()
{
  static const std::string pkgName = "fbc";
  return pkgName;
}


/*
 * Returns the default SBML Level this extension.
 */
unsigned int
FbcExtension::getDefaultLevel ()
{
  return 3;
}


/*
 * Returns the default SBML Version this extension.
 */
unsigned int
FbcExtension::getDefaultVersion ()
{
  return 1;
}


/*
 * Returns the default SBML version this extension.
 */
unsigned int
FbcExtension::getDefaultPackageVersion ()
{
  return 1;
}


/*
 * XML namespaces of package.
 */
const std::string&
FbcExtension::getXmlnsL3V1V2 ()
{
  static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/fbc/version2";
  return xmlns;
}

/*
 * XML namespaces of package.
 */
const std::string&
FbcExtension::getXmlnsL3V1V1 ()
{
  static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/fbc/version1";
  return xmlns;
}


/*
 * Adds this FbcExtension object to the SBMLExtensionRegistry class.
 * FbcExtension::init function is automatically invoked when this
 * object is instantiated
 */
static SBMLExtensionRegister<FbcExtension> fbcExtensionRegistry;


static
const char * SBML_FBC_TYPECODE_STRINGS[] = 
{
    "Association"
  , "FluxBound"
  , "FluxObjective"
  , "GeneAssociation"
  , "Objective"
  , "FbcAssociation"
  , "GeneProductAssociation"
  , "GeneProduct"
  , "GeneProductRef"
  , "FbcAnd"
  , "FbcOr"
};


/*
 * Instantiate SBMLExtensionNamespaces<FbcExtension>
 * (FbcPkgNamespaces) for DLL.
 */
template class LIBSBML_EXTERN  SBMLExtensionNamespaces<FbcExtension>;


/*------------------ (END) ----------------------------------*/

/*
 * Constructor
 */
FbcExtension::FbcExtension()
{
}


/*
 * Copy constructor
 */
FbcExtension::FbcExtension(const FbcExtension& orig) :
   SBMLExtension(orig)
{
}


/*
 * Assignment operator
 */
FbcExtension&
FbcExtension::operator=(const FbcExtension& rhs)
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
FbcExtension*
FbcExtension::clone () const
 {
  return new FbcExtension(*this);
}


/*
 * Destructor
 */
FbcExtension::~FbcExtension()
 {
}


/*
 * Returns the name of this package
 */
const std::string&
FbcExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns the URI (namespace) of the package
 */
const std::string&
FbcExtension::getURI(unsigned int sbmlLevel,
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
      if (pkgVersion == 2)
      {
        return getXmlnsL3V1V2();
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
FbcExtension::getLevel(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1() || uri == getXmlnsL3V1V2())
  {
    return 3;
  }

  return 0;
}


/*
 * Returns the SBML version with the given URI of this package.
 */
unsigned int
FbcExtension::getVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1() || uri == getXmlnsL3V1V2())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns the package version with the given URI of this package.
 */
unsigned int
FbcExtension::getPackageVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }
  if (uri == getXmlnsL3V1V2())
  {
    return 2;
  }
  return 0;
}


/*
 * Returns an SBMLExtensionNamespaces<FbcExtension> object 
 */
SBMLNamespaces*
FbcExtension::getSBMLExtensionNamespaces(const std::string &uri) const
{
  FbcPkgNamespaces* pkgns = NULL;
  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new FbcPkgNamespaces(3, 1, 1);
  }
  if (uri == getXmlnsL3V1V2())
  {
    pkgns = new FbcPkgNamespaces(3, 1, 2);
  }

  return pkgns;
}


/** @cond doxygenLibsbmlInternal */
bool
FbcExtension::hasMutiplePackageVersions() const
{
  return true;
}
/** @endcond */



/*
 * This method takes a type code from the Fbc package and returns a string representing 
 */
const char*
FbcExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_FBC_V1ASSOCIATION;
  int max = SBML_FBC_OR;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Fbc Type)";
  }

  return SBML_FBC_TYPECODE_STRINGS[typeCode - min];
}


/** @cond doxygenLibsbmlInternal */
/*
 * Initialization function of fbc extension module which is automatically invoked
 * by SBMLExtensionRegister class before main() function invoked. 
 */
void
FbcExtension::init()
{
  //----------------------------------------------------------------
  //
  // 1. Check if the fbc package has already been registered
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

  FbcExtension fbcExtension;

  //----------------------------------------------------------------
  //
  // 3. Creates the SBasePlugins required by this package
  //
  //----------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());
  packageURIs.push_back(getXmlnsL3V1V2());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint modelExtPoint("core", SBML_MODEL);
  SBaseExtensionPoint speciesExtPoint("core", SBML_SPECIES);
  SBaseExtensionPoint reactionExtPoint("core", SBML_REACTION);

  SBasePluginCreator<FbcSBMLDocumentPlugin, FbcExtension> sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<FbcModelPlugin, FbcExtension> modelPluginCreator(modelExtPoint, packageURIs);
  SBasePluginCreator<FbcSpeciesPlugin, FbcExtension> speciesPluginCreator(speciesExtPoint, packageURIs);
  SBasePluginCreator<FbcReactionPlugin, FbcExtension> reactionPluginCreator(reactionExtPoint, packageURIs);

  //----------------------------------------------------------------
  //
  // 4. Adds the creator objects to the extension
  //
  //----------------------------------------------------------------

  fbcExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  fbcExtension.addSBasePluginCreator(&modelPluginCreator);
  fbcExtension.addSBasePluginCreator(&speciesPluginCreator);
  fbcExtension.addSBasePluginCreator(&reactionPluginCreator);

  //----------------------------------------------------------------
  //
  // 5. Register the object with the registry
  //
  //----------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&fbcExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] FbcExtension::init() failed." << std::endl;
  }

  // 5. Register the cobra converter

  CobraToFbcConverter c1;
  SBMLConverterRegistry::getInstance().addConverter(&c1);
  FbcToCobraConverter c2;
  SBMLConverterRegistry::getInstance().addConverter(&c2);
  FbcV1ToV2Converter c3;
  SBMLConverterRegistry::getInstance().addConverter(&c3);
  FbcV2ToV1Converter c4;
  SBMLConverterRegistry::getInstance().addConverter(&c4);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Return error table entry.
 */
packageErrorTableEntryV2
FbcExtension::getErrorTableV2(unsigned int index) const
{
  return fbcErrorTableV2[index];
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Return error table index for this id.
 */
unsigned int
FbcExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize = sizeof(fbcErrorTableV2)/sizeof(fbcErrorTableV2[0]);
  unsigned int index = 0;

  for(unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == fbcErrorTableV2[i].code)
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
FbcExtension::getErrorIdOffset() const
{
  return 2000000;
}

/** @endcond */




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


