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
 *------------------------------------------------------------------------- -->
 */


#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/extension/FbcSBMLDocumentPlugin.h>
#include <sbml/packages/fbc/validator/FbcSBMLErrorTable.h>
#include <sbml/conversion/SBMLConverterRegistry.h>

#include <sbml/packages/fbc/util/CobraToFbcConverter.h>
#include <sbml/packages/fbc/util/FbcToCobraConverter.h>
#include <sbml/packages/fbc/util/FbcV1ToV2Converter.h>
#include <sbml/packages/fbc/util/FbcV2ToV1Converter.h>

#include <sbml/packages/fbc/extension/FbcModelPlugin.h>
#include <sbml/packages/fbc/extension/FbcSpeciesPlugin.h>
#include <sbml/packages/fbc/extension/FbcReactionPlugin.h>
#include <sbml/packages/fbc/extension/FbcSBasePlugin.h>


#ifdef __cplusplus


#include <iostream>
#include <string>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

static const packageErrorTableEntryV3 defaultErrorTableV3[] =
{
  // 10304
  { 0,
  "",
  0,
  LIBSBML_SEV_ERROR,
  LIBSBML_SEV_ERROR,
  LIBSBML_SEV_ERROR,
  "",
  { "",
  "",
  ""
  }
  }
};


/*---------------------------------------------------------------
 *
 * This block is global initialization code which should be automatically
 * executed before invoking main() block.
 *
 */

/*------------------ (START) ----------------------------------*/

/*
 * Returns the nickname of the SBML Level&nbsp;3 package implemented by this
 * libSBML extension.
 */
const std::string&
FbcExtension::getPackageName ()
{
  static const std::string pkgName = "fbc";
  return pkgName;
}


/*
 * Returns the default SBML Level implemented by this libSBML extension.
 */
unsigned int
FbcExtension::getDefaultLevel ()
{
  return 3;
}


/*
 * Returns the default SBML Version implemented by this libSBML extension.
 */
unsigned int
FbcExtension::getDefaultVersion ()
{
  return 1;
}


/*
 * Returns the default version of the SBML Level&nbsp;3 package implemented by
 * this libSBML extension.
 */
unsigned int
FbcExtension::getDefaultPackageVersion ()
{
  return 1;
}


/*
 * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
 * by this libSBML extension.
 */
const std::string&
FbcExtension::getXmlnsL3V1V1()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version1/fbc/version1";
  return xmlns;
}


/*
 * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
 * by this libSBML extension.
 */
const std::string&
FbcExtension::getXmlnsL3V1V2()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version1/fbc/version2";
  return xmlns;
}


/*
 * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
 * by this libSBML extension.
 */
const std::string&
FbcExtension::getXmlnsL3V1V3()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version1/fbc/version3";
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
  , "UserDefinedConstraintComponent"
  , "UserDefinedConstraint"
  , "KeyValuePair"
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
 * Assignment operator for FbcExtension.
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
 * Creates and returns a deep copy of this FbcExtension object.
 */
FbcExtension*
FbcExtension::clone () const
 {
  return new FbcExtension(*this);
}


/*
 * Destructor for FbcExtension.
 */
FbcExtension::~FbcExtension()
{
}


/*
 * Returns the name of this SBML Level&nbsp;3 package ("fbc").
 */
const std::string&
FbcExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns a string representing the SBML XML namespace of this SBML
 * Level&nbsp;3 package.
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
      if (pkgVersion == 3)
      {
        return getXmlnsL3V1V3();
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
FbcExtension::getLevel(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1() 
    || uri == getXmlnsL3V1V2()
    || uri == getXmlnsL3V1V3())
  {
    return 3;
  }

  return 0;
}


/*
 * Returns the SBML version with the given URI of this package.
 */
unsigned int
FbcExtension::getVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1() 
    || uri == getXmlnsL3V1V2()
    || uri == getXmlnsL3V1V3())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns the SBML Level&nbsp;3 package version for the given URI of this
 * package.
 */
unsigned int
FbcExtension::getPackageVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  if (uri == getXmlnsL3V1V2())
  {
    return 2;
  }

  if (uri == getXmlnsL3V1V3())
  {
    return 3;
  }

  return 0;
}


/*
 * Returns a FbcPkgNamespaces object.
 */
SBMLNamespaces*
FbcExtension::getSBMLExtensionNamespaces(const std::string& uri) const
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

  if (uri == getXmlnsL3V1V3())
  {
    pkgns = new FbcPkgNamespaces(3, 1, 3);
  }

  return pkgns;
}


/*
 * Takes a type code of the &ldquo;fbc&rdquo; package and returns a string
 * describing the code.
 */
const char*
FbcExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_FBC_V1ASSOCIATION;
  int max = SBML_FBC_KEYVALUEPAIR;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Fbc Type)";
  }

  return SBML_FBC_TYPECODE_STRINGS[typeCode - min];
}



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the entry in the error table at this index.
 */
packageErrorTableEntryV3
FbcExtension::getErrorTableV3(unsigned int index) const
{
  return fbcErrorTableV3[index];
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Return the index in the error table with the given errorId.
 */
unsigned int
FbcExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize = sizeof(fbcErrorTableV3)/sizeof(fbcErrorTableV3[0]);
  unsigned int index = 0;

  for (unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == fbcErrorTableV3[i].code)
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
 * Returns the offset for the errorId range for the "fbc" package.
 */
unsigned int
FbcExtension::getErrorIdOffset() const
{
  return 2000000;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns true if the package has multiple versions.
 */
bool
FbcExtension::hasMultiplePackageVersions() const
{
  return true;
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */
unsigned int
FbcExtension::getSeverity(unsigned int index, unsigned int pkgVersion) const
{
  if (hasMultiplePackageVersions())
  {
    packageErrorTableEntryV3 pkgErr = getErrorTableV3(index);
    switch (pkgVersion)
    {
    case 1:
      return pkgErr.l3v1v1_severity;
    case 2:
      return pkgErr.l3v1v2_severity;
    case 3:
    default:
      return pkgErr.l3v1v3_severity;
    }
  }
  else
  {
    return SBMLExtension::getSeverity(index, pkgVersion);
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
unsigned int
FbcExtension::getCategory(unsigned int index, unsigned int pkgVersion) const
{
  if (hasMultiplePackageVersions())
  {
    packageErrorTableEntryV3 pkgErr = getErrorTableV3(index);
    return pkgErr.category;
  }
  else
  {
    return SBMLExtension::getCategory(index, pkgVersion);
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
std::string
FbcExtension::getMessage(unsigned int index,
  unsigned int pkgVersion,
  const std::string& details) const
{
  ostringstream newMsg;
  std::string ref;

  if (hasMultiplePackageVersions())
  {
    packageErrorTableEntryV3 pkgErr = getErrorTableV3(index);
    newMsg << pkgErr.message << endl;
    switch (pkgVersion)
    {
    case 1:
      ref = pkgErr.reference.ref_l3v1v1;
    case 2:
      ref = pkgErr.reference.ref_l3v1v2;
    case 3:
    default:
      ref = pkgErr.reference.ref_l3v1v3;
    }

    if (!ref.empty())
    {
      newMsg << "Reference: " << ref << endl;
    }

    if (!details.empty())
    {
      newMsg << " " << details;
      if (details[details.size() - 1] != '\n') {
        newMsg << endl;
      }
    }

    return newMsg.str();

  }
  else
  {
    return SBMLExtension::getMessage(index, pkgVersion, details);
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
std::string
FbcExtension::getShortMessage(unsigned int index, unsigned int pkgVersion) const
{
  if (hasMultiplePackageVersions())
  {
    packageErrorTableEntryV3 pkgErr = getErrorTableV3(index);
    return pkgErr.shortMessage;
  }
  else
  {
    return SBMLExtension::getShortMessage(index, pkgVersion);
  }
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Initializes fbc extension by creating an object of this class with the
 * required SBasePlugin derived objects and registering the object to the
 * SBMLExtensionRegistry class
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

  packageURIs.push_back(getXmlnsL3V1V3());
  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint modelExtPoint("core", SBML_MODEL);
  SBaseExtensionPoint speciesExtPoint("core", SBML_SPECIES);
  SBaseExtensionPoint reactionExtPoint("core", SBML_REACTION);
  SBaseExtensionPoint sbaseExtPoint("all", SBML_GENERIC_SBASE);

  SBasePluginCreator<FbcSBMLDocumentPlugin, FbcExtension> sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<FbcModelPlugin, FbcExtension> modelPluginCreator(modelExtPoint, packageURIs);
  SBasePluginCreator<FbcSpeciesPlugin, FbcExtension> speciesPluginCreator(speciesExtPoint, packageURIs);
  SBasePluginCreator<FbcReactionPlugin, FbcExtension> reactionPluginCreator(reactionExtPoint, packageURIs);
  SBasePluginCreator<FbcSBasePlugin, FbcExtension> sbasePluginCreator(sbaseExtPoint, packageURIs);

  //----------------------------------------------------------------
  //
  // 4. Adds the creator objects to the extension
  //
  //----------------------------------------------------------------

  fbcExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  fbcExtension.addSBasePluginCreator(&modelPluginCreator);
  fbcExtension.addSBasePluginCreator(&speciesPluginCreator);
  fbcExtension.addSBasePluginCreator(&reactionPluginCreator);
  fbcExtension.addSBasePluginCreator(&sbasePluginCreator);

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


/** @endcond */
#endif /* __cplusplus */


static
const char* SBML_FBC_VARIABLE_TYPE_STRINGS[] =
{
  "linear"
, "quadratic"
, "invalid FbcVariableType value"
};


/*
 * Returns the string version of the provided #FbcVariableType_t enumeration.
 */
LIBSBML_EXTERN
const char*
FbcVariableType_toString(FbcVariableType_t fvt)
{
  int min = FBC_VARIABLE_TYPE_LINEAR;
  int max = FBC_VARIABLE_TYPE_INVALID;

  if (fvt < min || fvt > max)
  {
    return "(Unknown FbcVariableType value)";
  }

  return SBML_FBC_VARIABLE_TYPE_STRINGS[fvt - min];
}


/*
 * Returns the #FbcVariableType_t enumeration corresponding to the given string
 * or @sbmlconstant{FBC_VARIABLE_TYPE_INVALID, FbcVariableType_t} if there is
 * no such match.
 */
LIBSBML_EXTERN
FbcVariableType_t
FbcVariableType_fromString(const char* code)
{
  static int size = sizeof(SBML_FBC_VARIABLE_TYPE_STRINGS)/sizeof(SBML_FBC_VARIABLE_TYPE_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_FBC_VARIABLE_TYPE_STRINGS[i])
    {
      return (FbcVariableType_t)(i);
    }
  }

  return FBC_VARIABLE_TYPE_INVALID;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FbcVariableType_t is valid.
 */
LIBSBML_EXTERN
int
FbcVariableType_isValid(FbcVariableType_t fvt)
{
  int min = FBC_VARIABLE_TYPE_LINEAR;
  int max = FBC_VARIABLE_TYPE_INVALID;

  if (fvt < min || fvt >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FbcVariableType_t.
 */
LIBSBML_EXTERN
int
FbcVariableType_isValidString(const char* code)
{
  return FbcVariableType_isValid(FbcVariableType_fromString(code));
}






LIBSBML_CPP_NAMESPACE_END




