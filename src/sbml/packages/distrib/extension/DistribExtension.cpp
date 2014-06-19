/**
 * @file:   DistribExtension.cpp
 * @brief:  Implementation of the DistribExtension class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
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


#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPlugin.h>


#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/extension/DistribFunctionDefinitionPlugin.h>
#include <sbml/packages/distrib/extension/DistribSBasePlugin.h>
#include <sbml/packages/distrib/extension/DistribSBMLDocumentPlugin.h>
#include <sbml/packages/distrib/validator/DistribSBMLErrorTable.h>


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
DistribExtension::getPackageName ()
{
  static const std::string pkgName = "distrib";
  return pkgName;
}


/*
 * Returns the default SBML Level this extension.
 */
unsigned int
DistribExtension::getDefaultLevel ()
{
  return 3;
}


/*
 * Returns the default SBML Version this extension.
 */
unsigned int
DistribExtension::getDefaultVersion ()
{
  return 1;
}


/*
 * Returns the default SBML version this extension.
 */
unsigned int
DistribExtension::getDefaultPackageVersion ()
{
  return 1;
}


/*
 * XML namespaces of package.
 */
const std::string&
DistribExtension::getXmlnsL3V1V1 ()
{
  static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/distrib/version1";
  return xmlns;
}


/*
 * Adds this DistribExtension object to the SBMLExtensionRegistry class.
 * DistribExtension::init function is automatically invoked when this
 * object is instantiated
 */
static SBMLExtensionRegister<DistribExtension> distribExtensionRegistry;


static
const char * SBML_DISTRIB_TYPECODE_STRINGS[] = 
{
    "DrawFromDistribution"
  , "DistribInput"
  , "Uncertainty"
};


/*
 * Instantiate SBMLExtensionNamespaces<DistribExtension>
 * (DistribPkgNamespaces) for DLL.
 */
template class LIBSBML_EXTERN  SBMLExtensionNamespaces<DistribExtension>;


/*------------------ (END) ----------------------------------*/

/*
 * Constructor
 */
DistribExtension::DistribExtension()
{
}


/*
 * Copy constructor
 */
DistribExtension::DistribExtension(const DistribExtension& orig) :
   SBMLExtension(orig)
{
}


/*
 * Assignment operator
 */
DistribExtension&
DistribExtension::operator=(const DistribExtension& rhs)
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
DistribExtension*
DistribExtension::clone () const
 {
  return new DistribExtension(*this);
}


/*
 * Destructor
 */
DistribExtension::~DistribExtension()
 {
}


/*
 * Returns the name of this package
 */
const std::string&
DistribExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns the URI (namespace) of the package
 */
const std::string&
DistribExtension::getURI(unsigned int sbmlLevel,
                                  unsigned int sbmlVersion,
                                  unsigned int pkgVersion) const
{
  if (sbmlLevel == 3)
  {
    if (sbmlVersion == 1)
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
DistribExtension::getLevel(const std::string &uri) const
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
DistribExtension::getVersion(const std::string &uri) const
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
DistribExtension::getPackageVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns an SBMLExtensionNamespaces<DistribExtension> object 
 */
SBMLNamespaces*
DistribExtension::getSBMLExtensionNamespaces(const std::string &uri) const
{
  DistribPkgNamespaces* pkgns = NULL;
  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new DistribPkgNamespaces(3, 1, 1);
  }

  return pkgns;
}


/*
 * This method takes a type code from the Distrib package and returns a string representing 
 */
const char*
DistribExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_DISTRIB_DRAW_FROM_DISTRIBUTION;
  int max = SBML_DISTRIB_UNCERTAINTY;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Distrib Type)";
  }

  return SBML_DISTRIB_TYPECODE_STRINGS[typeCode - min];
}


/*
 * Initialization function of distrib extension module which is automatically invoked
 * by SBMLExtensionRegister class before main() function invoked. 
 */
void
DistribExtension::init()
{
  //----------------------------------------------------------------
  //
  // 1. Check if the distrib package has already been registered
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

  DistribExtension distribExtension;

  //----------------------------------------------------------------
  //
  // 3. Creates the SBasePlugins required by this package
  //
  //----------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint functiondefinitionExtPoint("core", SBML_FUNCTION_DEFINITION);
  SBaseExtensionPoint sbaseExtPoint("all", SBML_GENERIC_SBASE);

  SBasePluginCreator<DistribSBMLDocumentPlugin, DistribExtension> sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<DistribFunctionDefinitionPlugin, DistribExtension> functiondefinitionPluginCreator(functiondefinitionExtPoint, packageURIs);
  SBasePluginCreator<DistribSBasePlugin, DistribExtension> sbasePluginCreator(sbaseExtPoint, packageURIs);

  //----------------------------------------------------------------
  //
  // 4. Adds the creator objects to the extension
  //
  //----------------------------------------------------------------

  distribExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  distribExtension.addSBasePluginCreator(&functiondefinitionPluginCreator);
  distribExtension.addSBasePluginCreator(&sbasePluginCreator);

  //----------------------------------------------------------------
  //
  // 5. Register the object with the registry
  //
  //----------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&distribExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] DistribExtension::init() failed." << std::endl;
  }
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table entry. 
 */
packageErrorTableEntry
DistribExtension::getErrorTable(unsigned int index) const
{
  return distribErrorTable[index];
}

  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table index for this id. 
 */
unsigned int
DistribExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize = sizeof(distribErrorTable)/sizeof(distribErrorTable[0]);
  unsigned int index = 0;

  for(unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == distribErrorTable[i].code)
    {
      index = i;
      break;
    }

  }

  return index;
}

  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error offset. 
 */
unsigned int
DistribExtension::getErrorIdOffset() const
{
  return 5000000;
}

  /** @endcond doxygenLibsbmlInternal */




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


