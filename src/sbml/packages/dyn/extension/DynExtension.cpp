/**
 * @file:   DynExtension.cpp
 * @brief:  Implementation of the DynExtension class
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


#include <sbml/packages/dyn/extension/DynExtension.h>
#include <sbml/packages/dyn/extension/DynCompartmentPlugin.h>
#include <sbml/packages/dyn/extension/DynEventPlugin.h>
#include <sbml/packages/dyn/extension/DynSBasePlugin.h>
#include <sbml/packages/dyn/extension/DynSBMLDocumentPlugin.h>
#include <sbml/packages/dyn/validator/DynSBMLErrorTable.h>


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
DynExtension::getPackageName ()
{
  static const std::string pkgName = "dyn";
  return pkgName;
}


/*
 * Returns the default SBML Level this extension.
 */
unsigned int
DynExtension::getDefaultLevel ()
{
  return 3;
}


/*
 * Returns the default SBML Version this extension.
 */
unsigned int
DynExtension::getDefaultVersion ()
{
  return 1;
}


/*
 * Returns the default SBML version this extension.
 */
unsigned int
DynExtension::getDefaultPackageVersion ()
{
  return 1;
}


/*
 * XML namespaces of package.
 */
const std::string&
DynExtension::getXmlnsL3V1V1 ()
{
  static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/dyn/version1";
  return xmlns;
}


/*
 * Adds this DynExtension object to the SBMLExtensionRegistry class.
 * DynExtension::init function is automatically invoked when this
 * object is instantiated
 */
static SBMLExtensionRegister<DynExtension> dynExtensionRegistry;


static
const char * SBML_DYN_TYPECODE_STRINGS[] = 
{
    "DynElement"
  , "SpatialComponent"
};


/*
 * Instantiate SBMLExtensionNamespaces<DynExtension>
 * (DynPkgNamespaces) for DLL.
 */
template class LIBSBML_EXTERN  SBMLExtensionNamespaces<DynExtension>;


/*------------------ (END) ----------------------------------*/

/*
 * Constructor
 */
DynExtension::DynExtension()
{
}


/*
 * Copy constructor
 */
DynExtension::DynExtension(const DynExtension& orig) :
   SBMLExtension(orig)
{
}


/*
 * Assignment operator
 */
DynExtension&
DynExtension::operator=(const DynExtension& rhs)
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
DynExtension*
DynExtension::clone () const
 {
  return new DynExtension(*this);
}


/*
 * Destructor
 */
DynExtension::~DynExtension()
 {
}


/*
 * Returns the name of this package
 */
const std::string&
DynExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns the URI (namespace) of the package
 */
const std::string&
DynExtension::getURI(unsigned int sbmlLevel,
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
DynExtension::getLevel(const std::string &uri) const
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
DynExtension::getVersion(const std::string &uri) const
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
DynExtension::getPackageVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns an SBMLExtensionNamespaces<DynExtension> object 
 */
SBMLNamespaces*
DynExtension::getSBMLExtensionNamespaces(const std::string &uri) const
{
  DynPkgNamespaces* pkgns = NULL;
  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new DynPkgNamespaces(3, 1, 1);
  }

  return pkgns;
}


/*
 * This method takes a type code from the Dyn package and returns a string representing 
 */
const char*
DynExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_DYN_ELEMENT;
  int max = SBML_DYN_SPATIALCOMPONENT;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Dyn Type)";
  }

  return SBML_DYN_TYPECODE_STRINGS[typeCode - min];
}


static
const char * SBML_SPATIALKIND_STRINGS[] = 
{
   "Unknown SpatialKind"
 , "cartesianX"
 , "cartesianY"
 , "cartesianZ"
 , "alpha"
 , "beta"
 , "gamma"
 , "F_x"
 , "F_y"
 , "F_z"
};


/*
 * This method takes a type code from the SpatialKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
SpatialKind_toString(SpatialKind_t typeCode)
{
  int min = SPATIALKIND_UNKNOWN;
  int max = DYN_SPATIALKIND_FZ;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SpatialKind value)";
  }

  return SBML_SPATIALKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a SpatialKind code to match it
 */
LIBSBML_EXTERN
SpatialKind_t
SpatialKind_parse(const char* code)
{
  static const int size = sizeof(SBML_SPATIALKIND_STRINGS) / sizeof(SBML_SPATIALKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_SPATIALKIND_STRINGS[i])
      return (SpatialKind_t)i;
  }
  return SPATIALKIND_UNKNOWN;
}


/*
 * Initialization function of dyn extension module which is automatically invoked
 * by SBMLExtensionRegister class before main() function invoked. 
 */
void
DynExtension::init()
{
  //----------------------------------------------------------------
  //
  // 1. Check if the dyn package has already been registered
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

  DynExtension dynExtension;

  //----------------------------------------------------------------
  //
  // 3. Creates the SBasePlugins required by this package
  //
  //----------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint compartmentExtPoint("core", SBML_COMPARTMENT);
  SBaseExtensionPoint eventExtPoint("core", SBML_EVENT);
  SBaseExtensionPoint sbaseExtPoint("all", SBML_GENERIC_SBASE);

  SBasePluginCreator<DynSBMLDocumentPlugin, DynExtension> sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<DynCompartmentPlugin, DynExtension> compartmentPluginCreator(compartmentExtPoint, packageURIs);
  SBasePluginCreator<DynEventPlugin, DynExtension> eventPluginCreator(eventExtPoint, packageURIs);
  SBasePluginCreator<DynSBasePlugin, DynExtension> sbasePluginCreator(sbaseExtPoint, packageURIs);

  //----------------------------------------------------------------
  //
  // 4. Adds the creator objects to the extension
  //
  //----------------------------------------------------------------

  dynExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  dynExtension.addSBasePluginCreator(&compartmentPluginCreator);
  dynExtension.addSBasePluginCreator(&eventPluginCreator);
  dynExtension.addSBasePluginCreator(&sbasePluginCreator);

  //----------------------------------------------------------------
  //
  // 5. Register the object with the registry
  //
  //----------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&dynExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] DynExtension::init() failed." << std::endl;
  }
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table entry. 
 */
packageErrorTableEntry
DynExtension::getErrorTable(unsigned int index) const
{
  return dynErrorTable[index];
}

  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table index for this id. 
 */
unsigned int
DynExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize = sizeof(dynErrorTable)/sizeof(dynErrorTable[0]);
  unsigned int index = 0;

  for(unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == dynErrorTable[i].code)
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
DynExtension::getErrorIdOffset() const
{
  return 9000000;
}

  /** @endcond doxygenLibsbmlInternal */




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


