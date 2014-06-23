/**
 * @file:   ReqExtension.cpp
 * @brief:  Implementation of the ReqExtension class
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


#include <sbml/packages/req/extension/ReqExtension.h>
#include <sbml/packages/req/extension/ReqSBasePlugin.h>
#include <sbml/packages/req/extension/ReqSBMLDocumentPlugin.h>
#include <sbml/packages/req/validator/ReqSBMLErrorTable.h>


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
ReqExtension::getPackageName ()
{
  static const std::string pkgName = "req";
  return pkgName;
}


/*
 * Returns the default SBML Level this extension.
 */
unsigned int
ReqExtension::getDefaultLevel ()
{
  return 3;
}


/*
 * Returns the default SBML Version this extension.
 */
unsigned int
ReqExtension::getDefaultVersion ()
{
  return 1;
}


/*
 * Returns the default SBML version this extension.
 */
unsigned int
ReqExtension::getDefaultPackageVersion ()
{
  return 1;
}


/*
 * XML namespaces of package.
 */
const std::string&
ReqExtension::getXmlnsL3V1V1 ()
{
  static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/req/version1";
  return xmlns;
}


/*
 * Adds this ReqExtension object to the SBMLExtensionRegistry class.
 * ReqExtension::init function is automatically invoked when this
 * object is instantiated
 */
static SBMLExtensionRegister<ReqExtension> reqExtensionRegistry;


static
const char * SBML_REQ_TYPECODE_STRINGS[] = 
{
    "ChangedMath"
};


/*
 * Instantiate SBMLExtensionNamespaces<ReqExtension>
 * (ReqPkgNamespaces) for DLL.
 */
template class LIBSBML_EXTERN  SBMLExtensionNamespaces<ReqExtension>;


/*------------------ (END) ----------------------------------*/

/*
 * Constructor
 */
ReqExtension::ReqExtension()
{
}


/*
 * Copy constructor
 */
ReqExtension::ReqExtension(const ReqExtension& orig) :
   SBMLExtension(orig)
{
}


/*
 * Assignment operator
 */
ReqExtension&
ReqExtension::operator=(const ReqExtension& rhs)
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
ReqExtension*
ReqExtension::clone () const
 {
  return new ReqExtension(*this);
}


/*
 * Destructor
 */
ReqExtension::~ReqExtension()
 {
}


/*
 * Returns the name of this package
 */
const std::string&
ReqExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns the URI (namespace) of the package
 */
const std::string&
ReqExtension::getURI(unsigned int sbmlLevel,
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
ReqExtension::getLevel(const std::string &uri) const
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
ReqExtension::getVersion(const std::string &uri) const
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
ReqExtension::getPackageVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns an SBMLExtensionNamespaces<ReqExtension> object 
 */
SBMLNamespaces*
ReqExtension::getSBMLExtensionNamespaces(const std::string &uri) const
{
  ReqPkgNamespaces* pkgns = NULL;
  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new ReqPkgNamespaces(3, 1, 1);
  }

  return pkgns;
}


/*
 * This method takes a type code from the Req package and returns a string representing 
 */
const char*
ReqExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_REQ_CHANGED_MATH;
  int max = SBML_REQ_CHANGED_MATH;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Req Type)";
  }

  return SBML_REQ_TYPECODE_STRINGS[typeCode - min];
}


/*
 * Initialization function of req extension module which is automatically invoked
 * by SBMLExtensionRegister class before main() function invoked. 
 */
void
ReqExtension::init()
{
  //----------------------------------------------------------------
  //
  // 1. Check if the req package has already been registered
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

  ReqExtension reqExtension;

  //----------------------------------------------------------------
  //
  // 3. Creates the SBasePlugins required by this package
  //
  //----------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint sbaseExtPoint("all", SBML_GENERIC_SBASE);

  SBasePluginCreator<ReqSBMLDocumentPlugin, ReqExtension> sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<ReqSBasePlugin, ReqExtension> sbasePluginCreator(sbaseExtPoint, packageURIs);

  //----------------------------------------------------------------
  //
  // 4. Adds the creator objects to the extension
  //
  //----------------------------------------------------------------

  reqExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  reqExtension.addSBasePluginCreator(&sbasePluginCreator);

  //----------------------------------------------------------------
  //
  // 5. Register the object with the registry
  //
  //----------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&reqExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] ReqExtension::init() failed." << std::endl;
  }
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table entry. 
 */
packageErrorTableEntry
ReqExtension::getErrorTable(unsigned int index) const
{
  return reqErrorTable[index];
}

  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table index for this id. 
 */
unsigned int
ReqExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize = sizeof(reqErrorTable)/sizeof(reqErrorTable[0]);
  unsigned int index = 0;

  for(unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == reqErrorTable[i].code)
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
ReqExtension::getErrorIdOffset() const
{
  return 1100000;
}

  /** @endcond doxygenLibsbmlInternal */




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


