/**
 * @file:   ArraysExtension.cpp
 * @brief:  Implementation of the ArraysExtension class
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


#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/packages/arrays/extension/ArraysSBasePlugin.h>
#include <sbml/packages/arrays/extension/ArraysASTPlugin.h>
#include <sbml/packages/arrays/extension/ArraysSBMLDocumentPlugin.h>
#include <sbml/packages/arrays/validator/ArraysSBMLErrorTable.h>


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
ArraysExtension::getPackageName ()
{
  static const std::string pkgName = "arrays";
  return pkgName;
}


/*
 * Returns the default SBML Level this extension.
 */
unsigned int
ArraysExtension::getDefaultLevel ()
{
  return 3;
}


/*
 * Returns the default SBML Version this extension.
 */
unsigned int
ArraysExtension::getDefaultVersion ()
{
  return 1;
}


/*
 * Returns the default SBML version this extension.
 */
unsigned int
ArraysExtension::getDefaultPackageVersion ()
{
  return 1;
}


/*
 * XML namespaces of package.
 */
const std::string&
ArraysExtension::getXmlnsL3V1V1 ()
{
  static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/arrays/version1";
  return xmlns;
}


/*
 * Adds this ArraysExtension object to the SBMLExtensionRegistry class.
 * ArraysExtension::init function is automatically invoked when this
 * object is instantiated
 */
static SBMLExtensionRegister<ArraysExtension> arraysExtensionRegistry;


static
const char * SBML_ARRAYS_TYPECODE_STRINGS[] = 
{
    "Index"
  , "Dimension"
};


/*
 * Instantiate SBMLExtensionNamespaces<ArraysExtension>
 * (ArraysPkgNamespaces) for DLL.
 */
template class LIBSBML_EXTERN  SBMLExtensionNamespaces<ArraysExtension>;


/*------------------ (END) ----------------------------------*/

/*
 * Constructor
 */
ArraysExtension::ArraysExtension()
{
}


/*
 * Copy constructor
 */
ArraysExtension::ArraysExtension(const ArraysExtension& orig) :
   SBMLExtension(orig)
{
}


/*
 * Assignment operator
 */
ArraysExtension&
ArraysExtension::operator=(const ArraysExtension& rhs)
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
ArraysExtension*
ArraysExtension::clone () const
 {
  return new ArraysExtension(*this);
}


/*
 * Destructor
 */
ArraysExtension::~ArraysExtension()
 {
}


/*
 * Returns the name of this package
 */
const std::string&
ArraysExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns the URI (namespace) of the package
 */
const std::string&
ArraysExtension::getURI(unsigned int sbmlLevel,
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
ArraysExtension::getLevel(const std::string &uri) const
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
ArraysExtension::getVersion(const std::string &uri) const
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
ArraysExtension::getPackageVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns an SBMLExtensionNamespaces<ArraysExtension> object 
 */
SBMLNamespaces*
ArraysExtension::getSBMLExtensionNamespaces(const std::string &uri) const
{
  ArraysPkgNamespaces* pkgns = NULL;
  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new ArraysPkgNamespaces(3, 1, 1);
  }

  return pkgns;
}


/*
 * This method takes a type code from the Arrays package and returns a string representing 
 */
const char*
ArraysExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_ARRAYS_INDEX;
  int max = SBML_ARRAYS_DIMENSION;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Arrays Type)";
  }

  return SBML_ARRAYS_TYPECODE_STRINGS[typeCode - min];
}


/*
 * Initialization function of arrays extension module which is automatically invoked
 * by SBMLExtensionRegister class before main() function invoked. 
 */
void
ArraysExtension::init()
{
  //----------------------------------------------------------------
  //
  // 1. Check if the arrays package has already been registered
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

  ArraysExtension arraysExtension;

  //----------------------------------------------------------------
  //
  // 3. Creates the SBasePlugins required by this package
  //
  //----------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint sbaseExtPoint("all", SBML_GENERIC_SBASE);

  SBasePluginCreator<ArraysSBMLDocumentPlugin, ArraysExtension> sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<ArraysSBasePlugin, ArraysExtension> sbasePluginCreator(sbaseExtPoint, packageURIs);

  //----------------------------------------------------------------
  //
  // 4. Adds the creator objects to the extension
  //
  //----------------------------------------------------------------

  arraysExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  arraysExtension.addSBasePluginCreator(&sbasePluginCreator);
  ArraysASTPlugin arrays(getXmlnsL3V1V1());
  arraysExtension.setASTBasePlugin(&arrays);

  //----------------------------------------------------------------
  //
  // 5. Register the object with the registry
  //
  //----------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&arraysExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] ArraysExtension::init() failed." << std::endl;
  }
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table entry. 
 */
packageErrorTableEntry
ArraysExtension::getErrorTable(unsigned int index) const
{
  return arraysErrorTable[index];
}

  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table index for this id. 
 */
unsigned int
ArraysExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize = sizeof(arraysErrorTable)/sizeof(arraysErrorTable[0]);
  unsigned int index = 0;

  for(unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == arraysErrorTable[i].code)
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
ArraysExtension::getErrorIdOffset() const
{
  return 8000000;
}

  /** @endcond doxygenLibsbmlInternal */




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


