/**
 * @file ArraysExtension.cpp
 * @brief Implementation of ArraysExtension.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPlugin.h>

#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/packages/arrays/extension/ArraysASTPlugin.h>
#include <sbml/packages/arrays/extension/ArraysSBMLDocumentPlugin.h>
#include <sbml/packages/arrays/validator/ArraysSBMLErrorTable.h>
#include <sbml/packages/arrays/extension/ArraysSBasePlugin.h>

#include <sbml/packages/arrays/util/ArraysFlatteningConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>

using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Returns the nickname of the SBML Level&nbsp;3 package implemented by this
 * libSBML extension.
 */
const std::string&
ArraysExtension::getPackageName()
{
  static const std::string pkgName = "arrays";
  return pkgName;
}


/*
 * Returns the default SBML Level implemented by this libSBML extension.
 */
unsigned int
ArraysExtension::getDefaultLevel()
{
  return 3;
}


/*
 * Returns the default SBML Version implemented by this libSBML extension.
 */
unsigned int
ArraysExtension::getDefaultVersion()
{
  return 1;
}


/*
 * Returns the default version of the SBML Level&nbsp;3 package implemented by
 * this libSBML extension.
 */
unsigned int
ArraysExtension::getDefaultPackageVersion()
{
  return 1;
}


/*
 * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
 * by this libSBML extension.
 */
const std::string&
ArraysExtension::getXmlnsL3V1V1()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version1/arrays/version1";
  return xmlns;
}


/**
 *
 * Adds this ArraysExtension to the SBMLExtensionRegistry class
 *
 */
static SBMLExtensionRegister<ArraysExtension> arraysExtensionRegistry;

static
const char* SBML_ARRAYS_TYPECODE_STRINGS[] =
{
    "Index"
  , "Dimension"
};


/**
 *
 * Instantiate SBMLExtensionNamespaces<ArraysExtension> for DLL
 *
 */
template class LIBSBML_EXTERN SBMLExtensionNamespaces<ArraysExtension>;

/*
 * Creates a new ArraysExtension instance.
 */
ArraysExtension::ArraysExtension()
{
}


/*
 * Copy constructor for ArraysExtension.
 */
ArraysExtension::ArraysExtension(const ArraysExtension& orig)
  : SBMLExtension( orig )
{
}


/*
 * Assignment operator for ArraysExtension.
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
 * Creates and returns a deep copy of this ArraysExtension object.
 */
ArraysExtension*
ArraysExtension::clone() const
{
  return new ArraysExtension(*this);
}


/*
 * Destructor for ArraysExtension.
 */
ArraysExtension::~ArraysExtension()
{
}


/*
 * Returns the name of this SBML Level&nbsp;3 package ("arrays").
 */
const std::string&
ArraysExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns a string representing the SBML XML namespace of this SBML
 * Level&nbsp;3 package.
 */
const std::string&
ArraysExtension::getURI(unsigned int sbmlLevel,
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
 * Returns the SBML Level for the given URI of this package.
 */
unsigned int
ArraysExtension::getLevel(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 3;
  }

  return 0;
}


/*
 * Returns the Version within the SBML Level for the given URI of this package.
 */
unsigned int
ArraysExtension::getVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
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
ArraysExtension::getPackageVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns a ArraysPkgNamespaces object.
 */
SBMLNamespaces*
ArraysExtension::getSBMLExtensionNamespaces(const std::string& uri) const
{
  ArraysPkgNamespaces* pkgns = NULL;

  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new ArraysPkgNamespaces(3, 1, 1);
  }

  return pkgns;
}


/*
 * Takes a type code of the &ldquo;arrays&rdquo; package and returns a string
 * describing the code.
 */
const char*
ArraysExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_ARRAYS_INDEX;
  int max = SBML_ARRAYS_DIMENSION;

  if (typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Arrays Type)";
  }

  return SBML_ARRAYS_TYPECODE_STRINGS[typeCode - min];
}



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the entry in the error table at this index.
 */
packageErrorTableEntry
ArraysExtension::getErrorTable(unsigned int index) const
{
  return arraysErrorTable[index];
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Return the index in the error table with the given errorId.
 */
unsigned int
ArraysExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize =
    sizeof(arraysErrorTable)/sizeof(arraysErrorTable[0]);
  unsigned int index = 0;

  for (unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == arraysErrorTable[i].code)
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
 * Returns the offset for the errorId range for the "arrays" package.
 */
unsigned int
ArraysExtension::getErrorIdOffset() const
{
  return 8000000;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Initializes arrays extension by creating an object of this class with the
 * required SBasePlugin derived objects and registering the object to the
 * SBMLExtensionRegistry class
 */
void
ArraysExtension::init()
{
  if (SBMLExtensionRegistry::getInstance().isRegistered(getPackageName()))
  {
    return;
  }

  ArraysExtension arraysExtension;


  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint sbaseExtPoint("all", SBML_GENERIC_SBASE);

  SBasePluginCreator<ArraysSBMLDocumentPlugin, ArraysExtension>
    sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<ArraysSBasePlugin, ArraysExtension>
    sbasePluginCreator(sbaseExtPoint, packageURIs);

  arraysExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  arraysExtension.addSBasePluginCreator(&sbasePluginCreator);
  ArraysASTPlugin arrays(getXmlnsL3V1V1());
  arraysExtension.setASTBasePlugin(&arrays);

  int result =
    SBMLExtensionRegistry::getInstance().addExtension(&arraysExtension);

  // 6. Register the flattening converter

  ArraysFlatteningConverter c1;
  SBMLConverterRegistry::getInstance().addConverter(&c1);

}

/** @endcond */




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_END


