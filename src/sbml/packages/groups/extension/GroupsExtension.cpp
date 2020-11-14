/**
 * @file GroupsExtension.cpp
 * @brief Implementation of GroupsExtension.
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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

#include <sbml/packages/groups/extension/GroupsExtension.h>
#include <sbml/packages/groups/extension/GroupsSBMLDocumentPlugin.h>
#include <sbml/packages/groups/validator/GroupsSBMLErrorTable.h>
#include <sbml/packages/groups/extension/GroupsModelPlugin.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Returns the nickname of the SBML Level&nbsp;3 package implemented by this
 * libSBML extension.
 */
const std::string&
GroupsExtension::getPackageName()
{
  static const std::string pkgName = "groups";
  return pkgName;
}


/*
 * Returns the default SBML Level implemented by this libSBML extension.
 */
unsigned int
GroupsExtension::getDefaultLevel()
{
  return 3;
}


/*
 * Returns the default SBML Version implemented by this libSBML extension.
 */
unsigned int
GroupsExtension::getDefaultVersion()
{
  return 1;
}


/*
 * Returns the default version of the SBML Level&nbsp;3 package implemented by
 * this libSBML extension.
 */
unsigned int
GroupsExtension::getDefaultPackageVersion()
{
  return 1;
}


/*
 * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
 * by this libSBML extension.
 */
const std::string&
GroupsExtension::getXmlnsL3V1V1()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version1/groups/version1";
  return xmlns;
}


/**
 *
 * Adds this GroupsExtension to the SBMLExtensionRegistry class
 *
 */
static SBMLExtensionRegister<GroupsExtension> groupsExtensionRegistry;

static
const char* SBML_GROUPS_TYPECODE_STRINGS[] =
{
    "Member"
  , "Group"
};


/**
 *
 * Instantiate SBMLExtensionNamespaces<GroupsExtension> for DLL
 *
 */
template class LIBSBML_EXTERN SBMLExtensionNamespaces<GroupsExtension>;

/*
 * Creates a new GroupsExtension instance.
 */
GroupsExtension::GroupsExtension()
{
}


/*
 * Copy constructor for GroupsExtension.
 */
GroupsExtension::GroupsExtension(const GroupsExtension& orig)
  : SBMLExtension( orig )
{
}


/*
 * Assignment operator for GroupsExtension.
 */
GroupsExtension&
GroupsExtension::operator=(const GroupsExtension& rhs)
{
  if (&rhs != this)
  {
    SBMLExtension::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this GroupsExtension object.
 */
GroupsExtension*
GroupsExtension::clone() const
{
  return new GroupsExtension(*this);
}


/*
 * Destructor for GroupsExtension.
 */
GroupsExtension::~GroupsExtension()
{
}


/*
 * Returns the name of this SBML Level&nbsp;3 package ("groups").
 */
const std::string&
GroupsExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns a string representing the SBML XML namespace of this SBML
 * Level&nbsp;3 package.
 */
const std::string&
GroupsExtension::getURI(unsigned int sbmlLevel,
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
GroupsExtension::getLevel(const std::string& uri) const
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
GroupsExtension::getVersion(const std::string& uri) const
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
GroupsExtension::getPackageVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns a GroupsPkgNamespaces object.
 */
SBMLNamespaces*
GroupsExtension::getSBMLExtensionNamespaces(const std::string& uri) const
{
  GroupsPkgNamespaces* pkgns = NULL;

  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new GroupsPkgNamespaces(3, 1, 1);
  }

  return pkgns;
}


/*
 * Takes a type code of the &ldquo;groups&rdquo; package and returns a string
 * describing the code.
 */
const char*
GroupsExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_GROUPS_MEMBER;
  int max = SBML_GROUPS_GROUP;

  if (typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Groups Type)";
  }

  return SBML_GROUPS_TYPECODE_STRINGS[typeCode - min];
}



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the entry in the error table at this index.
 */
packageErrorTableEntry
GroupsExtension::getErrorTable(unsigned int index) const
{
  return groupsErrorTable[index];
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Return the index in the error table with the given errorId.
 */
unsigned int
GroupsExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize =
    sizeof(groupsErrorTable)/sizeof(groupsErrorTable[0]);
  unsigned int index = 0;

  for (unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == groupsErrorTable[i].code)
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
 * Returns the offset for the errorId range for the "groups" package.
 */
unsigned int
GroupsExtension::getErrorIdOffset() const
{
  return 4000000;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Initializes groups extension by creating an object of this class with the
 * required SBasePlugin derived objects and registering the object to the
 * SBMLExtensionRegistry class
 */
void
GroupsExtension::init()
{
  if (SBMLExtensionRegistry::getInstance().isRegistered(getPackageName()))
  {
    return;
  }

  GroupsExtension groupsExtension;


  std::vector<std::string> packageURIs;

  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint modelExtPoint("core", SBML_MODEL);

  SBasePluginCreator<GroupsSBMLDocumentPlugin, GroupsExtension>
    sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<GroupsModelPlugin, GroupsExtension>
    modelPluginCreator(modelExtPoint, packageURIs);

  groupsExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  groupsExtension.addSBasePluginCreator(&modelPluginCreator);

  SBMLExtensionRegistry::getInstance().addExtension(&groupsExtension);
}

/** @endcond */




#endif /* __cplusplus */


static
const char* SBML_GROUP_KIND_STRINGS[] =
{
  "classification"
, "partonomy"
, "collection"
, "(Unknown SBML Groups Type)"
};


/*
 * Returns the string version of the provided #GroupKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
GroupKind_toString(GroupKind_t gk)
{
  int min = GROUP_KIND_CLASSIFICATION;
  int max = GROUP_KIND_UNKNOWN;

  if (gk < min || gk > max)
  {
    return "(Unknown GroupKind value)";
  }

  return SBML_GROUP_KIND_STRINGS[gk - min];
}


/*
 * Returns the #GroupKind_t enumeration corresponding to the given string or
 * @sbmlconstant{GROUP_KIND_UNKNOWN, GroupKind_t} if there is no such match.
 */
LIBSBML_EXTERN
GroupKind_t
GroupKind_fromString(const char* code)
{
  static int size =
    sizeof(SBML_GROUP_KIND_STRINGS)/sizeof(SBML_GROUP_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_GROUP_KIND_STRINGS[i])
    {
      return (GroupKind_t)(i);
    }
  }

  return GROUP_KIND_UNKNOWN;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #GroupKind_t is valid.
 */
LIBSBML_EXTERN
int
GroupKind_isValid(GroupKind_t gk)
{
  int min = GROUP_KIND_CLASSIFICATION;
  int max = GROUP_KIND_UNKNOWN;

  if (gk < min || gk >= max)
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
 * given string is a valid #GroupKind_t.
 */
LIBSBML_EXTERN
int
GroupKind_isValidString(const char* code)
{
  return GroupKind_isValid(GroupKind_fromString(code));
}




LIBSBML_CPP_NAMESPACE_END


