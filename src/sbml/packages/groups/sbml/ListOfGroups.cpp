/**
 * @file ListOfGroups.cpp
 * @brief Implementation of the ListOfGroups class.
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
#include <sbml/packages/groups/sbml/ListOfGroups.h>
#include <sbml/packages/groups/validator/GroupsSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfGroups using the given SBML Level, Version and
 * &ldquo;groups&rdquo; package version.
 */
ListOfGroups::ListOfGroups(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new GroupsPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfGroups using the given GroupsPkgNamespaces object.
 */
ListOfGroups::ListOfGroups(GroupsPkgNamespaces *groupsns)
  : ListOf(groupsns)
{
  setElementNamespace(groupsns->getURI());
}


/*
 * Copy constructor for ListOfGroups.
 */
ListOfGroups::ListOfGroups(const ListOfGroups& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfGroups.
 */
ListOfGroups&
ListOfGroups::operator=(const ListOfGroups& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfGroups object.
 */
ListOfGroups*
ListOfGroups::clone() const
{
  return new ListOfGroups(*this);
}


/*
 * Destructor for ListOfGroups.
 */
ListOfGroups::~ListOfGroups()
{
}


/*
 * Get a Group from the ListOfGroups.
 */
Group*
ListOfGroups::get(unsigned int n)
{
  return static_cast<Group*>(ListOf::get(n));
}


/*
 * Get a Group from the ListOfGroups.
 */
const Group*
ListOfGroups::get(unsigned int n) const
{
  return static_cast<const Group*>(ListOf::get(n));
}


/*
 * Get a Group from the ListOfGroups based on its identifier.
 */
Group*
ListOfGroups::get(const std::string& sid)
{
  return const_cast<Group*>(static_cast<const ListOfGroups&>(*this).get(sid));
}


/*
 * Get a Group from the ListOfGroups based on its identifier.
 */
const Group*
ListOfGroups::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<Group>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Group*> (*result);
}


/*
 * Removes the nth Group from this ListOfGroups and returns a pointer to it.
 */
Group*
ListOfGroups::remove(unsigned int n)
{
  return static_cast<Group*>(ListOf::remove(n));
}


/*
 * Removes the Group from this ListOfGroups based on its identifier and returns
 * a pointer to it.
 */
Group*
ListOfGroups::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<Group>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Group*> (item);
}


/*
 * Adds a copy of the given Group to this ListOfGroups.
 */
int
ListOfGroups::addGroup(const Group* g)
{
  if (g == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (g->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != g->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != g->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(g)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(g);
  }
}


/*
 * Get the number of Group objects in this ListOfGroups.
 */
unsigned int
ListOfGroups::getNumGroups() const
{
  return size();
}


/*
 * Creates a new Group object, adds it to this ListOfGroups object and returns
 * the Group object created.
 */
Group*
ListOfGroups::createGroup()
{
  Group* g = NULL;

  try
  {
    GROUPS_CREATE_NS(groupsns, getSBMLNamespaces());
    g = new Group(groupsns);
    delete groupsns;
  }
  catch (...)
  {
  }

  if (g != NULL)
  {
    appendAndOwn(g);
  }

  return g;
}


/*
 * Returns the XML element name of this ListOfGroups object.
 */
const std::string&
ListOfGroups::getElementName() const
{
  static const string name = "listOfGroups";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfGroups object.
 */
int
ListOfGroups::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfGroups object.
 */
int
ListOfGroups::getItemTypeCode() const
{
  return SBML_GROUPS_GROUP;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Group in this ListOfGroups
 */
SBase*
ListOfGroups::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  GROUPS_CREATE_NS(groupsns, getSBMLNamespaces());

  if (name == "group")
  {
    object = new Group(groupsns);
    appendAndOwn(object);
  }

  delete groupsns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Groups package
 */
void
ListOfGroups::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(GroupsExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(GroupsExtension::getXmlnsL3V1V1(), prefix);
    }
  }

  stream << xmlns;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get a Group_t from the ListOf_t.
 */
LIBSBML_EXTERN
Group_t*
ListOfGroups_getGroup(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGroups*>(lo)->get(n);
}


/*
 * Get a Group_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
Group_t*
ListOfGroups_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfGroups*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth Group_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
Group_t*
ListOfGroups_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGroups*>(lo)->remove(n);
}


/*
 * Removes the Group_t from this ListOf_t based on its identifier and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
Group_t*
ListOfGroups_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfGroups*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


