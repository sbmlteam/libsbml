/**
 * @file ListOfCSGObjects.cpp
 * @brief Implementation of the ListOfCSGObjects class.
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
#include <sbml/packages/spatial/sbml/ListOfCSGObjects.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfCSGObjects using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfCSGObjects::ListOfCSGObjects(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfCSGObjects using the given SpatialPkgNamespaces object.
 */
ListOfCSGObjects::ListOfCSGObjects(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfCSGObjects.
 */
ListOfCSGObjects::ListOfCSGObjects(const ListOfCSGObjects& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfCSGObjects.
 */
ListOfCSGObjects&
ListOfCSGObjects::operator=(const ListOfCSGObjects& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfCSGObjects object.
 */
ListOfCSGObjects*
ListOfCSGObjects::clone() const
{
  return new ListOfCSGObjects(*this);
}


/*
 * Destructor for ListOfCSGObjects.
 */
ListOfCSGObjects::~ListOfCSGObjects()
{
}


/*
 * Get a CSGObject from the ListOfCSGObjects.
 */
CSGObject*
ListOfCSGObjects::get(unsigned int n)
{
  return static_cast<CSGObject*>(ListOf::get(n));
}


/*
 * Get a CSGObject from the ListOfCSGObjects.
 */
const CSGObject*
ListOfCSGObjects::get(unsigned int n) const
{
  return static_cast<const CSGObject*>(ListOf::get(n));
}


/*
 * Get a CSGObject from the ListOfCSGObjects based on its identifier.
 */
CSGObject*
ListOfCSGObjects::get(const std::string& sid)
{
  return const_cast<CSGObject*>(static_cast<const
    ListOfCSGObjects&>(*this).get(sid));
}


/*
 * Get a CSGObject from the ListOfCSGObjects based on its identifier.
 */
const CSGObject*
ListOfCSGObjects::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<CSGObject>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const CSGObject*>
    (*result);
}


/*
 * Removes the nth CSGObject from this ListOfCSGObjects and returns a pointer
 * to it.
 */
CSGObject*
ListOfCSGObjects::remove(unsigned int n)
{
  return static_cast<CSGObject*>(ListOf::remove(n));
}


/*
 * Removes the CSGObject from this ListOfCSGObjects based on its identifier and
 * returns a pointer to it.
 */
CSGObject*
ListOfCSGObjects::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<CSGObject>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <CSGObject*> (item);
}


/*
 * Adds a copy of the given CSGObject to this ListOfCSGObjects.
 */
int
ListOfCSGObjects::addCSGObject(const CSGObject* csgo)
{
  if (csgo == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (csgo->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (csgo->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != csgo->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != csgo->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(csgo)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(csgo);
  }
}


/*
 * Get the number of CSGObject objects in this ListOfCSGObjects.
 */
unsigned int
ListOfCSGObjects::getNumCSGObjects() const
{
  return size();
}


/*
 * Creates a new CSGObject object, adds it to this ListOfCSGObjects object and
 * returns the CSGObject object created.
 */
CSGObject*
ListOfCSGObjects::createCSGObject()
{
  CSGObject* csgo = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csgo = new CSGObject(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (csgo != NULL)
  {
    appendAndOwn(csgo);
  }

  return csgo;
}


/*
 * Used by ListOfCSGObjects::get() to lookup a CSGObject based on its
 * DomainType.
 */
struct IdEqDT2
{
  const string& id;
   
  IdEqDT2 (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<CSGObject*>(sb)->getDomainType() == id);
  }
};


/*
 * Get a CSGObject from the ListOfCSGObjects based on the DomainType to which
 * it refers.
 */
const CSGObject*
ListOfCSGObjects::getByDomainType(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqDT2(sid));
  return (result == mItems.end()) ? 0 : static_cast <const CSGObject*>
    (*result);
}


/*
 * Get a CSGObject from the ListOfCSGObjects based on the DomainType to which
 * it refers.
 */
CSGObject*
ListOfCSGObjects::getByDomainType(const std::string& sid)
{
  return const_cast<CSGObject*>(static_cast<const
    ListOfCSGObjects&>(*this).getByDomainType(sid));
}


/*
 * Returns the XML element name of this ListOfCSGObjects object.
 */
const std::string&
ListOfCSGObjects::getElementName() const
{
  static const string name = "listOfCSGObjects";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfCSGObjects object.
 */
int
ListOfCSGObjects::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfCSGObjects object.
 */
int
ListOfCSGObjects::getItemTypeCode() const
{
  return SBML_SPATIAL_CSGOBJECT;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new CSGObject in this ListOfCSGObjects
 */
SBase*
ListOfCSGObjects::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "csgObject")
  {
    object = new CSGObject(spatialns);
    appendAndOwn(object);
  }

  delete spatialns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Spatial package
 */
void
ListOfCSGObjects::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(SpatialExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(SpatialExtension::getXmlnsL3V1V1(), prefix);
    }
  }

  stream << xmlns;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get a CSGObject_t from the ListOf_t.
 */
LIBSBML_EXTERN
CSGObject_t*
ListOfCSGObjects_getCSGObject(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfCSGObjects*>(lo)->get(n);
}


/*
 * Get a CSGObject_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
CSGObject_t*
ListOfCSGObjects_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfCSGObjects*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth CSGObject_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
CSGObject_t*
ListOfCSGObjects_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfCSGObjects*>(lo)->remove(n);
}


/*
 * Removes the CSGObject_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
CSGObject_t*
ListOfCSGObjects_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfCSGObjects*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


