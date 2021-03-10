/**
 * @file ListOfParametricObjects.cpp
 * @brief Implementation of the ListOfParametricObjects class.
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
#include <sbml/packages/spatial/sbml/ListOfParametricObjects.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfParametricObjects using the given SBML Level, Version
 * and &ldquo;spatial&rdquo; package version.
 */
ListOfParametricObjects::ListOfParametricObjects(unsigned int level,
                                                 unsigned int version,
                                                 unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfParametricObjects using the given SpatialPkgNamespaces
 * object.
 */
ListOfParametricObjects::ListOfParametricObjects(SpatialPkgNamespaces
  *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfParametricObjects.
 */
ListOfParametricObjects::ListOfParametricObjects(const ListOfParametricObjects&
  orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfParametricObjects.
 */
ListOfParametricObjects&
ListOfParametricObjects::operator=(const ListOfParametricObjects& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfParametricObjects object.
 */
ListOfParametricObjects*
ListOfParametricObjects::clone() const
{
  return new ListOfParametricObjects(*this);
}


/*
 * Destructor for ListOfParametricObjects.
 */
ListOfParametricObjects::~ListOfParametricObjects()
{
}


/*
 * Get a ParametricObject from the ListOfParametricObjects.
 */
ParametricObject*
ListOfParametricObjects::get(unsigned int n)
{
  return static_cast<ParametricObject*>(ListOf::get(n));
}


/*
 * Get a ParametricObject from the ListOfParametricObjects.
 */
const ParametricObject*
ListOfParametricObjects::get(unsigned int n) const
{
  return static_cast<const ParametricObject*>(ListOf::get(n));
}


/*
 * Get a ParametricObject from the ListOfParametricObjects based on its
 * identifier.
 */
ParametricObject*
ListOfParametricObjects::get(const std::string& sid)
{
  return const_cast<ParametricObject*>(static_cast<const
    ListOfParametricObjects&>(*this).get(sid));
}


/*
 * Get a ParametricObject from the ListOfParametricObjects based on its
 * identifier.
 */
const ParametricObject*
ListOfParametricObjects::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<ParametricObject>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const ParametricObject*>
    (*result);
}


/*
 * Removes the nth ParametricObject from this ListOfParametricObjects and
 * returns a pointer to it.
 */
ParametricObject*
ListOfParametricObjects::remove(unsigned int n)
{
  return static_cast<ParametricObject*>(ListOf::remove(n));
}


/*
 * Removes the ParametricObject from this ListOfParametricObjects based on its
 * identifier and returns a pointer to it.
 */
ParametricObject*
ListOfParametricObjects::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<ParametricObject>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <ParametricObject*> (item);
}


/*
 * Adds a copy of the given ParametricObject to this ListOfParametricObjects.
 */
int
ListOfParametricObjects::addParametricObject(const ParametricObject* po)
{
  if (po == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (po->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != po->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != po->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(po)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(po);
  }
}


/*
 * Get the number of ParametricObject objects in this ListOfParametricObjects.
 */
unsigned int
ListOfParametricObjects::getNumParametricObjects() const
{
  return size();
}


/*
 * Creates a new ParametricObject object, adds it to this
 * ListOfParametricObjects object and returns the ParametricObject object
 * created.
 */
ParametricObject*
ListOfParametricObjects::createParametricObject()
{
  ParametricObject* po = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    po = new ParametricObject(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (po != NULL)
  {
    appendAndOwn(po);
  }

  return po;
}


/*
 * Used by ListOfParametricObjects::get() to lookup a ParametricObject based on
 * its DomainType.
 */
struct IdEqDT4
{
  const string& id;
   
  IdEqDT4 (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<ParametricObject*>(sb)->getDomainType() == id);
  }
};


/*
 * Get a ParametricObject from the ListOfParametricObjects based on the
 * DomainType to which it refers.
 */
const ParametricObject*
ListOfParametricObjects::getByDomainType(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqDT4(sid));
  return (result == mItems.end()) ? 0 : static_cast <const ParametricObject*>
    (*result);
}


/*
 * Get a ParametricObject from the ListOfParametricObjects based on the
 * DomainType to which it refers.
 */
ParametricObject*
ListOfParametricObjects::getByDomainType(const std::string& sid)
{
  return const_cast<ParametricObject*>(static_cast<const
    ListOfParametricObjects&>(*this).getByDomainType(sid));
}


/*
 * Returns the XML element name of this ListOfParametricObjects object.
 */
const std::string&
ListOfParametricObjects::getElementName() const
{
  static const string name = "listOfParametricObjects";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfParametricObjects object.
 */
int
ListOfParametricObjects::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfParametricObjects object.
 */
int
ListOfParametricObjects::getItemTypeCode() const
{
  return SBML_SPATIAL_PARAMETRICOBJECT;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new ParametricObject in this ListOfParametricObjects
 */
SBase*
ListOfParametricObjects::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "parametricObject")
  {
    object = new ParametricObject(spatialns);
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
ListOfParametricObjects::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a ParametricObject_t from the ListOf_t.
 */
LIBSBML_EXTERN
ParametricObject_t*
ListOfParametricObjects_getParametricObject(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfParametricObjects*>(lo)->get(n);
}


/*
 * Get a ParametricObject_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
ParametricObject_t*
ListOfParametricObjects_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfParametricObjects*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth ParametricObject_t from this ListOf_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
ParametricObject_t*
ListOfParametricObjects_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfParametricObjects*>(lo)->remove(n);
}


/*
 * Removes the ParametricObject_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
ParametricObject_t*
ListOfParametricObjects_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfParametricObjects*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


