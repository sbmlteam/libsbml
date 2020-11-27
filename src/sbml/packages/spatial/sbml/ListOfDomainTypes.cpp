/**
 * @file ListOfDomainTypes.cpp
 * @brief Implementation of the ListOfDomainTypes class.
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
#include <sbml/packages/spatial/sbml/ListOfDomainTypes.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfDomainTypes using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfDomainTypes::ListOfDomainTypes(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfDomainTypes using the given SpatialPkgNamespaces object.
 */
ListOfDomainTypes::ListOfDomainTypes(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfDomainTypes.
 */
ListOfDomainTypes::ListOfDomainTypes(const ListOfDomainTypes& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfDomainTypes.
 */
ListOfDomainTypes&
ListOfDomainTypes::operator=(const ListOfDomainTypes& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfDomainTypes object.
 */
ListOfDomainTypes*
ListOfDomainTypes::clone() const
{
  return new ListOfDomainTypes(*this);
}


/*
 * Destructor for ListOfDomainTypes.
 */
ListOfDomainTypes::~ListOfDomainTypes()
{
}


/*
 * Get a DomainType from the ListOfDomainTypes.
 */
DomainType*
ListOfDomainTypes::get(unsigned int n)
{
  return static_cast<DomainType*>(ListOf::get(n));
}


/*
 * Get a DomainType from the ListOfDomainTypes.
 */
const DomainType*
ListOfDomainTypes::get(unsigned int n) const
{
  return static_cast<const DomainType*>(ListOf::get(n));
}


/*
 * Get a DomainType from the ListOfDomainTypes based on its identifier.
 */
DomainType*
ListOfDomainTypes::get(const std::string& sid)
{
  return const_cast<DomainType*>(static_cast<const
    ListOfDomainTypes&>(*this).get(sid));
}


/*
 * Get a DomainType from the ListOfDomainTypes based on its identifier.
 */
const DomainType*
ListOfDomainTypes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<DomainType>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const DomainType*>
    (*result);
}


/*
 * Removes the nth DomainType from this ListOfDomainTypes and returns a pointer
 * to it.
 */
DomainType*
ListOfDomainTypes::remove(unsigned int n)
{
  return static_cast<DomainType*>(ListOf::remove(n));
}


/*
 * Removes the DomainType from this ListOfDomainTypes based on its identifier
 * and returns a pointer to it.
 */
DomainType*
ListOfDomainTypes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<DomainType>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <DomainType*> (item);
}


/*
 * Adds a copy of the given DomainType to this ListOfDomainTypes.
 */
int
ListOfDomainTypes::addDomainType(const DomainType* dt)
{
  if (dt == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (dt->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != dt->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != dt->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(dt)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(dt);
  }
}


/*
 * Get the number of DomainType objects in this ListOfDomainTypes.
 */
unsigned int
ListOfDomainTypes::getNumDomainTypes() const
{
  return size();
}


/*
 * Creates a new DomainType object, adds it to this ListOfDomainTypes object
 * and returns the DomainType object created.
 */
DomainType*
ListOfDomainTypes::createDomainType()
{
  DomainType* dt = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    dt = new DomainType(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (dt != NULL)
  {
    appendAndOwn(dt);
  }

  return dt;
}


/*
 * Returns the XML element name of this ListOfDomainTypes object.
 */
const std::string&
ListOfDomainTypes::getElementName() const
{
  static const string name = "listOfDomainTypes";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfDomainTypes object.
 */
int
ListOfDomainTypes::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfDomainTypes object.
 */
int
ListOfDomainTypes::getItemTypeCode() const
{
  return SBML_SPATIAL_DOMAINTYPE;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new DomainType in this ListOfDomainTypes
 */
SBase*
ListOfDomainTypes::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "domainType")
  {
    object = new DomainType(spatialns);
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
ListOfDomainTypes::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a DomainType_t from the ListOf_t.
 */
LIBSBML_EXTERN
DomainType_t*
ListOfDomainTypes_getDomainType(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDomainTypes*>(lo)->get(n);
}


/*
 * Get a DomainType_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
DomainType_t*
ListOfDomainTypes_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDomainTypes*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth DomainType_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
DomainType_t*
ListOfDomainTypes_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDomainTypes*>(lo)->remove(n);
}


/*
 * Removes the DomainType_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
DomainType_t*
ListOfDomainTypes_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDomainTypes*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


