/**
 * @file ListOfDomains.cpp
 * @brief Implementation of the ListOfDomains class.
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
#include <sbml/packages/spatial/sbml/ListOfDomains.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfDomains using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfDomains::ListOfDomains(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfDomains using the given SpatialPkgNamespaces object.
 */
ListOfDomains::ListOfDomains(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfDomains.
 */
ListOfDomains::ListOfDomains(const ListOfDomains& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfDomains.
 */
ListOfDomains&
ListOfDomains::operator=(const ListOfDomains& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfDomains object.
 */
ListOfDomains*
ListOfDomains::clone() const
{
  return new ListOfDomains(*this);
}


/*
 * Destructor for ListOfDomains.
 */
ListOfDomains::~ListOfDomains()
{
}


/*
 * Get a Domain from the ListOfDomains.
 */
Domain*
ListOfDomains::get(unsigned int n)
{
  return static_cast<Domain*>(ListOf::get(n));
}


/*
 * Get a Domain from the ListOfDomains.
 */
const Domain*
ListOfDomains::get(unsigned int n) const
{
  return static_cast<const Domain*>(ListOf::get(n));
}


/*
 * Get a Domain from the ListOfDomains based on its identifier.
 */
Domain*
ListOfDomains::get(const std::string& sid)
{
  return const_cast<Domain*>(static_cast<const
    ListOfDomains&>(*this).get(sid));
}


/*
 * Get a Domain from the ListOfDomains based on its identifier.
 */
const Domain*
ListOfDomains::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<Domain>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Domain*> (*result);
}


/*
 * Removes the nth Domain from this ListOfDomains and returns a pointer to it.
 */
Domain*
ListOfDomains::remove(unsigned int n)
{
  return static_cast<Domain*>(ListOf::remove(n));
}


/*
 * Removes the Domain from this ListOfDomains based on its identifier and
 * returns a pointer to it.
 */
Domain*
ListOfDomains::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<Domain>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Domain*> (item);
}


/*
 * Adds a copy of the given Domain to this ListOfDomains.
 */
int
ListOfDomains::addDomain(const Domain* d)
{
  if (d == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (d->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (d->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != d->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != d->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(d)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(d);
  }
}


/*
 * Get the number of Domain objects in this ListOfDomains.
 */
unsigned int
ListOfDomains::getNumDomains() const
{
  return size();
}


/*
 * Creates a new Domain object, adds it to this ListOfDomains object and
 * returns the Domain object created.
 */
Domain*
ListOfDomains::createDomain()
{
  Domain* d = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    d = new Domain(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (d != NULL)
  {
    appendAndOwn(d);
  }

  return d;
}


/*
 * Used by ListOfDomains::get() to lookup a Domain based on its DomainType.
 */
struct IdEqDT3
{
  const string& id;
   
  IdEqDT3 (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<Domain*>(sb)->getDomainType() == id);
  }
};


/*
 * Get a Domain from the ListOfDomains based on the DomainType to which it
 * refers.
 */
const Domain*
ListOfDomains::getByDomainType(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqDT3(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Domain*> (*result);
}


/*
 * Get a Domain from the ListOfDomains based on the DomainType to which it
 * refers.
 */
Domain*
ListOfDomains::getByDomainType(const std::string& sid)
{
  return const_cast<Domain*>(static_cast<const
    ListOfDomains&>(*this).getByDomainType(sid));
}


/*
 * Returns the XML element name of this ListOfDomains object.
 */
const std::string&
ListOfDomains::getElementName() const
{
  static const string name = "listOfDomains";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfDomains object.
 */
int
ListOfDomains::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfDomains object.
 */
int
ListOfDomains::getItemTypeCode() const
{
  return SBML_SPATIAL_DOMAIN;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Domain in this ListOfDomains
 */
SBase*
ListOfDomains::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "domain")
  {
    object = new Domain(spatialns);
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
ListOfDomains::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a Domain_t from the ListOf_t.
 */
LIBSBML_EXTERN
Domain_t*
ListOfDomains_getDomain(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDomains*>(lo)->get(n);
}


/*
 * Get a Domain_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
Domain_t*
ListOfDomains_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDomains*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth Domain_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
Domain_t*
ListOfDomains_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDomains*>(lo)->remove(n);
}


/*
 * Removes the Domain_t from this ListOf_t based on its identifier and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
Domain_t*
ListOfDomains_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDomains*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


