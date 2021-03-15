/**
 * @file ListOfAdjacentDomains.cpp
 * @brief Implementation of the ListOfAdjacentDomains class.
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
#include <sbml/packages/spatial/sbml/ListOfAdjacentDomains.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfAdjacentDomains using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfAdjacentDomains::ListOfAdjacentDomains(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfAdjacentDomains using the given SpatialPkgNamespaces
 * object.
 */
ListOfAdjacentDomains::ListOfAdjacentDomains(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfAdjacentDomains.
 */
ListOfAdjacentDomains::ListOfAdjacentDomains(const ListOfAdjacentDomains& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfAdjacentDomains.
 */
ListOfAdjacentDomains&
ListOfAdjacentDomains::operator=(const ListOfAdjacentDomains& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfAdjacentDomains object.
 */
ListOfAdjacentDomains*
ListOfAdjacentDomains::clone() const
{
  return new ListOfAdjacentDomains(*this);
}


/*
 * Destructor for ListOfAdjacentDomains.
 */
ListOfAdjacentDomains::~ListOfAdjacentDomains()
{
}


/*
 * Get an AdjacentDomains from the ListOfAdjacentDomains.
 */
AdjacentDomains*
ListOfAdjacentDomains::get(unsigned int n)
{
  return static_cast<AdjacentDomains*>(ListOf::get(n));
}


/*
 * Get an AdjacentDomains from the ListOfAdjacentDomains.
 */
const AdjacentDomains*
ListOfAdjacentDomains::get(unsigned int n) const
{
  return static_cast<const AdjacentDomains*>(ListOf::get(n));
}


/*
 * Get an AdjacentDomains from the ListOfAdjacentDomains based on its
 * identifier.
 */
AdjacentDomains*
ListOfAdjacentDomains::get(const std::string& sid)
{
  return const_cast<AdjacentDomains*>(static_cast<const
    ListOfAdjacentDomains&>(*this).get(sid));
}


/*
 * Get an AdjacentDomains from the ListOfAdjacentDomains based on its
 * identifier.
 */
const AdjacentDomains*
ListOfAdjacentDomains::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<AdjacentDomains>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const AdjacentDomains*>
    (*result);
}


/*
 * Removes the nth AdjacentDomains from this ListOfAdjacentDomains and returns
 * a pointer to it.
 */
AdjacentDomains*
ListOfAdjacentDomains::remove(unsigned int n)
{
  return static_cast<AdjacentDomains*>(ListOf::remove(n));
}


/*
 * Removes the AdjacentDomains from this ListOfAdjacentDomains based on its
 * identifier and returns a pointer to it.
 */
AdjacentDomains*
ListOfAdjacentDomains::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<AdjacentDomains>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <AdjacentDomains*> (item);
}


/*
 * Adds a copy of the given AdjacentDomains to this ListOfAdjacentDomains.
 */
int
ListOfAdjacentDomains::addAdjacentDomains(const AdjacentDomains* ad)
{
  if (ad == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ad->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ad->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ad->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(ad)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(ad);
  }
}


/*
 * Get the number of AdjacentDomains objects in this ListOfAdjacentDomains.
 */
unsigned int
ListOfAdjacentDomains::getNumAdjacentDomains() const
{
  return size();
}


/*
 * Creates a new AdjacentDomains object, adds it to this ListOfAdjacentDomains
 * object and returns the AdjacentDomains object created.
 */
AdjacentDomains*
ListOfAdjacentDomains::createAdjacentDomains()
{
  AdjacentDomains* ad = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    ad = new AdjacentDomains(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (ad != NULL)
  {
    appendAndOwn(ad);
  }

  return ad;
}


/*
 * Used by ListOfAdjacentDomains::get() to lookup an AdjacentDomains based on
 * its Domain1.
 */
struct IdEqD
{
  const string& id;
   
  IdEqD (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<AdjacentDomains*>(sb)->getDomain1() == id);
  }
};


/*
 * Get an AdjacentDomains from the ListOfAdjacentDomains based on the Domain1
 * to which it refers.
 */
const AdjacentDomains*
ListOfAdjacentDomains::getByDomain1(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqD(sid));
  return (result == mItems.end()) ? 0 : static_cast <const AdjacentDomains*>
    (*result);
}


/*
 * Get an AdjacentDomains from the ListOfAdjacentDomains based on the Domain1
 * to which it refers.
 */
AdjacentDomains*
ListOfAdjacentDomains::getByDomain1(const std::string& sid)
{
  return const_cast<AdjacentDomains*>(static_cast<const
    ListOfAdjacentDomains&>(*this).getByDomain1(sid));
}


/*
 * Used by ListOfAdjacentDomains::get() to lookup an AdjacentDomains based on
 * its Domain2.
 */
struct IdEqD_A
{
  const string& id;
   
  IdEqD_A (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<AdjacentDomains*>(sb)->getDomain2() == id);
  }
};


/*
 * Get an AdjacentDomains from the ListOfAdjacentDomains based on the Domain2
 * to which it refers.
 */
const AdjacentDomains*
ListOfAdjacentDomains::getByDomain2(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqD(sid));
  return (result == mItems.end()) ? 0 : static_cast <const AdjacentDomains*>
    (*result);
}


/*
 * Get an AdjacentDomains from the ListOfAdjacentDomains based on the Domain2
 * to which it refers.
 */
AdjacentDomains*
ListOfAdjacentDomains::getByDomain2(const std::string& sid)
{
  return const_cast<AdjacentDomains*>(static_cast<const
    ListOfAdjacentDomains&>(*this).getByDomain2(sid));
}


/*
 * Returns the XML element name of this ListOfAdjacentDomains object.
 */
const std::string&
ListOfAdjacentDomains::getElementName() const
{
  static const string name = "listOfAdjacentDomains";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfAdjacentDomains object.
 */
int
ListOfAdjacentDomains::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfAdjacentDomains object.
 */
int
ListOfAdjacentDomains::getItemTypeCode() const
{
  return SBML_SPATIAL_ADJACENTDOMAINS;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new AdjacentDomains in this ListOfAdjacentDomains
 */
SBase*
ListOfAdjacentDomains::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "adjacentDomains")
  {
    object = new AdjacentDomains(spatialns);
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
ListOfAdjacentDomains::writeXMLNS(XMLOutputStream& stream) const
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
 * Get an AdjacentDomains_t from the ListOf_t.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
ListOfAdjacentDomains_getAdjacentDomains(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfAdjacentDomains*>(lo)->get(n);
}


/*
 * Get an AdjacentDomains_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
ListOfAdjacentDomains_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfAdjacentDomains*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth AdjacentDomains_t from this ListOf_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
ListOfAdjacentDomains_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfAdjacentDomains*>(lo)->remove(n);
}


/*
 * Removes the AdjacentDomains_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
ListOfAdjacentDomains_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfAdjacentDomains*>(lo)->remove(sid)
    : NULL;
}




LIBSBML_CPP_NAMESPACE_END


