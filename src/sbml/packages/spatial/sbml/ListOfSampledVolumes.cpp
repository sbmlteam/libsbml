/**
 * @file ListOfSampledVolumes.cpp
 * @brief Implementation of the ListOfSampledVolumes class.
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
#include <sbml/packages/spatial/sbml/ListOfSampledVolumes.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfSampledVolumes using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfSampledVolumes::ListOfSampledVolumes(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfSampledVolumes using the given SpatialPkgNamespaces
 * object.
 */
ListOfSampledVolumes::ListOfSampledVolumes(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfSampledVolumes.
 */
ListOfSampledVolumes::ListOfSampledVolumes(const ListOfSampledVolumes& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfSampledVolumes.
 */
ListOfSampledVolumes&
ListOfSampledVolumes::operator=(const ListOfSampledVolumes& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfSampledVolumes object.
 */
ListOfSampledVolumes*
ListOfSampledVolumes::clone() const
{
  return new ListOfSampledVolumes(*this);
}


/*
 * Destructor for ListOfSampledVolumes.
 */
ListOfSampledVolumes::~ListOfSampledVolumes()
{
}


/*
 * Get a SampledVolume from the ListOfSampledVolumes.
 */
SampledVolume*
ListOfSampledVolumes::get(unsigned int n)
{
  return static_cast<SampledVolume*>(ListOf::get(n));
}


/*
 * Get a SampledVolume from the ListOfSampledVolumes.
 */
const SampledVolume*
ListOfSampledVolumes::get(unsigned int n) const
{
  return static_cast<const SampledVolume*>(ListOf::get(n));
}


/*
 * Get a SampledVolume from the ListOfSampledVolumes based on its identifier.
 */
SampledVolume*
ListOfSampledVolumes::get(const std::string& sid)
{
  return const_cast<SampledVolume*>(static_cast<const
    ListOfSampledVolumes&>(*this).get(sid));
}


/*
 * Get a SampledVolume from the ListOfSampledVolumes based on its identifier.
 */
const SampledVolume*
ListOfSampledVolumes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<SampledVolume>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const SampledVolume*>
    (*result);
}


/*
 * Removes the nth SampledVolume from this ListOfSampledVolumes and returns a
 * pointer to it.
 */
SampledVolume*
ListOfSampledVolumes::remove(unsigned int n)
{
  return static_cast<SampledVolume*>(ListOf::remove(n));
}


/*
 * Removes the SampledVolume from this ListOfSampledVolumes based on its
 * identifier and returns a pointer to it.
 */
SampledVolume*
ListOfSampledVolumes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<SampledVolume>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SampledVolume*> (item);
}


/*
 * Adds a copy of the given SampledVolume to this ListOfSampledVolumes.
 */
int
ListOfSampledVolumes::addSampledVolume(const SampledVolume* sv)
{
  if (sv == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sv->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sv->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sv->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(sv)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(sv);
  }
}


/*
 * Get the number of SampledVolume objects in this ListOfSampledVolumes.
 */
unsigned int
ListOfSampledVolumes::getNumSampledVolumes() const
{
  return size();
}


/*
 * Creates a new SampledVolume object, adds it to this ListOfSampledVolumes
 * object and returns the SampledVolume object created.
 */
SampledVolume*
ListOfSampledVolumes::createSampledVolume()
{
  SampledVolume* sv = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sv = new SampledVolume(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (sv != NULL)
  {
    appendAndOwn(sv);
  }

  return sv;
}


/*
 * Used by ListOfSampledVolumes::get() to lookup a SampledVolume based on its
 * DomainType.
 */
struct IdEqDT5
{
  const string& id;
   
  IdEqDT5 (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<SampledVolume*>(sb)->getDomainType() == id);
  }
};


/*
 * Get a SampledVolume from the ListOfSampledVolumes based on the DomainType to
 * which it refers.
 */
const SampledVolume*
ListOfSampledVolumes::getByDomainType(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqDT5(sid));
  return (result == mItems.end()) ? 0 : static_cast <const SampledVolume*>
    (*result);
}


/*
 * Get a SampledVolume from the ListOfSampledVolumes based on the DomainType to
 * which it refers.
 */
SampledVolume*
ListOfSampledVolumes::getByDomainType(const std::string& sid)
{
  return const_cast<SampledVolume*>(static_cast<const
    ListOfSampledVolumes&>(*this).getByDomainType(sid));
}


/*
 * Returns the XML element name of this ListOfSampledVolumes object.
 */
const std::string&
ListOfSampledVolumes::getElementName() const
{
  static const string name = "listOfSampledVolumes";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfSampledVolumes object.
 */
int
ListOfSampledVolumes::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfSampledVolumes object.
 */
int
ListOfSampledVolumes::getItemTypeCode() const
{
  return SBML_SPATIAL_SAMPLEDVOLUME;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SampledVolume in this ListOfSampledVolumes
 */
SBase*
ListOfSampledVolumes::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "sampledVolume")
  {
    object = new SampledVolume(spatialns);
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
ListOfSampledVolumes::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a SampledVolume_t from the ListOf_t.
 */
LIBSBML_EXTERN
SampledVolume_t*
ListOfSampledVolumes_getSampledVolume(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfSampledVolumes*>(lo)->get(n);
}


/*
 * Get a SampledVolume_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
SampledVolume_t*
ListOfSampledVolumes_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfSampledVolumes*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth SampledVolume_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
SampledVolume_t*
ListOfSampledVolumes_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfSampledVolumes*>(lo)->remove(n);
}


/*
 * Removes the SampledVolume_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
SampledVolume_t*
ListOfSampledVolumes_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfSampledVolumes*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


