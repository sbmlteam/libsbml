/**
 * @file ListOfAnalyticVolumes.cpp
 * @brief Implementation of the ListOfAnalyticVolumes class.
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
#include <sbml/packages/spatial/sbml/ListOfAnalyticVolumes.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/math/MathML.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfAnalyticVolumes using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfAnalyticVolumes::ListOfAnalyticVolumes(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfAnalyticVolumes using the given SpatialPkgNamespaces
 * object.
 */
ListOfAnalyticVolumes::ListOfAnalyticVolumes(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfAnalyticVolumes.
 */
ListOfAnalyticVolumes::ListOfAnalyticVolumes(const ListOfAnalyticVolumes& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfAnalyticVolumes.
 */
ListOfAnalyticVolumes&
ListOfAnalyticVolumes::operator=(const ListOfAnalyticVolumes& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfAnalyticVolumes object.
 */
ListOfAnalyticVolumes*
ListOfAnalyticVolumes::clone() const
{
  return new ListOfAnalyticVolumes(*this);
}


/*
 * Destructor for ListOfAnalyticVolumes.
 */
ListOfAnalyticVolumes::~ListOfAnalyticVolumes()
{
}


/*
 * Get an AnalyticVolume from the ListOfAnalyticVolumes.
 */
AnalyticVolume*
ListOfAnalyticVolumes::get(unsigned int n)
{
  return static_cast<AnalyticVolume*>(ListOf::get(n));
}


/*
 * Get an AnalyticVolume from the ListOfAnalyticVolumes.
 */
const AnalyticVolume*
ListOfAnalyticVolumes::get(unsigned int n) const
{
  return static_cast<const AnalyticVolume*>(ListOf::get(n));
}


/*
 * Get an AnalyticVolume from the ListOfAnalyticVolumes based on its
 * identifier.
 */
AnalyticVolume*
ListOfAnalyticVolumes::get(const std::string& sid)
{
  return const_cast<AnalyticVolume*>(static_cast<const
    ListOfAnalyticVolumes&>(*this).get(sid));
}


/*
 * Get an AnalyticVolume from the ListOfAnalyticVolumes based on its
 * identifier.
 */
const AnalyticVolume*
ListOfAnalyticVolumes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<AnalyticVolume>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const AnalyticVolume*>
    (*result);
}


/*
 * Removes the nth AnalyticVolume from this ListOfAnalyticVolumes and returns a
 * pointer to it.
 */
AnalyticVolume*
ListOfAnalyticVolumes::remove(unsigned int n)
{
  return static_cast<AnalyticVolume*>(ListOf::remove(n));
}


/*
 * Removes the AnalyticVolume from this ListOfAnalyticVolumes based on its
 * identifier and returns a pointer to it.
 */
AnalyticVolume*
ListOfAnalyticVolumes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<AnalyticVolume>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <AnalyticVolume*> (item);
}


/*
 * Adds a copy of the given AnalyticVolume to this ListOfAnalyticVolumes.
 */
int
ListOfAnalyticVolumes::addAnalyticVolume(const AnalyticVolume* av)
{
  if (av == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (av->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (av->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != av->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != av->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(av)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(av);
  }
}


/*
 * Get the number of AnalyticVolume objects in this ListOfAnalyticVolumes.
 */
unsigned int
ListOfAnalyticVolumes::getNumAnalyticVolumes() const
{
  return size();
}


/*
 * Creates a new AnalyticVolume object, adds it to this ListOfAnalyticVolumes
 * object and returns the AnalyticVolume object created.
 */
AnalyticVolume*
ListOfAnalyticVolumes::createAnalyticVolume()
{
  AnalyticVolume* av = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    av = new AnalyticVolume(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (av != NULL)
  {
    appendAndOwn(av);
  }

  return av;
}


/*
 * Used by ListOfAnalyticVolumes::get() to lookup an AnalyticVolume based on
 * its DomainType.
 */
struct IdEqDT1
{
  const string& id;
   
  IdEqDT1 (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<AnalyticVolume*>(sb)->getDomainType() == id);
  }
};


/*
 * Get an AnalyticVolume from the ListOfAnalyticVolumes based on the DomainType
 * to which it refers.
 */
const AnalyticVolume*
ListOfAnalyticVolumes::getByDomainType(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqDT1(sid));
  return (result == mItems.end()) ? 0 : static_cast <const AnalyticVolume*>
    (*result);
}


/*
 * Get an AnalyticVolume from the ListOfAnalyticVolumes based on the DomainType
 * to which it refers.
 */
AnalyticVolume*
ListOfAnalyticVolumes::getByDomainType(const std::string& sid)
{
  return const_cast<AnalyticVolume*>(static_cast<const
    ListOfAnalyticVolumes&>(*this).getByDomainType(sid));
}


/*
 * Returns the XML element name of this ListOfAnalyticVolumes object.
 */
const std::string&
ListOfAnalyticVolumes::getElementName() const
{
  static const string name = "listOfAnalyticVolumes";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfAnalyticVolumes object.
 */
int
ListOfAnalyticVolumes::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfAnalyticVolumes object.
 */
int
ListOfAnalyticVolumes::getItemTypeCode() const
{
  return SBML_SPATIAL_ANALYTICVOLUME;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new AnalyticVolume in this ListOfAnalyticVolumes
 */
SBase*
ListOfAnalyticVolumes::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "analyticVolume")
  {
    object = new AnalyticVolume(spatialns);
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
ListOfAnalyticVolumes::writeXMLNS(XMLOutputStream& stream) const
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
 * Get an AnalyticVolume_t from the ListOf_t.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
ListOfAnalyticVolumes_getAnalyticVolume(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfAnalyticVolumes*>(lo)->get(n);
}


/*
 * Get an AnalyticVolume_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
ListOfAnalyticVolumes_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfAnalyticVolumes*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth AnalyticVolume_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
ListOfAnalyticVolumes_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfAnalyticVolumes*>(lo)->remove(n);
}


/*
 * Removes the AnalyticVolume_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
AnalyticVolume_t*
ListOfAnalyticVolumes_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfAnalyticVolumes*>(lo)->remove(sid)
    : NULL;
}




LIBSBML_CPP_NAMESPACE_END


