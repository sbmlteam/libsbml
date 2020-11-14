/**
 * @file ListOfInteriorPoints.cpp
 * @brief Implementation of the ListOfInteriorPoints class.
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
#include <sbml/packages/spatial/sbml/ListOfInteriorPoints.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfInteriorPoints using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfInteriorPoints::ListOfInteriorPoints(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfInteriorPoints using the given SpatialPkgNamespaces
 * object.
 */
ListOfInteriorPoints::ListOfInteriorPoints(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfInteriorPoints.
 */
ListOfInteriorPoints::ListOfInteriorPoints(const ListOfInteriorPoints& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfInteriorPoints.
 */
ListOfInteriorPoints&
ListOfInteriorPoints::operator=(const ListOfInteriorPoints& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfInteriorPoints object.
 */
ListOfInteriorPoints*
ListOfInteriorPoints::clone() const
{
  return new ListOfInteriorPoints(*this);
}


/*
 * Destructor for ListOfInteriorPoints.
 */
ListOfInteriorPoints::~ListOfInteriorPoints()
{
}


/*
 * Get an InteriorPoint from the ListOfInteriorPoints.
 */
InteriorPoint*
ListOfInteriorPoints::get(unsigned int n)
{
  return static_cast<InteriorPoint*>(ListOf::get(n));
}


/*
 * Get an InteriorPoint from the ListOfInteriorPoints.
 */
const InteriorPoint*
ListOfInteriorPoints::get(unsigned int n) const
{
  return static_cast<const InteriorPoint*>(ListOf::get(n));
}


/*
 * Get an InteriorPoint from the ListOfInteriorPoints based on its identifier.
 */
InteriorPoint*
ListOfInteriorPoints::get(const std::string& sid)
{
  return const_cast<InteriorPoint*>(static_cast<const
    ListOfInteriorPoints&>(*this).get(sid));
}


/*
 * Get an InteriorPoint from the ListOfInteriorPoints based on its identifier.
 */
const InteriorPoint*
ListOfInteriorPoints::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<InteriorPoint>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const InteriorPoint*>
    (*result);
}


/*
 * Removes the nth InteriorPoint from this ListOfInteriorPoints and returns a
 * pointer to it.
 */
InteriorPoint*
ListOfInteriorPoints::remove(unsigned int n)
{
  return static_cast<InteriorPoint*>(ListOf::remove(n));
}


/*
 * Removes the InteriorPoint from this ListOfInteriorPoints based on its
 * identifier and returns a pointer to it.
 */
InteriorPoint*
ListOfInteriorPoints::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<InteriorPoint>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <InteriorPoint*> (item);
}


/*
 * Adds a copy of the given InteriorPoint to this ListOfInteriorPoints.
 */
int
ListOfInteriorPoints::addInteriorPoint(const InteriorPoint* ip)
{
  if (ip == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ip->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ip->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ip->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(ip)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(ip);
  }
}


/*
 * Get the number of InteriorPoint objects in this ListOfInteriorPoints.
 */
unsigned int
ListOfInteriorPoints::getNumInteriorPoints() const
{
  return size();
}


/*
 * Creates a new InteriorPoint object, adds it to this ListOfInteriorPoints
 * object and returns the InteriorPoint object created.
 */
InteriorPoint*
ListOfInteriorPoints::createInteriorPoint()
{
  InteriorPoint* ip = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    ip = new InteriorPoint(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (ip != NULL)
  {
    appendAndOwn(ip);
  }

  return ip;
}


/*
 * Returns the XML element name of this ListOfInteriorPoints object.
 */
const std::string&
ListOfInteriorPoints::getElementName() const
{
  static const string name = "listOfInteriorPoints";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfInteriorPoints object.
 */
int
ListOfInteriorPoints::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfInteriorPoints object.
 */
int
ListOfInteriorPoints::getItemTypeCode() const
{
  return SBML_SPATIAL_INTERIORPOINT;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new InteriorPoint in this ListOfInteriorPoints
 */
SBase*
ListOfInteriorPoints::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "interiorPoint")
  {
    object = new InteriorPoint(spatialns);
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
ListOfInteriorPoints::writeXMLNS(XMLOutputStream& stream) const
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
 * Get an InteriorPoint_t from the ListOf_t.
 */
LIBSBML_EXTERN
InteriorPoint_t*
ListOfInteriorPoints_getInteriorPoint(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfInteriorPoints*>(lo)->get(n);
}


/*
 * Get an InteriorPoint_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
InteriorPoint_t*
ListOfInteriorPoints_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfInteriorPoints*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth InteriorPoint_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
InteriorPoint_t*
ListOfInteriorPoints_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfInteriorPoints*>(lo)->remove(n);
}


/*
 * Removes the InteriorPoint_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
InteriorPoint_t*
ListOfInteriorPoints_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfInteriorPoints*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


