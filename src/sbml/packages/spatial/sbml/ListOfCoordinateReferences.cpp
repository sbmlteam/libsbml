/**
 * @file ListOfCoordinateReferences.cpp
 * @brief Implementation of the ListOfCoordinateReferences class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
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
#include <sbml/packages/spatial/sbml/ListOfCoordinateReferences.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfCoordinateReferences using the given SBML Level, Version
 * and &ldquo;spatial&rdquo; package version.
 */
ListOfCoordinateReferences::ListOfCoordinateReferences(unsigned int level,
                                                       unsigned int version,
                                                       unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfCoordinateReferences using the given
 * SpatialPkgNamespaces object.
 */
ListOfCoordinateReferences::ListOfCoordinateReferences(SpatialPkgNamespaces
  *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfCoordinateReferences.
 */
ListOfCoordinateReferences::ListOfCoordinateReferences(const
  ListOfCoordinateReferences& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfCoordinateReferences.
 */
ListOfCoordinateReferences&
ListOfCoordinateReferences::operator=(const ListOfCoordinateReferences& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfCoordinateReferences object.
 */
ListOfCoordinateReferences*
ListOfCoordinateReferences::clone() const
{
  return new ListOfCoordinateReferences(*this);
}


/*
 * Destructor for ListOfCoordinateReferences.
 */
ListOfCoordinateReferences::~ListOfCoordinateReferences()
{
}


/*
 * Get a CoordinateReference from the ListOfCoordinateReferences.
 */
CoordinateReference*
ListOfCoordinateReferences::get(unsigned int n)
{
  return static_cast<CoordinateReference*>(ListOf::get(n));
}


/*
 * Get a CoordinateReference from the ListOfCoordinateReferences.
 */
const CoordinateReference*
ListOfCoordinateReferences::get(unsigned int n) const
{
  return static_cast<const CoordinateReference*>(ListOf::get(n));
}


/*
 * Get a CoordinateReference from the ListOfCoordinateReferences based on its
 * identifier.
 */
CoordinateReference*
ListOfCoordinateReferences::get(const std::string& sid)
{
  return const_cast<CoordinateReference*>(static_cast<const
    ListOfCoordinateReferences&>(*this).get(sid));
}


/*
 * Get a CoordinateReference from the ListOfCoordinateReferences based on its
 * identifier.
 */
const CoordinateReference*
ListOfCoordinateReferences::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(),
    IdEq<CoordinateReference>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    CoordinateReference*> (*result);
}


/*
 * Removes the nth CoordinateReference from this ListOfCoordinateReferences and
 * returns a pointer to it.
 */
CoordinateReference*
ListOfCoordinateReferences::remove(unsigned int n)
{
  return static_cast<CoordinateReference*>(ListOf::remove(n));
}


/*
 * Removes the CoordinateReference from this ListOfCoordinateReferences based
 * on its identifier and returns a pointer to it.
 */
CoordinateReference*
ListOfCoordinateReferences::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(),
    IdEq<CoordinateReference>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <CoordinateReference*> (item);
}


/*
 * Adds a copy of the given CoordinateReference to this
 * ListOfCoordinateReferences.
 */
int
ListOfCoordinateReferences::addCoordinateReference(const CoordinateReference*
  cr)
{
  if (cr == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (cr->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != cr->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != cr->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(cr)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(cr);
  }
}


/*
 * Get the number of CoordinateReference objects in this
 * ListOfCoordinateReferences.
 */
unsigned int
ListOfCoordinateReferences::getNumCoordinateReferences() const
{
  return size();
}


/*
 * Creates a new CoordinateReference object, adds it to this
 * ListOfCoordinateReferences object and returns the CoordinateReference object
 * created.
 */
CoordinateReference*
ListOfCoordinateReferences::createCoordinateReference()
{
  CoordinateReference* cr = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    cr = new CoordinateReference(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (cr != NULL)
  {
    appendAndOwn(cr);
  }

  return cr;
}


/*
 * Returns the XML element name of this ListOfCoordinateReferences object.
 */
const std::string&
ListOfCoordinateReferences::getElementName() const
{
  static const string name = "listOfCoordinateReferences";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfCoordinateReferences object.
 */
int
ListOfCoordinateReferences::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfCoordinateReferences object.
 */
int
ListOfCoordinateReferences::getItemTypeCode() const
{
  return SBML_SPATIAL_COORDINATEREFERENCE;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new CoordinateReference in this ListOfCoordinateReferences
 */
SBase*
ListOfCoordinateReferences::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "coordinateReference")
  {
    object = new CoordinateReference(spatialns);
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
ListOfCoordinateReferences::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a CoordinateReference_t from the ListOf_t.
 */
LIBSBML_EXTERN
const CoordinateReference_t*
ListOfCoordinateReferences_getCoordinateReference(ListOf_t* lo,
                                                  unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfCoordinateReferences*>(lo)->get(n);
}


/*
 * Get a CoordinateReference_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
const CoordinateReference_t*
ListOfCoordinateReferences_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfCoordinateReferences*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth CoordinateReference_t from this ListOf_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
CoordinateReference_t*
ListOfCoordinateReferences_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfCoordinateReferences*>(lo)->remove(n);
}


/*
 * Removes the CoordinateReference_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
CoordinateReference_t*
ListOfCoordinateReferences_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfCoordinateReferences*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


