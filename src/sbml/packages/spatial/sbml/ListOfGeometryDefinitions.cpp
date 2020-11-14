/**
 * @file ListOfGeometryDefinitions.cpp
 * @brief Implementation of the ListOfGeometryDefinitions class.
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
#include <sbml/packages/spatial/sbml/ListOfGeometryDefinitions.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>

#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/sbml/CSGeometry.h>
#include <sbml/packages/spatial/sbml/ParametricGeometry.h>
#include <sbml/packages/spatial/sbml/MixedGeometry.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfGeometryDefinitions using the given SBML Level, Version
 * and &ldquo;spatial&rdquo; package version.
 */
ListOfGeometryDefinitions::ListOfGeometryDefinitions(unsigned int level,
                                                     unsigned int version,
                                                     unsigned int pkgVersion)
  : ListOf(level, version)
  , mElementName("listOfGeometryDefinitions")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfGeometryDefinitions using the given SpatialPkgNamespaces
 * object.
 */
ListOfGeometryDefinitions::ListOfGeometryDefinitions(SpatialPkgNamespaces
  *spatialns)
  : ListOf(spatialns)
  , mElementName("listOfGeometryDefinitions")
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfGeometryDefinitions.
 */
ListOfGeometryDefinitions::ListOfGeometryDefinitions(const
  ListOfGeometryDefinitions& orig)
  : ListOf( orig )
  , mElementName ( orig.mElementName )
{
}


/*
 * Assignment operator for ListOfGeometryDefinitions.
 */
ListOfGeometryDefinitions&
ListOfGeometryDefinitions::operator=(const ListOfGeometryDefinitions& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfGeometryDefinitions object.
 */
ListOfGeometryDefinitions*
ListOfGeometryDefinitions::clone() const
{
  return new ListOfGeometryDefinitions(*this);
}


/*
 * Destructor for ListOfGeometryDefinitions.
 */
ListOfGeometryDefinitions::~ListOfGeometryDefinitions()
{
}


/*
 * Get a GeometryDefinition from the ListOfGeometryDefinitions.
 */
GeometryDefinition*
ListOfGeometryDefinitions::get(unsigned int n)
{
  return static_cast<GeometryDefinition*>(ListOf::get(n));
}


/*
 * Get a GeometryDefinition from the ListOfGeometryDefinitions.
 */
const GeometryDefinition*
ListOfGeometryDefinitions::get(unsigned int n) const
{
  return static_cast<const GeometryDefinition*>(ListOf::get(n));
}


/*
 * Get a GeometryDefinition from the ListOfGeometryDefinitions based on its
 * identifier.
 */
GeometryDefinition*
ListOfGeometryDefinitions::get(const std::string& sid)
{
  return const_cast<GeometryDefinition*>(static_cast<const
    ListOfGeometryDefinitions&>(*this).get(sid));
}


/*
 * Get a GeometryDefinition from the ListOfGeometryDefinitions based on its
 * identifier.
 */
const GeometryDefinition*
ListOfGeometryDefinitions::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(),
    IdEq<GeometryDefinition>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const GeometryDefinition*>
    (*result);
}


/*
 * Removes the nth GeometryDefinition from this ListOfGeometryDefinitions and
 * returns a pointer to it.
 */
GeometryDefinition*
ListOfGeometryDefinitions::remove(unsigned int n)
{
  return static_cast<GeometryDefinition*>(ListOf::remove(n));
}


/*
 * Removes the GeometryDefinition from this ListOfGeometryDefinitions based on
 * its identifier and returns a pointer to it.
 */
GeometryDefinition*
ListOfGeometryDefinitions::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(),
    IdEq<GeometryDefinition>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <GeometryDefinition*> (item);
}


/*
 * Adds a copy of the given GeometryDefinition to this
 * ListOfGeometryDefinitions.
 */
int
ListOfGeometryDefinitions::addGeometryDefinition(const GeometryDefinition* gd)
{
  if (gd == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gd->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != gd->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != gd->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(gd)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(gd);
  }
}


/*
 * Get the number of GeometryDefinition objects in this
 * ListOfGeometryDefinitions.
 */
unsigned int
ListOfGeometryDefinitions::getNumGeometryDefinitions() const
{
  return size();
}


/*
 * Creates a new AnalyticGeometry object, adds it to this
 * ListOfGeometryDefinitions object and returns the AnalyticGeometry object
 * created.
 */
AnalyticGeometry*
ListOfGeometryDefinitions::createAnalyticGeometry()
{
  AnalyticGeometry* ag = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    ag = new AnalyticGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (ag != NULL)
  {
    appendAndOwn(ag);
  }

  return ag;
}


/*
 * Creates a new SampledFieldGeometry object, adds it to this
 * ListOfGeometryDefinitions object and returns the SampledFieldGeometry object
 * created.
 */
SampledFieldGeometry*
ListOfGeometryDefinitions::createSampledFieldGeometry()
{
  SampledFieldGeometry* sfg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sfg = new SampledFieldGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (sfg != NULL)
  {
    appendAndOwn(sfg);
  }

  return sfg;
}


/*
 * Creates a new CSGeometry object, adds it to this ListOfGeometryDefinitions
 * object and returns the CSGeometry object created.
 */
CSGeometry*
ListOfGeometryDefinitions::createCSGeometry()
{
  CSGeometry* csg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    csg = new CSGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (csg != NULL)
  {
    appendAndOwn(csg);
  }

  return csg;
}


/*
 * Creates a new ParametricGeometry object, adds it to this
 * ListOfGeometryDefinitions object and returns the ParametricGeometry object
 * created.
 */
ParametricGeometry*
ListOfGeometryDefinitions::createParametricGeometry()
{
  ParametricGeometry* pg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    pg = new ParametricGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (pg != NULL)
  {
    appendAndOwn(pg);
  }

  return pg;
}


/*
 * Creates a new MixedGeometry object, adds it to this
 * ListOfGeometryDefinitions object and returns the MixedGeometry object
 * created.
 */
MixedGeometry*
ListOfGeometryDefinitions::createMixedGeometry()
{
  MixedGeometry* mg = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    mg = new MixedGeometry(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (mg != NULL)
  {
    appendAndOwn(mg);
  }

  return mg;
}


/*
 * Returns the XML element name of this ListOfGeometryDefinitions object.
 */
const std::string&
ListOfGeometryDefinitions::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this ListOfGeometryDefinitions object.
 */
void
ListOfGeometryDefinitions::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this ListOfGeometryDefinitions object.
 */
int
ListOfGeometryDefinitions::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfGeometryDefinitions object.
 */
int
ListOfGeometryDefinitions::getItemTypeCode() const
{
  return SBML_SPATIAL_GEOMETRYDEFINITION;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new GeometryDefinition in this ListOfGeometryDefinitions
 */
SBase*
ListOfGeometryDefinitions::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "geometryDefinition")
  {
    object = new GeometryDefinition(spatialns);
    appendAndOwn(object);
  }

  if (name == "analyticGeometry")
  {
    object = new AnalyticGeometry(spatialns);
    appendAndOwn(object);
  }

  if (name == "sampledFieldGeometry")
  {
    object = new SampledFieldGeometry(spatialns);
    appendAndOwn(object);
  }

  if (name == "csGeometry")
  {
    object = new CSGeometry(spatialns);
    appendAndOwn(object);
  }

  if (name == "csgeometry")
  {
    object = new CSGeometry(spatialns);
    appendAndOwn(object);
  }

  if (name == "parametricGeometry")
  {
    object = new ParametricGeometry(spatialns);
    appendAndOwn(object);
  }

  if (name == "mixedGeometry")
  {
    object = new MixedGeometry(spatialns);
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
ListOfGeometryDefinitions::writeXMLNS(XMLOutputStream& stream) const
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



/** @cond doxygenLibsbmlInternal */

/*
 * checks concrete types
 */
bool
ListOfGeometryDefinitions::isValidTypeForList(SBase* item)
{
  unsigned int tc = item->getTypeCode();

  return ((tc == SBML_SPATIAL_ANALYTICGEOMETRY) || (tc ==
    SBML_SPATIAL_SAMPLEDFIELDGEOMETRY) || (tc == SBML_SPATIAL_CSGEOMETRY) || (tc
      == SBML_SPATIAL_PARAMETRICGEOMETRY) || (tc == SBML_SPATIAL_MIXEDGEOMETRY));
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get a GeometryDefinition_t from the ListOf_t.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
ListOfGeometryDefinitions_getGeometryDefinition(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGeometryDefinitions*>(lo)->get(n);
}


/*
 * Get a GeometryDefinition_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
ListOfGeometryDefinitions_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfGeometryDefinitions*>(lo)->get(sid)
    : NULL;
}


/*
 * Removes the nth GeometryDefinition_t from this ListOf_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
ListOfGeometryDefinitions_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGeometryDefinitions*>(lo)->remove(n);
}


/*
 * Removes the GeometryDefinition_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
ListOfGeometryDefinitions_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfGeometryDefinitions*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


