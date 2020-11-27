/**
 * @file Geometry.cpp
 * @brief Implementation of the Geometry class.
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
#include <sbml/packages/spatial/sbml/Geometry.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/spatial/sbml/AnalyticGeometry.h>
#include <sbml/packages/spatial/sbml/SampledFieldGeometry.h>
#include <sbml/packages/spatial/sbml/CSGeometry.h>
#include <sbml/packages/spatial/sbml/ParametricGeometry.h>
#include <sbml/packages/spatial/sbml/MixedGeometry.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Geometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
Geometry::Geometry(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
  : SBase(level, version)
  , mCoordinateSystem (SPATIAL_GEOMETRYKIND_INVALID)
  , mCoordinateComponents (level, version, pkgVersion)
  , mDomainTypes (level, version, pkgVersion)
  , mDomains (level, version, pkgVersion)
  , mAdjacentDomains (level, version, pkgVersion)
  , mGeometryDefinitions (level, version, pkgVersion)
  , mSampledFields (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new Geometry using the given SpatialPkgNamespaces object.
 */
Geometry::Geometry(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mCoordinateSystem (SPATIAL_GEOMETRYKIND_INVALID)
  , mCoordinateComponents (spatialns)
  , mDomainTypes (spatialns)
  , mDomains (spatialns)
  , mAdjacentDomains (spatialns)
  , mGeometryDefinitions (spatialns)
  , mSampledFields (spatialns)
{
  setElementNamespace(spatialns->getURI());
  connectToChild();
  loadPlugins(spatialns);
}


/*
 * Copy constructor for Geometry.
 */
Geometry::Geometry(const Geometry& orig)
  : SBase( orig )
  , mCoordinateSystem ( orig.mCoordinateSystem )
  , mCoordinateComponents ( orig.mCoordinateComponents )
  , mDomainTypes ( orig.mDomainTypes )
  , mDomains ( orig.mDomains )
  , mAdjacentDomains ( orig.mAdjacentDomains )
  , mGeometryDefinitions ( orig.mGeometryDefinitions )
  , mSampledFields ( orig.mSampledFields )
{
  connectToChild();
}


/*
 * Assignment operator for Geometry.
 */
Geometry&
Geometry::operator=(const Geometry& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mCoordinateSystem = rhs.mCoordinateSystem;
    mCoordinateComponents = rhs.mCoordinateComponents;
    mDomainTypes = rhs.mDomainTypes;
    mDomains = rhs.mDomains;
    mAdjacentDomains = rhs.mAdjacentDomains;
    mGeometryDefinitions = rhs.mGeometryDefinitions;
    mSampledFields = rhs.mSampledFields;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Geometry object.
 */
Geometry*
Geometry::clone() const
{
  return new Geometry(*this);
}


/*
 * Destructor for Geometry.
 */
Geometry::~Geometry()
{
}


/*
 * Returns the value of the "id" attribute of this Geometry.
 */
const std::string&
Geometry::getId() const
{
  return mId;
}


/*
 * Returns the value of the "coordinateSystem" attribute of this Geometry.
 */
GeometryKind_t
Geometry::getCoordinateSystem() const
{
  return mCoordinateSystem;
}


/*
 * Returns the value of the "coordinateSystem" attribute of this Geometry.
 */
std::string
Geometry::getCoordinateSystemAsString() const
{
  std::string code_str = GeometryKind_toString(mCoordinateSystem);
  return code_str;
}


/*
 * Predicate returning @c true if this Geometry's "id" attribute is set.
 */
bool
Geometry::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this Geometry's "coordinateSystem" attribute
 * is set.
 */
bool
Geometry::isSetCoordinateSystem() const
{
  return (mCoordinateSystem != SPATIAL_GEOMETRYKIND_INVALID);
}


/*
 * Sets the value of the "id" attribute of this Geometry.
 */
int
Geometry::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "coordinateSystem" attribute of this Geometry.
 */
int
Geometry::setCoordinateSystem(const GeometryKind_t coordinateSystem)
{
  if (GeometryKind_isValid(coordinateSystem) == 0)
  {
    mCoordinateSystem = SPATIAL_GEOMETRYKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoordinateSystem = coordinateSystem;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "coordinateSystem" attribute of this Geometry.
 */
int
Geometry::setCoordinateSystem(const std::string& coordinateSystem)
{
  mCoordinateSystem = GeometryKind_fromString(coordinateSystem.c_str());

  if (mCoordinateSystem == SPATIAL_GEOMETRYKIND_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this Geometry.
 */
int
Geometry::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "coordinateSystem" attribute of this Geometry.
 */
int
Geometry::unsetCoordinateSystem()
{
  mCoordinateSystem = SPATIAL_GEOMETRYKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the ListOfCoordinateComponents from this Geometry.
 */
const ListOfCoordinateComponents*
Geometry::getListOfCoordinateComponents() const
{
  return &mCoordinateComponents;
}


/*
 * Returns the ListOfCoordinateComponents from this Geometry.
 */
ListOfCoordinateComponents*
Geometry::getListOfCoordinateComponents()
{
  return &mCoordinateComponents;
}


/*
 * Get a CoordinateComponent from the Geometry.
 */
CoordinateComponent*
Geometry::getCoordinateComponent(unsigned int n)
{
  return mCoordinateComponents.get(n);
}


/*
 * Get a CoordinateComponent from the Geometry.
 */
const CoordinateComponent*
Geometry::getCoordinateComponent(unsigned int n) const
{
  return mCoordinateComponents.get(n);
}


/*
 * Get a CoordinateComponent from the Geometry based on its identifier.
 */
CoordinateComponent*
Geometry::getCoordinateComponent(const std::string& sid)
{
  return mCoordinateComponents.get(sid);
}


/*
 * Get a CoordinateComponent from the Geometry based on its identifier.
 */
const CoordinateComponent*
Geometry::getCoordinateComponent(const std::string& sid) const
{
  return mCoordinateComponents.get(sid);
}


/*
 * Adds a copy of the given CoordinateComponent to this Geometry.
 */
int
Geometry::addCoordinateComponent(const CoordinateComponent* cc)
{
  if (cc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (cc->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (cc->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != cc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != cc->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(cc)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (cc->isSetId() && (mCoordinateComponents.get(cc->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mCoordinateComponents.append(cc);
  }
}


/*
 * Get the number of CoordinateComponent objects in this Geometry.
 */
unsigned int
Geometry::getNumCoordinateComponents() const
{
  return mCoordinateComponents.size();
}


/*
 * Creates a new CoordinateComponent object, adds it to this Geometry object
 * and returns the CoordinateComponent object created.
 */
CoordinateComponent*
Geometry::createCoordinateComponent()
{
  CoordinateComponent* cc = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    cc = new CoordinateComponent(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (cc != NULL)
  {
    mCoordinateComponents.appendAndOwn(cc);
  }

  return cc;
}


/*
 * Removes the nth CoordinateComponent from this Geometry and returns a pointer
 * to it.
 */
CoordinateComponent*
Geometry::removeCoordinateComponent(unsigned int n)
{
  return mCoordinateComponents.remove(n);
}


/*
 * Removes the CoordinateComponent from this Geometry based on its identifier
 * and returns a pointer to it.
 */
CoordinateComponent*
Geometry::removeCoordinateComponent(const std::string& sid)
{
  return mCoordinateComponents.remove(sid);
}


/*
 * Returns the ListOfDomainTypes from this Geometry.
 */
const ListOfDomainTypes*
Geometry::getListOfDomainTypes() const
{
  return &mDomainTypes;
}


/*
 * Returns the ListOfDomainTypes from this Geometry.
 */
ListOfDomainTypes*
Geometry::getListOfDomainTypes()
{
  return &mDomainTypes;
}


/*
 * Get a DomainType from the Geometry.
 */
DomainType*
Geometry::getDomainType(unsigned int n)
{
  return mDomainTypes.get(n);
}


/*
 * Get a DomainType from the Geometry.
 */
const DomainType*
Geometry::getDomainType(unsigned int n) const
{
  return mDomainTypes.get(n);
}


/*
 * Get a DomainType from the Geometry based on its identifier.
 */
DomainType*
Geometry::getDomainType(const std::string& sid)
{
  return mDomainTypes.get(sid);
}


/*
 * Get a DomainType from the Geometry based on its identifier.
 */
const DomainType*
Geometry::getDomainType(const std::string& sid) const
{
  return mDomainTypes.get(sid);
}


/*
 * Adds a copy of the given DomainType to this Geometry.
 */
int
Geometry::addDomainType(const DomainType* dt)
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
  else if (dt->isSetId() && (mDomainTypes.get(dt->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mDomainTypes.append(dt);
  }
}


/*
 * Get the number of DomainType objects in this Geometry.
 */
unsigned int
Geometry::getNumDomainTypes() const
{
  return mDomainTypes.size();
}


/*
 * Creates a new DomainType object, adds it to this Geometry object and returns
 * the DomainType object created.
 */
DomainType*
Geometry::createDomainType()
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
    mDomainTypes.appendAndOwn(dt);
  }

  return dt;
}


/*
 * Removes the nth DomainType from this Geometry and returns a pointer to it.
 */
DomainType*
Geometry::removeDomainType(unsigned int n)
{
  return mDomainTypes.remove(n);
}


/*
 * Removes the DomainType from this Geometry based on its identifier and
 * returns a pointer to it.
 */
DomainType*
Geometry::removeDomainType(const std::string& sid)
{
  return mDomainTypes.remove(sid);
}


/*
 * Returns the ListOfDomains from this Geometry.
 */
const ListOfDomains*
Geometry::getListOfDomains() const
{
  return &mDomains;
}


/*
 * Returns the ListOfDomains from this Geometry.
 */
ListOfDomains*
Geometry::getListOfDomains()
{
  return &mDomains;
}


/*
 * Get a Domain from the Geometry.
 */
Domain*
Geometry::getDomain(unsigned int n)
{
  return mDomains.get(n);
}


/*
 * Get a Domain from the Geometry.
 */
const Domain*
Geometry::getDomain(unsigned int n) const
{
  return mDomains.get(n);
}


/*
 * Get a Domain from the Geometry based on its identifier.
 */
Domain*
Geometry::getDomain(const std::string& sid)
{
  return mDomains.get(sid);
}


/*
 * Get a Domain from the Geometry based on its identifier.
 */
const Domain*
Geometry::getDomain(const std::string& sid) const
{
  return mDomains.get(sid);
}


/*
 * Get a Domain from the Geometry based on the DomainType to which it refers.
 */
const Domain*
Geometry::getDomainByDomainType(const std::string& sid) const
{
  return mDomains.getByDomainType(sid);
}


/*
 * Get a Domain from the Geometry based on the DomainType to which it refers.
 */
Domain*
Geometry::getDomainByDomainType(const std::string& sid)
{
  return mDomains.getByDomainType(sid);
}


/*
 * Adds a copy of the given Domain to this Geometry.
 */
int
Geometry::addDomain(const Domain* d)
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
  else if (d->isSetId() && (mDomains.get(d->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mDomains.append(d);
  }
}


/*
 * Get the number of Domain objects in this Geometry.
 */
unsigned int
Geometry::getNumDomains() const
{
  return mDomains.size();
}


/*
 * Creates a new Domain object, adds it to this Geometry object and returns the
 * Domain object created.
 */
Domain*
Geometry::createDomain()
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
    mDomains.appendAndOwn(d);
  }

  return d;
}


/*
 * Removes the nth Domain from this Geometry and returns a pointer to it.
 */
Domain*
Geometry::removeDomain(unsigned int n)
{
  return mDomains.remove(n);
}


/*
 * Removes the Domain from this Geometry based on its identifier and returns a
 * pointer to it.
 */
Domain*
Geometry::removeDomain(const std::string& sid)
{
  return mDomains.remove(sid);
}


/*
 * Returns the ListOfAdjacentDomains from this Geometry.
 */
const ListOfAdjacentDomains*
Geometry::getListOfAdjacentDomains() const
{
  return &mAdjacentDomains;
}


/*
 * Returns the ListOfAdjacentDomains from this Geometry.
 */
ListOfAdjacentDomains*
Geometry::getListOfAdjacentDomains()
{
  return &mAdjacentDomains;
}


/*
 * Get an AdjacentDomains from the Geometry.
 */
AdjacentDomains*
Geometry::getAdjacentDomains(unsigned int n)
{
  return mAdjacentDomains.get(n);
}


/*
 * Get an AdjacentDomains from the Geometry.
 */
const AdjacentDomains*
Geometry::getAdjacentDomains(unsigned int n) const
{
  return mAdjacentDomains.get(n);
}


/*
 * Get an AdjacentDomains from the Geometry based on its identifier.
 */
AdjacentDomains*
Geometry::getAdjacentDomains(const std::string& sid)
{
  return mAdjacentDomains.get(sid);
}


/*
 * Get an AdjacentDomains from the Geometry based on its identifier.
 */
const AdjacentDomains*
Geometry::getAdjacentDomains(const std::string& sid) const
{
  return mAdjacentDomains.get(sid);
}


/*
 * Get an AdjacentDomains from the Geometry based on the Domain1 to which it
 * refers.
 */
const AdjacentDomains*
Geometry::getAdjacentDomainsByDomain1(const std::string& sid) const
{
  return mAdjacentDomains.getByDomain1(sid);
}


/*
 * Get an AdjacentDomains from the Geometry based on the Domain1 to which it
 * refers.
 */
AdjacentDomains*
Geometry::getAdjacentDomainsByDomain1(const std::string& sid)
{
  return mAdjacentDomains.getByDomain1(sid);
}


/*
 * Get an AdjacentDomains from the Geometry based on the Domain2 to which it
 * refers.
 */
const AdjacentDomains*
Geometry::getAdjacentDomainsByDomain2(const std::string& sid) const
{
  return mAdjacentDomains.getByDomain2(sid);
}


/*
 * Get an AdjacentDomains from the Geometry based on the Domain2 to which it
 * refers.
 */
AdjacentDomains*
Geometry::getAdjacentDomainsByDomain2(const std::string& sid)
{
  return mAdjacentDomains.getByDomain2(sid);
}


/*
 * Adds a copy of the given AdjacentDomains to this Geometry.
 */
int
Geometry::addAdjacentDomains(const AdjacentDomains* ad)
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
  else if (ad->isSetId() && (mAdjacentDomains.get(ad->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mAdjacentDomains.append(ad);
  }
}


/*
 * Get the number of AdjacentDomains objects in this Geometry.
 */
unsigned int
Geometry::getNumAdjacentDomains() const
{
  return mAdjacentDomains.size();
}


/*
 * Creates a new AdjacentDomains object, adds it to this Geometry object and
 * returns the AdjacentDomains object created.
 */
AdjacentDomains*
Geometry::createAdjacentDomains()
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
    //ad->setElementName("adjacentDomains");
    mAdjacentDomains.appendAndOwn(ad);
  }

  return ad;
}


/*
 * Removes the nth AdjacentDomains from this Geometry and returns a pointer to
 * it.
 */
AdjacentDomains*
Geometry::removeAdjacentDomains(unsigned int n)
{
  return mAdjacentDomains.remove(n);
}


/*
 * Removes the AdjacentDomains from this Geometry based on its identifier and
 * returns a pointer to it.
 */
AdjacentDomains*
Geometry::removeAdjacentDomains(const std::string& sid)
{
  return mAdjacentDomains.remove(sid);
}


/*
 * Returns the ListOfGeometryDefinitions from this Geometry.
 */
const ListOfGeometryDefinitions*
Geometry::getListOfGeometryDefinitions() const
{
  return &mGeometryDefinitions;
}


/*
 * Returns the ListOfGeometryDefinitions from this Geometry.
 */
ListOfGeometryDefinitions*
Geometry::getListOfGeometryDefinitions()
{
  return &mGeometryDefinitions;
}


/*
 * Get a GeometryDefinition from the Geometry.
 */
GeometryDefinition*
Geometry::getGeometryDefinition(unsigned int n)
{
  return mGeometryDefinitions.get(n);
}


/*
 * Get a GeometryDefinition from the Geometry.
 */
const GeometryDefinition*
Geometry::getGeometryDefinition(unsigned int n) const
{
  return mGeometryDefinitions.get(n);
}


/*
 * Get a GeometryDefinition from the Geometry based on its identifier.
 */
GeometryDefinition*
Geometry::getGeometryDefinition(const std::string& sid)
{
  return mGeometryDefinitions.get(sid);
}


/*
 * Get a GeometryDefinition from the Geometry based on its identifier.
 */
const GeometryDefinition*
Geometry::getGeometryDefinition(const std::string& sid) const
{
  return mGeometryDefinitions.get(sid);
}


/*
 * Adds a copy of the given GeometryDefinition to this Geometry.
 */
int
Geometry::addGeometryDefinition(const GeometryDefinition* gd)
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
  else if (gd->isSetId() && (mGeometryDefinitions.get(gd->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mGeometryDefinitions.append(gd);
  }
}


/*
 * Get the number of GeometryDefinition objects in this Geometry.
 */
unsigned int
Geometry::getNumGeometryDefinitions() const
{
  return mGeometryDefinitions.size();
}


/*
 * Creates a new AnalyticGeometry object, adds it to this Geometry object and
 * returns the AnalyticGeometry object created.
 */
AnalyticGeometry*
Geometry::createAnalyticGeometry()
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
    mGeometryDefinitions.appendAndOwn(ag);
  }

  return ag;
}


/*
 * Creates a new SampledFieldGeometry object, adds it to this Geometry object
 * and returns the SampledFieldGeometry object created.
 */
SampledFieldGeometry*
Geometry::createSampledFieldGeometry()
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
    mGeometryDefinitions.appendAndOwn(sfg);
  }

  return sfg;
}


/*
 * Creates a new CSGeometry object, adds it to this Geometry object and returns
 * the CSGeometry object created.
 */
CSGeometry*
Geometry::createCSGeometry()
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
    mGeometryDefinitions.appendAndOwn(csg);
  }

  return csg;
}


/*
 * Creates a new ParametricGeometry object, adds it to this Geometry object and
 * returns the ParametricGeometry object created.
 */
ParametricGeometry*
Geometry::createParametricGeometry()
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
    mGeometryDefinitions.appendAndOwn(pg);
  }

  return pg;
}


/*
 * Creates a new MixedGeometry object, adds it to this Geometry object and
 * returns the MixedGeometry object created.
 */
MixedGeometry*
Geometry::createMixedGeometry()
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
    mGeometryDefinitions.appendAndOwn(mg);
  }

  return mg;
}


/*
 * Removes the nth GeometryDefinition from this Geometry and returns a pointer
 * to it.
 */
GeometryDefinition*
Geometry::removeGeometryDefinition(unsigned int n)
{
  return mGeometryDefinitions.remove(n);
}


/*
 * Removes the GeometryDefinition from this Geometry based on its identifier
 * and returns a pointer to it.
 */
GeometryDefinition*
Geometry::removeGeometryDefinition(const std::string& sid)
{
  return mGeometryDefinitions.remove(sid);
}


/*
 * Returns the ListOfSampledFields from this Geometry.
 */
const ListOfSampledFields*
Geometry::getListOfSampledFields() const
{
  return &mSampledFields;
}


/*
 * Returns the ListOfSampledFields from this Geometry.
 */
ListOfSampledFields*
Geometry::getListOfSampledFields()
{
  return &mSampledFields;
}


/*
 * Get a SampledField from the Geometry.
 */
SampledField*
Geometry::getSampledField(unsigned int n)
{
  return mSampledFields.get(n);
}


/*
 * Get a SampledField from the Geometry.
 */
const SampledField*
Geometry::getSampledField(unsigned int n) const
{
  return mSampledFields.get(n);
}


/*
 * Get a SampledField from the Geometry based on its identifier.
 */
SampledField*
Geometry::getSampledField(const std::string& sid)
{
  return mSampledFields.get(sid);
}


/*
 * Get a SampledField from the Geometry based on its identifier.
 */
const SampledField*
Geometry::getSampledField(const std::string& sid) const
{
  return mSampledFields.get(sid);
}


/*
 * Adds a copy of the given SampledField to this Geometry.
 */
int
Geometry::addSampledField(const SampledField* sf)
{
  if (sf == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sf->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sf->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sf->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(sf)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (sf->isSetId() && (mSampledFields.get(sf->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mSampledFields.append(sf);
  }
}


/*
 * Get the number of SampledField objects in this Geometry.
 */
unsigned int
Geometry::getNumSampledFields() const
{
  return mSampledFields.size();
}


/*
 * Creates a new SampledField object, adds it to this Geometry object and
 * returns the SampledField object created.
 */
SampledField*
Geometry::createSampledField()
{
  SampledField* sf = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sf = new SampledField(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (sf != NULL)
  {
    mSampledFields.appendAndOwn(sf);
  }

  return sf;
}


/*
 * Removes the nth SampledField from this Geometry and returns a pointer to it.
 */
SampledField*
Geometry::removeSampledField(unsigned int n)
{
  return mSampledFields.remove(n);
}


/*
 * Removes the SampledField from this Geometry based on its identifier and
 * returns a pointer to it.
 */
SampledField*
Geometry::removeSampledField(const std::string& sid)
{
  return mSampledFields.remove(sid);
}

const CoordinateComponent*
Geometry::getCoordinateComponentByKind(CoordinateKind_t kind) const
{
    return const_cast <Geometry*> (this)->getCoordinateComponentByKind(kind);
}

CoordinateComponent*
Geometry::getCoordinateComponentByKind(CoordinateKind_t kind)
{
  for (size_t i = 0; i < mCoordinateComponents.size(); ++i)
  {
    CoordinateComponent* current = mCoordinateComponents.get(i);
    if (current->getType() == kind)
      return current;
  }
  return NULL;
}


/*
 * Returns the XML element name of this Geometry object.
 */
const std::string&
Geometry::getElementName() const
{
  static const string name = "geometry";
  return name;
}


/*
 * Returns the libSBML type code for this Geometry object.
 */
int
Geometry::getTypeCode() const
{
  return SBML_SPATIAL_GEOMETRY;
}


/*
 * Predicate returning @c true if all the required attributes for this Geometry
 * object have been set.
 */
bool
Geometry::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetCoordinateSystem() == false)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
Geometry::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (getNumCoordinateComponents() > 0)
  {
    mCoordinateComponents.write(stream);
  }

  if (getNumDomainTypes() > 0)
  {
    mDomainTypes.write(stream);
  }

  if (getNumDomains() > 0)
  {
    mDomains.write(stream);
  }

  if (getNumAdjacentDomains() > 0)
  {
    mAdjacentDomains.write(stream);
  }

  if (getNumGeometryDefinitions() > 0)
  {
    mGeometryDefinitions.write(stream);
  }

  if (getNumSampledFields() > 0)
  {
    mSampledFields.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Geometry::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mCoordinateComponents.accept(v);

  mDomainTypes.accept(v);

  mDomains.accept(v);

  mAdjacentDomains.accept(v);

  mGeometryDefinitions.accept(v);

  mSampledFields.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Geometry::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mCoordinateComponents.setSBMLDocument(d);

  mDomainTypes.setSBMLDocument(d);

  mDomains.setSBMLDocument(d);

  mAdjacentDomains.setSBMLDocument(d);

  mGeometryDefinitions.setSBMLDocument(d);

  mSampledFields.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
Geometry::connectToChild()
{
  SBase::connectToChild();

  mCoordinateComponents.connectToParent(this);

  mDomainTypes.connectToParent(this);

  mDomains.connectToParent(this);

  mAdjacentDomains.connectToParent(this);

  mGeometryDefinitions.connectToParent(this);

  mSampledFields.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Geometry::enablePackageInternal(const std::string& pkgURI,
                                const std::string& pkgPrefix,
                                bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mCoordinateComponents.enablePackageInternal(pkgURI, pkgPrefix, flag);

  mDomainTypes.enablePackageInternal(pkgURI, pkgPrefix, flag);

  mDomains.enablePackageInternal(pkgURI, pkgPrefix, flag);

  mAdjacentDomains.enablePackageInternal(pkgURI, pkgPrefix, flag);

  mGeometryDefinitions.enablePackageInternal(pkgURI, pkgPrefix, flag);

  mSampledFields.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
Geometry::updateSBMLNamespace(const std::string& package,
                              unsigned int level,
                              unsigned int version)
{
  SBase::updateSBMLNamespace(package, level, version);

  mCoordinateComponents.updateSBMLNamespace(package, level, version);

  mDomainTypes.updateSBMLNamespace(package, level, version);

  mDomains.updateSBMLNamespace(package, level, version);

  mAdjacentDomains.updateSBMLNamespace(package, level, version);

  mGeometryDefinitions.updateSBMLNamespace(package, level, version);

  mSampledFields.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::getAttribute(const std::string& attributeName,
                       unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::getAttribute(const std::string& attributeName,
                       std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "coordinateSystem")
  {
    value = getCoordinateSystemAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Geometry's attribute "attributeName" is
 * set.
 */
bool
Geometry::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "coordinateSystem")
  {
    value = isSetCoordinateSystem();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::setAttribute(const std::string& attributeName,
                       const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "coordinateSystem")
  {
    return_value = setCoordinateSystem(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Geometry.
 */
int
Geometry::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "coordinateSystem")
  {
    value = unsetCoordinateSystem();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this Geometry.
 */
SBase*
Geometry::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "coordinateComponent")
  {
    return createCoordinateComponent();
  }
  else if (elementName == "domainType")
  {
    return createDomainType();
  }
  else if (elementName == "domain")
  {
    return createDomain();
  }
  else if (elementName == "adjacentDomains")
  {
    return createAdjacentDomains();
  }
  else if (elementName == "analyticGeometry")
  {
    return createAnalyticGeometry();
  }
  else if (elementName == "sampledFieldGeometry")
  {
    return createSampledFieldGeometry();
  }
  else if (elementName == "csGeometry")
  {
    return createCSGeometry();
  }
  else if (elementName == "parametricGeometry")
  {
    return createParametricGeometry();
  }
  else if (elementName == "mixedGeometry")
  {
    return createMixedGeometry();
  }
  else if (elementName == "sampledField")
  {
    return createSampledField();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this Geometry.
 */
int
Geometry::addChildObject(const std::string& elementName, const SBase* element)
{
  if (elementName == "coordinateComponent" && element->getTypeCode() ==
    SBML_SPATIAL_COORDINATECOMPONENT)
  {
    return addCoordinateComponent((const CoordinateComponent*)(element));
  }
  else if (elementName == "domainType" && element->getTypeCode() ==
    SBML_SPATIAL_DOMAINTYPE)
  {
    return addDomainType((const DomainType*)(element));
  }
  else if (elementName == "domain" && element->getTypeCode() ==
    SBML_SPATIAL_DOMAIN)
  {
    return addDomain((const Domain*)(element));
  }
  else if (elementName == "adjacentDomains" && element->getTypeCode() ==
    SBML_SPATIAL_ADJACENTDOMAINS)
  {
    return addAdjacentDomains((const AdjacentDomains*)(element));
  }
  else if (elementName == "analyticGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_ANALYTICGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "sampledFieldGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_SAMPLEDFIELDGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "csGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_CSGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "parametricGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_PARAMETRICGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "mixedGeometry" && element->getTypeCode() ==
    SBML_SPATIAL_MIXEDGEOMETRY)
  {
    return addGeometryDefinition((const GeometryDefinition*)(element));
  }
  else if (elementName == "sampledField" && element->getTypeCode() ==
    SBML_SPATIAL_SAMPLEDFIELD)
  {
    return addSampledField((const SampledField*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * Geometry.
 */
SBase*
Geometry::removeChildObject(const std::string& elementName,
                            const std::string& id)
{
  if (elementName == "coordinateComponent")
  {
    return removeCoordinateComponent(id);
  }
  else if (elementName == "domainType")
  {
    return removeDomainType(id);
  }
  else if (elementName == "domain")
  {
    return removeDomain(id);
  }
  else if (elementName == "adjacentDomains")
  {
    return removeAdjacentDomains(id);
  }
  else if (elementName == "analyticGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "sampledFieldGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "csGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "parametricGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "mixedGeometry")
  {
    return removeGeometryDefinition(id);
  }
  else if (elementName == "sampledField")
  {
    return removeSampledField(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this Geometry.
 */
unsigned int
Geometry::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "coordinateComponent")
  {
    return getNumCoordinateComponents();
  }
  else if (elementName == "domainType")
  {
    return getNumDomainTypes();
  }
  else if (elementName == "domain")
  {
    return getNumDomains();
  }
  else if (elementName == "adjacentDomains")
  {
    return getNumAdjacentDomains();
  }
  else if (elementName == "geometryDefinition")
  {
    return getNumGeometryDefinitions();
  }
  else if (elementName == "sampledField")
  {
    return getNumSampledFields();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this Geometry.
 */
SBase*
Geometry::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "coordinateComponent")
  {
    return getCoordinateComponent(index);
  }
  else if (elementName == "domainType")
  {
    return getDomainType(index);
  }
  else if (elementName == "domain")
  {
    return getDomain(index);
  }
  else if (elementName == "adjacentDomains")
  {
    return getAdjacentDomains(index);
  }
  else if (elementName == "geometryDefinition")
  {
    return getGeometryDefinition(index);
  }
  else if (elementName == "sampledField")
  {
    return getSampledField(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
Geometry::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mCoordinateComponents.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mDomainTypes.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mDomains.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mAdjacentDomains.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mGeometryDefinitions.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mSampledFields.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
Geometry::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mCoordinateComponents.getMetaId() == metaid)
  {
    return &mCoordinateComponents;
  }

  if (mDomainTypes.getMetaId() == metaid)
  {
    return &mDomainTypes;
  }

  if (mDomains.getMetaId() == metaid)
  {
    return &mDomains;
  }

  if (mAdjacentDomains.getMetaId() == metaid)
  {
    return &mAdjacentDomains;
  }

  if (mGeometryDefinitions.getMetaId() == metaid)
  {
    return &mGeometryDefinitions;
  }

  if (mSampledFields.getMetaId() == metaid)
  {
    return &mSampledFields;
  }

  obj = mCoordinateComponents.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mDomainTypes.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mDomains.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mAdjacentDomains.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mGeometryDefinitions.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mSampledFields.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
Geometry::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mCoordinateComponents, filter);
  ADD_FILTERED_LIST(ret, sublist, mDomainTypes, filter);
  ADD_FILTERED_LIST(ret, sublist, mDomains, filter);
  ADD_FILTERED_LIST(ret, sublist, mAdjacentDomains, filter);
  ADD_FILTERED_LIST(ret, sublist, mGeometryDefinitions, filter);
  ADD_FILTERED_LIST(ret, sublist, mSampledFields, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
Geometry::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();

  if (name == "listOfCoordinateComponents")
  {
    if (mCoordinateComponents.size() != 0)
    {
      getErrorLog()->logPackageError("spatial", SpatialGeometryAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(),
          getColumn());
    }

    obj = &mCoordinateComponents;
  }
  else if (name == "listOfDomainTypes")
  {
    if (mDomainTypes.size() != 0)
    {
      getErrorLog()->logPackageError("spatial", SpatialGeometryAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(),
          getColumn());
    }

    obj = &mDomainTypes;
  }
  else if (name == "listOfDomains")
  {
    if (mDomains.size() != 0)
    {
      getErrorLog()->logPackageError("spatial", SpatialGeometryAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(),
          getColumn());
    }

    obj = &mDomains;
  }
  else if (name == "listOfAdjacentDomains")
  {
    if (mAdjacentDomains.size() != 0)
    {
      getErrorLog()->logPackageError("spatial", SpatialGeometryAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(),
          getColumn());
    }

    obj = &mAdjacentDomains;
  }
  else if (name == "listOfGeometryDefinitions")
  {
    if (mGeometryDefinitions.size() != 0)
    {
      getErrorLog()->logPackageError("spatial", SpatialGeometryAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(),
          getColumn());
    }

    obj = &mGeometryDefinitions;
  }
  else if (name == "listOfSampledFields")
  {
    if (mSampledFields.size() != 0)
    {
      getErrorLog()->logPackageError("spatial", SpatialGeometryAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(),
          getColumn());
    }

    obj = &mSampledFields;
  }

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
Geometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("coordinateSystem");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Geometry::readAttributes(const XMLAttributes& attributes,
                         const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialGeometryAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial", SpatialGeometryAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  //
  // id SId (use = "optional" )
  //

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<Geometry>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("spatial", SpatialIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  //
  // coordinateSystem enum (use = "required" )
  //

  std::string coordinateSystem;
  assigned = attributes.readInto("coordinateSystem", coordinateSystem);

  if (assigned == true)
  {
    if (coordinateSystem.empty() == true)
    {
      logEmptyString(coordinateSystem, level, version, "<Geometry>");
    }
    else
    {
      mCoordinateSystem = GeometryKind_fromString(coordinateSystem.c_str());

      if (GeometryKind_isValid(mCoordinateSystem) == 0)
      {
        std::string msg = "The coordinateSystem on the <Geometry> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + coordinateSystem + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialGeometryCoordinateSystemMustBeGeometryKindEnum, pkgVersion,
            level, version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'coordinateSystem' is missing.";
    log->logPackageError("spatial", SpatialGeometryAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Geometry::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetCoordinateSystem() == true)
  {
    stream.writeAttribute("coordinateSystem", getPrefix(),
      GeometryKind_toString(mCoordinateSystem));
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */


#endif /* __cplusplus */


/*
 * Creates a new Geometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
Geometry_t *
Geometry_create(unsigned int level,
                unsigned int version,
                unsigned int pkgVersion)
{
  return new Geometry(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Geometry_t object.
 */
LIBSBML_EXTERN
Geometry_t*
Geometry_clone(const Geometry_t* g)
{
  if (g != NULL)
  {
    return static_cast<Geometry_t*>(g->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Geometry_t object.
 */
LIBSBML_EXTERN
void
Geometry_free(Geometry_t* g)
{
  if (g != NULL)
  {
    delete g;
  }
}


/*
 * Returns the value of the "id" attribute of this Geometry_t.
 */
LIBSBML_EXTERN
char *
Geometry_getId(const Geometry_t * g)
{
  if (g == NULL)
  {
    return NULL;
  }

  return g->getId().empty() ? NULL : safe_strdup(g->getId().c_str());
}


/*
 * Returns the value of the "coordinateSystem" attribute of this Geometry_t.
 */
LIBSBML_EXTERN
GeometryKind_t
Geometry_getCoordinateSystem(const Geometry_t * g)
{
  if (g == NULL)
  {
    return SPATIAL_GEOMETRYKIND_INVALID;
  }

  return g->getCoordinateSystem();
}


/*
 * Returns the value of the "coordinateSystem" attribute of this Geometry_t.
 */
LIBSBML_EXTERN
char *
Geometry_getCoordinateSystemAsString(const Geometry_t * g)
{
  return (char*)(GeometryKind_toString(g->getCoordinateSystem()));
}


/*
 * Predicate returning @c 1 (true) if this Geometry_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
Geometry_isSetId(const Geometry_t * g)
{
  return (g != NULL) ? static_cast<int>(g->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Geometry_t's "coordinateSystem"
 * attribute is set.
 */
LIBSBML_EXTERN
int
Geometry_isSetCoordinateSystem(const Geometry_t * g)
{
  return (g != NULL) ? static_cast<int>(g->isSetCoordinateSystem()) : 0;
}


/*
 * Sets the value of the "id" attribute of this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_setId(Geometry_t * g, const char * id)
{
  return (g != NULL) ? g->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinateSystem" attribute of this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_setCoordinateSystem(Geometry_t * g, GeometryKind_t coordinateSystem)
{
  return (g != NULL) ? g->setCoordinateSystem(coordinateSystem) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coordinateSystem" attribute of this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_setCoordinateSystemAsString(Geometry_t * g,
                                     const char * coordinateSystem)
{
  return (g != NULL) ? g->setCoordinateSystem(coordinateSystem):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_unsetId(Geometry_t * g)
{
  return (g != NULL) ? g->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coordinateSystem" attribute of this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_unsetCoordinateSystem(Geometry_t * g)
{
  return (g != NULL) ? g->unsetCoordinateSystem() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing CoordinateComponent_t objects from this
 * Geometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfCoordinateComponents(Geometry_t* g)
{
  return (g != NULL) ? g->getListOfCoordinateComponents() : NULL;
}


/*
 * Get a CoordinateComponent_t from the Geometry_t.
 */
LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_getCoordinateComponent(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->getCoordinateComponent(n) : NULL;
}


/*
 * Get a CoordinateComponent_t from the Geometry_t based on its identifier.
 */
LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_getCoordinateComponentById(Geometry_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getCoordinateComponent(sid) : NULL;
}


/*
 * Adds a copy of the given CoordinateComponent_t to this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_addCoordinateComponent(Geometry_t* g,
                                const CoordinateComponent_t* cc)
{
  return (g != NULL) ? g->addCoordinateComponent(cc) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of CoordinateComponent_t objects in this Geometry_t.
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumCoordinateComponents(Geometry_t* g)
{
  return (g != NULL) ? g->getNumCoordinateComponents() : SBML_INT_MAX;
}


/*
 * Creates a new CoordinateComponent_t object, adds it to this Geometry_t
 * object and returns the CoordinateComponent_t object created.
 */
LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_createCoordinateComponent(Geometry_t* g)
{
  return (g != NULL) ? g->createCoordinateComponent() : NULL;
}


/*
 * Removes the nth CoordinateComponent_t from this Geometry_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_removeCoordinateComponent(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->removeCoordinateComponent(n) : NULL;
}


/*
 * Removes the CoordinateComponent_t from this Geometry_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_removeCoordinateComponentById(Geometry_t* g, const char* sid)
{
  return (g != NULL && sid != NULL) ? g->removeCoordinateComponent(sid) : NULL;
}


/*
 * Returns a ListOf_t * containing DomainType_t objects from this Geometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfDomainTypes(Geometry_t* g)
{
  return (g != NULL) ? g->getListOfDomainTypes() : NULL;
}


/*
 * Get a DomainType_t from the Geometry_t.
 */
LIBSBML_EXTERN
DomainType_t*
Geometry_getDomainType(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->getDomainType(n) : NULL;
}


/*
 * Get a DomainType_t from the Geometry_t based on its identifier.
 */
LIBSBML_EXTERN
DomainType_t*
Geometry_getDomainTypeById(Geometry_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getDomainType(sid) : NULL;
}


/*
 * Adds a copy of the given DomainType_t to this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_addDomainType(Geometry_t* g, const DomainType_t* dt)
{
  return (g != NULL) ? g->addDomainType(dt) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of DomainType_t objects in this Geometry_t.
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumDomainTypes(Geometry_t* g)
{
  return (g != NULL) ? g->getNumDomainTypes() : SBML_INT_MAX;
}


/*
 * Creates a new DomainType_t object, adds it to this Geometry_t object and
 * returns the DomainType_t object created.
 */
LIBSBML_EXTERN
DomainType_t*
Geometry_createDomainType(Geometry_t* g)
{
  return (g != NULL) ? g->createDomainType() : NULL;
}


/*
 * Removes the nth DomainType_t from this Geometry_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
DomainType_t*
Geometry_removeDomainType(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->removeDomainType(n) : NULL;
}


/*
 * Removes the DomainType_t from this Geometry_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
DomainType_t*
Geometry_removeDomainTypeById(Geometry_t* g, const char* sid)
{
  return (g != NULL && sid != NULL) ? g->removeDomainType(sid) : NULL;
}


/*
 * Returns a ListOf_t * containing Domain_t objects from this Geometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfDomains(Geometry_t* g)
{
  return (g != NULL) ? g->getListOfDomains() : NULL;
}


/*
 * Get a Domain_t from the Geometry_t.
 */
LIBSBML_EXTERN
Domain_t*
Geometry_getDomain(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->getDomain(n) : NULL;
}


/*
 * Get a Domain_t from the Geometry_t based on its identifier.
 */
LIBSBML_EXTERN
Domain_t*
Geometry_getDomainById(Geometry_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getDomain(sid) : NULL;
}


/*
 * Get a Domain_t from the Geometry_t based on the DomainType to which it
 * refers.
 */
LIBSBML_EXTERN
Domain_t*
Geometry_getDomainByDomainType(Geometry_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getDomainByDomainType(sid) : NULL;
}


/*
 * Adds a copy of the given Domain_t to this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_addDomain(Geometry_t* g, const Domain_t* d)
{
  return (g != NULL) ? g->addDomain(d) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of Domain_t objects in this Geometry_t.
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumDomains(Geometry_t* g)
{
  return (g != NULL) ? g->getNumDomains() : SBML_INT_MAX;
}


/*
 * Creates a new Domain_t object, adds it to this Geometry_t object and returns
 * the Domain_t object created.
 */
LIBSBML_EXTERN
Domain_t*
Geometry_createDomain(Geometry_t* g)
{
  return (g != NULL) ? g->createDomain() : NULL;
}


/*
 * Removes the nth Domain_t from this Geometry_t and returns a pointer to it.
 */
LIBSBML_EXTERN
Domain_t*
Geometry_removeDomain(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->removeDomain(n) : NULL;
}


/*
 * Removes the Domain_t from this Geometry_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
Domain_t*
Geometry_removeDomainById(Geometry_t* g, const char* sid)
{
  return (g != NULL && sid != NULL) ? g->removeDomain(sid) : NULL;
}


/*
 * Returns a ListOf_t * containing AdjacentDomains_t objects from this
 * Geometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfAdjacentDomains(Geometry_t* g)
{
  return (g != NULL) ? g->getListOfAdjacentDomains() : NULL;
}


/*
 * Get an AdjacentDomains_t from the Geometry_t.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_getAdjacentDomains(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->getAdjacentDomains(n) : NULL;
}


/*
 * Get an AdjacentDomains_t from the Geometry_t based on its identifier.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_getAdjacentDomainsById(Geometry_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getAdjacentDomains(sid) : NULL;
}


/*
 * Get an AdjacentDomains_t from the Geometry_t based on the Domain1 to which
 * it refers.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_getAdjacentDomainsByDomain1(Geometry_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getAdjacentDomainsByDomain1(sid) :
    NULL;
}


/*
 * Get an AdjacentDomains_t from the Geometry_t based on the Domain2 to which
 * it refers.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_getAdjacentDomainsByDomain2(Geometry_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getAdjacentDomainsByDomain2(sid) :
    NULL;
}


/*
 * Adds a copy of the given AdjacentDomains_t to this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_addAdjacentDomains(Geometry_t* g, const AdjacentDomains_t* ad)
{
  return (g != NULL) ? g->addAdjacentDomains(ad) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of AdjacentDomains_t objects in this Geometry_t.
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumAdjacentDomains(Geometry_t* g)
{
  return (g != NULL) ? g->getNumAdjacentDomains() : SBML_INT_MAX;
}


/*
 * Creates a new AdjacentDomains_t object, adds it to this Geometry_t object
 * and returns the AdjacentDomains_t object created.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_createAdjacentDomains(Geometry_t* g)
{
  return (g != NULL) ? g->createAdjacentDomains() : NULL;
}


/*
 * Removes the nth AdjacentDomains_t from this Geometry_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_removeAdjacentDomains(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->removeAdjacentDomains(n) : NULL;
}


/*
 * Removes the AdjacentDomains_t from this Geometry_t based on its identifier
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_removeAdjacentDomainsById(Geometry_t* g, const char* sid)
{
  return (g != NULL && sid != NULL) ? g->removeAdjacentDomains(sid) : NULL;
}


/*
 * Returns a ListOf_t * containing GeometryDefinition_t objects from this
 * Geometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfGeometryDefinitions(Geometry_t* g)
{
  return (g != NULL) ? g->getListOfGeometryDefinitions() : NULL;
}


/*
 * Get a GeometryDefinition_t from the Geometry_t.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
Geometry_getGeometryDefinition(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->getGeometryDefinition(n) : NULL;
}


/*
 * Get a GeometryDefinition_t from the Geometry_t based on its identifier.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
Geometry_getGeometryDefinitionById(Geometry_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getGeometryDefinition(sid) : NULL;
}


/*
 * Adds a copy of the given GeometryDefinition_t to this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_addGeometryDefinition(Geometry_t* g, const GeometryDefinition_t* gd)
{
  return (g != NULL) ? g->addGeometryDefinition(gd) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of GeometryDefinition_t objects in this Geometry_t.
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumGeometryDefinitions(Geometry_t* g)
{
  return (g != NULL) ? g->getNumGeometryDefinitions() : SBML_INT_MAX;
}


/*
 * Creates a new AnalyticGeometry_t object, adds it to this Geometry_t object
 * and returns the AnalyticGeometry_t object created.
 */
LIBSBML_EXTERN
AnalyticGeometry_t*
Geometry_createAnalyticGeometry(Geometry_t* g)
{
  return (g != NULL) ? g->createAnalyticGeometry() : NULL;
}


/*
 * Creates a new SampledFieldGeometry_t object, adds it to this Geometry_t
 * object and returns the SampledFieldGeometry_t object created.
 */
LIBSBML_EXTERN
SampledFieldGeometry_t*
Geometry_createSampledFieldGeometry(Geometry_t* g)
{
  return (g != NULL) ? g->createSampledFieldGeometry() : NULL;
}


/*
 * Creates a new CSGeometry_t object, adds it to this Geometry_t object and
 * returns the CSGeometry_t object created.
 */
LIBSBML_EXTERN
CSGeometry_t*
Geometry_createCSGeometry(Geometry_t* g)
{
  return (g != NULL) ? g->createCSGeometry() : NULL;
}


/*
 * Creates a new ParametricGeometry_t object, adds it to this Geometry_t object
 * and returns the ParametricGeometry_t object created.
 */
LIBSBML_EXTERN
ParametricGeometry_t*
Geometry_createParametricGeometry(Geometry_t* g)
{
  return (g != NULL) ? g->createParametricGeometry() : NULL;
}


/*
 * Creates a new MixedGeometry_t object, adds it to this Geometry_t object and
 * returns the MixedGeometry_t object created.
 */
LIBSBML_EXTERN
MixedGeometry_t*
Geometry_createMixedGeometry(Geometry_t* g)
{
  return (g != NULL) ? g->createMixedGeometry() : NULL;
}


/*
 * Removes the nth GeometryDefinition_t from this Geometry_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
Geometry_removeGeometryDefinition(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->removeGeometryDefinition(n) : NULL;
}


/*
 * Removes the GeometryDefinition_t from this Geometry_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
GeometryDefinition_t*
Geometry_removeGeometryDefinitionById(Geometry_t* g, const char* sid)
{
  return (g != NULL && sid != NULL) ? g->removeGeometryDefinition(sid) : NULL;
}


/*
 * Returns a ListOf_t * containing SampledField_t objects from this Geometry_t.
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfSampledFields(Geometry_t* g)
{
  return (g != NULL) ? g->getListOfSampledFields() : NULL;
}


/*
 * Get a SampledField_t from the Geometry_t.
 */
LIBSBML_EXTERN
SampledField_t*
Geometry_getSampledField(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->getSampledField(n) : NULL;
}


/*
 * Get a SampledField_t from the Geometry_t based on its identifier.
 */
LIBSBML_EXTERN
SampledField_t*
Geometry_getSampledFieldById(Geometry_t* g, const char *sid)
{
  return (g != NULL && sid != NULL) ? g->getSampledField(sid) : NULL;
}


/*
 * Adds a copy of the given SampledField_t to this Geometry_t.
 */
LIBSBML_EXTERN
int
Geometry_addSampledField(Geometry_t* g, const SampledField_t* sf)
{
  return (g != NULL) ? g->addSampledField(sf) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of SampledField_t objects in this Geometry_t.
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumSampledFields(Geometry_t* g)
{
  return (g != NULL) ? g->getNumSampledFields() : SBML_INT_MAX;
}


/*
 * Creates a new SampledField_t object, adds it to this Geometry_t object and
 * returns the SampledField_t object created.
 */
LIBSBML_EXTERN
SampledField_t*
Geometry_createSampledField(Geometry_t* g)
{
  return (g != NULL) ? g->createSampledField() : NULL;
}


/*
 * Removes the nth SampledField_t from this Geometry_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
SampledField_t*
Geometry_removeSampledField(Geometry_t* g, unsigned int n)
{
  return (g != NULL) ? g->removeSampledField(n) : NULL;
}


/*
 * Removes the SampledField_t from this Geometry_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
SampledField_t*
Geometry_removeSampledFieldById(Geometry_t* g, const char* sid)
{
  return (g != NULL && sid != NULL) ? g->removeSampledField(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Geometry_t object have been set.
 */
LIBSBML_EXTERN
int
Geometry_hasRequiredAttributes(const Geometry_t * g)
{
  return (g != NULL) ? static_cast<int>(g->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


