/**
 * @file:   Geometry.cpp
 * @brief:  Implementation of the Geometry class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
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


/*
 * Creates a new Geometry with the given level, version, and package version.
 */
Geometry::Geometry (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mCoordinateSystem (GEOMETRYKIND_UNKNOWN)
  , mCoordinateComponents (level, version, pkgVersion)
  , mDomainTypes (level, version, pkgVersion)
  , mDomains (level, version, pkgVersion)
  , mAdjacentDomains (level, version, pkgVersion)
  , mGeometryDefinitions (level, version, pkgVersion)
  , mSampledFields (level, version, pkgVersion)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new Geometry with the given SpatialPkgNamespaces object.
 */
Geometry::Geometry (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mCoordinateSystem (GEOMETRYKIND_UNKNOWN)
  , mCoordinateComponents (spatialns)
  , mDomainTypes (spatialns)
  , mDomains (spatialns)
  , mAdjacentDomains (spatialns)
  , mGeometryDefinitions (spatialns)
  , mSampledFields (spatialns)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for Geometry.
 */
Geometry::Geometry (const Geometry& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mCoordinateSystem  = orig.mCoordinateSystem;
    mCoordinateComponents  = orig.mCoordinateComponents;
    mDomainTypes  = orig.mDomainTypes;
    mDomains  = orig.mDomains;
    mAdjacentDomains  = orig.mAdjacentDomains;
    mGeometryDefinitions  = orig.mGeometryDefinitions;
    mSampledFields  = orig.mSampledFields;

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for Geometry.
 */
Geometry&
Geometry::operator=(const Geometry& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mCoordinateSystem  = rhs.mCoordinateSystem;
    mCoordinateComponents  = rhs.mCoordinateComponents;
    mDomainTypes  = rhs.mDomainTypes;
    mDomains  = rhs.mDomains;
    mAdjacentDomains  = rhs.mAdjacentDomains;
    mGeometryDefinitions  = rhs.mGeometryDefinitions;
    mSampledFields  = rhs.mSampledFields;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for Geometry.
 */
Geometry*
Geometry::clone () const
{
  return new Geometry(*this);
}


/*
 * Destructor for Geometry.
 */
Geometry::~Geometry ()
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
 * Returns true/false if id is set.
 */
bool
Geometry::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if coordinateSystem is set.
 */
bool
Geometry::isSetCoordinateSystem() const
{
  return mCoordinateSystem != GEOMETRYKIND_UNKNOWN;
}


/*
 * Sets id and returns value indicating success.
 */
int
Geometry::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets coordinateSystem and returns value indicating success.
 */
int
Geometry::setCoordinateSystem(GeometryKind_t coordinateSystem)
{
  mCoordinateSystem = coordinateSystem;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coordinateSystem and returns value indicating success.
 */
int
Geometry::setCoordinateSystem(const std::string& coordinateSystem)
{
  GeometryKind_t parsed = GeometryKind_parse(coordinateSystem.c_str());
  if (parsed == GEOMETRYKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mCoordinateSystem = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
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
 * Unsets coordinateSystem and returns value indicating success.
 */
int
Geometry::unsetCoordinateSystem()
{
  mCoordinateSystem = GEOMETRYKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the  "ListOfCoordinateComponents" in this Geometry object.
 */
const ListOfCoordinateComponents*
Geometry::getListOfCoordinateComponents() const
{
  return &mCoordinateComponents;
}


/*
 * Returns the  "ListOfCoordinateComponents" in this Geometry object.
 */
ListOfCoordinateComponents*
Geometry::getListOfCoordinateComponents()
{
  return &mCoordinateComponents;
}


/*
 * Removes the nth CoordinateComponent from the ListOfCoordinateComponents.
 */
CoordinateComponent*
Geometry::removeCoordinateComponent(unsigned int n)
{
	return mCoordinateComponents.remove(n);
}


/*
 * Removes the a CoordinateComponent with given id from the ListOfCoordinateComponents.
 */
CoordinateComponent*
Geometry::removeCoordinateComponent(const std::string& sid)
{
	return mCoordinateComponents.remove(sid);
}


/*
 * Return the nth CoordinateComponent in the ListOfCoordinateComponents within this Geometry.
 */
CoordinateComponent*
Geometry::getCoordinateComponent(unsigned int n)
{
	return mCoordinateComponents.get(n);
}


/*
 * Return the nth CoordinateComponent in the ListOfCoordinateComponents within this Geometry.
 */
const CoordinateComponent*
Geometry::getCoordinateComponent(unsigned int n) const
{
	return mCoordinateComponents.get(n);
}


/*
 * Return a CoordinateComponent from the ListOfCoordinateComponents by id.
 */
CoordinateComponent*
Geometry::getCoordinateComponent(const std::string& sid)
{
	return mCoordinateComponents.get(sid);
}


/*
 * Return a CoordinateComponent from the ListOfCoordinateComponents by id.
 */
const CoordinateComponent*
Geometry::getCoordinateComponent(const std::string& sid) const
{
	return mCoordinateComponents.get(sid);
}


/*
 * Adds a copy the given "CoordinateComponent" to this Geometry.
 *
 * @param cc; the CoordinateComponent object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
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
  else if (getLevel() != cc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != cc->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(cc)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mCoordinateComponents.append(cc);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of CoordinateComponent objects in this Geometry.
 *
 * @return the number of CoordinateComponent objects in this Geometry
 */
unsigned int
Geometry::getNumCoordinateComponents() const
{
  return mCoordinateComponents.size();
}


/*
 * Creates a new CoordinateComponent object, adds it to this Geometrys
 * Geometry and returns the CoordinateComponent object created. 
 *
 * @return a new CoordinateComponent object instance
 *
 * @see addCoordinateComponent(const CoordinateComponent* cc)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(cc != NULL)
  {
    mCoordinateComponents.appendAndOwn(cc);
  }

  return cc;
}


/*
 * Returns the  "ListOfDomainTypes" in this Geometry object.
 */
const ListOfDomainTypes*
Geometry::getListOfDomainTypes() const
{
  return &mDomainTypes;
}


/*
 * Returns the  "ListOfDomainTypes" in this Geometry object.
 */
ListOfDomainTypes*
Geometry::getListOfDomainTypes()
{
  return &mDomainTypes;
}


/*
 * Removes the nth DomainType from the ListOfDomainTypes.
 */
DomainType*
Geometry::removeDomainType(unsigned int n)
{
	return mDomainTypes.remove(n);
}


/*
 * Removes the a DomainType with given id from the ListOfDomainTypes.
 */
DomainType*
Geometry::removeDomainType(const std::string& sid)
{
	return mDomainTypes.remove(sid);
}


/*
 * Return the nth DomainType in the ListOfDomainTypes within this Geometry.
 */
DomainType*
Geometry::getDomainType(unsigned int n)
{
	return mDomainTypes.get(n);
}


/*
 * Return the nth DomainType in the ListOfDomainTypes within this Geometry.
 */
const DomainType*
Geometry::getDomainType(unsigned int n) const
{
	return mDomainTypes.get(n);
}


/*
 * Return a DomainType from the ListOfDomainTypes by id.
 */
DomainType*
Geometry::getDomainType(const std::string& sid)
{
	return mDomainTypes.get(sid);
}


/*
 * Return a DomainType from the ListOfDomainTypes by id.
 */
const DomainType*
Geometry::getDomainType(const std::string& sid) const
{
	return mDomainTypes.get(sid);
}


/*
 * Adds a copy the given "DomainType" to this Geometry.
 *
 * @param dt; the DomainType object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(dt)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mDomainTypes.append(dt);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of DomainType objects in this Geometry.
 *
 * @return the number of DomainType objects in this Geometry
 */
unsigned int
Geometry::getNumDomainTypes() const
{
  return mDomainTypes.size();
}


/*
 * Creates a new DomainType object, adds it to this Geometrys
 * Geometry and returns the DomainType object created. 
 *
 * @return a new DomainType object instance
 *
 * @see addDomainType(const DomainType* dt)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(dt != NULL)
  {
    mDomainTypes.appendAndOwn(dt);
  }

  return dt;
}


/*
 * Returns the  "ListOfDomains" in this Geometry object.
 */
const ListOfDomains*
Geometry::getListOfDomains() const
{
  return &mDomains;
}


/*
 * Returns the  "ListOfDomains" in this Geometry object.
 */
ListOfDomains*
Geometry::getListOfDomains()
{
  return &mDomains;
}


/*
 * Removes the nth Domain from the ListOfDomains.
 */
Domain*
Geometry::removeDomain(unsigned int n)
{
	return mDomains.remove(n);
}


/*
 * Removes the a Domain with given id from the ListOfDomains.
 */
Domain*
Geometry::removeDomain(const std::string& sid)
{
	return mDomains.remove(sid);
}


/*
 * Return the nth Domain in the ListOfDomains within this Geometry.
 */
Domain*
Geometry::getDomain(unsigned int n)
{
	return mDomains.get(n);
}


/*
 * Return the nth Domain in the ListOfDomains within this Geometry.
 */
const Domain*
Geometry::getDomain(unsigned int n) const
{
	return mDomains.get(n);
}


/*
 * Return a Domain from the ListOfDomains by id.
 */
Domain*
Geometry::getDomain(const std::string& sid)
{
	return mDomains.get(sid);
}


/*
 * Return a Domain from the ListOfDomains by id.
 */
const Domain*
Geometry::getDomain(const std::string& sid) const
{
	return mDomains.get(sid);
}


/*
 * Adds a copy the given "Domain" to this Geometry.
 *
 * @param d; the Domain object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
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
  else if (getLevel() != d->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != d->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(d)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mDomains.append(d);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of Domain objects in this Geometry.
 *
 * @return the number of Domain objects in this Geometry
 */
unsigned int
Geometry::getNumDomains() const
{
  return mDomains.size();
}


/*
 * Creates a new Domain object, adds it to this Geometrys
 * Geometry and returns the Domain object created. 
 *
 * @return a new Domain object instance
 *
 * @see addDomain(const Domain* d)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(d != NULL)
  {
    mDomains.appendAndOwn(d);
  }

  return d;
}


/*
 * Returns the  "ListOfAdjacentDomains" in this Geometry object.
 */
const ListOfAdjacentDomains*
Geometry::getListOfAdjacentDomains() const
{
  return &mAdjacentDomains;
}


/*
 * Returns the  "ListOfAdjacentDomains" in this Geometry object.
 */
ListOfAdjacentDomains*
Geometry::getListOfAdjacentDomains()
{
  return &mAdjacentDomains;
}


/*
 * Removes the nth AdjacentDomains from the ListOfAdjacentDomains.
 */
AdjacentDomains*
Geometry::removeAdjacentDomains(unsigned int n)
{
	return mAdjacentDomains.remove(n);
}


/*
 * Removes the a AdjacentDomains with given id from the ListOfAdjacentDomains.
 */
AdjacentDomains*
Geometry::removeAdjacentDomains(const std::string& sid)
{
	return mAdjacentDomains.remove(sid);
}


/*
 * Return the nth AdjacentDomains in the ListOfAdjacentDomains within this Geometry.
 */
AdjacentDomains*
Geometry::getAdjacentDomains(unsigned int n)
{
	return mAdjacentDomains.get(n);
}


/*
 * Return the nth AdjacentDomains in the ListOfAdjacentDomains within this Geometry.
 */
const AdjacentDomains*
Geometry::getAdjacentDomains(unsigned int n) const
{
	return mAdjacentDomains.get(n);
}


/*
 * Return a AdjacentDomains from the ListOfAdjacentDomains by id.
 */
AdjacentDomains*
Geometry::getAdjacentDomains(const std::string& sid)
{
	return mAdjacentDomains.get(sid);
}


/*
 * Return a AdjacentDomains from the ListOfAdjacentDomains by id.
 */
const AdjacentDomains*
Geometry::getAdjacentDomains(const std::string& sid) const
{
	return mAdjacentDomains.get(sid);
}


/*
 * Adds a copy the given "AdjacentDomains" to this Geometry.
 *
 * @param ad; the AdjacentDomains object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(ad)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mAdjacentDomains.append(ad);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of AdjacentDomains objects in this Geometry.
 *
 * @return the number of AdjacentDomains objects in this Geometry
 */
unsigned int
Geometry::getNumAdjacentDomains() const
{
  return mAdjacentDomains.size();
}


/*
 * Creates a new AdjacentDomains object, adds it to this Geometrys
 * Geometry and returns the AdjacentDomains object created. 
 *
 * @return a new AdjacentDomains object instance
 *
 * @see addAdjacentDomains(const AdjacentDomains* ad)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(ad != NULL)
  {
    mAdjacentDomains.appendAndOwn(ad);
  }

  return ad;
}


/*
 * Returns the  "ListOfGeometryDefinitions" in this Geometry object.
 */
const ListOfGeometryDefinitions*
Geometry::getListOfGeometryDefinitions() const
{
  return &mGeometryDefinitions;
}


/*
 * Returns the  "ListOfGeometryDefinitions" in this Geometry object.
 */
ListOfGeometryDefinitions*
Geometry::getListOfGeometryDefinitions()
{
  return &mGeometryDefinitions;
}


/*
 * Removes the nth GeometryDefinition from the ListOfGeometryDefinitions.
 */
GeometryDefinition*
Geometry::removeGeometryDefinition(unsigned int n)
{
	return mGeometryDefinitions.remove(n);
}


/*
 * Removes the a GeometryDefinition with given id from the ListOfGeometryDefinitions.
 */
GeometryDefinition*
Geometry::removeGeometryDefinition(const std::string& sid)
{
	return mGeometryDefinitions.remove(sid);
}


/*
 * Return the nth GeometryDefinition in the ListOfGeometryDefinitions within this Geometry.
 */
GeometryDefinition*
Geometry::getGeometryDefinition(unsigned int n)
{
	return mGeometryDefinitions.get(n);
}


/*
 * Return the nth GeometryDefinition in the ListOfGeometryDefinitions within this Geometry.
 */
const GeometryDefinition*
Geometry::getGeometryDefinition(unsigned int n) const
{
	return mGeometryDefinitions.get(n);
}


/*
 * Return a GeometryDefinition from the ListOfGeometryDefinitions by id.
 */
GeometryDefinition*
Geometry::getGeometryDefinition(const std::string& sid)
{
	return mGeometryDefinitions.get(sid);
}


/*
 * Return a GeometryDefinition from the ListOfGeometryDefinitions by id.
 */
const GeometryDefinition*
Geometry::getGeometryDefinition(const std::string& sid) const
{
	return mGeometryDefinitions.get(sid);
}


/*
 * Adds a copy the given "GeometryDefinition" to this Geometry.
 *
 * @param gd; the GeometryDefinition object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(gd)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mGeometryDefinitions.append(gd);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of GeometryDefinition objects in this Geometry.
 *
 * @return the number of GeometryDefinition objects in this Geometry
 */
unsigned int
Geometry::getNumGeometryDefinitions() const
{
  return mGeometryDefinitions.size();
}


/**
 * Creates a new AnalyticGeometry object, adds it to this Geometrys
 * ListOfGeometryDefinitions and returns the AnalyticGeometry object created. 
 *
 * @return a new AnalyticGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(ag != NULL)
  {
    mGeometryDefinitions.appendAndOwn(ag);
  }

  return ag;
}


/**
 * Creates a new SampledFieldGeometry object, adds it to this Geometrys
 * ListOfGeometryDefinitions and returns the SampledFieldGeometry object created. 
 *
 * @return a new SampledFieldGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(sfg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(sfg);
  }

  return sfg;
}


/**
 * Creates a new CSGeometry object, adds it to this Geometrys
 * ListOfGeometryDefinitions and returns the CSGeometry object created. 
 *
 * @return a new CSGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
 */
CSGeometry* 
Geometry::createCsGeometry()
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(csg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(csg);
  }

  return csg;
}


/**
 * Creates a new ParametricGeometry object, adds it to this Geometrys
 * ListOfGeometryDefinitions and returns the ParametricGeometry object created. 
 *
 * @return a new ParametricGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(pg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(pg);
  }

  return pg;
}


/**
 * Creates a new MixedGeometry object, adds it to this Geometrys
 * ListOfGeometryDefinitions and returns the MixedGeometry object created. 
 *
 * @return a new MixedGeometry object instance
 *
 * @see addGeometryDefinition(const GeometryDefinition*)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(mg != NULL)
  {
    mGeometryDefinitions.appendAndOwn(mg);
  }

  return mg;
}


/*
 * Returns the  "ListOfSampledFields" in this Geometry object.
 */
const ListOfSampledFields*
Geometry::getListOfSampledFields() const
{
  return &mSampledFields;
}


/*
 * Returns the  "ListOfSampledFields" in this Geometry object.
 */
ListOfSampledFields*
Geometry::getListOfSampledFields()
{
  return &mSampledFields;
}


/*
 * Removes the nth SampledField from the ListOfSampledFields.
 */
SampledField*
Geometry::removeSampledField(unsigned int n)
{
	return mSampledFields.remove(n);
}


/*
 * Removes the a SampledField with given id from the ListOfSampledFields.
 */
SampledField*
Geometry::removeSampledField(const std::string& sid)
{
	return mSampledFields.remove(sid);
}


/*
 * Return the nth SampledField in the ListOfSampledFields within this Geometry.
 */
SampledField*
Geometry::getSampledField(unsigned int n)
{
	return mSampledFields.get(n);
}


/*
 * Return the nth SampledField in the ListOfSampledFields within this Geometry.
 */
const SampledField*
Geometry::getSampledField(unsigned int n) const
{
	return mSampledFields.get(n);
}


/*
 * Return a SampledField from the ListOfSampledFields by id.
 */
SampledField*
Geometry::getSampledField(const std::string& sid)
{
	return mSampledFields.get(sid);
}


/*
 * Return a SampledField from the ListOfSampledFields by id.
 */
const SampledField*
Geometry::getSampledField(const std::string& sid) const
{
	return mSampledFields.get(sid);
}


/*
 * Adds a copy the given "SampledField" to this Geometry.
 *
 * @param sf; the SampledField object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
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
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(sf)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mSampledFields.append(sf);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of SampledField objects in this Geometry.
 *
 * @return the number of SampledField objects in this Geometry
 */
unsigned int
Geometry::getNumSampledFields() const
{
  return mSampledFields.size();
}


/*
 * Creates a new SampledField object, adds it to this Geometrys
 * Geometry and returns the SampledField object created. 
 *
 * @return a new SampledField object instance
 *
 * @see addSampledField(const SampledField* sf)
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
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(sf != NULL)
  {
    mSampledFields.appendAndOwn(sf);
  }

  return sf;
}


List*
Geometry::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
Geometry::getElementName () const
{
  static const string name = "geometry";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
Geometry::getTypeCode () const
{
  return SBML_SPATIAL_GEOMETRY;
}


/*
 * check if all the required attributes are set
 */
bool
Geometry::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetCoordinateSystem() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
Geometry::hasRequiredElements () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
Geometry::writeElements (XMLOutputStream& stream) const
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


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
Geometry::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */

  v.leave(*this);

  return true;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
Geometry::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  mCoordinateComponents.setSBMLDocument(d);
  mDomainTypes.setSBMLDocument(d);
  mDomains.setSBMLDocument(d);
  mAdjacentDomains.setSBMLDocument(d);
  mGeometryDefinitions.setSBMLDocument(d);
  mSampledFields.setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
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


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
Geometry::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mCoordinateComponents.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mDomainTypes.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mDomains.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mAdjacentDomains.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mGeometryDefinitions.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mSampledFields.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
Geometry::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const string& name = stream.peek().getName();

  if (name == "listOfCoordinateComponents")
  {
    object = &mCoordinateComponents;
  }
  else if (name == "listOfDomainTypes")
  {
    object = &mDomainTypes;
  }
  else if (name == "listOfDomains")
  {
    object = &mDomains;
  }
  else if (name == "listOfAdjacentDomains")
  {
    object = &mAdjacentDomains;
  }
  else if (name == "listOfGeometryDefinitions")
  {
    object = &mGeometryDefinitions;
  }
  else if (name == "listOfSampledFields")
  {
    object = &mSampledFields;
  }
  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
Geometry::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("coordinateSystem");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
Geometry::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // id SId  ( use = "required" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<Geometry>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'geometry' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // coordinateSystem enum  ( use = "required" )
  //
  mCoordinateSystem = GEOMETRYKIND_UNKNOWN;
  std::string stringValue;
  assigned = attributes.readInto("coordinateSystem", stringValue);

  if (assigned == true)
  {
    // parse enum

    mCoordinateSystem = GeometryKind_parse(stringValue.c_str());
    if(mCoordinateSystem == GEOMETRYKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'coordinateSystem' in 'geometry' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  if(mCoordinateSystem == GEOMETRYKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'coordinateSystem' is missing from 'geometry' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
Geometry::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetCoordinateSystem() == true)
    stream.writeAttribute("coordinateSystem", getPrefix(), GeometryKind_toString(mCoordinateSystem));

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
Geometry_t *
Geometry_create(unsigned int level, unsigned int version,
                unsigned int pkgVersion)
{
  return new Geometry(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
Geometry_free(Geometry_t * g)
{
  if (g != NULL)
    delete g;
}


LIBSBML_EXTERN
Geometry_t *
Geometry_clone(Geometry_t * g)
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


LIBSBML_EXTERN
const char *
Geometry_getId(const Geometry_t * g)
{
	return (g != NULL && g->isSetId()) ? g->getId().c_str() : NULL;
}


LIBSBML_EXTERN
GeometryKind_t
Geometry_getCoordinateSystem(const Geometry_t * g)
{
	return (g != NULL) ? g->getCoordinateSystem() : GEOMETRYKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
Geometry_isSetId(const Geometry_t * g)
{
  return (g != NULL) ? static_cast<int>(g->isSetId()) : 0;
}


LIBSBML_EXTERN
int
Geometry_isSetCoordinateSystem(const Geometry_t * g)
{
  return (g != NULL) ? static_cast<int>(g->isSetCoordinateSystem()) : 0;
}


LIBSBML_EXTERN
int
Geometry_setId(Geometry_t * g, const char * id)
{
  if (g != NULL)
    return (id == NULL) ? g->setId("") : g->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Geometry_setCoordinateSystem(Geometry_t * g, GeometryKind_t coordinateSystem)
{
  if (g != NULL)
    return g->setCoordinateSystem(coordinateSystem);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Geometry_unsetId(Geometry_t * g)
{
  return (g != NULL) ? g->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Geometry_unsetCoordinateSystem(Geometry_t * g)
{
  return (g != NULL) ? g->unsetCoordinateSystem() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Geometry_addCoordinateComponent(Geometry_t * g, CoordinateComponent_t * cc)
{
	return  (g != NULL) ? g->addCoordinateComponent(cc) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_createCoordinateComponent(Geometry_t * g)
{
	return  (g != NULL) ? g->createCoordinateComponent() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfCoordinateComponents(Geometry_t * g)
{
	return  (g != NULL) ? (ListOf_t *)g->getListOfCoordinateComponents() : NULL;
}

LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_getCoordinateComponent(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->getCoordinateComponent(n) : NULL;
}

LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_getCoordinateComponentById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->getCoordinateComponent(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
Geometry_getNumCoordinateComponents(Geometry_t * g)
{
	return  (g != NULL) ? g->getNumCoordinateComponents() : SBML_INT_MAX;
}

LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_removeCoordinateComponent(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->removeCoordinateComponent(n) : NULL;
}

LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_removeCoordinateComponentById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->removeCoordinateComponent(sid) : NULL;
}

LIBSBML_EXTERN
int
Geometry_addDomainType(Geometry_t * g, DomainType_t * dt)
{
	return  (g != NULL) ? g->addDomainType(dt) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
DomainType_t *
Geometry_createDomainType(Geometry_t * g)
{
	return  (g != NULL) ? g->createDomainType() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfDomainTypes(Geometry_t * g)
{
	return  (g != NULL) ? (ListOf_t *)g->getListOfDomainTypes() : NULL;
}

LIBSBML_EXTERN
DomainType_t *
Geometry_getDomainType(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->getDomainType(n) : NULL;
}

LIBSBML_EXTERN
DomainType_t *
Geometry_getDomainTypeById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->getDomainType(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
Geometry_getNumDomainTypes(Geometry_t * g)
{
	return  (g != NULL) ? g->getNumDomainTypes() : SBML_INT_MAX;
}

LIBSBML_EXTERN
DomainType_t *
Geometry_removeDomainType(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->removeDomainType(n) : NULL;
}

LIBSBML_EXTERN
DomainType_t *
Geometry_removeDomainTypeById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->removeDomainType(sid) : NULL;
}

LIBSBML_EXTERN
int
Geometry_addDomain(Geometry_t * g, Domain_t * d)
{
	return  (g != NULL) ? g->addDomain(d) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
Domain_t *
Geometry_createDomain(Geometry_t * g)
{
	return  (g != NULL) ? g->createDomain() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfDomains(Geometry_t * g)
{
	return  (g != NULL) ? (ListOf_t *)g->getListOfDomains() : NULL;
}

LIBSBML_EXTERN
Domain_t *
Geometry_getDomain(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->getDomain(n) : NULL;
}

LIBSBML_EXTERN
Domain_t *
Geometry_getDomainById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->getDomain(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
Geometry_getNumDomains(Geometry_t * g)
{
	return  (g != NULL) ? g->getNumDomains() : SBML_INT_MAX;
}

LIBSBML_EXTERN
Domain_t *
Geometry_removeDomain(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->removeDomain(n) : NULL;
}

LIBSBML_EXTERN
Domain_t *
Geometry_removeDomainById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->removeDomain(sid) : NULL;
}

LIBSBML_EXTERN
int
Geometry_addAdjacentDomains(Geometry_t * g, AdjacentDomains_t * ad)
{
	return  (g != NULL) ? g->addAdjacentDomains(ad) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_createAdjacentDomains(Geometry_t * g)
{
	return  (g != NULL) ? g->createAdjacentDomains() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfAdjacentDomains(Geometry_t * g)
{
	return  (g != NULL) ? (ListOf_t *)g->getListOfAdjacentDomains() : NULL;
}

LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_getAdjacentDomains(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->getAdjacentDomains(n) : NULL;
}

LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_getAdjacentDomainsById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->getAdjacentDomains(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
Geometry_getNumAdjacentDomains(Geometry_t * g)
{
	return  (g != NULL) ? g->getNumAdjacentDomains() : SBML_INT_MAX;
}

LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_removeAdjacentDomains(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->removeAdjacentDomains(n) : NULL;
}

LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_removeAdjacentDomainsById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->removeAdjacentDomains(sid) : NULL;
}

LIBSBML_EXTERN
int
Geometry_addGeometryDefinition(Geometry_t * g, GeometryDefinition_t * gd)
{
	return  (g != NULL) ? g->addGeometryDefinition(gd) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
AnalyticGeometry_t *
Geometry_createAnalyticGeometry(Geometry_t * g)
{
	return  (g != NULL) ? g->createAnalyticGeometry() : NULL;
}

LIBSBML_EXTERN
SampledFieldGeometry_t *
Geometry_createSampledFieldGeometry(Geometry_t * g)
{
	return  (g != NULL) ? g->createSampledFieldGeometry() : NULL;
}

LIBSBML_EXTERN
CSGeometry_t *
Geometry_createCsGeometry(Geometry_t * g)
{
	return  (g != NULL) ? g->createCsGeometry() : NULL;
}

LIBSBML_EXTERN
ParametricGeometry_t *
Geometry_createParametricGeometry(Geometry_t * g)
{
	return  (g != NULL) ? g->createParametricGeometry() : NULL;
}

LIBSBML_EXTERN
MixedGeometry_t *
Geometry_createMixedGeometry(Geometry_t * g)
{
	return  (g != NULL) ? g->createMixedGeometry() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfGeometryDefinitions(Geometry_t * g)
{
	return  (g != NULL) ? (ListOf_t *)g->getListOfGeometryDefinitions() : NULL;
}

LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_getGeometryDefinition(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->getGeometryDefinition(n) : NULL;
}

LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_getGeometryDefinitionById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->getGeometryDefinition(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
Geometry_getNumGeometryDefinitions(Geometry_t * g)
{
	return  (g != NULL) ? g->getNumGeometryDefinitions() : SBML_INT_MAX;
}

LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_removeGeometryDefinition(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->removeGeometryDefinition(n) : NULL;
}

LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_removeGeometryDefinitionById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->removeGeometryDefinition(sid) : NULL;
}

LIBSBML_EXTERN
int
Geometry_addSampledField(Geometry_t * g, SampledField_t * sf)
{
	return  (g != NULL) ? g->addSampledField(sf) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
SampledField_t *
Geometry_createSampledField(Geometry_t * g)
{
	return  (g != NULL) ? g->createSampledField() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfSampledFields(Geometry_t * g)
{
	return  (g != NULL) ? (ListOf_t *)g->getListOfSampledFields() : NULL;
}

LIBSBML_EXTERN
SampledField_t *
Geometry_getSampledField(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->getSampledField(n) : NULL;
}

LIBSBML_EXTERN
SampledField_t *
Geometry_getSampledFieldById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->getSampledField(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
Geometry_getNumSampledFields(Geometry_t * g)
{
	return  (g != NULL) ? g->getNumSampledFields() : SBML_INT_MAX;
}

LIBSBML_EXTERN
SampledField_t *
Geometry_removeSampledField(Geometry_t * g, unsigned int n)
{
	return  (g != NULL) ? g->removeSampledField(n) : NULL;
}

LIBSBML_EXTERN
SampledField_t *
Geometry_removeSampledFieldById(Geometry_t * g, const char * sid)
{
	return  (g != NULL) ? g->removeSampledField(sid) : NULL;
}

LIBSBML_EXTERN
int
Geometry_hasRequiredAttributes(const Geometry_t * g)
{
  return (g != NULL) ? static_cast<int>(g->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
Geometry_hasRequiredElements(const Geometry_t * g)
{
	return (g != NULL) ? static_cast<int>(g->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


