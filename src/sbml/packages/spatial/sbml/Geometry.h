/**
 * @file:   Geometry.h
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


#ifndef Geometry_H__
#define Geometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/packages/spatial/sbml/CoordinateComponent.h>
#include <sbml/packages/spatial/sbml/DomainType.h>
#include <sbml/packages/spatial/sbml/Domain.h>
#include <sbml/packages/spatial/sbml/AdjacentDomains.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/SampledField.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN Geometry : public SBase
{

protected:

  std::string   mId;
  GeometryKind_t   mCoordinateSystem;
  ListOfCoordinateComponents   mCoordinateComponents;
  ListOfDomainTypes   mDomainTypes;
  ListOfDomains   mDomains;
  ListOfAdjacentDomains   mAdjacentDomains;
  ListOfGeometryDefinitions   mGeometryDefinitions;
  ListOfSampledFields   mSampledFields;


public:

  /**
   * Creates a new Geometry with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Geometry
   *
   * @param version an unsigned int, the SBML Version to assign to this Geometry
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this Geometry
   */
  Geometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
           unsigned int version    = SpatialExtension::getDefaultVersion(),
           unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new Geometry with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  Geometry(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for Geometry.
   *
   * @param orig; the Geometry instance to copy.
   */
  Geometry(const Geometry& orig);


   /**
   * Assignment operator for Geometry.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  Geometry& operator=(const Geometry& rhs);


   /**
   * Creates and returns a deep copy of this Geometry object.
   *
   * @return a (deep) copy of this Geometry object.
   */
  virtual Geometry* clone () const;


   /**
   * Destructor for Geometry.
   */
  virtual ~Geometry();


   /**
   * Returns the value of the "id" attribute of this Geometry.
   *
   * @return the value of the "id" attribute of this Geometry as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "coordinateSystem" attribute of this Geometry.
   *
   * @return the value of the "coordinateSystem" attribute of this Geometry as a GeometryKind_t.
   */
  virtual GeometryKind_t getCoordinateSystem() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Geometry's "id" attribute has been set.
   *
   * @return @c true if this Geometry's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Geometry's "coordinateSystem" attribute has been set.
   *
   * @return @c true if this Geometry's "coordinateSystem" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoordinateSystem() const;


  /**
   * Sets the value of the "id" attribute of this Geometry.
   *
   * @param id; const std::string& value of the "id" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "coordinateSystem" attribute of this Geometry.
   *
   * @param coordinateSystem; GeometryKind_t value of the "coordinateSystem" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinateSystem(GeometryKind_t coordinateSystem);


  /**
   * Sets the value of the "coordinateSystem" attribute of this Geometry.
   *
   * @param coordinateSystem; string value of the "coordinateSystem" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinateSystem(const std::string& coordinateSystem);


  /**
   * Unsets the value of the "id" attribute of this Geometry.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "coordinateSystem" attribute of this Geometry.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoordinateSystem();


  /**
   * Returns the  "ListOfCoordinateComponents" in this Geometry object.
   *
   * @return the "ListOfCoordinateComponents" attribute of this Geometry.
   */
  const ListOfCoordinateComponents* getListOfCoordinateComponents() const;


  /**
   * Returns the  "ListOfCoordinateComponents" in this Geometry object.
   *
   * @return the "ListOfCoordinateComponents" attribute of this Geometry.
   */
  ListOfCoordinateComponents* getListOfCoordinateComponents();


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents.
   *
   * @param n the index number of the CoordinateComponent to get.
   *
   * @return the nth CoordinateComponent in the ListOfCoordinateComponents within this Geometry.
   *
   * @see getNumCoordinateComponents()
   */
	CoordinateComponent* getCoordinateComponent(unsigned int n);


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents.
   *
   * @param n the index number of the CoordinateComponent to get.
   *
   * @return the nth CoordinateComponent in the ListOfCoordinateComponents within this Geometry.
   *
   * @see getNumCoordinateComponents()
   */
	const CoordinateComponent* getCoordinateComponent(unsigned int n) const;


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CoordinateComponent to get.
   *
   * @return the CoordinateComponent in the ListOfCoordinateComponents
   * with the given id or NULL if no such
   * CoordinateComponent exists.
   *
   * @see getCoordinateComponent(unsigned int n)
   *
   * @see getNumCoordinateComponents()
   */
	CoordinateComponent* getCoordinateComponent(const std::string& sid);


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CoordinateComponent to get.
   *
   * @return the CoordinateComponent in the ListOfCoordinateComponents
   * with the given id or NULL if no such
   * CoordinateComponent exists.
   *
   * @see getCoordinateComponent(unsigned int n)
   *
   * @see getNumCoordinateComponents()
   */
	const CoordinateComponent* getCoordinateComponent(const std::string& sid) const;


  /**
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
  int addCoordinateComponent(const CoordinateComponent* cc);


  /**
   * Get the number of CoordinateComponent objects in this Geometry.
   *
   * @return the number of CoordinateComponent objects in this Geometry
   */
  unsigned int getNumCoordinateComponents() const;


  /**
   * Creates a new CoordinateComponent object, adds it to this Geometrys
   * ListOfCoordinateComponents and returns the CoordinateComponent object created. 
   *
   * @return a new CoordinateComponent object instance
   *
   * @see addCoordinateComponent(const CoordinateComponent* cc)
   */
  CoordinateComponent* createCoordinateComponent();


  /**
   * Removes the nth CoordinateComponent from the ListOfCoordinateComponents within this Geometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the CoordinateComponent to remove.
   *
   * @see getNumCoordinateComponents()
   */
	CoordinateComponent* removeCoordinateComponent(unsigned int n);


  /**
   * Removes the CoordinateComponent with the given identifier from the ListOfCoordinateComponents within this Geometry
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the CoordinateComponent to remove.
   *
   * @return the CoordinateComponent removed. As mentioned above, the caller owns the
   * returned item.
   */
	CoordinateComponent* removeCoordinateComponent(const std::string& sid);


  /**
   * Returns the  "ListOfDomainTypes" in this Geometry object.
   *
   * @return the "ListOfDomainTypes" attribute of this Geometry.
   */
  const ListOfDomainTypes* getListOfDomainTypes() const;


  /**
   * Returns the  "ListOfDomainTypes" in this Geometry object.
   *
   * @return the "ListOfDomainTypes" attribute of this Geometry.
   */
  ListOfDomainTypes* getListOfDomainTypes();


  /**
   * Get a DomainType from the ListOfDomainTypes.
   *
   * @param n the index number of the DomainType to get.
   *
   * @return the nth DomainType in the ListOfDomainTypes within this Geometry.
   *
   * @see getNumDomainTypes()
   */
	DomainType* getDomainType(unsigned int n);


  /**
   * Get a DomainType from the ListOfDomainTypes.
   *
   * @param n the index number of the DomainType to get.
   *
   * @return the nth DomainType in the ListOfDomainTypes within this Geometry.
   *
   * @see getNumDomainTypes()
   */
	const DomainType* getDomainType(unsigned int n) const;


  /**
   * Get a DomainType from the ListOfDomainTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the DomainType to get.
   *
   * @return the DomainType in the ListOfDomainTypes
   * with the given id or NULL if no such
   * DomainType exists.
   *
   * @see getDomainType(unsigned int n)
   *
   * @see getNumDomainTypes()
   */
	DomainType* getDomainType(const std::string& sid);


  /**
   * Get a DomainType from the ListOfDomainTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the DomainType to get.
   *
   * @return the DomainType in the ListOfDomainTypes
   * with the given id or NULL if no such
   * DomainType exists.
   *
   * @see getDomainType(unsigned int n)
   *
   * @see getNumDomainTypes()
   */
	const DomainType* getDomainType(const std::string& sid) const;


  /**
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
  int addDomainType(const DomainType* dt);


  /**
   * Get the number of DomainType objects in this Geometry.
   *
   * @return the number of DomainType objects in this Geometry
   */
  unsigned int getNumDomainTypes() const;


  /**
   * Creates a new DomainType object, adds it to this Geometrys
   * ListOfDomainTypes and returns the DomainType object created. 
   *
   * @return a new DomainType object instance
   *
   * @see addDomainType(const DomainType* dt)
   */
  DomainType* createDomainType();


  /**
   * Removes the nth DomainType from the ListOfDomainTypes within this Geometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the DomainType to remove.
   *
   * @see getNumDomainTypes()
   */
	DomainType* removeDomainType(unsigned int n);


  /**
   * Removes the DomainType with the given identifier from the ListOfDomainTypes within this Geometry
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the DomainType to remove.
   *
   * @return the DomainType removed. As mentioned above, the caller owns the
   * returned item.
   */
	DomainType* removeDomainType(const std::string& sid);


  /**
   * Returns the  "ListOfDomains" in this Geometry object.
   *
   * @return the "ListOfDomains" attribute of this Geometry.
   */
  const ListOfDomains* getListOfDomains() const;


  /**
   * Returns the  "ListOfDomains" in this Geometry object.
   *
   * @return the "ListOfDomains" attribute of this Geometry.
   */
  ListOfDomains* getListOfDomains();


  /**
   * Get a Domain from the ListOfDomains.
   *
   * @param n the index number of the Domain to get.
   *
   * @return the nth Domain in the ListOfDomains within this Geometry.
   *
   * @see getNumDomains()
   */
	Domain* getDomain(unsigned int n);


  /**
   * Get a Domain from the ListOfDomains.
   *
   * @param n the index number of the Domain to get.
   *
   * @return the nth Domain in the ListOfDomains within this Geometry.
   *
   * @see getNumDomains()
   */
	const Domain* getDomain(unsigned int n) const;


  /**
   * Get a Domain from the ListOfDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Domain to get.
   *
   * @return the Domain in the ListOfDomains
   * with the given id or NULL if no such
   * Domain exists.
   *
   * @see getDomain(unsigned int n)
   *
   * @see getNumDomains()
   */
	Domain* getDomain(const std::string& sid);


  /**
   * Get a Domain from the ListOfDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Domain to get.
   *
   * @return the Domain in the ListOfDomains
   * with the given id or NULL if no such
   * Domain exists.
   *
   * @see getDomain(unsigned int n)
   *
   * @see getNumDomains()
   */
	const Domain* getDomain(const std::string& sid) const;


  /**
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
  int addDomain(const Domain* d);


  /**
   * Get the number of Domain objects in this Geometry.
   *
   * @return the number of Domain objects in this Geometry
   */
  unsigned int getNumDomains() const;


  /**
   * Creates a new Domain object, adds it to this Geometrys
   * ListOfDomains and returns the Domain object created. 
   *
   * @return a new Domain object instance
   *
   * @see addDomain(const Domain* d)
   */
  Domain* createDomain();


  /**
   * Removes the nth Domain from the ListOfDomains within this Geometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the Domain to remove.
   *
   * @see getNumDomains()
   */
	Domain* removeDomain(unsigned int n);


  /**
   * Removes the Domain with the given identifier from the ListOfDomains within this Geometry
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the Domain to remove.
   *
   * @return the Domain removed. As mentioned above, the caller owns the
   * returned item.
   */
	Domain* removeDomain(const std::string& sid);


  /**
   * Returns the  "ListOfAdjacentDomains" in this Geometry object.
   *
   * @return the "ListOfAdjacentDomains" attribute of this Geometry.
   */
  const ListOfAdjacentDomains* getListOfAdjacentDomains() const;


  /**
   * Returns the  "ListOfAdjacentDomains" in this Geometry object.
   *
   * @return the "ListOfAdjacentDomains" attribute of this Geometry.
   */
  ListOfAdjacentDomains* getListOfAdjacentDomains();


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains.
   *
   * @param n the index number of the AdjacentDomains to get.
   *
   * @return the nth AdjacentDomains in the ListOfAdjacentDomains within this Geometry.
   *
   * @see getNumAdjacentDomainss()
   */
	AdjacentDomains* getAdjacentDomains(unsigned int n);


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains.
   *
   * @param n the index number of the AdjacentDomains to get.
   *
   * @return the nth AdjacentDomains in the ListOfAdjacentDomains within this Geometry.
   *
   * @see getNumAdjacentDomainss()
   */
	const AdjacentDomains* getAdjacentDomains(unsigned int n) const;


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the AdjacentDomains to get.
   *
   * @return the AdjacentDomains in the ListOfAdjacentDomains
   * with the given id or NULL if no such
   * AdjacentDomains exists.
   *
   * @see getAdjacentDomains(unsigned int n)
   *
   * @see getNumAdjacentDomainss()
   */
	AdjacentDomains* getAdjacentDomains(const std::string& sid);


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the AdjacentDomains to get.
   *
   * @return the AdjacentDomains in the ListOfAdjacentDomains
   * with the given id or NULL if no such
   * AdjacentDomains exists.
   *
   * @see getAdjacentDomains(unsigned int n)
   *
   * @see getNumAdjacentDomainss()
   */
	const AdjacentDomains* getAdjacentDomains(const std::string& sid) const;


  /**
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
  int addAdjacentDomains(const AdjacentDomains* ad);


  /**
   * Get the number of AdjacentDomains objects in this Geometry.
   *
   * @return the number of AdjacentDomains objects in this Geometry
   */
  unsigned int getNumAdjacentDomains() const;


  /**
   * Creates a new AdjacentDomains object, adds it to this Geometrys
   * ListOfAdjacentDomains and returns the AdjacentDomains object created. 
   *
   * @return a new AdjacentDomains object instance
   *
   * @see addAdjacentDomains(const AdjacentDomains* ad)
   */
  AdjacentDomains* createAdjacentDomains();


  /**
   * Removes the nth AdjacentDomains from the ListOfAdjacentDomains within this Geometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the AdjacentDomains to remove.
   *
   * @see getNumAdjacentDomainss()
   */
	AdjacentDomains* removeAdjacentDomains(unsigned int n);


  /**
   * Removes the AdjacentDomains with the given identifier from the ListOfAdjacentDomains within this Geometry
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the AdjacentDomains to remove.
   *
   * @return the AdjacentDomains removed. As mentioned above, the caller owns the
   * returned item.
   */
	AdjacentDomains* removeAdjacentDomains(const std::string& sid);


  /**
   * Returns the  "ListOfGeometryDefinitions" in this Geometry object.
   *
   * @return the "ListOfGeometryDefinitions" attribute of this Geometry.
   */
  const ListOfGeometryDefinitions* getListOfGeometryDefinitions() const;


  /**
   * Returns the  "ListOfGeometryDefinitions" in this Geometry object.
   *
   * @return the "ListOfGeometryDefinitions" attribute of this Geometry.
   */
  ListOfGeometryDefinitions* getListOfGeometryDefinitions();


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n the index number of the GeometryDefinition to get.
   *
   * @return the nth GeometryDefinition in the ListOfGeometryDefinitions within this Geometry.
   *
   * @see getNumGeometryDefinitions()
   */
	GeometryDefinition* getGeometryDefinition(unsigned int n);


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n the index number of the GeometryDefinition to get.
   *
   * @return the nth GeometryDefinition in the ListOfGeometryDefinitions within this Geometry.
   *
   * @see getNumGeometryDefinitions()
   */
	const GeometryDefinition* getGeometryDefinition(unsigned int n) const;


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeometryDefinition to get.
   *
   * @return the GeometryDefinition in the ListOfGeometryDefinitions
   * with the given id or NULL if no such
   * GeometryDefinition exists.
   *
   * @see getGeometryDefinition(unsigned int n)
   *
   * @see getNumGeometryDefinitions()
   */
	GeometryDefinition* getGeometryDefinition(const std::string& sid);


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeometryDefinition to get.
   *
   * @return the GeometryDefinition in the ListOfGeometryDefinitions
   * with the given id or NULL if no such
   * GeometryDefinition exists.
   *
   * @see getGeometryDefinition(unsigned int n)
   *
   * @see getNumGeometryDefinitions()
   */
	const GeometryDefinition* getGeometryDefinition(const std::string& sid) const;


  /**
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
  int addGeometryDefinition(const GeometryDefinition* gd);


  /**
   * Get the number of GeometryDefinition objects in this Geometry.
   *
   * @return the number of GeometryDefinition objects in this Geometry
   */
  unsigned int getNumGeometryDefinitions() const;


  /**
   * Creates a new AnalyticGeometry object, adds it to this Geometrys
   * ListOfGeometryDefinitions and returns the AnalyticGeometry object created. 
   *
   * @return a new AnalyticGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  AnalyticGeometry* createAnalyticGeometry();


  /**
   * Creates a new SampledFieldGeometry object, adds it to this Geometrys
   * ListOfGeometryDefinitions and returns the SampledFieldGeometry object created. 
   *
   * @return a new SampledFieldGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  SampledFieldGeometry* createSampledFieldGeometry();


  /**
   * Creates a new CSGeometry object, adds it to this Geometrys
   * ListOfGeometryDefinitions and returns the CSGeometry object created. 
   *
   * @return a new CSGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  CSGeometry* createCsGeometry();


  /**
   * Creates a new ParametricGeometry object, adds it to this Geometrys
   * ListOfGeometryDefinitions and returns the ParametricGeometry object created. 
   *
   * @return a new ParametricGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  ParametricGeometry* createParametricGeometry();


  /**
   * Creates a new MixedGeometry object, adds it to this Geometrys
   * ListOfGeometryDefinitions and returns the MixedGeometry object created. 
   *
   * @return a new MixedGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  MixedGeometry* createMixedGeometry();


  /**
   * Removes the nth GeometryDefinition from the ListOfGeometryDefinitions within this Geometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the GeometryDefinition to remove.
   *
   * @see getNumGeometryDefinitions()
   */
	GeometryDefinition* removeGeometryDefinition(unsigned int n);


  /**
   * Removes the GeometryDefinition with the given identifier from the ListOfGeometryDefinitions within this Geometry
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the GeometryDefinition to remove.
   *
   * @return the GeometryDefinition removed. As mentioned above, the caller owns the
   * returned item.
   */
	GeometryDefinition* removeGeometryDefinition(const std::string& sid);


  /**
   * Returns the  "ListOfSampledFields" in this Geometry object.
   *
   * @return the "ListOfSampledFields" attribute of this Geometry.
   */
  const ListOfSampledFields* getListOfSampledFields() const;


  /**
   * Returns the  "ListOfSampledFields" in this Geometry object.
   *
   * @return the "ListOfSampledFields" attribute of this Geometry.
   */
  ListOfSampledFields* getListOfSampledFields();


  /**
   * Get a SampledField from the ListOfSampledFields.
   *
   * @param n the index number of the SampledField to get.
   *
   * @return the nth SampledField in the ListOfSampledFields within this Geometry.
   *
   * @see getNumSampledFields()
   */
	SampledField* getSampledField(unsigned int n);


  /**
   * Get a SampledField from the ListOfSampledFields.
   *
   * @param n the index number of the SampledField to get.
   *
   * @return the nth SampledField in the ListOfSampledFields within this Geometry.
   *
   * @see getNumSampledFields()
   */
	const SampledField* getSampledField(unsigned int n) const;


  /**
   * Get a SampledField from the ListOfSampledFields
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SampledField to get.
   *
   * @return the SampledField in the ListOfSampledFields
   * with the given id or NULL if no such
   * SampledField exists.
   *
   * @see getSampledField(unsigned int n)
   *
   * @see getNumSampledFields()
   */
	SampledField* getSampledField(const std::string& sid);


  /**
   * Get a SampledField from the ListOfSampledFields
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SampledField to get.
   *
   * @return the SampledField in the ListOfSampledFields
   * with the given id or NULL if no such
   * SampledField exists.
   *
   * @see getSampledField(unsigned int n)
   *
   * @see getNumSampledFields()
   */
	const SampledField* getSampledField(const std::string& sid) const;


  /**
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
  int addSampledField(const SampledField* sf);


  /**
   * Get the number of SampledField objects in this Geometry.
   *
   * @return the number of SampledField objects in this Geometry
   */
  unsigned int getNumSampledFields() const;


  /**
   * Creates a new SampledField object, adds it to this Geometrys
   * ListOfSampledFields and returns the SampledField object created. 
   *
   * @return a new SampledField object instance
   *
   * @see addSampledField(const SampledField* sf)
   */
  SampledField* createSampledField();


  /**
   * Removes the nth SampledField from the ListOfSampledFields within this Geometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SampledField to remove.
   *
   * @see getNumSampledFields()
   */
	SampledField* removeSampledField(unsigned int n);


  /**
   * Removes the SampledField with the given identifier from the ListOfSampledFields within this Geometry
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SampledField to remove.
   *
   * @return the SampledField removed. As mentioned above, the caller owns the
   * returned item.
   */
	SampledField* removeSampledField(const std::string& sid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for Geometry, is
   * always @c "geometry".
   *
   * @return the name of this element, i.e. @c "geometry".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Predicate returning @c true if all the required attributes
   * for this Geometry object have been set.
   *
   * @note The required attributes for a Geometry object are:
   * @li "id"
   * @li "coordinateSystem"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this Geometry object have been set.
   *
   * @note The required elements for a Geometry object are:
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements.
   */
  virtual void connectToChild ();


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new Geometry_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * Geometry_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * Geometry_t structure.
 *
 * @returns the newly-created Geometry_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
Geometry_t *
Geometry_create(unsigned int level, unsigned int version,
                unsigned int pkgVersion);


/**
 * Frees the given Geometry_t structure.
 * 
 * @param g the Geometry_t structure to be freed.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
void
Geometry_free(Geometry_t * g);


/**
 * Creates a deep copy of the given Geometry_t structure.
 * 
 * @param g the Geometry_t structure to be copied.
 *
 * @returns a (deep) copy of the given Geometry_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
Geometry_t *
Geometry_clone(Geometry_t * g);


/**
 * Returns the value of the "id" attribute of the given Geometry_t
 * structure.
 *
 * @param g the Geometry_t structure.
 *
 * @return the id of this structure.
 *
 * @member of Geometry_t
 */
LIBSBML_EXTERN
const char *
Geometry_getId(const Geometry_t * g);


/**
 * Returns the value of the "coordinateSystem" attribute of the given Geometry_t
 * structure.
 *
 * @param g the Geometry_t structure.
 *
 * @return the coordinateSystem of this structure.
 *
 * @member of Geometry_t
 */
LIBSBML_EXTERN
GeometryKind_t
Geometry_getCoordinateSystem(const Geometry_t * g);


/**
 * Predicate returning @c 1 if the given Geometry_t structure's "id"
 * is set.
 *
 * @param g the Geometry_t structure.
 *
 * @return @c 1 if the "id" of this Geometry_t structure is
 * set, @c 0 otherwise.
 *
 * @member of Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_isSetId(const Geometry_t * g);


/**
 * Predicate returning @c 1 if the given Geometry_t structure's "coordinateSystem"
 * is set.
 *
 * @param g the Geometry_t structure.
 *
 * @return @c 1 if the "coordinateSystem" of this Geometry_t structure is
 * set, @c 0 otherwise.
 *
 * @member of Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_isSetCoordinateSystem(const Geometry_t * g);


/**
 * Sets the "id" attribute of the given Geometry_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs Geometry_unsetId() instead.
 *
 * @param g the Geometry_t structure.
 *
 * @param id the string to which the structures "id" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_setId(Geometry_t * g, const char * id);


/**
 * Sets the "coordinateSystem" attribute of the given Geometry_t structure.
 *
 * @param g the Geometry_t structure.
 *
 * @param coordinateSystem the string to which the structures "coordinateSystem" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_setCoordinateSystem(Geometry_t * g, GeometryKind_t coordinateSystem);


/**
 * Unsets the value of the "id" attribute of the given 
 *Geometry_t structure.
 *
 * @param g the Geometry_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_unsetId(Geometry_t * g);


/**
 * Unsets the value of the "coordinateSystem" attribute of the given 
 *Geometry_t structure.
 *
 * @param g the Geometry_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_unsetCoordinateSystem(Geometry_t * g);


LIBSBML_EXTERN
int
Geometry_addCoordinateComponent(Geometry_t * g, CoordinateComponent_t * cc);


LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_createCoordinateComponent(Geometry_t * g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfCoordinateComponents(Geometry_t * g) ;


LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_getCoordinateComponent(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_getCoordinateComponentById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
unsigned int
Geometry_getNumCoordinateComponents(Geometry_t * g);


LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_removeCoordinateComponent(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_removeCoordinateComponentById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
int
Geometry_addDomainType(Geometry_t * g, DomainType_t * dt);


LIBSBML_EXTERN
DomainType_t *
Geometry_createDomainType(Geometry_t * g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfDomainTypes(Geometry_t * g) ;


LIBSBML_EXTERN
DomainType_t *
Geometry_getDomainType(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
DomainType_t *
Geometry_getDomainTypeById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
unsigned int
Geometry_getNumDomainTypes(Geometry_t * g);


LIBSBML_EXTERN
DomainType_t *
Geometry_removeDomainType(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
DomainType_t *
Geometry_removeDomainTypeById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
int
Geometry_addDomain(Geometry_t * g, Domain_t * d);


LIBSBML_EXTERN
Domain_t *
Geometry_createDomain(Geometry_t * g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfDomains(Geometry_t * g) ;


LIBSBML_EXTERN
Domain_t *
Geometry_getDomain(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
Domain_t *
Geometry_getDomainById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
unsigned int
Geometry_getNumDomains(Geometry_t * g);


LIBSBML_EXTERN
Domain_t *
Geometry_removeDomain(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
Domain_t *
Geometry_removeDomainById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
int
Geometry_addAdjacentDomains(Geometry_t * g, AdjacentDomains_t * ad);


LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_createAdjacentDomains(Geometry_t * g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfAdjacentDomains(Geometry_t * g) ;


LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_getAdjacentDomains(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_getAdjacentDomainsById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
unsigned int
Geometry_getNumAdjacentDomains(Geometry_t * g);


LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_removeAdjacentDomains(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_removeAdjacentDomainsById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
int
Geometry_addGeometryDefinition(Geometry_t * g, GeometryDefinition_t * gd);


LIBSBML_EXTERN
AnalyticGeometry_t *
Geometry_createAnalyticGeometry(Geometry_t * g);


LIBSBML_EXTERN
SampledFieldGeometry_t *
Geometry_createSampledFieldGeometry(Geometry_t * g);


LIBSBML_EXTERN
CSGeometry_t *
Geometry_createCsGeometry(Geometry_t * g);


LIBSBML_EXTERN
ParametricGeometry_t *
Geometry_createParametricGeometry(Geometry_t * g);


LIBSBML_EXTERN
MixedGeometry_t *
Geometry_createMixedGeometry(Geometry_t * g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfGeometryDefinitions(Geometry_t * g) ;


LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_getGeometryDefinition(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_getGeometryDefinitionById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
unsigned int
Geometry_getNumGeometryDefinitions(Geometry_t * g);


LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_removeGeometryDefinition(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_removeGeometryDefinitionById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
int
Geometry_addSampledField(Geometry_t * g, SampledField_t * sf);


LIBSBML_EXTERN
SampledField_t *
Geometry_createSampledField(Geometry_t * g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfSampledFields(Geometry_t * g) ;


LIBSBML_EXTERN
SampledField_t *
Geometry_getSampledField(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
SampledField_t *
Geometry_getSampledFieldById(Geometry_t * g, const char * sid);


LIBSBML_EXTERN
unsigned int
Geometry_getNumSampledFields(Geometry_t * g);


LIBSBML_EXTERN
SampledField_t *
Geometry_removeSampledField(Geometry_t * g, unsigned int n);


LIBSBML_EXTERN
SampledField_t *
Geometry_removeSampledFieldById(Geometry_t * g, const char * sid);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given Geometry_t structure have been set.
 *
 * @param g the Geometry_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_hasRequiredAttributes(const Geometry_t * g);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given Geometry_t structure have been set.
 *
 * @param g the Geometry_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_hasRequiredElements(const Geometry_t * g);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  Geometry_H__  */

