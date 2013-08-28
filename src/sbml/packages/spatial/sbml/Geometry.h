/*
 * @file    Geometry.h
 * @brief   Definition of Geometry, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: Geometry.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/Geometry.h $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
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

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Geometry : public SBase
{
protected:

  std::string mCoordinateSystem;

  ListOfCoordinateComponents	mCoordinateComponents;
  ListOfDomainTypes				mDomainTypes;
  ListOfDomains					mDomains;
  ListOfAdjacentDomains			mAdjacentDomains;
  ListOfGeometryDefinitions		mGeometryDefinitions;

public:

  /**
   * Creates a new Geometry with the given level, version, and package version.
   */
   Geometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new Geometry with the given SpatialPkgNamespaces object.
   */
   Geometry(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   Geometry(const Geometry& source);

  /**
   * Assignment operator.
   */
   Geometry& operator=(const Geometry& source);

  /**
   * Destructor.
   */ 
  virtual ~Geometry ();

  /**
   * Returns the string of the "coordinateSystem" attribute of this Geometry.
   *
   * @return the string of the "coordinateSystem" attribute of this Geometry.
   */
  virtual const std::string& getCoordinateSystem () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * Geometry's "coordinateSystem" attribute has been set.
   *
   * @return @c true if this Geometry's "coordinateSystem" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCoordinateSystem () const;

  /**
   * Sets the SIdRef string of the "coordinateSystem" attribute of this Geometry.
   *
   * @param CoordinateSystem a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinateSystem (const std::string& coordinateSystem);

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
  virtual int unsetCoordinateSystem ();

/**
   * Adds a copy of the given CoordinateComponent object to this Geometry.
   *
   * @param cc the CoordinateComponent to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_LEVEL_MISMATCH
   * @li LIBSBML_VERSION_MISMATCH
   * @li LIBSBML_DUPLICATE_OBJECT_ID
   * @li LIBSBML_OPERATION_FAILED
   * 
   * @note This method should be used with some caution.  The fact that
   * this method @em copies the object passed to it means that the caller
   * will be left holding a physically different object instance than the
   * one contained in this Geometry.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the Geometry</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see Geometry::createCoordinateComponent()
   * for a method that does not lead to these issues.
   *
   * @see createCoordinateComponent()
   */
  int addCoordinateComponent (const CoordinateComponent* cc);

  /**
   * Adds a copy of the given DomainType object to this Geometry.
   *
   * @param dt the DomainType to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_LEVEL_MISMATCH
   * @li LIBSBML_VERSION_MISMATCH
   * @li LIBSBML_DUPLICATE_OBJECT_ID
   * @li LIBSBML_OPERATION_FAILED
   * 
   * @note This method should be used with some caution.  The fact that
   * this method @em copies the object passed to it means that the caller
   * will be left holding a physically different object instance than the
   * one contained in this Geometry.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the Geometry</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see Geometry::createDomainType()
   * for a method that does not lead to these issues.
   *
   * @see createDomainType()
   */
  int addDomainType (const DomainType* dt);

/**
   * Adds a copy of the given Domain object to this Geometry.
   *
   * @param d the Domain to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_LEVEL_MISMATCH
   * @li LIBSBML_VERSION_MISMATCH
   * @li LIBSBML_DUPLICATE_OBJECT_ID
   * @li LIBSBML_OPERATION_FAILED
   * 
   * @note This method should be used with some caution.  The fact that
   * this method @em copies the object passed to it means that the caller
   * will be left holding a physically different object instance than the
   * one contained in this Geometry.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the Geometry</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see Geometry::createDomain()
   * for a method that does not lead to these issues.
   *
   * @see createDomain()
   */
  int addDomain (const Domain* d);

  /**
   * Adds a copy of the given AdjacentDomains object to this Geometry.
   *
   * @param d the AdjacentDomains to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_LEVEL_MISMATCH
   * @li LIBSBML_VERSION_MISMATCH
   * @li LIBSBML_DUPLICATE_OBJECT_ID
   * @li LIBSBML_OPERATION_FAILED
   * 
   * @note This method should be used with some caution.  The fact that
   * this method @em copies the object passed to it means that the caller
   * will be left holding a physically different object instance than the
   * one contained in this Geometry.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the Geometry</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see Geometry::createAdjacentDomains()
   * for a method that does not lead to these issues.
   *
   * @see createAdjacentDomains()
   */
  int addAdjacentDomains (const AdjacentDomains* ad);

  /**
   * Adds a copy of the given GeometryDefintion object to this Geometry.
   *
   * @param d the GeometryDefintion to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_LEVEL_MISMATCH
   * @li LIBSBML_VERSION_MISMATCH
   * @li LIBSBML_DUPLICATE_OBJECT_ID
   * @li LIBSBML_OPERATION_FAILED
   * 
   * @note This method should be used with some caution.  The fact that
   * this method @em copies the object passed to it means that the caller
   * will be left holding a physically different object instance than the
   * one contained in this Geometry.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the Geometry</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see Geometry::createGeometryDefintion()
   * for a method that does not lead to these issues.
   *
   * @see createGeometryDefintion()
   */
  int addGeometryDefinition (const GeometryDefinition* gd);

  /**
   * Creates a new CoordinateComponent inside this Geometry and returns it.
   *
   * @return the CoordinateComponent object created
   *
   * @see addCoordinateComponent(const CoordinateComponent* cc)
   */
  CoordinateComponent* createCoordinateComponent ();

  /**
   * Creates a new DomainType inside this Geometry and returns it.
   *
   * @return the DomainType object created
   *
   * @see addDomainType(const DomainType* dt)
   */
  DomainType* createDomainType ();

 /**
   * Creates a new Domain inside this Geometry and returns it.
   *
   * @return the Domain object created
   *
   * @see addDomain(const Domain* d)
   */
  Domain* createDomain ();

  /**
   * Creates a new AdjacentDomains object inside this Geometry and returns it.
   *
   * @return the AdjacentDomains object created
   *
   * @see addAdjacentDomains(const AdjacentDomains* d)
   */
  AdjacentDomains* createAdjacentDomains ();

  /**
   * Creates a new AnalyticGeometry object inside this Geometry and returns it.
   *
   * @return the AnalyticGeometry object created
   *
   * @see addGeometryDefinition(const GeometryDefinition* ag)
   */
  AnalyticGeometry* createAnalyticGeometry ();

 /**
   * Creates a new SampledFieldGeometry object inside this Geometry and returns it.
   *
   * @return the SampledFieldGeometry object created
   *
   * @see addGeometryDefinition(const GeometryDefinition* sfg)
   */
  SampledFieldGeometry* createSampledFieldGeometry ();

 /**
   * Creates a new ParametricGeometry object inside this Geometry and returns it.
   *
   * @return the ParametricGeometry object created
   *
   * @see addGeometryDefinition(const GeometryDefinition* pg)
   */
  ParametricGeometry* createParametricGeometry ();

 /**
   * Creates a new CSGeometry object inside this Geometry and returns it.
   *
   * @return the CSGeometry object created
   *
   * @see addGeometryDefinition(const GeometryDefinition* csg)
   */
   
  CSGeometry* createCSGeometry ();


  /**
   * Get the ListOfCoordinateComponents object in this Geometry.
   * 
   * @return the list of CoordinateComponents for this Geometry.
   */
  const ListOfCoordinateComponents* getListOfCoordinateComponents () const;


  /**
   * Get the ListOfCoordinateComponents object in this Geometry.
   * 
   * @return the list of CoordinateComponents for this Geometry.
   */
  ListOfCoordinateComponents* getListOfCoordinateComponents ();

 
 /**
   * Get the ListOfDomainTypes object in this Geometry.
   * 
   * @return the list of DomainTypes for this Geometry.
   */
  const ListOfDomainTypes* getListOfDomainTypes () const;


  /**
   * Get the ListOfDomainTypes object in this Geometry.
   * 
   * @return the list of DomainTypes for this Geometry.
   */
  ListOfDomainTypes* getListOfDomainTypes ();

  /**
   * Get the ListOfDomains object in this Geometry.
   * 
   * @return the list of Domains for this Geometry.
   */
  const ListOfDomains* getListOfDomains () const;


  /**
   * Get the ListOfDomains object in this Geometry.
   * 
   * @return the list of Domains for this Geometry.
   */
  ListOfDomains* getListOfDomains ();

  /**
   * Get the ListOfAdjacentDomains object in this Geometry.
   * 
   * @return the list of AdjacentDomains for this Geometry.
   */
  const ListOfAdjacentDomains* getListOfAdjacentDomains () const;


  /**
   * Get the ListOfAdjacentDomains object in this Geometry.
   * 
   * @return the list of AdjacentDomains for this Geometry.
   */
  ListOfAdjacentDomains* getListOfAdjacentDomains ();

  /**
   * Get the ListOfGeometryDefintions object in this Geometry.
   * 
   * @return the list of GeometryDefintion for this Geometry.
   */
  const ListOfGeometryDefinitions* getListOfGeometryDefinitions () const;


  /**
   * Get the ListOfGeometryDefinitions object in this Geometry.
   * 
   * @return the list of GeometryDefinitions for this Geometry.
   */
  ListOfGeometryDefinitions* getListOfGeometryDefinitions ();

 /**
   * Get the nth CoordinateComponent object in this Geometry.
   * 
   * @return the nth CoordinateComponent of this Geometry.
   */
  const CoordinateComponent* getCoordinateComponent (unsigned int n) const;


  /**
   * Get the nth CoordinateComponents object in this Geometry.
   * 
   * @return the nth CoordinateComponent of this Geometry.
   */
  CoordinateComponent* getCoordinateComponent (unsigned int n);

 /**
   * Get a CoordinateComponent object based on its identifier.
   * 
   * @return the CoordinateComponent in this Geometry with the identifier
   * @p sid or NULL if no such CoordinateComponent exists.
   */
  const CoordinateComponent* getCoordinateComponent (const std::string& sid) const;


  /**
   * Get a CoordinateComponent object based on its identifier.
   * 
   * @return the CoordinateComponent in this Geometry with the identifier
   * @p sid or NULL if no such CoordinateComponent exists.
   */
  CoordinateComponent* getCoordinateComponent (const std::string& sid);

   /**
   * Get the nth DomainType object in this Geometry.
   * 
   * @return the nth DomainType of this Geometry.
   */
  const DomainType* getDomainType (unsigned int n) const;

  /**
   * Get the nth DomainType object in this Geometry.
   * 
   * @return the nth DomainType of this Geometry.
   */
  DomainType* getDomainType (unsigned int n);


  /**
   * Get a DomainType object based on its identifier.
   * 
   * @return the DomainType in this Geometry with the identifier
   * @p sid or NULL if no such DomainType exists.
   */
  const DomainType* getDomainType (const std::string& sid) const;


  /**
   * Get a DomainType object based on its identifier.
   * 
   * @return the DomainType in this Geometry with the identifier
   * @p sid or NULL if no such DomainType exists.
   */
  DomainType* getDomainType (const std::string& sid);

  /**
   * Get the nth Domain object in this Geometry.
   * 
   * @return the nth Domain of this Geometry.
   */
  const Domain* getDomain (unsigned int n) const;


  /**
   * Get the nth Domain object in this Geometry.
   * 
   * @return the nth Domain of this Geometry.
   */
  Domain* getDomain (unsigned int n);

  /**
   * Get a Domain object based on its identifier.
   * 
   * @return the Domain in this Geometry with the identifier
   * @p sid or NULL if no such Domain exists.
   */
  const Domain* getDomain (const std::string& sid) const;

  /**
   * Get a Domain object based on its identifier.
   * 
   * @return the Domain in this Geometry with the identifier
   * @p sid or NULL if no such Domain exists.
   */
  Domain* getDomain (const std::string& sid);

  /**
   * Get the nth AdjacentDomains object in this Geometry.
   * 
   * @return the nth AdjacentDomains of this Geometry.
   */
  const AdjacentDomains* getAdjacentDomains (unsigned int n) const;


  /**
   * Get the nth AdjacentDomains object in this Geometry.
   * 
   * @return the nth AdjacentDomains of this Geometry.
   */
  AdjacentDomains* getAdjacentDomains (unsigned int n);


  /**
   * Get a AdjacentDomains object based on its identifier.
   * 
   * @return the AdjacentDomains in this Geometry with the identifier
   * @p sid or NULL if no such AdjacentDomains exists.
   */
  const AdjacentDomains* getAdjacentDomains (const std::string& sid) const;


  /**
   * Get a AdjacentDomains object based on its identifier.
   * 
   * @return the AdjacentDomains in this Geometry with the identifier
   * @p sid or NULL if no such AdjacentDomains exists.
   */
  AdjacentDomains* getAdjacentDomains (const std::string& sid);

  /**
   * Get the nth GeometryDefinition object in this Geometry.
   * 
   * @return the nth GeometryDefinition of this Geometry.
   */
  const GeometryDefinition* getGeometryDefinition (unsigned int n) const;


  /**
   * Get the nth GeometryDefinition object in this Geometry.
   * 
   * @return the nth GeometryDefinition of this Geometry.
   */
  GeometryDefinition* getGeometryDefinition (unsigned int n);


  /**
   * Get a GeometryDefinition object based on its identifier.
   * 
   * @return the GeometryDefinition in this Geometry with the identifier
   * @p sid or NULL if no such GeometryDefinition exists.
   */
  const GeometryDefinition* getGeometryDefinition (const std::string& sid) const;


  /**
   * Get a GeometryDefinition object based on its identifier.
   * 
   * @return the GeometryDefinition in this Geometry with the identifier
   * @p sid or NULL if no such GeometryDefinition exists.
   */
  GeometryDefinition* getGeometryDefinition (const std::string& sid);



  /**
   * Get the number of CoordinateComponent objects in this Geometry.
   * 
   * @return the number of CoordinateComponents in this Geometry.
   */
  unsigned int getNumCoordinateComponents () const;

  /**
   * Get the number of DomainType objects in this Geometry.
   * 
   * @return the number of DomainTypes in this Geometry.
   */
  unsigned int getNumDomainTypes () const;

  /**
   * Get the number of Domain objects in this Geometry.
   * 
   * @return the number of Domains in this Geometry.
   */
  unsigned int getNumDomains () const;

  /**
   * Get the number of AdjacentDomains objects in this Geometry.
   * 
   * @return the number of AdjacentDomains in this Geometry.
   */
  unsigned int getNumAdjacentDomains () const;

  /**
   * Get the number of GeometryDefinitions objects in this Geometry.
   * 
   * @return the number of GeometryDefinitions in this Geometry.
   */
  unsigned int getNumGeometryDefinitions () const;


  /**
   * Removes the nth CoordinateComponent object from this Geometry object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the CoordinateComponent object to remove
   *
   * @return the CoordinateComponent object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  CoordinateComponent* removeCoordinateComponent (unsigned int n);


  /**
   * Removes the CoordinateComponent object with the given identifier from this Geometry 
   * object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   * If none of the CoordinateComponent objects in this Geometry object have the identifier 
   * @p sid, then @c NULL is returned.
   *
   * @param sid the identifier of the CoordinateComponent object to remove
   *
   * @return the CoordinateComponent object removed.  As mentioned above, the 
   * caller owns the returned object. NULL is returned if no CoordinateComponent
   * object with the identifier exists in this Geometry object.
   */
  CoordinateComponent* removeCoordinateComponent (const std::string& sid);


  /**
   * Removes the nth DomainType object from this Geometry object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the DomainType object to remove
   *
   * @return the DomainType object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  DomainType* removeDomainType (unsigned int n);


  /**
   * Removes the DomainType object with the given identifier from this Geometry 
   * object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   * If none of the DomainType objects in this Geometry object have the identifier 
   * @p sid, then @c NULL is returned.
   *
   * @param sid the identifier of the DomainType object to remove
   *
   * @return the DomainType object removed.  As mentioned above, the 
   * caller owns the returned object. NULL is returned if no DomainType
   * object with the identifier exists in this Geometry object.
   */
  DomainType* removeDomainType (const std::string& sid);

  /**
   * Removes the nth Domain object from this Geometry object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the Domain object to remove
   *
   * @return the Domain object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  Domain* removeDomain (unsigned int n);

  /**
   * Removes the Domain object with the given identifier from this Geometry 
   * object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   * If none of the Domain objects in this Geometry object have the identifier 
   * @p sid, then @c NULL is returned.
   *
   * @param sid the identifier of the Domain object to remove
   *
   * @return the Domain object removed.  As mentioned above, the 
   * caller owns the returned object. NULL is returned if no Domain
   * object with the identifier exists in this Geometry object.
   */
  Domain* removeDomain (const std::string& sid);

 /**
   * Removes the nth AdjacentDomains object from this Geometry object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the AdjacentDomains object to remove
   *
   * @return the AdjacentDomains object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  AdjacentDomains* removeAdjacentDomains (unsigned int n);


  /**
   * Removes the AdjacentDomains object with the given identifier from this Geometry 
   * object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   * If none of the AdjacentDomains objects in this Geometry object have the identifier 
   * @p sid, then @c NULL is returned.
   *
   * @param sid the identifier of the AdjacentDomains object to remove
   *
   * @return the AdjacentDomains object removed.  As mentioned above, the 
   * caller owns the returned object. NULL is returned if no AdjacentDomains
   * object with the identifier exists in this Geometry object.
   */
  AdjacentDomains* removeAdjacentDomains (const std::string& sid);

 /**
   * Removes the nth GeometryDefinition object from this Geometry object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the GeometryDefinition object to remove
   *
   * @return the GeometryDefinition object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  GeometryDefinition* removeGeometryDefinition (unsigned int n);


  /**
   * Removes the GeometryDefinition object with the given identifier from this Geometry 
   * object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   * If none of the GeometryDefinition objects in this Geometry object have the identifier 
   * @p sid, then @c NULL is returned.
   *
   * @param sid the identifier of the GeometryDefinition object to remove
   *
   * @return the GeometryDefinition object removed.  As mentioned above, the 
   * caller owns the returned object. NULL is returned if no GeometryDefinition
   * object with the identifier exists in this Geometry object.
   */
  GeometryDefinition* removeGeometryDefinition (const std::string& sid);

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;

  /**
   * @return a (deep) copy of this Geometry.
   */
  virtual Geometry* clone () const;

  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;


  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactans.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;
 

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygenLibsbmlInternal */
    

   /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument object to use
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /**
   * Sets this SBML object to child SBML objects (if any).
   * (Creates a child-parent relationship by the parent)
   *
   * Subclasses must override this function if they define
   * one ore more child elements.
   * Basically, this function needs to be called in
   * constructor, copy constructor, assignment operator.
   *
   * @see setSBMLDocument
   * @see enablePackageInternal
   */
  virtual void connectToChild ();


  /**
   * Enables/Disables the given package with this element and child
   * elements (if any).
   * (This is an internal implementation for enablePakcage function)
   *
   * @note Subclasses in which one or more child elements are defined
   * must override this function.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);
  /** @endcond doxygenLibsbmlInternal */



protected:
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase*
  createObject (XMLInputStream& stream);


  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes, 
                               const ExpectedAttributes& expectedAttributes);


  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.  For example:
   *
   *   SBase::writeAttributes(stream);
   *   stream.writeAttribute( "id"  , mId   );
   *   stream.writeAttribute( "name", mName );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;

  /* the validator classes need to be friends to access the 
   * protected constructor that takes no arguments
   */
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
/*  
  friend class L1CompatibilityValidator;
  friend class L2v1CompatibilityValidator;
  friend class L2v2CompatibilityValidator;
  friend class L2v3CompatibilityValidator;
  friend class L2v4CompatibilityValidator;
  friend class MathMLConsistencyValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;
*/
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;

  /** @endcond doxygenLibsbmlInternal */


};

#ifndef SWIG
/*template<>
struct IdEq<Geometry> : public std::unary_function<SBase*, bool>
{
  const std::string& coordSystem;

  IdEq (const std::string& coordSystem) : id(coordSystem) { }
  bool operator() (SBase* sb) 
       { return static_cast <Geometry*> (sb)->getCoordinateSystem() == coordSystem; }
};
*/
#endif
/** @endcond doxygenLibsbmlInternal */

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

//
// C API will be added here.
//


LIBSBML_EXTERN
const char *
Geometry_getCoordinateSystem (const Geometry_t *g);


LIBSBML_EXTERN
int
Geometry_isSetCoordinateSystem (const Geometry_t *g);


LIBSBML_EXTERN
int
Geometry_setCoordinateSystem (Geometry_t *g, const char *coordSys);


LIBSBML_EXTERN
int
Geometry_unsetCoordinateSystem (Geometry_t *g);


LIBSBML_EXTERN
int
Geometry_addCoordinateComponent (Geometry_t *g, const CoordinateComponent_t *cc);


LIBSBML_EXTERN
int
Geometry_addDomainType (Geometry_t *g, const DomainType_t *dt);


LIBSBML_EXTERN
int
Geometry_addDomain (Geometry_t *g, const Domain_t *d);


LIBSBML_EXTERN
int
Geometry_addAdjacentDomains (Geometry_t *g, const AdjacentDomains_t *ad);


LIBSBML_EXTERN
int
Geometry_addGeometryDefinition (Geometry_t *g, const GeometryDefinition_t *gd);


LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_createCoordinateComponent (Geometry_t *g);


LIBSBML_EXTERN
DomainType_t *
Geometry_createDomainType (Geometry_t *g);


LIBSBML_EXTERN
Domain_t *
Geometry_createDomain (Geometry_t *g);


LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_createAdjacentDomains (Geometry_t *g);


LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_createGeometryDefinition (Geometry_t *g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfCoordinateComponents (Geometry_t *g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfDomainTypes (Geometry_t *g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfDomains (Geometry_t *g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfAdjacentDomains (Geometry_t *g);


LIBSBML_EXTERN
ListOf_t *
Geometry_getListOfGeometryDefinitions (Geometry_t *g);


LIBSBML_EXTERN
Geometry_t *
Geometry_clone (const Geometry_t *g);


LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_getCoordinateComponent (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
CoordinateComponent_t *
Geometry_getCoordinateComponentById (Geometry_t *g, const char *sid);


LIBSBML_EXTERN
DomainType_t *
Geometry_getDomainType (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
DomainType_t *
Geometry_getDomainTypeById (Geometry_t *g, const char *sid);


LIBSBML_EXTERN
Domain_t *
Geometry_getDomain (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
Domain_t *
Geometry_getDomainById (Geometry_t *g, const char *sid);


LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_getAdjacentDomains (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
AdjacentDomains_t *
Geometry_getAdjacentDomainsById (Geometry_t *g, const char *sid);


LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_getGeometryDefinition (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
GeometryDefinition_t *
Geometry_getGeometryDefinitionById (Geometry_t *g, const char *sid);


LIBSBML_EXTERN
unsigned int
Geometry_getNumCoordinateComponents (const Geometry_t *g);


LIBSBML_EXTERN
unsigned int
Geometry_getNumDomainTypes (const Geometry_t *g);


LIBSBML_EXTERN
unsigned int
Geometry_getNumDomains (const Geometry_t *g);


LIBSBML_EXTERN
unsigned int
Geometry_getNumAdjacentDomains (const Geometry_t *g);


LIBSBML_EXTERN
unsigned int
Geometry_getNumGeometryDefinitions (const Geometry_t *g);


LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_removeCoordinateComponent (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_removeCoordinateComponentById (Geometry_t *g, const char* sid);


LIBSBML_EXTERN
DomainType_t*
Geometry_removeDomainType (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
DomainType_t*
Geometry_removeDomainTypeById (Geometry_t *g, const char* sid);


LIBSBML_EXTERN
Domain_t*
Geometry_removeDomain (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
Domain_t*
Geometry_removeDomainById (Geometry_t *g, const char* sid);


LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_removeAdjacentDomains (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_removeAdjacentDomainsById (Geometry_t *g, const char* sid);


LIBSBML_EXTERN
GeometryDefinition_t*
Geometry_removeGeometryDefinition (Geometry_t *g, unsigned int n);


LIBSBML_EXTERN
GeometryDefinition_t*
Geometry_removeGeometryDefinitionById (Geometry_t *g, const char* sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* Geometry_H__ */
