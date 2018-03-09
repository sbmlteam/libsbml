/**
 * @file Geometry.h
 * @brief Definition of the Geometry class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 *
 * @class Geometry
 * @sbmlbrief{spatial} TODO:Definition of the Geometry class.
 */


#ifndef Geometry_H__
#define Geometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/ListOfCoordinateComponents.h>
#include <sbml/packages/spatial/sbml/ListOfDomainTypes.h>
#include <sbml/packages/spatial/sbml/ListOfDomains.h>
#include <sbml/packages/spatial/sbml/ListOfAdjacentDomains.h>
#include <sbml/packages/spatial/sbml/ListOfGeometryDefinitions.h>
#include <sbml/packages/spatial/sbml/ListOfSampledFields.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Geometry : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mId;
  GeometryKind_t mCoordinateSystem;
  ListOfCoordinateComponents mCoordinateComponents;
  ListOfDomainTypes mDomainTypes;
  ListOfDomains mDomains;
  ListOfAdjacentDomains mAdjacentDomains;
  ListOfGeometryDefinitions mGeometryDefinitions;
  ListOfSampledFields mSampledFields;

  /** @endcond */

public:

  /**
   * Creates a new Geometry using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Geometry.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Geometry.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this Geometry.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  Geometry(unsigned int level = SpatialExtension::getDefaultLevel(),
           unsigned int version = SpatialExtension::getDefaultVersion(),
           unsigned int pkgVersion =
             SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new Geometry using the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  Geometry(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for Geometry.
   *
   * @param orig the Geometry instance to copy.
   */
  Geometry(const Geometry& orig);


  /**
   * Assignment operator for Geometry.
   *
   * @param rhs the Geometry object whose values are to be used as the basis of
   * the assignment.
   */
  Geometry& operator=(const Geometry& rhs);


  /**
   * Creates and returns a deep copy of this Geometry object.
   *
   * @return a (deep) copy of this Geometry object.
   */
  virtual Geometry* clone() const;


  /**
   * Destructor for Geometry.
   */
  virtual ~Geometry();


  /**
   * Returns the value of the "id" attribute of this Geometry.
   *
   * @return the value of the "id" attribute of this Geometry as a string.
   */
  const std::string& getId() const;


  /**
   * Returns the value of the "coordinateSystem" attribute of this Geometry.
   *
   * @return the value of the "coordinateSystem" attribute of this Geometry as
   * a GeometryKind_t.
   */
  GeometryKind_t getCoordinateSystem() const;


  /**
   * Returns the value of the "coordinateSystem" attribute of this Geometry.
   *
   * @return the value of the "coordinateSystem" attribute of this Geometry as
   * a string.
   */
  std::string getCoordinateSystemAsString() const;
  //bgoli22


  /**
   * Predicate returning @c true if this Geometry's "id" attribute is set.
   *
   * @return @c true if this Geometry's "id" attribute has been set, otherwise
   * @c false is returned.
   */
  bool isSetId() const;


  /**
   * Predicate returning @c true if this Geometry's "coordinateSystem"
   * attribute is set.
   *
   * @return @c true if this Geometry's "coordinateSystem" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetCoordinateSystem() const;


  /**
   * Sets the value of the "id" attribute of this Geometry.
   *
   * @param id std::string& value of the "id" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setId(const std::string& id);


  /**
   * Sets the value of the "coordinateSystem" attribute of this Geometry.
   *
   * @param coordinateSystem GeometryKind_t value of the "coordinateSystem"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoordinateSystem(const GeometryKind_t coordinateSystem);


  /**
   * Sets the value of the "coordinateSystem" attribute of this Geometry.
   *
   * @param coordinateSystem std::string& of the "coordinateSystem" attribute
   * to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoordinateSystem(const std::string& coordinateSystem);


  /**
   * Unsets the value of the "id" attribute of this Geometry.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetId();


  /**
   * Unsets the value of the "coordinateSystem" attribute of this Geometry.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCoordinateSystem();


  /**
   * Returns the ListOfCoordinateComponents from this Geometry.
   *
   * @return the ListOfCoordinateComponents from this Geometry.
   */
  const ListOfCoordinateComponents* getListOfCoordinateComponents() const;


  /**
   * Returns the ListOfCoordinateComponents from this Geometry.
   *
   * @return the ListOfCoordinateComponents from this Geometry.
   */
  ListOfCoordinateComponents* getListOfCoordinateComponents();


  /**
   * Get a CoordinateComponent from the Geometry.
   *
   * @param n an unsigned int representing the index of the CoordinateComponent
   * to retrieve.
   *
   * @return the nth CoordinateComponent in the ListOfCoordinateComponents
   * within this Geometry.
   *
   * @see getNumCoordinateComponents()
   */
  CoordinateComponent* getCoordinateComponent(unsigned int n);


  /**
   * Get a CoordinateComponent from the Geometry.
   *
   * @param n an unsigned int representing the index of the CoordinateComponent
   * to retrieve.
   *
   * @return the nth CoordinateComponent in the ListOfCoordinateComponents
   * within this Geometry.
   *
   * @see getNumCoordinateComponents()
   */
  const CoordinateComponent* getCoordinateComponent(unsigned int n) const;


  /**
   * Get a CoordinateComponent from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the CoordinateComponent
   * to retrieve.
   *
   * @return the CoordinateComponent in the ListOfCoordinateComponents within
   * this Geometry with the given id or NULL if no such CoordinateComponent
   * exists.
   *
   * @see getCoordinateComponent(unsigned int n)
   * @see getNumCoordinateComponents()
   */
  CoordinateComponent* getCoordinateComponent(const std::string& sid);


  /**
   * Get a CoordinateComponent from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the CoordinateComponent
   * to retrieve.
   *
   * @return the CoordinateComponent in the ListOfCoordinateComponents within
   * this Geometry with the given id or NULL if no such CoordinateComponent
   * exists.
   *
   * @see getCoordinateComponent(unsigned int n)
   * @see getNumCoordinateComponents()
   */
  const CoordinateComponent* getCoordinateComponent(const std::string& sid)
    const;


  /**
   * Adds a copy of the given CoordinateComponent to this Geometry.
   *
   * @param cc the CoordinateComponent object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createCoordinateComponent()
   */
  int addCoordinateComponent(const CoordinateComponent* cc);


  /**
   * Get the number of CoordinateComponent objects in this Geometry.
   *
   * @return the number of CoordinateComponent objects in this Geometry.
   */
  unsigned int getNumCoordinateComponents() const;


  /**
   * Creates a new CoordinateComponent object, adds it to this Geometry object
   * and returns the CoordinateComponent object created.
   *
   * @return a new CoordinateComponent object instance.
   *
   * @see addCoordinateComponent(const CoordinateComponent* cc)
   */
  CoordinateComponent* createCoordinateComponent();


  /**
   * Removes the nth CoordinateComponent from this Geometry and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the CoordinateComponent
   * to remove.
   *
   * @return a pointer to the nth CoordinateComponent in this Geometry.
   *
   * @see getNumCoordinateComponents
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  CoordinateComponent* removeCoordinateComponent(unsigned int n);


  /**
   * Removes the CoordinateComponent from this Geometry based on its identifier
   * and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the CoordinateComponent
   * to remove.
   *
   * @return the CoordinateComponent in this Geometry based on the identifier
   * or NULL if no such CoordinateComponent exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  CoordinateComponent* removeCoordinateComponent(const std::string& sid);


  /**
   * Returns the ListOfDomainTypes from this Geometry.
   *
   * @return the ListOfDomainTypes from this Geometry.
   */
  const ListOfDomainTypes* getListOfDomainTypes() const;


  /**
   * Returns the ListOfDomainTypes from this Geometry.
   *
   * @return the ListOfDomainTypes from this Geometry.
   */
  ListOfDomainTypes* getListOfDomainTypes();


  /**
   * Get a DomainType from the Geometry.
   *
   * @param n an unsigned int representing the index of the DomainType to
   * retrieve.
   *
   * @return the nth DomainType in the ListOfDomainTypes within this Geometry.
   *
   * @see getNumDomainTypes()
   */
  DomainType* getDomainType(unsigned int n);


  /**
   * Get a DomainType from the Geometry.
   *
   * @param n an unsigned int representing the index of the DomainType to
   * retrieve.
   *
   * @return the nth DomainType in the ListOfDomainTypes within this Geometry.
   *
   * @see getNumDomainTypes()
   */
  const DomainType* getDomainType(unsigned int n) const;


  /**
   * Get a DomainType from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the DomainType to
   * retrieve.
   *
   * @return the DomainType in the ListOfDomainTypes within this Geometry with
   * the given id or NULL if no such DomainType exists.
   *
   * @see getDomainType(unsigned int n)
   * @see getNumDomainTypes()
   */
  DomainType* getDomainType(const std::string& sid);


  /**
   * Get a DomainType from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the DomainType to
   * retrieve.
   *
   * @return the DomainType in the ListOfDomainTypes within this Geometry with
   * the given id or NULL if no such DomainType exists.
   *
   * @see getDomainType(unsigned int n)
   * @see getNumDomainTypes()
   */
  const DomainType* getDomainType(const std::string& sid) const;


  /**
   * Adds a copy of the given DomainType to this Geometry.
   *
   * @param dt the DomainType object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createDomainType()
   */
  int addDomainType(const DomainType* dt);


  /**
   * Get the number of DomainType objects in this Geometry.
   *
   * @return the number of DomainType objects in this Geometry.
   */
  unsigned int getNumDomainTypes() const;


  /**
   * Creates a new DomainType object, adds it to this Geometry object and
   * returns the DomainType object created.
   *
   * @return a new DomainType object instance.
   *
   * @see addDomainType(const DomainType* dt)
   */
  DomainType* createDomainType();


  /**
   * Removes the nth DomainType from this Geometry and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the DomainType to
   * remove.
   *
   * @return a pointer to the nth DomainType in this Geometry.
   *
   * @see getNumDomainTypes
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  DomainType* removeDomainType(unsigned int n);


  /**
   * Removes the DomainType from this Geometry based on its identifier and
   * returns a pointer to it.
   *
   * @param sid a string representing the identifier of the DomainType to
   * remove.
   *
   * @return the DomainType in this Geometry based on the identifier or NULL if
   * no such DomainType exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  DomainType* removeDomainType(const std::string& sid);


  /**
   * Returns the ListOfDomains from this Geometry.
   *
   * @return the ListOfDomains from this Geometry.
   */
  const ListOfDomains* getListOfDomains() const;


  /**
   * Returns the ListOfDomains from this Geometry.
   *
   * @return the ListOfDomains from this Geometry.
   */
  ListOfDomains* getListOfDomains();


  /**
   * Get a Domain from the Geometry.
   *
   * @param n an unsigned int representing the index of the Domain to retrieve.
   *
   * @return the nth Domain in the ListOfDomains within this Geometry.
   *
   * @see getNumDomains()
   */
  Domain* getDomain(unsigned int n);


  /**
   * Get a Domain from the Geometry.
   *
   * @param n an unsigned int representing the index of the Domain to retrieve.
   *
   * @return the nth Domain in the ListOfDomains within this Geometry.
   *
   * @see getNumDomains()
   */
  const Domain* getDomain(unsigned int n) const;


  /**
   * Get a Domain from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the Domain to retrieve.
   *
   * @return the Domain in the ListOfDomains within this Geometry with the
   * given id or NULL if no such Domain exists.
   *
   * @see getDomain(unsigned int n)
   * @see getNumDomains()
   */
  Domain* getDomain(const std::string& sid);


  /**
   * Get a Domain from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the Domain to retrieve.
   *
   * @return the Domain in the ListOfDomains within this Geometry with the
   * given id or NULL if no such Domain exists.
   *
   * @see getDomain(unsigned int n)
   * @see getNumDomains()
   */
  const Domain* getDomain(const std::string& sid) const;


  /**
   * Get a Domain from the Geometry based on the DomainType to which it refers.
   *
   * @param sid a string representing the domainType attribute of the Domain
   * object to retrieve.
   *
   * @return the first Domain in this Geometry based on the given domainType
   * attribute or NULL if no such Domain exists.
   */
  const Domain* getDomainByDomainType(const std::string& sid) const;


  /**
   * Get a Domain from the Geometry based on the DomainType to which it refers.
   *
   * @param sid a string representing the domainType attribute of the Domain
   * object to retrieve.
   *
   * @return the first Domain in this Geometry based on the given domainType
   * attribute or NULL if no such Domain exists.
   */
  Domain* getDomainByDomainType(const std::string& sid);


  /**
   * Adds a copy of the given Domain to this Geometry.
   *
   * @param d the Domain object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createDomain()
   */
  int addDomain(const Domain* d);


  /**
   * Get the number of Domain objects in this Geometry.
   *
   * @return the number of Domain objects in this Geometry.
   */
  unsigned int getNumDomains() const;


  /**
   * Creates a new Domain object, adds it to this Geometry object and returns
   * the Domain object created.
   *
   * @return a new Domain object instance.
   *
   * @see addDomain(const Domain* d)
   */
  Domain* createDomain();


  /**
   * Removes the nth Domain from this Geometry and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the Domain to remove.
   *
   * @return a pointer to the nth Domain in this Geometry.
   *
   * @see getNumDomains
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  Domain* removeDomain(unsigned int n);


  /**
   * Removes the Domain from this Geometry based on its identifier and returns
   * a pointer to it.
   *
   * @param sid a string representing the identifier of the Domain to remove.
   *
   * @return the Domain in this Geometry based on the identifier or NULL if no
   * such Domain exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  Domain* removeDomain(const std::string& sid);


  /**
   * Returns the ListOfAdjacentDomains from this Geometry.
   *
   * @return the ListOfAdjacentDomains from this Geometry.
   */
  const ListOfAdjacentDomains* getListOfAdjacentDomains() const;


  /**
   * Returns the ListOfAdjacentDomains from this Geometry.
   *
   * @return the ListOfAdjacentDomains from this Geometry.
   */
  ListOfAdjacentDomains* getListOfAdjacentDomains();


  /**
   * Get an AdjacentDomains from the Geometry.
   *
   * @param n an unsigned int representing the index of the AdjacentDomains to
   * retrieve.
   *
   * @return the nth AdjacentDomains in the ListOfAdjacentDomains within this
   * Geometry.
   *
   * @see getNumAdjacentDomains()
   */
  AdjacentDomains* getAdjacentDomains(unsigned int n);


  /**
   * Get an AdjacentDomains from the Geometry.
   *
   * @param n an unsigned int representing the index of the AdjacentDomains to
   * retrieve.
   *
   * @return the nth AdjacentDomains in the ListOfAdjacentDomains within this
   * Geometry.
   *
   * @see getNumAdjacentDomains()
   */
  const AdjacentDomains* getAdjacentDomains(unsigned int n) const;


  /**
   * Get an AdjacentDomains from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the AdjacentDomains to
   * retrieve.
   *
   * @return the AdjacentDomains in the ListOfAdjacentDomains within this
   * Geometry with the given id or NULL if no such AdjacentDomains exists.
   *
   * @see getAdjacentDomains(unsigned int n)
   * @see getNumAdjacentDomains()
   */
  AdjacentDomains* getAdjacentDomains(const std::string& sid);


  /**
   * Get an AdjacentDomains from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the AdjacentDomains to
   * retrieve.
   *
   * @return the AdjacentDomains in the ListOfAdjacentDomains within this
   * Geometry with the given id or NULL if no such AdjacentDomains exists.
   *
   * @see getAdjacentDomains(unsigned int n)
   * @see getNumAdjacentDomains()
   */
  const AdjacentDomains* getAdjacentDomains(const std::string& sid) const;


  /**
   * Get an AdjacentDomains from the Geometry based on the Domain1 to which it
   * refers.
   *
   * @param sid a string representing the domain1 attribute of the
   * AdjacentDomains object to retrieve.
   *
   * @return the first AdjacentDomains in this Geometry based on the given
   * domain1 attribute or NULL if no such AdjacentDomains exists.
   */
  const AdjacentDomains* getAdjacentDomainsByDomain1(const std::string& sid)
    const;


  /**
   * Get an AdjacentDomains from the Geometry based on the Domain1 to which it
   * refers.
   *
   * @param sid a string representing the domain1 attribute of the
   * AdjacentDomains object to retrieve.
   *
   * @return the first AdjacentDomains in this Geometry based on the given
   * domain1 attribute or NULL if no such AdjacentDomains exists.
   */
  AdjacentDomains* getAdjacentDomainsByDomain1(const std::string& sid);


  /**
   * Get an AdjacentDomains from the Geometry based on the Domain2 to which it
   * refers.
   *
   * @param sid a string representing the domain2 attribute of the
   * AdjacentDomains object to retrieve.
   *
   * @return the first AdjacentDomains in this Geometry based on the given
   * domain2 attribute or NULL if no such AdjacentDomains exists.
   */
  const AdjacentDomains* getAdjacentDomainsByDomain2(const std::string& sid)
    const;


  /**
   * Get an AdjacentDomains from the Geometry based on the Domain2 to which it
   * refers.
   *
   * @param sid a string representing the domain2 attribute of the
   * AdjacentDomains object to retrieve.
   *
   * @return the first AdjacentDomains in this Geometry based on the given
   * domain2 attribute or NULL if no such AdjacentDomains exists.
   */
  AdjacentDomains* getAdjacentDomainsByDomain2(const std::string& sid);


  /**
   * Adds a copy of the given AdjacentDomains to this Geometry.
   *
   * @param ad the AdjacentDomains object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createAdjacentDomains()
   */
  int addAdjacentDomains(const AdjacentDomains* ad);


  /**
   * Get the number of AdjacentDomains objects in this Geometry.
   *
   * @return the number of AdjacentDomains objects in this Geometry.
   */
  unsigned int getNumAdjacentDomains() const;


  /**
   * Creates a new AdjacentDomains object, adds it to this Geometry object and
   * returns the AdjacentDomains object created.
   *
   * @return a new AdjacentDomains object instance.
   *
   * @see addAdjacentDomains(const AdjacentDomains* ad)
   */
  AdjacentDomains* createAdjacentDomains();


  /**
   * Removes the nth AdjacentDomains from this Geometry and returns a pointer
   * to it.
   *
   * @param n an unsigned int representing the index of the AdjacentDomains to
   * remove.
   *
   * @return a pointer to the nth AdjacentDomains in this Geometry.
   *
   * @see getNumAdjacentDomains
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  AdjacentDomains* removeAdjacentDomains(unsigned int n);


  /**
   * Removes the AdjacentDomains from this Geometry based on its identifier and
   * returns a pointer to it.
   *
   * @param sid a string representing the identifier of the AdjacentDomains to
   * remove.
   *
   * @return the AdjacentDomains in this Geometry based on the identifier or
   * NULL if no such AdjacentDomains exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  AdjacentDomains* removeAdjacentDomains(const std::string& sid);


  /**
   * Returns the ListOfGeometryDefinitions from this Geometry.
   *
   * @return the ListOfGeometryDefinitions from this Geometry.
   */
  const ListOfGeometryDefinitions* getListOfGeometryDefinitions() const;


  /**
   * Returns the ListOfGeometryDefinitions from this Geometry.
   *
   * @return the ListOfGeometryDefinitions from this Geometry.
   */
  ListOfGeometryDefinitions* getListOfGeometryDefinitions();


  /**
   * Get a GeometryDefinition from the Geometry.
   *
   * @param n an unsigned int representing the index of the GeometryDefinition
   * to retrieve.
   *
   * @return the nth GeometryDefinition in the ListOfGeometryDefinitions within
   * this Geometry.
   *
   * @see getNumGeometryDefinitions()
   */
  GeometryDefinition* getGeometryDefinition(unsigned int n);


  /**
   * Get a GeometryDefinition from the Geometry.
   *
   * @param n an unsigned int representing the index of the GeometryDefinition
   * to retrieve.
   *
   * @return the nth GeometryDefinition in the ListOfGeometryDefinitions within
   * this Geometry.
   *
   * @see getNumGeometryDefinitions()
   */
  const GeometryDefinition* getGeometryDefinition(unsigned int n) const;


  /**
   * Get a GeometryDefinition from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the GeometryDefinition
   * to retrieve.
   *
   * @return the GeometryDefinition in the ListOfGeometryDefinitions within
   * this Geometry with the given id or NULL if no such GeometryDefinition
   * exists.
   *
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   */
  GeometryDefinition* getGeometryDefinition(const std::string& sid);


  /**
   * Get a GeometryDefinition from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the GeometryDefinition
   * to retrieve.
   *
   * @return the GeometryDefinition in the ListOfGeometryDefinitions within
   * this Geometry with the given id or NULL if no such GeometryDefinition
   * exists.
   *
   * @see getGeometryDefinition(unsigned int n)
   * @see getNumGeometryDefinitions()
   */
  const GeometryDefinition* getGeometryDefinition(const std::string& sid)
    const;


  /**
   * Adds a copy of the given GeometryDefinition to this Geometry.
   *
   * @param gd the GeometryDefinition object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createGeometryDefinition()
   */
  int addGeometryDefinition(const GeometryDefinition* gd);


  /**
   * Get the number of GeometryDefinition objects in this Geometry.
   *
   * @return the number of GeometryDefinition objects in this Geometry.
   */
  unsigned int getNumGeometryDefinitions() const;


  /**
   * Creates a new AnalyticGeometry object, adds it to this Geometry object and
   * returns the AnalyticGeometry object created.
   *
   * @return a new AnalyticGeometry object instance.
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  AnalyticGeometry* createAnalyticGeometry();


  /**
   * Creates a new SampledFieldGeometry object, adds it to this Geometry object
   * and returns the SampledFieldGeometry object created.
   *
   * @return a new SampledFieldGeometry object instance.
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  SampledFieldGeometry* createSampledFieldGeometry();


  /**
   * Creates a new CSGeometry object, adds it to this Geometry object and
   * returns the CSGeometry object created.
   *
   * @return a new CSGeometry object instance.
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  CSGeometry* createCSGeometry();


  /**
   * Creates a new ParametricGeometry object, adds it to this Geometry object
   * and returns the ParametricGeometry object created.
   *
   * @return a new ParametricGeometry object instance.
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  ParametricGeometry* createParametricGeometry();


  /**
   * Creates a new MixedGeometry object, adds it to this Geometry object and
   * returns the MixedGeometry object created.
   *
   * @return a new MixedGeometry object instance.
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  MixedGeometry* createMixedGeometry();


  /**
   * Removes the nth GeometryDefinition from this Geometry and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the GeometryDefinition
   * to remove.
   *
   * @return a pointer to the nth GeometryDefinition in this Geometry.
   *
   * @see getNumGeometryDefinitions
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  GeometryDefinition* removeGeometryDefinition(unsigned int n);


  /**
   * Removes the GeometryDefinition from this Geometry based on its identifier
   * and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the GeometryDefinition
   * to remove.
   *
   * @return the GeometryDefinition in this Geometry based on the identifier or
   * NULL if no such GeometryDefinition exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  GeometryDefinition* removeGeometryDefinition(const std::string& sid);


  /**
   * Returns the ListOfSampledFields from this Geometry.
   *
   * @return the ListOfSampledFields from this Geometry.
   */
  const ListOfSampledFields* getListOfSampledFields() const;


  /**
   * Returns the ListOfSampledFields from this Geometry.
   *
   * @return the ListOfSampledFields from this Geometry.
   */
  ListOfSampledFields* getListOfSampledFields();


  /**
   * Get a SampledField from the Geometry.
   *
   * @param n an unsigned int representing the index of the SampledField to
   * retrieve.
   *
   * @return the nth SampledField in the ListOfSampledFields within this
   * Geometry.
   *
   * @see getNumSampledFields()
   */
  SampledField* getSampledField(unsigned int n);


  /**
   * Get a SampledField from the Geometry.
   *
   * @param n an unsigned int representing the index of the SampledField to
   * retrieve.
   *
   * @return the nth SampledField in the ListOfSampledFields within this
   * Geometry.
   *
   * @see getNumSampledFields()
   */
  const SampledField* getSampledField(unsigned int n) const;


  /**
   * Get a SampledField from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the SampledField to
   * retrieve.
   *
   * @return the SampledField in the ListOfSampledFields within this Geometry
   * with the given id or NULL if no such SampledField exists.
   *
   * @see getSampledField(unsigned int n)
   * @see getNumSampledFields()
   */
  SampledField* getSampledField(const std::string& sid);


  /**
   * Get a SampledField from the Geometry based on its identifier.
   *
   * @param sid a string representing the identifier of the SampledField to
   * retrieve.
   *
   * @return the SampledField in the ListOfSampledFields within this Geometry
   * with the given id or NULL if no such SampledField exists.
   *
   * @see getSampledField(unsigned int n)
   * @see getNumSampledFields()
   */
  const SampledField* getSampledField(const std::string& sid) const;


  /**
   * Adds a copy of the given SampledField to this Geometry.
   *
   * @param sf the SampledField object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createSampledField()
   */
  int addSampledField(const SampledField* sf);


  /**
   * Get the number of SampledField objects in this Geometry.
   *
   * @return the number of SampledField objects in this Geometry.
   */
  unsigned int getNumSampledFields() const;


  /**
   * Creates a new SampledField object, adds it to this Geometry object and
   * returns the SampledField object created.
   *
   * @return a new SampledField object instance.
   *
   * @see addSampledField(const SampledField* sf)
   */
  SampledField* createSampledField();


  /**
   * Removes the nth SampledField from this Geometry and returns a pointer to
   * it.
   *
   * @param n an unsigned int representing the index of the SampledField to
   * remove.
   *
   * @return a pointer to the nth SampledField in this Geometry.
   *
   * @see getNumSampledFields
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  SampledField* removeSampledField(unsigned int n);


  /**
   * Removes the SampledField from this Geometry based on its identifier and
   * returns a pointer to it.
   *
   * @param sid a string representing the identifier of the SampledField to
   * remove.
   *
   * @return the SampledField in this Geometry based on the identifier or NULL
   * if no such SampledField exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  SampledField* removeSampledField(const std::string& sid);


  /**
   * Returns the XML element name of this Geometry object.
   *
   * For Geometry, the XML element name is always @c "geometry".
   *
   * @return the name of this element, i.e. @c "geometry".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Geometry object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   *
   * @sbmlconstant{SBML_SPATIAL_GEOMETRY, SBMLSpatialTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * Geometry object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * Geometry have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the Geometry object are:
   * @li "coordinateSystem"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this Geometry
   * object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * Geometry have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the Geometry object are:
   */
  virtual bool hasRequiredElements() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Write any contained elements
   */
  virtual void writeElements(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument
   */
  virtual void setSBMLDocument(SBMLDocument* d);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements
   */
  virtual void connectToChild();

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, bool& value)
    const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           double& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           unsigned int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           std::string& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this Geometry's attribute "attributeName"
   * is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this Geometry's attribute "attributeName" has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, bool value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, double value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           unsigned int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           const std::string& value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this Geometry.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this Geometry.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */




  #endif /* !SWIG */


  /**
   * Returns the first child element that has the given @p id in the model-wide
   * SId namespace, or @c NULL if no such object is found.
   *
   * @param id a string representing the id attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p id.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * filter, an ElementFilter that may impose restrictions on the objects to be
   * retrieved.
   *
   * @return a List* pointer of pointers to all SBase child objects with any
   * restriction imposed.
   */
  virtual List* getAllElements(ElementFilter * filter = NULL);


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new object from the next XMLToken on the XMLInputStream
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds the expected attributes for this element
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new Geometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Geometry_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Geometry_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this Geometry_t.
 *
 * @throws SBMLConstructorException
 * Thrown if the given @p level and @p version combination, or this kind of
 * SBML object, are either invalid or mismatched with respect to the parent
 * SBMLDocument object.
 * @copydetails doc_note_setting_lv
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
Geometry_t *
Geometry_create(unsigned int level,
                unsigned int version,
                unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Geometry_t object.
 *
 * @param g the Geometry_t structure.
 *
 * @return a (deep) copy of this Geometry_t object.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
Geometry_t*
Geometry_clone(const Geometry_t* g);


/**
 * Frees this Geometry_t object.
 *
 * @param g the Geometry_t structure.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
void
Geometry_free(Geometry_t* g);


/**
 * Returns the value of the "id" attribute of this Geometry_t.
 *
 * @param g the Geometry_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this Geometry_t as a pointer to a
 * string.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const char *
Geometry_getId(const Geometry_t * g);


/**
 * Returns the value of the "coordinateSystem" attribute of this Geometry_t.
 *
 * @param g the Geometry_t structure whose coordinateSystem is sought.
 *
 * @return the value of the "coordinateSystem" attribute of this Geometry_t as
 * a GeometryKind_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
GeometryKind_t
Geometry_getCoordinateSystem(const Geometry_t * g);


/**
 * Returns the value of the "coordinateSystem" attribute of this Geometry_t.
 *
 * @param g the Geometry_t structure whose coordinateSystem is sought.
 *
 * @return the value of the "coordinateSystem" attribute of this Geometry_t as
 * a const char *.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const char *
Geometry_getCoordinateSystemAsString(const Geometry_t * g);


/**
 * Predicate returning @c 1 if this Geometry_t's "id" attribute is set.
 *
 * @param g the Geometry_t structure.
 *
 * @return @c 1 if this Geometry_t's "id" attribute has been set, otherwise @c
 * 0 is returned.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_isSetId(const Geometry_t * g);


/**
 * Predicate returning @c 1 if this Geometry_t's "coordinateSystem" attribute
 * is set.
 *
 * @param g the Geometry_t structure.
 *
 * @return @c 1 if this Geometry_t's "coordinateSystem" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_isSetCoordinateSystem(const Geometry_t * g);


/**
 * Sets the value of the "id" attribute of this Geometry_t.
 *
 * @param g the Geometry_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_setId(Geometry_t * g, const char * id);


/**
 * Sets the value of the "coordinateSystem" attribute of this Geometry_t.
 *
 * @param g the Geometry_t structure.
 *
 * @param coordinateSystem GeometryKind_t value of the "coordinateSystem"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_setCoordinateSystem(Geometry_t * g, GeometryKind_t coordinateSystem);


/**
 * Sets the value of the "coordinateSystem" attribute of this Geometry_t.
 *
 * @param g the Geometry_t structure.
 *
 * @param coordinateSystem const char * of the "coordinateSystem" attribute to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_setCoordinateSystemAsString(Geometry_t * g,
                                     const char * coordinateSystem);


/**
 * Unsets the value of the "id" attribute of this Geometry_t.
 *
 * @param g the Geometry_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_unsetId(Geometry_t * g);


/**
 * Unsets the value of the "coordinateSystem" attribute of this Geometry_t.
 *
 * @param g the Geometry_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_unsetCoordinateSystem(Geometry_t * g);


/**
 * Returns a ListOf_t* containing CoordinateComponent_t objects from this
 * Geometry_t.
 *
 * @param g the Geometry_t structure whose "ListOfCoordinateComponents" is
 * sought.
 *
 * @return the "ListOfCoordinateComponents" from this Geometry_t as a ListOf_t
 * *.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfCoordinateComponents(Geometry_t* g);


/**
 * Get a CoordinateComponent_t from the Geometry_t.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the CoordinateComponent_t
 * to retrieve.
 *
 * @return the nth CoordinateComponent_t in the ListOfCoordinateComponents
 * within this Geometry.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const CoordinateComponent_t*
Geometry_getCoordinateComponent(Geometry_t* g, unsigned int n);


/**
 * Get a CoordinateComponent_t from the Geometry_t based on its identifier.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the CoordinateComponent_t
 * to retrieve.
 *
 * @return the CoordinateComponent_t in the ListOfCoordinateComponents within
 * this Geometry with the given id or NULL if no such CoordinateComponent_t
 * exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const CoordinateComponent_t*
Geometry_getCoordinateComponentById(Geometry_t* g, const char *sid);


/**
 * Adds a copy of the given CoordinateComponent_t to this Geometry_t.
 *
 * @param g the Geometry_t structure to which the CoordinateComponent_t should
 * be added.
 *
 * @param cc the CoordinateComponent_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_addCoordinateComponent(Geometry_t* g,
                                const CoordinateComponent_t* cc);


/**
 * Get the number of CoordinateComponent_t objects in this Geometry_t.
 *
 * @param g the Geometry_t structure to query.
 *
 * @return the number of CoordinateComponent_t objects in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumCoordinateComponents(Geometry_t* g);


/**
 * Creates a new CoordinateComponent_t object, adds it to this Geometry_t
 * object and returns the CoordinateComponent_t object created.
 *
 * @param g the Geometry_t structure to which the CoordinateComponent_t should
 * be added.
 *
 * @return a new CoordinateComponent_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_createCoordinateComponent(Geometry_t* g);


/**
 * Removes the nth CoordinateComponent_t from this Geometry_t and returns a
 * pointer to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the CoordinateComponent_t
 * to remove.
 *
 * @return a pointer to the nth CoordinateComponent_t in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_removeCoordinateComponent(Geometry_t* g, unsigned int n);


/**
 * Removes the CoordinateComponent_t from this Geometry_t based on its
 * identifier and returns a pointer to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the CoordinateComponent_t
 * to remove.
 *
 * @return the CoordinateComponent_t in this Geometry_t based on the identifier
 * or NULL if no such CoordinateComponent_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
CoordinateComponent_t*
Geometry_removeCoordinateComponentById(Geometry_t* g, const char* sid);


/**
 * Returns a ListOf_t* containing DomainType_t objects from this Geometry_t.
 *
 * @param g the Geometry_t structure whose "ListOfDomainTypes" is sought.
 *
 * @return the "ListOfDomainTypes" from this Geometry_t as a ListOf_t *.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfDomainTypes(Geometry_t* g);


/**
 * Get a DomainType_t from the Geometry_t.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the DomainType_t to
 * retrieve.
 *
 * @return the nth DomainType_t in the ListOfDomainTypes within this Geometry.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const DomainType_t*
Geometry_getDomainType(Geometry_t* g, unsigned int n);


/**
 * Get a DomainType_t from the Geometry_t based on its identifier.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the DomainType_t to
 * retrieve.
 *
 * @return the DomainType_t in the ListOfDomainTypes within this Geometry with
 * the given id or NULL if no such DomainType_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const DomainType_t*
Geometry_getDomainTypeById(Geometry_t* g, const char *sid);


/**
 * Adds a copy of the given DomainType_t to this Geometry_t.
 *
 * @param g the Geometry_t structure to which the DomainType_t should be added.
 *
 * @param dt the DomainType_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_addDomainType(Geometry_t* g, const DomainType_t* dt);


/**
 * Get the number of DomainType_t objects in this Geometry_t.
 *
 * @param g the Geometry_t structure to query.
 *
 * @return the number of DomainType_t objects in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumDomainTypes(Geometry_t* g);


/**
 * Creates a new DomainType_t object, adds it to this Geometry_t object and
 * returns the DomainType_t object created.
 *
 * @param g the Geometry_t structure to which the DomainType_t should be added.
 *
 * @return a new DomainType_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
DomainType_t*
Geometry_createDomainType(Geometry_t* g);


/**
 * Removes the nth DomainType_t from this Geometry_t and returns a pointer to
 * it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the DomainType_t to
 * remove.
 *
 * @return a pointer to the nth DomainType_t in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
DomainType_t*
Geometry_removeDomainType(Geometry_t* g, unsigned int n);


/**
 * Removes the DomainType_t from this Geometry_t based on its identifier and
 * returns a pointer to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the DomainType_t to
 * remove.
 *
 * @return the DomainType_t in this Geometry_t based on the identifier or NULL
 * if no such DomainType_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
DomainType_t*
Geometry_removeDomainTypeById(Geometry_t* g, const char* sid);


/**
 * Returns a ListOf_t* containing Domain_t objects from this Geometry_t.
 *
 * @param g the Geometry_t structure whose "ListOfDomains" is sought.
 *
 * @return the "ListOfDomains" from this Geometry_t as a ListOf_t *.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfDomains(Geometry_t* g);


/**
 * Get a Domain_t from the Geometry_t.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the Domain_t to retrieve.
 *
 * @return the nth Domain_t in the ListOfDomains within this Geometry.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const Domain_t*
Geometry_getDomain(Geometry_t* g, unsigned int n);


/**
 * Get a Domain_t from the Geometry_t based on its identifier.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the Domain_t to retrieve.
 *
 * @return the Domain_t in the ListOfDomains within this Geometry with the
 * given id or NULL if no such Domain_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const Domain_t*
Geometry_getDomainById(Geometry_t* g, const char *sid);


/**
 * Get a Domain_t from the Geometry_t based on the DomainType to which it
 * refers.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the domainType attribute of the Domain_t
 * object to retrieve.
 *
 * @return the first Domain_t in this Geometry_t based on the given domainType
 * attribute or NULL if no such Domain_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const Domain_t*
Geometry_getDomainByDomainType(Geometry_t* g, const char *sid);


/**
 * Adds a copy of the given Domain_t to this Geometry_t.
 *
 * @param g the Geometry_t structure to which the Domain_t should be added.
 *
 * @param d the Domain_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_addDomain(Geometry_t* g, const Domain_t* d);


/**
 * Get the number of Domain_t objects in this Geometry_t.
 *
 * @param g the Geometry_t structure to query.
 *
 * @return the number of Domain_t objects in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumDomains(Geometry_t* g);


/**
 * Creates a new Domain_t object, adds it to this Geometry_t object and returns
 * the Domain_t object created.
 *
 * @param g the Geometry_t structure to which the Domain_t should be added.
 *
 * @return a new Domain_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
Domain_t*
Geometry_createDomain(Geometry_t* g);


/**
 * Removes the nth Domain_t from this Geometry_t and returns a pointer to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the Domain_t to remove.
 *
 * @return a pointer to the nth Domain_t in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
Domain_t*
Geometry_removeDomain(Geometry_t* g, unsigned int n);


/**
 * Removes the Domain_t from this Geometry_t based on its identifier and
 * returns a pointer to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the Domain_t to remove.
 *
 * @return the Domain_t in this Geometry_t based on the identifier or NULL if
 * no such Domain_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
Domain_t*
Geometry_removeDomainById(Geometry_t* g, const char* sid);


/**
 * Returns a ListOf_t* containing AdjacentDomains_t objects from this
 * Geometry_t.
 *
 * @param g the Geometry_t structure whose "ListOfAdjacentDomains" is sought.
 *
 * @return the "ListOfAdjacentDomains" from this Geometry_t as a ListOf_t *.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfAdjacentDomains(Geometry_t* g);


/**
 * Get an AdjacentDomains_t from the Geometry_t.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the AdjacentDomains_t to
 * retrieve.
 *
 * @return the nth AdjacentDomains_t in the ListOfAdjacentDomains within this
 * Geometry.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const AdjacentDomains_t*
Geometry_getAdjacentDomains(Geometry_t* g, unsigned int n);


/**
 * Get an AdjacentDomains_t from the Geometry_t based on its identifier.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the AdjacentDomains_t to
 * retrieve.
 *
 * @return the AdjacentDomains_t in the ListOfAdjacentDomains within this
 * Geometry with the given id or NULL if no such AdjacentDomains_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const AdjacentDomains_t*
Geometry_getAdjacentDomainsById(Geometry_t* g, const char *sid);


/**
 * Get an AdjacentDomains_t from the Geometry_t based on the Domain1 to which
 * it refers.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the domain1 attribute of the
 * AdjacentDomains_t object to retrieve.
 *
 * @return the first AdjacentDomains_t in this Geometry_t based on the given
 * domain1 attribute or NULL if no such AdjacentDomains_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const AdjacentDomains_t*
Geometry_getAdjacentDomainsByDomain1(Geometry_t* g, const char *sid);


/**
 * Get an AdjacentDomains_t from the Geometry_t based on the Domain2 to which
 * it refers.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the domain2 attribute of the
 * AdjacentDomains_t object to retrieve.
 *
 * @return the first AdjacentDomains_t in this Geometry_t based on the given
 * domain2 attribute or NULL if no such AdjacentDomains_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const AdjacentDomains_t*
Geometry_getAdjacentDomainsByDomain2(Geometry_t* g, const char *sid);


/**
 * Adds a copy of the given AdjacentDomains_t to this Geometry_t.
 *
 * @param g the Geometry_t structure to which the AdjacentDomains_t should be
 * added.
 *
 * @param ad the AdjacentDomains_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_addAdjacentDomains(Geometry_t* g, const AdjacentDomains_t* ad);


/**
 * Get the number of AdjacentDomains_t objects in this Geometry_t.
 *
 * @param g the Geometry_t structure to query.
 *
 * @return the number of AdjacentDomains_t objects in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumAdjacentDomains(Geometry_t* g);


/**
 * Creates a new AdjacentDomains_t object, adds it to this Geometry_t object
 * and returns the AdjacentDomains_t object created.
 *
 * @param g the Geometry_t structure to which the AdjacentDomains_t should be
 * added.
 *
 * @return a new AdjacentDomains_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_createAdjacentDomains(Geometry_t* g);


/**
 * Removes the nth AdjacentDomains_t from this Geometry_t and returns a pointer
 * to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the AdjacentDomains_t to
 * remove.
 *
 * @return a pointer to the nth AdjacentDomains_t in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_removeAdjacentDomains(Geometry_t* g, unsigned int n);


/**
 * Removes the AdjacentDomains_t from this Geometry_t based on its identifier
 * and returns a pointer to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the AdjacentDomains_t to
 * remove.
 *
 * @return the AdjacentDomains_t in this Geometry_t based on the identifier or
 * NULL if no such AdjacentDomains_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
AdjacentDomains_t*
Geometry_removeAdjacentDomainsById(Geometry_t* g, const char* sid);


/**
 * Returns a ListOf_t* containing GeometryDefinition_t objects from this
 * Geometry_t.
 *
 * @param g the Geometry_t structure whose "ListOfGeometryDefinitions" is
 * sought.
 *
 * @return the "ListOfGeometryDefinitions" from this Geometry_t as a ListOf_t
 * *.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfGeometryDefinitions(Geometry_t* g);


/**
 * Get a GeometryDefinition_t from the Geometry_t.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the GeometryDefinition_t
 * to retrieve.
 *
 * @return the nth GeometryDefinition_t in the ListOfGeometryDefinitions within
 * this Geometry.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const GeometryDefinition_t*
Geometry_getGeometryDefinition(Geometry_t* g, unsigned int n);


/**
 * Get a GeometryDefinition_t from the Geometry_t based on its identifier.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the GeometryDefinition_t
 * to retrieve.
 *
 * @return the GeometryDefinition_t in the ListOfGeometryDefinitions within
 * this Geometry with the given id or NULL if no such GeometryDefinition_t
 * exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const GeometryDefinition_t*
Geometry_getGeometryDefinitionById(Geometry_t* g, const char *sid);


/**
 * Adds a copy of the given GeometryDefinition_t to this Geometry_t.
 *
 * @param g the Geometry_t structure to which the GeometryDefinition_t should
 * be added.
 *
 * @param gd the GeometryDefinition_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_addGeometryDefinition(Geometry_t* g, const GeometryDefinition_t* gd);


/**
 * Get the number of GeometryDefinition_t objects in this Geometry_t.
 *
 * @param g the Geometry_t structure to query.
 *
 * @return the number of GeometryDefinition_t objects in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumGeometryDefinitions(Geometry_t* g);


/**
 * Creates a new AnalyticGeometry_t object, adds it to this Geometry_t object
 * and returns the AnalyticGeometry_t object created.
 *
 * @param g the Geometry_t structure to which the AnalyticGeometry_t should be
 * added.
 *
 * @return a new AnalyticGeometry_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
AnalyticGeometry_t*
Geometry_createAnalyticGeometry(Geometry_t* g);


/**
 * Creates a new SampledFieldGeometry_t object, adds it to this Geometry_t
 * object and returns the SampledFieldGeometry_t object created.
 *
 * @param g the Geometry_t structure to which the SampledFieldGeometry_t should
 * be added.
 *
 * @return a new SampledFieldGeometry_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
SampledFieldGeometry_t*
Geometry_createSampledFieldGeometry(Geometry_t* g);


/**
 * Creates a new CSGeometry_t object, adds it to this Geometry_t object and
 * returns the CSGeometry_t object created.
 *
 * @param g the Geometry_t structure to which the CSGeometry_t should be added.
 *
 * @return a new CSGeometry_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
CSGeometry_t*
Geometry_createCSGeometry(Geometry_t* g);


/**
 * Creates a new ParametricGeometry_t object, adds it to this Geometry_t object
 * and returns the ParametricGeometry_t object created.
 *
 * @param g the Geometry_t structure to which the ParametricGeometry_t should
 * be added.
 *
 * @return a new ParametricGeometry_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
ParametricGeometry_t*
Geometry_createParametricGeometry(Geometry_t* g);


/**
 * Creates a new MixedGeometry_t object, adds it to this Geometry_t object and
 * returns the MixedGeometry_t object created.
 *
 * @param g the Geometry_t structure to which the MixedGeometry_t should be
 * added.
 *
 * @return a new MixedGeometry_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
MixedGeometry_t*
Geometry_createMixedGeometry(Geometry_t* g);


/**
 * Removes the nth GeometryDefinition_t from this Geometry_t and returns a
 * pointer to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the GeometryDefinition_t
 * to remove.
 *
 * @return a pointer to the nth GeometryDefinition_t in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
Geometry_removeGeometryDefinition(Geometry_t* g, unsigned int n);


/**
 * Removes the GeometryDefinition_t from this Geometry_t based on its
 * identifier and returns a pointer to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the GeometryDefinition_t
 * to remove.
 *
 * @return the GeometryDefinition_t in this Geometry_t based on the identifier
 * or NULL if no such GeometryDefinition_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
Geometry_removeGeometryDefinitionById(Geometry_t* g, const char* sid);


/**
 * Returns a ListOf_t* containing SampledField_t objects from this Geometry_t.
 *
 * @param g the Geometry_t structure whose "ListOfSampledFields" is sought.
 *
 * @return the "ListOfSampledFields" from this Geometry_t as a ListOf_t *.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
ListOf_t*
Geometry_getListOfSampledFields(Geometry_t* g);


/**
 * Get a SampledField_t from the Geometry_t.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the SampledField_t to
 * retrieve.
 *
 * @return the nth SampledField_t in the ListOfSampledFields within this
 * Geometry.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const SampledField_t*
Geometry_getSampledField(Geometry_t* g, unsigned int n);


/**
 * Get a SampledField_t from the Geometry_t based on its identifier.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the SampledField_t to
 * retrieve.
 *
 * @return the SampledField_t in the ListOfSampledFields within this Geometry
 * with the given id or NULL if no such SampledField_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
const SampledField_t*
Geometry_getSampledFieldById(Geometry_t* g, const char *sid);


/**
 * Adds a copy of the given SampledField_t to this Geometry_t.
 *
 * @param g the Geometry_t structure to which the SampledField_t should be
 * added.
 *
 * @param sf the SampledField_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_addSampledField(Geometry_t* g, const SampledField_t* sf);


/**
 * Get the number of SampledField_t objects in this Geometry_t.
 *
 * @param g the Geometry_t structure to query.
 *
 * @return the number of SampledField_t objects in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
unsigned int
Geometry_getNumSampledFields(Geometry_t* g);


/**
 * Creates a new SampledField_t object, adds it to this Geometry_t object and
 * returns the SampledField_t object created.
 *
 * @param g the Geometry_t structure to which the SampledField_t should be
 * added.
 *
 * @return a new SampledField_t object instance.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
SampledField_t*
Geometry_createSampledField(Geometry_t* g);


/**
 * Removes the nth SampledField_t from this Geometry_t and returns a pointer to
 * it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the SampledField_t to
 * remove.
 *
 * @return a pointer to the nth SampledField_t in this Geometry_t.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
SampledField_t*
Geometry_removeSampledField(Geometry_t* g, unsigned int n);


/**
 * Removes the SampledField_t from this Geometry_t based on its identifier and
 * returns a pointer to it.
 *
 * @param g the Geometry_t structure to search.
 *
 * @param sid a string representing the identifier of the SampledField_t to
 * remove.
 *
 * @return the SampledField_t in this Geometry_t based on the identifier or
 * NULL if no such SampledField_t exists.
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
SampledField_t*
Geometry_removeSampledFieldById(Geometry_t* g, const char* sid);


/**
 * Predicate returning @c 1 if all the required attributes for this Geometry_t
 * object have been set.
 *
 * @param g the Geometry_t structure.
 *
 * @return @c 1 to indicate that all the required attributes of this Geometry_t
 * have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required attributes for the Geometry_t object are:
 * @li "coordinateSystem"
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_hasRequiredAttributes(const Geometry_t * g);


/**
 * Predicate returning @c 1 if all the required elements for this Geometry_t
 * object have been set.
 *
 * @param g the Geometry_t structure.
 *
 * @return @c 1 to indicate that all the required elements of this Geometry_t
 * have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required elements for the Geometry_t object are:
 *
 * @memberof Geometry_t
 */
LIBSBML_EXTERN
int
Geometry_hasRequiredElements(const Geometry_t * g);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Geometry_H__ */


