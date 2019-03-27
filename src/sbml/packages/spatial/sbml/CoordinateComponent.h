/**
 * @file CoordinateComponent.h
 * @brief Definition of the CoordinateComponent class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 *
 * @class CoordinateComponent
 * @sbmlbrief{spatial} TODO:Definition of the CoordinateComponent class.
 */

/**
 * <!-- ~ ~ ~ ~ ~ Start of common documentation strings ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 * The following text is used as common documentation blocks copied multiple
 * times elsewhere in this file. The use of @class is a hack needed because
 * Doxygen's @copydetails command has limited functionality. Symbols
 * beginning with "doc_" are marked as ignored in our Doxygen configuration.
 * ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ -->
 *
 *
 * @class doc_coordinatecomponent_type
 *
 * @par
 * The attribute "type" on a CoordinateComponent object is used to TODO:add
 * explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Spatial specification, the following are the
 * allowable values for "type":
 * <ul>
 * <li> @c "cartesianX", TODO:add description
 *
 * <li> @c "cartesianY", TODO:add description
 *
 * <li> @c "cartesianZ", TODO:add description
 *
 * </ul>
 */


#ifndef CoordinateComponent_H__
#define CoordinateComponent_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/Boundary.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CoordinateComponent : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  CoordinateKind_t mType;
  std::string mUnit;
  Boundary* mBoundaryMin;
  Boundary* mBoundaryMax;

  /** @endcond */

public:

  /**
   * Creates a new CoordinateComponent using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CoordinateComponent.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CoordinateComponent.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CoordinateComponent.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CoordinateComponent(unsigned int level = SpatialExtension::getDefaultLevel(),
                      unsigned int version =
                        SpatialExtension::getDefaultVersion(),
                      unsigned int pkgVersion =
                        SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CoordinateComponent using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CoordinateComponent(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CoordinateComponent.
   *
   * @param orig the CoordinateComponent instance to copy.
   */
  CoordinateComponent(const CoordinateComponent& orig);


  /**
   * Assignment operator for CoordinateComponent.
   *
   * @param rhs the CoordinateComponent object whose values are to be used as
   * the basis of the assignment.
   */
  CoordinateComponent& operator=(const CoordinateComponent& rhs);


  /**
   * Creates and returns a deep copy of this CoordinateComponent object.
   *
   * @return a (deep) copy of this CoordinateComponent object.
   */
  virtual CoordinateComponent* clone() const;


  /**
   * Destructor for CoordinateComponent.
   */
  virtual ~CoordinateComponent();


  /**
   * Returns the value of the "id" attribute of this CoordinateComponent.
   *
   * @return the value of the "id" attribute of this CoordinateComponent as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this CoordinateComponent.
   *
   * @return the value of the "name" attribute of this CoordinateComponent as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "type" attribute of this CoordinateComponent.
   *
   * @return the value of the "type" attribute of this CoordinateComponent as a
   * CoordinateKind_t.
   *
   * @copydetails doc_coordinatecomponent_type
   * @if clike The value is drawn from the enumeration @ref CoordinateKind_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Y, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Z, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t}
   */
  CoordinateKind_t getType() const;


  /**
   * Returns the value of the "type" attribute of this CoordinateComponent.
   *
   * @return the value of the "type" attribute of this CoordinateComponent as a
   * string.
   *
   * @copydetails doc_coordinatecomponent_type
   * The possible values returned by this method are:
   * @li @c "cartesianX"
   * @li @c "cartesianY"
   * @li @c "cartesianZ"
   * @li @c "invalid CoordinateKind value"
   */
  const std::string& getTypeAsString() const;


  /**
   * Returns the value of the "unit" attribute of this CoordinateComponent.
   *
   * @return the value of the "unit" attribute of this CoordinateComponent as a
   * string.
   */
  const std::string& getUnit() const;


  /**
   * Predicate returning @c true if this CoordinateComponent's "id" attribute
   * is set.
   *
   * @return @c true if this CoordinateComponent's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this CoordinateComponent's "name" attribute
   * is set.
   *
   * @return @c true if this CoordinateComponent's "name" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this CoordinateComponent's "type" attribute
   * is set.
   *
   * @return @c true if this CoordinateComponent's "type" attribute has been
   * set, otherwise @c false is returned.
   *
   * @copydetails doc_coordinatecomponent_type
   */
  bool isSetType() const;


  /**
   * Predicate returning @c true if this CoordinateComponent's "unit" attribute
   * is set.
   *
   * @return @c true if this CoordinateComponent's "unit" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetUnit() const;


  /**
   * Sets the value of the "id" attribute of this CoordinateComponent.
   *
   * @param id std::string& value of the "id" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * Calling this function with @p id = @c NULL or an empty string is
   * equivalent to calling unsetId().
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "name" attribute of this CoordinateComponent.
   *
   * @param name std::string& value of the "name" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p name = @c NULL or an empty string is
   * equivalent to calling unsetName().
   */
  virtual int setName(const std::string& name);


  /**
   * Sets the value of the "type" attribute of this CoordinateComponent.
   *
   * @param type @if clike CoordinateKind_t@else int@endif value of the "type"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_coordinatecomponent_type
   */
  int setType(const CoordinateKind_t type);


  /**
   * Sets the value of the "type" attribute of this CoordinateComponent.
   *
   * @param type std::string& of the "type" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_coordinatecomponent_type
   */
  int setType(const std::string& type);


  /**
   * Sets the value of the "unit" attribute of this CoordinateComponent.
   *
   * @param unit std::string& value of the "unit" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setUnit(const std::string& unit);


  /**
   * Unsets the value of the "id" attribute of this CoordinateComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this CoordinateComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "type" attribute of this CoordinateComponent.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_coordinatecomponent_type
   */
  int unsetType();


  /**
   * Unsets the value of the "unit" attribute of this CoordinateComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetUnit();


  /**
   * Returns the value of the "boundaryMin" element of this
   * CoordinateComponent.
   *
   * @return the value of the "boundaryMin" element of this CoordinateComponent
   * as a Boundary*.
   */
  const Boundary* getBoundaryMin() const;


  /**
   * Returns the value of the "boundaryMin" element of this
   * CoordinateComponent.
   *
   * @return the value of the "boundaryMin" element of this CoordinateComponent
   * as a Boundary*.
   */
  Boundary* getBoundaryMin();


  /**
   * Returns the value of the "boundaryMax" element of this
   * CoordinateComponent.
   *
   * @return the value of the "boundaryMax" element of this CoordinateComponent
   * as a Boundary*.
   */
  const Boundary* getBoundaryMax() const;


  /**
   * Returns the value of the "boundaryMax" element of this
   * CoordinateComponent.
   *
   * @return the value of the "boundaryMax" element of this CoordinateComponent
   * as a Boundary*.
   */
  Boundary* getBoundaryMax();


  /**
   * Predicate returning @c true if this CoordinateComponent's "boundaryMin"
   * element is set.
   *
   * @return @c true if this CoordinateComponent's "boundaryMin" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetBoundaryMin() const;


  /**
   * Predicate returning @c true if this CoordinateComponent's "boundaryMax"
   * element is set.
   *
   * @return @c true if this CoordinateComponent's "boundaryMax" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetBoundaryMax() const;


  /**
   * Sets the value of the "boundaryMin" element of this CoordinateComponent.
   *
   * @param boundaryMin Boundary* value of the "boundaryMin" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setBoundaryMin(const Boundary* boundaryMin);


  /**
   * Sets the value of the "boundaryMax" element of this CoordinateComponent.
   *
   * @param boundaryMax Boundary* value of the "boundaryMax" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setBoundaryMax(const Boundary* boundaryMax);


  /**
   * Creates a new Boundary object, adds it to this CoordinateComponent object
   * and returns the Boundary object created.
   *
   * @return a new Boundary object instance.
   */
  Boundary* createBoundaryMin();


  /**
   * Creates a new Boundary object, adds it to this CoordinateComponent object
   * and returns the Boundary object created.
   *
   * @return a new Boundary object instance.
   */
  Boundary* createBoundaryMax();


  /**
   * Unsets the value of the "boundaryMin" element of this CoordinateComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetBoundaryMin();


  /**
   * Unsets the value of the "boundaryMax" element of this CoordinateComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetBoundaryMax();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this CoordinateComponent object.
   *
   * For CoordinateComponent, the XML element name is always
   * @c "coordinateComponent".
   *
   * @return the name of this element, i.e. @c "coordinateComponent".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CoordinateComponent object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_COORDINATECOMPONENT, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CoordinateComponent object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CoordinateComponent have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CoordinateComponent object are:
   * @li "id"
   * @li "type"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * CoordinateComponent object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * CoordinateComponent have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the CoordinateComponent object are:
   * @li "boundaryMin"
   * @li "boundaryMax"
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



  /** @cond doxygenLibsbmlInternal */

  /**
   * Updates the namespaces when setLevelVersion is used
   */
  virtual void updateSBMLNamespace(const std::string& package,
                                   unsigned int level,
                                   unsigned int version);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Gets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Gets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Gets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Gets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Predicate returning @c true if this CoordinateComponent's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CoordinateComponent's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Sets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Sets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Sets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Sets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Unsets the value of the "attributeName" attribute of this
   * CoordinateComponent.
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
   * Creates and returns an new "elementName" object in this
   * CoordinateComponent.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this CoordinateComponent.
   *
   * @param elementName, the name of the element to create.
   *
   * @param element, pointer to the element to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int addChildObject(const std::string& elementName,
                             const SBase* element);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Removes and returns the new "elementName" object with the given id in this
   * CoordinateComponent.
   *
   * @param elementName, the name of the element to remove.
   *
   * @param id, the id of the element to remove.
   *
   * @return pointer to the element removed.
   */
  virtual SBase* removeChildObject(const std::string& elementName,
                                   const std::string& id);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this CoordinateComponent.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this CoordinateComponent.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int the index of the object to retrieve.
   *
   * @return pointer to the object.
   */
  virtual SBase* getObject(const std::string& elementName, unsigned int index);

  /** @endcond */




  #endif /* !SWIG */


  /**
   * Returns the first child element that has the given @p id in the model-wide
   * SId namespace, or @c NULL if no such object is found.
   *
   * @param id a string representing the id attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p id. If no such
   * object is found, this method returns @c NULL.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid. If no
   * such object is found this method returns @c NULL.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @param filter an ElementFilter that may impose restrictions on the objects
   * to be retrieved.
   *
   * @return a List pointer of pointers to all SBase child objects with any
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
 * Creates a new CoordinateComponent_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CoordinateComponent_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CoordinateComponent_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CoordinateComponent_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
CoordinateComponent_t *
CoordinateComponent_create(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CoordinateComponent_t object.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return a (deep) copy of this CoordinateComponent_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
CoordinateComponent_t*
CoordinateComponent_clone(const CoordinateComponent_t* cc);


/**
 * Frees this CoordinateComponent_t object.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
void
CoordinateComponent_free(CoordinateComponent_t* cc);


/**
 * Returns the value of the "id" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this CoordinateComponent_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
char *
CoordinateComponent_getId(const CoordinateComponent_t * cc);


/**
 * Returns the value of the "name" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this CoordinateComponent_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
char *
CoordinateComponent_getName(const CoordinateComponent_t * cc);


/**
 * Returns the value of the "type" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure whose type is sought.
 *
 * @return the value of the "type" attribute of this CoordinateComponent_t as a
 * CoordinateKind_t.
 *
 * @copydetails doc_coordinatecomponent_type
 * @if clike The value is drawn from the enumeration @ref CoordinateKind_t
 * @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Y, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Z, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t}
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateComponent_getType(const CoordinateComponent_t * cc);


/**
 * Returns the value of the "type" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure whose type is sought.
 *
 * @return the value of the "type" attribute of this CoordinateComponent_t as a
 * const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_coordinatecomponent_type
 * The possible values returned by this method are:
 * @li @c "cartesianX"
 * @li @c "cartesianY"
 * @li @c "cartesianZ"
 * @li @c "invalid CoordinateKind value"
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
char *
CoordinateComponent_getTypeAsString(const CoordinateComponent_t * cc);


/**
 * Returns the value of the "unit" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure whose unit is sought.
 *
 * @return the value of the "unit" attribute of this CoordinateComponent_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
char *
CoordinateComponent_getUnit(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 (true) if this CoordinateComponent_t's "id"
 * attribute is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 (true) if this CoordinateComponent_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetId(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 (true) if this CoordinateComponent_t's "name"
 * attribute is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 (true) if this CoordinateComponent_t's "name" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetName(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 (true) if this CoordinateComponent_t's "type"
 * attribute is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 (true) if this CoordinateComponent_t's "type" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_coordinatecomponent_type
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetType(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 (true) if this CoordinateComponent_t's "unit"
 * attribute is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 (true) if this CoordinateComponent_t's "unit" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetUnit(const CoordinateComponent_t * cc);


/**
 * Sets the value of the "id" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling CoordinateComponent_unsetId().
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setId(CoordinateComponent_t * cc, const char * id);


/**
 * Sets the value of the "name" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling CoordinateComponent_unsetName().
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setName(CoordinateComponent_t * cc, const char * name);


/**
 * Sets the value of the "type" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @param type CoordinateKind_t value of the "type" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_coordinatecomponent_type
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setType(CoordinateComponent_t * cc,
                            CoordinateKind_t type);


/**
 * Sets the value of the "type" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @param type const char * of the "type" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_coordinatecomponent_type
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setTypeAsString(CoordinateComponent_t * cc,
                                    const char * type);


/**
 * Sets the value of the "unit" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @param unit const char * value of the "unit" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setUnit(CoordinateComponent_t * cc, const char * unit);


/**
 * Unsets the value of the "id" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_unsetId(CoordinateComponent_t * cc);


/**
 * Unsets the value of the "name" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_unsetName(CoordinateComponent_t * cc);


/**
 * Unsets the value of the "type" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_coordinatecomponent_type
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_unsetType(CoordinateComponent_t * cc);


/**
 * Unsets the value of the "unit" attribute of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_unsetUnit(CoordinateComponent_t * cc);


/**
 * Returns the value of the "boundaryMin" element of this
 * CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure whose boundaryMin is sought.
 *
 * @return the value of the "boundaryMin" element of this CoordinateComponent_t
 * as a Boundary*.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
const Boundary_t*
CoordinateComponent_getBoundaryMin(const CoordinateComponent_t * cc);


/**
 * Returns the value of the "boundaryMax" element of this
 * CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure whose boundaryMax is sought.
 *
 * @return the value of the "boundaryMax" element of this CoordinateComponent_t
 * as a Boundary*.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
const Boundary_t*
CoordinateComponent_getBoundaryMax(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 (true) if this CoordinateComponent_t's
 * "boundaryMin" element is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 (true) if this CoordinateComponent_t's "boundaryMin" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetBoundaryMin(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 (true) if this CoordinateComponent_t's
 * "boundaryMax" element is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 (true) if this CoordinateComponent_t's "boundaryMax" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetBoundaryMax(const CoordinateComponent_t * cc);


/**
 * Sets the value of the "boundaryMin" element of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @param boundaryMin Boundary_t* value of the "boundaryMin" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setBoundaryMin(CoordinateComponent_t * cc,
                                   const Boundary_t* boundaryMin);


/**
 * Sets the value of the "boundaryMax" element of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @param boundaryMax Boundary_t* value of the "boundaryMax" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setBoundaryMax(CoordinateComponent_t * cc,
                                   const Boundary_t* boundaryMax);


/**
 * Creates a new Boundary_t object, adds it to this CoordinateComponent_t
 * object and returns the Boundary_t object created.
 *
 * @param cc the CoordinateComponent_t structure to which the Boundary_t should
 * be added.
 *
 * @return a new Boundary_t object instance.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_createBoundaryMin(CoordinateComponent_t* cc);


/**
 * Creates a new Boundary_t object, adds it to this CoordinateComponent_t
 * object and returns the Boundary_t object created.
 *
 * @param cc the CoordinateComponent_t structure to which the Boundary_t should
 * be added.
 *
 * @return a new Boundary_t object instance.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_createBoundaryMax(CoordinateComponent_t* cc);


/**
 * Unsets the value of the "boundaryMin" element of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_unsetBoundaryMin(CoordinateComponent_t * cc);


/**
 * Unsets the value of the "boundaryMax" element of this CoordinateComponent_t.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_unsetBoundaryMax(CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CoordinateComponent_t object have been set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CoordinateComponent_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the CoordinateComponent_t object are:
 * @li "id"
 * @li "type"
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_hasRequiredAttributes(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * CoordinateComponent_t object have been set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * CoordinateComponent_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the CoordinateComponent_t object are:
 * @li "boundaryMin"
 * @li "boundaryMax"
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_hasRequiredElements(const CoordinateComponent_t * cc);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CoordinateComponent_H__ */


