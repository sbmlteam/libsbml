/**
 * @file DiffusionCoefficient.h
 * @brief Definition of the DiffusionCoefficient class.
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
 * @class DiffusionCoefficient
 * @sbmlbrief{spatial} TODO:Definition of the DiffusionCoefficient class.
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
 * @class doc_diffusioncoefficient_type
 *
 * @par
 * The attribute "type" on a DiffusionCoefficient object is used to TODO:add
 * explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Spatial specification, the following are the
 * allowable values for "type":
 * <ul>
 * <li> @c "isotropic", TODO:add description
 *
 * <li> @c "anisotropic", TODO:add description
 *
 * <li> @c "tensor", TODO:add description
 *
 * </ul>
 *
 * @class doc_diffusioncoefficient_coordinateReference1
 *
 * @par
 * The attribute "coordinateReference1" on a DiffusionCoefficient object is
 * used to TODO:add explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Spatial specification, the following are the
 * allowable values for "coordinateReference1":
 * <ul>
 * <li> @c "cartesianX", TODO:add description
 *
 * <li> @c "cartesianY", TODO:add description
 *
 * <li> @c "cartesianZ", TODO:add description
 *
 * </ul>
 *
 * @class doc_diffusioncoefficient_coordinateReference2
 *
 * @par
 * The attribute "coordinateReference2" on a DiffusionCoefficient object is
 * used to TODO:add explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Spatial specification, the following are the
 * allowable values for "coordinateReference2":
 * <ul>
 * <li> @c "cartesianX", TODO:add description
 *
 * <li> @c "cartesianY", TODO:add description
 *
 * <li> @c "cartesianZ", TODO:add description
 *
 * </ul>
 */


#ifndef DiffusionCoefficient_H__
#define DiffusionCoefficient_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DiffusionCoefficient : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mVariable;
  DiffusionKind_t mType;
  CoordinateKind_t mCoordinateReference1;
  CoordinateKind_t mCoordinateReference2;

  /** @endcond */

public:

  /**
   * Creates a new DiffusionCoefficient using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DiffusionCoefficient.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DiffusionCoefficient.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this DiffusionCoefficient.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DiffusionCoefficient(unsigned int level =
    SpatialExtension::getDefaultLevel(),
                       unsigned int version =
                         SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion =
                         SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new DiffusionCoefficient using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DiffusionCoefficient(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for DiffusionCoefficient.
   *
   * @param orig the DiffusionCoefficient instance to copy.
   */
  DiffusionCoefficient(const DiffusionCoefficient& orig);


  /**
   * Assignment operator for DiffusionCoefficient.
   *
   * @param rhs the DiffusionCoefficient object whose values are to be used as
   * the basis of the assignment.
   */
  DiffusionCoefficient& operator=(const DiffusionCoefficient& rhs);


  /**
   * Creates and returns a deep copy of this DiffusionCoefficient object.
   *
   * @return a (deep) copy of this DiffusionCoefficient object.
   */
  virtual DiffusionCoefficient* clone() const;


  /**
   * Destructor for DiffusionCoefficient.
   */
  virtual ~DiffusionCoefficient();


  /**
   * Returns the value of the "variable" attribute of this
   * DiffusionCoefficient.
   *
   * @return the value of the "variable" attribute of this DiffusionCoefficient
   * as a string.
   */
  const std::string& getVariable() const;


  /**
   * Returns the value of the "type" attribute of this DiffusionCoefficient.
   *
   * @return the value of the "type" attribute of this DiffusionCoefficient as
   * a DiffusionKind_t.
   *
   * @copydetails doc_diffusioncoefficient_type
   * @if clike The value is drawn from the enumeration @ref DiffusionKind_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{SPATIAL_DIFFUSIONKIND_ISOTROPIC, DiffusionKind_t}
   * @li @sbmlconstant{SPATIAL_DIFFUSIONKIND_ANISOTROPIC, DiffusionKind_t}
   * @li @sbmlconstant{SPATIAL_DIFFUSIONKIND_TENSOR, DiffusionKind_t}
   * @li @sbmlconstant{SPATIAL_DIFFUSIONKIND_INVALID, DiffusionKind_t}
   */
  DiffusionKind_t getType() const;


  /**
   * Returns the value of the "type" attribute of this DiffusionCoefficient.
   *
   * @return the value of the "type" attribute of this DiffusionCoefficient as
   * a string.
   *
   * @copydetails doc_diffusioncoefficient_type
   * The possible values returned by this method are:
   * @li @c "isotropic"
   * @li @c "anisotropic"
   * @li @c "tensor"
   * @li @c "invalid DiffusionKind value"
   */
  std::string getTypeAsString() const;


  /**
   * Returns the value of the "coordinateReference1" attribute of this
   * DiffusionCoefficient.
   *
   * @return the value of the "coordinateReference1" attribute of this
   * DiffusionCoefficient as a CoordinateKind_t.
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference1
   * @if clike The value is drawn from the enumeration @ref CoordinateKind_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Y, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Z, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t}
   */
  CoordinateKind_t getCoordinateReference1() const;


  /**
   * Returns the value of the "coordinateReference1" attribute of this
   * DiffusionCoefficient.
   *
   * @return the value of the "coordinateReference1" attribute of this
   * DiffusionCoefficient as a string.
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference1
   * The possible values returned by this method are:
   * @li @c "cartesianX"
   * @li @c "cartesianY"
   * @li @c "cartesianZ"
   * @li @c "invalid CoordinateKind value"
   */
  const std::string& getCoordinateReference1AsString() const;


  /**
   * Returns the value of the "coordinateReference2" attribute of this
   * DiffusionCoefficient.
   *
   * @return the value of the "coordinateReference2" attribute of this
   * DiffusionCoefficient as a CoordinateKind_t.
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference2
   * @if clike The value is drawn from the enumeration @ref CoordinateKind_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Y, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Z, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t}
   */
  CoordinateKind_t getCoordinateReference2() const;


  /**
   * Returns the value of the "coordinateReference2" attribute of this
   * DiffusionCoefficient.
   *
   * @return the value of the "coordinateReference2" attribute of this
   * DiffusionCoefficient as a string.
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference2
   * The possible values returned by this method are:
   * @li @c "cartesianX"
   * @li @c "cartesianY"
   * @li @c "cartesianZ"
   * @li @c "invalid CoordinateKind value"
   */
  std::string getCoordinateReference2AsString() const;


  /**
   * Predicate returning @c true if this DiffusionCoefficient's "variable"
   * attribute is set.
   *
   * @return @c true if this DiffusionCoefficient's "variable" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetVariable() const;


  /**
   * Predicate returning @c true if this DiffusionCoefficient's "type"
   * attribute is set.
   *
   * @return @c true if this DiffusionCoefficient's "type" attribute has been
   * set, otherwise @c false is returned.
   *
   * @copydetails doc_diffusioncoefficient_type
   */
  bool isSetType() const;


  /**
   * Predicate returning @c true if this DiffusionCoefficient's
   * "coordinateReference1" attribute is set.
   *
   * @return @c true if this DiffusionCoefficient's "coordinateReference1"
   * attribute has been set, otherwise @c false is returned.
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference1
   */
  bool isSetCoordinateReference1() const;


  /**
   * Predicate returning @c true if this DiffusionCoefficient's
   * "coordinateReference2" attribute is set.
   *
   * @return @c true if this DiffusionCoefficient's "coordinateReference2"
   * attribute has been set, otherwise @c false is returned.
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference2
   */
  bool isSetCoordinateReference2() const;


  /**
   * Sets the value of the "variable" attribute of this DiffusionCoefficient.
   *
   * @param variable std::string& value of the "variable" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVariable(const std::string& variable);


  /**
   * Sets the value of the "type" attribute of this DiffusionCoefficient.
   *
   * @param type @if clike DiffusionKind_t@else int@endif value of the "type"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_diffusioncoefficient_type
   */
  int setType(const DiffusionKind_t type);


  /**
   * Sets the value of the "type" attribute of this DiffusionCoefficient.
   *
   * @param type std::string& of the "type" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_diffusioncoefficient_type
   */
  int setType(const std::string& type);


  /**
   * Sets the value of the "coordinateReference1" attribute of this
   * DiffusionCoefficient.
   *
   * @param coordinateReference1 @if clike CoordinateKind_t@else int@endif
   * value of the "coordinateReference1" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference1
   */
  int setCoordinateReference1(const CoordinateKind_t coordinateReference1);


  /**
   * Sets the value of the "coordinateReference1" attribute of this
   * DiffusionCoefficient.
   *
   * @param coordinateReference1 std::string& of the "coordinateReference1"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference1
   */
  int setCoordinateReference1(const std::string& coordinateReference1);


  /**
   * Sets the value of the "coordinateReference2" attribute of this
   * DiffusionCoefficient.
   *
   * @param coordinateReference2 @if clike CoordinateKind_t@else int@endif
   * value of the "coordinateReference2" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference2
   */
  int setCoordinateReference2(const CoordinateKind_t coordinateReference2);


  /**
   * Sets the value of the "coordinateReference2" attribute of this
   * DiffusionCoefficient.
   *
   * @param coordinateReference2 std::string& of the "coordinateReference2"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference2
   */
  int setCoordinateReference2(const std::string& coordinateReference2);


  /**
   * Unsets the value of the "variable" attribute of this DiffusionCoefficient.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVariable();


  /**
   * Unsets the value of the "type" attribute of this DiffusionCoefficient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_diffusioncoefficient_type
   */
  int unsetType();


  /**
   * Unsets the value of the "coordinateReference1" attribute of this
   * DiffusionCoefficient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference1
   */
  int unsetCoordinateReference1();


  /**
   * Unsets the value of the "coordinateReference2" attribute of this
   * DiffusionCoefficient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_diffusioncoefficient_coordinateReference2
   */
  int unsetCoordinateReference2();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this DiffusionCoefficient object.
   *
   * For DiffusionCoefficient, the XML element name is always
   * @c "diffusionCoefficient".
   *
   * @return the name of this element, i.e. @c "diffusionCoefficient".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DiffusionCoefficient object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_DIFFUSIONCOEFFICIENT, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DiffusionCoefficient object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DiffusionCoefficient have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the DiffusionCoefficient object are:
   * @li "variable"
   * @li "type"
   */
  virtual bool hasRequiredAttributes() const;



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
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * DiffusionCoefficient.
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
   * DiffusionCoefficient.
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
   * DiffusionCoefficient.
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
   * DiffusionCoefficient.
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
   * DiffusionCoefficient.
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
   * Predicate returning @c true if this DiffusionCoefficient's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DiffusionCoefficient's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DiffusionCoefficient.
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
   * DiffusionCoefficient.
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
   * DiffusionCoefficient.
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
   * DiffusionCoefficient.
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
   * DiffusionCoefficient.
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
   * DiffusionCoefficient.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */




  #endif /* !SWIG */


protected:


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
 * Creates a new DiffusionCoefficient_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DiffusionCoefficient_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DiffusionCoefficient_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this DiffusionCoefficient_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
DiffusionCoefficient_t *
DiffusionCoefficient_create(unsigned int level,
                            unsigned int version,
                            unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DiffusionCoefficient_t object.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return a (deep) copy of this DiffusionCoefficient_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
DiffusionCoefficient_t*
DiffusionCoefficient_clone(const DiffusionCoefficient_t* dc);


/**
 * Frees this DiffusionCoefficient_t object.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
void
DiffusionCoefficient_free(DiffusionCoefficient_t* dc);


/**
 * Returns the value of the "variable" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure whose variable is sought.
 *
 * @return the value of the "variable" attribute of this DiffusionCoefficient_t
 * as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
char *
DiffusionCoefficient_getVariable(const DiffusionCoefficient_t * dc);


/**
 * Returns the value of the "type" attribute of this DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure whose type is sought.
 *
 * @return the value of the "type" attribute of this DiffusionCoefficient_t as
 * a DiffusionKind_t.
 *
 * @copydetails doc_diffusioncoefficient_type
 * @if clike The value is drawn from the enumeration @ref DiffusionKind_t
 * @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{SPATIAL_DIFFUSIONKIND_ISOTROPIC, DiffusionKind_t}
 * @li @sbmlconstant{SPATIAL_DIFFUSIONKIND_ANISOTROPIC, DiffusionKind_t}
 * @li @sbmlconstant{SPATIAL_DIFFUSIONKIND_TENSOR, DiffusionKind_t}
 * @li @sbmlconstant{SPATIAL_DIFFUSIONKIND_INVALID, DiffusionKind_t}
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
DiffusionKind_t
DiffusionCoefficient_getType(const DiffusionCoefficient_t * dc);


/**
 * Returns the value of the "type" attribute of this DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure whose type is sought.
 *
 * @return the value of the "type" attribute of this DiffusionCoefficient_t as
 * a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_diffusioncoefficient_type
 * The possible values returned by this method are:
 * @li @c "isotropic"
 * @li @c "anisotropic"
 * @li @c "tensor"
 * @li @c "invalid DiffusionKind value"
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
char *
DiffusionCoefficient_getTypeAsString(const DiffusionCoefficient_t * dc);


/**
 * Returns the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure whose coordinateReference1 is
 * sought.
 *
 * @return the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t as a CoordinateKind_t.
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference1
 * @if clike The value is drawn from the enumeration @ref CoordinateKind_t
 * @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Y, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Z, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t}
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
CoordinateKind_t
DiffusionCoefficient_getCoordinateReference1(const DiffusionCoefficient_t *
  dc);


/**
 * Returns the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure whose coordinateReference1 is
 * sought.
 *
 * @return the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t as a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference1
 * The possible values returned by this method are:
 * @li @c "cartesianX"
 * @li @c "cartesianY"
 * @li @c "cartesianZ"
 * @li @c "invalid CoordinateKind value"
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
char *
DiffusionCoefficient_getCoordinateReference1AsString(const
  DiffusionCoefficient_t * dc);


/**
 * Returns the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure whose coordinateReference2 is
 * sought.
 *
 * @return the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t as a CoordinateKind_t.
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference2
 * @if clike The value is drawn from the enumeration @ref CoordinateKind_t
 * @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Y, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Z, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t}
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
CoordinateKind_t
DiffusionCoefficient_getCoordinateReference2(const DiffusionCoefficient_t *
  dc);


/**
 * Returns the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure whose coordinateReference2 is
 * sought.
 *
 * @return the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t as a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference2
 * The possible values returned by this method are:
 * @li @c "cartesianX"
 * @li @c "cartesianY"
 * @li @c "cartesianZ"
 * @li @c "invalid CoordinateKind value"
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
char *
DiffusionCoefficient_getCoordinateReference2AsString(const
  DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 (true) if this DiffusionCoefficient_t's "variable"
 * attribute is set.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return @c 1 (true) if this DiffusionCoefficient_t's "variable" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetVariable(const DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 (true) if this DiffusionCoefficient_t's "type"
 * attribute is set.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return @c 1 (true) if this DiffusionCoefficient_t's "type" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_diffusioncoefficient_type
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetType(const DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 (true) if this DiffusionCoefficient_t's
 * "coordinateReference1" attribute is set.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return @c 1 (true) if this DiffusionCoefficient_t's "coordinateReference1"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference1
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetCoordinateReference1(const DiffusionCoefficient_t *
  dc);


/**
 * Predicate returning @c 1 (true) if this DiffusionCoefficient_t's
 * "coordinateReference2" attribute is set.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return @c 1 (true) if this DiffusionCoefficient_t's "coordinateReference2"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference2
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetCoordinateReference2(const DiffusionCoefficient_t *
  dc);


/**
 * Sets the value of the "variable" attribute of this DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param variable const char * value of the "variable" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setVariable(DiffusionCoefficient_t * dc,
                                 const char * variable);


/**
 * Sets the value of the "type" attribute of this DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param type DiffusionKind_t value of the "type" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_diffusioncoefficient_type
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setType(DiffusionCoefficient_t * dc,
                             DiffusionKind_t type);


/**
 * Sets the value of the "type" attribute of this DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param type const char * of the "type" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_diffusioncoefficient_type
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setTypeAsString(DiffusionCoefficient_t * dc,
                                     const char * type);


/**
 * Sets the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param coordinateReference1 CoordinateKind_t value of the
 * "coordinateReference1" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference1
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference1(DiffusionCoefficient_t * dc,
                                             CoordinateKind_t
                                               coordinateReference1);


/**
 * Sets the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param coordinateReference1 const char * of the "coordinateReference1"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference1
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference1AsString(
                                                     DiffusionCoefficient_t *
                                                       dc,
                                                     const char * coordinateReference1);


/**
 * Sets the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param coordinateReference2 CoordinateKind_t value of the
 * "coordinateReference2" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference2
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference2(DiffusionCoefficient_t * dc,
                                             CoordinateKind_t
                                               coordinateReference2);


/**
 * Sets the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param coordinateReference2 const char * of the "coordinateReference2"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference2
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference2AsString(
                                                     DiffusionCoefficient_t *
                                                       dc,
                                                     const char * coordinateReference2);


/**
 * Unsets the value of the "variable" attribute of this DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetVariable(DiffusionCoefficient_t * dc);


/**
 * Unsets the value of the "type" attribute of this DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_diffusioncoefficient_type
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetType(DiffusionCoefficient_t * dc);


/**
 * Unsets the value of the "coordinateReference1" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference1
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetCoordinateReference1(DiffusionCoefficient_t * dc);


/**
 * Unsets the value of the "coordinateReference2" attribute of this
 * DiffusionCoefficient_t.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_diffusioncoefficient_coordinateReference2
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetCoordinateReference2(DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DiffusionCoefficient_t object have been set.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DiffusionCoefficient_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the DiffusionCoefficient_t object are:
 * @li "variable"
 * @li "type"
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_hasRequiredAttributes(const DiffusionCoefficient_t * dc);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DiffusionCoefficient_H__ */


