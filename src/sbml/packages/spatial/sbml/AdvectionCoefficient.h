/**
 * @file AdvectionCoefficient.h
 * @brief Definition of the AdvectionCoefficient class.
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
 * @class AdvectionCoefficient
 * @sbmlbrief{spatial} TODO:Definition of the AdvectionCoefficient class.
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
 * @class doc_advectioncoefficient_coordinate
 *
 * @par
 * The attribute "coordinate" on a AdvectionCoefficient object is used to
 * TODO:add explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Spatial specification, the following are the
 * allowable values for "coordinate":
 * <ul>
 * <li> @c "cartesianX", TODO:add description
 *
 * <li> @c "cartesianY", TODO:add description
 *
 * <li> @c "cartesianZ", TODO:add description
 *
 * </ul>
 */


#ifndef AdvectionCoefficient_H__
#define AdvectionCoefficient_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN AdvectionCoefficient : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mVariable;
  CoordinateKind_t mCoordinate;

  /** @endcond */

public:

  /**
   * Creates a new AdvectionCoefficient using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * AdvectionCoefficient.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * AdvectionCoefficient.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this AdvectionCoefficient.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  AdvectionCoefficient(unsigned int level =
    SpatialExtension::getDefaultLevel(),
                       unsigned int version =
                         SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion =
                         SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AdvectionCoefficient using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  AdvectionCoefficient(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for AdvectionCoefficient.
   *
   * @param orig the AdvectionCoefficient instance to copy.
   */
  AdvectionCoefficient(const AdvectionCoefficient& orig);


  /**
   * Assignment operator for AdvectionCoefficient.
   *
   * @param rhs the AdvectionCoefficient object whose values are to be used as
   * the basis of the assignment.
   */
  AdvectionCoefficient& operator=(const AdvectionCoefficient& rhs);


  /**
   * Creates and returns a deep copy of this AdvectionCoefficient object.
   *
   * @return a (deep) copy of this AdvectionCoefficient object.
   */
  virtual AdvectionCoefficient* clone() const;


  /**
   * Destructor for AdvectionCoefficient.
   */
  virtual ~AdvectionCoefficient();


  /**
   * Returns the value of the "variable" attribute of this
   * AdvectionCoefficient.
   *
   * @return the value of the "variable" attribute of this AdvectionCoefficient
   * as a string.
   */
  const std::string& getVariable() const;


  /**
   * Returns the value of the "coordinate" attribute of this
   * AdvectionCoefficient.
   *
   * @return the value of the "coordinate" attribute of this
   * AdvectionCoefficient as a CoordinateKind_t.
   *
   * @copydetails doc_advectioncoefficient_coordinate
   * @if clike The value is drawn from the enumeration @ref CoordinateKind_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Y, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Z, CoordinateKind_t}
   * @li @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t}
   */
  CoordinateKind_t getCoordinate() const;


  /**
   * Returns the value of the "coordinate" attribute of this
   * AdvectionCoefficient.
   *
   * @return the value of the "coordinate" attribute of this
   * AdvectionCoefficient as a string.
   *
   * @copydetails doc_advectioncoefficient_coordinate
   * The possible values returned by this method are:
   * @li @c "cartesianX"
   * @li @c "cartesianY"
   * @li @c "cartesianZ"
   * @li @c "invalid CoordinateKind value"
   */
  const std::string& getCoordinateAsString() const;


  /**
   * Predicate returning @c true if this AdvectionCoefficient's "variable"
   * attribute is set.
   *
   * @return @c true if this AdvectionCoefficient's "variable" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetVariable() const;


  /**
   * Predicate returning @c true if this AdvectionCoefficient's "coordinate"
   * attribute is set.
   *
   * @return @c true if this AdvectionCoefficient's "coordinate" attribute has
   * been set, otherwise @c false is returned.
   *
   * @copydetails doc_advectioncoefficient_coordinate
   */
  bool isSetCoordinate() const;


  /**
   * Sets the value of the "variable" attribute of this AdvectionCoefficient.
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
   * Sets the value of the "coordinate" attribute of this AdvectionCoefficient.
   *
   * @param coordinate @if clike CoordinateKind_t@else int@endif value of the
   * "coordinate" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_advectioncoefficient_coordinate
   */
  int setCoordinate(const CoordinateKind_t coordinate);


  /**
   * Sets the value of the "coordinate" attribute of this AdvectionCoefficient.
   *
   * @param coordinate std::string& of the "coordinate" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_advectioncoefficient_coordinate
   */
  int setCoordinate(const std::string& coordinate);


  /**
   * Unsets the value of the "variable" attribute of this AdvectionCoefficient.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVariable();


  /**
   * Unsets the value of the "coordinate" attribute of this
   * AdvectionCoefficient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_advectioncoefficient_coordinate
   */
  int unsetCoordinate();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this AdvectionCoefficient object.
   *
   * For AdvectionCoefficient, the XML element name is always
   * @c "advectionCoefficient".
   *
   * @return the name of this element, i.e. @c "advectionCoefficient".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this AdvectionCoefficient object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_ADVECTIONCOEFFICIENT, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * AdvectionCoefficient object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * AdvectionCoefficient have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the AdvectionCoefficient object are:
   * @li "variable"
   * @li "coordinate"
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
   * AdvectionCoefficient.
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
   * AdvectionCoefficient.
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
   * AdvectionCoefficient.
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
   * AdvectionCoefficient.
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
   * AdvectionCoefficient.
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
   * Predicate returning @c true if this AdvectionCoefficient's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this AdvectionCoefficient's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * AdvectionCoefficient.
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
   * AdvectionCoefficient.
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
   * AdvectionCoefficient.
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
   * AdvectionCoefficient.
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
   * AdvectionCoefficient.
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
   * AdvectionCoefficient.
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
 * Creates a new AdvectionCoefficient_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * AdvectionCoefficient_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * AdvectionCoefficient_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this AdvectionCoefficient_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
AdvectionCoefficient_t *
AdvectionCoefficient_create(unsigned int level,
                            unsigned int version,
                            unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this AdvectionCoefficient_t object.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return a (deep) copy of this AdvectionCoefficient_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
AdvectionCoefficient_t*
AdvectionCoefficient_clone(const AdvectionCoefficient_t* ac);


/**
 * Frees this AdvectionCoefficient_t object.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
void
AdvectionCoefficient_free(AdvectionCoefficient_t* ac);


/**
 * Returns the value of the "variable" attribute of this
 * AdvectionCoefficient_t.
 *
 * @param ac the AdvectionCoefficient_t structure whose variable is sought.
 *
 * @return the value of the "variable" attribute of this AdvectionCoefficient_t
 * as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
char *
AdvectionCoefficient_getVariable(const AdvectionCoefficient_t * ac);


/**
 * Returns the value of the "coordinate" attribute of this
 * AdvectionCoefficient_t.
 *
 * @param ac the AdvectionCoefficient_t structure whose coordinate is sought.
 *
 * @return the value of the "coordinate" attribute of this
 * AdvectionCoefficient_t as a CoordinateKind_t.
 *
 * @copydetails doc_advectioncoefficient_coordinate
 * @if clike The value is drawn from the enumeration @ref CoordinateKind_t
 * @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Y, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Z, CoordinateKind_t}
 * @li @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t}
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
CoordinateKind_t
AdvectionCoefficient_getCoordinate(const AdvectionCoefficient_t * ac);


/**
 * Returns the value of the "coordinate" attribute of this
 * AdvectionCoefficient_t.
 *
 * @param ac the AdvectionCoefficient_t structure whose coordinate is sought.
 *
 * @return the value of the "coordinate" attribute of this
 * AdvectionCoefficient_t as a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_advectioncoefficient_coordinate
 * The possible values returned by this method are:
 * @li @c "cartesianX"
 * @li @c "cartesianY"
 * @li @c "cartesianZ"
 * @li @c "invalid CoordinateKind value"
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
char *
AdvectionCoefficient_getCoordinateAsString(const AdvectionCoefficient_t * ac);


/**
 * Predicate returning @c 1 (true) if this AdvectionCoefficient_t's "variable"
 * attribute is set.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return @c 1 (true) if this AdvectionCoefficient_t's "variable" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_isSetVariable(const AdvectionCoefficient_t * ac);


/**
 * Predicate returning @c 1 (true) if this AdvectionCoefficient_t's
 * "coordinate" attribute is set.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return @c 1 (true) if this AdvectionCoefficient_t's "coordinate" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_advectioncoefficient_coordinate
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_isSetCoordinate(const AdvectionCoefficient_t * ac);


/**
 * Sets the value of the "variable" attribute of this AdvectionCoefficient_t.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @param variable const char * value of the "variable" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_setVariable(AdvectionCoefficient_t * ac,
                                 const char * variable);


/**
 * Sets the value of the "coordinate" attribute of this AdvectionCoefficient_t.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @param coordinate CoordinateKind_t value of the "coordinate" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_advectioncoefficient_coordinate
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_setCoordinate(AdvectionCoefficient_t * ac,
                                   CoordinateKind_t coordinate);


/**
 * Sets the value of the "coordinate" attribute of this AdvectionCoefficient_t.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @param coordinate const char * of the "coordinate" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_advectioncoefficient_coordinate
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_setCoordinateAsString(AdvectionCoefficient_t * ac,
                                           const char * coordinate);


/**
 * Unsets the value of the "variable" attribute of this AdvectionCoefficient_t.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_unsetVariable(AdvectionCoefficient_t * ac);


/**
 * Unsets the value of the "coordinate" attribute of this
 * AdvectionCoefficient_t.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_advectioncoefficient_coordinate
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_unsetCoordinate(AdvectionCoefficient_t * ac);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * AdvectionCoefficient_t object have been set.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * AdvectionCoefficient_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the AdvectionCoefficient_t object are:
 * @li "variable"
 * @li "coordinate"
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_hasRequiredAttributes(const AdvectionCoefficient_t * ac);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !AdvectionCoefficient_H__ */


