/**
 * @file GeometryDefinition.h
 * @brief Definition of the GeometryDefinition class.
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
 * @class GeometryDefinition
 * @sbmlbrief{spatial} TODO:Definition of the GeometryDefinition class.
 */


#ifndef GeometryDefinition_H__
#define GeometryDefinition_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class AnalyticGeometry;
class SampledFieldGeometry;
class CSGeometry;
class ParametricGeometry;
class MixedGeometry;

class LIBSBML_EXTERN GeometryDefinition : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  bool mIsActive;
  bool mIsSetIsActive;
  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new GeometryDefinition using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * GeometryDefinition.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * GeometryDefinition.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this GeometryDefinition.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GeometryDefinition(unsigned int level = SpatialExtension::getDefaultLevel(),
                     unsigned int version =
                       SpatialExtension::getDefaultVersion(),
                     unsigned int pkgVersion =
                       SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new GeometryDefinition using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GeometryDefinition(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for GeometryDefinition.
   *
   * @param orig the GeometryDefinition instance to copy.
   */
  GeometryDefinition(const GeometryDefinition& orig);


  /**
   * Assignment operator for GeometryDefinition.
   *
   * @param rhs the GeometryDefinition object whose values are to be used as
   * the basis of the assignment.
   */
  GeometryDefinition& operator=(const GeometryDefinition& rhs);


  /**
   * Creates and returns a deep copy of this GeometryDefinition object.
   *
   * @return a (deep) copy of this GeometryDefinition object.
   */
  virtual GeometryDefinition* clone() const;


  /**
   * Destructor for GeometryDefinition.
   */
  virtual ~GeometryDefinition();


  /**
   * Returns the value of the "id" attribute of this GeometryDefinition.
   *
   * @return the value of the "id" attribute of this GeometryDefinition as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this GeometryDefinition.
   *
   * @return the value of the "name" attribute of this GeometryDefinition as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "isActive" attribute of this GeometryDefinition.
   *
   * @return the value of the "isActive" attribute of this GeometryDefinition
   * as a boolean.
   */
  bool getIsActive() const;


  /**
   * Predicate returning @c true if this GeometryDefinition's "id" attribute is
   * set.
   *
   * @return @c true if this GeometryDefinition's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this GeometryDefinition's "name" attribute
   * is set.
   *
   * @return @c true if this GeometryDefinition's "name" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this GeometryDefinition's "isActive"
   * attribute is set.
   *
   * @return @c true if this GeometryDefinition's "isActive" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetIsActive() const;


  /**
   * Sets the value of the "id" attribute of this GeometryDefinition.
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
   * Sets the value of the "name" attribute of this GeometryDefinition.
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
   * Sets the value of the "isActive" attribute of this GeometryDefinition.
   *
   * @param isActive bool value of the "isActive" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setIsActive(bool isActive);


  /**
   * Unsets the value of the "id" attribute of this GeometryDefinition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this GeometryDefinition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "isActive" attribute of this GeometryDefinition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetIsActive();


  /**
   * Predicate returning @c true if this abstract "GeometryDefinition" is of
   * type AnalyticGeometry
   *
   * @return @c true if this abstract "GeometryDefinition" is of type
   * AnalyticGeometry, @c false otherwise
   */
  virtual bool isAnalyticGeometry() const;


  /**
   * Predicate returning @c true if this abstract "GeometryDefinition" is of
   * type SampledFieldGeometry
   *
   * @return @c true if this abstract "GeometryDefinition" is of type
   * SampledFieldGeometry, @c false otherwise
   */
  virtual bool isSampledFieldGeometry() const;


  /**
   * Predicate returning @c true if this abstract "GeometryDefinition" is of
   * type CSGeometry
   *
   * @return @c true if this abstract "GeometryDefinition" is of type
   * CSGeometry, @c false otherwise
   */
  virtual bool isCSGeometry() const;


  /**
   * Predicate returning @c true if this abstract "GeometryDefinition" is of
   * type ParametricGeometry
   *
   * @return @c true if this abstract "GeometryDefinition" is of type
   * ParametricGeometry, @c false otherwise
   */
  virtual bool isParametricGeometry() const;


  /**
   * Predicate returning @c true if this abstract "GeometryDefinition" is of
   * type MixedGeometry
   *
   * @return @c true if this abstract "GeometryDefinition" is of type
   * MixedGeometry, @c false otherwise
   */
  virtual bool isMixedGeometry() const;


  /**
   * Returns the XML element name of this GeometryDefinition object.
   *
   * For GeometryDefinition, the XML element name is always
   * @c "geometryDefinition".
   *
   * @return the name of this element, i.e. @c "geometryDefinition".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this GeometryDefinition object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this GeometryDefinition object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_GEOMETRYDEFINITION, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * GeometryDefinition object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * GeometryDefinition have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the GeometryDefinition object are:
   * @li "id"
   * @li "isActive"
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
   * GeometryDefinition.
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
   * GeometryDefinition.
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
   * GeometryDefinition.
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
   * GeometryDefinition.
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
   * GeometryDefinition.
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
   * Predicate returning @c true if this GeometryDefinition's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this GeometryDefinition's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * GeometryDefinition.
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
   * GeometryDefinition.
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
   * GeometryDefinition.
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
   * GeometryDefinition.
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
   * GeometryDefinition.
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
   * GeometryDefinition.
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
 * Creates a new AnalyticGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GeometryDefinition_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GeometryDefinition_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this GeometryDefinition_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
AnalyticGeometry_t *
GeometryDefinition_createAnalyticGeometry(unsigned int level,
                                          unsigned int version,
                                          unsigned int pkgVersion);


/**
 * Creates a new SampledFieldGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GeometryDefinition_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GeometryDefinition_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this GeometryDefinition_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
SampledFieldGeometry_t *
GeometryDefinition_createSampledFieldGeometry(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion);


/**
 * Creates a new CSGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GeometryDefinition_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GeometryDefinition_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this GeometryDefinition_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
CSGeometry_t *
GeometryDefinition_createCSGeometry(unsigned int level,
                                    unsigned int version,
                                    unsigned int pkgVersion);


/**
 * Creates a new ParametricGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GeometryDefinition_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GeometryDefinition_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this GeometryDefinition_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
ParametricGeometry_t *
GeometryDefinition_createParametricGeometry(unsigned int level,
                                            unsigned int version,
                                            unsigned int pkgVersion);


/**
 * Creates a new MixedGeometry using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GeometryDefinition_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GeometryDefinition_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this GeometryDefinition_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
MixedGeometry_t *
GeometryDefinition_createMixedGeometry(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this GeometryDefinition_t object.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return a (deep) copy of this GeometryDefinition_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
GeometryDefinition_t*
GeometryDefinition_clone(const GeometryDefinition_t* gd);


/**
 * Frees this GeometryDefinition_t object.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
void
GeometryDefinition_free(GeometryDefinition_t* gd);


/**
 * Returns the value of the "id" attribute of this GeometryDefinition_t.
 *
 * @param gd the GeometryDefinition_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this GeometryDefinition_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
char *
GeometryDefinition_getId(const GeometryDefinition_t * gd);


/**
 * Returns the value of the "name" attribute of this GeometryDefinition_t.
 *
 * @param gd the GeometryDefinition_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this GeometryDefinition_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
char *
GeometryDefinition_getName(const GeometryDefinition_t * gd);


/**
 * Returns the value of the "isActive" attribute of this GeometryDefinition_t.
 *
 * @param gd the GeometryDefinition_t structure whose isActive is sought.
 *
 * @return the value of the "isActive" attribute of this GeometryDefinition_t
 * as a boolean.
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_getIsActive(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 (true) if this GeometryDefinition_t's "id"
 * attribute is set.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 (true) if this GeometryDefinition_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSetId(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 (true) if this GeometryDefinition_t's "name"
 * attribute is set.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 (true) if this GeometryDefinition_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSetName(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 (true) if this GeometryDefinition_t's "isActive"
 * attribute is set.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 (true) if this GeometryDefinition_t's "isActive" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSetIsActive(const GeometryDefinition_t * gd);


/**
 * Sets the value of the "id" attribute of this GeometryDefinition_t.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling GeometryDefinition_unsetId().
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_setId(GeometryDefinition_t * gd, const char * id);


/**
 * Sets the value of the "name" attribute of this GeometryDefinition_t.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling GeometryDefinition_unsetName().
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_setName(GeometryDefinition_t * gd, const char * name);


/**
 * Sets the value of the "isActive" attribute of this GeometryDefinition_t.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @param isActive int value of the "isActive" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_setIsActive(GeometryDefinition_t * gd, int isActive);


/**
 * Unsets the value of the "id" attribute of this GeometryDefinition_t.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_unsetId(GeometryDefinition_t * gd);


/**
 * Unsets the value of the "name" attribute of this GeometryDefinition_t.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_unsetName(GeometryDefinition_t * gd);


/**
 * Unsets the value of the "isActive" attribute of this GeometryDefinition_t.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_unsetIsActive(GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * AnalyticGeometry_t
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 if this GeometryDefinition_t is of type AnalyticGeometry_t,
 * @c 0 otherwise
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isAnalyticGeometry(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * SampledFieldGeometry_t
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 if this GeometryDefinition_t is of type SampledFieldGeometry_t,
 * @c 0 otherwise
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSampledFieldGeometry(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * CSGeometry_t
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 if this GeometryDefinition_t is of type CSGeometry_t, @c 0
 * otherwise
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isCSGeometry(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * ParametricGeometry_t
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 if this GeometryDefinition_t is of type ParametricGeometry_t,
 * @c 0 otherwise
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isParametricGeometry(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 if this GeometryDefinition_t is of type
 * MixedGeometry_t
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 if this GeometryDefinition_t is of type MixedGeometry_t, @c 0
 * otherwise
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isMixedGeometry(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GeometryDefinition_t object have been set.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * GeometryDefinition_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the GeometryDefinition_t object are:
 * @li "id"
 * @li "isActive"
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_hasRequiredAttributes(const GeometryDefinition_t * gd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !GeometryDefinition_H__ */


