/**
 * @file SampledVolume.h
 * @brief Definition of the SampledVolume class.
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
 * @class SampledVolume
 * @sbmlbrief{spatial} TODO:Definition of the SampledVolume class.
 */


#ifndef SampledVolume_H__
#define SampledVolume_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SampledVolume : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mDomainType;
  double mSampledValue;
  bool mIsSetSampledValue;
  double mMinValue;
  bool mIsSetMinValue;
  double mMaxValue;
  bool mIsSetMaxValue;

  /** @endcond */

public:

  /**
   * Creates a new SampledVolume using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * SampledVolume.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * SampledVolume.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this SampledVolume.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SampledVolume(unsigned int level = SpatialExtension::getDefaultLevel(),
                unsigned int version = SpatialExtension::getDefaultVersion(),
                unsigned int pkgVersion =
                  SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SampledVolume using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SampledVolume(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for SampledVolume.
   *
   * @param orig the SampledVolume instance to copy.
   */
  SampledVolume(const SampledVolume& orig);


  /**
   * Assignment operator for SampledVolume.
   *
   * @param rhs the SampledVolume object whose values are to be used as the
   * basis of the assignment.
   */
  SampledVolume& operator=(const SampledVolume& rhs);


  /**
   * Creates and returns a deep copy of this SampledVolume object.
   *
   * @return a (deep) copy of this SampledVolume object.
   */
  virtual SampledVolume* clone() const;


  /**
   * Destructor for SampledVolume.
   */
  virtual ~SampledVolume();


  /**
   * Returns the value of the "id" attribute of this SampledVolume.
   *
   * @return the value of the "id" attribute of this SampledVolume as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this SampledVolume.
   *
   * @return the value of the "name" attribute of this SampledVolume as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "domainType" attribute of this SampledVolume.
   *
   * @return the value of the "domainType" attribute of this SampledVolume as a
   * string.
   */
  const std::string& getDomainType() const;


  /**
   * Returns the value of the "sampledValue" attribute of this SampledVolume.
   *
   * @return the value of the "sampledValue" attribute of this SampledVolume as
   * a double.
   */
  double getSampledValue() const;


  /**
   * Returns the value of the "minValue" attribute of this SampledVolume.
   *
   * @return the value of the "minValue" attribute of this SampledVolume as a
   * double.
   */
  double getMinValue() const;


  /**
   * Returns the value of the "maxValue" attribute of this SampledVolume.
   *
   * @return the value of the "maxValue" attribute of this SampledVolume as a
   * double.
   */
  double getMaxValue() const;


  /**
   * Predicate returning @c true if this SampledVolume's "id" attribute is set.
   *
   * @return @c true if this SampledVolume's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this SampledVolume's "name" attribute is
   * set.
   *
   * @return @c true if this SampledVolume's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this SampledVolume's "domainType" attribute
   * is set.
   *
   * @return @c true if this SampledVolume's "domainType" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetDomainType() const;


  /**
   * Predicate returning @c true if this SampledVolume's "sampledValue"
   * attribute is set.
   *
   * @return @c true if this SampledVolume's "sampledValue" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetSampledValue() const;


  /**
   * Predicate returning @c true if this SampledVolume's "minValue" attribute
   * is set.
   *
   * @return @c true if this SampledVolume's "minValue" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetMinValue() const;


  /**
   * Predicate returning @c true if this SampledVolume's "maxValue" attribute
   * is set.
   *
   * @return @c true if this SampledVolume's "maxValue" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetMaxValue() const;


  /**
   * Sets the value of the "id" attribute of this SampledVolume.
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
   * Sets the value of the "name" attribute of this SampledVolume.
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
   * Sets the value of the "domainType" attribute of this SampledVolume.
   *
   * @param domainType std::string& value of the "domainType" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDomainType(const std::string& domainType);


  /**
   * Sets the value of the "sampledValue" attribute of this SampledVolume.
   *
   * @param sampledValue double value of the "sampledValue" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSampledValue(double sampledValue);


  /**
   * Sets the value of the "minValue" attribute of this SampledVolume.
   *
   * @param minValue double value of the "minValue" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMinValue(double minValue);


  /**
   * Sets the value of the "maxValue" attribute of this SampledVolume.
   *
   * @param maxValue double value of the "maxValue" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMaxValue(double maxValue);


  /**
   * Unsets the value of the "id" attribute of this SampledVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this SampledVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "domainType" attribute of this SampledVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDomainType();


  /**
   * Unsets the value of the "sampledValue" attribute of this SampledVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSampledValue();


  /**
   * Unsets the value of the "minValue" attribute of this SampledVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMinValue();


  /**
   * Unsets the value of the "maxValue" attribute of this SampledVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMaxValue();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this SampledVolume object.
   *
   * For SampledVolume, the XML element name is always @c "sampledVolume".
   *
   * @return the name of this element, i.e. @c "sampledVolume".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this SampledVolume object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_SAMPLEDVOLUME, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * SampledVolume object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * SampledVolume have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the SampledVolume object are:
   * @li "id"
   * @li "domainType"
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
   * Gets the value of the "attributeName" attribute of this SampledVolume.
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
   * Gets the value of the "attributeName" attribute of this SampledVolume.
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
   * Gets the value of the "attributeName" attribute of this SampledVolume.
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
   * Gets the value of the "attributeName" attribute of this SampledVolume.
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
   * Gets the value of the "attributeName" attribute of this SampledVolume.
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
   * Predicate returning @c true if this SampledVolume's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this SampledVolume's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this SampledVolume.
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
   * Sets the value of the "attributeName" attribute of this SampledVolume.
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
   * Sets the value of the "attributeName" attribute of this SampledVolume.
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
   * Sets the value of the "attributeName" attribute of this SampledVolume.
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
   * Sets the value of the "attributeName" attribute of this SampledVolume.
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
   * Unsets the value of the "attributeName" attribute of this SampledVolume.
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
 * Creates a new SampledVolume_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SampledVolume_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SampledVolume_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this SampledVolume_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
SampledVolume_t *
SampledVolume_create(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this SampledVolume_t object.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return a (deep) copy of this SampledVolume_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledVolume_clone(const SampledVolume_t* sv);


/**
 * Frees this SampledVolume_t object.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
void
SampledVolume_free(SampledVolume_t* sv);


/**
 * Returns the value of the "id" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this SampledVolume_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
char *
SampledVolume_getId(const SampledVolume_t * sv);


/**
 * Returns the value of the "name" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this SampledVolume_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
char *
SampledVolume_getName(const SampledVolume_t * sv);


/**
 * Returns the value of the "domainType" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure whose domainType is sought.
 *
 * @return the value of the "domainType" attribute of this SampledVolume_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
char *
SampledVolume_getDomainType(const SampledVolume_t * sv);


/**
 * Returns the value of the "sampledValue" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure whose sampledValue is sought.
 *
 * @return the value of the "sampledValue" attribute of this SampledVolume_t as
 * a double.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
double
SampledVolume_getSampledValue(const SampledVolume_t * sv);


/**
 * Returns the value of the "minValue" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure whose minValue is sought.
 *
 * @return the value of the "minValue" attribute of this SampledVolume_t as a
 * double.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
double
SampledVolume_getMinValue(const SampledVolume_t * sv);


/**
 * Returns the value of the "maxValue" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure whose maxValue is sought.
 *
 * @return the value of the "maxValue" attribute of this SampledVolume_t as a
 * double.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
double
SampledVolume_getMaxValue(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 (true) if this SampledVolume_t's "id" attribute is
 * set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 (true) if this SampledVolume_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetId(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 (true) if this SampledVolume_t's "name" attribute
 * is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 (true) if this SampledVolume_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetName(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 (true) if this SampledVolume_t's "domainType"
 * attribute is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 (true) if this SampledVolume_t's "domainType" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetDomainType(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 (true) if this SampledVolume_t's "sampledValue"
 * attribute is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 (true) if this SampledVolume_t's "sampledValue" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetSampledValue(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 (true) if this SampledVolume_t's "minValue"
 * attribute is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 (true) if this SampledVolume_t's "minValue" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetMinValue(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 (true) if this SampledVolume_t's "maxValue"
 * attribute is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 (true) if this SampledVolume_t's "maxValue" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetMaxValue(const SampledVolume_t * sv);


/**
 * Sets the value of the "id" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling SampledVolume_unsetId().
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setId(SampledVolume_t * sv, const char * id);


/**
 * Sets the value of the "name" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling SampledVolume_unsetName().
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setName(SampledVolume_t * sv, const char * name);


/**
 * Sets the value of the "domainType" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @param domainType const char * value of the "domainType" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setDomainType(SampledVolume_t * sv, const char * domainType);


/**
 * Sets the value of the "sampledValue" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @param sampledValue double value of the "sampledValue" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setSampledValue(SampledVolume_t * sv, double sampledValue);


/**
 * Sets the value of the "minValue" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @param minValue double value of the "minValue" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setMinValue(SampledVolume_t * sv, double minValue);


/**
 * Sets the value of the "maxValue" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @param maxValue double value of the "maxValue" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setMaxValue(SampledVolume_t * sv, double maxValue);


/**
 * Unsets the value of the "id" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetId(SampledVolume_t * sv);


/**
 * Unsets the value of the "name" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetName(SampledVolume_t * sv);


/**
 * Unsets the value of the "domainType" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetDomainType(SampledVolume_t * sv);


/**
 * Unsets the value of the "sampledValue" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetSampledValue(SampledVolume_t * sv);


/**
 * Unsets the value of the "minValue" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetMinValue(SampledVolume_t * sv);


/**
 * Unsets the value of the "maxValue" attribute of this SampledVolume_t.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetMaxValue(SampledVolume_t * sv);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SampledVolume_t object have been set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SampledVolume_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the SampledVolume_t object are:
 * @li "id"
 * @li "domainType"
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_hasRequiredAttributes(const SampledVolume_t * sv);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !SampledVolume_H__ */


