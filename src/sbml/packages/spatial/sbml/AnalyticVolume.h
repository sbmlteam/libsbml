/**
 * @file AnalyticVolume.h
 * @brief Definition of the AnalyticVolume class.
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
 * @class AnalyticVolume
 * @sbmlbrief{spatial} TODO:Definition of the AnalyticVolume class.
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
 * @class doc_analyticvolume_functionType
 *
 * @par
 * The attribute "functionType" on a AnalyticVolume object is used to TODO:add
 * explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Spatial specification, the following are the
 * allowable values for "functionType":
 * <ul>
 * <li> @c "layered", TODO:add description
 *
 * </ul>
 */


#ifndef AnalyticVolume_H__
#define AnalyticVolume_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/math/ASTNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN AnalyticVolume : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  FunctionKind_t mFunctionType;
  int mOrdinal;
  bool mIsSetOrdinal;
  std::string mDomainType;
  ASTNode* mMath;

  /** @endcond */

public:

  /**
   * Creates a new AnalyticVolume using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * AnalyticVolume.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * AnalyticVolume.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this AnalyticVolume.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  AnalyticVolume(unsigned int level = SpatialExtension::getDefaultLevel(),
                 unsigned int version = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion =
                   SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AnalyticVolume using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  AnalyticVolume(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for AnalyticVolume.
   *
   * @param orig the AnalyticVolume instance to copy.
   */
  AnalyticVolume(const AnalyticVolume& orig);


  /**
   * Assignment operator for AnalyticVolume.
   *
   * @param rhs the AnalyticVolume object whose values are to be used as the
   * basis of the assignment.
   */
  AnalyticVolume& operator=(const AnalyticVolume& rhs);


  /**
   * Creates and returns a deep copy of this AnalyticVolume object.
   *
   * @return a (deep) copy of this AnalyticVolume object.
   */
  virtual AnalyticVolume* clone() const;


  /**
   * Destructor for AnalyticVolume.
   */
  virtual ~AnalyticVolume();


  /**
   * Returns the value of the "id" attribute of this AnalyticVolume.
   *
   * @return the value of the "id" attribute of this AnalyticVolume as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this AnalyticVolume.
   *
   * @return the value of the "name" attribute of this AnalyticVolume as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @return the value of the "functionType" attribute of this AnalyticVolume
   * as a FunctionKind_t.
   *
   * @copydetails doc_analyticvolume_functionType
   * @if clike The value is drawn from the enumeration @ref FunctionKind_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{SPATIAL_FUNCTIONKIND_LAYERED, FunctionKind_t}
   * @li @sbmlconstant{SPATIAL_FUNCTIONKIND_INVALID, FunctionKind_t}
   */
  FunctionKind_t getFunctionType() const;


  /**
   * Returns the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @return the value of the "functionType" attribute of this AnalyticVolume
   * as a string.
   *
   * @copydetails doc_analyticvolume_functionType
   * The possible values returned by this method are:
   * @li @c "layered"
   * @li @c "invalid FunctionKind value"
   */
  const std::string& getFunctionTypeAsString() const;


  /**
   * Returns the value of the "ordinal" attribute of this AnalyticVolume.
   *
   * @return the value of the "ordinal" attribute of this AnalyticVolume as a
   * integer.
   */
  int getOrdinal() const;


  /**
   * Returns the value of the "domainType" attribute of this AnalyticVolume.
   *
   * @return the value of the "domainType" attribute of this AnalyticVolume as
   * a string.
   */
  const std::string& getDomainType() const;


  /**
   * Predicate returning @c true if this AnalyticVolume's "id" attribute is
   * set.
   *
   * @return @c true if this AnalyticVolume's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this AnalyticVolume's "name" attribute is
   * set.
   *
   * @return @c true if this AnalyticVolume's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this AnalyticVolume's "functionType"
   * attribute is set.
   *
   * @return @c true if this AnalyticVolume's "functionType" attribute has been
   * set, otherwise @c false is returned.
   *
   * @copydetails doc_analyticvolume_functionType
   */
  bool isSetFunctionType() const;


  /**
   * Predicate returning @c true if this AnalyticVolume's "ordinal" attribute
   * is set.
   *
   * @return @c true if this AnalyticVolume's "ordinal" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetOrdinal() const;


  /**
   * Predicate returning @c true if this AnalyticVolume's "domainType"
   * attribute is set.
   *
   * @return @c true if this AnalyticVolume's "domainType" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetDomainType() const;


  /**
   * Sets the value of the "id" attribute of this AnalyticVolume.
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
   * Sets the value of the "name" attribute of this AnalyticVolume.
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
   * Sets the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @param functionType @if clike FunctionKind_t@else int@endif value of the
   * "functionType" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_analyticvolume_functionType
   */
  int setFunctionType(const FunctionKind_t functionType);


  /**
   * Sets the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @param functionType std::string& of the "functionType" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_analyticvolume_functionType
   */
  int setFunctionType(const std::string& functionType);


  /**
   * Sets the value of the "ordinal" attribute of this AnalyticVolume.
   *
   * @param ordinal int value of the "ordinal" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setOrdinal(int ordinal);


  /**
   * Sets the value of the "domainType" attribute of this AnalyticVolume.
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
   * Unsets the value of the "id" attribute of this AnalyticVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this AnalyticVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_analyticvolume_functionType
   */
  int unsetFunctionType();


  /**
   * Unsets the value of the "ordinal" attribute of this AnalyticVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetOrdinal();


  /**
   * Unsets the value of the "domainType" attribute of this AnalyticVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDomainType();


  /**
   * Returns the value of the "math" element of this AnalyticVolume.
   *
   * @return the value of the "math" element of this AnalyticVolume as a
   * ASTNode*.
   */
  const ASTNode* getMath() const;


  /**
   * Returns the value of the "math" element of this AnalyticVolume.
   *
   * @return the value of the "math" element of this AnalyticVolume as a
   * ASTNode*.
   */
  ASTNode* getMath();


  /**
   * Predicate returning @c true if this AnalyticVolume's "math" element is
   * set.
   *
   * @return @c true if this AnalyticVolume's "math" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetMath() const;


  /**
   * Sets the value of the "math" element of this AnalyticVolume.
   *
   * @param math ASTNode* value of the "math" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMath(const ASTNode* math);


  /**
   * Unsets the value of the "math" element of this AnalyticVolume.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMath();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this AnalyticVolume object.
   *
   * For AnalyticVolume, the XML element name is always @c "analyticVolume".
   *
   * @return the name of this element, i.e. @c "analyticVolume".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this AnalyticVolume object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_ANALYTICVOLUME, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * AnalyticVolume object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * AnalyticVolume have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the AnalyticVolume object are:
   * @li "id"
   * @li "functionType"
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
   * Gets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Gets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Gets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Gets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Gets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Predicate returning @c true if this AnalyticVolume's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this AnalyticVolume's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Sets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Sets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Sets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Sets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Unsets the value of the "attributeName" attribute of this AnalyticVolume.
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
   * Reads other XML such as math/notes etc.
   */
  virtual bool readOtherXML(XMLInputStream& stream);

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
 * Creates a new AnalyticVolume_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * AnalyticVolume_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * AnalyticVolume_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this AnalyticVolume_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticVolume_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this AnalyticVolume_t object.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return a (deep) copy of this AnalyticVolume_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticVolume_clone(const AnalyticVolume_t* av);


/**
 * Frees this AnalyticVolume_t object.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
void
AnalyticVolume_free(AnalyticVolume_t* av);


/**
 * Returns the value of the "id" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this AnalyticVolume_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
char *
AnalyticVolume_getId(const AnalyticVolume_t * av);


/**
 * Returns the value of the "name" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this AnalyticVolume_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
char *
AnalyticVolume_getName(const AnalyticVolume_t * av);


/**
 * Returns the value of the "functionType" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure whose functionType is sought.
 *
 * @return the value of the "functionType" attribute of this AnalyticVolume_t
 * as a FunctionKind_t.
 *
 * @copydetails doc_analyticvolume_functionType
 * @if clike The value is drawn from the enumeration @ref FunctionKind_t @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{SPATIAL_FUNCTIONKIND_LAYERED, FunctionKind_t}
 * @li @sbmlconstant{SPATIAL_FUNCTIONKIND_INVALID, FunctionKind_t}
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
FunctionKind_t
AnalyticVolume_getFunctionType(const AnalyticVolume_t * av);


/**
 * Returns the value of the "functionType" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure whose functionType is sought.
 *
 * @return the value of the "functionType" attribute of this AnalyticVolume_t
 * as a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_analyticvolume_functionType
 * The possible values returned by this method are:
 * @li @c "layered"
 * @li @c "invalid FunctionKind value"
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
char *
AnalyticVolume_getFunctionTypeAsString(const AnalyticVolume_t * av);


/**
 * Returns the value of the "ordinal" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure whose ordinal is sought.
 *
 * @return the value of the "ordinal" attribute of this AnalyticVolume_t as a
 * integer.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_getOrdinal(const AnalyticVolume_t * av);


/**
 * Returns the value of the "domainType" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure whose domainType is sought.
 *
 * @return the value of the "domainType" attribute of this AnalyticVolume_t as
 * a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
char *
AnalyticVolume_getDomainType(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 (true) if this AnalyticVolume_t's "id" attribute is
 * set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 (true) if this AnalyticVolume_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetId(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 (true) if this AnalyticVolume_t's "name" attribute
 * is set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 (true) if this AnalyticVolume_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetName(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 (true) if this AnalyticVolume_t's "functionType"
 * attribute is set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 (true) if this AnalyticVolume_t's "functionType" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_analyticvolume_functionType
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetFunctionType(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 (true) if this AnalyticVolume_t's "ordinal"
 * attribute is set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 (true) if this AnalyticVolume_t's "ordinal" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetOrdinal(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 (true) if this AnalyticVolume_t's "domainType"
 * attribute is set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 (true) if this AnalyticVolume_t's "domainType" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetDomainType(const AnalyticVolume_t * av);


/**
 * Sets the value of the "id" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling AnalyticVolume_unsetId().
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setId(AnalyticVolume_t * av, const char * id);


/**
 * Sets the value of the "name" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling AnalyticVolume_unsetName().
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setName(AnalyticVolume_t * av, const char * name);


/**
 * Sets the value of the "functionType" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param functionType FunctionKind_t value of the "functionType" attribute to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_analyticvolume_functionType
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setFunctionType(AnalyticVolume_t * av,
                               FunctionKind_t functionType);


/**
 * Sets the value of the "functionType" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param functionType const char * of the "functionType" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_analyticvolume_functionType
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setFunctionTypeAsString(AnalyticVolume_t * av,
                                       const char * functionType);


/**
 * Sets the value of the "ordinal" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param ordinal int value of the "ordinal" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setOrdinal(AnalyticVolume_t * av, int ordinal);


/**
 * Sets the value of the "domainType" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param domainType const char * value of the "domainType" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setDomainType(AnalyticVolume_t * av, const char * domainType);


/**
 * Unsets the value of the "id" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetId(AnalyticVolume_t * av);


/**
 * Unsets the value of the "name" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetName(AnalyticVolume_t * av);


/**
 * Unsets the value of the "functionType" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_analyticvolume_functionType
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetFunctionType(AnalyticVolume_t * av);


/**
 * Unsets the value of the "ordinal" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetOrdinal(AnalyticVolume_t * av);


/**
 * Unsets the value of the "domainType" attribute of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetDomainType(AnalyticVolume_t * av);


/**
 * Returns the value of the "math" element of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure whose math is sought.
 *
 * @return the value of the "math" element of this AnalyticVolume_t as a
 * pointer to an ASTNode_t object.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
const ASTNode_t*
AnalyticVolume_getMath(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 (true) if this AnalyticVolume_t's "math" element is
 * set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 (true) if this AnalyticVolume_t's "math" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetMath(const AnalyticVolume_t * av);


/**
 * Sets the value of the "math" element of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param math ASTNode_t * pointer to the "math" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setMath(AnalyticVolume_t * av, const ASTNode_t* math);


/**
 * Unsets the value of the "math" element of this AnalyticVolume_t.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetMath(AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * AnalyticVolume_t object have been set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * AnalyticVolume_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the AnalyticVolume_t object are:
 * @li "id"
 * @li "functionType"
 * @li "domainType"
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_hasRequiredAttributes(const AnalyticVolume_t * av);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !AnalyticVolume_H__ */


