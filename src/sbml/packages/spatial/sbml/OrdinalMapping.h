/**
 * @file OrdinalMapping.h
 * @brief Definition of the OrdinalMapping class.
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
 * @class OrdinalMapping
 * @sbmlbrief{spatial} TODO:Definition of the OrdinalMapping class.
 */


#ifndef OrdinalMapping_H__
#define OrdinalMapping_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN OrdinalMapping : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mGeometryDefinition;
  int mOrdinal;
  bool mIsSetOrdinal;

  /** @endcond */

public:

  /**
   * Creates a new OrdinalMapping using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * OrdinalMapping.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * OrdinalMapping.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this OrdinalMapping.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  OrdinalMapping(unsigned int level = SpatialExtension::getDefaultLevel(),
                 unsigned int version = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion =
                   SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new OrdinalMapping using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  OrdinalMapping(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for OrdinalMapping.
   *
   * @param orig the OrdinalMapping instance to copy.
   */
  OrdinalMapping(const OrdinalMapping& orig);


  /**
   * Assignment operator for OrdinalMapping.
   *
   * @param rhs the OrdinalMapping object whose values are to be used as the
   * basis of the assignment.
   */
  OrdinalMapping& operator=(const OrdinalMapping& rhs);


  /**
   * Creates and returns a deep copy of this OrdinalMapping object.
   *
   * @return a (deep) copy of this OrdinalMapping object.
   */
  virtual OrdinalMapping* clone() const;


  /**
   * Destructor for OrdinalMapping.
   */
  virtual ~OrdinalMapping();


  /**
   * Returns the value of the "geometryDefinition" attribute of this
   * OrdinalMapping.
   *
   * @return the value of the "geometryDefinition" attribute of this
   * OrdinalMapping as a string.
   */
  const std::string& getGeometryDefinition() const;


  /**
   * Returns the value of the "ordinal" attribute of this OrdinalMapping.
   *
   * @return the value of the "ordinal" attribute of this OrdinalMapping as a
   * integer.
   */
  int getOrdinal() const;


  /**
   * Predicate returning @c true if this OrdinalMapping's "geometryDefinition"
   * attribute is set.
   *
   * @return @c true if this OrdinalMapping's "geometryDefinition" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetGeometryDefinition() const;


  /**
   * Predicate returning @c true if this OrdinalMapping's "ordinal" attribute
   * is set.
   *
   * @return @c true if this OrdinalMapping's "ordinal" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetOrdinal() const;


  /**
   * Sets the value of the "geometryDefinition" attribute of this
   * OrdinalMapping.
   *
   * @param geometryDefinition std::string& value of the "geometryDefinition"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setGeometryDefinition(const std::string& geometryDefinition);


  /**
   * Sets the value of the "ordinal" attribute of this OrdinalMapping.
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
   * Unsets the value of the "geometryDefinition" attribute of this
   * OrdinalMapping.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetGeometryDefinition();


  /**
   * Unsets the value of the "ordinal" attribute of this OrdinalMapping.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetOrdinal();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this OrdinalMapping object.
   *
   * For OrdinalMapping, the XML element name is always @c "ordinalMapping".
   *
   * @return the name of this element, i.e. @c "ordinalMapping".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this OrdinalMapping object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_ORDINALMAPPING, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * OrdinalMapping object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * OrdinalMapping have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the OrdinalMapping object are:
   * @li "geometryDefinition"
   * @li "ordinal"
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
   * Gets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Gets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Gets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Gets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Gets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Predicate returning @c true if this OrdinalMapping's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this OrdinalMapping's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Sets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Sets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Sets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Sets the value of the "attributeName" attribute of this OrdinalMapping.
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
   * Unsets the value of the "attributeName" attribute of this OrdinalMapping.
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
 * Creates a new OrdinalMapping_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * OrdinalMapping_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * OrdinalMapping_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this OrdinalMapping_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
OrdinalMapping_t *
OrdinalMapping_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this OrdinalMapping_t object.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return a (deep) copy of this OrdinalMapping_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
OrdinalMapping_t*
OrdinalMapping_clone(const OrdinalMapping_t* om);


/**
 * Frees this OrdinalMapping_t object.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
void
OrdinalMapping_free(OrdinalMapping_t* om);


/**
 * Returns the value of the "geometryDefinition" attribute of this
 * OrdinalMapping_t.
 *
 * @param om the OrdinalMapping_t structure whose geometryDefinition is sought.
 *
 * @return the value of the "geometryDefinition" attribute of this
 * OrdinalMapping_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
char *
OrdinalMapping_getGeometryDefinition(const OrdinalMapping_t * om);


/**
 * Returns the value of the "ordinal" attribute of this OrdinalMapping_t.
 *
 * @param om the OrdinalMapping_t structure whose ordinal is sought.
 *
 * @return the value of the "ordinal" attribute of this OrdinalMapping_t as a
 * integer.
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_getOrdinal(const OrdinalMapping_t * om);


/**
 * Predicate returning @c 1 (true) if this OrdinalMapping_t's
 * "geometryDefinition" attribute is set.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return @c 1 (true) if this OrdinalMapping_t's "geometryDefinition"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_isSetGeometryDefinition(const OrdinalMapping_t * om);


/**
 * Predicate returning @c 1 (true) if this OrdinalMapping_t's "ordinal"
 * attribute is set.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return @c 1 (true) if this OrdinalMapping_t's "ordinal" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_isSetOrdinal(const OrdinalMapping_t * om);


/**
 * Sets the value of the "geometryDefinition" attribute of this
 * OrdinalMapping_t.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @param geometryDefinition const char * value of the "geometryDefinition"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_setGeometryDefinition(OrdinalMapping_t * om,
                                     const char * geometryDefinition);


/**
 * Sets the value of the "ordinal" attribute of this OrdinalMapping_t.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @param ordinal int value of the "ordinal" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_setOrdinal(OrdinalMapping_t * om, int ordinal);


/**
 * Unsets the value of the "geometryDefinition" attribute of this
 * OrdinalMapping_t.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_unsetGeometryDefinition(OrdinalMapping_t * om);


/**
 * Unsets the value of the "ordinal" attribute of this OrdinalMapping_t.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_unsetOrdinal(OrdinalMapping_t * om);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * OrdinalMapping_t object have been set.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * OrdinalMapping_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the OrdinalMapping_t object are:
 * @li "geometryDefinition"
 * @li "ordinal"
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_hasRequiredAttributes(const OrdinalMapping_t * om);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !OrdinalMapping_H__ */


