/**
 * @file CompartmentMapping.h
 * @brief Definition of the CompartmentMapping class.
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
 * @class CompartmentMapping
 * @sbmlbrief{spatial} TODO:Definition of the CompartmentMapping class.
 */


#ifndef CompartmentMapping_H__
#define CompartmentMapping_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CompartmentMapping : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mDomainType;
  double mUnitSize;
  bool mIsSetUnitSize;

  /** @endcond */

public:

  /**
   * Creates a new CompartmentMapping using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CompartmentMapping.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CompartmentMapping.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CompartmentMapping.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CompartmentMapping(unsigned int level = SpatialExtension::getDefaultLevel(),
                     unsigned int version =
                       SpatialExtension::getDefaultVersion(),
                     unsigned int pkgVersion =
                       SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CompartmentMapping using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CompartmentMapping(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CompartmentMapping.
   *
   * @param orig the CompartmentMapping instance to copy.
   */
  CompartmentMapping(const CompartmentMapping& orig);


  /**
   * Assignment operator for CompartmentMapping.
   *
   * @param rhs the CompartmentMapping object whose values are to be used as
   * the basis of the assignment.
   */
  CompartmentMapping& operator=(const CompartmentMapping& rhs);


  /**
   * Creates and returns a deep copy of this CompartmentMapping object.
   *
   * @return a (deep) copy of this CompartmentMapping object.
   */
  virtual CompartmentMapping* clone() const;


  /**
   * Destructor for CompartmentMapping.
   */
  virtual ~CompartmentMapping();


  /**
   * Returns the value of the "id" attribute of this CompartmentMapping.
   *
   * @return the value of the "id" attribute of this CompartmentMapping as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this CompartmentMapping.
   *
   * @return the value of the "name" attribute of this CompartmentMapping as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "domainType" attribute of this
   * CompartmentMapping.
   *
   * @return the value of the "domainType" attribute of this CompartmentMapping
   * as a string.
   */
  const std::string& getDomainType() const;


  /**
   * Returns the value of the "unitSize" attribute of this CompartmentMapping.
   *
   * @return the value of the "unitSize" attribute of this CompartmentMapping
   * as a double.
   */
  double getUnitSize() const;


  /**
   * Predicate returning @c true if this CompartmentMapping's "id" attribute is
   * set.
   *
   * @return @c true if this CompartmentMapping's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this CompartmentMapping's "name" attribute
   * is set.
   *
   * @return @c true if this CompartmentMapping's "name" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this CompartmentMapping's "domainType"
   * attribute is set.
   *
   * @return @c true if this CompartmentMapping's "domainType" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetDomainType() const;


  /**
   * Predicate returning @c true if this CompartmentMapping's "unitSize"
   * attribute is set.
   *
   * @return @c true if this CompartmentMapping's "unitSize" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetUnitSize() const;


  /**
   * Sets the value of the "id" attribute of this CompartmentMapping.
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
   * Sets the value of the "name" attribute of this CompartmentMapping.
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
   * Sets the value of the "domainType" attribute of this CompartmentMapping.
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
   * Sets the value of the "unitSize" attribute of this CompartmentMapping.
   *
   * @param unitSize double value of the "unitSize" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setUnitSize(double unitSize);


  /**
   * Unsets the value of the "id" attribute of this CompartmentMapping.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this CompartmentMapping.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "domainType" attribute of this CompartmentMapping.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDomainType();


  /**
   * Unsets the value of the "unitSize" attribute of this CompartmentMapping.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetUnitSize();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this CompartmentMapping object.
   *
   * For CompartmentMapping, the XML element name is always
   * @c "compartmentMapping".
   *
   * @return the name of this element, i.e. @c "compartmentMapping".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CompartmentMapping object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_COMPARTMENTMAPPING, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CompartmentMapping object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CompartmentMapping have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CompartmentMapping object are:
   * @li "id"
   * @li "domainType"
   * @li "unitSize"
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
   * CompartmentMapping.
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
   * CompartmentMapping.
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
   * CompartmentMapping.
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
   * CompartmentMapping.
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
   * CompartmentMapping.
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
   * Predicate returning @c true if this CompartmentMapping's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CompartmentMapping's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * CompartmentMapping.
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
   * CompartmentMapping.
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
   * CompartmentMapping.
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
   * CompartmentMapping.
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
   * CompartmentMapping.
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
   * CompartmentMapping.
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
 * Creates a new CompartmentMapping_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CompartmentMapping_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CompartmentMapping_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CompartmentMapping_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
CompartmentMapping_t *
CompartmentMapping_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CompartmentMapping_t object.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return a (deep) copy of this CompartmentMapping_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
CompartmentMapping_t*
CompartmentMapping_clone(const CompartmentMapping_t* cm);


/**
 * Frees this CompartmentMapping_t object.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
void
CompartmentMapping_free(CompartmentMapping_t* cm);


/**
 * Returns the value of the "id" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this CompartmentMapping_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
char *
CompartmentMapping_getId(const CompartmentMapping_t * cm);


/**
 * Returns the value of the "name" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this CompartmentMapping_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
char *
CompartmentMapping_getName(const CompartmentMapping_t * cm);


/**
 * Returns the value of the "domainType" attribute of this
 * CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure whose domainType is sought.
 *
 * @return the value of the "domainType" attribute of this CompartmentMapping_t
 * as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
char *
CompartmentMapping_getDomainType(const CompartmentMapping_t * cm);


/**
 * Returns the value of the "unitSize" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure whose unitSize is sought.
 *
 * @return the value of the "unitSize" attribute of this CompartmentMapping_t
 * as a double.
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
double
CompartmentMapping_getUnitSize(const CompartmentMapping_t * cm);


/**
 * Predicate returning @c 1 (true) if this CompartmentMapping_t's "id"
 * attribute is set.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return @c 1 (true) if this CompartmentMapping_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetId(const CompartmentMapping_t * cm);


/**
 * Predicate returning @c 1 (true) if this CompartmentMapping_t's "name"
 * attribute is set.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return @c 1 (true) if this CompartmentMapping_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetName(const CompartmentMapping_t * cm);


/**
 * Predicate returning @c 1 (true) if this CompartmentMapping_t's "domainType"
 * attribute is set.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return @c 1 (true) if this CompartmentMapping_t's "domainType" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetDomainType(const CompartmentMapping_t * cm);


/**
 * Predicate returning @c 1 (true) if this CompartmentMapping_t's "unitSize"
 * attribute is set.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return @c 1 (true) if this CompartmentMapping_t's "unitSize" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetUnitSize(const CompartmentMapping_t * cm);


/**
 * Sets the value of the "id" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling CompartmentMapping_unsetId().
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_setId(CompartmentMapping_t * cm, const char * id);


/**
 * Sets the value of the "name" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling CompartmentMapping_unsetName().
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_setName(CompartmentMapping_t * cm, const char * name);


/**
 * Sets the value of the "domainType" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @param domainType const char * value of the "domainType" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_setDomainType(CompartmentMapping_t * cm,
                                 const char * domainType);


/**
 * Sets the value of the "unitSize" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @param unitSize double value of the "unitSize" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_setUnitSize(CompartmentMapping_t * cm, double unitSize);


/**
 * Unsets the value of the "id" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetId(CompartmentMapping_t * cm);


/**
 * Unsets the value of the "name" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetName(CompartmentMapping_t * cm);


/**
 * Unsets the value of the "domainType" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetDomainType(CompartmentMapping_t * cm);


/**
 * Unsets the value of the "unitSize" attribute of this CompartmentMapping_t.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetUnitSize(CompartmentMapping_t * cm);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CompartmentMapping_t object have been set.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CompartmentMapping_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the CompartmentMapping_t object are:
 * @li "id"
 * @li "domainType"
 * @li "unitSize"
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_hasRequiredAttributes(const CompartmentMapping_t * cm);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CompartmentMapping_H__ */


