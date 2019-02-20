/**
 * @file SpatialSymbolReference.h
 * @brief Definition of the SpatialSymbolReference class.
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
 * @class SpatialSymbolReference
 * @sbmlbrief{spatial} TODO:Definition of the SpatialSymbolReference class.
 */


#ifndef SpatialSymbolReference_H__
#define SpatialSymbolReference_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpatialSymbolReference : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mSpatialRef;

  /** @endcond */

public:

  /**
   * Creates a new SpatialSymbolReference using the given SBML Level, Version
   * and &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * SpatialSymbolReference.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * SpatialSymbolReference.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this SpatialSymbolReference.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpatialSymbolReference(
                         unsigned int level =
                           SpatialExtension::getDefaultLevel(),
                         unsigned int version =
                           SpatialExtension::getDefaultVersion(),
                         unsigned int pkgVersion =
                           SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpatialSymbolReference using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpatialSymbolReference(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for SpatialSymbolReference.
   *
   * @param orig the SpatialSymbolReference instance to copy.
   */
  SpatialSymbolReference(const SpatialSymbolReference& orig);


  /**
   * Assignment operator for SpatialSymbolReference.
   *
   * @param rhs the SpatialSymbolReference object whose values are to be used
   * as the basis of the assignment.
   */
  SpatialSymbolReference& operator=(const SpatialSymbolReference& rhs);


  /**
   * Creates and returns a deep copy of this SpatialSymbolReference object.
   *
   * @return a (deep) copy of this SpatialSymbolReference object.
   */
  virtual SpatialSymbolReference* clone() const;


  /**
   * Destructor for SpatialSymbolReference.
   */
  virtual ~SpatialSymbolReference();


  /**
   * Returns the value of the "spatialRef" attribute of this
   * SpatialSymbolReference.
   *
   * @return the value of the "spatialRef" attribute of this
   * SpatialSymbolReference as a string.
   */
  const std::string& getSpatialRef() const;


  /**
   * Predicate returning @c true if this SpatialSymbolReference's "spatialRef"
   * attribute is set.
   *
   * @return @c true if this SpatialSymbolReference's "spatialRef" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetSpatialRef() const;


  /**
   * Sets the value of the "spatialRef" attribute of this
   * SpatialSymbolReference.
   *
   * @param spatialRef std::string& value of the "spatialRef" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSpatialRef(const std::string& spatialRef);


  /**
   * Unsets the value of the "spatialRef" attribute of this
   * SpatialSymbolReference.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSpatialRef();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this SpatialSymbolReference object.
   *
   * For SpatialSymbolReference, the XML element name is always
   * @c "spatialSymbolReference".
   *
   * @return the name of this element, i.e. @c "spatialSymbolReference".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this SpatialSymbolReference object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_SPATIALSYMBOLREFERENCE, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * SpatialSymbolReference object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * SpatialSymbolReference have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the SpatialSymbolReference object are:
   * @li "spatialRef"
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
   * SpatialSymbolReference.
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
   * SpatialSymbolReference.
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
   * SpatialSymbolReference.
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
   * SpatialSymbolReference.
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
   * SpatialSymbolReference.
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
   * Predicate returning @c true if this SpatialSymbolReference's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this SpatialSymbolReference's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * SpatialSymbolReference.
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
   * SpatialSymbolReference.
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
   * SpatialSymbolReference.
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
   * SpatialSymbolReference.
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
   * SpatialSymbolReference.
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
   * SpatialSymbolReference.
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
 * Creates a new SpatialSymbolReference_t using the given SBML Level, Version
 * and &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SpatialSymbolReference_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpatialSymbolReference_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this SpatialSymbolReference_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
SpatialSymbolReference_t *
SpatialSymbolReference_create(unsigned int level,
                              unsigned int version,
                              unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this SpatialSymbolReference_t object.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @return a (deep) copy of this SpatialSymbolReference_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
SpatialSymbolReference_t*
SpatialSymbolReference_clone(const SpatialSymbolReference_t* ssr);


/**
 * Frees this SpatialSymbolReference_t object.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
void
SpatialSymbolReference_free(SpatialSymbolReference_t* ssr);


/**
 * Returns the value of the "spatialRef" attribute of this
 * SpatialSymbolReference_t.
 *
 * @param ssr the SpatialSymbolReference_t structure whose spatialRef is
 * sought.
 *
 * @return the value of the "spatialRef" attribute of this
 * SpatialSymbolReference_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
char *
SpatialSymbolReference_getSpatialRef(const SpatialSymbolReference_t * ssr);


/**
 * Predicate returning @c 1 (true) if this SpatialSymbolReference_t's
 * "spatialRef" attribute is set.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @return @c 1 (true) if this SpatialSymbolReference_t's "spatialRef"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_isSetSpatialRef(const SpatialSymbolReference_t * ssr);


/**
 * Sets the value of the "spatialRef" attribute of this
 * SpatialSymbolReference_t.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @param spatialRef const char * value of the "spatialRef" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_setSpatialRef(SpatialSymbolReference_t * ssr,
                                     const char * spatialRef);


/**
 * Unsets the value of the "spatialRef" attribute of this
 * SpatialSymbolReference_t.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_unsetSpatialRef(SpatialSymbolReference_t * ssr);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SpatialSymbolReference_t object have been set.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SpatialSymbolReference_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the SpatialSymbolReference_t object are:
 * @li "spatialRef"
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_hasRequiredAttributes(const SpatialSymbolReference_t *
  ssr);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !SpatialSymbolReference_H__ */


