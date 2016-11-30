/**
 * @file CSGPseudoPrimitive.h
 * @brief Definition of the CSGPseudoPrimitive class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
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
 * @class CSGPseudoPrimitive
 * @sbmlbrief{spatial} TODO:Definition of the CSGPseudoPrimitive class.
 */


#ifndef CSGPseudoPrimitive_H__
#define CSGPseudoPrimitive_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/CSGNode.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGPseudoPrimitive : public CSGNode
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mCsgObjectRef;

  /** @endcond */

public:

  /**
   * Creates a new CSGPseudoPrimitive using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CSGPseudoPrimitive.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGPseudoPrimitive.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGPseudoPrimitive.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  CSGPseudoPrimitive(unsigned int level = SpatialExtension::getDefaultLevel(),
                     unsigned int version =
                       SpatialExtension::getDefaultVersion(),
                     unsigned int pkgVersion =
                       SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGPseudoPrimitive using the given SpatialPkgNamespaces
   * object.
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  CSGPseudoPrimitive(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGPseudoPrimitive.
   *
   * @param orig the CSGPseudoPrimitive instance to copy.
   */
  CSGPseudoPrimitive(const CSGPseudoPrimitive& orig);


  /**
   * Assignment operator for CSGPseudoPrimitive.
   *
   * @param rhs the CSGPseudoPrimitive object whose values are to be used as
   * the basis of the assignment.
   */
  CSGPseudoPrimitive& operator=(const CSGPseudoPrimitive& rhs);


  /**
   * Creates and returns a deep copy of this CSGPseudoPrimitive object.
   *
   * @return a (deep) copy of this CSGPseudoPrimitive object.
   */
  virtual CSGPseudoPrimitive* clone() const;


  /**
   * Destructor for CSGPseudoPrimitive.
   */
  virtual ~CSGPseudoPrimitive();


  /**
   * Returns the value of the "csgObjectRef" attribute of this
   * CSGPseudoPrimitive.
   *
   * @return the value of the "csgObjectRef" attribute of this
   * CSGPseudoPrimitive as a string.
   */
  const std::string& getCsgObjectRef() const;


  /**
   * Predicate returning @c true if this CSGPseudoPrimitive's "csgObjectRef"
   * attribute is set.
   *
   * @return @c true if this CSGPseudoPrimitive's "csgObjectRef" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetCsgObjectRef() const;


  /**
   * Sets the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
   *
   * @param csgObjectRef std::string& value of the "csgObjectRef" attribute to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCsgObjectRef(const std::string& csgObjectRef);


  /**
   * Unsets the value of the "csgObjectRef" attribute of this
   * CSGPseudoPrimitive.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCsgObjectRef();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this CSGPseudoPrimitive object.
   *
   * For CSGPseudoPrimitive, the XML element name is always @c
   * "csgPseudoPrimitive".
   *
   * @return the name of this element, i.e. @c "csgPseudoPrimitive".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CSGPseudoPrimitive object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   *
   * @sbmlconstant{SBML_SPATIAL_CSGPSEUDOPRIMITIVE, SBMLSpatialTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGPseudoPrimitive object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGPseudoPrimitive have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CSGPseudoPrimitive object are:
   * @li "csgObjectRef"
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
   * CSGPseudoPrimitive.
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
   * CSGPseudoPrimitive.
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
   * CSGPseudoPrimitive.
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
   * CSGPseudoPrimitive.
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
   * CSGPseudoPrimitive.
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
   * Gets the value of the "attributeName" attribute of this
   * CSGPseudoPrimitive.
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
                           const char* value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this CSGPseudoPrimitive's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGPseudoPrimitive's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * CSGPseudoPrimitive.
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
   * CSGPseudoPrimitive.
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
   * CSGPseudoPrimitive.
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
   * CSGPseudoPrimitive.
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
   * CSGPseudoPrimitive.
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
   * Sets the value of the "attributeName" attribute of this
   * CSGPseudoPrimitive.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, const char*
    value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this
   * CSGPseudoPrimitive.
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
 * Creates a new CSGPseudoPrimitive_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CSGPseudoPrimitive_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGPseudoPrimitive_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGPseudoPrimitive_t.
 *
 * @throws SBMLConstructorException
 * Thrown if the given @p level and @p version combination, or this kind of
 * SBML object, are either invalid or mismatched with respect to the parent
 * SBMLDocument object.
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGPseudoPrimitive_create(
                          unsigned int level =
                            SpatialExtension::getDefaultLevel(),
                          unsigned int version =
                            SpatialExtension::getDefaultVersion(),
                          unsigned int pkgVersion =
                            SpatialExtension::getDefaultPackageVersion());


/**
 * Creates and returns a deep copy of this CSGPseudoPrimitive_t object.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @return a (deep) copy of this CSGPseudoPrimitive_t object.
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
CSGPseudoPrimitive_t*
CSGPseudoPrimitive_clone(const CSGPseudoPrimitive_t* csgpp);


/**
 * Frees this CSGPseudoPrimitive_t object.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
void
CSGPseudoPrimitive_free(CSGPseudoPrimitive_t* csgpp);


/**
 * Returns the value of the "csgObjectRef" attribute of this
 * CSGPseudoPrimitive_t.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure whose csgObjectRef is
 * sought.
 *
 * @return the value of the "csgObjectRef" attribute of this
 * CSGPseudoPrimitive_t as a pointer to a string.
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
const char *
CSGPseudoPrimitive_getCsgObjectRef(const CSGPseudoPrimitive_t * csgpp);


/**
 * Predicate returning @c 1 if this CSGPseudoPrimitive_t's "csgObjectRef"
 * attribute is set.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @return @c 1 if this CSGPseudoPrimitive_t's "csgObjectRef" attribute has
 * been set, otherwise @c 0 is returned.
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_isSetCsgObjectRef(const CSGPseudoPrimitive_t * csgpp);


/**
 * Sets the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive_t.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @param csgObjectRef const char * value of the "csgObjectRef" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_setCsgObjectRef(CSGPseudoPrimitive_t * csgpp,
                                   const char * csgObjectRef);


/**
 * Unsets the value of the "csgObjectRef" attribute of this
 * CSGPseudoPrimitive_t.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_unsetCsgObjectRef(CSGPseudoPrimitive_t * csgpp);


/**
 * Predicate returning @c 1 if all the required attributes for this
 * CSGPseudoPrimitive_t object have been set.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @return @c 1 to indicate that all the required attributes of this
 * CSGPseudoPrimitive_t have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required attributes for the CSGPseudoPrimitive_t object are:
 * @li "csgObjectRef"
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_hasRequiredAttributes(const CSGPseudoPrimitive_t * csgpp);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGPseudoPrimitive_H__ */


