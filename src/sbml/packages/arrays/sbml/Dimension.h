/**
 * @file Dimension.h
 * @brief Definition of the Dimension class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
 * @class Dimension
 * @sbmlbrief{arrays} TODO:Definition of the Dimension class.
 */


#ifndef Dimension_H__
#define Dimension_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/arrays/common/arraysfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Dimension : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mSize;
  unsigned int mArrayDimension;
  bool mIsSetArrayDimension;

  /** @endcond */

public:

  /**
   * Creates a new Dimension using the given SBML Level, Version and
   * &ldquo;arrays&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Dimension.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Dimension.
   *
   * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to
   * this Dimension.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  Dimension(unsigned int level = ArraysExtension::getDefaultLevel(),
            unsigned int version = ArraysExtension::getDefaultVersion(),
            unsigned int pkgVersion =
              ArraysExtension::getDefaultPackageVersion());


  /**
   * Creates a new Dimension using the given ArraysPkgNamespaces object.
   *
   * @param arraysns the ArraysPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  Dimension(ArraysPkgNamespaces *arraysns);


  /**
   * Copy constructor for Dimension.
   *
   * @param orig the Dimension instance to copy.
   */
  Dimension(const Dimension& orig);


  /**
   * Assignment operator for Dimension.
   *
   * @param rhs the Dimension object whose values are to be used as the basis
   * of the assignment.
   */
  Dimension& operator=(const Dimension& rhs);


  /**
   * Creates and returns a deep copy of this Dimension object.
   *
   * @return a (deep) copy of this Dimension object.
   */
  virtual Dimension* clone() const;


  /**
   * Destructor for Dimension.
   */
  virtual ~Dimension();


  /**
   * Returns the value of the "id" attribute of this Dimension.
   *
   * @return the value of the "id" attribute of this Dimension as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this Dimension.
   *
   * @return the value of the "name" attribute of this Dimension as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "size" attribute of this Dimension.
   *
   * @return the value of the "size" attribute of this Dimension as a string.
   */
  const std::string& getSize() const;


  /**
   * Returns the value of the "arrayDimension" attribute of this Dimension.
   *
   * @return the value of the "arrayDimension" attribute of this Dimension as a
   * unsigned integer.
   */
  unsigned int getArrayDimension() const;


  /**
   * Predicate returning @c true if this Dimension's "id" attribute is set.
   *
   * @return @c true if this Dimension's "id" attribute has been set, otherwise
   * @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this Dimension's "name" attribute is set.
   *
   * @return @c true if this Dimension's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this Dimension's "size" attribute is set.
   *
   * @return @c true if this Dimension's "size" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetSize() const;


  /**
   * Predicate returning @c true if this Dimension's "arrayDimension" attribute
   * is set.
   *
   * @return @c true if this Dimension's "arrayDimension" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetArrayDimension() const;


  /**
   * Sets the value of the "id" attribute of this Dimension.
   *
   * @param id std::string& value of the "id" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "name" attribute of this Dimension.
   *
   * @param name std::string& value of the "name" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Sets the value of the "size" attribute of this Dimension.
   *
   * @param size std::string& value of the "size" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSize(const std::string& size);


  /**
   * Sets the value of the "arrayDimension" attribute of this Dimension.
   *
   * @param arrayDimension unsigned int value of the "arrayDimension" attribute
   * to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setArrayDimension(unsigned int arrayDimension);


  /**
   * Unsets the value of the "id" attribute of this Dimension.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this Dimension.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "size" attribute of this Dimension.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSize();


  /**
   * Unsets the value of the "arrayDimension" attribute of this Dimension.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetArrayDimension();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this Dimension object.
   *
   * For Dimension, the XML element name is always @c "dimension".
   *
   * @return the name of this element, i.e. @c "dimension".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Dimension object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   *
   * @sbmlconstant{SBML_ARRAYS_DIMENSION, SBMLArraysTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * Dimension object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * Dimension have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the Dimension object are:
   * @li "size"
   * @li "arrayDimension"
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
   * Gets the value of the "attributeName" attribute of this Dimension.
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
   * Gets the value of the "attributeName" attribute of this Dimension.
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
   * Gets the value of the "attributeName" attribute of this Dimension.
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
   * Gets the value of the "attributeName" attribute of this Dimension.
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
   * Gets the value of the "attributeName" attribute of this Dimension.
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
   * Predicate returning @c true if this Dimension's attribute "attributeName"
   * is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this Dimension's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Dimension.
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
   * Sets the value of the "attributeName" attribute of this Dimension.
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
   * Sets the value of the "attributeName" attribute of this Dimension.
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
   * Sets the value of the "attributeName" attribute of this Dimension.
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
   * Sets the value of the "attributeName" attribute of this Dimension.
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
   * Unsets the value of the "attributeName" attribute of this Dimension.
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
 * Creates a new Dimension_t using the given SBML Level, Version and
 * &ldquo;arrays&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Dimension_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Dimension_t.
 *
 * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to this
 * Dimension_t.
 *
 * @throws SBMLConstructorException
 * Thrown if the given @p level and @p version combination, or this kind of
 * SBML object, are either invalid or mismatched with respect to the parent
 * SBMLDocument object.
 * @copydetails doc_note_setting_lv
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
Dimension_t *
Dimension_create(unsigned int level,
                 unsigned int version,
                 unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Dimension_t object.
 *
 * @param d the Dimension_t structure.
 *
 * @return a (deep) copy of this Dimension_t object.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
Dimension_t*
Dimension_clone(const Dimension_t* d);


/**
 * Frees this Dimension_t object.
 *
 * @param d the Dimension_t structure.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
void
Dimension_free(Dimension_t* d);


/**
 * Returns the value of the "id" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this Dimension_t as a pointer to
 * a string.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
const char *
Dimension_getId(const Dimension_t * d);


/**
 * Returns the value of the "name" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this Dimension_t as a pointer
 * to a string.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
const char *
Dimension_getName(const Dimension_t * d);


/**
 * Returns the value of the "size" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure whose size is sought.
 *
 * @return the value of the "size" attribute of this Dimension_t as a pointer
 * to a string.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
const char *
Dimension_getSize(const Dimension_t * d);


/**
 * Returns the value of the "arrayDimension" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure whose arrayDimension is sought.
 *
 * @return the value of the "arrayDimension" attribute of this Dimension_t as a
 * unsigned integer.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
unsigned int
Dimension_getArrayDimension(const Dimension_t * d);


/**
 * Predicate returning @c 1 if this Dimension_t's "id" attribute is set.
 *
 * @param d the Dimension_t structure.
 *
 * @return @c 1 if this Dimension_t's "id" attribute has been set, otherwise @c
 * 0 is returned.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_isSetId(const Dimension_t * d);


/**
 * Predicate returning @c 1 if this Dimension_t's "name" attribute is set.
 *
 * @param d the Dimension_t structure.
 *
 * @return @c 1 if this Dimension_t's "name" attribute has been set, otherwise
 * @c 0 is returned.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_isSetName(const Dimension_t * d);


/**
 * Predicate returning @c 1 if this Dimension_t's "size" attribute is set.
 *
 * @param d the Dimension_t structure.
 *
 * @return @c 1 if this Dimension_t's "size" attribute has been set, otherwise
 * @c 0 is returned.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_isSetSize(const Dimension_t * d);


/**
 * Predicate returning @c 1 if this Dimension_t's "arrayDimension" attribute is
 * set.
 *
 * @param d the Dimension_t structure.
 *
 * @return @c 1 if this Dimension_t's "arrayDimension" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_isSetArrayDimension(const Dimension_t * d);


/**
 * Sets the value of the "id" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_setId(Dimension_t * d, const char * id);


/**
 * Sets the value of the "name" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_setName(Dimension_t * d, const char * name);


/**
 * Sets the value of the "size" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure.
 *
 * @param size const char * value of the "size" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_setSize(Dimension_t * d, const char * size);


/**
 * Sets the value of the "arrayDimension" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure.
 *
 * @param arrayDimension unsigned int value of the "arrayDimension" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_setArrayDimension(Dimension_t * d, unsigned int arrayDimension);


/**
 * Unsets the value of the "id" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_unsetId(Dimension_t * d);


/**
 * Unsets the value of the "name" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_unsetName(Dimension_t * d);


/**
 * Unsets the value of the "size" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_unsetSize(Dimension_t * d);


/**
 * Unsets the value of the "arrayDimension" attribute of this Dimension_t.
 *
 * @param d the Dimension_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_unsetArrayDimension(Dimension_t * d);


/**
 * Predicate returning @c 1 if all the required attributes for this Dimension_t
 * object have been set.
 *
 * @param d the Dimension_t structure.
 *
 * @return @c 1 to indicate that all the required attributes of this
 * Dimension_t have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required attributes for the Dimension_t object are:
 * @li "size"
 * @li "arrayDimension"
 *
 * @memberof Dimension_t
 */
LIBSBML_EXTERN
int
Dimension_hasRequiredAttributes(const Dimension_t * d);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Dimension_H__ */


