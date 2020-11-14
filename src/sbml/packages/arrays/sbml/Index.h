/**
 * @file Index.h
 * @brief Definition of the Index class.
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
 * @class Index
 * @sbmlbrief{arrays} TODO:Definition of the Index class.
 */


#ifndef Index_H__
#define Index_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/arrays/common/arraysfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/math/ASTNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Index : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mReferencedAttribute;
  unsigned int mArrayDimension;
  bool mIsSetArrayDimension;
  ASTNode* mMath;

  /** @endcond */

public:

  /**
   * Creates a new Index using the given SBML Level, Version and
   * &ldquo;arrays&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Index.
   *
   * @param version an unsigned int, the SBML Version to assign to this Index.
   *
   * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to
   * this Index.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  Index(unsigned int level = ArraysExtension::getDefaultLevel(),
        unsigned int version = ArraysExtension::getDefaultVersion(),
        unsigned int pkgVersion = ArraysExtension::getDefaultPackageVersion());


  /**
   * Creates a new Index using the given ArraysPkgNamespaces object.
   *
   * @param arraysns the ArraysPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  Index(ArraysPkgNamespaces *arraysns);


  /**
   * Copy constructor for Index.
   *
   * @param orig the Index instance to copy.
   */
  Index(const Index& orig);


  /**
   * Assignment operator for Index.
   *
   * @param rhs the Index object whose values are to be used as the basis of
   * the assignment.
   */
  Index& operator=(const Index& rhs);


  /**
   * Creates and returns a deep copy of this Index object.
   *
   * @return a (deep) copy of this Index object.
   */
  virtual Index* clone() const;


  /**
   * Destructor for Index.
   */
  virtual ~Index();


  /**
   * Returns the value of the "referencedAttribute" attribute of this Index.
   *
   * @return the value of the "referencedAttribute" attribute of this Index as
   * a string.
   */
  const std::string& getReferencedAttribute() const;


  /**
   * Returns the value of the "arrayDimension" attribute of this Index.
   *
   * @return the value of the "arrayDimension" attribute of this Index as a
   * unsigned integer.
   */
  unsigned int getArrayDimension() const;


  /**
   * Predicate returning @c true if this Index's "referencedAttribute"
   * attribute is set.
   *
   * @return @c true if this Index's "referencedAttribute" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetReferencedAttribute() const;


  /**
   * Predicate returning @c true if this Index's "arrayDimension" attribute is
   * set.
   *
   * @return @c true if this Index's "arrayDimension" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetArrayDimension() const;


  /**
   * Sets the value of the "referencedAttribute" attribute of this Index.
   *
   * @param referencedAttribute std::string& value of the "referencedAttribute"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setReferencedAttribute(const std::string& referencedAttribute);


  /**
   * Sets the value of the "arrayDimension" attribute of this Index.
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
   * Unsets the value of the "referencedAttribute" attribute of this Index.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetReferencedAttribute();


  /**
   * Unsets the value of the "arrayDimension" attribute of this Index.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetArrayDimension();


  /**
   * Returns the value of the "math" element of this Index.
   *
   * @return the value of the "math" element of this Index as a ASTNode*.
   */
  const ASTNode* getMath() const;


  /**
   * Returns the value of the "math" element of this Index.
   *
   * @return the value of the "math" element of this Index as a ASTNode*.
   */
  ASTNode* getMath();


  /**
   * Predicate returning @c true if this Index's "math" element is set.
   *
   * @return @c true if this Index's "math" element has been set, otherwise @c
   * false is returned.
   */
  bool isSetMath() const;


  /**
   * Sets the value of the "math" element of this Index.
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
   * Unsets the value of the "math" element of this Index.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMath();


  /**
   * Returns the XML element name of this Index object.
   *
   * For Index, the XML element name is always @c "index".
   *
   * @return the name of this element, i.e. @c "index".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Index object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   *
   * @sbmlconstant{SBML_ARRAYS_INDEX, SBMLArraysTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this Index
   * object have been set.
   *
   * @return @c true to indicate that all the required attributes of this Index
   * have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the Index object are:
   * @li "referencedAttribute"
   * @li "arrayDimension"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this Index
   * object have been set.
   *
   * @return @c true to indicate that all the required elements of this Index
   * have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the Index object are:
   * @li "math"
   */
  virtual bool hasRequiredElements() const;



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




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Index.
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
   * Gets the value of the "attributeName" attribute of this Index.
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
   * Gets the value of the "attributeName" attribute of this Index.
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
   * Gets the value of the "attributeName" attribute of this Index.
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
   * Gets the value of the "attributeName" attribute of this Index.
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
   * Predicate returning @c true if this Index's attribute "attributeName" is
   * set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this Index's attribute "attributeName" has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Index.
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
   * Sets the value of the "attributeName" attribute of this Index.
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
   * Sets the value of the "attributeName" attribute of this Index.
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
   * Sets the value of the "attributeName" attribute of this Index.
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
   * Sets the value of the "attributeName" attribute of this Index.
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
   * Unsets the value of the "attributeName" attribute of this Index.
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
 * Creates a new Index_t using the given SBML Level, Version and
 * &ldquo;arrays&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Index_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this Index_t.
 *
 * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to this
 * Index_t.
 *
 * @throws SBMLConstructorException
 * Thrown if the given @p level and @p version combination, or this kind of
 * SBML object, are either invalid or mismatched with respect to the parent
 * SBMLDocument object.
 * @copydetails doc_note_setting_lv
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
Index_t *
Index_create(unsigned int level,
             unsigned int version,
             unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Index_t object.
 *
 * @param i the Index_t structure.
 *
 * @return a (deep) copy of this Index_t object.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
Index_t*
Index_clone(const Index_t* i);


/**
 * Frees this Index_t object.
 *
 * @param i the Index_t structure.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
void
Index_free(Index_t* i);


/**
 * Returns the value of the "referencedAttribute" attribute of this Index_t.
 *
 * @param i the Index_t structure whose referencedAttribute is sought.
 *
 * @return the value of the "referencedAttribute" attribute of this Index_t as
 * a pointer to a string.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
const char *
Index_getReferencedAttribute(const Index_t * i);


/**
 * Returns the value of the "arrayDimension" attribute of this Index_t.
 *
 * @param i the Index_t structure whose arrayDimension is sought.
 *
 * @return the value of the "arrayDimension" attribute of this Index_t as a
 * unsigned integer.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
unsigned int
Index_getArrayDimension(const Index_t * i);


/**
 * Predicate returning @c 1 if this Index_t's "referencedAttribute" attribute
 * is set.
 *
 * @param i the Index_t structure.
 *
 * @return @c 1 if this Index_t's "referencedAttribute" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_isSetReferencedAttribute(const Index_t * i);


/**
 * Predicate returning @c 1 if this Index_t's "arrayDimension" attribute is
 * set.
 *
 * @param i the Index_t structure.
 *
 * @return @c 1 if this Index_t's "arrayDimension" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_isSetArrayDimension(const Index_t * i);


/**
 * Sets the value of the "referencedAttribute" attribute of this Index_t.
 *
 * @param i the Index_t structure.
 *
 * @param referencedAttribute const char * value of the "referencedAttribute"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_setReferencedAttribute(Index_t * i, const char * referencedAttribute);


/**
 * Sets the value of the "arrayDimension" attribute of this Index_t.
 *
 * @param i the Index_t structure.
 *
 * @param arrayDimension unsigned int value of the "arrayDimension" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_setArrayDimension(Index_t * i, unsigned int arrayDimension);


/**
 * Unsets the value of the "referencedAttribute" attribute of this Index_t.
 *
 * @param i the Index_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_unsetReferencedAttribute(Index_t * i);


/**
 * Unsets the value of the "arrayDimension" attribute of this Index_t.
 *
 * @param i the Index_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_unsetArrayDimension(Index_t * i);


/**
 * Returns the value of the "math" element of this Index_t.
 *
 * @param i the Index_t structure whose math is sought.
 *
 * @return the value of the "math" element of this Index_t as a ASTNode*.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
const ASTNode_t*
Index_getMath(const Index_t * i);


/**
 * Predicate returning @c 1 if this Index_t's "math" element is set.
 *
 * @param i the Index_t structure.
 *
 * @return @c 1 if this Index_t's "math" element has been set, otherwise @c 0
 * is returned.
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_isSetMath(const Index_t * i);


/**
 * Sets the value of the "math" element of this Index_t.
 *
 * @param i the Index_t structure.
 *
 * @param math ASTNode_t* value of the "math" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_setMath(Index_t * i, const ASTNode_t* math);


/**
 * Unsets the value of the "math" element of this Index_t.
 *
 * @param i the Index_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_unsetMath(Index_t * i);


/**
 * Predicate returning @c 1 if all the required attributes for this Index_t
 * object have been set.
 *
 * @param i the Index_t structure.
 *
 * @return @c 1 to indicate that all the required attributes of this Index_t
 * have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required attributes for the Index_t object are:
 * @li "referencedAttribute"
 * @li "arrayDimension"
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_hasRequiredAttributes(const Index_t * i);


/**
 * Predicate returning @c 1 if all the required elements for this Index_t
 * object have been set.
 *
 * @param i the Index_t structure.
 *
 * @return @c 1 to indicate that all the required elements of this Index_t have
 * been set, otherwise @c 0 is returned.
 *
 *
 * @note The required elements for the Index_t object are:
 * @li "math"
 *
 * @memberof Index_t
 */
LIBSBML_EXTERN
int
Index_hasRequiredElements(const Index_t * i);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Index_H__ */


