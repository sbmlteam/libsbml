/**
 * @file KeyValuePair.h
 * @brief Definition of the KeyValuePair class.
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
 * @class KeyValuePair
 * @sbmlbrief{fbc} TODO:Definition of the KeyValuePair class.
 */


#ifndef KeyValuePair_H__
#define KeyValuePair_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN KeyValuePair : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mKey;
  std::string mValue;
  std::string mUri;

  /** @endcond */

public:

  /**
   * Creates a new KeyValuePair using the given SBML Level, Version and
   * &ldquo;fbc&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * KeyValuePair.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * KeyValuePair.
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this
   * KeyValuePair.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  KeyValuePair(unsigned int level = FbcExtension::getDefaultLevel(),
               unsigned int version = FbcExtension::getDefaultVersion(),
               unsigned int pkgVersion =
                 FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new KeyValuePair using the given FbcPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param fbcns the FbcPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  KeyValuePair(FbcPkgNamespaces *fbcns);


  /**
   * Copy constructor for KeyValuePair.
   *
   * @param orig the KeyValuePair instance to copy.
   */
  KeyValuePair(const KeyValuePair& orig);


  /**
   * Assignment operator for KeyValuePair.
   *
   * @param rhs the KeyValuePair object whose values are to be used as the
   * basis of the assignment.
   */
  KeyValuePair& operator=(const KeyValuePair& rhs);


  /**
   * Creates and returns a deep copy of this KeyValuePair object.
   *
   * @return a (deep) copy of this KeyValuePair object.
   */
  virtual KeyValuePair* clone() const;


  /**
   * Destructor for KeyValuePair.
   */
  virtual ~KeyValuePair();


  /**
   * Returns the value of the "id" attribute of this KeyValuePair.
   *
   * @return the value of the "id" attribute of this KeyValuePair as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this KeyValuePair.
   *
   * @return the value of the "name" attribute of this KeyValuePair as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "key" attribute of this KeyValuePair.
   *
   * @return the value of the "key" attribute of this KeyValuePair as a string.
   */
  const std::string& getKey() const;


  /**
   * Returns the value of the "value" attribute of this KeyValuePair.
   *
   * @return the value of the "value" attribute of this KeyValuePair as a
   * string.
   */
  const std::string& getValue() const;


  /**
   * Returns the value of the "uri" attribute of this KeyValuePair.
   *
   * @return the value of the "uri" attribute of this KeyValuePair as a string.
   */
  const std::string& getUri() const;


  /**
   * Predicate returning @c true if this KeyValuePair's "id" attribute is set.
   *
   * @return @c true if this KeyValuePair's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this KeyValuePair's "name" attribute is
   * set.
   *
   * @return @c true if this KeyValuePair's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this KeyValuePair's "key" attribute is set.
   *
   * @return @c true if this KeyValuePair's "key" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetKey() const;


  /**
   * Predicate returning @c true if this KeyValuePair's "value" attribute is
   * set.
   *
   * @return @c true if this KeyValuePair's "value" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetValue() const;


  /**
   * Predicate returning @c true if this KeyValuePair's "uri" attribute is set.
   *
   * @return @c true if this KeyValuePair's "uri" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetUri() const;


  /**
   * Sets the value of the "id" attribute of this KeyValuePair.
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
   * Sets the value of the "name" attribute of this KeyValuePair.
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
   * Sets the value of the "key" attribute of this KeyValuePair.
   *
   * @param key std::string& value of the "key" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p key = @c NULL or an empty string is
   * equivalent to calling unsetKey().
   */
  int setKey(const std::string& key);


  /**
   * Sets the value of the "value" attribute of this KeyValuePair.
   *
   * @param value std::string& value of the "value" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p value = @c NULL or an empty string is
   * equivalent to calling unsetValue().
   */
  int setValue(const std::string& value);


  /**
   * Sets the value of the "uri" attribute of this KeyValuePair.
   *
   * @param uri std::string& value of the "uri" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p uri = @c NULL or an empty string is
   * equivalent to calling unsetUri().
   */
  int setUri(const std::string& uri);


  /**
   * Unsets the value of the "id" attribute of this KeyValuePair.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this KeyValuePair.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "key" attribute of this KeyValuePair.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetKey();


  /**
   * Unsets the value of the "value" attribute of this KeyValuePair.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValue();


  /**
   * Unsets the value of the "uri" attribute of this KeyValuePair.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetUri();


  /**
   * Returns the XML element name of this KeyValuePair object.
   *
   * For KeyValuePair, the XML element name is always @c "keyValuePair".
   *
   * @return the name of this element, i.e. @c "keyValuePair".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this KeyValuePair object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_FBC_KEYVALUEPAIR, SBMLFbcTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * KeyValuePair object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * KeyValuePair have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the KeyValuePair object are:
   * @li "key"
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
   * Gets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Gets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Gets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Gets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Gets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Predicate returning @c true if this KeyValuePair's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this KeyValuePair's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Sets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Sets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Sets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Sets the value of the "attributeName" attribute of this KeyValuePair.
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
   * Unsets the value of the "attributeName" attribute of this KeyValuePair.
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


  /**
  * Creates an XMLNode object from this.
  */
  XMLNode toXML() const;




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
   * Reads the expected attributes into the member data variables
   */
  void readL3V1V3Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  void writeL3V1V3Attributes(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new KeyValuePair_t using the given SBML Level, Version and
 * &ldquo;fbc&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * KeyValuePair_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * KeyValuePair_t.
 *
 * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this
 * KeyValuePair_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
KeyValuePair_t *
KeyValuePair_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this KeyValuePair_t object.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @return a (deep) copy of this KeyValuePair_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
KeyValuePair_t*
KeyValuePair_clone(const KeyValuePair_t* kvp);


/**
 * Frees this KeyValuePair_t object.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
void
KeyValuePair_free(KeyValuePair_t* kvp);


/**
 * Returns the value of the "id" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this KeyValuePair_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
char *
KeyValuePair_getId(const KeyValuePair_t * kvp);


/**
 * Returns the value of the "name" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this KeyValuePair_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
char *
KeyValuePair_getName(const KeyValuePair_t * kvp);


/**
 * Returns the value of the "key" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure whose key is sought.
 *
 * @return the value of the "key" attribute of this KeyValuePair_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
char *
KeyValuePair_getKey(const KeyValuePair_t * kvp);


/**
 * Returns the value of the "value" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure whose value is sought.
 *
 * @return the value of the "value" attribute of this KeyValuePair_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
char *
KeyValuePair_getValue(const KeyValuePair_t * kvp);


/**
 * Returns the value of the "uri" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure whose uri is sought.
 *
 * @return the value of the "uri" attribute of this KeyValuePair_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
char *
KeyValuePair_getUri(const KeyValuePair_t * kvp);


/**
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "id" attribute is
 * set.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @return @c 1 (true) if this KeyValuePair_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetId(const KeyValuePair_t * kvp);


/**
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "name" attribute is
 * set.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @return @c 1 (true) if this KeyValuePair_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetName(const KeyValuePair_t * kvp);


/**
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "key" attribute is
 * set.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @return @c 1 (true) if this KeyValuePair_t's "key" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetKey(const KeyValuePair_t * kvp);


/**
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "value" attribute
 * is set.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @return @c 1 (true) if this KeyValuePair_t's "value" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetValue(const KeyValuePair_t * kvp);


/**
 * Predicate returning @c 1 (true) if this KeyValuePair_t's "uri" attribute is
 * set.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @return @c 1 (true) if this KeyValuePair_t's "uri" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_isSetUri(const KeyValuePair_t * kvp);


/**
 * Sets the value of the "id" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling KeyValuePair_unsetId().
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_setId(KeyValuePair_t * kvp, const char * id);


/**
 * Sets the value of the "name" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling KeyValuePair_unsetName().
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_setName(KeyValuePair_t * kvp, const char * name);


/**
 * Sets the value of the "key" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @param key const char * value of the "key" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p key = @c NULL or an empty string is equivalent
 * to calling KeyValuePair_unsetKey().
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_setKey(KeyValuePair_t * kvp, const char * key);


/**
 * Sets the value of the "value" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @param value const char * value of the "value" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p value = @c NULL or an empty string is
 * equivalent to calling KeyValuePair_unsetValue().
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_setValue(KeyValuePair_t * kvp, const char * value);


/**
 * Sets the value of the "uri" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @param uri const char * value of the "uri" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p uri = @c NULL or an empty string is equivalent
 * to calling KeyValuePair_unsetUri().
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_setUri(KeyValuePair_t * kvp, const char * uri);


/**
 * Unsets the value of the "id" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetId(KeyValuePair_t * kvp);


/**
 * Unsets the value of the "name" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetName(KeyValuePair_t * kvp);


/**
 * Unsets the value of the "key" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetKey(KeyValuePair_t * kvp);


/**
 * Unsets the value of the "value" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetValue(KeyValuePair_t * kvp);


/**
 * Unsets the value of the "uri" attribute of this KeyValuePair_t.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_unsetUri(KeyValuePair_t * kvp);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * KeyValuePair_t object have been set.
 *
 * @param kvp the KeyValuePair_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * KeyValuePair_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the KeyValuePair_t object are:
 * @li "key"
 *
 * @memberof KeyValuePair_t
 */
LIBSBML_EXTERN
int
KeyValuePair_hasRequiredAttributes(const KeyValuePair_t * kvp);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !KeyValuePair_H__ */


