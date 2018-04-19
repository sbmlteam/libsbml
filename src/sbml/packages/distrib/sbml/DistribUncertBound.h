/**
 * @file DistribUncertBound.h
 * @brief Definition of the DistribUncertBound class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @class DistribUncertBound
 * @sbmlbrief{distrib} TODO:Definition of the DistribUncertBound class.
 */


#ifndef DistribUncertBound_H__
#define DistribUncertBound_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribUncertValue.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribUncertBound : public DistribUncertValue
{
protected:

  /** @cond doxygenLibsbmlInternal */

  bool mInclusive;
  bool mIsSetInclusive;
  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new DistribUncertBound using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribUncertBound.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribUncertBound.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribUncertBound.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertBound(unsigned int level = DistribExtension::getDefaultLevel(),
                     unsigned int version =
                       DistribExtension::getDefaultVersion(),
                     unsigned int pkgVersion =
                       DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribUncertBound using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertBound(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribUncertBound.
   *
   * @param orig the DistribUncertBound instance to copy.
   */
  DistribUncertBound(const DistribUncertBound& orig);


  /**
   * Assignment operator for DistribUncertBound.
   *
   * @param rhs the DistribUncertBound object whose values are to be used as
   * the basis of the assignment.
   */
  DistribUncertBound& operator=(const DistribUncertBound& rhs);


  /**
   * Creates and returns a deep copy of this DistribUncertBound object.
   *
   * @return a (deep) copy of this DistribUncertBound object.
   */
  virtual DistribUncertBound* clone() const;


  /**
   * Destructor for DistribUncertBound.
   */
  virtual ~DistribUncertBound();


  /**
   * Returns the value of the "id" attribute of this DistribUncertBound.
   *
   * @return the value of the "id" attribute of this DistribUncertBound as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DistribUncertBound.
   *
   * @return the value of the "name" attribute of this DistribUncertBound as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "inclusive" attribute of this DistribUncertBound.
   *
   * @return the value of the "inclusive" attribute of this DistribUncertBound
   * as a boolean.
   */
  bool getInclusive() const;


  /**
   * Predicate returning @c true if this DistribUncertBound's "id" attribute is
   * set.
   *
   * @return @c true if this DistribUncertBound's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribUncertBound's "name" attribute
   * is set.
   *
   * @return @c true if this DistribUncertBound's "name" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this DistribUncertBound's "inclusive"
   * attribute is set.
   *
   * @return @c true if this DistribUncertBound's "inclusive" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetInclusive() const;


  /**
   * Sets the value of the "id" attribute of this DistribUncertBound.
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
   * Sets the value of the "name" attribute of this DistribUncertBound.
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
   * Sets the value of the "inclusive" attribute of this DistribUncertBound.
   *
   * @param inclusive bool value of the "inclusive" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setInclusive(bool inclusive);


  /**
   * Unsets the value of the "id" attribute of this DistribUncertBound.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DistribUncertBound.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "inclusive" attribute of this DistribUncertBound.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetInclusive();


  /**
   * Returns the XML element name of this DistribUncertBound object.
   *
   * For DistribUncertBound, the XML element name is always @c "uncertBound".
   *
   * @return the name of this element, i.e. @c "uncertBound".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this DistribUncertBound object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this DistribUncertBound object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_UNCERTBOUND, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribUncertBound object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribUncertBound have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the DistribUncertBound object are:
   * @li "inclusive"
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
   * DistribUncertBound.
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
   * DistribUncertBound.
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
   * DistribUncertBound.
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
   * DistribUncertBound.
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
   * DistribUncertBound.
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
   * Predicate returning @c true if this DistribUncertBound's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribUncertBound's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribUncertBound.
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
   * DistribUncertBound.
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
   * DistribUncertBound.
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
   * DistribUncertBound.
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
   * DistribUncertBound.
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
   * DistribUncertBound.
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
   * Reads the expected attributes into the member data variables
   */
  virtual void readL3V1V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readL3V2V1Attributes(const XMLAttributes& attributes);

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
  virtual void writeL3V1V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeL3V2V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new DistribUncertBound_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribUncertBound_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribUncertBound_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribUncertBound_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
DistribUncertBound_t *
DistribUncertBound_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribUncertBound_t object.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @return a (deep) copy of this DistribUncertBound_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribUncertBound_clone(const DistribUncertBound_t* dub);


/**
 * Frees this DistribUncertBound_t object.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
void
DistribUncertBound_free(DistribUncertBound_t* dub);


/**
 * Returns the value of the "id" attribute of this DistribUncertBound_t.
 *
 * @param dub the DistribUncertBound_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribUncertBound_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
char *
DistribUncertBound_getId(const DistribUncertBound_t * dub);


/**
 * Returns the value of the "name" attribute of this DistribUncertBound_t.
 *
 * @param dub the DistribUncertBound_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DistribUncertBound_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
char *
DistribUncertBound_getName(const DistribUncertBound_t * dub);


/**
 * Returns the value of the "inclusive" attribute of this DistribUncertBound_t.
 *
 * @param dub the DistribUncertBound_t structure whose inclusive is sought.
 *
 * @return the value of the "inclusive" attribute of this DistribUncertBound_t
 * as a boolean.
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_getInclusive(const DistribUncertBound_t * dub);


/**
 * Predicate returning @c 1 (true) if this DistribUncertBound_t's "id"
 * attribute is set.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @return @c 1 (true) if this DistribUncertBound_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_isSetId(const DistribUncertBound_t * dub);


/**
 * Predicate returning @c 1 (true) if this DistribUncertBound_t's "name"
 * attribute is set.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @return @c 1 (true) if this DistribUncertBound_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_isSetName(const DistribUncertBound_t * dub);


/**
 * Predicate returning @c 1 (true) if this DistribUncertBound_t's "inclusive"
 * attribute is set.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @return @c 1 (true) if this DistribUncertBound_t's "inclusive" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_isSetInclusive(const DistribUncertBound_t * dub);


/**
 * Sets the value of the "id" attribute of this DistribUncertBound_t.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribUncertBound_unsetId().
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_setId(DistribUncertBound_t * dub, const char * id);


/**
 * Sets the value of the "name" attribute of this DistribUncertBound_t.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribUncertBound_unsetName().
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_setName(DistribUncertBound_t * dub, const char * name);


/**
 * Sets the value of the "inclusive" attribute of this DistribUncertBound_t.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @param inclusive int value of the "inclusive" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_setInclusive(DistribUncertBound_t * dub, int inclusive);


/**
 * Unsets the value of the "id" attribute of this DistribUncertBound_t.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_unsetId(DistribUncertBound_t * dub);


/**
 * Unsets the value of the "name" attribute of this DistribUncertBound_t.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_unsetName(DistribUncertBound_t * dub);


/**
 * Unsets the value of the "inclusive" attribute of this DistribUncertBound_t.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_unsetInclusive(DistribUncertBound_t * dub);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertBound_t object have been set.
 *
 * @param dub the DistribUncertBound_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribUncertBound_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the DistribUncertBound_t object are:
 * @li "inclusive"
 *
 * @memberof DistribUncertBound_t
 */
LIBSBML_EXTERN
int
DistribUncertBound_hasRequiredAttributes(const DistribUncertBound_t * dub);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribUncertBound_H__ */


