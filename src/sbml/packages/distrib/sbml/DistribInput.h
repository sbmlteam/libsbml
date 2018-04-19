/**
 * @file DistribInput.h
 * @brief Definition of the DistribInput class.
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
 * @class DistribInput
 * @sbmlbrief{distrib} TODO:Definition of the DistribInput class.
 */


#ifndef DistribInput_H__
#define DistribInput_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribInput : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  unsigned int mIndex;
  bool mIsSetIndex;

  /** @endcond */

public:

  /**
   * Creates a new DistribInput using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribInput.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribInput.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribInput.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribInput(unsigned int level = DistribExtension::getDefaultLevel(),
               unsigned int version = DistribExtension::getDefaultVersion(),
               unsigned int pkgVersion =
                 DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribInput using the given DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribInput(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribInput.
   *
   * @param orig the DistribInput instance to copy.
   */
  DistribInput(const DistribInput& orig);


  /**
   * Assignment operator for DistribInput.
   *
   * @param rhs the DistribInput object whose values are to be used as the
   * basis of the assignment.
   */
  DistribInput& operator=(const DistribInput& rhs);


  /**
   * Creates and returns a deep copy of this DistribInput object.
   *
   * @return a (deep) copy of this DistribInput object.
   */
  virtual DistribInput* clone() const;


  /**
   * Destructor for DistribInput.
   */
  virtual ~DistribInput();


  /**
   * Returns the value of the "id" attribute of this DistribInput.
   *
   * @return the value of the "id" attribute of this DistribInput as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DistribInput.
   *
   * @return the value of the "name" attribute of this DistribInput as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "index" attribute of this DistribInput.
   *
   * @return the value of the "index" attribute of this DistribInput as a
   * unsigned integer.
   */
  unsigned int getIndex() const;


  /**
   * Predicate returning @c true if this DistribInput's "id" attribute is set.
   *
   * @return @c true if this DistribInput's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribInput's "name" attribute is
   * set.
   *
   * @return @c true if this DistribInput's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this DistribInput's "index" attribute is
   * set.
   *
   * @return @c true if this DistribInput's "index" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetIndex() const;


  /**
   * Sets the value of the "id" attribute of this DistribInput.
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
   * Sets the value of the "name" attribute of this DistribInput.
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
   * Sets the value of the "index" attribute of this DistribInput.
   *
   * @param index unsigned int value of the "index" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setIndex(unsigned int index);


  /**
   * Unsets the value of the "id" attribute of this DistribInput.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DistribInput.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "index" attribute of this DistribInput.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetIndex();


  /**
   * Returns the XML element name of this DistribInput object.
   *
   * For DistribInput, the XML element name is always @c "distribInput".
   *
   * @return the name of this element, i.e. @c "distribInput".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribInput object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_DISTRIBINPUT, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribInput object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribInput have been set, otherwise @c false is returned.
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
   * Gets the value of the "attributeName" attribute of this DistribInput.
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
   * Gets the value of the "attributeName" attribute of this DistribInput.
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
   * Gets the value of the "attributeName" attribute of this DistribInput.
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
   * Gets the value of the "attributeName" attribute of this DistribInput.
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
   * Gets the value of the "attributeName" attribute of this DistribInput.
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
   * Predicate returning @c true if this DistribInput's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribInput's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DistribInput.
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
   * Sets the value of the "attributeName" attribute of this DistribInput.
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
   * Sets the value of the "attributeName" attribute of this DistribInput.
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
   * Sets the value of the "attributeName" attribute of this DistribInput.
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
   * Sets the value of the "attributeName" attribute of this DistribInput.
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
   * Unsets the value of the "attributeName" attribute of this DistribInput.
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
 * Creates a new DistribInput_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribInput_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribInput_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribInput_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
DistribInput_t *
DistribInput_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribInput_t object.
 *
 * @param di the DistribInput_t structure.
 *
 * @return a (deep) copy of this DistribInput_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
DistribInput_t*
DistribInput_clone(const DistribInput_t* di);


/**
 * Frees this DistribInput_t object.
 *
 * @param di the DistribInput_t structure.
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
void
DistribInput_free(DistribInput_t* di);


/**
 * Returns the value of the "id" attribute of this DistribInput_t.
 *
 * @param di the DistribInput_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribInput_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
char *
DistribInput_getId(const DistribInput_t * di);


/**
 * Returns the value of the "name" attribute of this DistribInput_t.
 *
 * @param di the DistribInput_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DistribInput_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
char *
DistribInput_getName(const DistribInput_t * di);


/**
 * Returns the value of the "index" attribute of this DistribInput_t.
 *
 * @param di the DistribInput_t structure whose index is sought.
 *
 * @return the value of the "index" attribute of this DistribInput_t as a
 * unsigned integer.
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
unsigned int
DistribInput_getIndex(const DistribInput_t * di);


/**
 * Predicate returning @c 1 (true) if this DistribInput_t's "id" attribute is
 * set.
 *
 * @param di the DistribInput_t structure.
 *
 * @return @c 1 (true) if this DistribInput_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_isSetId(const DistribInput_t * di);


/**
 * Predicate returning @c 1 (true) if this DistribInput_t's "name" attribute is
 * set.
 *
 * @param di the DistribInput_t structure.
 *
 * @return @c 1 (true) if this DistribInput_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_isSetName(const DistribInput_t * di);


/**
 * Predicate returning @c 1 (true) if this DistribInput_t's "index" attribute
 * is set.
 *
 * @param di the DistribInput_t structure.
 *
 * @return @c 1 (true) if this DistribInput_t's "index" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_isSetIndex(const DistribInput_t * di);


/**
 * Sets the value of the "id" attribute of this DistribInput_t.
 *
 * @param di the DistribInput_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribInput_unsetId().
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_setId(DistribInput_t * di, const char * id);


/**
 * Sets the value of the "name" attribute of this DistribInput_t.
 *
 * @param di the DistribInput_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribInput_unsetName().
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_setName(DistribInput_t * di, const char * name);


/**
 * Sets the value of the "index" attribute of this DistribInput_t.
 *
 * @param di the DistribInput_t structure.
 *
 * @param index unsigned int value of the "index" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_setIndex(DistribInput_t * di, unsigned int index);


/**
 * Unsets the value of the "id" attribute of this DistribInput_t.
 *
 * @param di the DistribInput_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_unsetId(DistribInput_t * di);


/**
 * Unsets the value of the "name" attribute of this DistribInput_t.
 *
 * @param di the DistribInput_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_unsetName(DistribInput_t * di);


/**
 * Unsets the value of the "index" attribute of this DistribInput_t.
 *
 * @param di the DistribInput_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_unsetIndex(DistribInput_t * di);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribInput_t object have been set.
 *
 * @param di the DistribInput_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribInput_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribInput_t
 */
LIBSBML_EXTERN
int
DistribInput_hasRequiredAttributes(const DistribInput_t * di);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribInput_H__ */


