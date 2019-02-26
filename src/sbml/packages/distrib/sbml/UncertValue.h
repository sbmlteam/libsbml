/**
 * @file UncertValue.h
 * @brief Definition of the UncertValue class.
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
 * @class UncertValue
 * @sbmlbrief{distrib} TODO:Definition of the UncertValue class.
 */


#ifndef UncertValue_H__
#define UncertValue_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class ExternalParameter;

class LIBSBML_EXTERN UncertValue : public DistribBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  double mValue;
  bool mIsSetValue;
  std::string mVar;
  std::string mUnits;
  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new UncertValue using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * UncertValue.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * UncertValue.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this UncertValue.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UncertValue(unsigned int level = DistribExtension::getDefaultLevel(),
              unsigned int version = DistribExtension::getDefaultVersion(),
              unsigned int pkgVersion =
                DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new UncertValue using the given DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UncertValue(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for UncertValue.
   *
   * @param orig the UncertValue instance to copy.
   */
  UncertValue(const UncertValue& orig);


  /**
   * Assignment operator for UncertValue.
   *
   * @param rhs the UncertValue object whose values are to be used as the basis
   * of the assignment.
   */
  UncertValue& operator=(const UncertValue& rhs);


  /**
   * Creates and returns a deep copy of this UncertValue object.
   *
   * @return a (deep) copy of this UncertValue object.
   */
  virtual UncertValue* clone() const;


  /**
   * Destructor for UncertValue.
   */
  virtual ~UncertValue();


  /**
   * Returns the value of the "value" attribute of this UncertValue.
   *
   * @return the value of the "value" attribute of this UncertValue as a
   * double.
   */
  double getValue() const;


  /**
   * Returns the value of the "var" attribute of this UncertValue.
   *
   * @return the value of the "var" attribute of this UncertValue as a string.
   */
  const std::string& getVar() const;


  /**
   * Returns the value of the "units" attribute of this UncertValue.
   *
   * @return the value of the "units" attribute of this UncertValue as a
   * string.
   */
  const std::string& getUnits() const;


  /**
   * Predicate returning @c true if this UncertValue's "value" attribute is
   * set.
   *
   * @return @c true if this UncertValue's "value" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetValue() const;


  /**
   * Predicate returning @c true if this UncertValue's "var" attribute is set.
   *
   * @return @c true if this UncertValue's "var" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetVar() const;


  /**
   * Predicate returning @c true if this UncertValue's "units" attribute is
   * set.
   *
   * @return @c true if this UncertValue's "units" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetUnits() const;


  /**
   * Sets the value of the "value" attribute of this UncertValue.
   *
   * @param value double value of the "value" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setValue(double value);


  /**
   * Sets the value of the "var" attribute of this UncertValue.
   *
   * @param var std::string& value of the "var" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVar(const std::string& var);


  /**
   * Sets the value of the "units" attribute of this UncertValue.
   *
   * @param units std::string& value of the "units" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setUnits(const std::string& units);


  /**
   * Unsets the value of the "value" attribute of this UncertValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValue();


  /**
   * Unsets the value of the "var" attribute of this UncertValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVar();


  /**
   * Unsets the value of the "units" attribute of this UncertValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetUnits();


  /**
   * Predicate returning @c true if this abstract "UncertValue" is of type
   * ExternalParameter
   *
   * @return @c true if this abstract "UncertValue" is of type
   * ExternalParameter, @c false otherwise
   */
  virtual bool isExternalParameter() const;


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this UncertValue object.
   *
   * For UncertValue, the XML element name is always @c "uncertValue".
   *
   * @return the name of this element, i.e. @c "uncertValue".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this UncertValue object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this UncertValue object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_UNCERTVALUE, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * UncertValue object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * UncertValue have been set, otherwise @c false is returned.
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
   * Gets the value of the "attributeName" attribute of this UncertValue.
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
   * Gets the value of the "attributeName" attribute of this UncertValue.
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
   * Gets the value of the "attributeName" attribute of this UncertValue.
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
   * Gets the value of the "attributeName" attribute of this UncertValue.
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
   * Gets the value of the "attributeName" attribute of this UncertValue.
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
   * Predicate returning @c true if this UncertValue's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this UncertValue's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this UncertValue.
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
   * Sets the value of the "attributeName" attribute of this UncertValue.
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
   * Sets the value of the "attributeName" attribute of this UncertValue.
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
   * Sets the value of the "attributeName" attribute of this UncertValue.
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
   * Sets the value of the "attributeName" attribute of this UncertValue.
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
   * Unsets the value of the "attributeName" attribute of this UncertValue.
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
 * Creates a new UncertValue_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * UncertValue_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * UncertValue_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this UncertValue_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
UncertValue_t *
UncertValue_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this UncertValue_t object.
 *
 * @param uv the UncertValue_t structure.
 *
 * @return a (deep) copy of this UncertValue_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
UncertValue_t*
UncertValue_clone(const UncertValue_t* uv);


/**
 * Frees this UncertValue_t object.
 *
 * @param uv the UncertValue_t structure.
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
void
UncertValue_free(UncertValue_t* uv);


/**
 * Returns the value of the "value" attribute of this UncertValue_t.
 *
 * @param uv the UncertValue_t structure whose value is sought.
 *
 * @return the value of the "value" attribute of this UncertValue_t as a
 * double.
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
double
UncertValue_getValue(const UncertValue_t * uv);


/**
 * Returns the value of the "var" attribute of this UncertValue_t.
 *
 * @param uv the UncertValue_t structure whose var is sought.
 *
 * @return the value of the "var" attribute of this UncertValue_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
char *
UncertValue_getVar(const UncertValue_t * uv);


/**
 * Returns the value of the "units" attribute of this UncertValue_t.
 *
 * @param uv the UncertValue_t structure whose units is sought.
 *
 * @return the value of the "units" attribute of this UncertValue_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
char *
UncertValue_getUnits(const UncertValue_t * uv);


/**
 * Predicate returning @c 1 (true) if this UncertValue_t's "value" attribute is
 * set.
 *
 * @param uv the UncertValue_t structure.
 *
 * @return @c 1 (true) if this UncertValue_t's "value" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_isSetValue(const UncertValue_t * uv);


/**
 * Predicate returning @c 1 (true) if this UncertValue_t's "var" attribute is
 * set.
 *
 * @param uv the UncertValue_t structure.
 *
 * @return @c 1 (true) if this UncertValue_t's "var" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_isSetVar(const UncertValue_t * uv);


/**
 * Predicate returning @c 1 (true) if this UncertValue_t's "units" attribute is
 * set.
 *
 * @param uv the UncertValue_t structure.
 *
 * @return @c 1 (true) if this UncertValue_t's "units" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_isSetUnits(const UncertValue_t * uv);


/**
 * Sets the value of the "value" attribute of this UncertValue_t.
 *
 * @param uv the UncertValue_t structure.
 *
 * @param value double value of the "value" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_setValue(UncertValue_t * uv, double value);


/**
 * Sets the value of the "var" attribute of this UncertValue_t.
 *
 * @param uv the UncertValue_t structure.
 *
 * @param var const char * value of the "var" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_setVar(UncertValue_t * uv, const char * var);


/**
 * Sets the value of the "units" attribute of this UncertValue_t.
 *
 * @param uv the UncertValue_t structure.
 *
 * @param units const char * value of the "units" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_setUnits(UncertValue_t * uv, const char * units);


/**
 * Unsets the value of the "value" attribute of this UncertValue_t.
 *
 * @param uv the UncertValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_unsetValue(UncertValue_t * uv);


/**
 * Unsets the value of the "var" attribute of this UncertValue_t.
 *
 * @param uv the UncertValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_unsetVar(UncertValue_t * uv);


/**
 * Unsets the value of the "units" attribute of this UncertValue_t.
 *
 * @param uv the UncertValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_unsetUnits(UncertValue_t * uv);


/**
 * Predicate returning @c 1 if this UncertValue_t is of type
 * ExternalParameter_t
 *
 * @param uv the UncertValue_t structure.
 *
 * @return @c 1 if this UncertValue_t is of type ExternalParameter_t, @c 0
 * otherwise
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_isExternalParameter(const UncertValue_t * uv);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UncertValue_t object have been set.
 *
 * @param uv the UncertValue_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * UncertValue_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertValue_t
 */
LIBSBML_EXTERN
int
UncertValue_hasRequiredAttributes(const UncertValue_t * uv);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !UncertValue_H__ */


