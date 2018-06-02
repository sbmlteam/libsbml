/**
 * @file DistribUncertValue.h
 * @brief Definition of the DistribUncertValue class.
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
 * @class DistribUncertValue
 * @sbmlbrief{distrib} TODO:Definition of the DistribUncertValue class.
 */


#ifndef DistribUncertValue_H__
#define DistribUncertValue_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class DistribUncertBound;
class DistribExternalParameter;

class LIBSBML_EXTERN DistribUncertValue : public DistribBase
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
   * Creates a new DistribUncertValue using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribUncertValue.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribUncertValue.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribUncertValue.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertValue(unsigned int level = DistribExtension::getDefaultLevel(),
                     unsigned int version =
                       DistribExtension::getDefaultVersion(),
                     unsigned int pkgVersion =
                       DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribUncertValue using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertValue(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribUncertValue.
   *
   * @param orig the DistribUncertValue instance to copy.
   */
  DistribUncertValue(const DistribUncertValue& orig);


  /**
   * Assignment operator for DistribUncertValue.
   *
   * @param rhs the DistribUncertValue object whose values are to be used as
   * the basis of the assignment.
   */
  DistribUncertValue& operator=(const DistribUncertValue& rhs);


  /**
   * Creates and returns a deep copy of this DistribUncertValue object.
   *
   * @return a (deep) copy of this DistribUncertValue object.
   */
  virtual DistribUncertValue* clone() const;


  /**
   * Destructor for DistribUncertValue.
   */
  virtual ~DistribUncertValue();


  /**
   * Returns the value of the "value" attribute of this DistribUncertValue.
   *
   * @return the value of the "value" attribute of this DistribUncertValue as a
   * double.
   */
  double getValue() const;


  /**
   * Returns the value of the "var" attribute of this DistribUncertValue.
   *
   * @return the value of the "var" attribute of this DistribUncertValue as a
   * string.
   */
  const std::string& getVar() const;


  /**
   * Returns the value of the "units" attribute of this DistribUncertValue.
   *
   * @return the value of the "units" attribute of this DistribUncertValue as a
   * string.
   */
  const std::string& getUnits() const;


  /**
   * Predicate returning @c true if this DistribUncertValue's "value" attribute
   * is set.
   *
   * @return @c true if this DistribUncertValue's "value" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetValue() const;


  /**
   * Predicate returning @c true if this DistribUncertValue's "var" attribute
   * is set.
   *
   * @return @c true if this DistribUncertValue's "var" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetVar() const;


  /**
   * Predicate returning @c true if this DistribUncertValue's "units" attribute
   * is set.
   *
   * @return @c true if this DistribUncertValue's "units" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetUnits() const;


  /**
   * Sets the value of the "value" attribute of this DistribUncertValue.
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
   * Sets the value of the "var" attribute of this DistribUncertValue.
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
   * Sets the value of the "units" attribute of this DistribUncertValue.
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
   * Unsets the value of the "value" attribute of this DistribUncertValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValue();


  /**
   * Unsets the value of the "var" attribute of this DistribUncertValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVar();


  /**
   * Unsets the value of the "units" attribute of this DistribUncertValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetUnits();


  /**
   * Predicate returning @c true if this abstract "DistribUncertValue" is of
   * type DistribUncertBound
   *
   * @return @c true if this abstract "DistribUncertValue" is of type
   * DistribUncertBound, @c false otherwise
   */
  virtual bool isDistribUncertBound() const;


  /**
   * Predicate returning @c true if this abstract "DistribUncertValue" is of
   * type DistribExternalParameter
   *
   * @return @c true if this abstract "DistribUncertValue" is of type
   * DistribExternalParameter, @c false otherwise
   */
  virtual bool isDistribExternalParameter() const;


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this DistribUncertValue object.
   *
   * For DistribUncertValue, the XML element name is always @c "uncertValue".
   *
   * @return the name of this element, i.e. @c "uncertValue".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this DistribUncertValue object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this DistribUncertValue object.
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
   * DistribUncertValue object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribUncertValue have been set, otherwise @c false is returned.
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
   * DistribUncertValue.
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
   * DistribUncertValue.
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
   * DistribUncertValue.
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
   * DistribUncertValue.
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
   * DistribUncertValue.
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
   * Predicate returning @c true if this DistribUncertValue's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribUncertValue's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribUncertValue.
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
   * DistribUncertValue.
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
   * DistribUncertValue.
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
   * DistribUncertValue.
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
   * DistribUncertValue.
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
   * DistribUncertValue.
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
  void readL3V1V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  void readL3V2V1Attributes(const XMLAttributes& attributes);

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
  void writeL3V1V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  void writeL3V2V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new DistribUncertValue_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribUncertValue_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribUncertValue_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribUncertValue_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
DistribUncertValue_t *
DistribUncertValue_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribUncertValue_t object.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @return a (deep) copy of this DistribUncertValue_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertValue_clone(const DistribUncertValue_t* duv);


/**
 * Frees this DistribUncertValue_t object.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
void
DistribUncertValue_free(DistribUncertValue_t* duv);


/**
 * Returns the value of the "value" attribute of this DistribUncertValue_t.
 *
 * @param duv the DistribUncertValue_t structure whose value is sought.
 *
 * @return the value of the "value" attribute of this DistribUncertValue_t as a
 * double.
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
double
DistribUncertValue_getValue(const DistribUncertValue_t * duv);


/**
 * Returns the value of the "var" attribute of this DistribUncertValue_t.
 *
 * @param duv the DistribUncertValue_t structure whose var is sought.
 *
 * @return the value of the "var" attribute of this DistribUncertValue_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
char *
DistribUncertValue_getVar(const DistribUncertValue_t * duv);


/**
 * Returns the value of the "units" attribute of this DistribUncertValue_t.
 *
 * @param duv the DistribUncertValue_t structure whose units is sought.
 *
 * @return the value of the "units" attribute of this DistribUncertValue_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
char *
DistribUncertValue_getUnits(const DistribUncertValue_t * duv);


/**
 * Predicate returning @c 1 (true) if this DistribUncertValue_t's "value"
 * attribute is set.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @return @c 1 (true) if this DistribUncertValue_t's "value" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_isSetValue(const DistribUncertValue_t * duv);


/**
 * Predicate returning @c 1 (true) if this DistribUncertValue_t's "var"
 * attribute is set.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @return @c 1 (true) if this DistribUncertValue_t's "var" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_isSetVar(const DistribUncertValue_t * duv);


/**
 * Predicate returning @c 1 (true) if this DistribUncertValue_t's "units"
 * attribute is set.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @return @c 1 (true) if this DistribUncertValue_t's "units" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_isSetUnits(const DistribUncertValue_t * duv);


/**
 * Sets the value of the "value" attribute of this DistribUncertValue_t.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @param value double value of the "value" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_setValue(DistribUncertValue_t * duv, double value);


/**
 * Sets the value of the "var" attribute of this DistribUncertValue_t.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @param var const char * value of the "var" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_setVar(DistribUncertValue_t * duv, const char * var);


/**
 * Sets the value of the "units" attribute of this DistribUncertValue_t.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @param units const char * value of the "units" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_setUnits(DistribUncertValue_t * duv, const char * units);


/**
 * Unsets the value of the "value" attribute of this DistribUncertValue_t.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_unsetValue(DistribUncertValue_t * duv);


/**
 * Unsets the value of the "var" attribute of this DistribUncertValue_t.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_unsetVar(DistribUncertValue_t * duv);


/**
 * Unsets the value of the "units" attribute of this DistribUncertValue_t.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_unsetUnits(DistribUncertValue_t * duv);


/**
 * Predicate returning @c 1 if this DistribUncertValue_t is of type
 * DistribUncertBound_t
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @return @c 1 if this DistribUncertValue_t is of type DistribUncertBound_t,
 * @c 0 otherwise
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_isDistribUncertBound(const DistribUncertValue_t * duv);


/**
 * Predicate returning @c 1 if this DistribUncertValue_t is of type
 * DistribExternalParameter_t
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @return @c 1 if this DistribUncertValue_t is of type
 * DistribExternalParameter_t, @c 0 otherwise
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_isDistribExternalParameter(const DistribUncertValue_t *
  duv);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertValue_t object have been set.
 *
 * @param duv the DistribUncertValue_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribUncertValue_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertValue_t
 */
LIBSBML_EXTERN
int
DistribUncertValue_hasRequiredAttributes(const DistribUncertValue_t * duv);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribUncertValue_H__ */


