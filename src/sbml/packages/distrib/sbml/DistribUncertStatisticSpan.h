/**
 * @file DistribUncertStatisticSpan.h
 * @brief Definition of the DistribUncertStatisticSpan class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @class DistribUncertStatisticSpan
 * @sbmlbrief{distrib} TODO:Definition of the DistribUncertStatisticSpan class.
 */


#ifndef DistribUncertStatisticSpan_H__
#define DistribUncertStatisticSpan_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribUncertStatisticSpan : public DistribBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mVarLower;
  double mValueLower;
  bool mIsSetValueLower;
  std::string mVarUpper;
  double mValueUpper;
  bool mIsSetValueUpper;
  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new DistribUncertStatisticSpan using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribUncertStatisticSpan.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribUncertStatisticSpan.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribUncertStatisticSpan.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertStatisticSpan(
                             unsigned int level =
                               DistribExtension::getDefaultLevel(),
                             unsigned int version =
                               DistribExtension::getDefaultVersion(),
                             unsigned int pkgVersion =
                               DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribUncertStatisticSpan using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertStatisticSpan(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribUncertStatisticSpan.
   *
   * @param orig the DistribUncertStatisticSpan instance to copy.
   */
  DistribUncertStatisticSpan(const DistribUncertStatisticSpan& orig);


  /**
   * Assignment operator for DistribUncertStatisticSpan.
   *
   * @param rhs the DistribUncertStatisticSpan object whose values are to be
   * used as the basis of the assignment.
   */
  DistribUncertStatisticSpan& operator=(const DistribUncertStatisticSpan& rhs);


  /**
   * Creates and returns a deep copy of this DistribUncertStatisticSpan object.
   *
   * @return a (deep) copy of this DistribUncertStatisticSpan object.
   */
  virtual DistribUncertStatisticSpan* clone() const;


  /**
   * Destructor for DistribUncertStatisticSpan.
   */
  virtual ~DistribUncertStatisticSpan();


  /**
   * Returns the value of the "varLower" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @return the value of the "varLower" attribute of this
   * DistribUncertStatisticSpan as a string.
   */
  const std::string& getVarLower() const;


  /**
   * Returns the value of the "valueLower" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @return the value of the "valueLower" attribute of this
   * DistribUncertStatisticSpan as a double.
   */
  double getValueLower() const;


  /**
   * Returns the value of the "varUpper" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @return the value of the "varUpper" attribute of this
   * DistribUncertStatisticSpan as a string.
   */
  const std::string& getVarUpper() const;


  /**
   * Returns the value of the "valueUpper" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @return the value of the "valueUpper" attribute of this
   * DistribUncertStatisticSpan as a double.
   */
  double getValueUpper() const;


  /**
   * Predicate returning @c true if this DistribUncertStatisticSpan's
   * "varLower" attribute is set.
   *
   * @return @c true if this DistribUncertStatisticSpan's "varLower" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetVarLower() const;


  /**
   * Predicate returning @c true if this DistribUncertStatisticSpan's
   * "valueLower" attribute is set.
   *
   * @return @c true if this DistribUncertStatisticSpan's "valueLower"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetValueLower() const;


  /**
   * Predicate returning @c true if this DistribUncertStatisticSpan's
   * "varUpper" attribute is set.
   *
   * @return @c true if this DistribUncertStatisticSpan's "varUpper" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetVarUpper() const;


  /**
   * Predicate returning @c true if this DistribUncertStatisticSpan's
   * "valueUpper" attribute is set.
   *
   * @return @c true if this DistribUncertStatisticSpan's "valueUpper"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetValueUpper() const;


  /**
   * Sets the value of the "varLower" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @param varLower std::string& value of the "varLower" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVarLower(const std::string& varLower);


  /**
   * Sets the value of the "valueLower" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @param valueLower double value of the "valueLower" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setValueLower(double valueLower);


  /**
   * Sets the value of the "varUpper" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @param varUpper std::string& value of the "varUpper" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVarUpper(const std::string& varUpper);


  /**
   * Sets the value of the "valueUpper" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @param valueUpper double value of the "valueUpper" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setValueUpper(double valueUpper);


  /**
   * Unsets the value of the "varLower" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVarLower();


  /**
   * Unsets the value of the "valueLower" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValueLower();


  /**
   * Unsets the value of the "varUpper" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVarUpper();


  /**
   * Unsets the value of the "valueUpper" attribute of this
   * DistribUncertStatisticSpan.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValueUpper();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this DistribUncertStatisticSpan object.
   *
   * For DistribUncertStatisticSpan, the XML element name is always
   * @c "uncertStatisticSpan".
   *
   * @return the name of this element, i.e. @c "uncertStatisticSpan".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this DistribUncertStatisticSpan object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this DistribUncertStatisticSpan object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_UNCERTSTATISTICSPAN, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribUncertStatisticSpan object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribUncertStatisticSpan have been set, otherwise @c false is returned.
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
   * DistribUncertStatisticSpan.
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
   * DistribUncertStatisticSpan.
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
   * DistribUncertStatisticSpan.
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
   * DistribUncertStatisticSpan.
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
   * DistribUncertStatisticSpan.
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
   * Predicate returning @c true if this DistribUncertStatisticSpan's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribUncertStatisticSpan's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribUncertStatisticSpan.
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
   * DistribUncertStatisticSpan.
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
   * DistribUncertStatisticSpan.
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
   * DistribUncertStatisticSpan.
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
   * DistribUncertStatisticSpan.
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
   * DistribUncertStatisticSpan.
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
 * Creates a new DistribUncertStatisticSpan_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribUncertStatisticSpan_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribUncertStatisticSpan_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribUncertStatisticSpan_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t *
DistribUncertStatisticSpan_create(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribUncertStatisticSpan_t object.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @return a (deep) copy of this DistribUncertStatisticSpan_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatisticSpan_clone(const DistribUncertStatisticSpan_t* duss);


/**
 * Frees this DistribUncertStatisticSpan_t object.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
void
DistribUncertStatisticSpan_free(DistribUncertStatisticSpan_t* duss);


/**
 * Returns the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure whose varLower is
 * sought.
 *
 * @return the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
char *
DistribUncertStatisticSpan_getVarLower(const DistribUncertStatisticSpan_t *
  duss);


/**
 * Returns the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure whose valueLower is
 * sought.
 *
 * @return the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan_t as a double.
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
double
DistribUncertStatisticSpan_getValueLower(const DistribUncertStatisticSpan_t *
  duss);


/**
 * Returns the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure whose varUpper is
 * sought.
 *
 * @return the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
char *
DistribUncertStatisticSpan_getVarUpper(const DistribUncertStatisticSpan_t *
  duss);


/**
 * Returns the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure whose valueUpper is
 * sought.
 *
 * @return the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan_t as a double.
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
double
DistribUncertStatisticSpan_getValueUpper(const DistribUncertStatisticSpan_t *
  duss);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatisticSpan_t's
 * "varLower" attribute is set.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatisticSpan_t's "varLower"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_isSetVarLower(const DistribUncertStatisticSpan_t *
  duss);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatisticSpan_t's
 * "valueLower" attribute is set.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatisticSpan_t's "valueLower"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_isSetValueLower(const DistribUncertStatisticSpan_t *
  duss);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatisticSpan_t's
 * "varUpper" attribute is set.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatisticSpan_t's "varUpper"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_isSetVarUpper(const DistribUncertStatisticSpan_t *
  duss);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatisticSpan_t's
 * "valueUpper" attribute is set.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatisticSpan_t's "valueUpper"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_isSetValueUpper(const DistribUncertStatisticSpan_t *
  duss);


/**
 * Sets the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @param varLower const char * value of the "varLower" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_setVarLower(DistribUncertStatisticSpan_t * duss,
                                       const char * varLower);


/**
 * Sets the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @param valueLower double value of the "valueLower" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_setValueLower(DistribUncertStatisticSpan_t * duss,
                                         double valueLower);


/**
 * Sets the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @param varUpper const char * value of the "varUpper" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_setVarUpper(DistribUncertStatisticSpan_t * duss,
                                       const char * varUpper);


/**
 * Sets the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @param valueUpper double value of the "valueUpper" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_setValueUpper(DistribUncertStatisticSpan_t * duss,
                                         double valueUpper);


/**
 * Unsets the value of the "varLower" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_unsetVarLower(DistribUncertStatisticSpan_t * duss);


/**
 * Unsets the value of the "valueLower" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_unsetValueLower(DistribUncertStatisticSpan_t *
  duss);


/**
 * Unsets the value of the "varUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_unsetVarUpper(DistribUncertStatisticSpan_t * duss);


/**
 * Unsets the value of the "valueUpper" attribute of this
 * DistribUncertStatisticSpan_t.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_unsetValueUpper(DistribUncertStatisticSpan_t *
  duss);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertStatisticSpan_t object have been set.
 *
 * @param duss the DistribUncertStatisticSpan_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribUncertStatisticSpan_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribUncertStatisticSpan_t
 */
LIBSBML_EXTERN
int
DistribUncertStatisticSpan_hasRequiredAttributes(const
  DistribUncertStatisticSpan_t * duss);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribUncertStatisticSpan_H__ */


