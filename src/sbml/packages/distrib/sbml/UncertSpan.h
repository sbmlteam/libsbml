/**
 * @file UncertSpan.h
 * @brief Definition of the UncertSpan class.
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
 * @class UncertSpan
 * @sbmlbrief{distrib} TODO:Definition of the UncertSpan class.
 */


#ifndef UncertSpan_H__
#define UncertSpan_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/UncertParameter.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN UncertSpan : public UncertParameter
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mVarLower;
  double mValueLower;
  bool mIsSetValueLower;
  std::string mVarUpper;
  double mValueUpper;
  bool mIsSetValueUpper;

  /** @endcond */

public:

  /**
   * Creates a new UncertSpan using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this UncertSpan.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * UncertSpan.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this UncertSpan.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UncertSpan(unsigned int level = DistribExtension::getDefaultLevel(),
             unsigned int version = DistribExtension::getDefaultVersion(),
             unsigned int pkgVersion =
               DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new UncertSpan using the given DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UncertSpan(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for UncertSpan.
   *
   * @param orig the UncertSpan instance to copy.
   */
  UncertSpan(const UncertSpan& orig);


  /**
   * Assignment operator for UncertSpan.
   *
   * @param rhs the UncertSpan object whose values are to be used as the basis
   * of the assignment.
   */
  UncertSpan& operator=(const UncertSpan& rhs);


  /**
   * Creates and returns a deep copy of this UncertSpan object.
   *
   * @return a (deep) copy of this UncertSpan object.
   */
  virtual UncertSpan* clone() const;


  /**
   * Destructor for UncertSpan.
   */
  virtual ~UncertSpan();


  /**
   * Returns the value of the "varLower" attribute of this UncertSpan.
   *
   * @return the value of the "varLower" attribute of this UncertSpan as a
   * string.
   */
  const std::string& getVarLower() const;


  /**
   * Returns the value of the "valueLower" attribute of this UncertSpan.
   *
   * @return the value of the "valueLower" attribute of this UncertSpan as a
   * double.
   */
  double getValueLower() const;


  /**
   * Returns the value of the "varUpper" attribute of this UncertSpan.
   *
   * @return the value of the "varUpper" attribute of this UncertSpan as a
   * string.
   */
  const std::string& getVarUpper() const;


  /**
   * Returns the value of the "valueUpper" attribute of this UncertSpan.
   *
   * @return the value of the "valueUpper" attribute of this UncertSpan as a
   * double.
   */
  double getValueUpper() const;


  /**
   * Predicate returning @c true if this UncertSpan's "varLower" attribute is
   * set.
   *
   * @return @c true if this UncertSpan's "varLower" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetVarLower() const;


  /**
   * Predicate returning @c true if this UncertSpan's "valueLower" attribute is
   * set.
   *
   * @return @c true if this UncertSpan's "valueLower" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetValueLower() const;


  /**
   * Predicate returning @c true if this UncertSpan's "varUpper" attribute is
   * set.
   *
   * @return @c true if this UncertSpan's "varUpper" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetVarUpper() const;


  /**
   * Predicate returning @c true if this UncertSpan's "valueUpper" attribute is
   * set.
   *
   * @return @c true if this UncertSpan's "valueUpper" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetValueUpper() const;


  /**
   * Sets the value of the "varLower" attribute of this UncertSpan.
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
   * Sets the value of the "valueLower" attribute of this UncertSpan.
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
   * Sets the value of the "varUpper" attribute of this UncertSpan.
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
   * Sets the value of the "valueUpper" attribute of this UncertSpan.
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
   * Unsets the value of the "varLower" attribute of this UncertSpan.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVarLower();


  /**
   * Unsets the value of the "valueLower" attribute of this UncertSpan.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValueLower();


  /**
   * Unsets the value of the "varUpper" attribute of this UncertSpan.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVarUpper();


  /**
   * Unsets the value of the "valueUpper" attribute of this UncertSpan.
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
   * Returns the XML element name of this UncertSpan object.
   *
   * For UncertSpan, the XML element name is always @c "uncertSpan".
   *
   * @return the name of this element, i.e. @c "uncertSpan".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this UncertSpan object.
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
   * UncertSpan object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * UncertSpan have been set, otherwise @c false is returned.
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
   * Gets the value of the "attributeName" attribute of this UncertSpan.
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
   * Gets the value of the "attributeName" attribute of this UncertSpan.
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
   * Gets the value of the "attributeName" attribute of this UncertSpan.
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
   * Gets the value of the "attributeName" attribute of this UncertSpan.
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
   * Gets the value of the "attributeName" attribute of this UncertSpan.
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
   * Predicate returning @c true if this UncertSpan's attribute "attributeName"
   * is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this UncertSpan's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this UncertSpan.
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
   * Sets the value of the "attributeName" attribute of this UncertSpan.
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
   * Sets the value of the "attributeName" attribute of this UncertSpan.
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
   * Sets the value of the "attributeName" attribute of this UncertSpan.
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
   * Sets the value of the "attributeName" attribute of this UncertSpan.
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
   * Unsets the value of the "attributeName" attribute of this UncertSpan.
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
 * Creates a new UncertSpan_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this UncertSpan_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * UncertSpan_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this UncertSpan_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
UncertSpan_t *
UncertSpan_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this UncertSpan_t object.
 *
 * @param us the UncertSpan_t structure.
 *
 * @return a (deep) copy of this UncertSpan_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
UncertSpan_t*
UncertSpan_clone(const UncertSpan_t* us);


/**
 * Frees this UncertSpan_t object.
 *
 * @param us the UncertSpan_t structure.
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
void
UncertSpan_free(UncertSpan_t* us);


/**
 * Returns the value of the "varLower" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure whose varLower is sought.
 *
 * @return the value of the "varLower" attribute of this UncertSpan_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
char *
UncertSpan_getVarLower(const UncertSpan_t * us);


/**
 * Returns the value of the "valueLower" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure whose valueLower is sought.
 *
 * @return the value of the "valueLower" attribute of this UncertSpan_t as a
 * double.
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
double
UncertSpan_getValueLower(const UncertSpan_t * us);


/**
 * Returns the value of the "varUpper" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure whose varUpper is sought.
 *
 * @return the value of the "varUpper" attribute of this UncertSpan_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
char *
UncertSpan_getVarUpper(const UncertSpan_t * us);


/**
 * Returns the value of the "valueUpper" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure whose valueUpper is sought.
 *
 * @return the value of the "valueUpper" attribute of this UncertSpan_t as a
 * double.
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
double
UncertSpan_getValueUpper(const UncertSpan_t * us);


/**
 * Predicate returning @c 1 (true) if this UncertSpan_t's "varLower" attribute
 * is set.
 *
 * @param us the UncertSpan_t structure.
 *
 * @return @c 1 (true) if this UncertSpan_t's "varLower" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_isSetVarLower(const UncertSpan_t * us);


/**
 * Predicate returning @c 1 (true) if this UncertSpan_t's "valueLower"
 * attribute is set.
 *
 * @param us the UncertSpan_t structure.
 *
 * @return @c 1 (true) if this UncertSpan_t's "valueLower" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_isSetValueLower(const UncertSpan_t * us);


/**
 * Predicate returning @c 1 (true) if this UncertSpan_t's "varUpper" attribute
 * is set.
 *
 * @param us the UncertSpan_t structure.
 *
 * @return @c 1 (true) if this UncertSpan_t's "varUpper" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_isSetVarUpper(const UncertSpan_t * us);


/**
 * Predicate returning @c 1 (true) if this UncertSpan_t's "valueUpper"
 * attribute is set.
 *
 * @param us the UncertSpan_t structure.
 *
 * @return @c 1 (true) if this UncertSpan_t's "valueUpper" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_isSetValueUpper(const UncertSpan_t * us);


/**
 * Sets the value of the "varLower" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure.
 *
 * @param varLower const char * value of the "varLower" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_setVarLower(UncertSpan_t * us, const char * varLower);


/**
 * Sets the value of the "valueLower" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure.
 *
 * @param valueLower double value of the "valueLower" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_setValueLower(UncertSpan_t * us, double valueLower);


/**
 * Sets the value of the "varUpper" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure.
 *
 * @param varUpper const char * value of the "varUpper" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_setVarUpper(UncertSpan_t * us, const char * varUpper);


/**
 * Sets the value of the "valueUpper" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure.
 *
 * @param valueUpper double value of the "valueUpper" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_setValueUpper(UncertSpan_t * us, double valueUpper);


/**
 * Unsets the value of the "varLower" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_unsetVarLower(UncertSpan_t * us);


/**
 * Unsets the value of the "valueLower" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_unsetValueLower(UncertSpan_t * us);


/**
 * Unsets the value of the "varUpper" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_unsetVarUpper(UncertSpan_t * us);


/**
 * Unsets the value of the "valueUpper" attribute of this UncertSpan_t.
 *
 * @param us the UncertSpan_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_unsetValueUpper(UncertSpan_t * us);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UncertSpan_t object have been set.
 *
 * @param us the UncertSpan_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * UncertSpan_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertSpan_t
 */
LIBSBML_EXTERN
int
UncertSpan_hasRequiredAttributes(const UncertSpan_t * us);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !UncertSpan_H__ */


