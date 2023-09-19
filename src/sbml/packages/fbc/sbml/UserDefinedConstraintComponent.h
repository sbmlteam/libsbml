/**
 * @file UserDefinedConstraintComponent.h
 * @brief Definition of the UserDefinedConstraintComponent class.
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
 * @class UserDefinedConstraintComponent
 * @sbmlbrief{fbc} TODO:Definition of the UserDefinedConstraintComponent class.
 */

/**
 * <!-- ~ ~ ~ ~ ~ Start of common documentation strings ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 * The following text is used as common documentation blocks copied multiple
 * times elsewhere in this file. The use of @class is a hack needed because
 * Doxygen's @copydetails command has limited functionality. Symbols
 * beginning with "doc_" are marked as ignored in our Doxygen configuration.
 * ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ -->
 *
 *
 * @class doc_userdefinedconstraintcomponent_variableType
 *
 * @par
 * The attribute "variableType" on a UserDefinedConstraintComponent object is
 * used to TODO:add explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Fbc specification, the following are the
 * allowable values for "variableType":
 * <ul>
 * <li> @c "linear", TODO:add description
 *
 * <li> @c "quadratic", TODO:add description
 *
 * </ul>
 */


#ifndef UserDefinedConstraintComponent_H__
#define UserDefinedConstraintComponent_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN UserDefinedConstraintComponent : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mCoefficient;
  std::string mVariable;
  std::string mVariable2;
  FbcVariableType_t mVariableType;

  /** @endcond */

public:

  /**
   * Creates a new UserDefinedConstraintComponent using the given SBML Level,
   * Version and &ldquo;fbc&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * UserDefinedConstraintComponent.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * UserDefinedConstraintComponent.
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this
   * UserDefinedConstraintComponent.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UserDefinedConstraintComponent(
                                 unsigned int level =
                                   FbcExtension::getDefaultLevel(),
                                 unsigned int version =
                                   FbcExtension::getDefaultVersion(),
                                 unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new UserDefinedConstraintComponent using the given
   * FbcPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param fbcns the FbcPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UserDefinedConstraintComponent(FbcPkgNamespaces *fbcns);


  /**
   * Copy constructor for UserDefinedConstraintComponent.
   *
   * @param orig the UserDefinedConstraintComponent instance to copy.
   */
  UserDefinedConstraintComponent(const UserDefinedConstraintComponent& orig);


  /**
   * Assignment operator for UserDefinedConstraintComponent.
   *
   * @param rhs the UserDefinedConstraintComponent object whose values are to
   * be used as the basis of the assignment.
   */
  UserDefinedConstraintComponent& operator=(const
    UserDefinedConstraintComponent& rhs);


  /**
   * Creates and returns a deep copy of this UserDefinedConstraintComponent
   * object.
   *
   * @return a (deep) copy of this UserDefinedConstraintComponent object.
   */
  virtual UserDefinedConstraintComponent* clone() const;


  /**
   * Destructor for UserDefinedConstraintComponent.
   */
  virtual ~UserDefinedConstraintComponent();


  /**
   * Returns the value of the "id" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @return the value of the "id" attribute of this
   * UserDefinedConstraintComponent as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @return the value of the "name" attribute of this
   * UserDefinedConstraintComponent as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "coefficient" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @return the value of the "coefficient" attribute of this
   * UserDefinedConstraintComponent as a string.
   */
  const std::string& getCoefficient() const;


  /**
   * Returns the value of the "variable" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @return the value of the "variable" attribute of this
   * UserDefinedConstraintComponent as a string.
   */
  const std::string& getVariable() const;


  /**
   * Returns the value of the "variable2" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @return the value of the "variable2" attribute of this
   * UserDefinedConstraintComponent as a string.
   */
  const std::string& getVariable2() const;


  /**
   * Returns the value of the "variableType" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @return the value of the "variableType" attribute of this
   * UserDefinedConstraintComponent as a FbcVariableType_t.
   *
   * @copydetails doc_userdefinedconstraintcomponent_variableType
   * @if clike The value is drawn from the enumeration @ref FbcVariableType_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{FBC_VARIABLE_TYPE_LINEAR, FbcVariableType_t}
   * @li @sbmlconstant{FBC_VARIABLE_TYPE_QUADRATIC, FbcVariableType_t}
   * @li @sbmlconstant{FBC_VARIABLE_TYPE_INVALID, FbcVariableType_t}
   */
  FbcVariableType_t getVariableType() const;


  /**
   * Returns the value of the "variableType" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @return the value of the "variableType" attribute of this
   * UserDefinedConstraintComponent as a string.
   *
   * @copydetails doc_userdefinedconstraintcomponent_variableType
   * The possible values returned by this method are:
   * @li @c "linear"
   * @li @c "quadratic"
   * @li @c "invalid FbcVariableType value"
   */
  std::string getVariableTypeAsString() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraintComponent's "id"
   * attribute is set.
   *
   * @return @c true if this UserDefinedConstraintComponent's "id" attribute
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraintComponent's
   * "name" attribute is set.
   *
   * @return @c true if this UserDefinedConstraintComponent's "name" attribute
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraintComponent's
   * "coefficient" attribute is set.
   *
   * @return @c true if this UserDefinedConstraintComponent's "coefficient"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetCoefficient() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraintComponent's
   * "variable" attribute is set.
   *
   * @return @c true if this UserDefinedConstraintComponent's "variable"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetVariable() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraintComponent's
   * "variable2" attribute is set.
   *
   * @return @c true if this UserDefinedConstraintComponent's "variable2"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetVariable2() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraintComponent's
   * "variableType" attribute is set.
   *
   * @return @c true if this UserDefinedConstraintComponent's "variableType"
   * attribute has been set, otherwise @c false is returned.
   *
   * @copydetails doc_userdefinedconstraintcomponent_variableType
   */
  bool isSetVariableType() const;


  /**
   * Sets the value of the "id" attribute of this
   * UserDefinedConstraintComponent.
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
   * Sets the value of the "name" attribute of this
   * UserDefinedConstraintComponent.
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
   * Sets the value of the "coefficient" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @param coefficient std::string& value of the "coefficient" attribute to be
   * set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p coefficient = @c NULL or an empty string is
   * equivalent to calling unsetCoefficient().
   */
  int setCoefficient(const std::string& coefficient);


  /**
   * Sets the value of the "variable" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @param variable std::string& value of the "variable" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVariable(const std::string& variable);


  /**
   * Sets the value of the "variable2" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @param variable std::string& value of the "variable2" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVariable2(const std::string& variable);


  /**
   * Sets the value of the "variableType" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @param variableType @if clike FbcVariableType_t@else int@endif value of
   * the "variableType" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_userdefinedconstraintcomponent_variableType
   */
  int setVariableType(const FbcVariableType_t variableType);


  /**
   * Sets the value of the "variableType" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @param variableType std::string& of the "variableType" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_userdefinedconstraintcomponent_variableType
   */
  int setVariableType(const std::string& variableType);


  /**
   * Unsets the value of the "id" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "coefficient" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCoefficient();


  /**
   * Unsets the value of the "variable" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVariable();


  /**
   * Unsets the value of the "variable2" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVariable2();

  /**
   * Unsets the value of the "variableType" attribute of this
   * UserDefinedConstraintComponent.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_userdefinedconstraintcomponent_variableType
   */
  int unsetVariableType();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this UserDefinedConstraintComponent
   * object.
   *
   * For UserDefinedConstraintComponent, the XML element name is always
   * @c "userDefinedConstraintComponent".
   *
   * @return the name of this element, i.e.
   * @c "userDefinedConstraintComponent".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this UserDefinedConstraintComponent
   * object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_FBC_USERDEFINEDCONSTRAINTCOMPONENT, SBMLFbcTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * UserDefinedConstraintComponent object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * UserDefinedConstraintComponent have been set, otherwise @c false is
   * returned.
   *
   *
   * @note The required attributes for the UserDefinedConstraintComponent
   * object are:
   * @li "coefficient"
   * @li "variable"
   * @li "variableType"
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
   * UserDefinedConstraintComponent.
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
   * UserDefinedConstraintComponent.
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
   * UserDefinedConstraintComponent.
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
   * UserDefinedConstraintComponent.
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
   * UserDefinedConstraintComponent.
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
   * Predicate returning @c true if this UserDefinedConstraintComponent's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this UserDefinedConstraintComponent's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * UserDefinedConstraintComponent.
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
   * UserDefinedConstraintComponent.
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
   * UserDefinedConstraintComponent.
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
   * UserDefinedConstraintComponent.
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
   * UserDefinedConstraintComponent.
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
   * UserDefinedConstraintComponent.
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
 * Creates a new UserDefinedConstraintComponent_t using the given SBML Level,
 * Version and &ldquo;fbc&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * UserDefinedConstraintComponent_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * UserDefinedConstraintComponent_t.
 *
 * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this
 * UserDefinedConstraintComponent_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t *
UserDefinedConstraintComponent_create(unsigned int level,
                                      unsigned int version,
                                      unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this UserDefinedConstraintComponent_t
 * object.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @return a (deep) copy of this UserDefinedConstraintComponent_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraintComponent_clone(const UserDefinedConstraintComponent_t*
  udcc);


/**
 * Frees this UserDefinedConstraintComponent_t object.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
void
UserDefinedConstraintComponent_free(UserDefinedConstraintComponent_t* udcc);


/**
 * Returns the value of the "id" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure whose id is
 * sought.
 *
 * @return the value of the "id" attribute of this
 * UserDefinedConstraintComponent_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getId(const UserDefinedConstraintComponent_t *
  udcc);


/**
 * Returns the value of the "name" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure whose name is
 * sought.
 *
 * @return the value of the "name" attribute of this
 * UserDefinedConstraintComponent_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getName(const UserDefinedConstraintComponent_t *
  udcc);


/**
 * Returns the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure whose coefficient
 * is sought.
 *
 * @return the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getCoefficient(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Returns the value of the "variable" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure whose variable is
 * sought.
 *
 * @return the value of the "variable" attribute of this
 * UserDefinedConstraintComponent_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getVariable(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Returns the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure whose variable2
 * is sought.
 *
 * @return the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getVariable2(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Returns the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure whose
 * variableType is sought.
 *
 * @return the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t as a FbcVariableType_t.
 *
 * @copydetails doc_userdefinedconstraintcomponent_variableType
 * @if clike The value is drawn from the enumeration @ref FbcVariableType_t
 * @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{FBC_VARIABLE_TYPE_LINEAR, FbcVariableType_t}
 * @li @sbmlconstant{FBC_VARIABLE_TYPE_QUADRATIC, FbcVariableType_t}
 * @li @sbmlconstant{FBC_VARIABLE_TYPE_INVALID, FbcVariableType_t}
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
FbcVariableType_t
UserDefinedConstraintComponent_getVariableType(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Returns the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure whose
 * variableType is sought.
 *
 * @return the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t as a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_userdefinedconstraintcomponent_variableType
 * The possible values returned by this method are:
 * @li @c "linear"
 * @li @c "quadratic"
 * @li @c "invalid FbcVariableType value"
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getVariableTypeAsString(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Returns the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure whose variable is
 * sought.
 *
 * @return the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraintComponent_getVariable2(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "id" attribute is set.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraintComponent_t's "id"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetId(const UserDefinedConstraintComponent_t *
  udcc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "name" attribute is set.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraintComponent_t's "name"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetName(const UserDefinedConstraintComponent_t
  * udcc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "coefficient" attribute is set.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraintComponent_t's "coefficient"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetCoefficient(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "variable" attribute is set.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraintComponent_t's "variable"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetVariable(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "variable2" attribute is set.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraintComponent_t's "variable2"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetVariable2(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "variableType" attribute is set.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "variableType" attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_userdefinedconstraintcomponent_variableType
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetVariableType(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraintComponent_t's
 * "variable2" attribute is set.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraintComponent_t's "variable2"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_isSetVariable2(const
  UserDefinedConstraintComponent_t * udcc);


/**
 * Sets the value of the "id" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling UserDefinedConstraintComponent_unsetId().
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setId(UserDefinedConstraintComponent_t * udcc,
                                     const char * id);


/**
 * Sets the value of the "name" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling UserDefinedConstraintComponent_unsetName().
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setName(UserDefinedConstraintComponent_t * udcc,
                                       const char * name);


/**
 * Sets the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @param coefficient const char * value of the "coefficient" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p coefficient = @c NULL or an empty string is
 * equivalent to calling UserDefinedConstraintComponent_unsetCoefficient().
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setCoefficient(
                                              UserDefinedConstraintComponent_t
                                                * udcc,
                                              const char * coefficient);


/**
 * Sets the value of the "variable" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @param variable const char * value of the "variable" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setVariable(
                                           UserDefinedConstraintComponent_t *
                                             udcc,
                                           const char * variable);


/**
 * Sets the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @param variable2 const char * value of the "variable2" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setVariable2(
                                            UserDefinedConstraintComponent_t *
                                              udcc,
                                            const char * variable2);


/**
 * Sets the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @param variableType FbcVariableType_t value of the "variableType" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_userdefinedconstraintcomponent_variableType
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setVariableType(
                                               UserDefinedConstraintComponent_t
                                                 * udcc,
                                               FbcVariableType_t variableType);


/**
 * Sets the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @param variableType const char * of the "variableType" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_userdefinedconstraintcomponent_variableType
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_setVariableTypeAsString(
                                                       UserDefinedConstraintComponent_t
                                                         * udcc,
                                                       const char *
                                                         variableType);


/**
 * Unsets the value of the "id" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetId(UserDefinedConstraintComponent_t *
  udcc);


/**
 * Unsets the value of the "name" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetName(UserDefinedConstraintComponent_t *
  udcc);


/**
 * Unsets the value of the "coefficient" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetCoefficient(UserDefinedConstraintComponent_t
  * udcc);


/**
 * Unsets the value of the "variable" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetVariable(UserDefinedConstraintComponent_t *
  udcc);


/**
 * Unsets the value of the "variable2" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetVariable2(UserDefinedConstraintComponent_t
  * udcc);


/**
 * Unsets the value of the "variableType" attribute of this
 * UserDefinedConstraintComponent_t.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_userdefinedconstraintcomponent_variableType
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_unsetVariableType(UserDefinedConstraintComponent_t
  * udcc);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UserDefinedConstraintComponent_t object have been set.
 *
 * @param udcc the UserDefinedConstraintComponent_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * UserDefinedConstraintComponent_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required attributes for the UserDefinedConstraintComponent_t
 * object are:
 * @li "coefficient"
 * @li "variable"
 * @li "variableType"
 *
 * @memberof UserDefinedConstraintComponent_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraintComponent_hasRequiredAttributes(const
  UserDefinedConstraintComponent_t * udcc);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !UserDefinedConstraintComponent_H__ */


