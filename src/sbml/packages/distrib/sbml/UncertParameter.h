/**
 * @file UncertParameter.h
 * @brief Definition of the UncertParameter class.
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
 * @class UncertParameter
 * @sbmlbrief{distrib} TODO:Definition of the UncertParameter class.
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
 * @class doc_uncertparameter_type
 *
 * @par
 * The attribute "type" on a UncertParameter object is used to TODO:add
 * explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Distrib specification, the following are the
 * allowable values for "type":
 * <ul>
 * <li> @c "distribution", TODO:add description
 *
 * <li> @c "externalParameter", TODO:add description
 *
 * <li> @c "coeffientOfVariation", TODO:add description
 *
 * <li> @c "kurtosis", TODO:add description
 *
 * <li> @c "mean", TODO:add description
 *
 * <li> @c "median", TODO:add description
 *
 * <li> @c "mode", TODO:add description
 *
 * <li> @c "sampleSize", TODO:add description
 *
 * <li> @c "skewness", TODO:add description
 *
 * <li> @c "standardDeviation", TODO:add description
 *
 * <li> @c "standardError", TODO:add description
 *
 * <li> @c "variance", TODO:add description
 *
 * <li> @c "confidenceInterval", TODO:add description
 *
 * <li> @c "credibleInterval", TODO:add description
 *
 * <li> @c "interquartileRange", TODO:add description
 *
 * <li> @c "range", TODO:add description
 *
 * </ul>
 */


#ifndef UncertParameter_H__
#define UncertParameter_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/math/ASTNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class ListOfUncertParameters;

class LIBSBML_EXTERN UncertParameter : public DistribBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  double mValue;
  bool mIsSetValue;
  std::string mVar;
  std::string mUnits;
  UncertType_t mType;
  std::string mDefinitionURL;
  ListOfUncertParameters * mUncertParameters;
  ASTNode* mMath;

  /** @endcond */

public:

  /**
   * Creates a new UncertParameter using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * UncertParameter.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * UncertParameter.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this UncertParameter.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UncertParameter(unsigned int level = DistribExtension::getDefaultLevel(),
                  unsigned int version = DistribExtension::getDefaultVersion(),
                  unsigned int pkgVersion =
                    DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new UncertParameter using the given DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UncertParameter(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for UncertParameter.
   *
   * @param orig the UncertParameter instance to copy.
   */
  UncertParameter(const UncertParameter& orig);


  /**
   * Assignment operator for UncertParameter.
   *
   * @param rhs the UncertParameter object whose values are to be used as the
   * basis of the assignment.
   */
  UncertParameter& operator=(const UncertParameter& rhs);


  /**
   * Creates and returns a deep copy of this UncertParameter object.
   *
   * @return a (deep) copy of this UncertParameter object.
   */
  virtual UncertParameter* clone() const;


  /**
   * Destructor for UncertParameter.
   */
  virtual ~UncertParameter();


  /**
   * Returns the value of the "value" attribute of this UncertParameter.
   *
   * @return the value of the "value" attribute of this UncertParameter as a
   * double.
   */
  double getValue() const;


  /**
   * Returns the value of the "var" attribute of this UncertParameter.
   *
   * @return the value of the "var" attribute of this UncertParameter as a
   * string.
   */
  const std::string& getVar() const;


  /**
   * Returns the value of the "units" attribute of this UncertParameter.
   *
   * @return the value of the "units" attribute of this UncertParameter as a
   * string.
   */
  const std::string& getUnits() const;


  /**
   * Returns the value of the "type" attribute of this UncertParameter.
   *
   * @return the value of the "type" attribute of this UncertParameter as a
   * UncertType_t.
   *
   * @copydetails doc_uncertparameter_type
   * @if clike The value is drawn from the enumeration @ref UncertType_t @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_DISTRIBUTION, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_EXTERNALPARAMETER, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_COEFFIENTOFVARIATION, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_KURTOSIS, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_MEAN, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_MEDIAN, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_MODE, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_SAMPLESIZE, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_SKEWNESS, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_STANDARDDEVIATION, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_STANDARDERROR, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_VARIANCE, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_CONFIDENCEINTERVAL, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_CREDIBLEINTERVAL, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_INTERQUARTILERANGE, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_RANGE, UncertType_t}
   * @li @sbmlconstant{DISTRIB_UNCERTTYPE_INVALID, UncertType_t}
   */
  UncertType_t getType() const;


  /**
   * Returns the value of the "type" attribute of this UncertParameter.
   *
   * @return the value of the "type" attribute of this UncertParameter as a
   * string.
   *
   * @copydetails doc_uncertparameter_type
   * The possible values returned by this method are:
   * @li @c "distribution"
   * @li @c "externalParameter"
   * @li @c "coeffientOfVariation"
   * @li @c "kurtosis"
   * @li @c "mean"
   * @li @c "median"
   * @li @c "mode"
   * @li @c "sampleSize"
   * @li @c "skewness"
   * @li @c "standardDeviation"
   * @li @c "standardError"
   * @li @c "variance"
   * @li @c "confidenceInterval"
   * @li @c "credibleInterval"
   * @li @c "interquartileRange"
   * @li @c "range"
   * @li @c "invalid UncertType value"
   */
  std::string getTypeAsString() const;


  /**
   * Returns the value of the "definitionURL" attribute of this
   * UncertParameter.
   *
   * @return the value of the "definitionURL" attribute of this UncertParameter
   * as a string.
   */
  const std::string& getDefinitionURL() const;


  /**
   * Predicate returning @c true if this UncertParameter's "value" attribute is
   * set.
   *
   * @return @c true if this UncertParameter's "value" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetValue() const;


  /**
   * Predicate returning @c true if this UncertParameter's "var" attribute is
   * set.
   *
   * @return @c true if this UncertParameter's "var" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetVar() const;


  /**
   * Predicate returning @c true if this UncertParameter's "units" attribute is
   * set.
   *
   * @return @c true if this UncertParameter's "units" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetUnits() const;


  /**
   * Predicate returning @c true if this UncertParameter's "type" attribute is
   * set.
   *
   * @return @c true if this UncertParameter's "type" attribute has been set,
   * otherwise @c false is returned.
   *
   * @copydetails doc_uncertparameter_type
   */
  bool isSetType() const;


  /**
   * Predicate returning @c true if this UncertParameter's "definitionURL"
   * attribute is set.
   *
   * @return @c true if this UncertParameter's "definitionURL" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetDefinitionURL() const;


  /**
   * Sets the value of the "value" attribute of this UncertParameter.
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
   * Sets the value of the "var" attribute of this UncertParameter.
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
   * Sets the value of the "units" attribute of this UncertParameter.
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
   * Sets the value of the "type" attribute of this UncertParameter.
   *
   * @param type @if clike UncertType_t@else int@endif value of the "type"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_uncertparameter_type
   */
  int setType(const UncertType_t type);


  /**
   * Sets the value of the "type" attribute of this UncertParameter.
   *
   * @param type std::string& of the "type" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_uncertparameter_type
   */
  int setType(const std::string& type);


  /**
   * Sets the value of the "definitionURL" attribute of this UncertParameter.
   *
   * @param definitionURL std::string& value of the "definitionURL" attribute
   * to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p definitionURL = @c NULL or an empty string
   * is equivalent to calling unsetDefinitionURL().
   */
  int setDefinitionURL(const std::string& definitionURL);


  /**
   * Unsets the value of the "value" attribute of this UncertParameter.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValue();


  /**
   * Unsets the value of the "var" attribute of this UncertParameter.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVar();


  /**
   * Unsets the value of the "units" attribute of this UncertParameter.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetUnits();


  /**
   * Unsets the value of the "type" attribute of this UncertParameter.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_uncertparameter_type
   */
  int unsetType();


  /**
   * Unsets the value of the "definitionURL" attribute of this UncertParameter.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDefinitionURL();


  /**
   * Returns the value of the "math" element of this UncertParameter.
   *
   * @return the value of the "math" element of this UncertParameter as a
   * ASTNode*.
   */
  const ASTNode* getMath() const;


  /**
   * Returns the value of the "math" element of this UncertParameter.
   *
   * @return the value of the "math" element of this UncertParameter as a
   * ASTNode*.
   */
  ASTNode* getMath();


  /**
   * Predicate returning @c true if this UncertParameter's "math" element is
   * set.
   *
   * @return @c true if this UncertParameter's "math" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetMath() const;


  /**
   * Sets the value of the "math" element of this UncertParameter.
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
   * Unsets the value of the "math" element of this UncertParameter.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMath();


  /**
   * Returns the ListOfUncertParameters * from this UncertParameter.
   *
   * @return the ListOfUncertParameters * from this UncertParameter.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see getUncertParameter(const std::string& sid)
   * @see getUncertParameter(unsigned int n)
   * @see getNumUncertParameters()
   * @see removeUncertParameter(const std::string& sid)
   * @see removeUncertParameter(unsigned int n)
   */
  const ListOfUncertParameters * getListOfUncertParameters() const;


  /**
   * Returns the ListOfUncertParameters * from this UncertParameter.
   *
   * @return the ListOfUncertParameters * from this UncertParameter.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see getUncertParameter(const std::string& sid)
   * @see getUncertParameter(unsigned int n)
   * @see getNumUncertParameters()
   * @see removeUncertParameter(const std::string& sid)
   * @see removeUncertParameter(unsigned int n)
   */
  ListOfUncertParameters * getListOfUncertParameters();


  /**
   * Get an UncertParameter from the UncertParameter.
   *
   * @param n an unsigned int representing the index of the UncertParameter to
   * retrieve.
   *
   * @return the nth UncertParameter in the ListOfUncertParameters * within
   * this UncertParameter or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see getUncertParameter(const std::string& sid)
   * @see getNumUncertParameters()
   * @see removeUncertParameter(const std::string& sid)
   * @see removeUncertParameter(unsigned int n)
   */
  UncertParameter* getUncertParameter(unsigned int n);


  /**
   * Get an UncertParameter from the UncertParameter.
   *
   * @param n an unsigned int representing the index of the UncertParameter to
   * retrieve.
   *
   * @return the nth UncertParameter in the ListOfUncertParameters * within
   * this UncertParameter or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see getUncertParameter(const std::string& sid)
   * @see getNumUncertParameters()
   * @see removeUncertParameter(const std::string& sid)
   * @see removeUncertParameter(unsigned int n)
   */
  const UncertParameter* getUncertParameter(unsigned int n) const;


  /**
   * Get an UncertParameter from the UncertParameter based on the element to
   * which it refers.
   *
   * @param sid a string representing the "var" attribute of the
   * UncertParameter object to retrieve.
   *
   * @return the first UncertParameter in this UncertParameter based on the
   * given var attribute or NULL if no such UncertParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const UncertParameter* getUncertParameterByVar(const std::string& sid) const;


  /**
   * Get an UncertParameter from the UncertParameter based on the element to
   * which it refers.
   *
   * @param sid a string representing the "var" attribute of the
   * UncertParameter object to retrieve.
   *
   * @return the first UncertParameter in this UncertParameter based on the
   * given var attribute or NULL if no such UncertParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  UncertParameter* getUncertParameterByVar(const std::string& sid);


  /**
   * Adds a copy of the given UncertParameter to this UncertParameter.
   *
   * @param up1 the UncertParameter object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createUncertParameter()
   * @see getUncertParameter(const std::string& sid)
   * @see getUncertParameter(unsigned int n)
   * @see getNumUncertParameters()
   * @see removeUncertParameter(const std::string& sid)
   * @see removeUncertParameter(unsigned int n)
   */
  int addUncertParameter(const UncertParameter* up1);

  int addUncertSpan(const UncertSpan* us1);

  /**
   * Get the number of UncertParameter objects in this UncertParameter.
   *
   * @return the number of UncertParameter objects in this UncertParameter.
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see getUncertParameter(const std::string& sid)
   * @see getUncertParameter(unsigned int n)
   * @see removeUncertParameter(const std::string& sid)
   * @see removeUncertParameter(unsigned int n)
   */
  unsigned int getNumUncertParameters() const;


  /**
   * Creates a new UncertParameter object, adds it to this UncertParameter
   * object and returns the UncertParameter object created.
   *
   * @return a new UncertParameter object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see getUncertParameter(const std::string& sid)
   * @see getUncertParameter(unsigned int n)
   * @see getNumUncertParameters()
   * @see removeUncertParameter(const std::string& sid)
   * @see removeUncertParameter(unsigned int n)
   */
  UncertParameter* createUncertParameter();


  UncertSpan* createUncertSpan();
  /**
   * Removes the nth UncertParameter from this UncertParameter and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the UncertParameter to
   * remove.
   *
   * @return a pointer to the nth UncertParameter in this UncertParameter.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUncertParameter(const UncertParameter* object)
   * @see createUncertParameter()
   * @see getUncertParameter(const std::string& sid)
   * @see getUncertParameter(unsigned int n)
   * @see getNumUncertParameters()
   * @see removeUncertParameter(const std::string& sid)
   */
  UncertParameter* removeUncertParameter(unsigned int n);


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * @copydoc doc_renameunitsidref_common
   */
  virtual void renameUnitSIdRefs(const std::string& oldid, const std::string& newid);


  /** @cond doxygenLibsbmlInternal */
  /**
   * Replace all nodes with the name 'id' from the child 'math' object with the provided function.
   *
   */
  virtual void replaceSIDWithFunction(const std::string& id, const ASTNode* function);
  /** @endcond */

  /**
   * Returns the XML element name of this UncertParameter object.
   *
   * For UncertParameter, the XML element name is always @c "uncertParameter".
   *
   * @return the name of this element, i.e. @c "uncertParameter".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this UncertParameter object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_UNCERTPARAMETER, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * UncertParameter object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * UncertParameter have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the UncertParameter object are:
   * @li "type"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * UncertParameter object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * UncertParameter have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the UncertParameter object are:
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



  /** @cond doxygenLibsbmlInternal */

  /**
   * Updates the namespaces when setLevelVersion is used
   */
  virtual void updateSBMLNamespace(const std::string& package,
                                   unsigned int level,
                                   unsigned int version);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this UncertParameter.
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
   * Gets the value of the "attributeName" attribute of this UncertParameter.
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
   * Gets the value of the "attributeName" attribute of this UncertParameter.
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
   * Gets the value of the "attributeName" attribute of this UncertParameter.
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
   * Gets the value of the "attributeName" attribute of this UncertParameter.
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
   * Predicate returning @c true if this UncertParameter's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this UncertParameter's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this UncertParameter.
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
   * Sets the value of the "attributeName" attribute of this UncertParameter.
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
   * Sets the value of the "attributeName" attribute of this UncertParameter.
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
   * Sets the value of the "attributeName" attribute of this UncertParameter.
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
   * Sets the value of the "attributeName" attribute of this UncertParameter.
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
   * Unsets the value of the "attributeName" attribute of this UncertParameter.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this UncertParameter.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this UncertParameter.
   *
   * @param elementName, the name of the element to create.
   *
   * @param element, pointer to the element to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int addChildObject(const std::string& elementName,
                             const SBase* element);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Removes and returns the new "elementName" object with the given id in this
   * UncertParameter.
   *
   * @param elementName, the name of the element to remove.
   *
   * @param id, the id of the element to remove.
   *
   * @return pointer to the element removed.
   */
  virtual SBase* removeChildObject(const std::string& elementName,
                                   const std::string& id);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this UncertParameter.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this UncertParameter.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int the index of the object to retrieve.
   *
   * @return pointer to the object.
   */
  virtual SBase* getObject(const std::string& elementName, unsigned int index);

  /** @endcond */




  #endif /* !SWIG */


  /**
   * Returns the first child element that has the given @p id in the model-wide
   * SId namespace, or @c NULL if no such object is found.
   *
   * @param id a string representing the id attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p id. If no such
   * object is found, this method returns @c NULL.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid. If no
   * such object is found this method returns @c NULL.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @param filter an ElementFilter that may impose restrictions on the objects
   * to be retrieved.
   *
   * @return a List pointer of pointers to all SBase child objects with any
   * restriction imposed.
   */
  virtual List* getAllElements(ElementFilter * filter = NULL);


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
 * Creates a new UncertParameter_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * UncertParameter_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * UncertParameter_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this UncertParameter_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
UncertParameter_t *
UncertParameter_create(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this UncertParameter_t object.
 *
 * @param up the UncertParameter_t structure.
 *
 * @return a (deep) copy of this UncertParameter_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_clone(const UncertParameter_t* up);


/**
 * Frees this UncertParameter_t object.
 *
 * @param up the UncertParameter_t structure.
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
void
UncertParameter_free(UncertParameter_t* up);


/**
 * Returns the value of the "value" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure whose value is sought.
 *
 * @return the value of the "value" attribute of this UncertParameter_t as a
 * double.
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
double
UncertParameter_getValue(const UncertParameter_t * up);


/**
 * Returns the value of the "var" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure whose var is sought.
 *
 * @return the value of the "var" attribute of this UncertParameter_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
char *
UncertParameter_getVar(const UncertParameter_t * up);


/**
 * Returns the value of the "units" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure whose units is sought.
 *
 * @return the value of the "units" attribute of this UncertParameter_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
char *
UncertParameter_getUnits(const UncertParameter_t * up);


/**
 * Returns the value of the "type" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure whose type is sought.
 *
 * @return the value of the "type" attribute of this UncertParameter_t as a
 * UncertType_t.
 *
 * @copydetails doc_uncertparameter_type
 * @if clike The value is drawn from the enumeration @ref UncertType_t @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_DISTRIBUTION, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_EXTERNALPARAMETER, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_COEFFIENTOFVARIATION, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_KURTOSIS, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_MEAN, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_MEDIAN, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_MODE, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_SAMPLESIZE, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_SKEWNESS, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_STANDARDDEVIATION, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_STANDARDERROR, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_VARIANCE, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_CONFIDENCEINTERVAL, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_CREDIBLEINTERVAL, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_INTERQUARTILERANGE, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_RANGE, UncertType_t}
 * @li @sbmlconstant{DISTRIB_UNCERTTYPE_INVALID, UncertType_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
UncertType_t
UncertParameter_getType(const UncertParameter_t * up);


/**
 * Returns the value of the "type" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure whose type is sought.
 *
 * @return the value of the "type" attribute of this UncertParameter_t as a
 * const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_uncertparameter_type
 * The possible values returned by this method are:
 * @li @c "distribution"
 * @li @c "externalParameter"
 * @li @c "coeffientOfVariation"
 * @li @c "kurtosis"
 * @li @c "mean"
 * @li @c "median"
 * @li @c "mode"
 * @li @c "sampleSize"
 * @li @c "skewness"
 * @li @c "standardDeviation"
 * @li @c "standardError"
 * @li @c "variance"
 * @li @c "confidenceInterval"
 * @li @c "credibleInterval"
 * @li @c "interquartileRange"
 * @li @c "range"
 * @li @c "invalid UncertType value"
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
char *
UncertParameter_getTypeAsString(const UncertParameter_t * up);


/**
 * Returns the value of the "definitionURL" attribute of this
 * UncertParameter_t.
 *
 * @param up the UncertParameter_t structure whose definitionURL is sought.
 *
 * @return the value of the "definitionURL" attribute of this UncertParameter_t
 * as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
char *
UncertParameter_getDefinitionURL(const UncertParameter_t * up);


/**
 * Predicate returning @c 1 (true) if this UncertParameter_t's "value"
 * attribute is set.
 *
 * @param up the UncertParameter_t structure.
 *
 * @return @c 1 (true) if this UncertParameter_t's "value" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_isSetValue(const UncertParameter_t * up);


/**
 * Predicate returning @c 1 (true) if this UncertParameter_t's "var" attribute
 * is set.
 *
 * @param up the UncertParameter_t structure.
 *
 * @return @c 1 (true) if this UncertParameter_t's "var" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_isSetVar(const UncertParameter_t * up);


/**
 * Predicate returning @c 1 (true) if this UncertParameter_t's "units"
 * attribute is set.
 *
 * @param up the UncertParameter_t structure.
 *
 * @return @c 1 (true) if this UncertParameter_t's "units" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_isSetUnits(const UncertParameter_t * up);


/**
 * Predicate returning @c 1 (true) if this UncertParameter_t's "type" attribute
 * is set.
 *
 * @param up the UncertParameter_t structure.
 *
 * @return @c 1 (true) if this UncertParameter_t's "type" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_uncertparameter_type
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_isSetType(const UncertParameter_t * up);


/**
 * Predicate returning @c 1 (true) if this UncertParameter_t's "definitionURL"
 * attribute is set.
 *
 * @param up the UncertParameter_t structure.
 *
 * @return @c 1 (true) if this UncertParameter_t's "definitionURL" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_isSetDefinitionURL(const UncertParameter_t * up);


/**
 * Sets the value of the "value" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @param value double value of the "value" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_setValue(UncertParameter_t * up, double value);


/**
 * Sets the value of the "var" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @param var const char * value of the "var" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_setVar(UncertParameter_t * up, const char * var);


/**
 * Sets the value of the "units" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @param units const char * value of the "units" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_setUnits(UncertParameter_t * up, const char * units);


/**
 * Sets the value of the "type" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @param type UncertType_t value of the "type" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_uncertparameter_type
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_setType(UncertParameter_t * up, UncertType_t type);


/**
 * Sets the value of the "type" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @param type const char * of the "type" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_uncertparameter_type
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_setTypeAsString(UncertParameter_t * up, const char * type);


/**
 * Sets the value of the "definitionURL" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @param definitionURL const char * value of the "definitionURL" attribute to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p definitionURL = @c NULL or an empty string is
 * equivalent to calling UncertParameter_unsetDefinitionURL().
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_setDefinitionURL(UncertParameter_t * up,
                                 const char * definitionURL);


/**
 * Unsets the value of the "value" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_unsetValue(UncertParameter_t * up);


/**
 * Unsets the value of the "var" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_unsetVar(UncertParameter_t * up);


/**
 * Unsets the value of the "units" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_unsetUnits(UncertParameter_t * up);


/**
 * Unsets the value of the "type" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_uncertparameter_type
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_unsetType(UncertParameter_t * up);


/**
 * Unsets the value of the "definitionURL" attribute of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_unsetDefinitionURL(UncertParameter_t * up);


/**
 * Returns the value of the "math" element of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure whose math is sought.
 *
 * @return the value of the "math" element of this UncertParameter_t as a
 * pointer to an ASTNode_t object.
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
const ASTNode_t*
UncertParameter_getMath(const UncertParameter_t * up);


/**
 * Predicate returning @c 1 (true) if this UncertParameter_t's "math" element
 * is set.
 *
 * @param up the UncertParameter_t structure.
 *
 * @return @c 1 (true) if this UncertParameter_t's "math" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_isSetMath(const UncertParameter_t * up);


/**
 * Sets the value of the "math" element of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @param math ASTNode_t * pointer to the "math" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_setMath(UncertParameter_t * up, const ASTNode_t* math);


/**
 * Unsets the value of the "math" element of this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_unsetMath(UncertParameter_t * up);


/**
 * Returns a ListOf_t * containing UncertParameter_t objects from this
 * UncertParameter_t.
 *
 * @param up the UncertParameter_t structure whose ListOfUncertParameters * is
 * sought.
 *
 * @return the ListOfUncertParameters * from this UncertParameter_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see UncertParameter_addUncertParameter()
 * @see UncertParameter_createUncertParameter()
 * @see UncertParameter_getUncertParameterById()
 * @see UncertParameter_getUncertParameter()
 * @see UncertParameter_getNumUncertParameters()
 * @see UncertParameter_removeUncertParameterById()
 * @see UncertParameter_removeUncertParameter()
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
ListOf_t*
UncertParameter_getListOfUncertParameters(UncertParameter_t* up);


/**
 * Get an UncertParameter_t from the UncertParameter_t.
 *
 * @param up the UncertParameter_t structure to search.
 *
 * @param n an unsigned int representing the index of the UncertParameter_t to
 * retrieve.
 *
 * @return the nth UncertParameter_t in the ListOfUncertParameters * within
 * this UncertParameter or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_getUncertParameter(UncertParameter_t* up, unsigned int n);


/**
 * Get an UncertParameter_t from the UncertParameter_t based on the element to
 * which it refers.
 *
 * @param up the UncertParameter_t structure to search.
 *
 * @param sid a string representing the "var" attribute of the
 * UncertParameter_t object to retrieve.
 *
 * @return the first UncertParameter_t in this UncertParameter_t based on the
 * given var attribute or NULL if no such UncertParameter_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_getUncertParameterByVar(UncertParameter_t* up,
                                        const char *sid);


/**
 * Adds a copy of the given UncertParameter_t to this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure to which the UncertParameter_t
 * should be added.
 *
 * @param up1 the UncertParameter_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_addUncertParameter(UncertParameter_t* up,
                                   const UncertParameter_t* up1);


/**
 * Get the number of UncertParameter_t objects in this UncertParameter_t.
 *
 * @param up the UncertParameter_t structure to query.
 *
 * @return the number of UncertParameter_t objects in this UncertParameter_t.
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
unsigned int
UncertParameter_getNumUncertParameters(UncertParameter_t* up);


/**
 * Creates a new UncertParameter_t object, adds it to this UncertParameter_t
 * object and returns the UncertParameter_t object created.
 *
 * @param up the UncertParameter_t structure to which the UncertParameter_t
 * should be added.
 *
 * @return a new UncertParameter_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_createUncertParameter(UncertParameter_t* up);


/**
 * Removes the nth UncertParameter_t from this UncertParameter_t and returns a
 * pointer to it.
 *
 * @param up the UncertParameter_t structure to search.
 *
 * @param n an unsigned int representing the index of the UncertParameter_t to
 * remove.
 *
 * @return a pointer to the nth UncertParameter_t in this UncertParameter_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
UncertParameter_t*
UncertParameter_removeUncertParameter(UncertParameter_t* up, unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UncertParameter_t object have been set.
 *
 * @param up the UncertParameter_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * UncertParameter_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the UncertParameter_t object are:
 * @li "type"
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_hasRequiredAttributes(const UncertParameter_t * up);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * UncertParameter_t object have been set.
 *
 * @param up the UncertParameter_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * UncertParameter_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the UncertParameter_t object are:
 *
 * @memberof UncertParameter_t
 */
LIBSBML_EXTERN
int
UncertParameter_hasRequiredElements(const UncertParameter_t * up);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !UncertParameter_H__ */


