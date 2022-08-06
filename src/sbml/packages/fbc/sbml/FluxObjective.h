/**
 * @file    FluxObjective.h
 * @brief   Definition of FluxObjective, the SBase derived class of the fbc package.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
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
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class FluxObjective
 * @sbmlbrief{fbc} An objective function for a flux.
 *
 * An integral component in a complete description of a steady-state model is
 * the so-called <em>objective function</em>, which generally consists of a
 * linear combination of model variables (fluxes) and a sense (direction). In
 * the SBML Level&nbsp;3 @ref fbc (&ldquo;fbc&rdquo;) package, this concept
 * is succinctly captured in the Objective class.  An Objective object
 * includes a list of flux objectives, each in the form of a FluxObjective
 * object.

 * The FluxObjective class is a relatively simple container for a model
 * variable weighted by a signed linear coefficient.  In addition to the
 * common SBML object attributes of "id" and "name" (both of which are
 * optional), it adds two required attributes: "reaction" and "coefficient".
 *
 * The "reaction" attribute must have a value of type <code>SIdRef</code>,
 * and its value is restricted to the identifier of a Reaction object in the
 * model.  The "reaction" attribute identifiers the reaction to which the
 * FluxObjective applies.  The "coefficient" attribute must have a value of
 * type <code>double</code>, and refers to the coefficient that this
 * FluxObjective takes in the enclosing Objective.  Its unit of measurement
 * is <code>dimensionless</code>.  The meaning of these two attributes
 * together is given by the formula <em>coefficient &times;
 * reaction-flux</em>.  Since reactions in SBML Level&nbsp;3 are in units of
 * <em>extent</em>, the units of a flux objective are thus <em>extent per
 * time</em>.
 *
 * The following example * illustrates the use of these attributes in an
 * example of a * ListOfObjectives:
 * @verbatim
<fbc:listOfObjectives fbc:activeObjective="obj1">
 <fbc:objective fbc:id="obj1" fbc:type="maximize">
  <fbc:listOfFluxObjectives>
   <fbc:fluxObjective fbc:reaction="R1" fbc:coefficient="1"/>
   <fbc:fluxObjective fbc:reaction="R2" fbc:coefficient="2"/>
  </fbc:listOfFluxObjectives>
 </fbc:objective>
</fbc:listOfObjectives>
@endverbatim
 *
 * @see Objective
 * @see ListOfObjectives
 * @see ListOfFluxObjectives
 */

#ifndef FluxObjective_H__
#define FluxObjective_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN FluxObjective : public SBase
{

protected:
  /** @cond doxygenLibsbmlInternal */
//  std::string   mId;
//  std::string   mName;
  std::string   mReaction;
  double        mCoefficient;
  bool          mIsSetCoefficient;
  FbcVariableType_t mVariableType;
  /** @endcond */

public:

  /**
   * Creates a new FluxObjective with the given SBML Level, Version, and
   * &ldquo;fbc&rdquo;package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * FluxObjective.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * FluxObjective.
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to
   * this FluxObjective.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  FluxObjective(unsigned int level      = FbcExtension::getDefaultLevel(),
                unsigned int version    = FbcExtension::getDefaultVersion(),
                unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new FluxObjective with the given FbcPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param fbcns the FbcPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  FluxObjective(FbcPkgNamespaces* fbcns);


   /**
   * Copy constructor for FluxObjective.
   *
   * @param orig the FluxObjective instance to copy.
   */
  FluxObjective(const FluxObjective& orig);


   /**
   * Assignment operator for FluxObjective.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment.
   */
  FluxObjective& operator=(const FluxObjective& rhs);


   /**
   * Creates and returns a deep copy of this FluxObjective object.
   *
   * @return a (deep) copy of this FluxObjective object.
   */
  virtual FluxObjective* clone () const;


   /**
   * Destructor for FluxObjective.
   */
  virtual ~FluxObjective();


  /**
   * Returns the value of the "id" attribute of this FluxObjective.
   *
   * @note Because of the inconsistent behavior of this function with 
   * respect to assignments and rules, it is now recommended to
   * use the getIdAttribute() function instead.
   *
   * @copydetails doc_id_attribute
   *
   * @return the id of this FluxObjective.
   *
   * @see getIdAttribute()
   * @see setIdAttribute(const std::string& sid)
   * @see isSetIdAttribute()
   * @see unsetIdAttribute()
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this FluxObjective object.
   *
   * @copydetails doc_get_name
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "reaction" attribute of this FluxObjective.
   *
   * @return the value of the "reaction" attribute of this FluxObjective as a
   * string.
   */
  virtual const std::string& getReaction() const;


  /**
   * Returns the value of the "coefficient" attribute of this FluxObjective.
   *
   * @return the value of the "coefficient" attribute of this FluxObjective
   * as a double.
   */
  virtual double getCoefficient() const;


    /**
   * Returns the value of the "variableType" attribute of this FluxObjective.
   *
   * @return the value of the "variableType" attribute of this FluxObjective as
   * a FbcVariableType_t.
   *
   * @copydetails doc_fluxobjective_variableType
   * @if clike The value is drawn from the enumeration @ref FbcVariableType_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{FBC_VARIABLE_TYPE_LINEAR, FbcVariableType_t}
   * @li @sbmlconstant{FBC_VARIABLE_TYPE_QUADRATIC, FbcVariableType_t}
   * @li @sbmlconstant{FBC_VARIABLE_TYPE_INVALID, FbcVariableType_t}
   */
  FbcVariableType_t getVariableType() const;


  /**
   * Returns the value of the "variableType" attribute of this FluxObjective.
   *
   * @return the value of the "variableType" attribute of this FluxObjective as
   * a string.
   *
   * @copydetails doc_fluxobjective_variableType
   * The possible values returned by this method are:
   * @li @c "linear"
   * @li @c "quadratic"
   * @li @c "invalid FbcVariableType value"
   */
  std::string getVariableTypeAsString() const;


/**
   * Predicate returning @c true if this FluxObjective's "id" attribute is
   * set.
   *
   * @copydetails doc_isset_id
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this FluxObjective's "name" attribute is
   * set.
   *
   * @copydetails doc_isset_name
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this FluxObjective's "reaction" attribute
   * is set.
   *
   * @return @c true if this FluxObjective's "reaction" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetReaction() const;


  /**
   * Predicate returning @c true if this FluxObjective's "coefficient"
   * attribute is set.
   *
   * @return @c true if this FluxObjective's "coefficient" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetCoefficient() const;


  /**
   * Predicate returning @c true if this FluxObjective's "variableType"
   * attribute is set.
   *
   * @return @c true if this FluxObjective's "variableType" attribute has been
   * set, otherwise @c false is returned.
   *
   * @copydetails doc_fluxobjective_variableType
   */
  bool isSetVariableType() const;


  /**
   * Sets the value of the "id" attribute of this FluxObjective.
   *
   * @copydetails doc_set_id
   */
  virtual int setId(const std::string& sid);


  /**
   * Sets the value of the "name" attribute of this FluxObjective.
   *
   * @copydetails doc_set_name
   */
  virtual int setName(const std::string& name);


  /**
   * Sets the value of the "reaction" attribute of this FluxObjective.
   *
   * @param reaction the value of the "reaction" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setReaction(const std::string& reaction);


  /**
   * Sets the value of the "coefficient" attribute of this FluxObjective.
   *
   * @param coefficient the value of the "coefficient" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setCoefficient(double coefficient);


  /**
   * Sets the value of the "variableType" attribute of this FluxObjective.
   *
   * @param variableType @if clike FbcVariableType_t@else int@endif value of
   * the "variableType" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_fluxobjective_variableType
   */
  int setVariableType(const FbcVariableType_t variableType);


  /**
   * Sets the value of the "variableType" attribute of this FluxObjective.
   *
   * @param variableType std::string& of the "variableType" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_fluxobjective_variableType
   */
  int setVariableType(const std::string& variableType);


  /**
   * Unsets the value of the "id" attribute of this FluxObjective.
   *
   * @copydetails doc_unset_id
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this FluxObjective.
   *
   * @copydetails doc_unset_name
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "reaction" attribute of this FluxObjective.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetReaction();


  /**
   * Unsets the value of the "coefficient" attribute of this FluxObjective.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetCoefficient();


  /**
   * Unsets the value of the "variableType" attribute of this FluxObjective.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_fluxobjective_variableType
   */
  int unsetVariableType();


  /**
   * @copydoc doc_renamesidref_common
   */
   virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
   * Returns the XML element name of this object.
   *
   * For FluxObjective, the XML element name is always @c "fluxObjective".
   *
   * @return the name of this element, i.e. @c "fluxObjective".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_FBC_FLUXOBJECTIVE, SBMLTypeCode_t} (default).
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Predicate returning @c true if all the required attributes
   * for this FluxObjective object have been set.
   *
   * @note The required attributes for a FluxObjective object are:
   * @li "reaction"
   * @li "coefficient"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parent's
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);

  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);

  /** @endcond */


  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this FluxObjective.
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
   * Returns the value of the "attributeName" attribute of this FluxObjective.
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
   * Returns the value of the "attributeName" attribute of this FluxObjective.
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
   * Returns the value of the "attributeName" attribute of this FluxObjective.
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
   * Returns the value of the "attributeName" attribute of this FluxObjective.
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
   * Predicate returning @c true if this FluxObjective's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this FluxObjective's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this FluxObjective.
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
   * Sets the value of the "attributeName" attribute of this FluxObjective.
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
   * Sets the value of the "attributeName" attribute of this FluxObjective.
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
   * Sets the value of the "attributeName" attribute of this FluxObjective.
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
   * Sets the value of the "attributeName" attribute of this FluxObjective.
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
   * Unsets the value of the "attributeName" attribute of this FluxObjective.
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
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);

  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;

  /** @endcond */



};

/**
 * @class ListOfFluxObjectives
 * @sbmlbrief{fbc} A list of FluxObjective objects.
 * 
 * The ListOfFluxObjectives is a container for the &ldquo;fbc&rdquo;
 * Objective that indicate which fluxes (and what ratios for those fluxes)
 * are to be used in maximizing or minimizing the Objective.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see Objective
 * @see FluxObjective
 */
class LIBSBML_EXTERN ListOfFluxObjectives : public ListOf
{

public:

  /**
   * Creates a new ListOfFluxObjectives with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfFluxObjectives.
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfFluxObjectives.
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this ListOfFluxObjectives.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfFluxObjectives(unsigned int level      = FbcExtension::getDefaultLevel(),
                       unsigned int version    = FbcExtension::getDefaultVersion(),
                       unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfFluxObjectives with the given FbcPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param fbcns the FbcPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfFluxObjectives(FbcPkgNamespaces* fbcns);


   /**
   * Creates and returns a deep copy of this ListOfFluxObjectives object.
   *
   * @return a (deep) copy of this ListOfFluxObjectives object.
   */
  virtual ListOfFluxObjectives* clone () const;


   /**
   * Get a FluxObjective from the ListOfFluxObjectives.
   *
   * @param n the index number of the FluxObjective to get.
   *
   * @return the nth FluxObjective in this ListOfFluxObjectives.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual FluxObjective* get(unsigned int n);


  /**
   * Get a FluxObjective from the ListOfFluxObjectives.
   *
   * @param n the index number of the FluxObjective to get.
   *
   * @return the nth FluxObjective in this ListOfFluxObjectives.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual const FluxObjective* get(unsigned int n) const;


  /**
   * Get a FluxObjective from the ListOfFluxObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the FluxObjective to get.
   *
   * @return FluxObjective in this ListOfFluxObjectives
   * with the given id or @c NULL if no such
   * FluxObjective exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual FluxObjective* get(const std::string& sid);


  /**
   * Get a FluxObjective from the ListOfFluxObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the FluxObjective to get.
   *
   * @return FluxObjective in this ListOfFluxObjectives
   * with the given id or @c NULL if no such
   * FluxObjective exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const FluxObjective* get(const std::string& sid) const;


  /**
    * Adds a copy the given FluxObjective to this ListOfFluxObjectives.
    *
    * @param fo the FluxObjective object to add.
    *
    * @copydetails doc_returns_success_code
    * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
    * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
    */
  int addFluxObjective(const FluxObjective* fo);


  /**
   * Get the number of FluxObjective objects in this ListOfFluxObjectives.
   *
   * @return the number of FluxObjective objects in this ListOfFluxObjectives
   */
  unsigned int getNumFluxObjectives() const;


  /**
   * Creates a new FluxObjective object, adds it to the
   * ListOfFluxObjectives and returns the FluxObjective object created. 
   *
   * @return a new FluxObjective object instance
   *
   * @see addFluxObjective(const FluxObjective* fo)
   */
  FluxObjective* createFluxObjective();


  /**
   * Removes the nth FluxObjective from this ListOfFluxObjectives
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the FluxObjective to remove.
   *
   * @see size()
   */
  virtual FluxObjective* remove(unsigned int n);


  /**
   * Removes the FluxObjective from this ListOfFluxObjectives with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the FluxObjective to remove.
   *
   * @return the FluxObjective removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual FluxObjective* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object.
   *
   * For ListOfFluxObjectives, the XML element name is always @c "listOfFluxObjectives".
   *
   * @return the name of this element, i.e. @c "listOfFluxObjectives".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_LIST_OF, SBMLTypeCode_t} (default).
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns the libSBML type code for the objects contained in this ListOf.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for the objects contained in this ListOf
   * instance: @sbmlconstant{SBML_FBC_FLUXOBJECTIVE, SBMLTypeCode_t} (default).
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new FluxObjective in this ListOfFluxObjectives
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the &ldquo;fbc&rdquo; package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new FluxObjective_t structure using the given SBML @p level and
 * @p version, and the @p pkgVersion package version.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * FluxObjective_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * FluxObjective_t structure.
 *
 * @param pkgVersion an unsigned int, the version of the package to assign
 * to this FluxObjective_t structure.
 *
 * @returns the newly-created FluxObjective_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
FluxObjective_t *
FluxObjective_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion);


/**
 * Frees the given FluxObjective_t structure.
 * 
 * @param fo the FluxObjective_t structure to be freed.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
void
FluxObjective_free(FluxObjective_t * fo);


/**
 * Creates a deep copy of the given FluxObjective_t structure.
 * 
 * @param fo the FluxObjective_t structure to be copied.
 *
 * @returns a (deep) copy of the given FluxObjective_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
FluxObjective_t *
FluxObjective_clone(FluxObjective_t * fo);


/**
 * Returns the value of the "id" attribute of the given FluxObjective_t
 * structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return the id of this structure.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
const char *
FluxObjective_getId(const FluxObjective_t * fo);


/**
 * Returns the value of the "name" attribute of the given FluxObjective_t
 * structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return the name of this structure.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
const char *
FluxObjective_getName(const FluxObjective_t * fo);


/**
 * Returns the value of the "reaction" attribute of the given FluxObjective_t
 * structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return the reaction of this structure.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
const char *
FluxObjective_getReaction(const FluxObjective_t * fo);


/**
 * Returns the value of the "coefficient" attribute of the given FluxObjective_t
 * structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return the coefficient of this structure.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
double
FluxObjective_getCoefficient(const FluxObjective_t * fo);


/**
 * Returns the value of the "variableType" attribute of this FluxObjective_t.
 *
 * @param fo the FluxObjective_t structure whose variableType is sought.
 *
 * @return the value of the "variableType" attribute of this FluxObjective_t as
 * a FbcVariableType_t.
 *
 * @copydetails doc_fluxobjective_variableType
 * @if clike The value is drawn from the enumeration @ref FbcVariableType_t
 * @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{FBC_VARIABLE_TYPE_LINEAR, FbcVariableType_t}
 * @li @sbmlconstant{FBC_VARIABLE_TYPE_QUADRATIC, FbcVariableType_t}
 * @li @sbmlconstant{FBC_VARIABLE_TYPE_INVALID, FbcVariableType_t}
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
FbcVariableType_t
FluxObjective_getVariableType(const FluxObjective_t * fo);


/**
 * Returns the value of the "variableType" attribute of this FluxObjective_t.
 *
 * @param fo the FluxObjective_t structure whose variableType is sought.
 *
 * @return the value of the "variableType" attribute of this FluxObjective_t as
 * a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_fluxobjective_variableType
 * The possible values returned by this method are:
 * @li @c "linear"
 * @li @c "quadratic"
 * @li @c "invalid FbcVariableType value"
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
char *
FluxObjective_getVariableTypeAsString(const FluxObjective_t * fo);


/**
 * Predicate returning @c 1 (true) if this FluxObjective_t's "id" attribute is
 * set.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return @c 1 (true) if the "id" of this FluxObjective_t structure is
 * set, @c 0 (false) otherwise.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_isSetId(const FluxObjective_t * fo);


/**
 * Predicate returning @c 1 (true) if the given FluxObjective_t structure's "name"
 * is set.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return @c 1 (true) if the "name" of this FluxObjective_t structure is
 * set, @c 0 (false) otherwise.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_isSetName(const FluxObjective_t * fo);


/**
 * Predicate returning @c 1 (true) if the given FluxObjective_t structure's "reaction"
 * is set.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return @c 1 (true) if the "reaction" of this FluxObjective_t structure is
 * set, @c 0 (false) otherwise.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_isSetReaction(const FluxObjective_t * fo);


/**
 * Predicate returning @c 1 (true) if the given FluxObjective_t structure's "coefficient"
 * is set.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return @c 1 (true) if the "coefficient" of this FluxObjective_t structure is
 * set, @c 0 (false) otherwise.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_isSetCoefficient(const FluxObjective_t * fo);


/**
 * Predicate returning @c 1 (true) if this FluxObjective_t's "variableType"
 * attribute is set.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @return @c 1 (true) if this FluxObjective_t's "variableType" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_fluxobjective_variableType
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_isSetVariableType(const FluxObjective_t * fo);


/**
 * Sets the value of the "id" attribute of this FluxObjective_t.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param id the string to which the structures "id" attribute should be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @note Using this function with a null pointer for @p id is equivalent to
 * unsetting the value of the "id" attribute.
 * 
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setId(FluxObjective_t * fo, const char * id);


/**
 * Sets the "name" attribute of the given FluxObjective_t structure.
 *
 * This function copies the string given in @p name.  If the string is
 * a null pointer, this function is equivalent to calling FluxObjective_unsetName().
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param name the string to which the structures "name" attribute should be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setName(FluxObjective_t * fo, const char * name);


/**
 * Sets the "reaction" attribute of the given FluxObjective_t structure.
 *
 * This function copies the string given in @p reaction.  If the string is
 * a null pointer, this function is equivalent to calling FluxObjective_unsetReaction().
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param reaction the string to which the structures "reaction" attribute should be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @note Using this function with a null pointer for @p reaction is equivalent to
 * unsetting the value of the "reaction" attribute.
 * 
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setReaction(FluxObjective_t * fo, const char * reaction);


/**
 * Sets the "coefficient" attribute of the given FluxObjective_t structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param coefficient the string to which the structures "coefficient" attribute should be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setCoefficient(FluxObjective_t * fo, double coefficient);


/**
 * Sets the value of the "variableType" attribute of this FluxObjective_t.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param variableType FbcVariableType_t value of the "variableType" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_fluxobjective_variableType
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setVariableType(FluxObjective_t * fo,
                              FbcVariableType_t variableType);


/**
 * Sets the value of the "variableType" attribute of this FluxObjective_t.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @param variableType const char * of the "variableType" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_fluxobjective_variableType
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_setVariableTypeAsString(FluxObjective_t * fo,
                                      const char * variableType);


/**
 * Unsets the value of the "id" attribute of this FluxObjective_t.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_unsetId(FluxObjective_t * fo);


/**
 * Unsets the value of the "name" attribute of the given 
 * FluxObjective_t structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_unsetName(FluxObjective_t * fo);


/**
 * Unsets the value of the "reaction" attribute of the given 
 * FluxObjective_t structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_unsetReaction(FluxObjective_t * fo);


/**
 * Unsets the value of the "coefficient" attribute of the given 
 * FluxObjective_t structure.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_unsetCoefficient(FluxObjective_t * fo);


/**
 * Unsets the value of the "variableType" attribute of this FluxObjective_t.
 *
 * @param fo the FluxObjective_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_fluxobjective_variableType
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_unsetVariableType(FluxObjective_t * fo);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether all the required
 * attributes of the given FluxObjective_t structure have been set.
 *
 * @param fo the FluxObjective_t structure to check.
 *
 * @return @c 1 (true) if all the required attributes for this
 * structure have been defined, @c 0 (false) otherwise.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
int
FluxObjective_hasRequiredAttributes(const FluxObjective_t * fo);


/**
 * Return the structure indicated by the given @p sid.
 *
 * @param lo the ListOf_t structure to use.
 *
 * @param sid a string matching the "id" attribute of the element sought.
 *
 * @return the structure for the given variable, or @c NULL if no such
 * object exists in the list.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
FluxObjective_t *
ListOfFluxObjectives_getById(ListOf_t * lo, const char * sid);


/**
 * Removes the structure with the given @p sid
 * from the given ListOf_t structure and returns a pointer to it.
 *
 * The caller owns the returned structure and is responsible for deleting it.
 *
 * @param lo the ListOf_t structure.
 * @param sid the string of the "id" attribute of the sought structure.
 *
 * @return the structure removed.  As mentioned above, the
 * caller owns the returned structure. @c NULL is returned if no
 * structure with the "id" attribute exists in the given ListOf_t structure.
 *
 * @memberof FluxObjective_t
 */
LIBSBML_EXTERN
FluxObjective_t *
ListOfFluxObjectives_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  FluxObjective_H__  */

