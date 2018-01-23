/**
 * @file BoundaryCondition.h
 * @brief Definition of the BoundaryCondition class.
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
 * @class BoundaryCondition
 * @sbmlbrief{spatial} TODO:Definition of the BoundaryCondition class.
 */


#ifndef BoundaryCondition_H__
#define BoundaryCondition_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN BoundaryCondition : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mVariable;
  BoundaryConditionKind_t mType;
  std::string mCoordinateBoundary;
  std::string mBoundaryDomainType;

  /** @endcond */

public:

  /**
   * Creates a new BoundaryCondition using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * BoundaryCondition.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * BoundaryCondition.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this BoundaryCondition.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  BoundaryCondition(unsigned int level = SpatialExtension::getDefaultLevel(),
                    unsigned int version =
                      SpatialExtension::getDefaultVersion(),
                    unsigned int pkgVersion =
                      SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new BoundaryCondition using the given SpatialPkgNamespaces
   * object.
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  BoundaryCondition(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for BoundaryCondition.
   *
   * @param orig the BoundaryCondition instance to copy.
   */
  BoundaryCondition(const BoundaryCondition& orig);


  /**
   * Assignment operator for BoundaryCondition.
   *
   * @param rhs the BoundaryCondition object whose values are to be used as the
   * basis of the assignment.
   */
  BoundaryCondition& operator=(const BoundaryCondition& rhs);


  /**
   * Creates and returns a deep copy of this BoundaryCondition object.
   *
   * @return a (deep) copy of this BoundaryCondition object.
   */
  virtual BoundaryCondition* clone() const;


  /**
   * Destructor for BoundaryCondition.
   */
  virtual ~BoundaryCondition();


  /**
   * Returns the value of the "variable" attribute of this BoundaryCondition.
   *
   * @return the value of the "variable" attribute of this BoundaryCondition as
   * a string.
   */
  const std::string& getVariable() const;


  /**
   * Returns the value of the "type" attribute of this BoundaryCondition.
   *
   * @return the value of the "type" attribute of this BoundaryCondition as a
   * BoundaryConditionKind_t.
   */
  BoundaryConditionKind_t getType() const;


  /**
   * Returns the value of the "type" attribute of this BoundaryCondition.
   *
   * @return the value of the "type" attribute of this BoundaryCondition as a
   * string.
   */
  const std::string& getTypeAsString() const;


  /**
   * Returns the value of the "coordinateBoundary" attribute of this
   * BoundaryCondition.
   *
   * @return the value of the "coordinateBoundary" attribute of this
   * BoundaryCondition as a string.
   */
  const std::string& getCoordinateBoundary() const;


  /**
   * Returns the value of the "boundaryDomainType" attribute of this
   * BoundaryCondition.
   *
   * @return the value of the "boundaryDomainType" attribute of this
   * BoundaryCondition as a string.
   */
  const std::string& getBoundaryDomainType() const;


  /**
   * Predicate returning @c true if this BoundaryCondition's "variable"
   * attribute is set.
   *
   * @return @c true if this BoundaryCondition's "variable" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetVariable() const;


  /**
   * Predicate returning @c true if this BoundaryCondition's "type" attribute
   * is set.
   *
   * @return @c true if this BoundaryCondition's "type" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetType() const;


  /**
   * Predicate returning @c true if this BoundaryCondition's
   * "coordinateBoundary" attribute is set.
   *
   * @return @c true if this BoundaryCondition's "coordinateBoundary" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetCoordinateBoundary() const;


  /**
   * Predicate returning @c true if this BoundaryCondition's
   * "boundaryDomainType" attribute is set.
   *
   * @return @c true if this BoundaryCondition's "boundaryDomainType" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetBoundaryDomainType() const;


  /**
   * Sets the value of the "variable" attribute of this BoundaryCondition.
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
   * Sets the value of the "type" attribute of this BoundaryCondition.
   *
   * @param type BoundaryConditionKind_t value of the "type" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setType(const BoundaryConditionKind_t type);


  /**
   * Sets the value of the "type" attribute of this BoundaryCondition.
   *
   * @param type std::string& of the "type" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setType(const std::string& type);


  /**
   * Sets the value of the "coordinateBoundary" attribute of this
   * BoundaryCondition.
   *
   * @param coordinateBoundary std::string& value of the "coordinateBoundary"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoordinateBoundary(const std::string& coordinateBoundary);


  /**
   * Sets the value of the "boundaryDomainType" attribute of this
   * BoundaryCondition.
   *
   * @param boundaryDomainType std::string& value of the "boundaryDomainType"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setBoundaryDomainType(const std::string& boundaryDomainType);


  /**
   * Unsets the value of the "variable" attribute of this BoundaryCondition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVariable();


  /**
   * Unsets the value of the "type" attribute of this BoundaryCondition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetType();


  /**
   * Unsets the value of the "coordinateBoundary" attribute of this
   * BoundaryCondition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCoordinateBoundary();


  /**
   * Unsets the value of the "boundaryDomainType" attribute of this
   * BoundaryCondition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetBoundaryDomainType();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this BoundaryCondition object.
   *
   * For BoundaryCondition, the XML element name is always @c
   * "boundaryCondition".
   *
   * @return the name of this element, i.e. @c "boundaryCondition".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this BoundaryCondition object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   *
   * @sbmlconstant{SBML_SPATIAL_BOUNDARYCONDITION, SBMLSpatialTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * BoundaryCondition object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * BoundaryCondition have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the BoundaryCondition object are:
   * @li "variable"
   * @li "type"
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
   * Gets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * Gets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * Gets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * Gets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * Gets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * Predicate returning @c true if this BoundaryCondition's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this BoundaryCondition's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * Sets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * Sets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * Sets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * Sets the value of the "attributeName" attribute of this BoundaryCondition.
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
   * BoundaryCondition.
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
 * Creates a new BoundaryCondition_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * BoundaryCondition_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * BoundaryCondition_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this BoundaryCondition_t.
 *
 * @throws SBMLConstructorException
 * Thrown if the given @p level and @p version combination, or this kind of
 * SBML object, are either invalid or mismatched with respect to the parent
 * SBMLDocument object.
 * @copydetails doc_note_setting_lv
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
BoundaryCondition_t *
BoundaryCondition_create(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this BoundaryCondition_t object.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return a (deep) copy of this BoundaryCondition_t object.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
BoundaryCondition_t*
BoundaryCondition_clone(const BoundaryCondition_t* bc);


/**
 * Frees this BoundaryCondition_t object.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
void
BoundaryCondition_free(BoundaryCondition_t* bc);


/**
 * Returns the value of the "variable" attribute of this BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure whose variable is sought.
 *
 * @return the value of the "variable" attribute of this BoundaryCondition_t as
 * a pointer to a string.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
const char *
BoundaryCondition_getVariable(const BoundaryCondition_t * bc);


/**
 * Returns the value of the "type" attribute of this BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure whose type is sought.
 *
 * @return the value of the "type" attribute of this BoundaryCondition_t as a
 * BoundaryConditionKind_t.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
BoundaryConditionKind_t
BoundaryCondition_getType(const BoundaryCondition_t * bc);


/**
 * Returns the value of the "type" attribute of this BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure whose type is sought.
 *
 * @return the value of the "type" attribute of this BoundaryCondition_t as a
 * const char *.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
const char *
BoundaryCondition_getTypeAsString(const BoundaryCondition_t * bc);


/**
 * Returns the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure whose coordinateBoundary is
 * sought.
 *
 * @return the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition_t as a pointer to a string.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
const char *
BoundaryCondition_getCoordinateBoundary(const BoundaryCondition_t * bc);


/**
 * Returns the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure whose boundaryDomainType is
 * sought.
 *
 * @return the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition_t as a pointer to a string.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
const char *
BoundaryCondition_getBoundaryDomainType(const BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 if this BoundaryCondition_t's "variable" attribute
 * is set.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return @c 1 if this BoundaryCondition_t's "variable" attribute has been
 * set, otherwise @c 0 is returned.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetVariable(const BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 if this BoundaryCondition_t's "type" attribute is
 * set.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return @c 1 if this BoundaryCondition_t's "type" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetType(const BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 if this BoundaryCondition_t's "coordinateBoundary"
 * attribute is set.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return @c 1 if this BoundaryCondition_t's "coordinateBoundary" attribute
 * has been set, otherwise @c 0 is returned.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetCoordinateBoundary(const BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 if this BoundaryCondition_t's "boundaryDomainType"
 * attribute is set.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return @c 1 if this BoundaryCondition_t's "boundaryDomainType" attribute
 * has been set, otherwise @c 0 is returned.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetBoundaryDomainType(const BoundaryCondition_t * bc);


/**
 * Sets the value of the "variable" attribute of this BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @param variable const char * value of the "variable" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_setVariable(BoundaryCondition_t * bc,
                              const char * variable);


/**
 * Sets the value of the "type" attribute of this BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @param type BoundaryConditionKind_t value of the "type" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_setType(BoundaryCondition_t * bc,
                          BoundaryConditionKind_t type);


/**
 * Sets the value of the "type" attribute of this BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @param type const char * of the "type" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_setTypeAsString(BoundaryCondition_t * bc,
                                  const char * type);


/**
 * Sets the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @param coordinateBoundary const char * value of the "coordinateBoundary"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_setCoordinateBoundary(BoundaryCondition_t * bc,
                                        const char * coordinateBoundary);


/**
 * Sets the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @param boundaryDomainType const char * value of the "boundaryDomainType"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_setBoundaryDomainType(BoundaryCondition_t * bc,
                                        const char * boundaryDomainType);


/**
 * Unsets the value of the "variable" attribute of this BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetVariable(BoundaryCondition_t * bc);


/**
 * Unsets the value of the "type" attribute of this BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetType(BoundaryCondition_t * bc);


/**
 * Unsets the value of the "coordinateBoundary" attribute of this
 * BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetCoordinateBoundary(BoundaryCondition_t * bc);


/**
 * Unsets the value of the "boundaryDomainType" attribute of this
 * BoundaryCondition_t.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetBoundaryDomainType(BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 if all the required attributes for this
 * BoundaryCondition_t object have been set.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return @c 1 to indicate that all the required attributes of this
 * BoundaryCondition_t have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required attributes for the BoundaryCondition_t object are:
 * @li "variable"
 * @li "type"
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_hasRequiredAttributes(const BoundaryCondition_t * bc);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !BoundaryCondition_H__ */


