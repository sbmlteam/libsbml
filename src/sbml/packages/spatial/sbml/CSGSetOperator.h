/**
 * @file CSGSetOperator.h
 * @brief Definition of the CSGSetOperator class.
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
 * @class CSGSetOperator
 * @sbmlbrief{spatial} TODO:Definition of the CSGSetOperator class.
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
 * @class doc_csgsetoperator_operationType
 *
 * @par
 * The attribute "operationType" on a CSGSetOperator object is used to TODO:add
 * explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Spatial specification, the following are the
 * allowable values for "operationType":
 * <ul>
 * <li> @c "union", TODO:add description
 *
 * <li> @c "intersection", TODO:add description
 *
 * <li> @c "difference", TODO:add description
 *
 * </ul>
 */


#ifndef CSGSetOperator_H__
#define CSGSetOperator_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/CSGNode.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/ListOfCSGNodes.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGSetOperator : public CSGNode
{
protected:

  /** @cond doxygenLibsbmlInternal */

  SetOperation_t mOperationType;
  std::string mComplementA;
  std::string mComplementB;
  ListOfCSGNodes mCSGNodes;

  /** @endcond */

public:

  /**
   * Creates a new CSGSetOperator using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CSGSetOperator.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGSetOperator.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGSetOperator.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGSetOperator(unsigned int level = SpatialExtension::getDefaultLevel(),
                 unsigned int version = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion =
                   SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGSetOperator using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGSetOperator(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGSetOperator.
   *
   * @param orig the CSGSetOperator instance to copy.
   */
  CSGSetOperator(const CSGSetOperator& orig);


  /**
   * Assignment operator for CSGSetOperator.
   *
   * @param rhs the CSGSetOperator object whose values are to be used as the
   * basis of the assignment.
   */
  CSGSetOperator& operator=(const CSGSetOperator& rhs);


  /**
   * Creates and returns a deep copy of this CSGSetOperator object.
   *
   * @return a (deep) copy of this CSGSetOperator object.
   */
  virtual CSGSetOperator* clone() const;


  /**
   * Destructor for CSGSetOperator.
   */
  virtual ~CSGSetOperator();


  /**
   * Returns the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @return the value of the "operationType" attribute of this CSGSetOperator
   * as a SetOperation_t.
   *
   * @copydetails doc_csgsetoperator_operationType
   * @if clike The value is drawn from the enumeration @ref SetOperation_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{SPATIAL_SETOPERATION_UNION, SetOperation_t}
   * @li @sbmlconstant{SPATIAL_SETOPERATION_INTERSECTION, SetOperation_t}
   * @li @sbmlconstant{SPATIAL_SETOPERATION_DIFFERENCE, SetOperation_t}
   * @li @sbmlconstant{SPATIAL_SETOPERATION_INVALID, SetOperation_t}
   */
  SetOperation_t getOperationType() const;


  /**
   * Returns the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @return the value of the "operationType" attribute of this CSGSetOperator
   * as a string.
   *
   * @copydetails doc_csgsetoperator_operationType
   * The possible values returned by this method are:
   * @li @c "union"
   * @li @c "intersection"
   * @li @c "difference"
   * @li @c "invalid SetOperation value"
   */
  std::string getOperationTypeAsString() const;


  /**
   * Returns the value of the "complementA" attribute of this CSGSetOperator.
   *
   * @return the value of the "complementA" attribute of this CSGSetOperator as
   * a string.
   */
  const std::string& getComplementA() const;


  /**
   * Returns the value of the "complementB" attribute of this CSGSetOperator.
   *
   * @return the value of the "complementB" attribute of this CSGSetOperator as
   * a string.
   */
  const std::string& getComplementB() const;


  /**
   * Predicate returning @c true if this CSGSetOperator's "operationType"
   * attribute is set.
   *
   * @return @c true if this CSGSetOperator's "operationType" attribute has
   * been set, otherwise @c false is returned.
   *
   * @copydetails doc_csgsetoperator_operationType
   */
  bool isSetOperationType() const;


  /**
   * Predicate returning @c true if this CSGSetOperator's "complementA"
   * attribute is set.
   *
   * @return @c true if this CSGSetOperator's "complementA" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetComplementA() const;


  /**
   * Predicate returning @c true if this CSGSetOperator's "complementB"
   * attribute is set.
   *
   * @return @c true if this CSGSetOperator's "complementB" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetComplementB() const;


  /**
   * Sets the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @param operationType @if clike SetOperation_t@else int@endif value of the
   * "operationType" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_csgsetoperator_operationType
   */
  int setOperationType(const SetOperation_t operationType);


  /**
   * Sets the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @param operationType std::string& of the "operationType" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_csgsetoperator_operationType
   */
  int setOperationType(const std::string& operationType);


  /**
   * Sets the value of the "complementA" attribute of this CSGSetOperator.
   *
   * @param complementA std::string& value of the "complementA" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setComplementA(const std::string& complementA);


  /**
   * Sets the value of the "complementB" attribute of this CSGSetOperator.
   *
   * @param complementB std::string& value of the "complementB" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setComplementB(const std::string& complementB);


  /**
   * Unsets the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_csgsetoperator_operationType
   */
  int unsetOperationType();


  /**
   * Unsets the value of the "complementA" attribute of this CSGSetOperator.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetComplementA();


  /**
   * Unsets the value of the "complementB" attribute of this CSGSetOperator.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetComplementB();


  /**
   * Returns the ListOfCSGNodes from this CSGSetOperator.
   *
   * @return the ListOfCSGNodes from this CSGSetOperator.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  const ListOfCSGNodes* getListOfCSGNodes() const;


  /**
   * Returns the ListOfCSGNodes from this CSGSetOperator.
   *
   * @return the ListOfCSGNodes from this CSGSetOperator.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  ListOfCSGNodes* getListOfCSGNodes();


  /**
   * Get a CSGNode from the CSGSetOperator.
   *
   * @param n an unsigned int representing the index of the CSGNode to
   * retrieve.
   *
   * @return the nth CSGNode in the ListOfCSGNodes within this CSGSetOperator
   * or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see getCSGNode(const std::string& sid)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  CSGNode* getCSGNode(unsigned int n);


  /**
   * Get a CSGNode from the CSGSetOperator.
   *
   * @param n an unsigned int representing the index of the CSGNode to
   * retrieve.
   *
   * @return the nth CSGNode in the ListOfCSGNodes within this CSGSetOperator
   * or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see getCSGNode(const std::string& sid)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  const CSGNode* getCSGNode(unsigned int n) const;


  /**
   * Get a CSGNode from the CSGSetOperator based on its identifier.
   *
   * @param sid a string representing the identifier of the CSGNode to
   * retrieve.
   *
   * @return the CSGNode in the ListOfCSGNodes within this CSGSetOperator with
   * the given @p sid or @c NULL if no such CSGNode exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  CSGNode* getCSGNode(const std::string& sid);


  /**
   * Get a CSGNode from the CSGSetOperator based on its identifier.
   *
   * @param sid a string representing the identifier of the CSGNode to
   * retrieve.
   *
   * @return the CSGNode in the ListOfCSGNodes within this CSGSetOperator with
   * the given @p sid or @c NULL if no such CSGNode exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  const CSGNode* getCSGNode(const std::string& sid) const;


  /**
   * Adds a copy of the given CSGNode to this CSGSetOperator.
   *
   * @param csgn the CSGNode object to add.
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
   * @see createCSGNode()
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  int addCSGNode(const CSGNode* csgn);


  /**
   * Get the number of CSGNode objects in this CSGSetOperator.
   *
   * @return the number of CSGNode objects in this CSGSetOperator.
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  unsigned int getNumCSGNodes() const;


  /**
   * Creates a new CSGPrimitive object, adds it to this CSGSetOperator object
   * and returns the CSGPrimitive object created.
   *
   * @return a new CSGPrimitive object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  CSGPrimitive* createCSGPrimitive();


  /**
   * Creates a new CSGTranslation object, adds it to this CSGSetOperator object
   * and returns the CSGTranslation object created.
   *
   * @return a new CSGTranslation object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  CSGTranslation* createCSGTranslation();


  /**
   * Creates a new CSGRotation object, adds it to this CSGSetOperator object
   * and returns the CSGRotation object created.
   *
   * @return a new CSGRotation object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  CSGRotation* createCSGRotation();


  /**
   * Creates a new CSGScale object, adds it to this CSGSetOperator object and
   * returns the CSGScale object created.
   *
   * @return a new CSGScale object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  CSGScale* createCSGScale();


  /**
   * Creates a new CSGHomogeneousTransformation object, adds it to this
   * CSGSetOperator object and returns the CSGHomogeneousTransformation object
   * created.
   *
   * @return a new CSGHomogeneousTransformation object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  CSGHomogeneousTransformation* createCSGHomogeneousTransformation();


  /**
   * Creates a new CSGSetOperator object, adds it to this CSGSetOperator object
   * and returns the CSGSetOperator object created.
   *
   * @return a new CSGSetOperator object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   * @see removeCSGNode(unsigned int n)
   */
  CSGSetOperator* createCSGSetOperator();


  /**
   * Removes the nth CSGNode from this CSGSetOperator and returns a pointer to
   * it.
   *
   * @param n an unsigned int representing the index of the CSGNode to remove.
   *
   * @return a pointer to the nth CSGNode in this CSGSetOperator.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(const std::string& sid)
   */
  CSGNode* removeCSGNode(unsigned int n);


  /**
   * Removes the CSGNode from this CSGSetOperator based on its identifier and
   * returns a pointer to it.
   *
   * @param sid a string representing the identifier of the CSGNode to remove.
   *
   * @return the CSGNode in this CSGSetOperator based on the identifier or NULL
   * if no such CSGNode exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addCSGNode(const CSGNode* object)
   * @see createCSGNode()
   * @see getCSGNode(const std::string& sid)
   * @see getCSGNode(unsigned int n)
   * @see getNumCSGNodes()
   * @see removeCSGNode(unsigned int n)
   */
  CSGNode* removeCSGNode(const std::string& sid);


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this CSGSetOperator object.
   *
   * For CSGSetOperator, the XML element name is always @c "csgSetOperator".
   *
   * @return the name of this element, i.e. @c "csgSetOperator".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CSGSetOperator object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_CSGSETOPERATOR, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGSetOperator object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGSetOperator have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CSGSetOperator object are:
   * @li "operationType"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * CSGSetOperator object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * CSGSetOperator have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the CSGSetOperator object are:
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
   * Gets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Gets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Gets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Gets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Gets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Predicate returning @c true if this CSGSetOperator's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGSetOperator's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Sets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Sets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Sets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Sets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Unsets the value of the "attributeName" attribute of this CSGSetOperator.
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
   * Creates and returns an new "elementName" object in this CSGSetOperator.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this CSGSetOperator.
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
   * CSGSetOperator.
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
   * Returns the number of "elementName" in this CSGSetOperator.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this CSGSetOperator.
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
 * Creates a new CSGSetOperator_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CSGSetOperator_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGSetOperator_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGSetOperator_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGSetOperator_t *
CSGSetOperator_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CSGSetOperator_t object.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return a (deep) copy of this CSGSetOperator_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGSetOperator_t*
CSGSetOperator_clone(const CSGSetOperator_t* csgso);


/**
 * Frees this CSGSetOperator_t object.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
void
CSGSetOperator_free(CSGSetOperator_t* csgso);


/**
 * Returns the value of the "operationType" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure whose operationType is sought.
 *
 * @return the value of the "operationType" attribute of this CSGSetOperator_t
 * as a SetOperation_t.
 *
 * @copydetails doc_csgsetoperator_operationType
 * @if clike The value is drawn from the enumeration @ref SetOperation_t @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{SPATIAL_SETOPERATION_UNION, SetOperation_t}
 * @li @sbmlconstant{SPATIAL_SETOPERATION_INTERSECTION, SetOperation_t}
 * @li @sbmlconstant{SPATIAL_SETOPERATION_DIFFERENCE, SetOperation_t}
 * @li @sbmlconstant{SPATIAL_SETOPERATION_INVALID, SetOperation_t}
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
SetOperation_t
CSGSetOperator_getOperationType(const CSGSetOperator_t * csgso);


/**
 * Returns the value of the "operationType" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure whose operationType is sought.
 *
 * @return the value of the "operationType" attribute of this CSGSetOperator_t
 * as a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_csgsetoperator_operationType
 * The possible values returned by this method are:
 * @li @c "union"
 * @li @c "intersection"
 * @li @c "difference"
 * @li @c "invalid SetOperation value"
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
char *
CSGSetOperator_getOperationTypeAsString(const CSGSetOperator_t * csgso);


/**
 * Returns the value of the "complementA" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure whose complementA is sought.
 *
 * @return the value of the "complementA" attribute of this CSGSetOperator_t as
 * a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
char *
CSGSetOperator_getComplementA(const CSGSetOperator_t * csgso);


/**
 * Returns the value of the "complementB" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure whose complementB is sought.
 *
 * @return the value of the "complementB" attribute of this CSGSetOperator_t as
 * a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
char *
CSGSetOperator_getComplementB(const CSGSetOperator_t * csgso);


/**
 * Predicate returning @c 1 (true) if this CSGSetOperator_t's "operationType"
 * attribute is set.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return @c 1 (true) if this CSGSetOperator_t's "operationType" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_csgsetoperator_operationType
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_isSetOperationType(const CSGSetOperator_t * csgso);


/**
 * Predicate returning @c 1 (true) if this CSGSetOperator_t's "complementA"
 * attribute is set.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return @c 1 (true) if this CSGSetOperator_t's "complementA" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_isSetComplementA(const CSGSetOperator_t * csgso);


/**
 * Predicate returning @c 1 (true) if this CSGSetOperator_t's "complementB"
 * attribute is set.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return @c 1 (true) if this CSGSetOperator_t's "complementB" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_isSetComplementB(const CSGSetOperator_t * csgso);


/**
 * Sets the value of the "operationType" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @param operationType SetOperation_t value of the "operationType" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_csgsetoperator_operationType
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_setOperationType(CSGSetOperator_t * csgso,
                                SetOperation_t operationType);


/**
 * Sets the value of the "operationType" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @param operationType const char * of the "operationType" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_csgsetoperator_operationType
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_setOperationTypeAsString(CSGSetOperator_t * csgso,
                                        const char * operationType);


/**
 * Sets the value of the "complementA" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @param complementA const char * value of the "complementA" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_setComplementA(CSGSetOperator_t * csgso,
                              const char * complementA);


/**
 * Sets the value of the "complementB" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @param complementB const char * value of the "complementB" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_setComplementB(CSGSetOperator_t * csgso,
                              const char * complementB);


/**
 * Unsets the value of the "operationType" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_csgsetoperator_operationType
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_unsetOperationType(CSGSetOperator_t * csgso);


/**
 * Unsets the value of the "complementA" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_unsetComplementA(CSGSetOperator_t * csgso);


/**
 * Unsets the value of the "complementB" attribute of this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_unsetComplementB(CSGSetOperator_t * csgso);


/**
 * Returns a ListOf_t * containing CSGNode_t objects from this
 * CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure whose ListOfCSGNodes is sought.
 *
 * @return the ListOfCSGNodes from this CSGSetOperator_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see CSGSetOperator_addCSGNode()
 * @see CSGSetOperator_createCSGNode()
 * @see CSGSetOperator_getCSGNodeById()
 * @see CSGSetOperator_getCSGNode()
 * @see CSGSetOperator_getNumCSGNodes()
 * @see CSGSetOperator_removeCSGNodeById()
 * @see CSGSetOperator_removeCSGNode()
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
ListOf_t*
CSGSetOperator_getListOfCSGNodes(CSGSetOperator_t* csgso);


/**
 * Get a CSGNode_t from the CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure to search.
 *
 * @param n an unsigned int representing the index of the CSGNode_t to
 * retrieve.
 *
 * @return the nth CSGNode_t in the ListOfCSGNodes within this CSGSetOperator
 * or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_getCSGNode(CSGSetOperator_t* csgso, unsigned int n);


/**
 * Get a CSGNode_t from the CSGSetOperator_t based on its identifier.
 *
 * @param csgso the CSGSetOperator_t structure to search.
 *
 * @param sid a string representing the identifier of the CSGNode_t to
 * retrieve.
 *
 * @return the CSGNode_t in the ListOfCSGNodes within this CSGSetOperator with
 * the given @p sid or @c NULL if no such CSGNode_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_getCSGNodeById(CSGSetOperator_t* csgso, const char *sid);


/**
 * Adds a copy of the given CSGNode_t to this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure to which the CSGNode_t should be
 * added.
 *
 * @param csgn the CSGNode_t object to add.
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
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_addCSGNode(CSGSetOperator_t* csgso, const CSGNode_t* csgn);


/**
 * Get the number of CSGNode_t objects in this CSGSetOperator_t.
 *
 * @param csgso the CSGSetOperator_t structure to query.
 *
 * @return the number of CSGNode_t objects in this CSGSetOperator_t.
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
unsigned int
CSGSetOperator_getNumCSGNodes(CSGSetOperator_t* csgso);


/**
 * Creates a new CSGPrimitive_t object, adds it to this CSGSetOperator_t object
 * and returns the CSGPrimitive_t object created.
 *
 * @param csgso the CSGSetOperator_t structure to which the CSGPrimitive_t
 * should be added.
 *
 * @return a new CSGPrimitive_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGPrimitive_t*
CSGSetOperator_createCSGPrimitive(CSGSetOperator_t* csgso);


/**
 * Creates a new CSGTranslation_t object, adds it to this CSGSetOperator_t
 * object and returns the CSGTranslation_t object created.
 *
 * @param csgso the CSGSetOperator_t structure to which the CSGTranslation_t
 * should be added.
 *
 * @return a new CSGTranslation_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGTranslation_t*
CSGSetOperator_createCSGTranslation(CSGSetOperator_t* csgso);


/**
 * Creates a new CSGRotation_t object, adds it to this CSGSetOperator_t object
 * and returns the CSGRotation_t object created.
 *
 * @param csgso the CSGSetOperator_t structure to which the CSGRotation_t
 * should be added.
 *
 * @return a new CSGRotation_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGRotation_t*
CSGSetOperator_createCSGRotation(CSGSetOperator_t* csgso);


/**
 * Creates a new CSGScale_t object, adds it to this CSGSetOperator_t object and
 * returns the CSGScale_t object created.
 *
 * @param csgso the CSGSetOperator_t structure to which the CSGScale_t should
 * be added.
 *
 * @return a new CSGScale_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGScale_t*
CSGSetOperator_createCSGScale(CSGSetOperator_t* csgso);


/**
 * Creates a new CSGHomogeneousTransformation_t object, adds it to this
 * CSGSetOperator_t object and returns the CSGHomogeneousTransformation_t
 * object created.
 *
 * @param csgso the CSGSetOperator_t structure to which the
 * CSGHomogeneousTransformation_t should be added.
 *
 * @return a new CSGHomogeneousTransformation_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t*
CSGSetOperator_createCSGHomogeneousTransformation(CSGSetOperator_t* csgso);


/**
 * Creates a new CSGSetOperator_t object, adds it to this CSGSetOperator_t
 * object and returns the CSGSetOperator_t object created.
 *
 * @param csgso the CSGSetOperator_t structure to which the CSGSetOperator_t
 * should be added.
 *
 * @return a new CSGSetOperator_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGSetOperator_t*
CSGSetOperator_createCSGSetOperator(CSGSetOperator_t* csgso);


/**
 * Removes the nth CSGNode_t from this CSGSetOperator_t and returns a pointer
 * to it.
 *
 * @param csgso the CSGSetOperator_t structure to search.
 *
 * @param n an unsigned int representing the index of the CSGNode_t to remove.
 *
 * @return a pointer to the nth CSGNode_t in this CSGSetOperator_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_removeCSGNode(CSGSetOperator_t* csgso, unsigned int n);


/**
 * Removes the CSGNode_t from this CSGSetOperator_t based on its identifier and
 * returns a pointer to it.
 *
 * @param csgso the CSGSetOperator_t structure to search.
 *
 * @param sid a string representing the identifier of the CSGNode_t to remove.
 *
 * @return the CSGNode_t in this CSGSetOperator_t based on the identifier or
 * NULL if no such CSGNode_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_removeCSGNodeById(CSGSetOperator_t* csgso, const char* sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGSetOperator_t object have been set.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CSGSetOperator_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the CSGSetOperator_t object are:
 * @li "operationType"
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_hasRequiredAttributes(const CSGSetOperator_t * csgso);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * CSGSetOperator_t object have been set.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * CSGSetOperator_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the CSGSetOperator_t object are:
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_hasRequiredElements(const CSGSetOperator_t * csgso);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGSetOperator_H__ */


