/**
 * @file:   CSGSetOperator.h
 * @brief:  Implementation of the CSGSetOperator class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#ifndef CSGSetOperator_H__
#define CSGSetOperator_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>

#include <sbml/packages/spatial/sbml/CSGNode.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN CSGSetOperator : public CSGNode
{

protected:

  SetOperation_t   mOperationType;
  std::string   mComplementA;
  std::string   mComplementB;
  ListOfCSGNodes   mCsgNodes;


public:

  /**
   * Creates a new CSGSetOperator with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGSetOperator
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGSetOperator
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGSetOperator
   */
  CSGSetOperator(unsigned int level      = SpatialExtension::getDefaultLevel(),
                 unsigned int version    = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGSetOperator with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGSetOperator(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGSetOperator.
   *
   * @param orig; the CSGSetOperator instance to copy.
   */
  CSGSetOperator(const CSGSetOperator& orig);


   /**
   * Assignment operator for CSGSetOperator.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGSetOperator& operator=(const CSGSetOperator& rhs);


   /**
   * Creates and returns a deep copy of this CSGSetOperator object.
   *
   * @return a (deep) copy of this CSGSetOperator object.
   */
  virtual CSGSetOperator* clone () const;


   /**
   * Destructor for CSGSetOperator.
   */
  virtual ~CSGSetOperator();


   /**
   * Returns the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @return the value of the "operationType" attribute of this CSGSetOperator as a SetOperation_t.
   */
  virtual SetOperation_t getOperationType() const;


  /**
   * Returns the value of the "complementA" attribute of this CSGSetOperator.
   *
   * @return the value of the "complementA" attribute of this CSGSetOperator as a string.
   */
  virtual const std::string& getComplementA() const;


  /**
   * Returns the value of the "complementB" attribute of this CSGSetOperator.
   *
   * @return the value of the "complementB" attribute of this CSGSetOperator as a string.
   */
  virtual const std::string& getComplementB() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGSetOperator's "operationType" attribute has been set.
   *
   * @return @c true if this CSGSetOperator's "operationType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetOperationType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGSetOperator's "complementA" attribute has been set.
   *
   * @return @c true if this CSGSetOperator's "complementA" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetComplementA() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGSetOperator's "complementB" attribute has been set.
   *
   * @return @c true if this CSGSetOperator's "complementB" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetComplementB() const;


  /**
   * Sets the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @param operationType; SetOperation_t value of the "operationType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setOperationType(SetOperation_t operationType);


  /**
   * Sets the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @param operationType; string value of the "operationType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setOperationType(const std::string& operationType);


  /**
   * Sets the value of the "complementA" attribute of this CSGSetOperator.
   *
   * @param complementA; const std::string& value of the "complementA" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setComplementA(const std::string& complementA);


  /**
   * Sets the value of the "complementB" attribute of this CSGSetOperator.
   *
   * @param complementB; const std::string& value of the "complementB" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setComplementB(const std::string& complementB);


  /**
   * Unsets the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetOperationType();


  /**
   * Unsets the value of the "complementA" attribute of this CSGSetOperator.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetComplementA();


  /**
   * Unsets the value of the "complementB" attribute of this CSGSetOperator.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetComplementB();


  /**
   * Returns the  "ListOfCSGNodes" in this CSGSetOperator object.
   *
   * @return the "ListOfCSGNodes" attribute of this CSGSetOperator.
   */
  const ListOfCSGNodes* getListOfCsgNodes() const;


  /**
   * Returns the  "ListOfCSGNodes" in this CSGSetOperator object.
   *
   * @return the "ListOfCSGNodes" attribute of this CSGSetOperator.
   */
  ListOfCSGNodes* getListOfCsgNodes();


  /**
   * Get a CsgNode from the ListOfCSGNodes.
   *
   * @param n the index number of the CsgNode to get.
   *
   * @return the nth CsgNode in the ListOfCSGNodes within this CSGSetOperator.
   *
   * @see getNumCsgNodes()
   */
	CSGNode* getCsgNode(unsigned int n);


  /**
   * Get a CsgNode from the ListOfCSGNodes.
   *
   * @param n the index number of the CsgNode to get.
   *
   * @return the nth CsgNode in the ListOfCSGNodes within this CSGSetOperator.
   *
   * @see getNumCsgNodes()
   */
	const CSGNode* getCsgNode(unsigned int n) const;


  /**
   * Get a CsgNode from the ListOfCSGNodes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CsgNode to get.
   *
   * @return the CsgNode in the ListOfCSGNodes
   * with the given id or NULL if no such
   * CsgNode exists.
   *
   * @see getCsgNode(unsigned int n)
   *
   * @see getNumCsgNodes()
   */
	CSGNode* getCsgNode(const std::string& sid);


  /**
   * Get a CsgNode from the ListOfCSGNodes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CsgNode to get.
   *
   * @return the CsgNode in the ListOfCSGNodes
   * with the given id or NULL if no such
   * CsgNode exists.
   *
   * @see getCsgNode(unsigned int n)
   *
   * @see getNumCsgNodes()
   */
	const CSGNode* getCsgNode(const std::string& sid) const;


  /**
   * Adds a copy the given "CSGNode" to this CSGSetOperator.
   *
   * @param csgn; the CSGNode object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addCsgNode(const CSGNode* csgn);


  /**
   * Get the number of CSGNode objects in this CSGSetOperator.
   *
   * @return the number of CSGNode objects in this CSGSetOperator
   */
  unsigned int getNumCsgNodes() const;


  /**
   * Creates a new CSGPrimitive object, adds it to this CSGSetOperators
   * ListOfCSGNodes and returns the CSGPrimitive object created. 
   *
   * @return a new CSGPrimitive object instance
   *
   * @see addCSGNode(const CSGNode* csgn)
   */
  CSGPrimitive* createCsgPrimitive();


  /**
   * Creates a new CSGTranslation object, adds it to this CSGSetOperators
   * ListOfCSGNodes and returns the CSGTranslation object created. 
   *
   * @return a new CSGTranslation object instance
   *
   * @see addCSGNode(const CSGNode* csgn)
   */
  CSGTranslation* createCsgTranslation();


  /**
   * Creates a new CSGRotation object, adds it to this CSGSetOperators
   * ListOfCSGNodes and returns the CSGRotation object created. 
   *
   * @return a new CSGRotation object instance
   *
   * @see addCSGNode(const CSGNode* csgn)
   */
  CSGRotation* createCsgRotation();


  /**
   * Creates a new CSGScale object, adds it to this CSGSetOperators
   * ListOfCSGNodes and returns the CSGScale object created. 
   *
   * @return a new CSGScale object instance
   *
   * @see addCSGNode(const CSGNode* csgn)
   */
  CSGScale* createCsgScale();


  /**
   * Creates a new CSGHomogeneousTransformation object, adds it to this CSGSetOperators
   * ListOfCSGNodes and returns the CSGHomogeneousTransformation object created. 
   *
   * @return a new CSGHomogeneousTransformation object instance
   *
   * @see addCSGNode(const CSGNode* csgn)
   */
  CSGHomogeneousTransformation* createCsgHomogeneousTransformation();


  /**
   * Creates a new CSGPseudoPrimitive object, adds it to this CSGSetOperators
   * ListOfCSGNodes and returns the CSGPseudoPrimitive object created. 
   *
   * @return a new CSGPseudoPrimitive object instance
   *
   * @see addCSGNode(const CSGNode* csgn)
   */
  CSGPseudoPrimitive* createCsgPseudoPrimitive();


  /**
   * Creates a new CSGSetOperator object, adds it to this CSGSetOperators
   * ListOfCSGNodes and returns the CSGSetOperator object created. 
   *
   * @return a new CSGSetOperator object instance
   *
   * @see addCSGNode(const CSGNode* csgn)
   */
  CSGSetOperator* createCsgSetOperator();


  /**
   * Removes the nth CsgNode from the ListOfCSGNodes within this CSGSetOperator.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the CsgNode to remove.
   *
   * @see getNumCsgNodes()
   */
	CSGNode* removeCsgNode(unsigned int n);


  /**
   * Removes the CsgNode with the given identifier from the ListOfCSGNodes within this CSGSetOperator
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the CsgNode to remove.
   *
   * @return the CsgNode removed. As mentioned above, the caller owns the
   * returned item.
   */
	CSGNode* removeCsgNode(const std::string& sid);


  /**
   * Renames all the @c SIdRef attributes on this element, including any
   * found in MathML content (if such exists).
   *
   * This method works by looking at all attributes and (if appropriate)
   * mathematical formulas, comparing the identifiers to the value of @p
   * oldid.  If any matches are found, the matching identifiers are replaced
   * with @p newid.  The method does @em not descend into child elements.
   *
   * @param oldid the old identifier
   * @param newid the new identifier
   */
   virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for CSGSetOperator, is
   * always @c "cSGSetOperator".
   *
   * @return the name of this element, i.e. @c "cSGSetOperator".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Predicate returning @c true if all the required attributes
   * for this CSGSetOperator object have been set.
   *
   * @note The required attributes for a CSGSetOperator object are:
   * @li "operationType"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this CSGSetOperator object have been set.
   *
   * @note The required elements for a CSGSetOperator object are:
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements.
   */
  virtual void connectToChild ();


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new CSGSetOperator_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGSetOperator_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGSetOperator_t structure.
 *
 * @returns the newly-created CSGSetOperator_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGSetOperator_t *
CSGSetOperator_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion);


/**
 * Frees the given CSGSetOperator_t structure.
 * 
 * @param csgso the CSGSetOperator_t structure to be freed.
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
void
CSGSetOperator_free(CSGSetOperator_t * csgso);


/**
 * Creates a deep copy of the given CSGSetOperator_t structure.
 * 
 * @param csgso the CSGSetOperator_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGSetOperator_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGSetOperator_t
 */
LIBSBML_EXTERN
CSGSetOperator_t *
CSGSetOperator_clone(CSGSetOperator_t * csgso);


/**
 * Returns the value of the "operationType" attribute of the given CSGSetOperator_t
 * structure.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return the operationType of this structure.
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
SetOperation_t
CSGSetOperator_getOperationType(const CSGSetOperator_t * csgso);


/**
 * Returns the value of the "complementA" attribute of the given CSGSetOperator_t
 * structure.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return the complementA of this structure.
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
const char *
CSGSetOperator_getComplementA(const CSGSetOperator_t * csgso);


/**
 * Returns the value of the "complementB" attribute of the given CSGSetOperator_t
 * structure.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return the complementB of this structure.
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
const char *
CSGSetOperator_getComplementB(const CSGSetOperator_t * csgso);


/**
 * Predicate returning @c 1 if the given CSGSetOperator_t structure's "operationType"
 * is set.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return @c 1 if the "operationType" of this CSGSetOperator_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_isSetOperationType(const CSGSetOperator_t * csgso);


/**
 * Predicate returning @c 1 if the given CSGSetOperator_t structure's "complementA"
 * is set.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return @c 1 if the "complementA" of this CSGSetOperator_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_isSetComplementA(const CSGSetOperator_t * csgso);


/**
 * Predicate returning @c 1 if the given CSGSetOperator_t structure's "complementB"
 * is set.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return @c 1 if the "complementB" of this CSGSetOperator_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_isSetComplementB(const CSGSetOperator_t * csgso);


/**
 * Sets the "operationType" attribute of the given CSGSetOperator_t structure.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @param operationType the string to which the structures "operationType" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_setOperationType(CSGSetOperator_t * csgso, SetOperation_t operationType);


/**
 * Sets the "complementA" attribute of the given CSGSetOperator_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CSGSetOperator_unsetComplementA() instead.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @param complementA the string to which the structures "complementA" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_setComplementA(CSGSetOperator_t * csgso, const char * complementA);


/**
 * Sets the "complementB" attribute of the given CSGSetOperator_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CSGSetOperator_unsetComplementB() instead.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @param complementB the string to which the structures "complementB" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_setComplementB(CSGSetOperator_t * csgso, const char * complementB);


/**
 * Unsets the value of the "operationType" attribute of the given 
 *CSGSetOperator_t structure.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_unsetOperationType(CSGSetOperator_t * csgso);


/**
 * Unsets the value of the "complementA" attribute of the given 
 *CSGSetOperator_t structure.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_unsetComplementA(CSGSetOperator_t * csgso);


/**
 * Unsets the value of the "complementB" attribute of the given 
 *CSGSetOperator_t structure.
 *
 * @param csgso the CSGSetOperator_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_unsetComplementB(CSGSetOperator_t * csgso);


LIBSBML_EXTERN
int
CSGSetOperator_addCsgNode(CSGSetOperator_t * csgso, CSGNode_t * csgn);


LIBSBML_EXTERN
CSGPrimitive_t *
CSGSetOperator_createCsgPrimitive(CSGSetOperator_t * csgso);


LIBSBML_EXTERN
CSGTranslation_t *
CSGSetOperator_createCsgTranslation(CSGSetOperator_t * csgso);


LIBSBML_EXTERN
CSGRotation_t *
CSGSetOperator_createCsgRotation(CSGSetOperator_t * csgso);


LIBSBML_EXTERN
CSGScale_t *
CSGSetOperator_createCsgScale(CSGSetOperator_t * csgso);


LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGSetOperator_createCsgHomogeneousTransformation(CSGSetOperator_t * csgso);


LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGSetOperator_createCsgPseudoPrimitive(CSGSetOperator_t * csgso);


LIBSBML_EXTERN
CSGSetOperator_t *
CSGSetOperator_createCsgSetOperator(CSGSetOperator_t * csgso);


LIBSBML_EXTERN
ListOf_t *
CSGSetOperator_getListOfCSGNodes(CSGSetOperator_t * csgso) ;


LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_getCsgNode(CSGSetOperator_t * csgso, unsigned int n);


LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_getCsgNodeById(CSGSetOperator_t * csgso, const char * sid);


LIBSBML_EXTERN
unsigned int
CSGSetOperator_getNumCsgNodes(CSGSetOperator_t * csgso);


LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_removeCsgNode(CSGSetOperator_t * csgso, unsigned int n);


LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_removeCsgNodeById(CSGSetOperator_t * csgso, const char * sid);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGSetOperator_t structure have been set.
 *
 * @param csgso the CSGSetOperator_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_hasRequiredAttributes(const CSGSetOperator_t * csgso);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given CSGSetOperator_t structure have been set.
 *
 * @param csgso the CSGSetOperator_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGSetOperator_t
 */
LIBSBML_EXTERN
int
CSGSetOperator_hasRequiredElements(const CSGSetOperator_t * csgso);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGSetOperator_H__  */

