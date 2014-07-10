/**
 * @file:   CSGNode.h
 * @brief:  Implementation of the CSGNode class
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


#ifndef CSGNode_H__
#define CSGNode_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN

class CSGPrimitive;
class CSGTranslation;
class CSGRotation;
class CSGScale;
class CSGHomogeneousTransformation;
class CSGPseudoPrimitive;
class CSGSetOperator;



class LIBSBML_EXTERN CSGNode : public SBase
{

protected:

  std::string   mId;


public:

  /**
   * Creates a new CSGNode with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGNode
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGNode
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGNode
   */
  CSGNode(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGNode with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGNode(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGNode.
   *
   * @param orig; the CSGNode instance to copy.
   */
  CSGNode(const CSGNode& orig);


   /**
   * Assignment operator for CSGNode.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGNode& operator=(const CSGNode& rhs);


   /**
   * Creates and returns a deep copy of this CSGNode object.
   *
   * @return a (deep) copy of this CSGNode object.
   */
  virtual CSGNode* clone () const;


   /**
   * Destructor for CSGNode.
   */
  virtual ~CSGNode();


   /**
   * Returns the value of the "id" attribute of this CSGNode.
   *
   * @return the value of the "id" attribute of this CSGNode as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGNode's "id" attribute has been set.
   *
   * @return @c true if this CSGNode's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this CSGNode.
   *
   * @param id; const std::string& value of the "id" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this CSGNode.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetId();


  /**
   * Returns @c true, if this abstract "CSGNode" is of type CSGPrimitive.
   *
   * @return @c true, if this abstract "CSGNode" is of type CSGPrimitive.
   *
   */
  virtual bool isCSGPrimitive() const;


  /**
   * Returns @c true, if this abstract "CSGNode" is of type CSGTranslation.
   *
   * @return @c true, if this abstract "CSGNode" is of type CSGTranslation.
   *
   */
  virtual bool isCSGTranslation() const;


  /**
   * Returns @c true, if this abstract "CSGNode" is of type CSGRotation.
   *
   * @return @c true, if this abstract "CSGNode" is of type CSGRotation.
   *
   */
  virtual bool isCSGRotation() const;


  /**
   * Returns @c true, if this abstract "CSGNode" is of type CSGScale.
   *
   * @return @c true, if this abstract "CSGNode" is of type CSGScale.
   *
   */
  virtual bool isCSGScale() const;


  /**
   * Returns @c true, if this abstract "CSGNode" is of type CSGHomogeneousTransformation.
   *
   * @return @c true, if this abstract "CSGNode" is of type CSGHomogeneousTransformation.
   *
   */
  virtual bool isCSGHomogeneousTransformation() const;


  /**
   * Returns @c true, if this abstract "CSGNode" is of type CSGPseudoPrimitive.
   *
   * @return @c true, if this abstract "CSGNode" is of type CSGPseudoPrimitive.
   *
   */
  virtual bool isCSGPseudoPrimitive() const;


  /**
   * Returns @c true, if this abstract "CSGNode" is of type CSGSetOperator.
   *
   * @return @c true, if this abstract "CSGNode" is of type CSGSetOperator.
   *
   */
  virtual bool isCSGSetOperator() const;


  /**
   * Returns the XML element name of this object, which for CSGNode, is
   * always @c "cSGNode".
   *
   * @return the name of this element, i.e. @c "cSGNode".
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
   * for this CSGNode object have been set.
   *
   * @note The required attributes for a CSGNode object are:
   * @li "id"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


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
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


protected:

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

class LIBSBML_EXTERN ListOfCSGNodes : public ListOf
{

public:

  /**
   * Creates a new ListOfCSGNodes with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfCSGNodes
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfCSGNodes
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfCSGNodes
   */
  ListOfCSGNodes(unsigned int level      = SpatialExtension::getDefaultLevel(),
                 unsigned int version    = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCSGNodes with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfCSGNodes(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfCSGNodes object.
   *
   * @return a (deep) copy of this ListOfCSGNodes object.
   */
  virtual ListOfCSGNodes* clone () const;


   /**
   * Get a CsgNode from the ListOfCSGNodes.
   *
   * @param n the index number of the CsgNode to get.
   *
   * @return the nth CsgNode in this ListOfCSGNodes.
   *
   * @see size()
   */
	virtual CSGNode* get(unsigned int n);


  /**
   * Get a CsgNode from the ListOfCSGNodes.
   *
   * @param n the index number of the CsgNode to get.
   *
   * @return the nth CsgNode in this ListOfCSGNodes.
   *
   * @see size()
   */
	virtual const CSGNode* get(unsigned int n) const;


  /**
   * Get a CsgNode from the ListOfCSGNodes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CsgNode to get.
   *
   * @return CsgNode in this ListOfCSGNodes
   * with the given id or NULL if no such
   * CsgNode exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual CSGNode* get(const std::string& sid);


  /**
   * Get a CsgNode from the ListOfCSGNodes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CsgNode to get.
   *
   * @return CsgNode in this ListOfCSGNodes
   * with the given id or NULL if no such
   * CsgNode exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const CSGNode* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "CsgNode" to this ListOfCSGNodes.
	 *
	 * @param cn; the CsgNode object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addCsgNode(const CSGNode* cn);


	/**
	 * Get the number of CsgNode objects in this ListOfCSGNodes.
	 *
	 * @return the number of CsgNode objects in this ListOfCSGNodes
	 */
	unsigned int getNumCsgNodes() const;


	/**
	 * Creates a new CsgNode object, adds it to the
	 * ListOfCSGNodes and returns the CsgNode object created. 
	 *
	 * @return a new CsgNode object instance
	 *
	 * @see addCsgNode(const CSGNode* cn)
	 */
	CSGPrimitive* createCsgPrimitive();


	/**
	 * Creates a new CsgNode object, adds it to the
	 * ListOfCSGNodes and returns the CsgNode object created. 
	 *
	 * @return a new CsgNode object instance
	 *
	 * @see addCsgNode(const CSGNode* cn)
	 */
	CSGTranslation* createCsgTranslation();


	/**
	 * Creates a new CsgNode object, adds it to the
	 * ListOfCSGNodes and returns the CsgNode object created. 
	 *
	 * @return a new CsgNode object instance
	 *
	 * @see addCsgNode(const CSGNode* cn)
	 */
	CSGRotation* createCsgRotation();


	/**
	 * Creates a new CsgNode object, adds it to the
	 * ListOfCSGNodes and returns the CsgNode object created. 
	 *
	 * @return a new CsgNode object instance
	 *
	 * @see addCsgNode(const CSGNode* cn)
	 */
	CSGScale* createCsgScale();


	/**
	 * Creates a new CsgNode object, adds it to the
	 * ListOfCSGNodes and returns the CsgNode object created. 
	 *
	 * @return a new CsgNode object instance
	 *
	 * @see addCsgNode(const CSGNode* cn)
	 */
	CSGHomogeneousTransformation* createCsgHomogeneousTransformation();


	/**
	 * Creates a new CsgNode object, adds it to the
	 * ListOfCSGNodes and returns the CsgNode object created. 
	 *
	 * @return a new CsgNode object instance
	 *
	 * @see addCsgNode(const CSGNode* cn)
	 */
	CSGPseudoPrimitive* createCsgPseudoPrimitive();


	/**
	 * Creates a new CsgNode object, adds it to the
	 * ListOfCSGNodes and returns the CsgNode object created. 
	 *
	 * @return a new CsgNode object instance
	 *
	 * @see addCsgNode(const CSGNode* cn)
	 */
	CSGSetOperator* createCsgSetOperator();


  /**
   * Removes the nth CsgNode from this ListOfCSGNodes
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the CsgNode to remove.
   *
   * @see size()
   */
	virtual CSGNode* remove(unsigned int n);


  /**
   * Removes the CsgNode from this ListOfCSGNodes with the given identifier
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
	virtual CSGNode* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfCSGNodes, is
   * always @c "listOfCSGNodes".
   *
   * @return the name of this element, i.e. @c "listOfCSGNodes".
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
   * Returns the libSBML type code for the SBML objects
   * contained in this ListOf object
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
   * @return the SBML type code for the objects in this ListOf instance, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getItemTypeCode () const;


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new CsgNode in this ListOfCsgNodes
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Spatial package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


	virtual bool isValidTypeForList(SBase * item) {
		int code = item->getTypeCode();
		return code == getItemTypeCode() || code == SBML_SPATIAL_CSGPRIMITIVE || code == SBML_SPATIAL_CSGTRANSLATION || code == SBML_SPATIAL_CSGROTATION || code == SBML_SPATIAL_CSGSCALE || code == SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION || code == SBML_SPATIAL_CSGPSEUDOPRIMITIVE || code == SBML_SPATIAL_CSGSETOPERATOR ;
	}



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new CSGNode_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGNode_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGNode_t structure.
 *
 * @returns the newly-created CSGNode_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGNode_t
 */
LIBSBML_EXTERN
CSGNode_t *
CSGNode_create(unsigned int level, unsigned int version,
               unsigned int pkgVersion);


/**
 * Frees the given CSGNode_t structure.
 * 
 * @param csgn the CSGNode_t structure to be freed.
 *
 * @memberof CSGNode_t
 */
LIBSBML_EXTERN
void
CSGNode_free(CSGNode_t * csgn);


/**
 * Creates a deep copy of the given CSGNode_t structure.
 * 
 * @param csgn the CSGNode_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGNode_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGNode_t
 */
LIBSBML_EXTERN
CSGNode_t *
CSGNode_clone(CSGNode_t * csgn);


/**
 * Returns the value of the "id" attribute of the given CSGNode_t
 * structure.
 *
 * @param csgn the CSGNode_t structure.
 *
 * @return the id of this structure.
 *
 * @member of CSGNode_t
 */
LIBSBML_EXTERN
const char *
CSGNode_getId(const CSGNode_t * csgn);


/**
 * Predicate returning @c 1 if the given CSGNode_t structure's "id"
 * is set.
 *
 * @param csgn the CSGNode_t structure.
 *
 * @return @c 1 if the "id" of this CSGNode_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGNode_t
 */
LIBSBML_EXTERN
int
CSGNode_isSetId(const CSGNode_t * csgn);


/**
 * Sets the "id" attribute of the given CSGNode_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CSGNode_unsetId() instead.
 *
 * @param csgn the CSGNode_t structure.
 *
 * @param id the string to which the structures "id" attribute should be
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
 * @member of CSGNode_t
 */
LIBSBML_EXTERN
int
CSGNode_setId(CSGNode_t * csgn, const char * id);


/**
 * Unsets the value of the "id" attribute of the given 
 *CSGNode_t structure.
 *
 * @param csgn the CSGNode_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGNode_t
 */
LIBSBML_EXTERN
int
CSGNode_unsetId(CSGNode_t * csgn);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGNode_t structure have been set.
 *
 * @param csgn the CSGNode_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGNode_t
 */
LIBSBML_EXTERN
int
CSGNode_hasRequiredAttributes(const CSGNode_t * csgn);


LIBSBML_EXTERN
CSGNode_t *
ListOfCsgNodes_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
CSGNode_t *
ListOfCsgNodes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGNode_H__  */

