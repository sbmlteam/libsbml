/**
 * @file:   CSGObject.h
 * @brief:  Implementation of the CSGObject class
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


#ifndef CSGObject_H__
#define CSGObject_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/packages/spatial/sbml/CSGNode.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN CSGObject : public SBase
{

protected:

  std::string   mId;
  std::string   mDomainType;
  int           mOrdinal;
  bool          mIsSetOrdinal;
  CSGNode*      mCsgNode;


public:

  /**
   * Creates a new CSGObject with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGObject
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGObject
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGObject
   */
  CSGObject(unsigned int level      = SpatialExtension::getDefaultLevel(),
            unsigned int version    = SpatialExtension::getDefaultVersion(),
            unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGObject with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGObject(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGObject.
   *
   * @param orig; the CSGObject instance to copy.
   */
  CSGObject(const CSGObject& orig);


   /**
   * Assignment operator for CSGObject.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGObject& operator=(const CSGObject& rhs);


   /**
   * Creates and returns a deep copy of this CSGObject object.
   *
   * @return a (deep) copy of this CSGObject object.
   */
  virtual CSGObject* clone () const;


   /**
   * Destructor for CSGObject.
   */
  virtual ~CSGObject();


   /**
   * Returns the value of the "id" attribute of this CSGObject.
   *
   * @return the value of the "id" attribute of this CSGObject as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "domainType" attribute of this CSGObject.
   *
   * @return the value of the "domainType" attribute of this CSGObject as a string.
   */
  virtual const std::string& getDomainType() const;


  /**
   * Returns the value of the "ordinal" attribute of this CSGObject.
   *
   * @return the value of the "ordinal" attribute of this CSGObject as a integer.
   */
  virtual int getOrdinal() const;


  /**
   * Returns the "csgNode" element of this CSGObject.
   *
   * @return the "csgNode" element of this CSGObject.
   */
  virtual const CSGNode* getCsgNode() const;


  /**
   * Returns the "csgNode" element of this CSGObject.
   *
   * @return the "csgNode" element of this CSGObject.
   */
  virtual CSGNode* getCsgNode();


  /**
   * Creates a new "csgNode" and sets it for this CSGObject.
   */
  virtual CSGPrimitive* createCsgPrimitive();


  /**
   * Creates a new "csgNode" and sets it for this CSGObject.
   */
  virtual CSGTranslation* createCsgTranslation();


  /**
   * Creates a new "csgNode" and sets it for this CSGObject.
   */
  virtual CSGRotation* createCsgRotation();


  /**
   * Creates a new "csgNode" and sets it for this CSGObject.
   */
  virtual CSGScale* createCsgScale();


  /**
   * Creates a new "csgNode" and sets it for this CSGObject.
   */
  virtual CSGHomogeneousTransformation* createCsgHomogeneousTransformation();


  /**
   * Creates a new "csgNode" and sets it for this CSGObject.
   */
  virtual CSGPseudoPrimitive* createCsgPseudoPrimitive();


  /**
   * Creates a new "csgNode" and sets it for this CSGObject.
   */
  virtual CSGSetOperator* createCsgSetOperator();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGObject's "id" attribute has been set.
   *
   * @return @c true if this CSGObject's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGObject's "domainType" attribute has been set.
   *
   * @return @c true if this CSGObject's "domainType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDomainType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGObject's "ordinal" attribute has been set.
   *
   * @return @c true if this CSGObject's "ordinal" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetOrdinal() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGObject's "csgNode" element has been set.
   *
   * @return @c true if this CSGObject's "csgNode" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCsgNode() const;


  /**
   * Sets the value of the "id" attribute of this CSGObject.
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
   * Sets the value of the "domainType" attribute of this CSGObject.
   *
   * @param domainType; const std::string& value of the "domainType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDomainType(const std::string& domainType);


  /**
   * Sets the value of the "ordinal" attribute of this CSGObject.
   *
   * @param ordinal; int value of the "ordinal" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setOrdinal(int ordinal);


  /**
   * Sets the "csgNode" element of this CSGObject.
   *
   * @param csgNode; CSGNode* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCsgNode(CSGNode* csgNode);


  /**
   * Unsets the value of the "id" attribute of this CSGObject.
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
   * Unsets the value of the "domainType" attribute of this CSGObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomainType();


  /**
   * Unsets the value of the "ordinal" attribute of this CSGObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetOrdinal();


  /**
   * Unsets the "csgNode" element of this CSGObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCsgNode();


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
   * Returns the XML element name of this object, which for CSGObject, is
   * always @c "cSGObject".
   *
   * @return the name of this element, i.e. @c "cSGObject".
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
   * for this CSGObject object have been set.
   *
   * @note The required attributes for a CSGObject object are:
   * @li "id"
   * @li "domainType"
   * @li "ordinal"
   * @li "csgNode"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this CSGObject object have been set.
   *
   * @note The required elements for a CSGObject object are:
   * @li "csgNode"
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

class LIBSBML_EXTERN ListOfCSGObjects : public ListOf
{

public:

  /**
   * Creates a new ListOfCSGObjects with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfCSGObjects
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfCSGObjects
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfCSGObjects
   */
  ListOfCSGObjects(unsigned int level      = SpatialExtension::getDefaultLevel(),
                   unsigned int version    = SpatialExtension::getDefaultVersion(),
                   unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCSGObjects with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfCSGObjects(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfCSGObjects object.
   *
   * @return a (deep) copy of this ListOfCSGObjects object.
   */
  virtual ListOfCSGObjects* clone () const;


   /**
   * Get a CsgObject from the ListOfCSGObjects.
   *
   * @param n the index number of the CsgObject to get.
   *
   * @return the nth CsgObject in this ListOfCSGObjects.
   *
   * @see size()
   */
	virtual CSGObject* get(unsigned int n);


  /**
   * Get a CsgObject from the ListOfCSGObjects.
   *
   * @param n the index number of the CsgObject to get.
   *
   * @return the nth CsgObject in this ListOfCSGObjects.
   *
   * @see size()
   */
	virtual const CSGObject* get(unsigned int n) const;


  /**
   * Get a CsgObject from the ListOfCSGObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CsgObject to get.
   *
   * @return CsgObject in this ListOfCSGObjects
   * with the given id or NULL if no such
   * CsgObject exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual CSGObject* get(const std::string& sid);


  /**
   * Get a CsgObject from the ListOfCSGObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CsgObject to get.
   *
   * @return CsgObject in this ListOfCSGObjects
   * with the given id or NULL if no such
   * CsgObject exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const CSGObject* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "CsgObject" to this ListOfCSGObjects.
	 *
	 * @param co; the CsgObject object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addCsgObject(const CSGObject* co);


	/**
	 * Get the number of CsgObject objects in this ListOfCSGObjects.
	 *
	 * @return the number of CsgObject objects in this ListOfCSGObjects
	 */
	unsigned int getNumCsgObjects() const;


	/**
	 * Creates a new CsgObject object, adds it to the
	 * ListOfCSGObjects and returns the CsgObject object created. 
	 *
	 * @return a new CsgObject object instance
	 *
	 * @see addCsgObject(const CSGObject* co)
	 */
	CSGObject* createCsgObject();


  /**
   * Removes the nth CsgObject from this ListOfCSGObjects
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the CsgObject to remove.
   *
   * @see size()
   */
	virtual CSGObject* remove(unsigned int n);


  /**
   * Removes the CsgObject from this ListOfCSGObjects with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the CsgObject to remove.
   *
   * @return the CsgObject removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual CSGObject* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfCSGObjects, is
   * always @c "listOfCSGObjects".
   *
   * @return the name of this element, i.e. @c "listOfCSGObjects".
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
   * Creates a new CsgObject in this ListOfCsgObjects
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Spatial package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new CSGObject_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGObject_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGObject_t structure.
 *
 * @returns the newly-created CSGObject_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGObject_t *
CSGObject_create(unsigned int level, unsigned int version,
                 unsigned int pkgVersion);


/**
 * Frees the given CSGObject_t structure.
 * 
 * @param csgo the CSGObject_t structure to be freed.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
void
CSGObject_free(CSGObject_t * csgo);


/**
 * Creates a deep copy of the given CSGObject_t structure.
 * 
 * @param csgo the CSGObject_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGObject_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGObject_t *
CSGObject_clone(CSGObject_t * csgo);


/**
 * Returns the value of the "id" attribute of the given CSGObject_t
 * structure.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return the id of this structure.
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
const char *
CSGObject_getId(const CSGObject_t * csgo);


/**
 * Returns the value of the "domainType" attribute of the given CSGObject_t
 * structure.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return the domainType of this structure.
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
const char *
CSGObject_getDomainType(const CSGObject_t * csgo);


/**
 * Returns the value of the "ordinal" attribute of the given CSGObject_t
 * structure.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return the ordinal of this structure.
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_getOrdinal(const CSGObject_t * csgo);


LIBSBML_EXTERN
CSGNode_t*
CSGObject_getCsgNode(CSGObject_t * csgo);


LIBSBML_EXTERN
CSGPrimitive_t *
CSGObject_createCsgPrimitive(CSGObject_t * csgo);


LIBSBML_EXTERN
CSGTranslation_t *
CSGObject_createCsgTranslation(CSGObject_t * csgo);


LIBSBML_EXTERN
CSGRotation_t *
CSGObject_createCsgRotation(CSGObject_t * csgo);


LIBSBML_EXTERN
CSGScale_t *
CSGObject_createCsgScale(CSGObject_t * csgo);


LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGObject_createCsgHomogeneousTransformation(CSGObject_t * csgo);


LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGObject_createCsgPseudoPrimitive(CSGObject_t * csgo);


LIBSBML_EXTERN
CSGSetOperator_t *
CSGObject_createCsgSetOperator(CSGObject_t * csgo);


/**
 * Predicate returning @c 1 if the given CSGObject_t structure's "id"
 * is set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 if the "id" of this CSGObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_isSetId(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 if the given CSGObject_t structure's "domainType"
 * is set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 if the "domainType" of this CSGObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_isSetDomainType(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 if the given CSGObject_t structure's "ordinal"
 * is set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 if the "ordinal" of this CSGObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_isSetOrdinal(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 if the given CSGObject_t structure's "csgNode"
 * is set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 if the "csgNode" of this CSGObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_isSetCsgNode(const CSGObject_t * csgo);


/**
 * Sets the "id" attribute of the given CSGObject_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CSGObject_unsetId() instead.
 *
 * @param csgo the CSGObject_t structure.
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
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_setId(CSGObject_t * csgo, const char * id);


/**
 * Sets the "domainType" attribute of the given CSGObject_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CSGObject_unsetDomainType() instead.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @param domainType the string to which the structures "domainType" attribute should be
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
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_setDomainType(CSGObject_t * csgo, const char * domainType);


/**
 * Sets the "ordinal" attribute of the given CSGObject_t structure.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @param ordinal the string to which the structures "ordinal" attribute should be
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
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_setOrdinal(CSGObject_t * csgo, int ordinal);


LIBSBML_EXTERN
int
CSGObject_setCsgNode(CSGObject_t * csgo, CSGNode_t* csgNode);


/**
 * Unsets the value of the "id" attribute of the given 
 *CSGObject_t structure.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_unsetId(CSGObject_t * csgo);


/**
 * Unsets the value of the "domainType" attribute of the given 
 *CSGObject_t structure.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_unsetDomainType(CSGObject_t * csgo);


/**
 * Unsets the value of the "ordinal" attribute of the given 
 *CSGObject_t structure.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_unsetOrdinal(CSGObject_t * csgo);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGObject_t structure have been set.
 *
 * @param csgo the CSGObject_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_hasRequiredAttributes(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given CSGObject_t structure have been set.
 *
 * @param csgo the CSGObject_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_hasRequiredElements(const CSGObject_t * csgo);


LIBSBML_EXTERN
CSGObject_t *
ListOfCsgObjects_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
CSGObject_t *
ListOfCsgObjects_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGObject_H__  */

