/**
 * @file:   ParametricObject.h
 * @brief:  Implementation of the ParametricObject class
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


#ifndef ParametricObject_H__
#define ParametricObject_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/packages/spatial/sbml/PolygonObject.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN ParametricObject : public SBase
{

protected:

  std::string   mId;
  std::string   mPolygonType;
  std::string   mDomain;
  PolygonObject*      mPolygonObject;


public:

  /**
   * Creates a new ParametricObject with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ParametricObject
   *
   * @param version an unsigned int, the SBML Version to assign to this ParametricObject
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ParametricObject
   */
  ParametricObject(unsigned int level      = SpatialExtension::getDefaultLevel(),
                   unsigned int version    = SpatialExtension::getDefaultVersion(),
                   unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ParametricObject with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ParametricObject(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for ParametricObject.
   *
   * @param orig; the ParametricObject instance to copy.
   */
  ParametricObject(const ParametricObject& orig);


   /**
   * Assignment operator for ParametricObject.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  ParametricObject& operator=(const ParametricObject& rhs);


   /**
   * Creates and returns a deep copy of this ParametricObject object.
   *
   * @return a (deep) copy of this ParametricObject object.
   */
  virtual ParametricObject* clone () const;


   /**
   * Destructor for ParametricObject.
   */
  virtual ~ParametricObject();


   /**
   * Returns the value of the "id" attribute of this ParametricObject.
   *
   * @return the value of the "id" attribute of this ParametricObject as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "polygonType" attribute of this ParametricObject.
   *
   * @return the value of the "polygonType" attribute of this ParametricObject as a string.
   */
  virtual const std::string& getPolygonType() const;


  /**
   * Returns the value of the "domain" attribute of this ParametricObject.
   *
   * @return the value of the "domain" attribute of this ParametricObject as a string.
   */
  virtual const std::string& getDomain() const;


  /**
   * Returns the "polygonObject" element of this ParametricObject.
   *
   * @return the "polygonObject" element of this ParametricObject.
   */
  virtual const PolygonObject* getPolygonObject() const;


  /**
   * Returns the "polygonObject" element of this ParametricObject.
   *
   * @return the "polygonObject" element of this ParametricObject.
   */
  virtual PolygonObject* getPolygonObject();


  /**
   * Creates a new "PolygonObject" and sets it for this ParametricObject.
   *
   * @return the created "PolygonObject" element of this ParametricObject.
   */
  virtual PolygonObject* createPolygonObject();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ParametricObject's "id" attribute has been set.
   *
   * @return @c true if this ParametricObject's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ParametricObject's "polygonType" attribute has been set.
   *
   * @return @c true if this ParametricObject's "polygonType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetPolygonType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ParametricObject's "domain" attribute has been set.
   *
   * @return @c true if this ParametricObject's "domain" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDomain() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ParametricObject's "polygonObject" element has been set.
   *
   * @return @c true if this ParametricObject's "polygonObject" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetPolygonObject() const;


  /**
   * Sets the value of the "id" attribute of this ParametricObject.
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
   * Sets the value of the "polygonType" attribute of this ParametricObject.
   *
   * @param polygonType; const std::string& value of the "polygonType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setPolygonType(const std::string& polygonType);


  /**
   * Sets the value of the "domain" attribute of this ParametricObject.
   *
   * @param domain; const std::string& value of the "domain" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDomain(const std::string& domain);


  /**
   * Sets the "polygonObject" element of this ParametricObject.
   *
   * @param polygonObject; PolygonObject* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setPolygonObject(PolygonObject* polygonObject);


  /**
   * Unsets the value of the "id" attribute of this ParametricObject.
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
   * Unsets the value of the "polygonType" attribute of this ParametricObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetPolygonType();


  /**
   * Unsets the value of the "domain" attribute of this ParametricObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomain();


  /**
   * Unsets the "polygonObject" element of this ParametricObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetPolygonObject();


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
   * Returns the XML element name of this object, which for ParametricObject, is
   * always @c "parametricObject".
   *
   * @return the name of this element, i.e. @c "parametricObject".
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
   * for this ParametricObject object have been set.
   *
   * @note The required attributes for a ParametricObject object are:
   * @li "id"
   * @li "polygonType"
   * @li "domain"
   * @li "polygonObject"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this ParametricObject object have been set.
   *
   * @note The required elements for a ParametricObject object are:
   * @li "polygonObject"
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
 * Creates a new ParametricObject_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * ParametricObject_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * ParametricObject_t structure.
 *
 * @returns the newly-created ParametricObject_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof ParametricObject_t
 */
LIBSBML_EXTERN
ParametricObject_t *
ParametricObject_create(unsigned int level, unsigned int version,
                        unsigned int pkgVersion);


/**
 * Frees the given ParametricObject_t structure.
 * 
 * @param po the ParametricObject_t structure to be freed.
 *
 * @memberof ParametricObject_t
 */
LIBSBML_EXTERN
void
ParametricObject_free(ParametricObject_t * po);


/**
 * Creates a deep copy of the given ParametricObject_t structure.
 * 
 * @param po the ParametricObject_t structure to be copied.
 *
 * @returns a (deep) copy of the given ParametricObject_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof ParametricObject_t
 */
LIBSBML_EXTERN
ParametricObject_t *
ParametricObject_clone(ParametricObject_t * po);


/**
 * Returns the value of the "id" attribute of the given ParametricObject_t
 * structure.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return the id of this structure.
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
const char *
ParametricObject_getId(const ParametricObject_t * po);


/**
 * Returns the value of the "polygonType" attribute of the given ParametricObject_t
 * structure.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return the polygonType of this structure.
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
const char *
ParametricObject_getPolygonType(const ParametricObject_t * po);


/**
 * Returns the value of the "domain" attribute of the given ParametricObject_t
 * structure.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return the domain of this structure.
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
const char *
ParametricObject_getDomain(const ParametricObject_t * po);


LIBSBML_EXTERN
PolygonObject_t*
ParametricObject_getPolygonObject(ParametricObject_t * po);


LIBSBML_EXTERN
PolygonObject_t*
ParametricObject_createPolygonObject(ParametricObject_t * po);


/**
 * Predicate returning @c 1 if the given ParametricObject_t structure's "id"
 * is set.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return @c 1 if the "id" of this ParametricObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_isSetId(const ParametricObject_t * po);


/**
 * Predicate returning @c 1 if the given ParametricObject_t structure's "polygonType"
 * is set.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return @c 1 if the "polygonType" of this ParametricObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_isSetPolygonType(const ParametricObject_t * po);


/**
 * Predicate returning @c 1 if the given ParametricObject_t structure's "domain"
 * is set.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return @c 1 if the "domain" of this ParametricObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_isSetDomain(const ParametricObject_t * po);


/**
 * Predicate returning @c 1 if the given ParametricObject_t structure's "polygonObject"
 * is set.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return @c 1 if the "polygonObject" of this ParametricObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_isSetPolygonObject(const ParametricObject_t * po);


/**
 * Sets the "id" attribute of the given ParametricObject_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs ParametricObject_unsetId() instead.
 *
 * @param po the ParametricObject_t structure.
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
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_setId(ParametricObject_t * po, const char * id);


/**
 * Sets the "polygonType" attribute of the given ParametricObject_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs ParametricObject_unsetPolygonType() instead.
 *
 * @param po the ParametricObject_t structure.
 *
 * @param polygonType the string to which the structures "polygonType" attribute should be
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
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_setPolygonType(ParametricObject_t * po, const char * polygonType);


/**
 * Sets the "domain" attribute of the given ParametricObject_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs ParametricObject_unsetDomain() instead.
 *
 * @param po the ParametricObject_t structure.
 *
 * @param domain the string to which the structures "domain" attribute should be
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
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_setDomain(ParametricObject_t * po, const char * domain);


LIBSBML_EXTERN
int
ParametricObject_setPolygonObject(ParametricObject_t * po, PolygonObject_t* polygonObject);


/**
 * Unsets the value of the "id" attribute of the given 
 *ParametricObject_t structure.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_unsetId(ParametricObject_t * po);


/**
 * Unsets the value of the "polygonType" attribute of the given 
 *ParametricObject_t structure.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_unsetPolygonType(ParametricObject_t * po);


/**
 * Unsets the value of the "domain" attribute of the given 
 *ParametricObject_t structure.
 *
 * @param po the ParametricObject_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_unsetDomain(ParametricObject_t * po);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given ParametricObject_t structure have been set.
 *
 * @param po the ParametricObject_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_hasRequiredAttributes(const ParametricObject_t * po);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given ParametricObject_t structure have been set.
 *
 * @param po the ParametricObject_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of ParametricObject_t
 */
LIBSBML_EXTERN
int
ParametricObject_hasRequiredElements(const ParametricObject_t * po);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  ParametricObject_H__  */

