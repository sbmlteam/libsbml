/**
 * @file:   OrdinalMapping.h
 * @brief:  Implementation of the OrdinalMapping class
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


#ifndef OrdinalMapping_H__
#define OrdinalMapping_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN OrdinalMapping : public SBase
{

protected:

  std::string   mGeometryDefinition;
  int           mOrdinal;
  bool          mIsSetOrdinal;


public:

  /**
   * Creates a new OrdinalMapping with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this OrdinalMapping
   *
   * @param version an unsigned int, the SBML Version to assign to this OrdinalMapping
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this OrdinalMapping
   */
  OrdinalMapping(unsigned int level      = SpatialExtension::getDefaultLevel(),
                 unsigned int version    = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new OrdinalMapping with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  OrdinalMapping(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for OrdinalMapping.
   *
   * @param orig; the OrdinalMapping instance to copy.
   */
  OrdinalMapping(const OrdinalMapping& orig);


   /**
   * Assignment operator for OrdinalMapping.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  OrdinalMapping& operator=(const OrdinalMapping& rhs);


   /**
   * Creates and returns a deep copy of this OrdinalMapping object.
   *
   * @return a (deep) copy of this OrdinalMapping object.
   */
  virtual OrdinalMapping* clone () const;


   /**
   * Destructor for OrdinalMapping.
   */
  virtual ~OrdinalMapping();


   /**
   * Returns the value of the "geometryDefinition" attribute of this OrdinalMapping.
   *
   * @return the value of the "geometryDefinition" attribute of this OrdinalMapping as a string.
   */
  virtual const std::string& getGeometryDefinition() const;


  /**
   * Returns the value of the "ordinal" attribute of this OrdinalMapping.
   *
   * @return the value of the "ordinal" attribute of this OrdinalMapping as a integer.
   */
  virtual int getOrdinal() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * OrdinalMapping's "geometryDefinition" attribute has been set.
   *
   * @return @c true if this OrdinalMapping's "geometryDefinition" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetGeometryDefinition() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * OrdinalMapping's "ordinal" attribute has been set.
   *
   * @return @c true if this OrdinalMapping's "ordinal" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetOrdinal() const;


  /**
   * Sets the value of the "geometryDefinition" attribute of this OrdinalMapping.
   *
   * @param geometryDefinition; const std::string& value of the "geometryDefinition" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setGeometryDefinition(const std::string& geometryDefinition);


  /**
   * Sets the value of the "ordinal" attribute of this OrdinalMapping.
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
   * Unsets the value of the "geometryDefinition" attribute of this OrdinalMapping.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetGeometryDefinition();


  /**
   * Unsets the value of the "ordinal" attribute of this OrdinalMapping.
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
   * Returns the XML element name of this object, which for OrdinalMapping, is
   * always @c "ordinalMapping".
   *
   * @return the name of this element, i.e. @c "ordinalMapping".
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
   * for this OrdinalMapping object have been set.
   *
   * @note The required attributes for a OrdinalMapping object are:
   * @li "geometryDefinition"
   * @li "ordinal"
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

class LIBSBML_EXTERN ListOfOrdinalMappings : public ListOf
{

public:

  /**
   * Creates a new ListOfOrdinalMappings with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfOrdinalMappings
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfOrdinalMappings
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfOrdinalMappings
   */
  ListOfOrdinalMappings(unsigned int level      = SpatialExtension::getDefaultLevel(),
                        unsigned int version    = SpatialExtension::getDefaultVersion(),
                        unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfOrdinalMappings with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfOrdinalMappings(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfOrdinalMappings object.
   *
   * @return a (deep) copy of this ListOfOrdinalMappings object.
   */
  virtual ListOfOrdinalMappings* clone () const;


   /**
   * Get a OrdinalMapping from the ListOfOrdinalMappings.
   *
   * @param n the index number of the OrdinalMapping to get.
   *
   * @return the nth OrdinalMapping in this ListOfOrdinalMappings.
   *
   * @see size()
   */
	virtual OrdinalMapping* get(unsigned int n);


  /**
   * Get a OrdinalMapping from the ListOfOrdinalMappings.
   *
   * @param n the index number of the OrdinalMapping to get.
   *
   * @return the nth OrdinalMapping in this ListOfOrdinalMappings.
   *
   * @see size()
   */
	virtual const OrdinalMapping* get(unsigned int n) const;


  /**
   * Get a OrdinalMapping from the ListOfOrdinalMappings
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the OrdinalMapping to get.
   *
   * @return OrdinalMapping in this ListOfOrdinalMappings
   * with the given id or NULL if no such
   * OrdinalMapping exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual OrdinalMapping* get(const std::string& sid);


  /**
   * Get a OrdinalMapping from the ListOfOrdinalMappings
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the OrdinalMapping to get.
   *
   * @return OrdinalMapping in this ListOfOrdinalMappings
   * with the given id or NULL if no such
   * OrdinalMapping exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const OrdinalMapping* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "OrdinalMapping" to this ListOfOrdinalMappings.
	 *
	 * @param om; the OrdinalMapping object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addOrdinalMapping(const OrdinalMapping* om);


	/**
	 * Get the number of OrdinalMapping objects in this ListOfOrdinalMappings.
	 *
	 * @return the number of OrdinalMapping objects in this ListOfOrdinalMappings
	 */
	unsigned int getNumOrdinalMappings() const;


	/**
	 * Creates a new OrdinalMapping object, adds it to the
	 * ListOfOrdinalMappings and returns the OrdinalMapping object created. 
	 *
	 * @return a new OrdinalMapping object instance
	 *
	 * @see addOrdinalMapping(const OrdinalMapping* om)
	 */
	OrdinalMapping* createOrdinalMapping();


  /**
   * Removes the nth OrdinalMapping from this ListOfOrdinalMappings
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the OrdinalMapping to remove.
   *
   * @see size()
   */
	virtual OrdinalMapping* remove(unsigned int n);


  /**
   * Removes the OrdinalMapping from this ListOfOrdinalMappings with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the OrdinalMapping to remove.
   *
   * @return the OrdinalMapping removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual OrdinalMapping* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfOrdinalMappings, is
   * always @c "listOfOrdinalMappings".
   *
   * @return the name of this element, i.e. @c "listOfOrdinalMappings".
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
   * Creates a new OrdinalMapping in this ListOfOrdinalMappings
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
 * Creates a new OrdinalMapping_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * OrdinalMapping_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * OrdinalMapping_t structure.
 *
 * @returns the newly-created OrdinalMapping_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
OrdinalMapping_t *
OrdinalMapping_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion);


/**
 * Frees the given OrdinalMapping_t structure.
 * 
 * @param om the OrdinalMapping_t structure to be freed.
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
void
OrdinalMapping_free(OrdinalMapping_t * om);


/**
 * Creates a deep copy of the given OrdinalMapping_t structure.
 * 
 * @param om the OrdinalMapping_t structure to be copied.
 *
 * @returns a (deep) copy of the given OrdinalMapping_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof OrdinalMapping_t
 */
LIBSBML_EXTERN
OrdinalMapping_t *
OrdinalMapping_clone(OrdinalMapping_t * om);


/**
 * Returns the value of the "geometryDefinition" attribute of the given OrdinalMapping_t
 * structure.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return the geometryDefinition of this structure.
 *
 * @member of OrdinalMapping_t
 */
LIBSBML_EXTERN
const char *
OrdinalMapping_getGeometryDefinition(const OrdinalMapping_t * om);


/**
 * Returns the value of the "ordinal" attribute of the given OrdinalMapping_t
 * structure.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return the ordinal of this structure.
 *
 * @member of OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_getOrdinal(const OrdinalMapping_t * om);


/**
 * Predicate returning @c 1 if the given OrdinalMapping_t structure's "geometryDefinition"
 * is set.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return @c 1 if the "geometryDefinition" of this OrdinalMapping_t structure is
 * set, @c 0 otherwise.
 *
 * @member of OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_isSetGeometryDefinition(const OrdinalMapping_t * om);


/**
 * Predicate returning @c 1 if the given OrdinalMapping_t structure's "ordinal"
 * is set.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return @c 1 if the "ordinal" of this OrdinalMapping_t structure is
 * set, @c 0 otherwise.
 *
 * @member of OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_isSetOrdinal(const OrdinalMapping_t * om);


/**
 * Sets the "geometryDefinition" attribute of the given OrdinalMapping_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs OrdinalMapping_unsetGeometryDefinition() instead.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @param geometryDefinition the string to which the structures "geometryDefinition" attribute should be
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
 * @member of OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_setGeometryDefinition(OrdinalMapping_t * om, const char * geometryDefinition);


/**
 * Sets the "ordinal" attribute of the given OrdinalMapping_t structure.
 *
 * @param om the OrdinalMapping_t structure.
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
 * @member of OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_setOrdinal(OrdinalMapping_t * om, int ordinal);


/**
 * Unsets the value of the "geometryDefinition" attribute of the given 
 *OrdinalMapping_t structure.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_unsetGeometryDefinition(OrdinalMapping_t * om);


/**
 * Unsets the value of the "ordinal" attribute of the given 
 *OrdinalMapping_t structure.
 *
 * @param om the OrdinalMapping_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_unsetOrdinal(OrdinalMapping_t * om);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given OrdinalMapping_t structure have been set.
 *
 * @param om the OrdinalMapping_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of OrdinalMapping_t
 */
LIBSBML_EXTERN
int
OrdinalMapping_hasRequiredAttributes(const OrdinalMapping_t * om);


LIBSBML_EXTERN
OrdinalMapping_t *
ListOfOrdinalMappings_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
OrdinalMapping_t *
ListOfOrdinalMappings_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  OrdinalMapping_H__  */

