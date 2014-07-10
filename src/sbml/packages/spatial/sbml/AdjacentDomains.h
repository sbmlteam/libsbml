/**
 * @file:   AdjacentDomains.h
 * @brief:  Implementation of the AdjacentDomains class
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


#ifndef AdjacentDomains_H__
#define AdjacentDomains_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN AdjacentDomains : public SBase
{

protected:

  std::string   mId;
  std::string   mDomain1;
  std::string   mDomain2;


public:

  /**
   * Creates a new AdjacentDomains with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this AdjacentDomains
   *
   * @param version an unsigned int, the SBML Version to assign to this AdjacentDomains
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this AdjacentDomains
   */
  AdjacentDomains(unsigned int level      = SpatialExtension::getDefaultLevel(),
                  unsigned int version    = SpatialExtension::getDefaultVersion(),
                  unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AdjacentDomains with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  AdjacentDomains(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for AdjacentDomains.
   *
   * @param orig; the AdjacentDomains instance to copy.
   */
  AdjacentDomains(const AdjacentDomains& orig);


   /**
   * Assignment operator for AdjacentDomains.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  AdjacentDomains& operator=(const AdjacentDomains& rhs);


   /**
   * Creates and returns a deep copy of this AdjacentDomains object.
   *
   * @return a (deep) copy of this AdjacentDomains object.
   */
  virtual AdjacentDomains* clone () const;


   /**
   * Destructor for AdjacentDomains.
   */
  virtual ~AdjacentDomains();


   /**
   * Returns the value of the "id" attribute of this AdjacentDomains.
   *
   * @return the value of the "id" attribute of this AdjacentDomains as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "domain1" attribute of this AdjacentDomains.
   *
   * @return the value of the "domain1" attribute of this AdjacentDomains as a string.
   */
  virtual const std::string& getDomain1() const;


  /**
   * Returns the value of the "domain2" attribute of this AdjacentDomains.
   *
   * @return the value of the "domain2" attribute of this AdjacentDomains as a string.
   */
  virtual const std::string& getDomain2() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AdjacentDomains's "id" attribute has been set.
   *
   * @return @c true if this AdjacentDomains's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AdjacentDomains's "domain1" attribute has been set.
   *
   * @return @c true if this AdjacentDomains's "domain1" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDomain1() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AdjacentDomains's "domain2" attribute has been set.
   *
   * @return @c true if this AdjacentDomains's "domain2" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDomain2() const;


  /**
   * Sets the value of the "id" attribute of this AdjacentDomains.
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
   * Sets the value of the "domain1" attribute of this AdjacentDomains.
   *
   * @param domain1; const std::string& value of the "domain1" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDomain1(const std::string& domain1);


  /**
   * Sets the value of the "domain2" attribute of this AdjacentDomains.
   *
   * @param domain2; const std::string& value of the "domain2" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDomain2(const std::string& domain2);


  /**
   * Unsets the value of the "id" attribute of this AdjacentDomains.
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
   * Unsets the value of the "domain1" attribute of this AdjacentDomains.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomain1();


  /**
   * Unsets the value of the "domain2" attribute of this AdjacentDomains.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomain2();


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
   * Returns the XML element name of this object, which for AdjacentDomains, is
   * always @c "adjacentDomains".
   *
   * @return the name of this element, i.e. @c "adjacentDomains".
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
   * for this AdjacentDomains object have been set.
   *
   * @note The required attributes for a AdjacentDomains object are:
   * @li "id"
   * @li "domain1"
   * @li "domain2"
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

class LIBSBML_EXTERN ListOfAdjacentDomains : public ListOf
{

public:

  /**
   * Creates a new ListOfAdjacentDomains with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfAdjacentDomains
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfAdjacentDomains
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfAdjacentDomains
   */
  ListOfAdjacentDomains(unsigned int level      = SpatialExtension::getDefaultLevel(),
                        unsigned int version    = SpatialExtension::getDefaultVersion(),
                        unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfAdjacentDomains with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfAdjacentDomains(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfAdjacentDomains object.
   *
   * @return a (deep) copy of this ListOfAdjacentDomains object.
   */
  virtual ListOfAdjacentDomains* clone () const;


   /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains.
   *
   * @param n the index number of the AdjacentDomains to get.
   *
   * @return the nth AdjacentDomains in this ListOfAdjacentDomains.
   *
   * @see size()
   */
	virtual AdjacentDomains* get(unsigned int n);


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains.
   *
   * @param n the index number of the AdjacentDomains to get.
   *
   * @return the nth AdjacentDomains in this ListOfAdjacentDomains.
   *
   * @see size()
   */
	virtual const AdjacentDomains* get(unsigned int n) const;


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the AdjacentDomains to get.
   *
   * @return AdjacentDomains in this ListOfAdjacentDomains
   * with the given id or NULL if no such
   * AdjacentDomains exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual AdjacentDomains* get(const std::string& sid);


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the AdjacentDomains to get.
   *
   * @return AdjacentDomains in this ListOfAdjacentDomains
   * with the given id or NULL if no such
   * AdjacentDomains exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const AdjacentDomains* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "AdjacentDomains" to this ListOfAdjacentDomains.
	 *
	 * @param ad; the AdjacentDomains object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addAdjacentDomains(const AdjacentDomains* ad);


	/**
	 * Get the number of AdjacentDomains objects in this ListOfAdjacentDomains.
	 *
	 * @return the number of AdjacentDomains objects in this ListOfAdjacentDomains
	 */
	unsigned int getNumAdjacentDomains() const;


	/**
	 * Creates a new AdjacentDomains object, adds it to the
	 * ListOfAdjacentDomains and returns the AdjacentDomains object created. 
	 *
	 * @return a new AdjacentDomains object instance
	 *
	 * @see addAdjacentDomains(const AdjacentDomains* ad)
	 */
	AdjacentDomains* createAdjacentDomains();


  /**
   * Removes the nth AdjacentDomains from this ListOfAdjacentDomains
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the AdjacentDomains to remove.
   *
   * @see size()
   */
	virtual AdjacentDomains* remove(unsigned int n);


  /**
   * Removes the AdjacentDomains from this ListOfAdjacentDomains with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the AdjacentDomains to remove.
   *
   * @return the AdjacentDomains removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual AdjacentDomains* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfAdjacentDomains, is
   * always @c "listOfAdjacentDomains".
   *
   * @return the name of this element, i.e. @c "listOfAdjacentDomains".
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
   * Creates a new AdjacentDomains in this ListOfAdjacentDomains
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
 * Creates a new AdjacentDomains_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * AdjacentDomains_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * AdjacentDomains_t structure.
 *
 * @returns the newly-created AdjacentDomains_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
AdjacentDomains_t *
AdjacentDomains_create(unsigned int level, unsigned int version,
                       unsigned int pkgVersion);


/**
 * Frees the given AdjacentDomains_t structure.
 * 
 * @param ad the AdjacentDomains_t structure to be freed.
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
void
AdjacentDomains_free(AdjacentDomains_t * ad);


/**
 * Creates a deep copy of the given AdjacentDomains_t structure.
 * 
 * @param ad the AdjacentDomains_t structure to be copied.
 *
 * @returns a (deep) copy of the given AdjacentDomains_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
AdjacentDomains_t *
AdjacentDomains_clone(AdjacentDomains_t * ad);


/**
 * Returns the value of the "id" attribute of the given AdjacentDomains_t
 * structure.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return the id of this structure.
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
const char *
AdjacentDomains_getId(const AdjacentDomains_t * ad);


/**
 * Returns the value of the "domain1" attribute of the given AdjacentDomains_t
 * structure.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return the domain1 of this structure.
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
const char *
AdjacentDomains_getDomain1(const AdjacentDomains_t * ad);


/**
 * Returns the value of the "domain2" attribute of the given AdjacentDomains_t
 * structure.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return the domain2 of this structure.
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
const char *
AdjacentDomains_getDomain2(const AdjacentDomains_t * ad);


/**
 * Predicate returning @c 1 if the given AdjacentDomains_t structure's "id"
 * is set.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return @c 1 if the "id" of this AdjacentDomains_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetId(const AdjacentDomains_t * ad);


/**
 * Predicate returning @c 1 if the given AdjacentDomains_t structure's "domain1"
 * is set.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return @c 1 if the "domain1" of this AdjacentDomains_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain1(const AdjacentDomains_t * ad);


/**
 * Predicate returning @c 1 if the given AdjacentDomains_t structure's "domain2"
 * is set.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return @c 1 if the "domain2" of this AdjacentDomains_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain2(const AdjacentDomains_t * ad);


/**
 * Sets the "id" attribute of the given AdjacentDomains_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs AdjacentDomains_unsetId() instead.
 *
 * @param ad the AdjacentDomains_t structure.
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
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_setId(AdjacentDomains_t * ad, const char * id);


/**
 * Sets the "domain1" attribute of the given AdjacentDomains_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs AdjacentDomains_unsetDomain1() instead.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @param domain1 the string to which the structures "domain1" attribute should be
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
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_setDomain1(AdjacentDomains_t * ad, const char * domain1);


/**
 * Sets the "domain2" attribute of the given AdjacentDomains_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs AdjacentDomains_unsetDomain2() instead.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @param domain2 the string to which the structures "domain2" attribute should be
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
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_setDomain2(AdjacentDomains_t * ad, const char * domain2);


/**
 * Unsets the value of the "id" attribute of the given 
 *AdjacentDomains_t structure.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetId(AdjacentDomains_t * ad);


/**
 * Unsets the value of the "domain1" attribute of the given 
 *AdjacentDomains_t structure.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain1(AdjacentDomains_t * ad);


/**
 * Unsets the value of the "domain2" attribute of the given 
 *AdjacentDomains_t structure.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain2(AdjacentDomains_t * ad);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given AdjacentDomains_t structure have been set.
 *
 * @param ad the AdjacentDomains_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_hasRequiredAttributes(const AdjacentDomains_t * ad);


LIBSBML_EXTERN
AdjacentDomains_t *
ListOfAdjacentDomains_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
AdjacentDomains_t *
ListOfAdjacentDomains_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  AdjacentDomains_H__  */

