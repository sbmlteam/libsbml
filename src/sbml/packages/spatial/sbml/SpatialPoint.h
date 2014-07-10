/**
 * @file:   SpatialPoint.h
 * @brief:  Implementation of the SpatialPoint class
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


#ifndef SpatialPoint_H__
#define SpatialPoint_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN SpatialPoint : public SBase
{

protected:

  std::string   mId;
  double        mCoord1;
  bool          mIsSetCoord1;
  double        mCoord2;
  bool          mIsSetCoord2;
  double        mCoord3;
  bool          mIsSetCoord3;
  std::string   mDomain;


public:

  /**
   * Creates a new SpatialPoint with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SpatialPoint
   *
   * @param version an unsigned int, the SBML Version to assign to this SpatialPoint
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this SpatialPoint
   */
  SpatialPoint(unsigned int level      = SpatialExtension::getDefaultLevel(),
               unsigned int version    = SpatialExtension::getDefaultVersion(),
               unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpatialPoint with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  SpatialPoint(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for SpatialPoint.
   *
   * @param orig; the SpatialPoint instance to copy.
   */
  SpatialPoint(const SpatialPoint& orig);


   /**
   * Assignment operator for SpatialPoint.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SpatialPoint& operator=(const SpatialPoint& rhs);


   /**
   * Creates and returns a deep copy of this SpatialPoint object.
   *
   * @return a (deep) copy of this SpatialPoint object.
   */
  virtual SpatialPoint* clone () const;


   /**
   * Destructor for SpatialPoint.
   */
  virtual ~SpatialPoint();


   /**
   * Returns the value of the "id" attribute of this SpatialPoint.
   *
   * @return the value of the "id" attribute of this SpatialPoint as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "coord1" attribute of this SpatialPoint.
   *
   * @return the value of the "coord1" attribute of this SpatialPoint as a double.
   */
  virtual double getCoord1() const;


  /**
   * Returns the value of the "coord2" attribute of this SpatialPoint.
   *
   * @return the value of the "coord2" attribute of this SpatialPoint as a double.
   */
  virtual double getCoord2() const;


  /**
   * Returns the value of the "coord3" attribute of this SpatialPoint.
   *
   * @return the value of the "coord3" attribute of this SpatialPoint as a double.
   */
  virtual double getCoord3() const;


  /**
   * Returns the value of the "domain" attribute of this SpatialPoint.
   *
   * @return the value of the "domain" attribute of this SpatialPoint as a string.
   */
  virtual const std::string& getDomain() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "id" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "coord1" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "coord1" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord1() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "coord2" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "coord2" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord2() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "coord3" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "coord3" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord3() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "domain" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "domain" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDomain() const;


  /**
   * Sets the value of the "id" attribute of this SpatialPoint.
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
   * Sets the value of the "coord1" attribute of this SpatialPoint.
   *
   * @param coord1; double value of the "coord1" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoord1(double coord1);


  /**
   * Sets the value of the "coord2" attribute of this SpatialPoint.
   *
   * @param coord2; double value of the "coord2" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoord2(double coord2);


  /**
   * Sets the value of the "coord3" attribute of this SpatialPoint.
   *
   * @param coord3; double value of the "coord3" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoord3(double coord3);


  /**
   * Sets the value of the "domain" attribute of this SpatialPoint.
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
   * Unsets the value of the "id" attribute of this SpatialPoint.
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
   * Unsets the value of the "coord1" attribute of this SpatialPoint.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoord1();


  /**
   * Unsets the value of the "coord2" attribute of this SpatialPoint.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoord2();


  /**
   * Unsets the value of the "coord3" attribute of this SpatialPoint.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoord3();


  /**
   * Unsets the value of the "domain" attribute of this SpatialPoint.
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
   * Returns the XML element name of this object, which for SpatialPoint, is
   * always @c "spatialPoint".
   *
   * @return the name of this element, i.e. @c "spatialPoint".
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
   * for this SpatialPoint object have been set.
   *
   * @note The required attributes for a SpatialPoint object are:
   * @li "id"
   * @li "coord1"
   * @li "domain"
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

class LIBSBML_EXTERN ListOfSpatialPoints : public ListOf
{

public:

  /**
   * Creates a new ListOfSpatialPoints with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfSpatialPoints
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfSpatialPoints
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfSpatialPoints
   */
  ListOfSpatialPoints(unsigned int level      = SpatialExtension::getDefaultLevel(),
                      unsigned int version    = SpatialExtension::getDefaultVersion(),
                      unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpatialPoints with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfSpatialPoints(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfSpatialPoints object.
   *
   * @return a (deep) copy of this ListOfSpatialPoints object.
   */
  virtual ListOfSpatialPoints* clone () const;


   /**
   * Get a SpatialPoint from the ListOfSpatialPoints.
   *
   * @param n the index number of the SpatialPoint to get.
   *
   * @return the nth SpatialPoint in this ListOfSpatialPoints.
   *
   * @see size()
   */
	virtual SpatialPoint* get(unsigned int n);


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints.
   *
   * @param n the index number of the SpatialPoint to get.
   *
   * @return the nth SpatialPoint in this ListOfSpatialPoints.
   *
   * @see size()
   */
	virtual const SpatialPoint* get(unsigned int n) const;


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpatialPoint to get.
   *
   * @return SpatialPoint in this ListOfSpatialPoints
   * with the given id or NULL if no such
   * SpatialPoint exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual SpatialPoint* get(const std::string& sid);


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpatialPoint to get.
   *
   * @return SpatialPoint in this ListOfSpatialPoints
   * with the given id or NULL if no such
   * SpatialPoint exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SpatialPoint* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "SpatialPoint" to this ListOfSpatialPoints.
	 *
	 * @param sp; the SpatialPoint object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addSpatialPoint(const SpatialPoint* sp);


	/**
	 * Get the number of SpatialPoint objects in this ListOfSpatialPoints.
	 *
	 * @return the number of SpatialPoint objects in this ListOfSpatialPoints
	 */
	unsigned int getNumSpatialPoints() const;


	/**
	 * Creates a new SpatialPoint object, adds it to the
	 * ListOfSpatialPoints and returns the SpatialPoint object created. 
	 *
	 * @return a new SpatialPoint object instance
	 *
	 * @see addSpatialPoint(const SpatialPoint* sp)
	 */
	SpatialPoint* createSpatialPoint();


  /**
   * Removes the nth SpatialPoint from this ListOfSpatialPoints
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SpatialPoint to remove.
   *
   * @see size()
   */
	virtual SpatialPoint* remove(unsigned int n);


  /**
   * Removes the SpatialPoint from this ListOfSpatialPoints with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SpatialPoint to remove.
   *
   * @return the SpatialPoint removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual SpatialPoint* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfSpatialPoints, is
   * always @c "listOfSpatialPoints".
   *
   * @return the name of this element, i.e. @c "listOfSpatialPoints".
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
   * Creates a new SpatialPoint in this ListOfSpatialPoints
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
 * Creates a new SpatialPoint_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * SpatialPoint_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * SpatialPoint_t structure.
 *
 * @returns the newly-created SpatialPoint_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof SpatialPoint_t
 */
LIBSBML_EXTERN
SpatialPoint_t *
SpatialPoint_create(unsigned int level, unsigned int version,
                    unsigned int pkgVersion);


/**
 * Frees the given SpatialPoint_t structure.
 * 
 * @param sp the SpatialPoint_t structure to be freed.
 *
 * @memberof SpatialPoint_t
 */
LIBSBML_EXTERN
void
SpatialPoint_free(SpatialPoint_t * sp);


/**
 * Creates a deep copy of the given SpatialPoint_t structure.
 * 
 * @param sp the SpatialPoint_t structure to be copied.
 *
 * @returns a (deep) copy of the given SpatialPoint_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof SpatialPoint_t
 */
LIBSBML_EXTERN
SpatialPoint_t *
SpatialPoint_clone(SpatialPoint_t * sp);


/**
 * Returns the value of the "id" attribute of the given SpatialPoint_t
 * structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return the id of this structure.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
const char *
SpatialPoint_getId(const SpatialPoint_t * sp);


/**
 * Returns the value of the "coord1" attribute of the given SpatialPoint_t
 * structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return the coord1 of this structure.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
double
SpatialPoint_getCoord1(const SpatialPoint_t * sp);


/**
 * Returns the value of the "coord2" attribute of the given SpatialPoint_t
 * structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return the coord2 of this structure.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
double
SpatialPoint_getCoord2(const SpatialPoint_t * sp);


/**
 * Returns the value of the "coord3" attribute of the given SpatialPoint_t
 * structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return the coord3 of this structure.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
double
SpatialPoint_getCoord3(const SpatialPoint_t * sp);


/**
 * Returns the value of the "domain" attribute of the given SpatialPoint_t
 * structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return the domain of this structure.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
const char *
SpatialPoint_getDomain(const SpatialPoint_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoint_t structure's "id"
 * is set.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return @c 1 if the "id" of this SpatialPoint_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_isSetId(const SpatialPoint_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoint_t structure's "coord1"
 * is set.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return @c 1 if the "coord1" of this SpatialPoint_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_isSetCoord1(const SpatialPoint_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoint_t structure's "coord2"
 * is set.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return @c 1 if the "coord2" of this SpatialPoint_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_isSetCoord2(const SpatialPoint_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoint_t structure's "coord3"
 * is set.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return @c 1 if the "coord3" of this SpatialPoint_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_isSetCoord3(const SpatialPoint_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoint_t structure's "domain"
 * is set.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return @c 1 if the "domain" of this SpatialPoint_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_isSetDomain(const SpatialPoint_t * sp);


/**
 * Sets the "id" attribute of the given SpatialPoint_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SpatialPoint_unsetId() instead.
 *
 * @param sp the SpatialPoint_t structure.
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
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_setId(SpatialPoint_t * sp, const char * id);


/**
 * Sets the "coord1" attribute of the given SpatialPoint_t structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @param coord1 the string to which the structures "coord1" attribute should be
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
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_setCoord1(SpatialPoint_t * sp, double coord1);


/**
 * Sets the "coord2" attribute of the given SpatialPoint_t structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @param coord2 the string to which the structures "coord2" attribute should be
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
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_setCoord2(SpatialPoint_t * sp, double coord2);


/**
 * Sets the "coord3" attribute of the given SpatialPoint_t structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @param coord3 the string to which the structures "coord3" attribute should be
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
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_setCoord3(SpatialPoint_t * sp, double coord3);


/**
 * Sets the "domain" attribute of the given SpatialPoint_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SpatialPoint_unsetDomain() instead.
 *
 * @param sp the SpatialPoint_t structure.
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
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_setDomain(SpatialPoint_t * sp, const char * domain);


/**
 * Unsets the value of the "id" attribute of the given 
 *SpatialPoint_t structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_unsetId(SpatialPoint_t * sp);


/**
 * Unsets the value of the "coord1" attribute of the given 
 *SpatialPoint_t structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_unsetCoord1(SpatialPoint_t * sp);


/**
 * Unsets the value of the "coord2" attribute of the given 
 *SpatialPoint_t structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_unsetCoord2(SpatialPoint_t * sp);


/**
 * Unsets the value of the "coord3" attribute of the given 
 *SpatialPoint_t structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_unsetCoord3(SpatialPoint_t * sp);


/**
 * Unsets the value of the "domain" attribute of the given 
 *SpatialPoint_t structure.
 *
 * @param sp the SpatialPoint_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_unsetDomain(SpatialPoint_t * sp);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given SpatialPoint_t structure have been set.
 *
 * @param sp the SpatialPoint_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of SpatialPoint_t
 */
LIBSBML_EXTERN
int
SpatialPoint_hasRequiredAttributes(const SpatialPoint_t * sp);


LIBSBML_EXTERN
SpatialPoint_t *
ListOfSpatialPoints_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
SpatialPoint_t *
ListOfSpatialPoints_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpatialPoint_H__  */

