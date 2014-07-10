/**
 * @file:   InteriorPoint.h
 * @brief:  Implementation of the InteriorPoint class
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


#ifndef InteriorPoint_H__
#define InteriorPoint_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN InteriorPoint : public SBase
{

protected:

  double        mCoord1;
  bool          mIsSetCoord1;
  double        mCoord2;
  bool          mIsSetCoord2;
  double        mCoord3;
  bool          mIsSetCoord3;


public:

  /**
   * Creates a new InteriorPoint with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this InteriorPoint
   *
   * @param version an unsigned int, the SBML Version to assign to this InteriorPoint
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this InteriorPoint
   */
  InteriorPoint(unsigned int level      = SpatialExtension::getDefaultLevel(),
                unsigned int version    = SpatialExtension::getDefaultVersion(),
                unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new InteriorPoint with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  InteriorPoint(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for InteriorPoint.
   *
   * @param orig; the InteriorPoint instance to copy.
   */
  InteriorPoint(const InteriorPoint& orig);


   /**
   * Assignment operator for InteriorPoint.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  InteriorPoint& operator=(const InteriorPoint& rhs);


   /**
   * Creates and returns a deep copy of this InteriorPoint object.
   *
   * @return a (deep) copy of this InteriorPoint object.
   */
  virtual InteriorPoint* clone () const;


   /**
   * Destructor for InteriorPoint.
   */
  virtual ~InteriorPoint();


   /**
   * Returns the value of the "coord1" attribute of this InteriorPoint.
   *
   * @return the value of the "coord1" attribute of this InteriorPoint as a double.
   */
  virtual double getCoord1() const;


  /**
   * Returns the value of the "coord2" attribute of this InteriorPoint.
   *
   * @return the value of the "coord2" attribute of this InteriorPoint as a double.
   */
  virtual double getCoord2() const;


  /**
   * Returns the value of the "coord3" attribute of this InteriorPoint.
   *
   * @return the value of the "coord3" attribute of this InteriorPoint as a double.
   */
  virtual double getCoord3() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * InteriorPoint's "coord1" attribute has been set.
   *
   * @return @c true if this InteriorPoint's "coord1" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord1() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * InteriorPoint's "coord2" attribute has been set.
   *
   * @return @c true if this InteriorPoint's "coord2" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord2() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * InteriorPoint's "coord3" attribute has been set.
   *
   * @return @c true if this InteriorPoint's "coord3" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord3() const;


  /**
   * Sets the value of the "coord1" attribute of this InteriorPoint.
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
   * Sets the value of the "coord2" attribute of this InteriorPoint.
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
   * Sets the value of the "coord3" attribute of this InteriorPoint.
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
   * Unsets the value of the "coord1" attribute of this InteriorPoint.
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
   * Unsets the value of the "coord2" attribute of this InteriorPoint.
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
   * Unsets the value of the "coord3" attribute of this InteriorPoint.
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
   * Returns the XML element name of this object, which for InteriorPoint, is
   * always @c "interiorPoint".
   *
   * @return the name of this element, i.e. @c "interiorPoint".
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
   * for this InteriorPoint object have been set.
   *
   * @note The required attributes for a InteriorPoint object are:
   * @li "coord1"
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

class LIBSBML_EXTERN ListOfInteriorPoints : public ListOf
{

public:

  /**
   * Creates a new ListOfInteriorPoints with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfInteriorPoints
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfInteriorPoints
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfInteriorPoints
   */
  ListOfInteriorPoints(unsigned int level      = SpatialExtension::getDefaultLevel(),
                       unsigned int version    = SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfInteriorPoints with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfInteriorPoints(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfInteriorPoints object.
   *
   * @return a (deep) copy of this ListOfInteriorPoints object.
   */
  virtual ListOfInteriorPoints* clone () const;


   /**
   * Get a InteriorPoint from the ListOfInteriorPoints.
   *
   * @param n the index number of the InteriorPoint to get.
   *
   * @return the nth InteriorPoint in this ListOfInteriorPoints.
   *
   * @see size()
   */
	virtual InteriorPoint* get(unsigned int n);


  /**
   * Get a InteriorPoint from the ListOfInteriorPoints.
   *
   * @param n the index number of the InteriorPoint to get.
   *
   * @return the nth InteriorPoint in this ListOfInteriorPoints.
   *
   * @see size()
   */
	virtual const InteriorPoint* get(unsigned int n) const;


  /**
   * Get a InteriorPoint from the ListOfInteriorPoints
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the InteriorPoint to get.
   *
   * @return InteriorPoint in this ListOfInteriorPoints
   * with the given id or NULL if no such
   * InteriorPoint exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual InteriorPoint* get(const std::string& sid);


  /**
   * Get a InteriorPoint from the ListOfInteriorPoints
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the InteriorPoint to get.
   *
   * @return InteriorPoint in this ListOfInteriorPoints
   * with the given id or NULL if no such
   * InteriorPoint exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const InteriorPoint* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "InteriorPoint" to this ListOfInteriorPoints.
	 *
	 * @param ip; the InteriorPoint object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addInteriorPoint(const InteriorPoint* ip);


	/**
	 * Get the number of InteriorPoint objects in this ListOfInteriorPoints.
	 *
	 * @return the number of InteriorPoint objects in this ListOfInteriorPoints
	 */
	unsigned int getNumInteriorPoints() const;


	/**
	 * Creates a new InteriorPoint object, adds it to the
	 * ListOfInteriorPoints and returns the InteriorPoint object created. 
	 *
	 * @return a new InteriorPoint object instance
	 *
	 * @see addInteriorPoint(const InteriorPoint* ip)
	 */
	InteriorPoint* createInteriorPoint();


  /**
   * Removes the nth InteriorPoint from this ListOfInteriorPoints
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the InteriorPoint to remove.
   *
   * @see size()
   */
	virtual InteriorPoint* remove(unsigned int n);


  /**
   * Removes the InteriorPoint from this ListOfInteriorPoints with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the InteriorPoint to remove.
   *
   * @return the InteriorPoint removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual InteriorPoint* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfInteriorPoints, is
   * always @c "listOfInteriorPoints".
   *
   * @return the name of this element, i.e. @c "listOfInteriorPoints".
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
   * Creates a new InteriorPoint in this ListOfInteriorPoints
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
 * Creates a new InteriorPoint_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * InteriorPoint_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * InteriorPoint_t structure.
 *
 * @returns the newly-created InteriorPoint_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
InteriorPoint_t *
InteriorPoint_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion);


/**
 * Frees the given InteriorPoint_t structure.
 * 
 * @param ip the InteriorPoint_t structure to be freed.
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
void
InteriorPoint_free(InteriorPoint_t * ip);


/**
 * Creates a deep copy of the given InteriorPoint_t structure.
 * 
 * @param ip the InteriorPoint_t structure to be copied.
 *
 * @returns a (deep) copy of the given InteriorPoint_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
InteriorPoint_t *
InteriorPoint_clone(InteriorPoint_t * ip);


/**
 * Returns the value of the "coord1" attribute of the given InteriorPoint_t
 * structure.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return the coord1 of this structure.
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
double
InteriorPoint_getCoord1(const InteriorPoint_t * ip);


/**
 * Returns the value of the "coord2" attribute of the given InteriorPoint_t
 * structure.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return the coord2 of this structure.
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
double
InteriorPoint_getCoord2(const InteriorPoint_t * ip);


/**
 * Returns the value of the "coord3" attribute of the given InteriorPoint_t
 * structure.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return the coord3 of this structure.
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
double
InteriorPoint_getCoord3(const InteriorPoint_t * ip);


/**
 * Predicate returning @c 1 if the given InteriorPoint_t structure's "coord1"
 * is set.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return @c 1 if the "coord1" of this InteriorPoint_t structure is
 * set, @c 0 otherwise.
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_isSetCoord1(const InteriorPoint_t * ip);


/**
 * Predicate returning @c 1 if the given InteriorPoint_t structure's "coord2"
 * is set.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return @c 1 if the "coord2" of this InteriorPoint_t structure is
 * set, @c 0 otherwise.
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_isSetCoord2(const InteriorPoint_t * ip);


/**
 * Predicate returning @c 1 if the given InteriorPoint_t structure's "coord3"
 * is set.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return @c 1 if the "coord3" of this InteriorPoint_t structure is
 * set, @c 0 otherwise.
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_isSetCoord3(const InteriorPoint_t * ip);


/**
 * Sets the "coord1" attribute of the given InteriorPoint_t structure.
 *
 * @param ip the InteriorPoint_t structure.
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
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_setCoord1(InteriorPoint_t * ip, double coord1);


/**
 * Sets the "coord2" attribute of the given InteriorPoint_t structure.
 *
 * @param ip the InteriorPoint_t structure.
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
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_setCoord2(InteriorPoint_t * ip, double coord2);


/**
 * Sets the "coord3" attribute of the given InteriorPoint_t structure.
 *
 * @param ip the InteriorPoint_t structure.
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
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_setCoord3(InteriorPoint_t * ip, double coord3);


/**
 * Unsets the value of the "coord1" attribute of the given 
 *InteriorPoint_t structure.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_unsetCoord1(InteriorPoint_t * ip);


/**
 * Unsets the value of the "coord2" attribute of the given 
 *InteriorPoint_t structure.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_unsetCoord2(InteriorPoint_t * ip);


/**
 * Unsets the value of the "coord3" attribute of the given 
 *InteriorPoint_t structure.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_unsetCoord3(InteriorPoint_t * ip);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given InteriorPoint_t structure have been set.
 *
 * @param ip the InteriorPoint_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_hasRequiredAttributes(const InteriorPoint_t * ip);


LIBSBML_EXTERN
InteriorPoint_t *
ListOfInteriorPoints_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
InteriorPoint_t *
ListOfInteriorPoints_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  InteriorPoint_H__  */

