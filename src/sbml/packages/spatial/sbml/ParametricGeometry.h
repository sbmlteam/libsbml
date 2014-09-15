/**
 * @file:   ParametricGeometry.h
 * @brief:  Implementation of the ParametricGeometry class
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


#ifndef ParametricGeometry_H__
#define ParametricGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>

#include <sbml/packages/spatial/sbml/SpatialPoint.h>
#include <sbml/packages/spatial/sbml/ParametricObject.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN ParametricGeometry : public GeometryDefinition
{

protected:

  ListOfSpatialPoints   mSpatialPoints;
  ListOfParametricObjects   mParametricObjects;


public:

  /**
   * Creates a new ParametricGeometry with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ParametricGeometry
   *
   * @param version an unsigned int, the SBML Version to assign to this ParametricGeometry
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ParametricGeometry
   */
  ParametricGeometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
                     unsigned int version    = SpatialExtension::getDefaultVersion(),
                     unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ParametricGeometry with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ParametricGeometry(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for ParametricGeometry.
   *
   * @param orig; the ParametricGeometry instance to copy.
   */
  ParametricGeometry(const ParametricGeometry& orig);


   /**
   * Assignment operator for ParametricGeometry.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  ParametricGeometry& operator=(const ParametricGeometry& rhs);


   /**
   * Creates and returns a deep copy of this ParametricGeometry object.
   *
   * @return a (deep) copy of this ParametricGeometry object.
   */
  virtual ParametricGeometry* clone () const;


   /**
   * Destructor for ParametricGeometry.
   */
  virtual ~ParametricGeometry();


   /**
   * Returns the  "ListOfSpatialPoints" in this ParametricGeometry object.
   *
   * @return the "ListOfSpatialPoints" attribute of this ParametricGeometry.
   */
  const ListOfSpatialPoints* getListOfSpatialPoints() const;


  /**
   * Returns the  "ListOfSpatialPoints" in this ParametricGeometry object.
   *
   * @return the "ListOfSpatialPoints" attribute of this ParametricGeometry.
   */
  ListOfSpatialPoints* getListOfSpatialPoints();


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints.
   *
   * @param n the index number of the SpatialPoint to get.
   *
   * @return the nth SpatialPoint in the ListOfSpatialPoints within this ParametricGeometry.
   *
   * @see getNumSpatialPoints()
   */
	SpatialPoint* getSpatialPoint(unsigned int n);


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints.
   *
   * @param n the index number of the SpatialPoint to get.
   *
   * @return the nth SpatialPoint in the ListOfSpatialPoints within this ParametricGeometry.
   *
   * @see getNumSpatialPoints()
   */
	const SpatialPoint* getSpatialPoint(unsigned int n) const;


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpatialPoint to get.
   *
   * @return the SpatialPoint in the ListOfSpatialPoints
   * with the given id or NULL if no such
   * SpatialPoint exists.
   *
   * @see getSpatialPoint(unsigned int n)
   *
   * @see getNumSpatialPoints()
   */
	SpatialPoint* getSpatialPoint(const std::string& sid);


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpatialPoint to get.
   *
   * @return the SpatialPoint in the ListOfSpatialPoints
   * with the given id or NULL if no such
   * SpatialPoint exists.
   *
   * @see getSpatialPoint(unsigned int n)
   *
   * @see getNumSpatialPoints()
   */
	const SpatialPoint* getSpatialPoint(const std::string& sid) const;


  /**
   * Adds a copy the given "SpatialPoint" to this ParametricGeometry.
   *
   * @param sp; the SpatialPoint object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addSpatialPoint(const SpatialPoint* sp);


  /**
   * Get the number of SpatialPoint objects in this ParametricGeometry.
   *
   * @return the number of SpatialPoint objects in this ParametricGeometry
   */
  unsigned int getNumSpatialPoints() const;


  /**
   * Creates a new SpatialPoint object, adds it to this ParametricGeometrys
   * ListOfSpatialPoints and returns the SpatialPoint object created. 
   *
   * @return a new SpatialPoint object instance
   *
   * @see addSpatialPoint(const SpatialPoint* sp)
   */
  SpatialPoint* createSpatialPoint();


  /**
   * Removes the nth SpatialPoint from the ListOfSpatialPoints within this ParametricGeometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SpatialPoint to remove.
   *
   * @see getNumSpatialPoints()
   */
	SpatialPoint* removeSpatialPoint(unsigned int n);


  /**
   * Removes the SpatialPoint with the given identifier from the ListOfSpatialPoints within this ParametricGeometry
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
	SpatialPoint* removeSpatialPoint(const std::string& sid);


  /**
   * Returns the  "ListOfParametricObjects" in this ParametricGeometry object.
   *
   * @return the "ListOfParametricObjects" attribute of this ParametricGeometry.
   */
  const ListOfParametricObjects* getListOfParametricObjects() const;


  /**
   * Returns the  "ListOfParametricObjects" in this ParametricGeometry object.
   *
   * @return the "ListOfParametricObjects" attribute of this ParametricGeometry.
   */
  ListOfParametricObjects* getListOfParametricObjects();


  /**
   * Get a ParametricObject from the ListOfParametricObjects.
   *
   * @param n the index number of the ParametricObject to get.
   *
   * @return the nth ParametricObject in the ListOfParametricObjects within this ParametricGeometry.
   *
   * @see getNumParametricObjects()
   */
	ParametricObject* getParametricObject(unsigned int n);


  /**
   * Get a ParametricObject from the ListOfParametricObjects.
   *
   * @param n the index number of the ParametricObject to get.
   *
   * @return the nth ParametricObject in the ListOfParametricObjects within this ParametricGeometry.
   *
   * @see getNumParametricObjects()
   */
	const ParametricObject* getParametricObject(unsigned int n) const;


  /**
   * Get a ParametricObject from the ListOfParametricObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the ParametricObject to get.
   *
   * @return the ParametricObject in the ListOfParametricObjects
   * with the given id or NULL if no such
   * ParametricObject exists.
   *
   * @see getParametricObject(unsigned int n)
   *
   * @see getNumParametricObjects()
   */
	ParametricObject* getParametricObject(const std::string& sid);


  /**
   * Get a ParametricObject from the ListOfParametricObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the ParametricObject to get.
   *
   * @return the ParametricObject in the ListOfParametricObjects
   * with the given id or NULL if no such
   * ParametricObject exists.
   *
   * @see getParametricObject(unsigned int n)
   *
   * @see getNumParametricObjects()
   */
	const ParametricObject* getParametricObject(const std::string& sid) const;


  /**
   * Adds a copy the given "ParametricObject" to this ParametricGeometry.
   *
   * @param po; the ParametricObject object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addParametricObject(const ParametricObject* po);


  /**
   * Get the number of ParametricObject objects in this ParametricGeometry.
   *
   * @return the number of ParametricObject objects in this ParametricGeometry
   */
  unsigned int getNumParametricObjects() const;


  /**
   * Creates a new ParametricObject object, adds it to this ParametricGeometrys
   * ListOfParametricObjects and returns the ParametricObject object created. 
   *
   * @return a new ParametricObject object instance
   *
   * @see addParametricObject(const ParametricObject* po)
   */
  ParametricObject* createParametricObject();


  /**
   * Removes the nth ParametricObject from the ListOfParametricObjects within this ParametricGeometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the ParametricObject to remove.
   *
   * @see getNumParametricObjects()
   */
	ParametricObject* removeParametricObject(unsigned int n);


  /**
   * Removes the ParametricObject with the given identifier from the ListOfParametricObjects within this ParametricGeometry
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the ParametricObject to remove.
   *
   * @return the ParametricObject removed. As mentioned above, the caller owns the
   * returned item.
   */
	ParametricObject* removeParametricObject(const std::string& sid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for ParametricGeometry, is
   * always @c "parametricGeometry".
   *
   * @return the name of this element, i.e. @c "parametricGeometry".
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
   * for this ParametricGeometry object have been set.
   *
   * @note The required attributes for a ParametricGeometry object are:
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this ParametricGeometry object have been set.
   *
   * @note The required elements for a ParametricGeometry object are:
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
 * Creates a new ParametricGeometry_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * ParametricGeometry_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * ParametricGeometry_t structure.
 *
 * @returns the newly-created ParametricGeometry_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricGeometry_t *
ParametricGeometry_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion);


/**
 * Frees the given ParametricGeometry_t structure.
 * 
 * @param pg the ParametricGeometry_t structure to be freed.
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
void
ParametricGeometry_free(ParametricGeometry_t * pg);


/**
 * Creates a deep copy of the given ParametricGeometry_t structure.
 * 
 * @param pg the ParametricGeometry_t structure to be copied.
 *
 * @returns a (deep) copy of the given ParametricGeometry_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricGeometry_t *
ParametricGeometry_clone(ParametricGeometry_t * pg);


LIBSBML_EXTERN
int
ParametricGeometry_addSpatialPoint(ParametricGeometry_t * pg, SpatialPoint_t * sp);


LIBSBML_EXTERN
SpatialPoint_t *
ParametricGeometry_createSpatialPoint(ParametricGeometry_t * pg);


LIBSBML_EXTERN
ListOf_t *
ParametricGeometry_getListOfSpatialPoints(ParametricGeometry_t * pg) ;


LIBSBML_EXTERN
SpatialPoint_t *
ParametricGeometry_getSpatialPoint(ParametricGeometry_t * pg, unsigned int n);


LIBSBML_EXTERN
SpatialPoint_t *
ParametricGeometry_getSpatialPointById(ParametricGeometry_t * pg, const char * sid);


LIBSBML_EXTERN
unsigned int
ParametricGeometry_getNumSpatialPoints(ParametricGeometry_t * pg);


LIBSBML_EXTERN
SpatialPoint_t *
ParametricGeometry_removeSpatialPoint(ParametricGeometry_t * pg, unsigned int n);


LIBSBML_EXTERN
SpatialPoint_t *
ParametricGeometry_removeSpatialPointById(ParametricGeometry_t * pg, const char * sid);


LIBSBML_EXTERN
int
ParametricGeometry_addParametricObject(ParametricGeometry_t * pg, ParametricObject_t * po);


LIBSBML_EXTERN
ParametricObject_t *
ParametricGeometry_createParametricObject(ParametricGeometry_t * pg);


LIBSBML_EXTERN
ListOf_t *
ParametricGeometry_getListOfParametricObjects(ParametricGeometry_t * pg) ;


LIBSBML_EXTERN
ParametricObject_t *
ParametricGeometry_getParametricObject(ParametricGeometry_t * pg, unsigned int n);


LIBSBML_EXTERN
ParametricObject_t *
ParametricGeometry_getParametricObjectById(ParametricGeometry_t * pg, const char * sid);


LIBSBML_EXTERN
unsigned int
ParametricGeometry_getNumParametricObjects(ParametricGeometry_t * pg);


LIBSBML_EXTERN
ParametricObject_t *
ParametricGeometry_removeParametricObject(ParametricGeometry_t * pg, unsigned int n);


LIBSBML_EXTERN
ParametricObject_t *
ParametricGeometry_removeParametricObjectById(ParametricGeometry_t * pg, const char * sid);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given ParametricGeometry_t structure have been set.
 *
 * @param pg the ParametricGeometry_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of ParametricGeometry_t
 */
LIBSBML_EXTERN
int
ParametricGeometry_hasRequiredAttributes(const ParametricGeometry_t * pg);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given ParametricGeometry_t structure have been set.
 *
 * @param pg the ParametricGeometry_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of ParametricGeometry_t
 */
LIBSBML_EXTERN
int
ParametricGeometry_hasRequiredElements(const ParametricGeometry_t * pg);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  ParametricGeometry_H__  */

