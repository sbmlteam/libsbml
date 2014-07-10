/**
 * @file:   CSGeometry.h
 * @brief:  Implementation of the CSGeometry class
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


#ifndef CSGeometry_H__
#define CSGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>

#include <sbml/packages/spatial/sbml/CSGObject.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN CSGeometry : public GeometryDefinition
{

protected:

  ListOfCSGObjects   mCsgObjects;


public:

  /**
   * Creates a new CSGeometry with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGeometry
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGeometry
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGeometry
   */
  CSGeometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
             unsigned int version    = SpatialExtension::getDefaultVersion(),
             unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGeometry with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGeometry(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGeometry.
   *
   * @param orig; the CSGeometry instance to copy.
   */
  CSGeometry(const CSGeometry& orig);


   /**
   * Assignment operator for CSGeometry.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGeometry& operator=(const CSGeometry& rhs);


   /**
   * Creates and returns a deep copy of this CSGeometry object.
   *
   * @return a (deep) copy of this CSGeometry object.
   */
  virtual CSGeometry* clone () const;


   /**
   * Destructor for CSGeometry.
   */
  virtual ~CSGeometry();


   /**
   * Returns the  "ListOfCSGObjects" in this CSGeometry object.
   *
   * @return the "ListOfCSGObjects" attribute of this CSGeometry.
   */
  const ListOfCSGObjects* getListOfCsgObjects() const;


  /**
   * Returns the  "ListOfCSGObjects" in this CSGeometry object.
   *
   * @return the "ListOfCSGObjects" attribute of this CSGeometry.
   */
  ListOfCSGObjects* getListOfCsgObjects();


  /**
   * Get a CsgObject from the ListOfCSGObjects.
   *
   * @param n the index number of the CsgObject to get.
   *
   * @return the nth CsgObject in the ListOfCSGObjects within this CSGeometry.
   *
   * @see getNumCsgObjects()
   */
	CSGObject* getCsgObject(unsigned int n);


  /**
   * Get a CsgObject from the ListOfCSGObjects.
   *
   * @param n the index number of the CsgObject to get.
   *
   * @return the nth CsgObject in the ListOfCSGObjects within this CSGeometry.
   *
   * @see getNumCsgObjects()
   */
	const CSGObject* getCsgObject(unsigned int n) const;


  /**
   * Get a CsgObject from the ListOfCSGObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CsgObject to get.
   *
   * @return the CsgObject in the ListOfCSGObjects
   * with the given id or NULL if no such
   * CsgObject exists.
   *
   * @see getCsgObject(unsigned int n)
   *
   * @see getNumCsgObjects()
   */
	CSGObject* getCsgObject(const std::string& sid);


  /**
   * Get a CsgObject from the ListOfCSGObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CsgObject to get.
   *
   * @return the CsgObject in the ListOfCSGObjects
   * with the given id or NULL if no such
   * CsgObject exists.
   *
   * @see getCsgObject(unsigned int n)
   *
   * @see getNumCsgObjects()
   */
	const CSGObject* getCsgObject(const std::string& sid) const;


  /**
   * Adds a copy the given "CSGObject" to this CSGeometry.
   *
   * @param csgo; the CSGObject object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addCsgObject(const CSGObject* csgo);


  /**
   * Get the number of CSGObject objects in this CSGeometry.
   *
   * @return the number of CSGObject objects in this CSGeometry
   */
  unsigned int getNumCsgObjects() const;


  /**
   * Creates a new CSGObject object, adds it to this CSGeometrys
   * ListOfCSGObjects and returns the CSGObject object created. 
   *
   * @return a new CSGObject object instance
   *
   * @see addCsgObject(const CSGObject* csgo)
   */
  CSGObject* createCsgObject();


  /**
   * Removes the nth CsgObject from the ListOfCSGObjects within this CSGeometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the CsgObject to remove.
   *
   * @see getNumCsgObjects()
   */
	CSGObject* removeCsgObject(unsigned int n);


  /**
   * Removes the CsgObject with the given identifier from the ListOfCSGObjects within this CSGeometry
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
	CSGObject* removeCsgObject(const std::string& sid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for CSGeometry, is
   * always @c "cSGeometry".
   *
   * @return the name of this element, i.e. @c "cSGeometry".
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
   * for this CSGeometry object have been set.
   *
   * @note The required attributes for a CSGeometry object are:
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this CSGeometry object have been set.
   *
   * @note The required elements for a CSGeometry object are:
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
 * Creates a new CSGeometry_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGeometry_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGeometry_t structure.
 *
 * @returns the newly-created CSGeometry_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGeometry_t *
CSGeometry_create(unsigned int level, unsigned int version,
                  unsigned int pkgVersion);


/**
 * Frees the given CSGeometry_t structure.
 * 
 * @param csg the CSGeometry_t structure to be freed.
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
void
CSGeometry_free(CSGeometry_t * csg);


/**
 * Creates a deep copy of the given CSGeometry_t structure.
 * 
 * @param csg the CSGeometry_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGeometry_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGeometry_t *
CSGeometry_clone(CSGeometry_t * csg);


LIBSBML_EXTERN
int
CSGeometry_addCsgObject(CSGeometry_t * csg, CSGObject_t * csgo);


LIBSBML_EXTERN
CSGObject_t *
CSGeometry_createCsgObject(CSGeometry_t * csg);


LIBSBML_EXTERN
ListOf_t *
CSGeometry_getListOfCSGObjects(CSGeometry_t * csg) ;


LIBSBML_EXTERN
CSGObject_t *
CSGeometry_getCsgObject(CSGeometry_t * csg, unsigned int n);


LIBSBML_EXTERN
CSGObject_t *
CSGeometry_getCsgObjectById(CSGeometry_t * csg, const char * sid);


LIBSBML_EXTERN
unsigned int
CSGeometry_getNumCsgObjects(CSGeometry_t * csg);


LIBSBML_EXTERN
CSGObject_t *
CSGeometry_removeCsgObject(CSGeometry_t * csg, unsigned int n);


LIBSBML_EXTERN
CSGObject_t *
CSGeometry_removeCsgObjectById(CSGeometry_t * csg, const char * sid);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGeometry_t structure have been set.
 *
 * @param csg the CSGeometry_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGeometry_t
 */
LIBSBML_EXTERN
int
CSGeometry_hasRequiredAttributes(const CSGeometry_t * csg);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given CSGeometry_t structure have been set.
 *
 * @param csg the CSGeometry_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGeometry_t
 */
LIBSBML_EXTERN
int
CSGeometry_hasRequiredElements(const CSGeometry_t * csg);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGeometry_H__  */

