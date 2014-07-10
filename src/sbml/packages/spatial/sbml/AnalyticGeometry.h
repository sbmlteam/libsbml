/**
 * @file:   AnalyticGeometry.h
 * @brief:  Implementation of the AnalyticGeometry class
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


#ifndef AnalyticGeometry_H__
#define AnalyticGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>

#include <sbml/packages/spatial/sbml/AnalyticVolume.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN AnalyticGeometry : public GeometryDefinition
{

protected:

  ListOfAnalyticVolumes   mAnalyticVolumes;


public:

  /**
   * Creates a new AnalyticGeometry with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this AnalyticGeometry
   *
   * @param version an unsigned int, the SBML Version to assign to this AnalyticGeometry
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this AnalyticGeometry
   */
  AnalyticGeometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
                   unsigned int version    = SpatialExtension::getDefaultVersion(),
                   unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AnalyticGeometry with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  AnalyticGeometry(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for AnalyticGeometry.
   *
   * @param orig; the AnalyticGeometry instance to copy.
   */
  AnalyticGeometry(const AnalyticGeometry& orig);


   /**
   * Assignment operator for AnalyticGeometry.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  AnalyticGeometry& operator=(const AnalyticGeometry& rhs);


   /**
   * Creates and returns a deep copy of this AnalyticGeometry object.
   *
   * @return a (deep) copy of this AnalyticGeometry object.
   */
  virtual AnalyticGeometry* clone () const;


   /**
   * Destructor for AnalyticGeometry.
   */
  virtual ~AnalyticGeometry();


   /**
   * Returns the  "ListOfAnalyticVolumes" in this AnalyticGeometry object.
   *
   * @return the "ListOfAnalyticVolumes" attribute of this AnalyticGeometry.
   */
  const ListOfAnalyticVolumes* getListOfAnalyticVolumes() const;


  /**
   * Returns the  "ListOfAnalyticVolumes" in this AnalyticGeometry object.
   *
   * @return the "ListOfAnalyticVolumes" attribute of this AnalyticGeometry.
   */
  ListOfAnalyticVolumes* getListOfAnalyticVolumes();


  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes.
   *
   * @param n the index number of the AnalyticVolume to get.
   *
   * @return the nth AnalyticVolume in the ListOfAnalyticVolumes within this AnalyticGeometry.
   *
   * @see getNumAnalyticVolumes()
   */
	AnalyticVolume* getAnalyticVolume(unsigned int n);


  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes.
   *
   * @param n the index number of the AnalyticVolume to get.
   *
   * @return the nth AnalyticVolume in the ListOfAnalyticVolumes within this AnalyticGeometry.
   *
   * @see getNumAnalyticVolumes()
   */
	const AnalyticVolume* getAnalyticVolume(unsigned int n) const;


  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the AnalyticVolume to get.
   *
   * @return the AnalyticVolume in the ListOfAnalyticVolumes
   * with the given id or NULL if no such
   * AnalyticVolume exists.
   *
   * @see getAnalyticVolume(unsigned int n)
   *
   * @see getNumAnalyticVolumes()
   */
	AnalyticVolume* getAnalyticVolume(const std::string& sid);


  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the AnalyticVolume to get.
   *
   * @return the AnalyticVolume in the ListOfAnalyticVolumes
   * with the given id or NULL if no such
   * AnalyticVolume exists.
   *
   * @see getAnalyticVolume(unsigned int n)
   *
   * @see getNumAnalyticVolumes()
   */
	const AnalyticVolume* getAnalyticVolume(const std::string& sid) const;


  /**
   * Adds a copy the given "AnalyticVolume" to this AnalyticGeometry.
   *
   * @param av; the AnalyticVolume object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addAnalyticVolume(const AnalyticVolume* av);


  /**
   * Get the number of AnalyticVolume objects in this AnalyticGeometry.
   *
   * @return the number of AnalyticVolume objects in this AnalyticGeometry
   */
  unsigned int getNumAnalyticVolumes() const;


  /**
   * Creates a new AnalyticVolume object, adds it to this AnalyticGeometrys
   * ListOfAnalyticVolumes and returns the AnalyticVolume object created. 
   *
   * @return a new AnalyticVolume object instance
   *
   * @see addAnalyticVolume(const AnalyticVolume* av)
   */
  AnalyticVolume* createAnalyticVolume();


  /**
   * Removes the nth AnalyticVolume from the ListOfAnalyticVolumes within this AnalyticGeometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the AnalyticVolume to remove.
   *
   * @see getNumAnalyticVolumes()
   */
	AnalyticVolume* removeAnalyticVolume(unsigned int n);


  /**
   * Removes the AnalyticVolume with the given identifier from the ListOfAnalyticVolumes within this AnalyticGeometry
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the AnalyticVolume to remove.
   *
   * @return the AnalyticVolume removed. As mentioned above, the caller owns the
   * returned item.
   */
	AnalyticVolume* removeAnalyticVolume(const std::string& sid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for AnalyticGeometry, is
   * always @c "analyticGeometry".
   *
   * @return the name of this element, i.e. @c "analyticGeometry".
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
   * for this AnalyticGeometry object have been set.
   *
   * @note The required attributes for a AnalyticGeometry object are:
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this AnalyticGeometry object have been set.
   *
   * @note The required elements for a AnalyticGeometry object are:
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
 * Creates a new AnalyticGeometry_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * AnalyticGeometry_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * AnalyticGeometry_t structure.
 *
 * @returns the newly-created AnalyticGeometry_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
AnalyticGeometry_t *
AnalyticGeometry_create(unsigned int level, unsigned int version,
                        unsigned int pkgVersion);


/**
 * Frees the given AnalyticGeometry_t structure.
 * 
 * @param ag the AnalyticGeometry_t structure to be freed.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
void
AnalyticGeometry_free(AnalyticGeometry_t * ag);


/**
 * Creates a deep copy of the given AnalyticGeometry_t structure.
 * 
 * @param ag the AnalyticGeometry_t structure to be copied.
 *
 * @returns a (deep) copy of the given AnalyticGeometry_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof AnalyticGeometry_t
 */
LIBSBML_EXTERN
AnalyticGeometry_t *
AnalyticGeometry_clone(AnalyticGeometry_t * ag);


LIBSBML_EXTERN
int
AnalyticGeometry_addAnalyticVolume(AnalyticGeometry_t * ag, AnalyticVolume_t * av);


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_createAnalyticVolume(AnalyticGeometry_t * ag);


LIBSBML_EXTERN
ListOf_t *
AnalyticGeometry_getListOfAnalyticVolumes(AnalyticGeometry_t * ag) ;


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_getAnalyticVolume(AnalyticGeometry_t * ag, unsigned int n);


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_getAnalyticVolumeById(AnalyticGeometry_t * ag, const char * sid);


LIBSBML_EXTERN
unsigned int
AnalyticGeometry_getNumAnalyticVolumes(AnalyticGeometry_t * ag);


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_removeAnalyticVolume(AnalyticGeometry_t * ag, unsigned int n);


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_removeAnalyticVolumeById(AnalyticGeometry_t * ag, const char * sid);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given AnalyticGeometry_t structure have been set.
 *
 * @param ag the AnalyticGeometry_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of AnalyticGeometry_t
 */
LIBSBML_EXTERN
int
AnalyticGeometry_hasRequiredAttributes(const AnalyticGeometry_t * ag);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given AnalyticGeometry_t structure have been set.
 *
 * @param ag the AnalyticGeometry_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of AnalyticGeometry_t
 */
LIBSBML_EXTERN
int
AnalyticGeometry_hasRequiredElements(const AnalyticGeometry_t * ag);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  AnalyticGeometry_H__  */

