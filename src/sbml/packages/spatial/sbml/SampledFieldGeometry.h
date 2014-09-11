/**
 * @file:   SampledFieldGeometry.h
 * @brief:  Implementation of the SampledFieldGeometry class
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


#ifndef SampledFieldGeometry_H__
#define SampledFieldGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>

#include <sbml/packages/spatial/sbml/SampledVolume.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN SampledFieldGeometry : public GeometryDefinition
{

protected:

  ListOfSampledVolumes   mSampledVolumes;
  std::string   mSampledField;


public:

  /**
   * Creates a new SampledFieldGeometry with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SampledFieldGeometry
   *
   * @param version an unsigned int, the SBML Version to assign to this SampledFieldGeometry
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this SampledFieldGeometry
   */
  SampledFieldGeometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
                       unsigned int version    = SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SampledFieldGeometry with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  SampledFieldGeometry(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for SampledFieldGeometry.
   *
   * @param orig; the SampledFieldGeometry instance to copy.
   */
  SampledFieldGeometry(const SampledFieldGeometry& orig);


   /**
   * Assignment operator for SampledFieldGeometry.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SampledFieldGeometry& operator=(const SampledFieldGeometry& rhs);


   /**
   * Creates and returns a deep copy of this SampledFieldGeometry object.
   *
   * @return a (deep) copy of this SampledFieldGeometry object.
   */
  virtual SampledFieldGeometry* clone () const;


   /**
   * Destructor for SampledFieldGeometry.
   */
  virtual ~SampledFieldGeometry();


   /**
   * Returns the value of the "sampledField" attribute of this SampledFieldGeometry.
   *
   * @return the value of the "sampledField" attribute of this SampledFieldGeometry as a string.
   */
  virtual const std::string& getSampledField() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledFieldGeometry's "sampledField" attribute has been set.
   *
   * @return @c true if this SampledFieldGeometry's "sampledField" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSampledField() const;


  /**
   * Sets the value of the "sampledField" attribute of this SampledFieldGeometry.
   *
   * @param sampledField; const std::string& value of the "sampledField" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSampledField(const std::string& sampledField);


  /**
   * Unsets the value of the "sampledField" attribute of this SampledFieldGeometry.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSampledField();


  /**
   * Returns the  "ListOfSampledVolumes" in this SampledFieldGeometry object.
   *
   * @return the "ListOfSampledVolumes" attribute of this SampledFieldGeometry.
   */
  const ListOfSampledVolumes* getListOfSampledVolumes() const;


  /**
   * Returns the  "ListOfSampledVolumes" in this SampledFieldGeometry object.
   *
   * @return the "ListOfSampledVolumes" attribute of this SampledFieldGeometry.
   */
  ListOfSampledVolumes* getListOfSampledVolumes();


  /**
   * Get a SampledVolume from the ListOfSampledVolumes.
   *
   * @param n the index number of the SampledVolume to get.
   *
   * @return the nth SampledVolume in the ListOfSampledVolumes within this SampledFieldGeometry.
   *
   * @see getNumSampledVolumes()
   */
	SampledVolume* getSampledVolume(unsigned int n);


  /**
   * Get a SampledVolume from the ListOfSampledVolumes.
   *
   * @param n the index number of the SampledVolume to get.
   *
   * @return the nth SampledVolume in the ListOfSampledVolumes within this SampledFieldGeometry.
   *
   * @see getNumSampledVolumes()
   */
	const SampledVolume* getSampledVolume(unsigned int n) const;


  /**
   * Get a SampledVolume from the ListOfSampledVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SampledVolume to get.
   *
   * @return the SampledVolume in the ListOfSampledVolumes
   * with the given id or NULL if no such
   * SampledVolume exists.
   *
   * @see getSampledVolume(unsigned int n)
   *
   * @see getNumSampledVolumes()
   */
	SampledVolume* getSampledVolume(const std::string& sid);


  /**
   * Get a SampledVolume from the ListOfSampledVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SampledVolume to get.
   *
   * @return the SampledVolume in the ListOfSampledVolumes
   * with the given id or NULL if no such
   * SampledVolume exists.
   *
   * @see getSampledVolume(unsigned int n)
   *
   * @see getNumSampledVolumes()
   */
	const SampledVolume* getSampledVolume(const std::string& sid) const;


  /**
   * Adds a copy the given "SampledVolume" to this SampledFieldGeometry.
   *
   * @param sv; the SampledVolume object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addSampledVolume(const SampledVolume* sv);


  /**
   * Get the number of SampledVolume objects in this SampledFieldGeometry.
   *
   * @return the number of SampledVolume objects in this SampledFieldGeometry
   */
  unsigned int getNumSampledVolumes() const;


  /**
   * Creates a new SampledVolume object, adds it to this SampledFieldGeometrys
   * ListOfSampledVolumes and returns the SampledVolume object created. 
   *
   * @return a new SampledVolume object instance
   *
   * @see addSampledVolume(const SampledVolume* sv)
   */
  SampledVolume* createSampledVolume();


  /**
   * Removes the nth SampledVolume from the ListOfSampledVolumes within this SampledFieldGeometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SampledVolume to remove.
   *
   * @see getNumSampledVolumes()
   */
	SampledVolume* removeSampledVolume(unsigned int n);


  /**
   * Removes the SampledVolume with the given identifier from the ListOfSampledVolumes within this SampledFieldGeometry
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SampledVolume to remove.
   *
   * @return the SampledVolume removed. As mentioned above, the caller owns the
   * returned item.
   */
	SampledVolume* removeSampledVolume(const std::string& sid);


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
   * Returns the XML element name of this object, which for SampledFieldGeometry, is
   * always @c "sampledFieldGeometry".
   *
   * @return the name of this element, i.e. @c "sampledFieldGeometry".
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
   * for this SampledFieldGeometry object have been set.
   *
   * @note The required attributes for a SampledFieldGeometry object are:
   * @li "sampledField"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this SampledFieldGeometry object have been set.
   *
   * @note The required elements for a SampledFieldGeometry object are:
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
 * Creates a new SampledFieldGeometry_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * SampledFieldGeometry_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * SampledFieldGeometry_t structure.
 *
 * @returns the newly-created SampledFieldGeometry_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledFieldGeometry_t *
SampledFieldGeometry_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion);


/**
 * Frees the given SampledFieldGeometry_t structure.
 * 
 * @param sfg the SampledFieldGeometry_t structure to be freed.
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
void
SampledFieldGeometry_free(SampledFieldGeometry_t * sfg);


/**
 * Creates a deep copy of the given SampledFieldGeometry_t structure.
 * 
 * @param sfg the SampledFieldGeometry_t structure to be copied.
 *
 * @returns a (deep) copy of the given SampledFieldGeometry_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof SampledFieldGeometry_t
 */
LIBSBML_EXTERN
SampledFieldGeometry_t *
SampledFieldGeometry_clone(SampledFieldGeometry_t * sfg);


/**
 * Returns the value of the "sampledField" attribute of the given SampledFieldGeometry_t
 * structure.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @return the sampledField of this structure.
 *
 * @member of SampledFieldGeometry_t
 */
LIBSBML_EXTERN
const char *
SampledFieldGeometry_getSampledField(const SampledFieldGeometry_t * sfg);


/**
 * Predicate returning @c 1 if the given SampledFieldGeometry_t structure's "sampledField"
 * is set.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @return @c 1 if the "sampledField" of this SampledFieldGeometry_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_isSetSampledField(const SampledFieldGeometry_t * sfg);


/**
 * Sets the "sampledField" attribute of the given SampledFieldGeometry_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SampledFieldGeometry_unsetSampledField() instead.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @param sampledField the string to which the structures "sampledField" attribute should be
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
 * @member of SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_setSampledField(SampledFieldGeometry_t * sfg, const char * sampledField);


/**
 * Unsets the value of the "sampledField" attribute of the given 
 *SampledFieldGeometry_t structure.
 *
 * @param sfg the SampledFieldGeometry_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_unsetSampledField(SampledFieldGeometry_t * sfg);


LIBSBML_EXTERN
int
SampledFieldGeometry_addSampledVolume(SampledFieldGeometry_t * sfg, SampledVolume_t * sv);


LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_createSampledVolume(SampledFieldGeometry_t * sfg);


LIBSBML_EXTERN
ListOf_t *
SampledFieldGeometry_getListOfSampledVolumes(SampledFieldGeometry_t * sfg) ;


LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_getSampledVolume(SampledFieldGeometry_t * sfg, unsigned int n);


LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_getSampledVolumeById(SampledFieldGeometry_t * sfg, const char * sid);


LIBSBML_EXTERN
unsigned int
SampledFieldGeometry_getNumSampledVolumes(SampledFieldGeometry_t * sfg);


LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_removeSampledVolume(SampledFieldGeometry_t * sfg, unsigned int n);


LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_removeSampledVolumeById(SampledFieldGeometry_t * sfg, const char * sid);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given SampledFieldGeometry_t structure have been set.
 *
 * @param sfg the SampledFieldGeometry_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_hasRequiredAttributes(const SampledFieldGeometry_t * sfg);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given SampledFieldGeometry_t structure have been set.
 *
 * @param sfg the SampledFieldGeometry_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of SampledFieldGeometry_t
 */
LIBSBML_EXTERN
int
SampledFieldGeometry_hasRequiredElements(const SampledFieldGeometry_t * sfg);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SampledFieldGeometry_H__  */

