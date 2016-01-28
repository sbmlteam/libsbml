/**
 * @file:   GeometryDefinition.h
 * @brief:  Implementation of the GeometryDefinition class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
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


#ifndef GeometryDefinition_H__
#define GeometryDefinition_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN

class AnalyticGeometry;
class SampledFieldGeometry;
class CSGeometry;
class ParametricGeometry;
class MixedGeometry;



class LIBSBML_EXTERN GeometryDefinition : public SBase
{

protected:

  std::string   mId;
  bool          mIsActive;
  bool          mIsSetIsActive;


public:

  /**
   * Creates a new GeometryDefinition with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this GeometryDefinition
   *
   * @param version an unsigned int, the SBML Version to assign to this GeometryDefinition
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this GeometryDefinition
   */
  GeometryDefinition(unsigned int level      = SpatialExtension::getDefaultLevel(),
                     unsigned int version    = SpatialExtension::getDefaultVersion(),
                     unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new GeometryDefinition with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  GeometryDefinition(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for GeometryDefinition.
   *
   * @param orig; the GeometryDefinition instance to copy.
   */
  GeometryDefinition(const GeometryDefinition& orig);


   /**
   * Assignment operator for GeometryDefinition.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  GeometryDefinition& operator=(const GeometryDefinition& rhs);


   /**
   * Creates and returns a deep copy of this GeometryDefinition object.
   *
   * @return a (deep) copy of this GeometryDefinition object.
   */
  virtual GeometryDefinition* clone () const;


   /**
   * Destructor for GeometryDefinition.
   */
  virtual ~GeometryDefinition();


   /**
   * Returns the value of the "id" attribute of this GeometryDefinition.
   *
   * @return the value of the "id" attribute of this GeometryDefinition as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "isActive" attribute of this GeometryDefinition.
   *
   * @return the value of the "isActive" attribute of this GeometryDefinition as a boolean.
   */
  virtual bool getIsActive() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeometryDefinition's "id" attribute has been set.
   *
   * @return @c true if this GeometryDefinition's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeometryDefinition's "isActive" attribute has been set.
   *
   * @return @c true if this GeometryDefinition's "isActive" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetIsActive() const;


  /**
   * Sets the value of the "id" attribute of this GeometryDefinition.
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
   * Sets the value of the "isActive" attribute of this GeometryDefinition.
   *
   * @param isActive; bool value of the "isActive" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setIsActive(bool isActive);


  /**
   * Unsets the value of the "id" attribute of this GeometryDefinition.
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
   * Unsets the value of the "isActive" attribute of this GeometryDefinition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetIsActive();


  /**
   * Returns @c true, if this abstract "GeometryDefinition" is of type AnalyticGeometry.
   *
   * @return @c true, if this abstract "GeometryDefinition" is of type AnalyticGeometry.
   *
   */
  virtual bool isAnalyticGeometry() const;


  /**
   * Returns @c true, if this abstract "GeometryDefinition" is of type SampledFieldGeometry.
   *
   * @return @c true, if this abstract "GeometryDefinition" is of type SampledFieldGeometry.
   *
   */
  virtual bool isSampledFieldGeometry() const;


  /**
   * Returns @c true, if this abstract "GeometryDefinition" is of type CSGeometry.
   *
   * @return @c true, if this abstract "GeometryDefinition" is of type CSGeometry.
   *
   */
  virtual bool isCSGeometry() const;


  /**
   * Returns @c true, if this abstract "GeometryDefinition" is of type ParametricGeometry.
   *
   * @return @c true, if this abstract "GeometryDefinition" is of type ParametricGeometry.
   *
   */
  virtual bool isParametricGeometry() const;


  /**
   * Returns @c true, if this abstract "GeometryDefinition" is of type MixedGeometry.
   *
   * @return @c true, if this abstract "GeometryDefinition" is of type MixedGeometry.
   *
   */
  virtual bool isMixedGeometry() const;


  /**
   * Returns the XML element name of this object, which for GeometryDefinition, is
   * always @c "geometryDefinition".
   *
   * @return the name of this element, i.e. @c "geometryDefinition".
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
   * for this GeometryDefinition object have been set.
   *
   * @note The required attributes for a GeometryDefinition object are:
   * @li "id"
   * @li "isActive"
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

class LIBSBML_EXTERN ListOfGeometryDefinitions : public ListOf
{

public:

  /**
   * Creates a new ListOfGeometryDefinitions with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfGeometryDefinitions
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfGeometryDefinitions
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfGeometryDefinitions
   */
  ListOfGeometryDefinitions(unsigned int level      = SpatialExtension::getDefaultLevel(),
                            unsigned int version    = SpatialExtension::getDefaultVersion(),
                            unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfGeometryDefinitions with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfGeometryDefinitions(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfGeometryDefinitions object.
   *
   * @return a (deep) copy of this ListOfGeometryDefinitions object.
   */
  virtual ListOfGeometryDefinitions* clone () const;


   /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n the index number of the GeometryDefinition to get.
   *
   * @return the nth GeometryDefinition in this ListOfGeometryDefinitions.
   *
   * @see size()
   */
	virtual GeometryDefinition* get(unsigned int n);


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n the index number of the GeometryDefinition to get.
   *
   * @return the nth GeometryDefinition in this ListOfGeometryDefinitions.
   *
   * @see size()
   */
	virtual const GeometryDefinition* get(unsigned int n) const;


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeometryDefinition to get.
   *
   * @return GeometryDefinition in this ListOfGeometryDefinitions
   * with the given id or NULL if no such
   * GeometryDefinition exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual GeometryDefinition* get(const std::string& sid);


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeometryDefinition to get.
   *
   * @return GeometryDefinition in this ListOfGeometryDefinitions
   * with the given id or NULL if no such
   * GeometryDefinition exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const GeometryDefinition* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "GeometryDefinition" to this ListOfGeometryDefinitions.
	 *
	 * @param gd; the GeometryDefinition object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addGeometryDefinition(const GeometryDefinition* gd);


	/**
	 * Get the number of GeometryDefinition objects in this ListOfGeometryDefinitions.
	 *
	 * @return the number of GeometryDefinition objects in this ListOfGeometryDefinitions
	 */
	unsigned int getNumGeometryDefinitions() const;


	/**
	 * Creates a new GeometryDefinition object, adds it to the
	 * ListOfGeometryDefinitions and returns the GeometryDefinition object created. 
	 *
	 * @return a new GeometryDefinition object instance
	 *
	 * @see addGeometryDefinition(const GeometryDefinition* gd)
	 */
	AnalyticGeometry* createAnalyticGeometry();


	/**
	 * Creates a new GeometryDefinition object, adds it to the
	 * ListOfGeometryDefinitions and returns the GeometryDefinition object created. 
	 *
	 * @return a new GeometryDefinition object instance
	 *
	 * @see addGeometryDefinition(const GeometryDefinition* gd)
	 */
	SampledFieldGeometry* createSampledFieldGeometry();


	/**
	 * Creates a new GeometryDefinition object, adds it to the
	 * ListOfGeometryDefinitions and returns the GeometryDefinition object created. 
	 *
	 * @return a new GeometryDefinition object instance
	 *
	 * @see addGeometryDefinition(const GeometryDefinition* gd)
	 */
	CSGeometry* createCsGeometry();


	/**
	 * Creates a new GeometryDefinition object, adds it to the
	 * ListOfGeometryDefinitions and returns the GeometryDefinition object created. 
	 *
	 * @return a new GeometryDefinition object instance
	 *
	 * @see addGeometryDefinition(const GeometryDefinition* gd)
	 */
	ParametricGeometry* createParametricGeometry();


	/**
	 * Creates a new GeometryDefinition object, adds it to the
	 * ListOfGeometryDefinitions and returns the GeometryDefinition object created. 
	 *
	 * @return a new GeometryDefinition object instance
	 *
	 * @see addGeometryDefinition(const GeometryDefinition* gd)
	 */
	MixedGeometry* createMixedGeometry();


  /**
   * Removes the nth GeometryDefinition from this ListOfGeometryDefinitions
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the GeometryDefinition to remove.
   *
   * @see size()
   */
	virtual GeometryDefinition* remove(unsigned int n);


  /**
   * Removes the GeometryDefinition from this ListOfGeometryDefinitions with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the GeometryDefinition to remove.
   *
   * @return the GeometryDefinition removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual GeometryDefinition* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfGeometryDefinitions, is
   * always @c "listOfGeometryDefinitions".
   *
   * @return the name of this element, i.e. @c "listOfGeometryDefinitions".
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
   * Creates a new GeometryDefinition in this ListOfGeometryDefinitions
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Spatial package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


	virtual bool isValidTypeForList(SBase * item) {
		int code = item->getTypeCode();
		return code == getItemTypeCode() || code == SBML_SPATIAL_ANALYTICGEOMETRY || code == SBML_SPATIAL_SAMPLEDFIELDGEOMETRY || code == SBML_SPATIAL_CSGEOMETRY || code == SBML_SPATIAL_PARAMETRICGEOMETRY || code == SBML_SPATIAL_MIXEDGEOMETRY ;
	}



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new GeometryDefinition_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * GeometryDefinition_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * GeometryDefinition_t structure.
 *
 * @returns the newly-created GeometryDefinition_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
GeometryDefinition_t *
GeometryDefinition_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion);


/**
 * Frees the given GeometryDefinition_t structure.
 * 
 * @param gd the GeometryDefinition_t structure to be freed.
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
void
GeometryDefinition_free(GeometryDefinition_t * gd);


/**
 * Creates a deep copy of the given GeometryDefinition_t structure.
 * 
 * @param gd the GeometryDefinition_t structure to be copied.
 *
 * @returns a (deep) copy of the given GeometryDefinition_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof GeometryDefinition_t
 */
LIBSBML_EXTERN
GeometryDefinition_t *
GeometryDefinition_clone(GeometryDefinition_t * gd);


/**
 * Returns the value of the "id" attribute of the given GeometryDefinition_t
 * structure.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return the id of this structure.
 *
 * @member of GeometryDefinition_t
 */
LIBSBML_EXTERN
const char *
GeometryDefinition_getId(const GeometryDefinition_t * gd);


/**
 * Returns the value of the "isActive" attribute of the given GeometryDefinition_t
 * structure.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return the isActive of this structure.
 *
 * @member of GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_getIsActive(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 if the given GeometryDefinition_t structure's "id"
 * is set.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 if the "id" of this GeometryDefinition_t structure is
 * set, @c 0 otherwise.
 *
 * @member of GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSetId(const GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 if the given GeometryDefinition_t structure's "isActive"
 * is set.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return @c 1 if the "isActive" of this GeometryDefinition_t structure is
 * set, @c 0 otherwise.
 *
 * @member of GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_isSetIsActive(const GeometryDefinition_t * gd);


/**
 * Sets the "id" attribute of the given GeometryDefinition_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs GeometryDefinition_unsetId() instead.
 *
 * @param gd the GeometryDefinition_t structure.
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
 * @member of GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_setId(GeometryDefinition_t * gd, const char * id);


/**
 * Sets the "isActive" attribute of the given GeometryDefinition_t structure.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @param isActive the string to which the structures "isActive" attribute should be
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
 * @member of GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_setIsActive(GeometryDefinition_t * gd, int isActive);


/**
 * Unsets the value of the "id" attribute of the given 
 * GeometryDefinition_t structure.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_unsetId(GeometryDefinition_t * gd);


/**
 * Unsets the value of the "isActive" attribute of the given 
 * GeometryDefinition_t structure.
 *
 * @param gd the GeometryDefinition_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_unsetIsActive(GeometryDefinition_t * gd);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given GeometryDefinition_t structure have been set.
 *
 * @param gd the GeometryDefinition_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of GeometryDefinition_t
 */
LIBSBML_EXTERN
int
GeometryDefinition_hasRequiredAttributes(const GeometryDefinition_t * gd);


LIBSBML_EXTERN
GeometryDefinition_t *
ListOfGeometryDefinitions_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
GeometryDefinition_t *
ListOfGeometryDefinitions_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  GeometryDefinition_H__  */

