/**
 * @file:   MixedGeometry.h
 * @brief:  Implementation of the MixedGeometry class
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


#ifndef MixedGeometry_H__
#define MixedGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>

#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/OrdinalMapping.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN MixedGeometry : public GeometryDefinition
{

protected:

  ListOfGeometryDefinitions   mGeometryDefinitions;
  ListOfOrdinalMappings   mOrdinalMappings;


public:

  /**
   * Creates a new MixedGeometry with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this MixedGeometry
   *
   * @param version an unsigned int, the SBML Version to assign to this MixedGeometry
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this MixedGeometry
   */
  MixedGeometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
                unsigned int version    = SpatialExtension::getDefaultVersion(),
                unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new MixedGeometry with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  MixedGeometry(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for MixedGeometry.
   *
   * @param orig; the MixedGeometry instance to copy.
   */
  MixedGeometry(const MixedGeometry& orig);


   /**
   * Assignment operator for MixedGeometry.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  MixedGeometry& operator=(const MixedGeometry& rhs);


   /**
   * Creates and returns a deep copy of this MixedGeometry object.
   *
   * @return a (deep) copy of this MixedGeometry object.
   */
  virtual MixedGeometry* clone () const;


   /**
   * Destructor for MixedGeometry.
   */
  virtual ~MixedGeometry();


   /**
   * Returns the  "ListOfGeometryDefinitions" in this MixedGeometry object.
   *
   * @return the "ListOfGeometryDefinitions" attribute of this MixedGeometry.
   */
  const ListOfGeometryDefinitions* getListOfGeometryDefinitions() const;


  /**
   * Returns the  "ListOfGeometryDefinitions" in this MixedGeometry object.
   *
   * @return the "ListOfGeometryDefinitions" attribute of this MixedGeometry.
   */
  ListOfGeometryDefinitions* getListOfGeometryDefinitions();


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n the index number of the GeometryDefinition to get.
   *
   * @return the nth GeometryDefinition in the ListOfGeometryDefinitions within this MixedGeometry.
   *
   * @see getNumGeometryDefinitions()
   */
	GeometryDefinition* getGeometryDefinition(unsigned int n);


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n the index number of the GeometryDefinition to get.
   *
   * @return the nth GeometryDefinition in the ListOfGeometryDefinitions within this MixedGeometry.
   *
   * @see getNumGeometryDefinitions()
   */
	const GeometryDefinition* getGeometryDefinition(unsigned int n) const;


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeometryDefinition to get.
   *
   * @return the GeometryDefinition in the ListOfGeometryDefinitions
   * with the given id or NULL if no such
   * GeometryDefinition exists.
   *
   * @see getGeometryDefinition(unsigned int n)
   *
   * @see getNumGeometryDefinitions()
   */
	GeometryDefinition* getGeometryDefinition(const std::string& sid);


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeometryDefinition to get.
   *
   * @return the GeometryDefinition in the ListOfGeometryDefinitions
   * with the given id or NULL if no such
   * GeometryDefinition exists.
   *
   * @see getGeometryDefinition(unsigned int n)
   *
   * @see getNumGeometryDefinitions()
   */
	const GeometryDefinition* getGeometryDefinition(const std::string& sid) const;


  /**
   * Adds a copy the given "GeometryDefinition" to this MixedGeometry.
   *
   * @param gd; the GeometryDefinition object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addGeometryDefinition(const GeometryDefinition* gd);


  /**
   * Get the number of GeometryDefinition objects in this MixedGeometry.
   *
   * @return the number of GeometryDefinition objects in this MixedGeometry
   */
  unsigned int getNumGeometryDefinitions() const;


  /**
   * Creates a new AnalyticGeometry object, adds it to this MixedGeometrys
   * ListOfGeometryDefinitions and returns the AnalyticGeometry object created. 
   *
   * @return a new AnalyticGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  AnalyticGeometry* createAnalyticGeometry();


  /**
   * Creates a new SampledFieldGeometry object, adds it to this MixedGeometrys
   * ListOfGeometryDefinitions and returns the SampledFieldGeometry object created. 
   *
   * @return a new SampledFieldGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  SampledFieldGeometry* createSampledFieldGeometry();


  /**
   * Creates a new CSGeometry object, adds it to this MixedGeometrys
   * ListOfGeometryDefinitions and returns the CSGeometry object created. 
   *
   * @return a new CSGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  CSGeometry* createCsGeometry();


  /**
   * Creates a new ParametricGeometry object, adds it to this MixedGeometrys
   * ListOfGeometryDefinitions and returns the ParametricGeometry object created. 
   *
   * @return a new ParametricGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  ParametricGeometry* createParametricGeometry();


  /**
   * Creates a new MixedGeometry object, adds it to this MixedGeometrys
   * ListOfGeometryDefinitions and returns the MixedGeometry object created. 
   *
   * @return a new MixedGeometry object instance
   *
   * @see addGeometryDefinition(const GeometryDefinition* gd)
   */
  MixedGeometry* createMixedGeometry();


  /**
   * Removes the nth GeometryDefinition from the ListOfGeometryDefinitions within this MixedGeometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the GeometryDefinition to remove.
   *
   * @see getNumGeometryDefinitions()
   */
	GeometryDefinition* removeGeometryDefinition(unsigned int n);


  /**
   * Removes the GeometryDefinition with the given identifier from the ListOfGeometryDefinitions within this MixedGeometry
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
	GeometryDefinition* removeGeometryDefinition(const std::string& sid);


  /**
   * Returns the  "ListOfOrdinalMappings" in this MixedGeometry object.
   *
   * @return the "ListOfOrdinalMappings" attribute of this MixedGeometry.
   */
  const ListOfOrdinalMappings* getListOfOrdinalMappings() const;


  /**
   * Returns the  "ListOfOrdinalMappings" in this MixedGeometry object.
   *
   * @return the "ListOfOrdinalMappings" attribute of this MixedGeometry.
   */
  ListOfOrdinalMappings* getListOfOrdinalMappings();


  /**
   * Get a OrdinalMapping from the ListOfOrdinalMappings.
   *
   * @param n the index number of the OrdinalMapping to get.
   *
   * @return the nth OrdinalMapping in the ListOfOrdinalMappings within this MixedGeometry.
   *
   * @see getNumOrdinalMappings()
   */
	OrdinalMapping* getOrdinalMapping(unsigned int n);


  /**
   * Get a OrdinalMapping from the ListOfOrdinalMappings.
   *
   * @param n the index number of the OrdinalMapping to get.
   *
   * @return the nth OrdinalMapping in the ListOfOrdinalMappings within this MixedGeometry.
   *
   * @see getNumOrdinalMappings()
   */
	const OrdinalMapping* getOrdinalMapping(unsigned int n) const;


  /**
   * Get a OrdinalMapping from the ListOfOrdinalMappings
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the OrdinalMapping to get.
   *
   * @return the OrdinalMapping in the ListOfOrdinalMappings
   * with the given id or NULL if no such
   * OrdinalMapping exists.
   *
   * @see getOrdinalMapping(unsigned int n)
   *
   * @see getNumOrdinalMappings()
   */
	OrdinalMapping* getOrdinalMapping(const std::string& sid);


  /**
   * Get a OrdinalMapping from the ListOfOrdinalMappings
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the OrdinalMapping to get.
   *
   * @return the OrdinalMapping in the ListOfOrdinalMappings
   * with the given id or NULL if no such
   * OrdinalMapping exists.
   *
   * @see getOrdinalMapping(unsigned int n)
   *
   * @see getNumOrdinalMappings()
   */
	const OrdinalMapping* getOrdinalMapping(const std::string& sid) const;


  /**
   * Adds a copy the given "OrdinalMapping" to this MixedGeometry.
   *
   * @param om; the OrdinalMapping object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addOrdinalMapping(const OrdinalMapping* om);


  /**
   * Get the number of OrdinalMapping objects in this MixedGeometry.
   *
   * @return the number of OrdinalMapping objects in this MixedGeometry
   */
  unsigned int getNumOrdinalMappings() const;


  /**
   * Creates a new OrdinalMapping object, adds it to this MixedGeometrys
   * ListOfOrdinalMappings and returns the OrdinalMapping object created. 
   *
   * @return a new OrdinalMapping object instance
   *
   * @see addOrdinalMapping(const OrdinalMapping* om)
   */
  OrdinalMapping* createOrdinalMapping();


  /**
   * Removes the nth OrdinalMapping from the ListOfOrdinalMappings within this MixedGeometry.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the OrdinalMapping to remove.
   *
   * @see getNumOrdinalMappings()
   */
	OrdinalMapping* removeOrdinalMapping(unsigned int n);


  /**
   * Removes the OrdinalMapping with the given identifier from the ListOfOrdinalMappings within this MixedGeometry
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
	OrdinalMapping* removeOrdinalMapping(const std::string& sid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for MixedGeometry, is
   * always @c "mixedGeometry".
   *
   * @return the name of this element, i.e. @c "mixedGeometry".
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
   * for this MixedGeometry object have been set.
   *
   * @note The required attributes for a MixedGeometry object are:
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this MixedGeometry object have been set.
   *
   * @note The required elements for a MixedGeometry object are:
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
 * Creates a new MixedGeometry_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * MixedGeometry_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * MixedGeometry_t structure.
 *
 * @returns the newly-created MixedGeometry_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
MixedGeometry_t *
MixedGeometry_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion);


/**
 * Frees the given MixedGeometry_t structure.
 * 
 * @param mg the MixedGeometry_t structure to be freed.
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
void
MixedGeometry_free(MixedGeometry_t * mg);


/**
 * Creates a deep copy of the given MixedGeometry_t structure.
 * 
 * @param mg the MixedGeometry_t structure to be copied.
 *
 * @returns a (deep) copy of the given MixedGeometry_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof MixedGeometry_t
 */
LIBSBML_EXTERN
MixedGeometry_t *
MixedGeometry_clone(MixedGeometry_t * mg);


LIBSBML_EXTERN
int
MixedGeometry_addGeometryDefinition(MixedGeometry_t * mg, GeometryDefinition_t * gd);


LIBSBML_EXTERN
AnalyticGeometry_t *
MixedGeometry_createAnalyticGeometry(MixedGeometry_t * mg);


LIBSBML_EXTERN
SampledFieldGeometry_t *
MixedGeometry_createSampledFieldGeometry(MixedGeometry_t * mg);


LIBSBML_EXTERN
CSGeometry_t *
MixedGeometry_createCsGeometry(MixedGeometry_t * mg);


LIBSBML_EXTERN
ParametricGeometry_t *
MixedGeometry_createParametricGeometry(MixedGeometry_t * mg);


LIBSBML_EXTERN
MixedGeometry_t *
MixedGeometry_createMixedGeometry(MixedGeometry_t * mg);


LIBSBML_EXTERN
ListOf_t *
MixedGeometry_getListOfGeometryDefinitions(MixedGeometry_t * mg) ;


LIBSBML_EXTERN
GeometryDefinition_t *
MixedGeometry_getGeometryDefinition(MixedGeometry_t * mg, unsigned int n);


LIBSBML_EXTERN
GeometryDefinition_t *
MixedGeometry_getGeometryDefinitionById(MixedGeometry_t * mg, const char * sid);


LIBSBML_EXTERN
unsigned int
MixedGeometry_getNumGeometryDefinitions(MixedGeometry_t * mg);


LIBSBML_EXTERN
GeometryDefinition_t *
MixedGeometry_removeGeometryDefinition(MixedGeometry_t * mg, unsigned int n);


LIBSBML_EXTERN
GeometryDefinition_t *
MixedGeometry_removeGeometryDefinitionById(MixedGeometry_t * mg, const char * sid);


LIBSBML_EXTERN
int
MixedGeometry_addOrdinalMapping(MixedGeometry_t * mg, OrdinalMapping_t * om);


LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_createOrdinalMapping(MixedGeometry_t * mg);


LIBSBML_EXTERN
ListOf_t *
MixedGeometry_getListOfOrdinalMappings(MixedGeometry_t * mg) ;


LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_getOrdinalMapping(MixedGeometry_t * mg, unsigned int n);


LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_getOrdinalMappingById(MixedGeometry_t * mg, const char * sid);


LIBSBML_EXTERN
unsigned int
MixedGeometry_getNumOrdinalMappings(MixedGeometry_t * mg);


LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_removeOrdinalMapping(MixedGeometry_t * mg, unsigned int n);


LIBSBML_EXTERN
OrdinalMapping_t *
MixedGeometry_removeOrdinalMappingById(MixedGeometry_t * mg, const char * sid);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given MixedGeometry_t structure have been set.
 *
 * @param mg the MixedGeometry_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of MixedGeometry_t
 */
LIBSBML_EXTERN
int
MixedGeometry_hasRequiredAttributes(const MixedGeometry_t * mg);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given MixedGeometry_t structure have been set.
 *
 * @param mg the MixedGeometry_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of MixedGeometry_t
 */
LIBSBML_EXTERN
int
MixedGeometry_hasRequiredElements(const MixedGeometry_t * mg);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  MixedGeometry_H__  */

