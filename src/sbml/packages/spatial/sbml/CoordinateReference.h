/**
 * @file:   CoordinateReference.h
 * @brief:  Implementation of the CoordinateReference class
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


#ifndef CoordinateReference_H__
#define CoordinateReference_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN CoordinateReference : public SBase
{

protected:

  CoordinateKind_t   mCoordinate;


public:

  /**
   * Creates a new CoordinateReference with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CoordinateReference
   *
   * @param version an unsigned int, the SBML Version to assign to this CoordinateReference
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CoordinateReference
   */
  CoordinateReference(unsigned int level      = SpatialExtension::getDefaultLevel(),
                      unsigned int version    = SpatialExtension::getDefaultVersion(),
                      unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CoordinateReference with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CoordinateReference(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CoordinateReference.
   *
   * @param orig; the CoordinateReference instance to copy.
   */
  CoordinateReference(const CoordinateReference& orig);


   /**
   * Assignment operator for CoordinateReference.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CoordinateReference& operator=(const CoordinateReference& rhs);


   /**
   * Creates and returns a deep copy of this CoordinateReference object.
   *
   * @return a (deep) copy of this CoordinateReference object.
   */
  virtual CoordinateReference* clone () const;


   /**
   * Destructor for CoordinateReference.
   */
  virtual ~CoordinateReference();


   /**
   * Returns the value of the "coordinate" attribute of this CoordinateReference.
   *
   * @return the value of the "coordinate" attribute of this CoordinateReference as a CoordinateKind_t.
   */
  virtual CoordinateKind_t getCoordinate() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateReference's "coordinate" attribute has been set.
   *
   * @return @c true if this CoordinateReference's "coordinate" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoordinate() const;


  /**
   * Sets the value of the "coordinate" attribute of this CoordinateReference.
   *
   * @param coordinate; CoordinateKind_t value of the "coordinate" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinate(CoordinateKind_t coordinate);


  /**
   * Sets the value of the "coordinate" attribute of this CoordinateReference.
   *
   * @param coordinate; string value of the "coordinate" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinate(const std::string& coordinate);


  /**
   * Unsets the value of the "coordinate" attribute of this CoordinateReference.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoordinate();


  /**
   * Returns the XML element name of this object, which for CoordinateReference, is
   * always @c "coordinateReference".
   *
   * @return the name of this element, i.e. @c "coordinateReference".
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
   * for this CoordinateReference object have been set.
   *
   * @note The required attributes for a CoordinateReference object are:
   * @li "coordinate"
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

class LIBSBML_EXTERN ListOfCoordinateReferences : public ListOf
{

public:

  /**
   * Creates a new ListOfCoordinateReferences with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfCoordinateReferences
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfCoordinateReferences
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfCoordinateReferences
   */
  ListOfCoordinateReferences(unsigned int level      = SpatialExtension::getDefaultLevel(),
                             unsigned int version    = SpatialExtension::getDefaultVersion(),
                             unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCoordinateReferences with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfCoordinateReferences(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfCoordinateReferences object.
   *
   * @return a (deep) copy of this ListOfCoordinateReferences object.
   */
  virtual ListOfCoordinateReferences* clone () const;


   /**
   * Get a CoordinateReference from the ListOfCoordinateReferences.
   *
   * @param n the index number of the CoordinateReference to get.
   *
   * @return the nth CoordinateReference in this ListOfCoordinateReferences.
   *
   * @see size()
   */
	virtual CoordinateReference* get(unsigned int n);


  /**
   * Get a CoordinateReference from the ListOfCoordinateReferences.
   *
   * @param n the index number of the CoordinateReference to get.
   *
   * @return the nth CoordinateReference in this ListOfCoordinateReferences.
   *
   * @see size()
   */
	virtual const CoordinateReference* get(unsigned int n) const;


  /**
   * Get a CoordinateReference from the ListOfCoordinateReferences
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CoordinateReference to get.
   *
   * @return CoordinateReference in this ListOfCoordinateReferences
   * with the given id or NULL if no such
   * CoordinateReference exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual CoordinateReference* get(const std::string& sid);


  /**
   * Get a CoordinateReference from the ListOfCoordinateReferences
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CoordinateReference to get.
   *
   * @return CoordinateReference in this ListOfCoordinateReferences
   * with the given id or NULL if no such
   * CoordinateReference exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const CoordinateReference* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "CoordinateReference" to this ListOfCoordinateReferences.
	 *
	 * @param cr; the CoordinateReference object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addCoordinateReference(const CoordinateReference* cr);


	/**
	 * Get the number of CoordinateReference objects in this ListOfCoordinateReferences.
	 *
	 * @return the number of CoordinateReference objects in this ListOfCoordinateReferences
	 */
	unsigned int getNumCoordinateReferences() const;


	/**
	 * Creates a new CoordinateReference object, adds it to the
	 * ListOfCoordinateReferences and returns the CoordinateReference object created. 
	 *
	 * @return a new CoordinateReference object instance
	 *
	 * @see addCoordinateReference(const CoordinateReference* cr)
	 */
	CoordinateReference* createCoordinateReference();


  /**
   * Removes the nth CoordinateReference from this ListOfCoordinateReferences
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the CoordinateReference to remove.
   *
   * @see size()
   */
	virtual CoordinateReference* remove(unsigned int n);


  /**
   * Removes the CoordinateReference from this ListOfCoordinateReferences with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the CoordinateReference to remove.
   *
   * @return the CoordinateReference removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual CoordinateReference* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfCoordinateReferences, is
   * always @c "listOfCoordinateReferences".
   *
   * @return the name of this element, i.e. @c "listOfCoordinateReferences".
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
   * Creates a new CoordinateReference in this ListOfCoordinateReferences
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
 * Creates a new CoordinateReference_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CoordinateReference_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CoordinateReference_t structure.
 *
 * @returns the newly-created CoordinateReference_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
CoordinateReference_t *
CoordinateReference_create(unsigned int level, unsigned int version,
                           unsigned int pkgVersion);


/**
 * Frees the given CoordinateReference_t structure.
 * 
 * @param cr the CoordinateReference_t structure to be freed.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
void
CoordinateReference_free(CoordinateReference_t * cr);


/**
 * Creates a deep copy of the given CoordinateReference_t structure.
 * 
 * @param cr the CoordinateReference_t structure to be copied.
 *
 * @returns a (deep) copy of the given CoordinateReference_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
CoordinateReference_t *
CoordinateReference_clone(CoordinateReference_t * cr);


/**
 * Returns the value of the "coordinate" attribute of the given CoordinateReference_t
 * structure.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @return the coordinate of this structure.
 *
 * @member of CoordinateReference_t
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateReference_getCoordinate(const CoordinateReference_t * cr);


/**
 * Predicate returning @c 1 if the given CoordinateReference_t structure's "coordinate"
 * is set.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @return @c 1 if the "coordinate" of this CoordinateReference_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CoordinateReference_t
 */
LIBSBML_EXTERN
int
CoordinateReference_isSetCoordinate(const CoordinateReference_t * cr);


/**
 * Sets the "coordinate" attribute of the given CoordinateReference_t structure.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @param coordinate the string to which the structures "coordinate" attribute should be
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
 * @member of CoordinateReference_t
 */
LIBSBML_EXTERN
int
CoordinateReference_setCoordinate(CoordinateReference_t * cr, CoordinateKind_t coordinate);


/**
 * Unsets the value of the "coordinate" attribute of the given 
 *CoordinateReference_t structure.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CoordinateReference_t
 */
LIBSBML_EXTERN
int
CoordinateReference_unsetCoordinate(CoordinateReference_t * cr);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CoordinateReference_t structure have been set.
 *
 * @param cr the CoordinateReference_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CoordinateReference_t
 */
LIBSBML_EXTERN
int
CoordinateReference_hasRequiredAttributes(const CoordinateReference_t * cr);


LIBSBML_EXTERN
CoordinateReference_t *
ListOfCoordinateReferences_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
CoordinateReference_t *
ListOfCoordinateReferences_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CoordinateReference_H__  */

