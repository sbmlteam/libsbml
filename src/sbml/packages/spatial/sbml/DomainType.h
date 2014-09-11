/**
 * @file:   DomainType.h
 * @brief:  Implementation of the DomainType class
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


#ifndef DomainType_H__
#define DomainType_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN DomainType : public SBase
{

protected:

  std::string   mId;
  int           mSpatialDimensions;
  bool          mIsSetSpatialDimensions;


public:

  /**
   * Creates a new DomainType with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this DomainType
   *
   * @param version an unsigned int, the SBML Version to assign to this DomainType
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this DomainType
   */
  DomainType(unsigned int level      = SpatialExtension::getDefaultLevel(),
             unsigned int version    = SpatialExtension::getDefaultVersion(),
             unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new DomainType with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  DomainType(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for DomainType.
   *
   * @param orig; the DomainType instance to copy.
   */
  DomainType(const DomainType& orig);


   /**
   * Assignment operator for DomainType.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  DomainType& operator=(const DomainType& rhs);


   /**
   * Creates and returns a deep copy of this DomainType object.
   *
   * @return a (deep) copy of this DomainType object.
   */
  virtual DomainType* clone () const;


   /**
   * Destructor for DomainType.
   */
  virtual ~DomainType();


   /**
   * Returns the value of the "id" attribute of this DomainType.
   *
   * @return the value of the "id" attribute of this DomainType as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "spatialDimensions" attribute of this DomainType.
   *
   * @return the value of the "spatialDimensions" attribute of this DomainType as a integer.
   */
  virtual int getSpatialDimensions() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * DomainType's "id" attribute has been set.
   *
   * @return @c true if this DomainType's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * DomainType's "spatialDimensions" attribute has been set.
   *
   * @return @c true if this DomainType's "spatialDimensions" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialDimensions() const;


  /**
   * Sets the value of the "id" attribute of this DomainType.
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
   * Sets the value of the "spatialDimensions" attribute of this DomainType.
   *
   * @param spatialDimensions; int value of the "spatialDimensions" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSpatialDimensions(int spatialDimensions);


  /**
   * Unsets the value of the "id" attribute of this DomainType.
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
   * Unsets the value of the "spatialDimensions" attribute of this DomainType.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSpatialDimensions();


  /**
   * Returns the XML element name of this object, which for DomainType, is
   * always @c "domainType".
   *
   * @return the name of this element, i.e. @c "domainType".
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
   * for this DomainType object have been set.
   *
   * @note The required attributes for a DomainType object are:
   * @li "id"
   * @li "spatialDimensions"
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

class LIBSBML_EXTERN ListOfDomainTypes : public ListOf
{

public:

  /**
   * Creates a new ListOfDomainTypes with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfDomainTypes
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfDomainTypes
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfDomainTypes
   */
  ListOfDomainTypes(unsigned int level      = SpatialExtension::getDefaultLevel(),
                    unsigned int version    = SpatialExtension::getDefaultVersion(),
                    unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDomainTypes with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfDomainTypes(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfDomainTypes object.
   *
   * @return a (deep) copy of this ListOfDomainTypes object.
   */
  virtual ListOfDomainTypes* clone () const;


   /**
   * Get a DomainType from the ListOfDomainTypes.
   *
   * @param n the index number of the DomainType to get.
   *
   * @return the nth DomainType in this ListOfDomainTypes.
   *
   * @see size()
   */
	virtual DomainType* get(unsigned int n);


  /**
   * Get a DomainType from the ListOfDomainTypes.
   *
   * @param n the index number of the DomainType to get.
   *
   * @return the nth DomainType in this ListOfDomainTypes.
   *
   * @see size()
   */
	virtual const DomainType* get(unsigned int n) const;


  /**
   * Get a DomainType from the ListOfDomainTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the DomainType to get.
   *
   * @return DomainType in this ListOfDomainTypes
   * with the given id or NULL if no such
   * DomainType exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual DomainType* get(const std::string& sid);


  /**
   * Get a DomainType from the ListOfDomainTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the DomainType to get.
   *
   * @return DomainType in this ListOfDomainTypes
   * with the given id or NULL if no such
   * DomainType exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const DomainType* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "DomainType" to this ListOfDomainTypes.
	 *
	 * @param dt; the DomainType object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addDomainType(const DomainType* dt);


	/**
	 * Get the number of DomainType objects in this ListOfDomainTypes.
	 *
	 * @return the number of DomainType objects in this ListOfDomainTypes
	 */
	unsigned int getNumDomainTypes() const;


	/**
	 * Creates a new DomainType object, adds it to the
	 * ListOfDomainTypes and returns the DomainType object created. 
	 *
	 * @return a new DomainType object instance
	 *
	 * @see addDomainType(const DomainType* dt)
	 */
	DomainType* createDomainType();


  /**
   * Removes the nth DomainType from this ListOfDomainTypes
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the DomainType to remove.
   *
   * @see size()
   */
	virtual DomainType* remove(unsigned int n);


  /**
   * Removes the DomainType from this ListOfDomainTypes with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the DomainType to remove.
   *
   * @return the DomainType removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual DomainType* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfDomainTypes, is
   * always @c "listOfDomainTypes".
   *
   * @return the name of this element, i.e. @c "listOfDomainTypes".
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
   * Creates a new DomainType in this ListOfDomainTypes
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
 * Creates a new DomainType_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * DomainType_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * DomainType_t structure.
 *
 * @returns the newly-created DomainType_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
DomainType_t *
DomainType_create(unsigned int level, unsigned int version,
                  unsigned int pkgVersion);


/**
 * Frees the given DomainType_t structure.
 * 
 * @param dt the DomainType_t structure to be freed.
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
void
DomainType_free(DomainType_t * dt);


/**
 * Creates a deep copy of the given DomainType_t structure.
 * 
 * @param dt the DomainType_t structure to be copied.
 *
 * @returns a (deep) copy of the given DomainType_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
DomainType_t *
DomainType_clone(DomainType_t * dt);


/**
 * Returns the value of the "id" attribute of the given DomainType_t
 * structure.
 *
 * @param dt the DomainType_t structure.
 *
 * @return the id of this structure.
 *
 * @member of DomainType_t
 */
LIBSBML_EXTERN
const char *
DomainType_getId(const DomainType_t * dt);


/**
 * Returns the value of the "spatialDimensions" attribute of the given DomainType_t
 * structure.
 *
 * @param dt the DomainType_t structure.
 *
 * @return the spatialDimensions of this structure.
 *
 * @member of DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_getSpatialDimensions(const DomainType_t * dt);


/**
 * Predicate returning @c 1 if the given DomainType_t structure's "id"
 * is set.
 *
 * @param dt the DomainType_t structure.
 *
 * @return @c 1 if the "id" of this DomainType_t structure is
 * set, @c 0 otherwise.
 *
 * @member of DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_isSetId(const DomainType_t * dt);


/**
 * Predicate returning @c 1 if the given DomainType_t structure's "spatialDimensions"
 * is set.
 *
 * @param dt the DomainType_t structure.
 *
 * @return @c 1 if the "spatialDimensions" of this DomainType_t structure is
 * set, @c 0 otherwise.
 *
 * @member of DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_isSetSpatialDimensions(const DomainType_t * dt);


/**
 * Sets the "id" attribute of the given DomainType_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs DomainType_unsetId() instead.
 *
 * @param dt the DomainType_t structure.
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
 * @member of DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_setId(DomainType_t * dt, const char * id);


/**
 * Sets the "spatialDimensions" attribute of the given DomainType_t structure.
 *
 * @param dt the DomainType_t structure.
 *
 * @param spatialDimensions the string to which the structures "spatialDimensions" attribute should be
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
 * @member of DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_setSpatialDimensions(DomainType_t * dt, int spatialDimensions);


/**
 * Unsets the value of the "id" attribute of the given 
 *DomainType_t structure.
 *
 * @param dt the DomainType_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_unsetId(DomainType_t * dt);


/**
 * Unsets the value of the "spatialDimensions" attribute of the given 
 *DomainType_t structure.
 *
 * @param dt the DomainType_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_unsetSpatialDimensions(DomainType_t * dt);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given DomainType_t structure have been set.
 *
 * @param dt the DomainType_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_hasRequiredAttributes(const DomainType_t * dt);


LIBSBML_EXTERN
DomainType_t *
ListOfDomainTypes_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
DomainType_t *
ListOfDomainTypes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  DomainType_H__  */

