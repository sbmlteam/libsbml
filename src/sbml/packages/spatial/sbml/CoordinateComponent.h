/**
 * @file:   CoordinateComponent.h
 * @brief:  Implementation of the CoordinateComponent class
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


#ifndef CoordinateComponent_H__
#define CoordinateComponent_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/packages/spatial/sbml/Boundary.h>
#include <sbml/packages/spatial/sbml/Boundary.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN CoordinateComponent : public SBase
{

protected:

  std::string   mId;
  CoordinateKind_t   mType;
  std::string   mUnit;
  Boundary*      mBoundaryMin;
  Boundary*      mBoundaryMax;


public:

  /**
   * Creates a new CoordinateComponent with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CoordinateComponent
   *
   * @param version an unsigned int, the SBML Version to assign to this CoordinateComponent
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CoordinateComponent
   */
  CoordinateComponent(unsigned int level      = SpatialExtension::getDefaultLevel(),
                      unsigned int version    = SpatialExtension::getDefaultVersion(),
                      unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CoordinateComponent with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CoordinateComponent(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CoordinateComponent.
   *
   * @param orig; the CoordinateComponent instance to copy.
   */
  CoordinateComponent(const CoordinateComponent& orig);


   /**
   * Assignment operator for CoordinateComponent.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CoordinateComponent& operator=(const CoordinateComponent& rhs);


   /**
   * Creates and returns a deep copy of this CoordinateComponent object.
   *
   * @return a (deep) copy of this CoordinateComponent object.
   */
  virtual CoordinateComponent* clone () const;


   /**
   * Destructor for CoordinateComponent.
   */
  virtual ~CoordinateComponent();


   /**
   * Returns the value of the "id" attribute of this CoordinateComponent.
   *
   * @return the value of the "id" attribute of this CoordinateComponent as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "type" attribute of this CoordinateComponent.
   *
   * @return the value of the "type" attribute of this CoordinateComponent as a CoordinateKind_t.
   */
  virtual CoordinateKind_t getType() const;


  /**
   * Returns the value of the "unit" attribute of this CoordinateComponent.
   *
   * @return the value of the "unit" attribute of this CoordinateComponent as a string.
   */
  virtual const std::string& getUnit() const;


  /**
   * Returns the "boundaryMin" element of this CoordinateComponent.
   *
   * @return the "boundaryMin" element of this CoordinateComponent.
   */
  virtual const Boundary* getBoundaryMin() const;


  /**
   * Returns the "boundaryMin" element of this CoordinateComponent.
   *
   * @return the "boundaryMin" element of this CoordinateComponent.
   */
  virtual Boundary* getBoundaryMin();


  /**
   * Creates a new "Boundary" and sets it for this CoordinateComponent.
   *
   * @return the created "Boundary" element of this CoordinateComponent.
   */
  virtual Boundary* createBoundaryMin();


  /**
   * Returns the "boundaryMax" element of this CoordinateComponent.
   *
   * @return the "boundaryMax" element of this CoordinateComponent.
   */
  virtual const Boundary* getBoundaryMax() const;


  /**
   * Returns the "boundaryMax" element of this CoordinateComponent.
   *
   * @return the "boundaryMax" element of this CoordinateComponent.
   */
  virtual Boundary* getBoundaryMax();


  /**
   * Creates a new "Boundary" and sets it for this CoordinateComponent.
   *
   * @return the created "Boundary" element of this CoordinateComponent.
   */
  virtual Boundary* createBoundaryMax();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "id" attribute has been set.
   *
   * @return @c true if this CoordinateComponent's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "type" attribute has been set.
   *
   * @return @c true if this CoordinateComponent's "type" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "unit" attribute has been set.
   *
   * @return @c true if this CoordinateComponent's "unit" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetUnit() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "boundaryMin" element has been set.
   *
   * @return @c true if this CoordinateComponent's "boundaryMin" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetBoundaryMin() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "boundaryMax" element has been set.
   *
   * @return @c true if this CoordinateComponent's "boundaryMax" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetBoundaryMax() const;


  /**
   * Sets the value of the "id" attribute of this CoordinateComponent.
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
   * Sets the value of the "type" attribute of this CoordinateComponent.
   *
   * @param type; CoordinateKind_t value of the "type" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setType(CoordinateKind_t type);


  /**
   * Sets the value of the "type" attribute of this CoordinateComponent.
   *
   * @param type; string value of the "type" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setType(const std::string& type);


  /**
   * Sets the value of the "unit" attribute of this CoordinateComponent.
   *
   * @param unit; const std::string& value of the "unit" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setUnit(const std::string& unit);


  /**
   * Sets the "boundaryMin" element of this CoordinateComponent.
   *
   * @param boundaryMin; Boundary* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setBoundaryMin(Boundary* boundaryMin);


  /**
   * Sets the "boundaryMax" element of this CoordinateComponent.
   *
   * @param boundaryMax; Boundary* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setBoundaryMax(Boundary* boundaryMax);


  /**
   * Unsets the value of the "id" attribute of this CoordinateComponent.
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
   * Unsets the value of the "type" attribute of this CoordinateComponent.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetType();


  /**
   * Unsets the value of the "unit" attribute of this CoordinateComponent.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetUnit();


  /**
   * Unsets the "boundaryMin" element of this CoordinateComponent.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetBoundaryMin();


  /**
   * Unsets the "boundaryMax" element of this CoordinateComponent.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetBoundaryMax();


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
   * Returns the XML element name of this object, which for CoordinateComponent, is
   * always @c "coordinateComponent".
   *
   * @return the name of this element, i.e. @c "coordinateComponent".
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
   * for this CoordinateComponent object have been set.
   *
   * @note The required attributes for a CoordinateComponent object are:
   * @li "id"
   * @li "type"
   * @li "unit"
   * @li "boundaryMin"
   * @li "boundaryMax"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this CoordinateComponent object have been set.
   *
   * @note The required elements for a CoordinateComponent object are:
   * @li "boundaryMin"
   * @li "boundaryMax"
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

class LIBSBML_EXTERN ListOfCoordinateComponents : public ListOf
{

public:

  /**
   * Creates a new ListOfCoordinateComponents with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfCoordinateComponents
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfCoordinateComponents
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfCoordinateComponents
   */
  ListOfCoordinateComponents(unsigned int level      = SpatialExtension::getDefaultLevel(),
                             unsigned int version    = SpatialExtension::getDefaultVersion(),
                             unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCoordinateComponents with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfCoordinateComponents(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfCoordinateComponents object.
   *
   * @return a (deep) copy of this ListOfCoordinateComponents object.
   */
  virtual ListOfCoordinateComponents* clone () const;


   /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents.
   *
   * @param n the index number of the CoordinateComponent to get.
   *
   * @return the nth CoordinateComponent in this ListOfCoordinateComponents.
   *
   * @see size()
   */
	virtual CoordinateComponent* get(unsigned int n);


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents.
   *
   * @param n the index number of the CoordinateComponent to get.
   *
   * @return the nth CoordinateComponent in this ListOfCoordinateComponents.
   *
   * @see size()
   */
	virtual const CoordinateComponent* get(unsigned int n) const;


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CoordinateComponent to get.
   *
   * @return CoordinateComponent in this ListOfCoordinateComponents
   * with the given id or NULL if no such
   * CoordinateComponent exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual CoordinateComponent* get(const std::string& sid);


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CoordinateComponent to get.
   *
   * @return CoordinateComponent in this ListOfCoordinateComponents
   * with the given id or NULL if no such
   * CoordinateComponent exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const CoordinateComponent* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "CoordinateComponent" to this ListOfCoordinateComponents.
	 *
	 * @param cc; the CoordinateComponent object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addCoordinateComponent(const CoordinateComponent* cc);


	/**
	 * Get the number of CoordinateComponent objects in this ListOfCoordinateComponents.
	 *
	 * @return the number of CoordinateComponent objects in this ListOfCoordinateComponents
	 */
	unsigned int getNumCoordinateComponents() const;


	/**
	 * Creates a new CoordinateComponent object, adds it to the
	 * ListOfCoordinateComponents and returns the CoordinateComponent object created. 
	 *
	 * @return a new CoordinateComponent object instance
	 *
	 * @see addCoordinateComponent(const CoordinateComponent* cc)
	 */
	CoordinateComponent* createCoordinateComponent();


  /**
   * Removes the nth CoordinateComponent from this ListOfCoordinateComponents
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the CoordinateComponent to remove.
   *
   * @see size()
   */
	virtual CoordinateComponent* remove(unsigned int n);


  /**
   * Removes the CoordinateComponent from this ListOfCoordinateComponents with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the CoordinateComponent to remove.
   *
   * @return the CoordinateComponent removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual CoordinateComponent* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfCoordinateComponents, is
   * always @c "listOfCoordinateComponents".
   *
   * @return the name of this element, i.e. @c "listOfCoordinateComponents".
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
   * Creates a new CoordinateComponent in this ListOfCoordinateComponents
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
 * Creates a new CoordinateComponent_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CoordinateComponent_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CoordinateComponent_t structure.
 *
 * @returns the newly-created CoordinateComponent_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
CoordinateComponent_t *
CoordinateComponent_create(unsigned int level, unsigned int version,
                           unsigned int pkgVersion);


/**
 * Frees the given CoordinateComponent_t structure.
 * 
 * @param cc the CoordinateComponent_t structure to be freed.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
void
CoordinateComponent_free(CoordinateComponent_t * cc);


/**
 * Creates a deep copy of the given CoordinateComponent_t structure.
 * 
 * @param cc the CoordinateComponent_t structure to be copied.
 *
 * @returns a (deep) copy of the given CoordinateComponent_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CoordinateComponent_t
 */
LIBSBML_EXTERN
CoordinateComponent_t *
CoordinateComponent_clone(CoordinateComponent_t * cc);


/**
 * Returns the value of the "id" attribute of the given CoordinateComponent_t
 * structure.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return the id of this structure.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
const char *
CoordinateComponent_getId(const CoordinateComponent_t * cc);


/**
 * Returns the value of the "type" attribute of the given CoordinateComponent_t
 * structure.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return the type of this structure.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateComponent_getType(const CoordinateComponent_t * cc);


/**
 * Returns the value of the "unit" attribute of the given CoordinateComponent_t
 * structure.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return the unit of this structure.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
const char *
CoordinateComponent_getUnit(const CoordinateComponent_t * cc);


LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_getBoundaryMin(CoordinateComponent_t * cc);


LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_createBoundaryMin(CoordinateComponent_t * cc);


LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_getBoundaryMax(CoordinateComponent_t * cc);


LIBSBML_EXTERN
Boundary_t*
CoordinateComponent_createBoundaryMax(CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 if the given CoordinateComponent_t structure's "id"
 * is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 if the "id" of this CoordinateComponent_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetId(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 if the given CoordinateComponent_t structure's "type"
 * is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 if the "type" of this CoordinateComponent_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetType(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 if the given CoordinateComponent_t structure's "unit"
 * is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 if the "unit" of this CoordinateComponent_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetUnit(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 if the given CoordinateComponent_t structure's "boundaryMin"
 * is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 if the "boundaryMin" of this CoordinateComponent_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetBoundaryMin(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 if the given CoordinateComponent_t structure's "boundaryMax"
 * is set.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return @c 1 if the "boundaryMax" of this CoordinateComponent_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_isSetBoundaryMax(const CoordinateComponent_t * cc);


/**
 * Sets the "id" attribute of the given CoordinateComponent_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CoordinateComponent_unsetId() instead.
 *
 * @param cc the CoordinateComponent_t structure.
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
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setId(CoordinateComponent_t * cc, const char * id);


/**
 * Sets the "type" attribute of the given CoordinateComponent_t structure.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @param type the string to which the structures "type" attribute should be
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
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setType(CoordinateComponent_t * cc, CoordinateKind_t type);


/**
 * Sets the "unit" attribute of the given CoordinateComponent_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CoordinateComponent_unsetUnit() instead.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @param unit the string to which the structures "unit" attribute should be
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
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_setUnit(CoordinateComponent_t * cc, const char * unit);


LIBSBML_EXTERN
int
CoordinateComponent_setBoundaryMin(CoordinateComponent_t * cc, Boundary_t* boundaryMin);


LIBSBML_EXTERN
int
CoordinateComponent_setBoundaryMax(CoordinateComponent_t * cc, Boundary_t* boundaryMax);


/**
 * Unsets the value of the "id" attribute of the given 
 *CoordinateComponent_t structure.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_unsetId(CoordinateComponent_t * cc);


/**
 * Unsets the value of the "type" attribute of the given 
 *CoordinateComponent_t structure.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_unsetType(CoordinateComponent_t * cc);


/**
 * Unsets the value of the "unit" attribute of the given 
 *CoordinateComponent_t structure.
 *
 * @param cc the CoordinateComponent_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_unsetUnit(CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CoordinateComponent_t structure have been set.
 *
 * @param cc the CoordinateComponent_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_hasRequiredAttributes(const CoordinateComponent_t * cc);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given CoordinateComponent_t structure have been set.
 *
 * @param cc the CoordinateComponent_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CoordinateComponent_t
 */
LIBSBML_EXTERN
int
CoordinateComponent_hasRequiredElements(const CoordinateComponent_t * cc);


LIBSBML_EXTERN
CoordinateComponent_t *
ListOfCoordinateComponents_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
CoordinateComponent_t *
ListOfCoordinateComponents_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CoordinateComponent_H__  */

