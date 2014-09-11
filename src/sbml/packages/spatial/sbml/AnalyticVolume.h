/**
 * @file:   AnalyticVolume.h
 * @brief:  Implementation of the AnalyticVolume class
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


#ifndef AnalyticVolume_H__
#define AnalyticVolume_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN AnalyticVolume : public SBase
{

protected:

  std::string   mId;
  FunctionKind_t   mFunctionType;
  int           mOrdinal;
  bool          mIsSetOrdinal;
  std::string   mDomainType;
  ASTNode*      mMath;


public:

  /**
   * Creates a new AnalyticVolume with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this AnalyticVolume
   *
   * @param version an unsigned int, the SBML Version to assign to this AnalyticVolume
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this AnalyticVolume
   */
  AnalyticVolume(unsigned int level      = SpatialExtension::getDefaultLevel(),
                 unsigned int version    = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AnalyticVolume with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  AnalyticVolume(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for AnalyticVolume.
   *
   * @param orig; the AnalyticVolume instance to copy.
   */
  AnalyticVolume(const AnalyticVolume& orig);


   /**
   * Assignment operator for AnalyticVolume.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  AnalyticVolume& operator=(const AnalyticVolume& rhs);


   /**
   * Creates and returns a deep copy of this AnalyticVolume object.
   *
   * @return a (deep) copy of this AnalyticVolume object.
   */
  virtual AnalyticVolume* clone () const;


   /**
   * Destructor for AnalyticVolume.
   */
  virtual ~AnalyticVolume();


   /**
   * Returns the value of the "id" attribute of this AnalyticVolume.
   *
   * @return the value of the "id" attribute of this AnalyticVolume as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @return the value of the "functionType" attribute of this AnalyticVolume as a FunctionKind_t.
   */
  virtual FunctionKind_t getFunctionType() const;


  /**
   * Returns the value of the "ordinal" attribute of this AnalyticVolume.
   *
   * @return the value of the "ordinal" attribute of this AnalyticVolume as a integer.
   */
  virtual int getOrdinal() const;


  /**
   * Returns the value of the "domainType" attribute of this AnalyticVolume.
   *
   * @return the value of the "domainType" attribute of this AnalyticVolume as a string.
   */
  virtual const std::string& getDomainType() const;


  /**
   * Returns the "math" element of this AnalyticVolume.
   *
   * @return the "math" element of this AnalyticVolume.
   */
  virtual const ASTNode* getMath() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "id" attribute has been set.
   *
   * @return @c true if this AnalyticVolume's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "functionType" attribute has been set.
   *
   * @return @c true if this AnalyticVolume's "functionType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetFunctionType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "ordinal" attribute has been set.
   *
   * @return @c true if this AnalyticVolume's "ordinal" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetOrdinal() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "domainType" attribute has been set.
   *
   * @return @c true if this AnalyticVolume's "domainType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDomainType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "math" element has been set.
   *
   * @return @c true if this AnalyticVolume's "math" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetMath() const;


  /**
   * Sets the value of the "id" attribute of this AnalyticVolume.
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
   * Sets the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @param functionType; FunctionKind_t value of the "functionType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setFunctionType(FunctionKind_t functionType);


  /**
   * Sets the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @param functionType; string value of the "functionType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setFunctionType(const std::string& functionType);


  /**
   * Sets the value of the "ordinal" attribute of this AnalyticVolume.
   *
   * @param ordinal; int value of the "ordinal" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setOrdinal(int ordinal);


  /**
   * Sets the value of the "domainType" attribute of this AnalyticVolume.
   *
   * @param domainType; const std::string& value of the "domainType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDomainType(const std::string& domainType);


  /**
   * Sets the "math" element of this AnalyticVolume.
   *
   * @param math; const ASTNode* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setMath(const ASTNode* math);


  /**
   * Unsets the value of the "id" attribute of this AnalyticVolume.
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
   * Unsets the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetFunctionType();


  /**
   * Unsets the value of the "ordinal" attribute of this AnalyticVolume.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetOrdinal();


  /**
   * Unsets the value of the "domainType" attribute of this AnalyticVolume.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomainType();


  /**
   * Unsets the "math" element of this AnalyticVolume.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetMath();


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
   * Returns the XML element name of this object, which for AnalyticVolume, is
   * always @c "analyticVolume".
   *
   * @return the name of this element, i.e. @c "analyticVolume".
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
   * for this AnalyticVolume object have been set.
   *
   * @note The required attributes for a AnalyticVolume object are:
   * @li "id"
   * @li "functionType"
   * @li "ordinal"
   * @li "domainType"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this AnalyticVolume object have been set.
   *
   * @note The required elements for a AnalyticVolume object are:
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
   * Subclasses should override this method ro read other XML.
   *
   * return true if read from stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};

class LIBSBML_EXTERN ListOfAnalyticVolumes : public ListOf
{

public:

  /**
   * Creates a new ListOfAnalyticVolumes with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfAnalyticVolumes
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfAnalyticVolumes
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfAnalyticVolumes
   */
  ListOfAnalyticVolumes(unsigned int level      = SpatialExtension::getDefaultLevel(),
                        unsigned int version    = SpatialExtension::getDefaultVersion(),
                        unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfAnalyticVolumes with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfAnalyticVolumes(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfAnalyticVolumes object.
   *
   * @return a (deep) copy of this ListOfAnalyticVolumes object.
   */
  virtual ListOfAnalyticVolumes* clone () const;


   /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes.
   *
   * @param n the index number of the AnalyticVolume to get.
   *
   * @return the nth AnalyticVolume in this ListOfAnalyticVolumes.
   *
   * @see size()
   */
	virtual AnalyticVolume* get(unsigned int n);


  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes.
   *
   * @param n the index number of the AnalyticVolume to get.
   *
   * @return the nth AnalyticVolume in this ListOfAnalyticVolumes.
   *
   * @see size()
   */
	virtual const AnalyticVolume* get(unsigned int n) const;


  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the AnalyticVolume to get.
   *
   * @return AnalyticVolume in this ListOfAnalyticVolumes
   * with the given id or NULL if no such
   * AnalyticVolume exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual AnalyticVolume* get(const std::string& sid);


  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the AnalyticVolume to get.
   *
   * @return AnalyticVolume in this ListOfAnalyticVolumes
   * with the given id or NULL if no such
   * AnalyticVolume exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const AnalyticVolume* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "AnalyticVolume" to this ListOfAnalyticVolumes.
	 *
	 * @param av; the AnalyticVolume object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addAnalyticVolume(const AnalyticVolume* av);


	/**
	 * Get the number of AnalyticVolume objects in this ListOfAnalyticVolumes.
	 *
	 * @return the number of AnalyticVolume objects in this ListOfAnalyticVolumes
	 */
	unsigned int getNumAnalyticVolumes() const;


	/**
	 * Creates a new AnalyticVolume object, adds it to the
	 * ListOfAnalyticVolumes and returns the AnalyticVolume object created. 
	 *
	 * @return a new AnalyticVolume object instance
	 *
	 * @see addAnalyticVolume(const AnalyticVolume* av)
	 */
	AnalyticVolume* createAnalyticVolume();


  /**
   * Removes the nth AnalyticVolume from this ListOfAnalyticVolumes
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the AnalyticVolume to remove.
   *
   * @see size()
   */
	virtual AnalyticVolume* remove(unsigned int n);


  /**
   * Removes the AnalyticVolume from this ListOfAnalyticVolumes with the given identifier
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
	virtual AnalyticVolume* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfAnalyticVolumes, is
   * always @c "listOfAnalyticVolumes".
   *
   * @return the name of this element, i.e. @c "listOfAnalyticVolumes".
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
   * Creates a new AnalyticVolume in this ListOfAnalyticVolumes
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
 * Creates a new AnalyticVolume_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * AnalyticVolume_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * AnalyticVolume_t structure.
 *
 * @returns the newly-created AnalyticVolume_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticVolume_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion);


/**
 * Frees the given AnalyticVolume_t structure.
 * 
 * @param av the AnalyticVolume_t structure to be freed.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
void
AnalyticVolume_free(AnalyticVolume_t * av);


/**
 * Creates a deep copy of the given AnalyticVolume_t structure.
 * 
 * @param av the AnalyticVolume_t structure to be copied.
 *
 * @returns a (deep) copy of the given AnalyticVolume_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof AnalyticVolume_t
 */
LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticVolume_clone(AnalyticVolume_t * av);


/**
 * Returns the value of the "id" attribute of the given AnalyticVolume_t
 * structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return the id of this structure.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
const char *
AnalyticVolume_getId(const AnalyticVolume_t * av);


/**
 * Returns the value of the "functionType" attribute of the given AnalyticVolume_t
 * structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return the functionType of this structure.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
FunctionKind_t
AnalyticVolume_getFunctionType(const AnalyticVolume_t * av);


/**
 * Returns the value of the "ordinal" attribute of the given AnalyticVolume_t
 * structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return the ordinal of this structure.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_getOrdinal(const AnalyticVolume_t * av);


/**
 * Returns the value of the "domainType" attribute of the given AnalyticVolume_t
 * structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return the domainType of this structure.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
const char *
AnalyticVolume_getDomainType(const AnalyticVolume_t * av);


/**
 * Gets the mathematical expression of this AnalyticVolume_t structure as an
 * ASTNode_t structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return the math for this AnalyticVolume_t, as an ASTNode_t.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
const ASTNode_t*
AnalyticVolume_getMath(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 if the given AnalyticVolume_t structure's "id"
 * is set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 if the "id" of this AnalyticVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetId(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 if the given AnalyticVolume_t structure's "functionType"
 * is set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 if the "functionType" of this AnalyticVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetFunctionType(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 if the given AnalyticVolume_t structure's "ordinal"
 * is set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 if the "ordinal" of this AnalyticVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetOrdinal(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 if the given AnalyticVolume_t structure's "domainType"
 * is set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 if the "domainType" of this AnalyticVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetDomainType(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 if the given AnalyticVolume_t structure's "math"
 * is set.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return @c 1 if the "math" of this AnalyticVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_isSetMath(const AnalyticVolume_t * av);


/**
 * Sets the "id" attribute of the given AnalyticVolume_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs AnalyticVolume_unsetId() instead.
 *
 * @param av the AnalyticVolume_t structure.
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
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setId(AnalyticVolume_t * av, const char * id);


/**
 * Sets the "functionType" attribute of the given AnalyticVolume_t structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param functionType the string to which the structures "functionType" attribute should be
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
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setFunctionType(AnalyticVolume_t * av, FunctionKind_t functionType);


/**
 * Sets the "ordinal" attribute of the given AnalyticVolume_t structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param ordinal the string to which the structures "ordinal" attribute should be
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
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setOrdinal(AnalyticVolume_t * av, int ordinal);


/**
 * Sets the "domainType" attribute of the given AnalyticVolume_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs AnalyticVolume_unsetDomainType() instead.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param domainType the string to which the structures "domainType" attribute should be
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
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setDomainType(AnalyticVolume_t * av, const char * domainType);


/**
 * Sets the mathematical expression of the given AnalyticVolume_t structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @param math an ASTNode_t structure to be assigned as the "math"
 * subelement of this AnalyticVolume_t.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_setMath(AnalyticVolume_t * av, const ASTNode_t* math);


/**
 * Unsets the value of the "id" attribute of the given 
 *AnalyticVolume_t structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetId(AnalyticVolume_t * av);


/**
 * Unsets the value of the "functionType" attribute of the given 
 *AnalyticVolume_t structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetFunctionType(AnalyticVolume_t * av);


/**
 * Unsets the value of the "ordinal" attribute of the given 
 *AnalyticVolume_t structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetOrdinal(AnalyticVolume_t * av);


/**
 * Unsets the value of the "domainType" attribute of the given 
 *AnalyticVolume_t structure.
 *
 * @param av the AnalyticVolume_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_unsetDomainType(AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given AnalyticVolume_t structure have been set.
 *
 * @param av the AnalyticVolume_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_hasRequiredAttributes(const AnalyticVolume_t * av);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given AnalyticVolume_t structure have been set.
 *
 * @param av the AnalyticVolume_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of AnalyticVolume_t
 */
LIBSBML_EXTERN
int
AnalyticVolume_hasRequiredElements(const AnalyticVolume_t * av);


LIBSBML_EXTERN
AnalyticVolume_t *
ListOfAnalyticVolumes_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
AnalyticVolume_t *
ListOfAnalyticVolumes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  AnalyticVolume_H__  */

