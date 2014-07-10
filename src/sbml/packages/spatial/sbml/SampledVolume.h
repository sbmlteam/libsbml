/**
 * @file:   SampledVolume.h
 * @brief:  Implementation of the SampledVolume class
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


#ifndef SampledVolume_H__
#define SampledVolume_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN SampledVolume : public SBase
{

protected:

  std::string   mId;
  std::string   mDomainType;
  double        mSampledValue;
  bool          mIsSetSampledValue;
  double        mMinValue;
  bool          mIsSetMinValue;
  double        mMaxValue;
  bool          mIsSetMaxValue;


public:

  /**
   * Creates a new SampledVolume with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SampledVolume
   *
   * @param version an unsigned int, the SBML Version to assign to this SampledVolume
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this SampledVolume
   */
  SampledVolume(unsigned int level      = SpatialExtension::getDefaultLevel(),
                unsigned int version    = SpatialExtension::getDefaultVersion(),
                unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SampledVolume with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  SampledVolume(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for SampledVolume.
   *
   * @param orig; the SampledVolume instance to copy.
   */
  SampledVolume(const SampledVolume& orig);


   /**
   * Assignment operator for SampledVolume.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SampledVolume& operator=(const SampledVolume& rhs);


   /**
   * Creates and returns a deep copy of this SampledVolume object.
   *
   * @return a (deep) copy of this SampledVolume object.
   */
  virtual SampledVolume* clone () const;


   /**
   * Destructor for SampledVolume.
   */
  virtual ~SampledVolume();


   /**
   * Returns the value of the "id" attribute of this SampledVolume.
   *
   * @return the value of the "id" attribute of this SampledVolume as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "domainType" attribute of this SampledVolume.
   *
   * @return the value of the "domainType" attribute of this SampledVolume as a string.
   */
  virtual const std::string& getDomainType() const;


  /**
   * Returns the value of the "sampledValue" attribute of this SampledVolume.
   *
   * @return the value of the "sampledValue" attribute of this SampledVolume as a double.
   */
  virtual double getSampledValue() const;


  /**
   * Returns the value of the "minValue" attribute of this SampledVolume.
   *
   * @return the value of the "minValue" attribute of this SampledVolume as a double.
   */
  virtual double getMinValue() const;


  /**
   * Returns the value of the "maxValue" attribute of this SampledVolume.
   *
   * @return the value of the "maxValue" attribute of this SampledVolume as a double.
   */
  virtual double getMaxValue() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "id" attribute has been set.
   *
   * @return @c true if this SampledVolume's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "domainType" attribute has been set.
   *
   * @return @c true if this SampledVolume's "domainType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDomainType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "sampledValue" attribute has been set.
   *
   * @return @c true if this SampledVolume's "sampledValue" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSampledValue() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "minValue" attribute has been set.
   *
   * @return @c true if this SampledVolume's "minValue" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetMinValue() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "maxValue" attribute has been set.
   *
   * @return @c true if this SampledVolume's "maxValue" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetMaxValue() const;


  /**
   * Sets the value of the "id" attribute of this SampledVolume.
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
   * Sets the value of the "domainType" attribute of this SampledVolume.
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
   * Sets the value of the "sampledValue" attribute of this SampledVolume.
   *
   * @param sampledValue; double value of the "sampledValue" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSampledValue(double sampledValue);


  /**
   * Sets the value of the "minValue" attribute of this SampledVolume.
   *
   * @param minValue; double value of the "minValue" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setMinValue(double minValue);


  /**
   * Sets the value of the "maxValue" attribute of this SampledVolume.
   *
   * @param maxValue; double value of the "maxValue" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setMaxValue(double maxValue);


  /**
   * Unsets the value of the "id" attribute of this SampledVolume.
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
   * Unsets the value of the "domainType" attribute of this SampledVolume.
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
   * Unsets the value of the "sampledValue" attribute of this SampledVolume.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSampledValue();


  /**
   * Unsets the value of the "minValue" attribute of this SampledVolume.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetMinValue();


  /**
   * Unsets the value of the "maxValue" attribute of this SampledVolume.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetMaxValue();


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
   * Returns the XML element name of this object, which for SampledVolume, is
   * always @c "sampledVolume".
   *
   * @return the name of this element, i.e. @c "sampledVolume".
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
   * for this SampledVolume object have been set.
   *
   * @note The required attributes for a SampledVolume object are:
   * @li "id"
   * @li "domainType"
   * @li "sampledValue"
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

class LIBSBML_EXTERN ListOfSampledVolumes : public ListOf
{

public:

  /**
   * Creates a new ListOfSampledVolumes with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfSampledVolumes
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfSampledVolumes
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfSampledVolumes
   */
  ListOfSampledVolumes(unsigned int level      = SpatialExtension::getDefaultLevel(),
                       unsigned int version    = SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSampledVolumes with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfSampledVolumes(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfSampledVolumes object.
   *
   * @return a (deep) copy of this ListOfSampledVolumes object.
   */
  virtual ListOfSampledVolumes* clone () const;


   /**
   * Get a SampledVolume from the ListOfSampledVolumes.
   *
   * @param n the index number of the SampledVolume to get.
   *
   * @return the nth SampledVolume in this ListOfSampledVolumes.
   *
   * @see size()
   */
	virtual SampledVolume* get(unsigned int n);


  /**
   * Get a SampledVolume from the ListOfSampledVolumes.
   *
   * @param n the index number of the SampledVolume to get.
   *
   * @return the nth SampledVolume in this ListOfSampledVolumes.
   *
   * @see size()
   */
	virtual const SampledVolume* get(unsigned int n) const;


  /**
   * Get a SampledVolume from the ListOfSampledVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SampledVolume to get.
   *
   * @return SampledVolume in this ListOfSampledVolumes
   * with the given id or NULL if no such
   * SampledVolume exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual SampledVolume* get(const std::string& sid);


  /**
   * Get a SampledVolume from the ListOfSampledVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SampledVolume to get.
   *
   * @return SampledVolume in this ListOfSampledVolumes
   * with the given id or NULL if no such
   * SampledVolume exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SampledVolume* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "SampledVolume" to this ListOfSampledVolumes.
	 *
	 * @param sv; the SampledVolume object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addSampledVolume(const SampledVolume* sv);


	/**
	 * Get the number of SampledVolume objects in this ListOfSampledVolumes.
	 *
	 * @return the number of SampledVolume objects in this ListOfSampledVolumes
	 */
	unsigned int getNumSampledVolumes() const;


	/**
	 * Creates a new SampledVolume object, adds it to the
	 * ListOfSampledVolumes and returns the SampledVolume object created. 
	 *
	 * @return a new SampledVolume object instance
	 *
	 * @see addSampledVolume(const SampledVolume* sv)
	 */
	SampledVolume* createSampledVolume();


  /**
   * Removes the nth SampledVolume from this ListOfSampledVolumes
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SampledVolume to remove.
   *
   * @see size()
   */
	virtual SampledVolume* remove(unsigned int n);


  /**
   * Removes the SampledVolume from this ListOfSampledVolumes with the given identifier
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
	virtual SampledVolume* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfSampledVolumes, is
   * always @c "listOfSampledVolumes".
   *
   * @return the name of this element, i.e. @c "listOfSampledVolumes".
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
   * Creates a new SampledVolume in this ListOfSampledVolumes
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
 * Creates a new SampledVolume_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * SampledVolume_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * SampledVolume_t structure.
 *
 * @returns the newly-created SampledVolume_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
SampledVolume_t *
SampledVolume_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion);


/**
 * Frees the given SampledVolume_t structure.
 * 
 * @param sv the SampledVolume_t structure to be freed.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
void
SampledVolume_free(SampledVolume_t * sv);


/**
 * Creates a deep copy of the given SampledVolume_t structure.
 * 
 * @param sv the SampledVolume_t structure to be copied.
 *
 * @returns a (deep) copy of the given SampledVolume_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof SampledVolume_t
 */
LIBSBML_EXTERN
SampledVolume_t *
SampledVolume_clone(SampledVolume_t * sv);


/**
 * Returns the value of the "id" attribute of the given SampledVolume_t
 * structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return the id of this structure.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
const char *
SampledVolume_getId(const SampledVolume_t * sv);


/**
 * Returns the value of the "domainType" attribute of the given SampledVolume_t
 * structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return the domainType of this structure.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
const char *
SampledVolume_getDomainType(const SampledVolume_t * sv);


/**
 * Returns the value of the "sampledValue" attribute of the given SampledVolume_t
 * structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return the sampledValue of this structure.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
double
SampledVolume_getSampledValue(const SampledVolume_t * sv);


/**
 * Returns the value of the "minValue" attribute of the given SampledVolume_t
 * structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return the minValue of this structure.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
double
SampledVolume_getMinValue(const SampledVolume_t * sv);


/**
 * Returns the value of the "maxValue" attribute of the given SampledVolume_t
 * structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return the maxValue of this structure.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
double
SampledVolume_getMaxValue(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 if the given SampledVolume_t structure's "id"
 * is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 if the "id" of this SampledVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetId(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 if the given SampledVolume_t structure's "domainType"
 * is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 if the "domainType" of this SampledVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetDomainType(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 if the given SampledVolume_t structure's "sampledValue"
 * is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 if the "sampledValue" of this SampledVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetSampledValue(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 if the given SampledVolume_t structure's "minValue"
 * is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 if the "minValue" of this SampledVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetMinValue(const SampledVolume_t * sv);


/**
 * Predicate returning @c 1 if the given SampledVolume_t structure's "maxValue"
 * is set.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return @c 1 if the "maxValue" of this SampledVolume_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_isSetMaxValue(const SampledVolume_t * sv);


/**
 * Sets the "id" attribute of the given SampledVolume_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SampledVolume_unsetId() instead.
 *
 * @param sv the SampledVolume_t structure.
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
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setId(SampledVolume_t * sv, const char * id);


/**
 * Sets the "domainType" attribute of the given SampledVolume_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SampledVolume_unsetDomainType() instead.
 *
 * @param sv the SampledVolume_t structure.
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
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setDomainType(SampledVolume_t * sv, const char * domainType);


/**
 * Sets the "sampledValue" attribute of the given SampledVolume_t structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @param sampledValue the string to which the structures "sampledValue" attribute should be
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
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setSampledValue(SampledVolume_t * sv, double sampledValue);


/**
 * Sets the "minValue" attribute of the given SampledVolume_t structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @param minValue the string to which the structures "minValue" attribute should be
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
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setMinValue(SampledVolume_t * sv, double minValue);


/**
 * Sets the "maxValue" attribute of the given SampledVolume_t structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @param maxValue the string to which the structures "maxValue" attribute should be
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
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_setMaxValue(SampledVolume_t * sv, double maxValue);


/**
 * Unsets the value of the "id" attribute of the given 
 *SampledVolume_t structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetId(SampledVolume_t * sv);


/**
 * Unsets the value of the "domainType" attribute of the given 
 *SampledVolume_t structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetDomainType(SampledVolume_t * sv);


/**
 * Unsets the value of the "sampledValue" attribute of the given 
 *SampledVolume_t structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetSampledValue(SampledVolume_t * sv);


/**
 * Unsets the value of the "minValue" attribute of the given 
 *SampledVolume_t structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetMinValue(SampledVolume_t * sv);


/**
 * Unsets the value of the "maxValue" attribute of the given 
 *SampledVolume_t structure.
 *
 * @param sv the SampledVolume_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_unsetMaxValue(SampledVolume_t * sv);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given SampledVolume_t structure have been set.
 *
 * @param sv the SampledVolume_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of SampledVolume_t
 */
LIBSBML_EXTERN
int
SampledVolume_hasRequiredAttributes(const SampledVolume_t * sv);


LIBSBML_EXTERN
SampledVolume_t *
ListOfSampledVolumes_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
SampledVolume_t *
ListOfSampledVolumes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SampledVolume_H__  */

