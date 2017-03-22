/**
 * @file:   SpeciesFeatureType.h
 * @brief:  Implementation of the SpeciesFeatureType class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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


#ifndef SpeciesFeatureType_H__
#define SpeciesFeatureType_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>

#include <sbml/packages/multi/sbml/PossibleSpeciesFeatureValue.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpeciesFeatureType : public SBase
{

protected:

////  std::string   mId;
////  std::string   mName;
  unsigned int  mOccur;
  bool          mIsSetOccur;
  ListOfPossibleSpeciesFeatureValues   mPossibleSpeciesFeatureValues;


public:

  /**
   * Creates a new SpeciesFeatureType with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SpeciesFeatureType
   *
   * @param version an unsigned int, the SBML Version to assign to this SpeciesFeatureType
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this SpeciesFeatureType
   */
  SpeciesFeatureType(unsigned int level      = MultiExtension::getDefaultLevel(),
                     unsigned int version    = MultiExtension::getDefaultVersion(),
                     unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpeciesFeatureType with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  SpeciesFeatureType(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for SpeciesFeatureType.
   *
   * @param orig; the SpeciesFeatureType instance to copy.
   */
  SpeciesFeatureType(const SpeciesFeatureType& orig);


   /**
   * Assignment operator for SpeciesFeatureType.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SpeciesFeatureType& operator=(const SpeciesFeatureType& rhs);


   /**
   * Creates and returns a deep copy of this SpeciesFeatureType object.
   *
   * @return a (deep) copy of this SpeciesFeatureType object.
   */
  virtual SpeciesFeatureType* clone () const;


   /**
   * Destructor for SpeciesFeatureType.
   */
  virtual ~SpeciesFeatureType();


   /**
   * Returns the value of the "id" attribute of this SpeciesFeatureType.
   *
   * @return the value of the "id" attribute of this SpeciesFeatureType as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesFeatureType's "id" attribute has been set.
   *
   * @return @c true if this SpeciesFeatureType's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this SpeciesFeatureType.
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
   * Unsets the value of the "id" attribute of this SpeciesFeatureType.
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
   * Returns the value of the "name" attribute of this SpeciesFeatureType.
   *
   * @return the value of the "name" attribute of this SpeciesFeatureType as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesFeatureType's "name" attribute has been set.
   *
   * @return @c true if this SpeciesFeatureType's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this SpeciesFeatureType.
   *
   * @param name; const std::string& value of the "name" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this SpeciesFeatureType.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetName();


  /**
   * Returns the value of the "occur" attribute of this SpeciesFeatureType.
   *
   * @return the value of the "occur" attribute of this SpeciesFeatureType as a unsigned integer.
   */
  virtual unsigned int getOccur() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesFeatureType's "occur" attribute has been set.
   *
   * @return @c true if this SpeciesFeatureType's "occur" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetOccur() const;


  /**
   * Sets the value of the "occur" attribute of this SpeciesFeatureType.
   *
   * @param occur; unsigned int value of the "occur" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setOccur(unsigned int occur);


  /**
   * Unsets the value of the "occur" attribute of this SpeciesFeatureType.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetOccur();


  /**
   * Returns the  "ListOfPossibleSpeciesFeatureValues" in this SpeciesFeatureType object.
   *
   * @return the "ListOfPossibleSpeciesFeatureValues" attribute of this SpeciesFeatureType.
   */
  const ListOfPossibleSpeciesFeatureValues* getListOfPossibleSpeciesFeatureValues() const;


  /**
   * Returns the  "ListOfPossibleSpeciesFeatureValues" in this SpeciesFeatureType object.
   *
   * @return the "ListOfPossibleSpeciesFeatureValues" attribute of this SpeciesFeatureType.
   */
  ListOfPossibleSpeciesFeatureValues* getListOfPossibleSpeciesFeatureValues();


  /**
   * Get a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues.
   *
   * @param n the index number of the PossibleSpeciesFeatureValue to get.
   *
   * @return the nth PossibleSpeciesFeatureValue in the ListOfPossibleSpeciesFeatureValues within this SpeciesFeatureType.
   *
   * @see getNumPossibleSpeciesFeatureValues()
   */
  PossibleSpeciesFeatureValue* getPossibleSpeciesFeatureValue(unsigned int n);


  /**
   * Get a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues.
   *
   * @param n the index number of the PossibleSpeciesFeatureValue to get.
   *
   * @return the nth PossibleSpeciesFeatureValue in the ListOfPossibleSpeciesFeatureValues within this SpeciesFeatureType.
   *
   * @see getNumPossibleSpeciesFeatureValues()
   */
  const PossibleSpeciesFeatureValue* getPossibleSpeciesFeatureValue(unsigned int n) const;


  /**
   * Get a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the PossibleSpeciesFeatureValue to get.
   *
   * @return the PossibleSpeciesFeatureValue in the ListOfPossibleSpeciesFeatureValues
   * with the given id or NULL if no such
   * PossibleSpeciesFeatureValue exists.
   *
   * @see getPossibleSpeciesFeatureValue(unsigned int n)
   *
   * @see getNumPossibleSpeciesFeatureValues()
   */
  PossibleSpeciesFeatureValue* getPossibleSpeciesFeatureValue(const std::string& sid);


  /**
   * Get a PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the PossibleSpeciesFeatureValue to get.
   *
   * @return the PossibleSpeciesFeatureValue in the ListOfPossibleSpeciesFeatureValues
   * with the given id or NULL if no such
   * PossibleSpeciesFeatureValue exists.
   *
   * @see getPossibleSpeciesFeatureValue(unsigned int n)
   *
   * @see getNumPossibleSpeciesFeatureValues()
   */
  const PossibleSpeciesFeatureValue* getPossibleSpeciesFeatureValue(const std::string& sid) const;


  /**
   * Adds a copy the given "PossibleSpeciesFeatureValue" to this SpeciesFeatureType.
   *
   * @param psfv; the PossibleSpeciesFeatureValue object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addPossibleSpeciesFeatureValue(const PossibleSpeciesFeatureValue* psfv);


  /**
   * Get the number of PossibleSpeciesFeatureValue objects in this SpeciesFeatureType.
   *
   * @return the number of PossibleSpeciesFeatureValue objects in this SpeciesFeatureType
   */
  unsigned int getNumPossibleSpeciesFeatureValues() const;


  /**
   * Creates a new PossibleSpeciesFeatureValue object, adds it to this SpeciesFeatureTypes
   * ListOfPossibleSpeciesFeatureValues and returns the PossibleSpeciesFeatureValue object created. 
   *
   * @return a new PossibleSpeciesFeatureValue object instance
   *
   * @see addPossibleSpeciesFeatureValue(const PossibleSpeciesFeatureValue* psfv)
   */
  PossibleSpeciesFeatureValue* createPossibleSpeciesFeatureValue();


  /**
   * Removes the nth PossibleSpeciesFeatureValue from the ListOfPossibleSpeciesFeatureValues within this SpeciesFeatureType.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the PossibleSpeciesFeatureValue to remove.
   *
   * @see getNumPossibleSpeciesFeatureValues()
   */
  PossibleSpeciesFeatureValue* removePossibleSpeciesFeatureValue(unsigned int n);


  /**
   * Removes the PossibleSpeciesFeatureValue with the given identifier from the ListOfPossibleSpeciesFeatureValues within this SpeciesFeatureType
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the PossibleSpeciesFeatureValue to remove.
   *
   * @return the PossibleSpeciesFeatureValue removed. As mentioned above, the caller owns the
   * returned item.
   */
  PossibleSpeciesFeatureValue* removePossibleSpeciesFeatureValue(const std::string& sid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for SpeciesFeatureType, is
   * always @c "speciesFeatureType".
   *
   * @return the name of this element, i.e. @c "speciesFeatureType".
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
   * for this SpeciesFeatureType object have been set.
   *
   * @note The required attributes for a SpeciesFeatureType object are:
   * @li "id"
   * @li "occur"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this SpeciesFeatureType object have been set.
   *
   * @note The required elements for a SpeciesFeatureType object are:
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

class LIBSBML_EXTERN ListOfSpeciesFeatureTypes : public ListOf
{

public:

  /**
   * Creates a new ListOfSpeciesFeatureTypes with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfSpeciesFeatureTypes
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfSpeciesFeatureTypes
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this ListOfSpeciesFeatureTypes
   */
  ListOfSpeciesFeatureTypes(unsigned int level      = MultiExtension::getDefaultLevel(),
                            unsigned int version    = MultiExtension::getDefaultVersion(),
                            unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpeciesFeatureTypes with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  ListOfSpeciesFeatureTypes(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this ListOfSpeciesFeatureTypes object.
   *
   * @return a (deep) copy of this ListOfSpeciesFeatureTypes object.
   */
  virtual ListOfSpeciesFeatureTypes* clone () const;


   /**
   * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes.
   *
   * @param n the index number of the SpeciesFeatureType to get.
   *
   * @return the nth SpeciesFeatureType in this ListOfSpeciesFeatureTypes.
   *
   * @see size()
   */
  virtual SpeciesFeatureType* get(unsigned int n);


  /**
   * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes.
   *
   * @param n the index number of the SpeciesFeatureType to get.
   *
   * @return the nth SpeciesFeatureType in this ListOfSpeciesFeatureTypes.
   *
   * @see size()
   */
  virtual const SpeciesFeatureType* get(unsigned int n) const;


  /**
   * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesFeatureType to get.
   *
   * @return SpeciesFeatureType in this ListOfSpeciesFeatureTypes
   * with the given id or NULL if no such
   * SpeciesFeatureType exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual SpeciesFeatureType* get(const std::string& sid);


  /**
   * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesFeatureType to get.
   *
   * @return SpeciesFeatureType in this ListOfSpeciesFeatureTypes
   * with the given id or NULL if no such
   * SpeciesFeatureType exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SpeciesFeatureType* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesFeatureType from this ListOfSpeciesFeatureTypes
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SpeciesFeatureType to remove.
   *
   * @see size()
   */
  virtual SpeciesFeatureType* remove(unsigned int n);


  /**
   * Removes the SpeciesFeatureType from this ListOfSpeciesFeatureTypes with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SpeciesFeatureType to remove.
   *
   * @return the SpeciesFeatureType removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual SpeciesFeatureType* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfSpeciesFeatureTypes, is
   * always @c "listOfSpeciesFeatureTypes".
   *
   * @return the name of this element, i.e. @c "listOfSpeciesFeatureTypes".
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
   * Creates a new SpeciesFeatureType in this ListOfSpeciesFeatureTypes
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Multi package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
SpeciesFeatureType_t *
SpeciesFeatureType_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion);


LIBSBML_EXTERN
void
SpeciesFeatureType_free(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
SpeciesFeatureType_t *
SpeciesFeatureType_clone(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
char *
SpeciesFeatureType_getId(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
char *
SpeciesFeatureType_getName(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
unsigned int
SpeciesFeatureType_getOccur(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
int
SpeciesFeatureType_isSetId(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
int
SpeciesFeatureType_isSetName(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
int
SpeciesFeatureType_isSetOccur(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
int
SpeciesFeatureType_setId(SpeciesFeatureType_t * sft, const char * id);


LIBSBML_EXTERN
int
SpeciesFeatureType_setName(SpeciesFeatureType_t * sft, const char * name);


LIBSBML_EXTERN
int
SpeciesFeatureType_setOccur(SpeciesFeatureType_t * sft, unsigned int occur);


LIBSBML_EXTERN
int
SpeciesFeatureType_unsetId(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
int
SpeciesFeatureType_unsetName(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
int
SpeciesFeatureType_unsetOccur(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
int
SpeciesFeatureType_addPossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft, PossibleSpeciesFeatureValue_t * psfv);


LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_createPossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
ListOf_t *
SpeciesFeatureType_getListOfPossibleSpeciesFeatureValues(SpeciesFeatureType_t * sft) ;


LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_getPossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft, unsigned int n);


LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_getPossibleSpeciesFeatureValueById(SpeciesFeatureType_t * sft, const char * sid);


LIBSBML_EXTERN
unsigned int
SpeciesFeatureType_getNumPossibleSpeciesFeatureValues(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_removePossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft, unsigned int n);


LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_removePossibleSpeciesFeatureValueById(SpeciesFeatureType_t * sft, const char * sid);


LIBSBML_EXTERN
int
SpeciesFeatureType_hasRequiredAttributes(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
int
SpeciesFeatureType_hasRequiredElements(SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
SpeciesFeatureType_t *
ListOfSpeciesFeatureTypes_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
SpeciesFeatureType_t *
ListOfSpeciesFeatureTypes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpeciesFeatureType_H__  */

