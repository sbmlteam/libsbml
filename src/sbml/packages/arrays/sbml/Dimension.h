/**
 * @file:   Dimension.h
 * @brief:  Implementation of the Dimension class
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


#ifndef Dimension_H__
#define Dimension_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/arrays/common/arraysfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Dimension : public SBase
{

protected:

  std::string   mId;
  std::string   mName;
  std::string   mSize;
  unsigned int  mArrayDimension;
  bool          mIsSetArrayDimension;


public:

  /**
   * Creates a new Dimension with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Dimension
   *
   * @param version an unsigned int, the SBML Version to assign to this Dimension
   *
   * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to this Dimension
   */
  Dimension(unsigned int level      = ArraysExtension::getDefaultLevel(),
            unsigned int version    = ArraysExtension::getDefaultVersion(),
            unsigned int pkgVersion = ArraysExtension::getDefaultPackageVersion());


  /**
   * Creates a new Dimension with the given ArraysPkgNamespaces object.
   *
   * @param arraysns the ArraysPkgNamespaces object
   */
  Dimension(ArraysPkgNamespaces* arraysns);


   /**
   * Copy constructor for Dimension.
   *
   * @param orig; the Dimension instance to copy.
   */
  Dimension(const Dimension& orig);


   /**
   * Assignment operator for Dimension.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  Dimension& operator=(const Dimension& rhs);


   /**
   * Creates and returns a deep copy of this Dimension object.
   *
   * @return a (deep) copy of this Dimension object.
   */
  virtual Dimension* clone () const;


   /**
   * Destructor for Dimension.
   */
  virtual ~Dimension();


   /**
   * Returns the value of the "id" attribute of this Dimension.
   *
   * @return the value of the "id" attribute of this Dimension as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this Dimension.
   *
   * @return the value of the "name" attribute of this Dimension as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "size" attribute of this Dimension.
   *
   * @return the value of the "size" attribute of this Dimension as a string.
   */
  virtual const std::string& getSize() const;


  /**
   * Returns the value of the "arrayDimension" attribute of this Dimension.
   *
   * @return the value of the "arrayDimension" attribute of this Dimension as a unsigned integer.
   */
  virtual unsigned int getArrayDimension() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Dimension's "id" attribute has been set.
   *
   * @return @c true if this Dimension's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Dimension's "name" attribute has been set.
   *
   * @return @c true if this Dimension's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Dimension's "size" attribute has been set.
   *
   * @return @c true if this Dimension's "size" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSize() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Dimension's "arrayDimension" attribute has been set.
   *
   * @return @c true if this Dimension's "arrayDimension" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetArrayDimension() const;


  /**
   * Sets the value of the "id" attribute of this Dimension.
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
   * Sets the value of the "name" attribute of this Dimension.
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
   * Sets the value of the "size" attribute of this Dimension.
   *
   * @param size; const std::string& value of the "size" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSize(const std::string& size);


  /**
   * Sets the value of the "arrayDimension" attribute of this Dimension.
   *
   * @param arrayDimension; unsigned int value of the "arrayDimension" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setArrayDimension(unsigned int arrayDimension);


  /**
   * Unsets the value of the "id" attribute of this Dimension.
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
   * Unsets the value of the "name" attribute of this Dimension.
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
   * Unsets the value of the "size" attribute of this Dimension.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSize();


  /**
   * Unsets the value of the "arrayDimension" attribute of this Dimension.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetArrayDimension();


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
   * Returns the XML element name of this object, which for Dimension, is
   * always @c "dimension".
   *
   * @return the name of this element, i.e. @c "dimension".
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
   * for this Dimension object have been set.
   *
   * @note The required attributes for a Dimension object are:
   * @li "size"
   * @li "arrayDimension"
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

class LIBSBML_EXTERN ListOfDimensions : public ListOf
{

public:

  /**
   * Creates a new ListOfDimensions with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfDimensions
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfDimensions
   *
   * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to this ListOfDimensions
   */
  ListOfDimensions(unsigned int level      = ArraysExtension::getDefaultLevel(),
                   unsigned int version    = ArraysExtension::getDefaultVersion(),
                   unsigned int pkgVersion = ArraysExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDimensions with the given ArraysPkgNamespaces object.
   *
   * @param arraysns the ArraysPkgNamespaces object
   */
  ListOfDimensions(ArraysPkgNamespaces* arraysns);


   /**
   * Creates and returns a deep copy of this ListOfDimensions object.
   *
   * @return a (deep) copy of this ListOfDimensions object.
   */
  virtual ListOfDimensions* clone () const;


   /**
   * Get a Dimension from the ListOfDimensions.
   *
   * @param n the index number of the Dimension to get.
   *
   * @return the nth Dimension in this ListOfDimensions.
   *
   * @see size()
   */
  virtual Dimension* get(unsigned int n);


  /**
   * Get a Dimension from the ListOfDimensions.
   *
   * @param n the index number of the Dimension to get.
   *
   * @return the nth Dimension in this ListOfDimensions.
   *
   * @see size()
   */
  virtual const Dimension* get(unsigned int n) const;


  /**
   * Get a Dimension from the ListOfDimensions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Dimension to get.
   *
   * @return Dimension in this ListOfDimensions
   * with the given id or NULL if no such
   * Dimension exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual Dimension* get(const std::string& sid);


  /**
   * Get a Dimension from the ListOfDimensions
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Dimension to get.
   *
   * @return Dimension in this ListOfDimensions
   * with the given id or NULL if no such
   * Dimension exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const Dimension* get(const std::string& sid) const;


  /**
   * Removes the nth Dimension from this ListOfDimensions
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the Dimension to remove.
   *
   * @see size()
   */
  virtual Dimension* remove(unsigned int n);


  /**
   * Removes the Dimension from this ListOfDimensions with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the Dimension to remove.
   *
   * @return the Dimension removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual Dimension* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfDimensions, is
   * always @c "listOfDimensions".
   *
   * @return the name of this element, i.e. @c "listOfDimensions".
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
   * Creates a new Dimension in this ListOfDimensions
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Arrays package.
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
Dimension_t *
Dimension_create(unsigned int level, unsigned int version,
                 unsigned int pkgVersion);


LIBSBML_EXTERN
void
Dimension_free(Dimension_t * d);


LIBSBML_EXTERN
Dimension_t *
Dimension_clone(Dimension_t * d);


LIBSBML_EXTERN
char *
Dimension_getId(Dimension_t * d);


LIBSBML_EXTERN
char *
Dimension_getName(Dimension_t * d);


LIBSBML_EXTERN
char *
Dimension_getSize(Dimension_t * d);


LIBSBML_EXTERN
unsigned int
Dimension_getArrayDimension(Dimension_t * d);


LIBSBML_EXTERN
int
Dimension_isSetId(Dimension_t * d);


LIBSBML_EXTERN
int
Dimension_isSetName(Dimension_t * d);


LIBSBML_EXTERN
int
Dimension_isSetSize(Dimension_t * d);


LIBSBML_EXTERN
int
Dimension_isSetArrayDimension(Dimension_t * d);


LIBSBML_EXTERN
int
Dimension_setId(Dimension_t * d, const char * id);


LIBSBML_EXTERN
int
Dimension_setName(Dimension_t * d, const char * name);


LIBSBML_EXTERN
int
Dimension_setSize(Dimension_t * d, const char * size);


LIBSBML_EXTERN
int
Dimension_setArrayDimension(Dimension_t * d, unsigned int arrayDimension);


LIBSBML_EXTERN
int
Dimension_unsetId(Dimension_t * d);


LIBSBML_EXTERN
int
Dimension_unsetName(Dimension_t * d);


LIBSBML_EXTERN
int
Dimension_unsetSize(Dimension_t * d);


LIBSBML_EXTERN
int
Dimension_unsetArrayDimension(Dimension_t * d);


LIBSBML_EXTERN
int
Dimension_hasRequiredAttributes(Dimension_t * d);


LIBSBML_EXTERN
Dimension_t *
ListOfDimensions_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
Dimension_t *
ListOfDimensions_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  Dimension_H__  */

