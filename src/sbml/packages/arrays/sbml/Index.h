/**
 * @file:   Index.h
 * @brief:  Implementation of the Index class
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


#ifndef Index_H__
#define Index_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/arrays/common/arraysfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Index : public SBase
{

protected:

  std::string   mReferencedAttribute;
  unsigned int  mArrayDimension;
  bool          mIsSetArrayDimension;
  ASTNode* mMath;


public:

  /**
   * Creates a new Index with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Index
   *
   * @param version an unsigned int, the SBML Version to assign to this Index
   *
   * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to this Index
   */
  Index(unsigned int level      = ArraysExtension::getDefaultLevel(),
        unsigned int version    = ArraysExtension::getDefaultVersion(),
        unsigned int pkgVersion = ArraysExtension::getDefaultPackageVersion());


  /**
   * Creates a new Index with the given ArraysPkgNamespaces object.
   *
   * @param arraysns the ArraysPkgNamespaces object
   */
  Index(ArraysPkgNamespaces* arraysns);


   /**
   * Copy constructor for Index.
   *
   * @param orig; the Index instance to copy.
   */
  Index(const Index& orig);


   /**
   * Assignment operator for Index.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  Index& operator=(const Index& rhs);


   /**
   * Creates and returns a deep copy of this Index object.
   *
   * @return a (deep) copy of this Index object.
   */
  virtual Index* clone () const;


   /**
   * Destructor for Index.
   */
  virtual ~Index();


   /**
   * Returns the value of the "referencedAttribute" attribute of this Index.
   *
   * @return the value of the "referencedAttribute" attribute of this Index as a string.
   */
  virtual const std::string& getReferencedAttribute() const;


  /**
   * Returns the value of the "arrayDimension" attribute of this Index.
   *
   * @return the value of the "arrayDimension" attribute of this Index as a unsigned integer.
   */
  virtual unsigned int getArrayDimension() const;


  /**
   * Returns the "math" element of this Index.
   *
   * @return the "math" element of this Index.
   */
  virtual const ASTNode* getMath() const;


  /**
   * Returns the "math" element of this Index.
   *
   * @return the "math" element of this Index.
   */
  virtual ASTNode* getMath();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Index's "referencedAttribute" attribute has been set.
   *
   * @return @c true if this Index's "referencedAttribute" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetReferencedAttribute() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Index's "arrayDimension" attribute has been set.
   *
   * @return @c true if this Index's "arrayDimension" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetArrayDimension() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Index's "math" element has been set.
   *
   * @return @c true if this Index's "math" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetMath() const;


  /**
   * Sets the value of the "referencedAttribute" attribute of this Index.
   *
   * @param referencedAttribute; const std::string& value of the "referencedAttribute" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setReferencedAttribute(const std::string& referencedAttribute);


  /**
   * Sets the value of the "arrayDimension" attribute of this Index.
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
   * Sets the "math" element of this Index.
   *
   * @param math; ASTNode* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setMath(ASTNode* math);


  /**
   * Unsets the value of the "referencedAttribute" attribute of this Index.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetReferencedAttribute();


  /**
   * Unsets the value of the "arrayDimension" attribute of this Index.
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
   * Unsets the "math" element of this Index.
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
   * Returns the XML element name of this object, which for Index, is
   * always @c "index".
   *
   * @return the name of this element, i.e. @c "index".
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
   * for this Index object have been set.
   *
   * @note The required attributes for a Index object are:
   * @li "referencedAttribute"
   * @li "arrayDimension"
   * @li "math"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this Index object have been set.
   *
   * @note The required elements for a Index object are:
   * @li "math"
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

class LIBSBML_EXTERN ListOfIndices : public ListOf
{

public:

  /**
   * Creates a new ListOfIndices with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfIndices
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfIndices
   *
   * @param pkgVersion an unsigned int, the SBML Arrays Version to assign to this ListOfIndices
   */
  ListOfIndices(unsigned int level      = ArraysExtension::getDefaultLevel(),
               unsigned int version    = ArraysExtension::getDefaultVersion(),
               unsigned int pkgVersion = ArraysExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfIndices with the given ArraysPkgNamespaces object.
   *
   * @param arraysns the ArraysPkgNamespaces object
   */
  ListOfIndices(ArraysPkgNamespaces* arraysns);


   /**
   * Creates and returns a deep copy of this ListOfIndices object.
   *
   * @return a (deep) copy of this ListOfIndices object.
   */
  virtual ListOfIndices* clone () const;


   /**
   * Get a Index from the ListOfIndices.
   *
   * @param n the index number of the Index to get.
   *
   * @return the nth Index in this ListOfIndices.
   *
   * @see size()
   */
  virtual Index* get(unsigned int n);


  /**
   * Get a Index from the ListOfIndices.
   *
   * @param n the index number of the Index to get.
   *
   * @return the nth Index in this ListOfIndices.
   *
   * @see size()
   */
  virtual const Index* get(unsigned int n) const;


  /**
   * Get a Index from the ListOfIndices
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Index to get.
   *
   * @return Index in this ListOfIndices
   * with the given id or NULL if no such
   * Index exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual Index* get(const std::string& sid);


  /**
   * Get a Index from the ListOfIndices
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Index to get.
   *
   * @return Index in this ListOfIndices
   * with the given id or NULL if no such
   * Index exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const Index* get(const std::string& sid) const;


  /**
   * Removes the nth Index from this ListOfIndices
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the Index to remove.
   *
   * @see size()
   */
  virtual Index* remove(unsigned int n);


  /**
   * Removes the Index from this ListOfIndices with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the Index to remove.
   *
   * @return the Index removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual Index* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfIndices, is
   * always @c "listOfIndices".
   *
   * @return the name of this element, i.e. @c "listOfIndices".
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
   * Creates a new Index in this ListOfIndices
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
Index_t *
Index_create(unsigned int level, unsigned int version,
             unsigned int pkgVersion);


LIBSBML_EXTERN
void
Index_free(Index_t * i);


LIBSBML_EXTERN
Index_t *
Index_clone(Index_t * i);


LIBSBML_EXTERN
char *
Index_getReferencedAttribute(Index_t * i);


LIBSBML_EXTERN
unsigned int
Index_getArrayDimension(Index_t * i);


LIBSBML_EXTERN
int
Index_isSetReferencedAttribute(Index_t * i);


LIBSBML_EXTERN
int
Index_isSetArrayDimension(Index_t * i);


LIBSBML_EXTERN
int
Index_setReferencedAttribute(Index_t * i, const char * referencedAttribute);


LIBSBML_EXTERN
int
Index_setArrayDimension(Index_t * i, unsigned int arrayDimension);


LIBSBML_EXTERN
int
Index_unsetReferencedAttribute(Index_t * i);


LIBSBML_EXTERN
int
Index_unsetArrayDimension(Index_t * i);


LIBSBML_EXTERN
int
Index_hasRequiredAttributes(Index_t * i);


LIBSBML_EXTERN
int
Index_hasRequiredElements(Index_t * i);


LIBSBML_EXTERN
Index_t *
ListOfIndices_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
Index_t *
ListOfIndices_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  Index_H__  */

