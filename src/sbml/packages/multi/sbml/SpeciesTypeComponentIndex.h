/**
 * @file:   SpeciesTypeComponentIndex.h
 * @brief:  Implementation of the SpeciesTypeComponentIndex class
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


#ifndef SpeciesTypeComponentIndex_H__
#define SpeciesTypeComponentIndex_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpeciesTypeComponentIndex : public SBase
{

protected:

////  std::string   mId;
  std::string   mComponent;
  std::string   mIdentifyingParent;


public:

  /**
   * Creates a new SpeciesTypeComponentIndex with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SpeciesTypeComponentIndex
   *
   * @param version an unsigned int, the SBML Version to assign to this SpeciesTypeComponentIndex
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this SpeciesTypeComponentIndex
   */
  SpeciesTypeComponentIndex(unsigned int level      = MultiExtension::getDefaultLevel(),
                            unsigned int version    = MultiExtension::getDefaultVersion(),
                            unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpeciesTypeComponentIndex with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  SpeciesTypeComponentIndex(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for SpeciesTypeComponentIndex.
   *
   * @param orig; the SpeciesTypeComponentIndex instance to copy.
   */
  SpeciesTypeComponentIndex(const SpeciesTypeComponentIndex& orig);


   /**
   * Assignment operator for SpeciesTypeComponentIndex.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SpeciesTypeComponentIndex& operator=(const SpeciesTypeComponentIndex& rhs);


   /**
   * Creates and returns a deep copy of this SpeciesTypeComponentIndex object.
   *
   * @return a (deep) copy of this SpeciesTypeComponentIndex object.
   */
  virtual SpeciesTypeComponentIndex* clone () const;


   /**
   * Destructor for SpeciesTypeComponentIndex.
   */
  virtual ~SpeciesTypeComponentIndex();


   /**
   * Returns the value of the "id" attribute of this SpeciesTypeComponentIndex.
   *
   * @return the value of the "id" attribute of this SpeciesTypeComponentIndex as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesTypeComponentIndex's "id" attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentIndex's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this SpeciesTypeComponentIndex.
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
   * Unsets the value of the "id" attribute of this SpeciesTypeComponentIndex.
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
   * Returns the value of the "name" attribute of this SpeciesTypeComponentIndex.
   *
   * @return the value of the "name" attribute of this SpeciesTypeComponentIndex as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesTypeComponentIndex's "name" attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentIndex's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this SpeciesTypeComponentIndex.
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
   * Unsets the value of the "name" attribute of this SpeciesTypeComponentIndex.
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
   * Returns the value of the "component" attribute of this SpeciesTypeComponentIndex.
   *
   * @return the value of the "component" attribute of this SpeciesTypeComponentIndex as a string.
   */
  virtual const std::string& getComponent() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesTypeComponentIndex's "component" attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentIndex's "component" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetComponent() const;


  /**
   * Sets the value of the "component" attribute of this SpeciesTypeComponentIndex.
   *
   * @param component; const std::string& value of the "component" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setComponent(const std::string& component);


  /**
   * Unsets the value of the "component" attribute of this SpeciesTypeComponentIndex.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetComponent();


  /**
   * Returns the value of the "identifyingParent" attribute of this SpeciesTypeComponentIndex.
   *
   * @return the value of the "identifyingParent" attribute of this SpeciesTypeComponentIndex as a string.
   */
  virtual const std::string& getIdentifyingParent() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesTypeComponentIndex's "identifyingParent" attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentIndex's "identifyingParent" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetIdentifyingParent() const;


  /**
   * Sets the value of the "identifyingParent" attribute of this SpeciesTypeComponentIndex.
   *
   * @param identifyingParent; const std::string& value of the "identifyingParent" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setIdentifyingParent(const std::string& identifyingParent);


  /**
   * Unsets the value of the "identifyingParent" attribute of this SpeciesTypeComponentIndex.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetIdentifyingParent();


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
   * Returns the XML element name of this object, which for SpeciesTypeComponentIndex, is
   * always @c "speciesTypeComponentIndex".
   *
   * @return the name of this element, i.e. @c "speciesTypeComponentIndex".
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
   * for this SpeciesTypeComponentIndex object have been set.
   *
   * @note The required attributes for a SpeciesTypeComponentIndex object are:
   * @li "id"
   * @li "component"
   * @li "occur"
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

class LIBSBML_EXTERN ListOfSpeciesTypeComponentIndexes : public ListOf
{

public:

  /**
   * Creates a new ListOfSpeciesTypeComponentIndexes with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfSpeciesTypeComponentIndexes
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfSpeciesTypeComponentIndexes
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this ListOfSpeciesTypeComponentIndexes
   */
  ListOfSpeciesTypeComponentIndexes(unsigned int level      = MultiExtension::getDefaultLevel(),
                                   unsigned int version    = MultiExtension::getDefaultVersion(),
                                   unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpeciesTypeComponentIndexes with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  ListOfSpeciesTypeComponentIndexes(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this ListOfSpeciesTypeComponentIndexes object.
   *
   * @return a (deep) copy of this ListOfSpeciesTypeComponentIndexes object.
   */
  virtual ListOfSpeciesTypeComponentIndexes* clone () const;


   /**
   * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index number of the SpeciesTypeComponentIndex to get.
   *
   * @return the nth SpeciesTypeComponentIndex in this ListOfSpeciesTypeComponentIndexes.
   *
   * @see size()
   */
  virtual SpeciesTypeComponentIndex* get(unsigned int n);


  /**
   * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index number of the SpeciesTypeComponentIndex to get.
   *
   * @return the nth SpeciesTypeComponentIndex in this ListOfSpeciesTypeComponentIndexes.
   *
   * @see size()
   */
  virtual const SpeciesTypeComponentIndex* get(unsigned int n) const;


  /**
   * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentIndex to get.
   *
   * @return SpeciesTypeComponentIndex in this ListOfSpeciesTypeComponentIndexes
   * with the given id or NULL if no such
   * SpeciesTypeComponentIndex exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual SpeciesTypeComponentIndex* get(const std::string& sid);


  /**
   * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentIndex to get.
   *
   * @return SpeciesTypeComponentIndex in this ListOfSpeciesTypeComponentIndexes
   * with the given id or NULL if no such
   * SpeciesTypeComponentIndex exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SpeciesTypeComponentIndex* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesTypeComponentIndex from this ListOfSpeciesTypeComponentIndexes
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SpeciesTypeComponentIndex to remove.
   *
   * @see size()
   */
  virtual SpeciesTypeComponentIndex* remove(unsigned int n);


  /**
   * Removes the SpeciesTypeComponentIndex from this ListOfSpeciesTypeComponentIndexes with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SpeciesTypeComponentIndex to remove.
   *
   * @return the SpeciesTypeComponentIndex removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual SpeciesTypeComponentIndex* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfSpeciesTypeComponentIndexes, is
   * always @c "listOfSpeciesTypeComponentIndexes".
   *
   * @return the name of this element, i.e. @c "listOfSpeciesTypeComponentIndexes".
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
   * Creates a new SpeciesTypeComponentIndex in this ListOfSpeciesTypeComponentIndexes
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
SpeciesTypeComponentIndex_t *
SpeciesTypeComponentIndex_create(unsigned int level, unsigned int version,
                                 unsigned int pkgVersion);


LIBSBML_EXTERN
void
SpeciesTypeComponentIndex_free(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
SpeciesTypeComponentIndex_clone(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getId(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getName(SpeciesTypeComponentIndex_t * cr);


LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getComponent(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
unsigned int
SpeciesTypeComponentIndex_getOccur(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getIdentifyingParent(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetId(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetName(SpeciesTypeComponentIndex_t * cr);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetComponent(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetOccur(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetIdentifyingParent(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setId(SpeciesTypeComponentIndex_t * stci, const char * id);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setName(SpeciesTypeComponentIndex_t * cr, const char * name);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setComponent(SpeciesTypeComponentIndex_t * stci, const char * component);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setOccur(SpeciesTypeComponentIndex_t * stci, unsigned int occur);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setIdentifyingParent(SpeciesTypeComponentIndex_t * stci, const char * identifyingParent);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetId(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetName(SpeciesTypeComponentIndex_t * cr);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetComponent(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetOccur(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetIdentifyingParent(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_hasRequiredAttributes(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_hasRequiredElements(SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
ListOfSpeciesTypeComponentIndexes_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
ListOfSpeciesTypeComponentIndexes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpeciesTypeComponentIndex_H__  */

