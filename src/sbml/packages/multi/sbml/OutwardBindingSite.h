/**
 * @file:   OutwardBindingSite.h
 * @brief:  Implementation of the OutwardBindingSite class
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


#ifndef OutwardBindingSite_H__
#define OutwardBindingSite_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>

typedef enum
{
    MULTI_BINDING_STATUS_BOUND
  , MULTI_BINDING_STATUS_UNBOUND
  , MULTI_BINDING_STATUS_EITHER
  , MULTI_BINDING_STATUS_UNKNOWN
} BindingStatus_t;


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN OutwardBindingSite : public SBase
{

protected:

  BindingStatus_t   mBindingStatus;
  std::string   mComponent;


public:

  /**
   * Creates a new OutwardBindingSite with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this OutwardBindingSite
   *
   * @param version an unsigned int, the SBML Version to assign to this OutwardBindingSite
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this OutwardBindingSite
   */
  OutwardBindingSite(unsigned int level      = MultiExtension::getDefaultLevel(),
                     unsigned int version    = MultiExtension::getDefaultVersion(),
                     unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new OutwardBindingSite with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  OutwardBindingSite(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for OutwardBindingSite.
   *
   * @param orig; the OutwardBindingSite instance to copy.
   */
  OutwardBindingSite(const OutwardBindingSite& orig);


   /**
   * Assignment operator for OutwardBindingSite.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  OutwardBindingSite& operator=(const OutwardBindingSite& rhs);


   /**
   * Creates and returns a deep copy of this OutwardBindingSite object.
   *
   * @return a (deep) copy of this OutwardBindingSite object.
   */
  virtual OutwardBindingSite* clone () const;


   /**
   * Destructor for OutwardBindingSite.
   */
  virtual ~OutwardBindingSite();


   /**
   * Returns the value of the "id" attribute of this OutwardBindingSite.
   *
   * @return the value of the "id" attribute of this OutwardBindingSite as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * OutwardBindingSite's "id" attribute has been set.
   *
   * @return @c true if this OutwardBindingSite's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this OutwardBindingSite.
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
   * Unsets the value of the "id" attribute of this OutwardBindingSite.
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
   * Returns the value of the "name" attribute of this OutwardBindingSite.
   *
   * @return the value of the "name" attribute of this OutwardBindingSite as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * OutwardBindingSite's "name" attribute has been set.
   *
   * @return @c true if this OutwardBindingSite's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this OutwardBindingSite.
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
   * Unsets the value of the "name" attribute of this OutwardBindingSite.
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
   * Returns the value of the "bindingStatus" attribute of this OutwardBindingSite.
   *
   * @return the value of the "bindingStatus" attribute of this OutwardBindingSite as a FIX ME.
   */
  virtual BindingStatus_t getBindingStatus() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * OutwardBindingSite's "bindingStatus" attribute has been set.
   *
   * @return @c true if this OutwardBindingSite's "bindingStatus" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetBindingStatus() const;


  /**
   * Sets the value of the "bindingStatus" attribute of this OutwardBindingSite.
   *
   * @param bindingStatus; FIX ME value of the "bindingStatus" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setBindingStatus(BindingStatus_t bindingStatus);


  /**
   * Unsets the value of the "bindingStatus" attribute of this OutwardBindingSite.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetBindingStatus();


  /**
   * Returns the value of the "component" attribute of this OutwardBindingSite.
   *
   * @return the value of the "component" attribute of this OutwardBindingSite as a string.
   */
  virtual const std::string& getComponent() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * OutwardBindingSite's "component" attribute has been set.
   *
   * @return @c true if this OutwardBindingSite's "component" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetComponent() const;


  /**
   * Sets the value of the "component" attribute of this OutwardBindingSite.
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
   * Unsets the value of the "component" attribute of this OutwardBindingSite.
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
   * Returns the XML element name of this object, which for OutwardBindingSite, is
   * always @c "outwardBindingSite".
   *
   * @return the name of this element, i.e. @c "outwardBindingSite".
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
   * for this OutwardBindingSite object have been set.
   *
   * @note The required attributes for a OutwardBindingSite object are:
   * @li "bindingStatus"
   * @li "component"
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

class LIBSBML_EXTERN ListOfOutwardBindingSites : public ListOf
{

public:

  /**
   * Creates a new ListOfOutwardBindingSites with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfOutwardBindingSites
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfOutwardBindingSites
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this ListOfOutwardBindingSites
   */
  ListOfOutwardBindingSites(unsigned int level      = MultiExtension::getDefaultLevel(),
                            unsigned int version    = MultiExtension::getDefaultVersion(),
                            unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfOutwardBindingSites with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  ListOfOutwardBindingSites(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this ListOfOutwardBindingSites object.
   *
   * @return a (deep) copy of this ListOfOutwardBindingSites object.
   */
  virtual ListOfOutwardBindingSites* clone () const;


   /**
   * Get a OutwardBindingSite from the ListOfOutwardBindingSites.
   *
   * @param n the index number of the OutwardBindingSite to get.
   *
   * @return the nth OutwardBindingSite in this ListOfOutwardBindingSites.
   *
   * @see size()
   */
  virtual OutwardBindingSite* get(unsigned int n);


  /**
   * Get a OutwardBindingSite from the ListOfOutwardBindingSites.
   *
   * @param n the index number of the OutwardBindingSite to get.
   *
   * @return the nth OutwardBindingSite in this ListOfOutwardBindingSites.
   *
   * @see size()
   */
  virtual const OutwardBindingSite* get(unsigned int n) const;


  /**
   * Get a OutwardBindingSite from the ListOfOutwardBindingSites
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the OutwardBindingSite to get.
   *
   * @return OutwardBindingSite in this ListOfOutwardBindingSites
   * with the given id or NULL if no such
   * OutwardBindingSite exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual OutwardBindingSite* get(const std::string& sid);


  /**
   * Get a OutwardBindingSite from the ListOfOutwardBindingSites
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the OutwardBindingSite to get.
   *
   * @return OutwardBindingSite in this ListOfOutwardBindingSites
   * with the given id or NULL if no such
   * OutwardBindingSite exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const OutwardBindingSite* get(const std::string& sid) const;


  /**
   * Removes the nth OutwardBindingSite from this ListOfOutwardBindingSites
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the OutwardBindingSite to remove.
   *
   * @see size()
   */
  virtual OutwardBindingSite* remove(unsigned int n);


  /**
   * Removes the OutwardBindingSite from this ListOfOutwardBindingSites with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the OutwardBindingSite to remove.
   *
   * @return the OutwardBindingSite removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual OutwardBindingSite* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfOutwardBindingSites, is
   * always @c "listOfOutwardBindingSites".
   *
   * @return the name of this element, i.e. @c "listOfOutwardBindingSites".
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
   * Creates a new OutwardBindingSite in this ListOfOutwardBindingSites
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

BEGIN_C_DECLS


LIBSBML_EXTERN
const char* 
BindingStatus_toString(BindingStatus_t bindingStatus);


LIBSBML_EXTERN
BindingStatus_t 
BindingStatus_fromString(const char* s);


END_C_DECLS

LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
OutwardBindingSite_t *
OutwardBindingSite_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion);


LIBSBML_EXTERN
void
OutwardBindingSite_free(OutwardBindingSite_t * obs);


LIBSBML_EXTERN
OutwardBindingSite_t *
OutwardBindingSite_clone(OutwardBindingSite_t * obs);


LIBSBML_EXTERN
char *
OutwardBindingSite_getId(OutwardBindingSite_t * cr);


LIBSBML_EXTERN
char *
OutwardBindingSite_getName(OutwardBindingSite_t * cr);


LIBSBML_EXTERN
BindingStatus_t
OutwardBindingSite_getBindingStatus(OutwardBindingSite_t * obs);


LIBSBML_EXTERN
char *
OutwardBindingSite_getComponent(OutwardBindingSite_t * obs);


LIBSBML_EXTERN
int
OutwardBindingSite_isSetId(OutwardBindingSite_t * cr);


LIBSBML_EXTERN
int
OutwardBindingSite_isSetName(OutwardBindingSite_t * cr);


LIBSBML_EXTERN
int
OutwardBindingSite_isSetBindingStatus(OutwardBindingSite_t * obs);


LIBSBML_EXTERN
int
OutwardBindingSite_isSetComponent(OutwardBindingSite_t * obs);


LIBSBML_EXTERN
int
OutwardBindingSite_setId(OutwardBindingSite_t * cr, const char * id);


LIBSBML_EXTERN
int
OutwardBindingSite_setName(OutwardBindingSite_t * cr, const char * name);


LIBSBML_EXTERN
int
OutwardBindingSite_setBindingStatus(OutwardBindingSite_t * obs, 
                                    BindingStatus_t bindingStatus);

LIBSBML_EXTERN
int
OutwardBindingSite_setComponent(OutwardBindingSite_t * obs, const char * component);


LIBSBML_EXTERN
int
OutwardBindingSite_unsetId(OutwardBindingSite_t * cr);


LIBSBML_EXTERN
int
OutwardBindingSite_unsetName(OutwardBindingSite_t * cr);


LIBSBML_EXTERN
int
OutwardBindingSite_unsetBindingStatus(OutwardBindingSite_t * obs);


LIBSBML_EXTERN
int
OutwardBindingSite_unsetComponent(OutwardBindingSite_t * obs);


LIBSBML_EXTERN
int
OutwardBindingSite_hasRequiredAttributes(OutwardBindingSite_t * obs);


LIBSBML_EXTERN
OutwardBindingSite_t *
ListOfOutwardBindingSites_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
OutwardBindingSite_t *
ListOfOutwardBindingSites_removeById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
int 
OutwardBindingSite_isValidBindingStatus(BindingStatus_t bindigStatus);


LIBSBML_EXTERN
int 
OutwardBindingSite_isValidBindingStatusString(const char* s);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  OutwardBindingSite_H__  */

