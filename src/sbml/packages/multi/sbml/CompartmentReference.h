/**
 * @file:   CompartmentReference.h
 * @brief:  Implementation of the CompartmentReference class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
 *
 * @class CompartmentReference
 * @sbmlbrief{multi} Child of a Compartment that references a different Compartment.
 *
 * A CompartmentReference object is a child of an extended Compartment (via
 * the MultiCompartmentPlugin), and provides a way for that Compartment to
 * reference another Compartment, and indicates that the referenced
 * Compartment is a sub-compartment in a composite parent compartment.
 * Compartments may be arbitrarily nested in this way, but this nesting
 * cannot be circular.
 *
 * @class ListOfCompartmentReferences
 * @sbmlbrief{multi} A list of CompartmentReference objects.
 *
 * The ListOfCompartmentReferences is a container for CompartmentReference objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see CompartmentReference
 */

#ifndef CompartmentReference_H__
#define CompartmentReference_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CompartmentReference : public SBase
{

protected:
  /** @cond doxygenLibsbmlInternal */

  ////  std::string   mId;
  ////  std::string   mName;
  std::string   mCompartment;

  /** @endcond */


public:

  /**
   * Creates a new CompartmentReference.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CompartmentReference(unsigned int level      = MultiExtension::getDefaultLevel(),
                       unsigned int version    = MultiExtension::getDefaultVersion(),
                       unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new CompartmentReference with the given MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CompartmentReference(MultiPkgNamespaces* multins);


  /**
   * Copy constructor for CompartmentReference.
   *
   * @param orig the CompartmentReference instance to copy.
   */
  CompartmentReference(const CompartmentReference& orig);


  /**
   * Assignment operator for CompartmentReference.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  CompartmentReference& operator=(const CompartmentReference& rhs);


  /**
   * Creates and returns a deep copy of this CompartmentReference object.
   *
   * @return a (deep) copy of this CompartmentReference object.
   */
  virtual CompartmentReference* clone () const;


  /**
   * Destructor for CompartmentReference.
   */
  virtual ~CompartmentReference();


  /**
   * Returns the value of the "id" attribute of this CompartmentReference.
   *
   * @return the value of the "id" attribute of this CompartmentReference as
   * a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns @c true if this CompartmentReference's "id" attribute has been
   * set.
   *
   * @return @c true if this CompartmentReference's "id" attribute has been
   * set; otherwise, @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this CompartmentReference.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this CompartmentReference.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this CompartmentReference.
   *
   * @return the value of the "name" attribute of this CompartmentReference
   * as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this CompartmentReference's "name" attribute has been
   * set.
   *
   * @return @c true if this CompartmentReference's "name" attribute has been
   * set; otherwise, @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this CompartmentReference.
   *
   * @param name const std::string& value of the "name" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this CompartmentReference.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "compartment" attribute of this
   * CompartmentReference.
   *
   * @return the value of the "compartment" attribute of this
   * CompartmentReference as a string.
   */
  virtual const std::string& getCompartment() const;


  /**
   * Returns @c true if this CompartmentReference's "compartment" attribute
   * has been set.
   *
   * @return @c true if this CompartmentReference's "compartment" attribute
   * has been set; otherwise, @c false is returned.
   */
  virtual bool isSetCompartment() const;


  /**
   * Sets the value of the "compartment" attribute of this
   * CompartmentReference.
   *
   * @param compartment the new attribute value.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setCompartment(const std::string& compartment);


  /**
   * Unsets the value of the "compartment" attribute of this
   * CompartmentReference.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetCompartment();


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
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "compartmentReference".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_MULTI_BINDING_SITE_SPECIES_TYPE, SBMLMultiTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns @c true if all the required attributes for this
   * CompartmentReference object have been set.
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
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);
  /** @endcond */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond */


};

class LIBSBML_EXTERN ListOfCompartmentReferences : public ListOf
{

public:

  /**
   * Creates a new ListOfCompartmentReferences with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfCompartmentReferences
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfCompartmentReferences
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this ListOfCompartmentReferences
   */
  ListOfCompartmentReferences(unsigned int level      = MultiExtension::getDefaultLevel(),
                              unsigned int version    = MultiExtension::getDefaultVersion(),
                              unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCompartmentReferences with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  ListOfCompartmentReferences(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this ListOfCompartmentReferences object.
   *
   * @return a (deep) copy of this ListOfCompartmentReferences object.
   */
  virtual ListOfCompartmentReferences* clone () const;


   /**
   * Get a CompartmentReference from the ListOfCompartmentReferences.
   *
   * @param n the index number of the CompartmentReference to get.
   *
   * @return the nth CompartmentReference in this ListOfCompartmentReferences.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual CompartmentReference* get(unsigned int n);


  /**
   * Get a CompartmentReference from the ListOfCompartmentReferences.
   *
   * @param n the index number of the CompartmentReference to get.
   *
   * @return the nth CompartmentReference in this ListOfCompartmentReferences.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual const CompartmentReference* get(unsigned int n) const;


  /**
   * Get a CompartmentReference from the ListOfCompartmentReferences
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CompartmentReference to get.
   *
   * @return CompartmentReference in this ListOfCompartmentReferences
   * with the given id or @c NULL if no such
   * CompartmentReference exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual CompartmentReference* get(const std::string& sid);


  /**
   * Get a CompartmentReference from the ListOfCompartmentReferences
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the CompartmentReference to get.
   *
   * @return CompartmentReference in this ListOfCompartmentReferences
   * with the given id or @c NULL if no such
   * CompartmentReference exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const CompartmentReference* get(const std::string& sid) const;


  /**
   * Removes the nth CompartmentReference from this ListOfCompartmentReferences
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the CompartmentReference to remove.
   *
   * @see size()
   */
  virtual CompartmentReference* remove(unsigned int n);


  /**
   * Removes the CompartmentReference from this ListOfCompartmentReferences with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the CompartmentReference to remove.
   *
   * @return the CompartmentReference removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual CompartmentReference* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfCompartmentReferences, is
   * always @c "listOfCompartmentReferences".
   *
   * @return the name of this element, i.e. @c "listOfCompartmentReferences".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters
   * @c SBML_. @endif@if java LibSBML attaches an identifying code to every
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
   * The names of the type codes all begin with the characters
   * @c SBML_. @endif@if java LibSBML attaches an identifying code to every
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
   * Creates a new CompartmentReference in this ListOfCompartmentReferences
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Multi package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new CompartmentReference_t using the given SBML Level, Version and
 * &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CompartmentReference_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CompartmentReference_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * CompartmentReference_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
CompartmentReference_t *
CompartmentReference_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion);

/**
 * Frees this CompartmentReference_t object.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
void
CompartmentReference_free(CompartmentReference_t * cr);


/**
 * Creates and returns a deep copy of this CompartmentReference_t object.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @return a (deep) copy of this CompartmentReference_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
CompartmentReference_t *
CompartmentReference_clone(CompartmentReference_t * cr);


/**
 * Returns the value of the "id" attribute of this CompartmentReference_t.
 *
 * @param cr the CompartmentReference_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this CompartmentReference_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
char *
CompartmentReference_getId(const CompartmentReference_t * cr);


/**
 * Returns the value of the "name" attribute of this CompartmentReference_t.
 *
 * @param cr the CompartmentReference_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this CompartmentReference_t as
 * a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
char *
CompartmentReference_getName(const CompartmentReference_t * cr);


/**
 * Returns the value of the "compartment" attribute of this
 * CompartmentReference_t.
 *
 * @param cr the CompartmentReference_t structure whose compartment is sought.
 *
 * @return the value of the "compartment" attribute of this
 * CompartmentReference_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
char *
CompartmentReference_getCompartment(const CompartmentReference_t * cr);


/**
 * Predicate returning @c 1 (true) if this CompartmentReference_t's "id"
 * attribute is set.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @return @c 1 (true) if this CompartmentReference_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_isSetId(const CompartmentReference_t * cr);


/**
 * Predicate returning @c 1 (true) if this CompartmentReference_t's "name"
 * attribute is set.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @return @c 1 (true) if this CompartmentReference_t's "name" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_isSetName(const CompartmentReference_t * cr);


/**
 * Predicate returning @c 1 (true) if this CompartmentReference_t's
 * "compartment" attribute is set.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @return @c 1 (true) if this CompartmentReference_t's "compartment" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_isSetCompartment(const CompartmentReference_t * cr);


/**
 * Sets the value of the "id" attribute of this CompartmentReference_t.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling CompartmentReference_unsetId().
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_setId(CompartmentReference_t * cr, const char * id);


/**
 * Sets the value of the "name" attribute of this CompartmentReference_t.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling CompartmentReference_unsetName().
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_setName(CompartmentReference_t * cr, const char * name);


/**
 * Sets the value of the "compartment" attribute of this
 * CompartmentReference_t.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @param compartment const char * value of the "compartment" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_setCompartment(CompartmentReference_t * cr,
                                    const char * compartment);


/**
 * Unsets the value of the "id" attribute of this CompartmentReference_t.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_unsetId(CompartmentReference_t * cr);


/**
 * Unsets the value of the "name" attribute of this CompartmentReference_t.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_unsetName(CompartmentReference_t * cr);


/**
 * Unsets the value of the "compartment" attribute of this
 * CompartmentReference_t.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_unsetCompartment(CompartmentReference_t * cr);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CompartmentReference_t object have been set.
 *
 * @param cr the CompartmentReference_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CompartmentReference_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the CompartmentReference_t object are:
 * @li "compartment"
 *
 * @memberof CompartmentReference_t
 */
LIBSBML_EXTERN
int
CompartmentReference_hasRequiredAttributes(CompartmentReference_t * cr);


/**
 * Return the structure indicated by the given @p sid.
 *
 * @param lo the ListOf_t structure to use.
 *
 * @param sid a string matching the "id" attribute of the element sought.
 *
 * @return the structure for the given variable, or @c NULL if no such
 * object exists in the list.
 *
 * @memberof ListOfCompartmentReferences_t
 */
LIBSBML_EXTERN
CompartmentReference_t *
ListOfCompartmentReferences_getById(ListOf_t * lo, const char * sid);


/**
 * Removes the structure with the given @p sid
 * from the given list and returns a pointer to it.
 *
 * The caller owns the returned structure and is responsible for deleting it.
 *
 * @param lo the ListOf_t structure.
 * @param sid a string matching the "id" attribute of the element sought.
 *
 * @return the structure removed.  As mentioned above, the
 * caller owns the returned structure. @c NULL is returned if no 
 * structure with the "id" attribute exists in the given list.
 *
 * @memberof ListOfCompartmentReferences_t
 */
LIBSBML_EXTERN
CompartmentReference_t *
ListOfCompartmentReferences_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CompartmentReference_H__  */

