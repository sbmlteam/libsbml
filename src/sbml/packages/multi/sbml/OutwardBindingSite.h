/**
 * @file:   OutwardBindingSite.h
 * @brief:  Implementation of the OutwardBindingSite class
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
 * @class OutwardBindingSite
 * @sbmlbrief{multi} Defines a outward-facing binding site for a MultiSpeciesType.
 *
 * The OutwardBindingSite object is a child of a Species (via the
 * MultiSpeciesPlugin).  It has two optional attributes, "id" and "name", and
 * two required attributes, "bindingStatus" and "component". A binding site
 * not involved in any InSpeciesTypeBond object in the MultiSpeciesType
 * referenced by a Species is an OutwardBindingSite. The bindingStatus
 * attribute is of type BindingStatus_t.  The component attribute,
 * of type SIdRef, references a component which ultimately references a
 * BindingSiteSpeciesType object. The attribute value must be the identifier
 * of a SpeciesTypeInstance, SpeciesTypeComponentIndex or MultiSpeciesType
 * object. An OutwardBindingSite cannot be a binding site referenced by any
 * InSpeciesTypeBond in the species. There are three scenarios for the
 * component attribute to have the value of an identifier of
 * MultiSpeciesType, SpeciesTypeInstance, or SpeciesTypeComponentIndex
 * respectively:
 * <ul>
 * <li> When a Species references a simple BindingSiteSpeciesType, the value
 * of the component attribute of the OutwardBindingSite of the Species can
 * only be the id of the referenced MultiSpeciesType.
 * <li> When a Species references a MultiSpeciesType with a
 * SpeciesTypeInstance being a binding site (have an id of
 * BindingSiteSpeciesType as its "speciesType" attribute) and the id of the
 * SpeciesTypeInstance can identify the binding site within the
 * MultiSpeciesType (referenced by the Species) unambiguously, and therefore,
 * the value of the component attribute of an OutwardBindingSite of the
 * species can be the id of the SpeciesTypeInstance.
 * <li> When a Species references a MultiSpeciesType with a
 * SpeciesTypeInstance being a binding site (directly or indirectly) and id of
 * the SpeciesTypeInstance can NOT identify the binding site without
 * ambiguity, an id of SpeciesTypeComponentIndex can be used as the value of
 * the component attribute of an OutwardBindingSite of the Species.
 * </ul>
 *
 * @class ListOfOutwardBindingSites
 * @sbmlbrief{multi} A list of OutwardBindingSite objects.
 *
 * The ListOfOutwardBindingSites is a container for OutwardBindingSite objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see OutwardBindingSite
 */


 /**
 * <!-- ~ ~ ~ ~ ~ Start of common documentation strings ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 * The following text is used as common documentation blocks copied multiple
 * times elsewhere in this file. The use of @class is a hack needed because
 * Doxygen's @copydetails command has limited functionality. Symbols
 * beginning with "doc_" are marked as ignored in our Doxygen configuration.
 * ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ -->
 *
 *
 * @class doc_outwardbindingsite_bindingStatus
 *
 * @par
 * The attribute "bindingStatus" on a OutwardBindingSite object is used to
 * describe the status of the binding site.
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Multi specification, the following are the
 * allowable values for "bindingStatus":
 * <ul>
 * <li> @c "bound", the binding site is bound.
 *
 * <li> @c "unbound", the binding site is not bound.
 *
 * <li> @c "either", the binding site may either be bound or unbound.
 *
 * </ul>
 */


#ifndef OutwardBindingSite_H__
#define OutwardBindingSite_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>

/**
 * @enum  BindingStatus_t
 * @brief Enumeration of possible binding status of a OutwardBindingSite in
 * the libSBML "multi" package implementation.
 *
 * @copydetails doc_what_are_typecodes
 *
 * @copydetails doc_additional_typecode_details
 */
typedef enum
{
    MULTI_BINDING_STATUS_BOUND /** The status of the OutwardBindingSite is 'bound'. */
  , MULTI_BINDING_STATUS_UNBOUND /** The status of the OutwardBindingSite is 'unbound'. */
  , MULTI_BINDING_STATUS_EITHER /** The status of the OutwardBindingSite is either 'bound' or 'unbound'. */
  , MULTI_BINDING_STATUS_UNKNOWN /** The status of the OutwardBindingSite is unknown.  This value is not permitted for valid SBML models. */
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

  /** @cond doxygenLibsbmlInternal */

  BindingStatus_t   mBindingStatus;
  std::string   mComponent;

  /** @endcond */


public:

  /**
   * Creates a new OutwardBindingSite object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  OutwardBindingSite(unsigned int level      = MultiExtension::getDefaultLevel(),
                     unsigned int version    = MultiExtension::getDefaultVersion(),
                     unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new OutwardBindingSite with the given MultiPkgNamespaces
   * object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  OutwardBindingSite(MultiPkgNamespaces* multins);


  /**
   * Copy constructor for OutwardBindingSite.
   *
   * @param orig the OutwardBindingSite instance to copy.
   */
  OutwardBindingSite(const OutwardBindingSite& orig);


  /**
   * Assignment operator for OutwardBindingSite.
   *
   * @param rhs the object whose values are used as the basis
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
   * Returns @c true if this OutwardBindingSite's "id" attribute has been
   * set.
   *
   * @return @c true if this OutwardBindingSite's "id" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this OutwardBindingSite.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this OutwardBindingSite.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this OutwardBindingSite.
   *
   * @return the value of the "name" attribute of this OutwardBindingSite as
   * a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this OutwardBindingSite's "name" attribute has been
   * set.
   *
   * @return @c true if this OutwardBindingSite's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this OutwardBindingSite.
   *
   * @param name const std::string& value of the "name" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this OutwardBindingSite.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


   /**
   * Returns the value of the "bindingStatus" attribute of this
   * OutwardBindingSite.
   *
   * @return the value of the "bindingStatus" attribute of this
   * OutwardBindingSite.
   */
  virtual BindingStatus_t getBindingStatus() const;


  /**
   * Returns @c true if this OutwardBindingSite's "bindingStatus" attribute
   * has been set.
   *
   * @return @c true if this OutwardBindingSite's "bindingStatus" attribute
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetBindingStatus() const;


  /**
   * Sets the value of the "bindingStatus" attribute of this
   * OutwardBindingSite.
   *
   * @param bindingStatus the new value of the "bindingStatus" attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setBindingStatus(BindingStatus_t bindingStatus);


  /**
   * Sets the value of the "bindingStatus" attribute of this
   * OutwardBindingSite.
   *
   * @param bindingStatus std::string& of the "bindingStatus" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_outwardbindingsite_bindingStatus
   */
  int setBindingStatus(const std::string& bindingStatus);


  /**
   * Unsets the value of the "bindingStatus" attribute of this
   * OutwardBindingSite.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetBindingStatus();


  /**
   * Returns the value of the "component" attribute of this
   * OutwardBindingSite.
   *
   * @return the value of the "component" attribute of this
   * OutwardBindingSite as a string.
   */
  virtual const std::string& getComponent() const;


  /**
   * Returns @c true if this OutwardBindingSite's "component" attribute has
   * been set.
   *
   * @return @c true if this OutwardBindingSite's "component" attribute has
   * been set; otherwise, @c false is returned.
   */
  virtual bool isSetComponent() const;


  /**
   * Sets the value of the "component" attribute of this OutwardBindingSite.
   *
   * @param component const std::string& value of the "component" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setComponent(const std::string& component);


  /**
   * Unsets the value of the "component" attribute of this OutwardBindingSite.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
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
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "outwardBindingSite".
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
   * Creates a new ListOfOutwardBindingSites with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfOutwardBindingSites(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this ListOfOutwardBindingSites
   * object.
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
   * If the index @p n is invalid, @c NULL is returned.
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
   * If the index @p n is invalid, @c NULL is returned.
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
   * with the given id or @c NULL if no such
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
   * with the given id or @c NULL if no such
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
   * Creates a new OutwardBindingSite in this ListOfOutwardBindingSites
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

/**
 * Creates a new OutwardBindingSite_t using the given SBML Level, Version and
 * &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * OutwardBindingSite_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * OutwardBindingSite_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * OutwardBindingSite_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
OutwardBindingSite_t *
OutwardBindingSite_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion);


/**
 * Frees this OutwardBindingSite_t object.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
void
OutwardBindingSite_free(OutwardBindingSite_t * obs);


/**
 * Creates and returns a deep copy of this OutwardBindingSite_t object.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @return a (deep) copy of this OutwardBindingSite_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
OutwardBindingSite_t *
OutwardBindingSite_clone(OutwardBindingSite_t * obs);


/**
 * Returns the value of the "id" attribute of this OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this OutwardBindingSite_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
char *
OutwardBindingSite_getId(const OutwardBindingSite_t * obs);


/**
 * Returns the value of the "name" attribute of this OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this OutwardBindingSite_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
char *
OutwardBindingSite_getName(const OutwardBindingSite_t * obs);


/**
 * Returns the value of the "bindingStatus" attribute of this
 * OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure whose bindingStatus is sought.
 *
 * @return the value of the "bindingStatus" attribute of this
 * OutwardBindingSite_t as a BindingStatus_t.
 *
 * @copydetails doc_outwardbindingsite_bindingStatus
 * @if clike The value is drawn from the enumeration @ref BindingStatus_t @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{MULTI_BINDING_STATUS_BOUND, BindingStatus_t}
 * @li @sbmlconstant{MULTI_BINDING_STATUS_UNBOUND, BindingStatus_t}
 * @li @sbmlconstant{MULTI_BINDING_STATUS_EITHER, BindingStatus_t}
 * @li @sbmlconstant{MULTI_BINDING_STATUS_UNKNOWN,
 * BindingStatus_t}
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
BindingStatus_t
OutwardBindingSite_getBindingStatus(const OutwardBindingSite_t * obs);


/**
 * Returns the value of the "bindingStatus" attribute of this
 * OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure whose bindingStatus is sought.
 *
 * @return the value of the "bindingStatus" attribute of this
 * OutwardBindingSite_t as a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_outwardbindingsite_bindingStatus
 * The possible values returned by this method are:
 * @li @c "bound"
 * @li @c "unbound"
 * @li @c "either"
 * @li @c NULL
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
const char *
OutwardBindingSite_getBindingStatusAsString(const OutwardBindingSite_t * obs);


/**
 * Returns the value of the "component" attribute of this OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure whose component is sought.
 *
 * @return the value of the "component" attribute of this OutwardBindingSite_t
 * as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
char *
OutwardBindingSite_getComponent(const OutwardBindingSite_t * obs);


/**
 * Predicate returning @c 1 (true) if this OutwardBindingSite_t's "id"
 * attribute is set.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @return @c 1 (true) if this OutwardBindingSite_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_isSetId(const OutwardBindingSite_t * obs);


/**
 * Predicate returning @c 1 (true) if this OutwardBindingSite_t's "name"
 * attribute is set.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @return @c 1 (true) if this OutwardBindingSite_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_isSetName(const OutwardBindingSite_t * obs);


/**
 * Predicate returning @c 1 (true) if this OutwardBindingSite_t's
 * "bindingStatus" attribute is set.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @return @c 1 (true) if this OutwardBindingSite_t's "bindingStatus" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_outwardbindingsite_bindingStatus
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_isSetBindingStatus(const OutwardBindingSite_t * obs);


/**
 * Predicate returning @c 1 (true) if this OutwardBindingSite_t's "component"
 * attribute is set.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @return @c 1 (true) if this OutwardBindingSite_t's "component" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_isSetComponent(const OutwardBindingSite_t * obs);


/**
 * Sets the value of the "id" attribute of this OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling OutwardBindingSite_unsetId().
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_setId(OutwardBindingSite_t * obs, const char * id);


/**
 * Sets the value of the "name" attribute of this OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling OutwardBindingSite_unsetName().
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_setName(OutwardBindingSite_t * obs, const char * name);


/**
 * Sets the value of the "bindingStatus" attribute of this
 * OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @param bindingStatus BindingStatus_t value of the "bindingStatus" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_outwardbindingsite_bindingStatus
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_setBindingStatus(OutwardBindingSite_t * obs,
                                    BindingStatus_t bindingStatus);


/**
 * Sets the value of the "bindingStatus" attribute of this
 * OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @param bindingStatus const char * of the "bindingStatus" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_outwardbindingsite_bindingStatus
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_setBindingStatusAsString(OutwardBindingSite_t * obs,
                                            const char * bindingStatus);


/**
 * Sets the value of the "component" attribute of this OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @param component const char * value of the "component" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_setComponent(OutwardBindingSite_t * obs,
                                const char * component);


/**
 * Unsets the value of the "id" attribute of this OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_unsetId(OutwardBindingSite_t * obs);


/**
 * Unsets the value of the "name" attribute of this OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_unsetName(OutwardBindingSite_t * obs);


/**
 * Unsets the value of the "bindingStatus" attribute of this
 * OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_outwardbindingsite_bindingStatus
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_unsetBindingStatus(OutwardBindingSite_t * obs);


/**
 * Unsets the value of the "component" attribute of this OutwardBindingSite_t.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_unsetComponent(OutwardBindingSite_t * obs);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * OutwardBindingSite_t object have been set.
 *
 * @param obs the OutwardBindingSite_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * OutwardBindingSite_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the OutwardBindingSite_t object are:
 * @li "bindingStatus"
 * @li "component"
 *
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int
OutwardBindingSite_hasRequiredAttributes(OutwardBindingSite_t * obs);


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
 * @memberof ListOfOutwardBindingSites_t
 */
LIBSBML_EXTERN
OutwardBindingSite_t *
ListOfOutwardBindingSites_getById(ListOf_t * lo, const char * sid);


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
 * @memberof ListOfOutwardBindingSites_t
 */
LIBSBML_EXTERN
OutwardBindingSite_t *
ListOfOutwardBindingSites_removeById(ListOf_t * lo, const char * sid);


/**
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int 
OutwardBindingSite_isValidBindingStatus(BindingStatus_t bindigStatus);


/**
 * @memberof OutwardBindingSite_t
 */
LIBSBML_EXTERN
int 
OutwardBindingSite_isValidBindingStatusString(const char* s);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  OutwardBindingSite_H__  */

