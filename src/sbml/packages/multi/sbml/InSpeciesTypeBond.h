/**
 * @file:   InSpeciesTypeBond.h
 * @brief:  Implementation of the InSpeciesTypeBond class
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
 * @class InSpeciesTypeBond
 * @sbmlbrief{multi} Defines a bond within a MultiSpeciesType.
 *
 * The InSpeciesTypeBond object is a child of MultiSpeciesType, and defines a
 * bond existing within that MultiSpeciesType. The bond therefore exists in
 * every species that references the MultiSpeciesType.  The binding
 * relationship in an InSpeciesTypeBond is one-to-one. The uniqueness of an
 * InSpeciesTypeBond is ensured by the pair of referenced attributes
 * "bindingSite1" and "bindingSite2", both of type SBaseRef.  The referenced
 * identifiers of the binding sites can be the ids of SpeciesTypeInstance
 * objects (binding sites), or the ids of the SpeciesTypeComponentIndex
 * objects indexing the binding sites and the ultimately referenced
 * components must be the BindingSiteSpeciesType objects. Obviously,
 * attributes "bindingSite1" and "bindingSite2" must not reference the same
 * BindingSiteSpeciesType object.
 *
 * @class ListOfInSpeciesTypeBonds
 * @sbmlbrief{multi} A list of InSpeciesTypeBond objects.
 *
 * The ListOfInSpeciesTypeBonds is a container for InSpeciesTypeBond objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see InSpeciesTypeBond
 */


#ifndef InSpeciesTypeBond_H__
#define InSpeciesTypeBond_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN InSpeciesTypeBond : public SBase
{

protected:
  /** @cond doxygenLibsbmlInternal */

  ////  std::string   mId;
  ////  std::string   mName;
  std::string   mBindingSite1;
  std::string   mBindingSite2;

  /** @endcond */


public:

  /**
   * Creates a new InSpeciesTypeBond object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  InSpeciesTypeBond(unsigned int level      = MultiExtension::getDefaultLevel(),
                    unsigned int version    = MultiExtension::getDefaultVersion(),
                    unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new InSpeciesTypeBond with the given MultiPkgNamespaces
   * object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  InSpeciesTypeBond(MultiPkgNamespaces* multins);


  /**
   * Copy constructor for InSpeciesTypeBond.
   *
   * @param orig the InSpeciesTypeBond instance to copy.
   */
  InSpeciesTypeBond(const InSpeciesTypeBond& orig);


  /**
   * Assignment operator for InSpeciesTypeBond.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  InSpeciesTypeBond& operator=(const InSpeciesTypeBond& rhs);


  /**
   * Creates and returns a deep copy of this InSpeciesTypeBond object.
   *
   * @return a (deep) copy of this InSpeciesTypeBond object.
   */
  virtual InSpeciesTypeBond* clone () const;


  /**
   * Destructor for InSpeciesTypeBond.
   */
  virtual ~InSpeciesTypeBond();


  /**
   * Returns the value of the "id" attribute of this InSpeciesTypeBond.
   *
   * @return the value of the "id" attribute of this InSpeciesTypeBond as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns @c true if this InSpeciesTypeBond's "id" attribute has been set.
   *
   * @return @c true if this InSpeciesTypeBond's "id" attribute has been set;
   * otherwise, @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this InSpeciesTypeBond.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this InSpeciesTypeBond.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this InSpeciesTypeBond.
   *
   * @return the value of the "name" attribute of this InSpeciesTypeBond as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this InSpeciesTypeBond's "name" attribute has been
   * set.
   *
   * @return @c true if this InSpeciesTypeBond's "name" attribute has been
   * set; otherwise, @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this InSpeciesTypeBond.
   *
   * @param name const std::string& value of the "name" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this InSpeciesTypeBond.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "bindingSite1" attribute of this
   * InSpeciesTypeBond.
   *
   * @return the value of the "bindingSite1" attribute of this
   * InSpeciesTypeBond as a string.
   */
  virtual const std::string& getBindingSite1() const;


  /**
   * Returns @c true if this InSpeciesTypeBond's "bindingSite1" attribute has
   * been set.
   *
   * @return @c true if this InSpeciesTypeBond's "bindingSite1" attribute has
   * been set; otherwise, @c false is returned.
   */
  virtual bool isSetBindingSite1() const;


  /**
   * Sets the value of the "bindingSite1" attribute of this InSpeciesTypeBond.
   *
   * @param bindingSite1 the new value for the "bindingSite1" attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setBindingSite1(const std::string& bindingSite1);


  /**
   * Unsets the value of the "bindingSite1" attribute of this
   * InSpeciesTypeBond.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetBindingSite1();


  /**
   * Returns the value of the "bindingSite2" attribute of this
   * InSpeciesTypeBond.
   *
   * @return the value of the "bindingSite2" attribute of this
   * InSpeciesTypeBond as a string.
   */
  virtual const std::string& getBindingSite2() const;


  /**
   * Returns @c true if this InSpeciesTypeBond's "bindingSite2" attribute has
   * been set.
   *
   * @return @c true if this InSpeciesTypeBond's "bindingSite2" attribute has
   * been set; otherwise, @c false is returned.
   */
  virtual bool isSetBindingSite2() const;


  /**
   * Sets the value of the "bindingSite2" attribute of this
   * InSpeciesTypeBond.
   *
   * @param bindingSite2 the new value of the "bindingSite2" attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setBindingSite2(const std::string& bindingSite2);


  /**
   * Unsets the value of the "bindingSite2" attribute of this InSpeciesTypeBond.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetBindingSite2();


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
   * @return the name of this element, i.e. @c "inSpeciesTypeBond".
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
   * for this InSpeciesTypeBond object have been set.
   *
   * @note The required attributes for a InSpeciesTypeBond object are:
   * @li "bindingSite1"
   * @li "bindingSite2"
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

class LIBSBML_EXTERN ListOfInSpeciesTypeBonds : public ListOf
{

public:

  /**
   * Creates a new ListOfInSpeciesTypeBonds with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfInSpeciesTypeBonds
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfInSpeciesTypeBonds
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this ListOfInSpeciesTypeBonds
   */
  ListOfInSpeciesTypeBonds(unsigned int level      = MultiExtension::getDefaultLevel(),
                           unsigned int version    = MultiExtension::getDefaultVersion(),
                           unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfInSpeciesTypeBonds with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  ListOfInSpeciesTypeBonds(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this ListOfInSpeciesTypeBonds object.
   *
   * @return a (deep) copy of this ListOfInSpeciesTypeBonds object.
   */
  virtual ListOfInSpeciesTypeBonds* clone () const;


   /**
   * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds.
   *
   * @param n the index number of the InSpeciesTypeBond to get.
   *
   * @return the nth InSpeciesTypeBond in this ListOfInSpeciesTypeBonds.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual InSpeciesTypeBond* get(unsigned int n);


  /**
   * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds.
   *
   * @param n the index number of the InSpeciesTypeBond to get.
   *
   * @return the nth InSpeciesTypeBond in this ListOfInSpeciesTypeBonds.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual const InSpeciesTypeBond* get(unsigned int n) const;


  /**
   * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the InSpeciesTypeBond to get.
   *
   * @return InSpeciesTypeBond in this ListOfInSpeciesTypeBonds
   * with the given id or @c NULL if no such
   * InSpeciesTypeBond exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual InSpeciesTypeBond* get(const std::string& sid);


  /**
   * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the InSpeciesTypeBond to get.
   *
   * @return InSpeciesTypeBond in this ListOfInSpeciesTypeBonds
   * with the given id or @c NULL if no such
   * InSpeciesTypeBond exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const InSpeciesTypeBond* get(const std::string& sid) const;


  /**
   * Removes the nth InSpeciesTypeBond from this ListOfInSpeciesTypeBonds
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the InSpeciesTypeBond to remove.
   *
   * @see size()
   */
  virtual InSpeciesTypeBond* remove(unsigned int n);


  /**
   * Removes the InSpeciesTypeBond from this ListOfInSpeciesTypeBonds with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the InSpeciesTypeBond to remove.
   *
   * @return the InSpeciesTypeBond removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual InSpeciesTypeBond* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfInSpeciesTypeBonds, is
   * always @c "listOfInSpeciesTypeBonds".
   *
   * @return the name of this element, i.e. @c "listOfInSpeciesTypeBonds".
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
   * Creates a new InSpeciesTypeBond in this ListOfInSpeciesTypeBonds
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
 * Creates a new InSpeciesTypeBond_t using the given SBML Level, Version and
 * &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * InSpeciesTypeBond_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * InSpeciesTypeBond_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * InSpeciesTypeBond_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t *
InSpeciesTypeBond_create(unsigned int level, unsigned int version,
                         unsigned int pkgVersion);


/**
 * Frees this InSpeciesTypeBond_t object.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
void
InSpeciesTypeBond_free(InSpeciesTypeBond_t * istb);


/**
 * Creates and returns a deep copy of this InSpeciesTypeBond_t object.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @return a (deep) copy of this InSpeciesTypeBond_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t *
InSpeciesTypeBond_clone(InSpeciesTypeBond_t * istb);


/**
 * Returns the value of the "id" attribute of this InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this InSpeciesTypeBond_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
char *
InSpeciesTypeBond_getId(const InSpeciesTypeBond_t * istb);


/**
 * Returns the value of the "name" attribute of this InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this InSpeciesTypeBond_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
char *
InSpeciesTypeBond_getName(const InSpeciesTypeBond_t * istb);


/**
 * Returns the value of the "bindingSite1" attribute of this
 * InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure whose bindingSite1 is sought.
 *
 * @return the value of the "bindingSite1" attribute of this
 * InSpeciesTypeBond_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
char *
InSpeciesTypeBond_getBindingSite1(const InSpeciesTypeBond_t * istb);


/**
 * Returns the value of the "bindingSite2" attribute of this
 * InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure whose bindingSite2 is sought.
 *
 * @return the value of the "bindingSite2" attribute of this
 * InSpeciesTypeBond_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
char *
InSpeciesTypeBond_getBindingSite2(const InSpeciesTypeBond_t * istb);


/**
 * Predicate returning @c 1 (true) if this InSpeciesTypeBond_t's "id" attribute
 * is set.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @return @c 1 (true) if this InSpeciesTypeBond_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_isSetId(const InSpeciesTypeBond_t * istb);


/**
 * Predicate returning @c 1 (true) if this InSpeciesTypeBond_t's "name"
 * attribute is set.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @return @c 1 (true) if this InSpeciesTypeBond_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_isSetName(const InSpeciesTypeBond_t * istb);


/**
 * Predicate returning @c 1 (true) if this InSpeciesTypeBond_t's "bindingSite1"
 * attribute is set.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @return @c 1 (true) if this InSpeciesTypeBond_t's "bindingSite1" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_isSetBindingSite1(const InSpeciesTypeBond_t * istb);


/**
 * Predicate returning @c 1 (true) if this InSpeciesTypeBond_t's "bindingSite2"
 * attribute is set.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @return @c 1 (true) if this InSpeciesTypeBond_t's "bindingSite2" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_isSetBindingSite2(const InSpeciesTypeBond_t * istb);


/**
 * Sets the value of the "id" attribute of this InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling InSpeciesTypeBond_unsetId().
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_setId(InSpeciesTypeBond_t * istb, const char * id);


/**
 * Sets the value of the "name" attribute of this InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling InSpeciesTypeBond_unsetName().
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_setName(InSpeciesTypeBond_t * istb, const char * name);


/**
 * Sets the value of the "bindingSite1" attribute of this InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @param bindingSite1 const char * value of the "bindingSite1" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_setBindingSite1(InSpeciesTypeBond_t * istb,
                                  const char * bindingSite1);


/**
 * Sets the value of the "bindingSite2" attribute of this InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @param bindingSite2 const char * value of the "bindingSite2" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_setBindingSite2(InSpeciesTypeBond_t * istb,
                                  const char * bindingSite2);


/**
 * Unsets the value of the "id" attribute of this InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_unsetId(InSpeciesTypeBond_t * istb);


/**
 * Unsets the value of the "name" attribute of this InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_unsetName(InSpeciesTypeBond_t * istb);


/**
 * Unsets the value of the "bindingSite1" attribute of this
 * InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_unsetBindingSite1(InSpeciesTypeBond_t * istb);


/**
 * Unsets the value of the "bindingSite2" attribute of this
 * InSpeciesTypeBond_t.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_unsetBindingSite2(InSpeciesTypeBond_t * istb);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * InSpeciesTypeBond_t object have been set.
 *
 * @param istb the InSpeciesTypeBond_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * InSpeciesTypeBond_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the InSpeciesTypeBond_t object are:
 * @li "bindingSite1"
 * @li "bindingSite2"
 *
 * @memberof InSpeciesTypeBond_t
 */
LIBSBML_EXTERN
int
InSpeciesTypeBond_hasRequiredAttributes(InSpeciesTypeBond_t * istb);


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
 * @memberof ListOfInSpeciesTypeBonds_t
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t *
ListOfInSpeciesTypeBonds_getById(ListOf_t * lo, const char * sid);


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
 * @memberof ListOfInSpeciesTypeBonds_t
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t *
ListOfInSpeciesTypeBonds_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  InSpeciesTypeBond_H__  */

