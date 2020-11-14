/**
 * @file:   SpeciesTypeComponentIndex.h
 * @brief:  Implementation of the SpeciesTypeComponentIndex class
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
 * @class SpeciesTypeComponentIndex
 * @sbmlbrief{multi} Identifies a component within a MultiSpeciesType.
 *
 * The SpeciesTypeComponentIndex object is a child of MultiSpeciesType, and
 * provides a way to identify or index a component within that
 * MultiSpeciesType. A SpeciesTypeComponentIndex object can be referenced by
 * other class objects, such as InSpeciesTypeBond, OutwardBindingSite,
 * SpeciesFeature or SpeciesTypeComponentMapInProduct objects, which need to
 * identify a component in a particular MultiSpeciesType.  A
 * SpeciesTypeComponentIndex should be unambiguous. For example, a
 * SpeciesTypeComponentIndex should not reference a MultiSpeciesType which is
 * referenced by two SpeciesTypeInstance objects contained in the same
 * MultiSpeciesType object.
 *
 * @class ListOfSpeciesTypeComponentIndexes
 * @sbmlbrief{multi} A list of SpeciesTypeComponentIndex objects.
 *
 * The ListOfSpeciesTypeComponentIndexes is a container for SpeciesTypeComponentIndex objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see SpeciesTypeComponentIndex
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

  /** @cond doxygenLibsbmlInternal */

////  std::string   mId;
  std::string   mComponent;
  std::string   mIdentifyingParent;

  /** @endcond */


public:

  /**
   * Creates a new SpeciesTypeComponentIndex object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesTypeComponentIndex(unsigned int level      = MultiExtension::getDefaultLevel(),
                            unsigned int version    = MultiExtension::getDefaultVersion(),
                            unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpeciesTypeComponentIndex with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesTypeComponentIndex(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for SpeciesTypeComponentIndex.
   *
   * @param orig the SpeciesTypeComponentIndex instance to copy.
   */
  SpeciesTypeComponentIndex(const SpeciesTypeComponentIndex& orig);


  /**
   * Assignment operator for SpeciesTypeComponentIndex.
   *
   * @param rhs the object whose values are used as the basis
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
   * Returns the value of the "id" attribute of this
   * SpeciesTypeComponentIndex.
   *
   * @return the value of the "id" attribute of this SpeciesTypeComponentIndex as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns @c true if this SpeciesTypeComponentIndex's "id" attribute has
   * been set.
   *
   * @return @c true if this SpeciesTypeComponentIndex's "id" attribute has
   * been set; otherwise, @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this SpeciesTypeComponentIndex.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this SpeciesTypeComponentIndex.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this
   * SpeciesTypeComponentIndex.
   *
   * @return the value of the "name" attribute of this
   * SpeciesTypeComponentIndex as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this SpeciesTypeComponentIndex's "name" attribute has
   * been set.
   *
   * @return @c true if this SpeciesTypeComponentIndex's "name" attribute has
   * been set; otherwise, @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this SpeciesTypeComponentIndex.
   *
   * @param name const std::string& value of the "name" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this SpeciesTypeComponentIndex.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "component" attribute of this
   * SpeciesTypeComponentIndex.
   *
   * @return the value of the "component" attribute of this
   * SpeciesTypeComponentIndex as a string.
   */
  virtual const std::string& getComponent() const;


  /**
   * Returns @c true if this SpeciesTypeComponentIndex's "component"
   * attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentIndex's "component"
   * attribute has been set; otherwise, @c false is returned.
   */
  virtual bool isSetComponent() const;


  /**
   * Sets the value of the "component" attribute of this
   * SpeciesTypeComponentIndex.
   *
   * @param component const std::string& value of the "component" attribute
   * to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setComponent(const std::string& component);


  /**
   * Unsets the value of the "component" attribute of this
   * SpeciesTypeComponentIndex.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetComponent();


  /**
   * Returns the value of the "identifyingParent" attribute of this
   * SpeciesTypeComponentIndex.
   *
   * @return the value of the "identifyingParent" attribute of this
   * SpeciesTypeComponentIndex as a string.
   */
  virtual const std::string& getIdentifyingParent() const;


  /**
   * Returns @c true if this SpeciesTypeComponentIndex's "identifyingParent"
   * attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentIndex's "identifyingParent"
   * attribute has been set, otherwise @c false is returned.
   */
  virtual bool isSetIdentifyingParent() const;


  /**
   * Sets the value of the "identifyingParent" attribute of this
   * SpeciesTypeComponentIndex.
   *
   * @param identifyingParent const std::string& value of the
   * "identifyingParent" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setIdentifyingParent(const std::string& identifyingParent);


  /**
   * Unsets the value of the "identifyingParent" attribute of this
   * SpeciesTypeComponentIndex.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
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
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "speciesTypeComponentIndex".
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
   * for this SpeciesTypeComponentIndex object have been set.
   *
   * @note The required attributes for a SpeciesTypeComponentIndex object are:
   * @li "id"
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
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond */


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

class LIBSBML_EXTERN ListOfSpeciesTypeComponentIndexes : public ListOf
{

public:

  /**
   * Creates a new ListOfSpeciesTypeComponentIndexes object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesTypeComponentIndexes(unsigned int level      = MultiExtension::getDefaultLevel(),
                                   unsigned int version    = MultiExtension::getDefaultVersion(),
                                   unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpeciesTypeComponentIndexes with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesTypeComponentIndexes(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this
   * ListOfSpeciesTypeComponentIndexes object.
   *
   * @return a (deep) copy of this ListOfSpeciesTypeComponentIndexes object.
   */
  virtual ListOfSpeciesTypeComponentIndexes* clone () const;


   /**
   * Get the nth SpeciesTypeComponentIndex object from the
   * ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index number of the SpeciesTypeComponentIndex to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual SpeciesTypeComponentIndex* get(unsigned int n);


  /**
   * Get the nth SpeciesTypeComponentIndex object from the
   * ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index number of the SpeciesTypeComponentIndex to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual const SpeciesTypeComponentIndex* get(unsigned int n) const;


  /**
   * Get the SpeciesTypeComponentIndex object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentIndex to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual SpeciesTypeComponentIndex* get(const std::string& sid);


  /**
   * Get the SpeciesTypeComponentIndex object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentIndex to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SpeciesTypeComponentIndex* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesTypeComponentIndex object from this
   * ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index of the SpeciesTypeComponentIndex to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see size()
   */
  virtual SpeciesTypeComponentIndex* remove(unsigned int n);


  /**
   * Removes the SpeciesTypeComponentIndex object with the given identifier
   * @p sid.
   *
   * @param sid the identifier of the SpeciesTypeComponentIndex to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  virtual SpeciesTypeComponentIndex* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e.
   * @c "listOfSpeciesTypeComponentIndexes".
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
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., Compartment objects, if the list is non-empty).
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for the objects contained in this ListOf
   * instance: @sbmlconstant{SBML_COMPARTMENT, SBMLTypeCode_t} (default).
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new SpeciesTypeComponentIndex in this ListOfSpeciesTypeComponentIndexes
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
 * Creates a new SpeciesTypeComponentIndex_t using the given SBML Level,
 * Version and &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SpeciesTypeComponentIndex_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesTypeComponentIndex_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * SpeciesTypeComponentIndex_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
SpeciesTypeComponentIndex_create(unsigned int level, unsigned int version,
                                 unsigned int pkgVersion);


/**
 * Frees this SpeciesTypeComponentIndex_t object.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
void
SpeciesTypeComponentIndex_free(SpeciesTypeComponentIndex_t * stci);


/**
 * Creates and returns a deep copy of this SpeciesTypeComponentIndex_t object.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @return a (deep) copy of this SpeciesTypeComponentIndex_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
SpeciesTypeComponentIndex_clone(SpeciesTypeComponentIndex_t * stci);


/**
 * Returns the value of the "id" attribute of this SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this SpeciesTypeComponentIndex_t
 * as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getId(SpeciesTypeComponentIndex_t * stci);


/**
 * Returns the value of the "name" attribute of this
 * SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this
 * SpeciesTypeComponentIndex_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getName(SpeciesTypeComponentIndex_t * stci);


/**
 * Returns the value of the "component" attribute of this
 * SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure whose component is
 * sought.
 *
 * @return the value of the "component" attribute of this
 * SpeciesTypeComponentIndex_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getComponent(SpeciesTypeComponentIndex_t * stci);


/**
 * Returns the value of the "identifyingParent" attribute of this
 * SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure whose
 * identifyingParent is sought.
 *
 * @return the value of the "identifyingParent" attribute of this
 * SpeciesTypeComponentIndex_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeComponentIndex_getIdentifyingParent(SpeciesTypeComponentIndex_t * stci);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeComponentIndex_t's "id"
 * attribute is set.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeComponentIndex_t's "id" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetId(SpeciesTypeComponentIndex_t * stci);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeComponentIndex_t's "name"
 * attribute is set.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeComponentIndex_t's "name" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetName(SpeciesTypeComponentIndex_t * stci);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeComponentIndex_t's
 * "component" attribute is set.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeComponentIndex_t's "component"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetComponent(SpeciesTypeComponentIndex_t * stci);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeComponentIndex_t's
 * "identifyingParent" attribute is set.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeComponentIndex_t's
 * "identifyingParent" attribute has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_isSetIdentifyingParent(SpeciesTypeComponentIndex_t * stci);


/**
 * Sets the value of the "id" attribute of this SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling SpeciesTypeComponentIndex_unsetId().
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setId(SpeciesTypeComponentIndex_t * stci, const char * id);


/**
 * Sets the value of the "name" attribute of this SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling SpeciesTypeComponentIndex_unsetName().
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setName(SpeciesTypeComponentIndex_t * stci, const char * name);


/**
 * Sets the value of the "component" attribute of this
 * SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @param component const char * value of the "component" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setComponent(SpeciesTypeComponentIndex_t * stci, const char * component);


/**
 * Sets the value of the "identifyingParent" attribute of this
 * SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @param identifyingParent const char * value of the "identifyingParent"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_setIdentifyingParent(SpeciesTypeComponentIndex_t * stci, const char * identifyingParent);


/**
 * Unsets the value of the "id" attribute of this SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetId(SpeciesTypeComponentIndex_t * stci);


/**
 * Unsets the value of the "name" attribute of this
 * SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetName(SpeciesTypeComponentIndex_t * stci);


/**
 * Unsets the value of the "component" attribute of this
 * SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetComponent(SpeciesTypeComponentIndex_t * stci);


/**
 * Unsets the value of the "identifyingParent" attribute of this
 * SpeciesTypeComponentIndex_t.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_unsetIdentifyingParent(SpeciesTypeComponentIndex_t * stci);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SpeciesTypeComponentIndex_t object have been set.
 *
 * @param stci the SpeciesTypeComponentIndex_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SpeciesTypeComponentIndex_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required attributes for the SpeciesTypeComponentIndex_t object
 * are:
 * @li "id"
 * @li "component"
 *
 * @memberof SpeciesTypeComponentIndex_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentIndex_hasRequiredAttributes(SpeciesTypeComponentIndex_t * stci);


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
 * @memberof ListOfSpeciesTypeComponentIndexes_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
ListOfSpeciesTypeComponentIndexes_getById(ListOf_t * lo, const char * sid);


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
 * @memberof ListOfSpeciesTypeComponentIndexes_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
ListOfSpeciesTypeComponentIndexes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpeciesTypeComponentIndex_H__  */

