/**
 * @file:   SpeciesTypeInstance.h
 * @brief:  Implementation of the SpeciesTypeInstance class
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
 * @class SpeciesTypeInstance
 * @sbmlbrief{multi} Allows construction of structured MultiSpeciesType objects.
 *
 * The SpeciesTypeInstance object is a child of MultiSpeciesType, and
 * provides a way to construct MultiSpeciesType objects and Species with
 * multiple components. A MultiSpeciesType can contain a list of instances of
 * other MultiSpeciesType objects which can also have their own
 * SpeciesTypeInstance objects, so the complete construct of a
 * MultiSpeciesType has a tree structure. A MultiSpeciesType cannot contain
 * an instance of any other MultiSpeciesType that already contains the
 * instance of it. In other words, circular references are not allowed when
 * constructing MultiSpeciesType objects. For example, if a MultiSpeciesType
 * "A" contains the instance of another MultiSpeciesType "B", "B" must not
 * contain the instance of "A" anywhere in the complete structure of "B".
 * The optional attribute compartmentReference, of type SIdRef, can be used
 * to indicate which sub-compartment in a composite compartment the
 * SpeciesTypeInstance is located in.
 *
 * @class ListOfSpeciesTypeInstances
 * @sbmlbrief{multi} A list of SpeciesTypeInstance objects.
 *
 * The ListOfSpeciesTypeInstances is a container for SpeciesTypeInstance objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see SpeciesTypeInstance
 */


#ifndef SpeciesTypeInstance_H__
#define SpeciesTypeInstance_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpeciesTypeInstance : public SBase
{

protected:

  /** @cond doxygenLibsbmlInternal */

////  std::string   mId;
////  std::string   mName;
  std::string   mSpeciesType;
  std::string   mCompartmentReference;

  /** @endcond */


public:

  /**
   * Creates a new SpeciesTypeInstance object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesTypeInstance(unsigned int level      = MultiExtension::getDefaultLevel(),
                      unsigned int version    = MultiExtension::getDefaultVersion(),
                      unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpeciesTypeInstance with the given MultiPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesTypeInstance(MultiPkgNamespaces* multins);


  /**
   * Copy constructor for SpeciesTypeInstance.
   *
   * @param orig the SpeciesTypeInstance instance to copy.
   */
  SpeciesTypeInstance(const SpeciesTypeInstance& orig);


  /**
   * Assignment operator for SpeciesTypeInstance.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  SpeciesTypeInstance& operator=(const SpeciesTypeInstance& rhs);


  /**
   * Creates and returns a deep copy of this SpeciesTypeInstance object.
   *
   * @return a (deep) copy of this SpeciesTypeInstance object.
   */
  virtual SpeciesTypeInstance* clone () const;


  /**
   * Destructor for SpeciesTypeInstance.
   */
  virtual ~SpeciesTypeInstance();


  /**
   * Returns the value of the "id" attribute of this SpeciesTypeInstance.
   *
   * @return the value of the "id" attribute of this SpeciesTypeInstance as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns @c true if this SpeciesTypeInstance's "id" attribute has been
   * set.
   *
   * @return @c true if this SpeciesTypeInstance's "id" attribute has been
   * set; otherwise, @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this SpeciesTypeInstance.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this SpeciesTypeInstance.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this SpeciesTypeInstance.
   *
   * @return the value of the "name" attribute of this SpeciesTypeInstance as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this SpeciesTypeInstance's "name" attribute has been
   * set.
   *
   * @return @c true if this SpeciesTypeInstance's "name" attribute has been
   * set; otherwise, @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this SpeciesTypeInstance.
   *
   * @param name the new value for the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this SpeciesTypeInstance.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "speciesType" attribute of this SpeciesTypeInstance.
   *
   * @return the value of the "speciesType" attribute of this SpeciesTypeInstance as a string.
   */
  virtual const std::string& getSpeciesType() const;


  /**
   * Returns @c true if this SpeciesTypeInstance's "speciesType" attribute
   * has been set.
   *
   * @return @c true if this SpeciesTypeInstance's "speciesType" attribute
   * has been set; otherwise, @c false is returned.
   */
  virtual bool isSetSpeciesType() const;


  /**
   * Sets the value of the "speciesType" attribute of this
   * SpeciesTypeInstance.
   *
   * @param speciesType the new value for the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setSpeciesType(const std::string& speciesType);


  /**
   * Unsets the value of the "speciesType" attribute of this
   * SpeciesTypeInstance.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetSpeciesType();


  /**
   * Returns the value of the "compartmentReference" attribute of this
   * SpeciesTypeInstance.
   *
   * @return the value of the "compartmentReference" attribute of this
   * SpeciesTypeInstance as a string.
   */
  virtual const std::string& getCompartmentReference() const;


  /**
   * Returns @c true if this SpeciesTypeInstance's "compartmentReference"
   * attribute has been set.
   *
   * @return @c true if this SpeciesTypeInstance's "compartmentReference"
   * attribute has been set; otherwise, @c false is returned.
   */
  virtual bool isSetCompartmentReference() const;


  /**
   * Sets the value of the "compartmentReference" attribute of this
   * SpeciesTypeInstance.
   *
   * @param compartmentReference the new value for the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setCompartmentReference(const std::string& compartmentReference);


  /**
   * Unsets the value of the "compartmentReference" attribute of this
   * SpeciesTypeInstance.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetCompartmentReference();


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
   * @return the name of this element, i.e. @c "speciesTypeInstance".
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
   * for this SpeciesTypeInstance object have been set.
   *
   * @note The required attributes for a SpeciesTypeInstance object are:
   * @li "id"
   * @li "speciesType"
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

class LIBSBML_EXTERN ListOfSpeciesTypeInstances : public ListOf
{

public:

  /**
   * Creates a new ListOfSpeciesTypeInstances object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesTypeInstances(unsigned int level      = MultiExtension::getDefaultLevel(),
                             unsigned int version    = MultiExtension::getDefaultVersion(),
                             unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpeciesTypeInstances with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesTypeInstances(MultiPkgNamespaces* multins);


  /**
   * Creates and returns a deep copy of this ListOfSpeciesTypeInstances object.
   *
   * @return a (deep) copy of this ListOfSpeciesTypeInstances object.
   */
  virtual ListOfSpeciesTypeInstances* clone () const;


  /**
   * Get the nth SpeciesTypeInstance object from the
   * ListOfSpeciesTypeInstances.
   *
   * @param n the index number of the SpeciesTypeInstance to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual SpeciesTypeInstance* get(unsigned int n);


  /**
   * Get the nth SpeciesTypeInstance object from the
   * ListOfSpeciesTypeInstances.
   *
   * @param n the index number of the SpeciesTypeInstance to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual const SpeciesTypeInstance* get(unsigned int n) const;


  /**
   * Get the SpeciesTypeInstance object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * SpeciesTypeInstance to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual SpeciesTypeInstance* get(const std::string& sid);


  /**
   * Get the SpeciesTypeInstance object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * SpeciesTypeInstance to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SpeciesTypeInstance* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesTypeInstance object from this
   * ListOfSpeciesTypeInstances.
   *
   * @param n the index of the SpeciesTypeInstance to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see size()
   */
  virtual SpeciesTypeInstance* remove(unsigned int n);


  /**
   * Removes the SpeciesTypeInstance object with the given identifier @p sid.
   *
   * @param sid the identifier of the SpeciesTypeInstance to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  virtual SpeciesTypeInstance* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "listOfSpeciesTypeInstances".
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
   * Creates a new SpeciesTypeInstance in this ListOfSpeciesTypeInstances
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
 * Creates a new SpeciesTypeInstance_t using the given SBML Level, Version and
 * &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SpeciesTypeInstance_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesTypeInstance_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * SpeciesTypeInstance_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t *
SpeciesTypeInstance_create(unsigned int level, unsigned int version,
                           unsigned int pkgVersion);


/**
 * Frees this SpeciesTypeInstance_t object.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
void
SpeciesTypeInstance_free(SpeciesTypeInstance_t * sti);


/**
 * Creates and returns a deep copy of this SpeciesTypeInstance_t object.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @return a (deep) copy of this SpeciesTypeInstance_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t *
SpeciesTypeInstance_clone(SpeciesTypeInstance_t * sti);


/**
 * Returns the value of the "id" attribute of this SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this SpeciesTypeInstance_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeInstance_getId(SpeciesTypeInstance_t * sti);


/**
 * Returns the value of the "name" attribute of this SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this SpeciesTypeInstance_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeInstance_getName(SpeciesTypeInstance_t * sti);


/**
 * Returns the value of the "speciesType" attribute of this
 * SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure whose speciesType is sought.
 *
 * @return the value of the "speciesType" attribute of this
 * SpeciesTypeInstance_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeInstance_getSpeciesType(SpeciesTypeInstance_t * sti);


/**
 * Returns the value of the "compartmentReference" attribute of this
 * SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure whose compartmentReference is
 * sought.
 *
 * @return the value of the "compartmentReference" attribute of this
 * SpeciesTypeInstance_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeInstance_getCompartmentReference(SpeciesTypeInstance_t * sti);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeInstance_t's "id"
 * attribute is set.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeInstance_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_isSetId(SpeciesTypeInstance_t * sti);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeInstance_t's "name"
 * attribute is set.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeInstance_t's "name" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_isSetName(SpeciesTypeInstance_t * sti);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeInstance_t's
 * "speciesType" attribute is set.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeInstance_t's "speciesType" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_isSetSpeciesType(SpeciesTypeInstance_t * sti);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeInstance_t's
 * "compartmentReference" attribute is set.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeInstance_t's "compartmentReference"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_isSetCompartmentReference(SpeciesTypeInstance_t * sti);


/**
 * Sets the value of the "id" attribute of this SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling SpeciesTypeInstance_unsetId().
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_setId(SpeciesTypeInstance_t * sti, const char * id);


/**
 * Sets the value of the "name" attribute of this SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling SpeciesTypeInstance_unsetName().
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_setName(SpeciesTypeInstance_t * sti, const char * name);


/**
 * Sets the value of the "speciesType" attribute of this SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @param speciesType const char * value of the "speciesType" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_setSpeciesType(SpeciesTypeInstance_t * sti, const char * speciesType);


/**
 * Sets the value of the "compartmentReference" attribute of this
 * SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @param compartmentReference const char * value of the "compartmentReference"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_setCompartmentReference(SpeciesTypeInstance_t * sti, const char * compartmentReference);


/**
 * Unsets the value of the "id" attribute of this SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_unsetId(SpeciesTypeInstance_t * sti);


/**
 * Unsets the value of the "name" attribute of this SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_unsetName(SpeciesTypeInstance_t * sti);


/**
 * Unsets the value of the "speciesType" attribute of this
 * SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_unsetSpeciesType(SpeciesTypeInstance_t * sti);


/**
 * Unsets the value of the "compartmentReference" attribute of this
 * SpeciesTypeInstance_t.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_unsetCompartmentReference(SpeciesTypeInstance_t * sti);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SpeciesTypeInstance_t object have been set.
 *
 * @param sti the SpeciesTypeInstance_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SpeciesTypeInstance_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the SpeciesTypeInstance_t object are:
 * @li "id"
 * @li "speciesType"
 *
 * @memberof SpeciesTypeInstance_t
 */
LIBSBML_EXTERN
int
SpeciesTypeInstance_hasRequiredAttributes(SpeciesTypeInstance_t * sti);


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
 * @memberof ListOfSpeciesTypeInstances_t
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t *
ListOfSpeciesTypeInstances_getById(ListOf_t * lo, const char * sid);


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
 * @memberof ListOfSpeciesTypeInstances_t
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t *
ListOfSpeciesTypeInstances_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpeciesTypeInstance_H__  */

