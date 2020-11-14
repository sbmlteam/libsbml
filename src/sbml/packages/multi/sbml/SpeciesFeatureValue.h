/**
 * @file:   SpeciesFeatureValue.h
 * @brief:  Implementation of the SpeciesFeatureValue class
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
 * @class SpeciesFeatureValue
 * @sbmlbrief{multi} Defines a particular value for a SpeciesFeature.
 *
 * The SpeciesFeatureValue object is a child of a SpeciesFeature, and serves
 * to specify a value for a SpeciesFeature to select from the
 * ListOfPossibleSpeciesFeatureValues defined in the SpeciesFeatureType
 * referenced by the parent SpeciesFeature.
 *
 * @class ListOfSpeciesFeatureValues
 * @sbmlbrief{multi} A list of SpeciesFeatureValue objects.
 *
 * The ListOfSpeciesFeatureValues is a container for SpeciesFeatureValue objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see SpeciesFeatureValue
 */


#ifndef SpeciesFeatureValue_H__
#define SpeciesFeatureValue_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpeciesFeatureValue : public SBase
{

protected:

  /** @cond doxygenLibsbmlInternal */

  std::string   mValue;

  /** @endcond */


public:

  /**
   * Creates a new SpeciesFeatureValue object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesFeatureValue(unsigned int level      = MultiExtension::getDefaultLevel(),
                      unsigned int version    = MultiExtension::getDefaultVersion(),
                      unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpeciesFeatureValue with the given MultiPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesFeatureValue(MultiPkgNamespaces* multins);


  /**
   * Copy constructor for SpeciesFeatureValue.
   *
   * @param orig the SpeciesFeatureValue instance to copy.
   */
  SpeciesFeatureValue(const SpeciesFeatureValue& orig);


  /**
   * Assignment operator for SpeciesFeatureValue.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  SpeciesFeatureValue& operator=(const SpeciesFeatureValue& rhs);


  /**
   * Creates and returns a deep copy of this SpeciesFeatureValue object.
   *
   * @return a (deep) copy of this SpeciesFeatureValue object.
   */
  virtual SpeciesFeatureValue* clone () const;


  /**
   * Destructor for SpeciesFeatureValue.
   */
  virtual ~SpeciesFeatureValue();


  /**
   * Returns the value of the "value" attribute of this SpeciesFeatureValue.
   *
   * @return the value of the "value" attribute of this SpeciesFeatureValue as a string.
   */
  virtual const std::string& getValue() const;


  /**
   * Returns @c true if this SpeciesFeatureValue's "value" attribute has been
   * set.
   *
   * @return @c true if this SpeciesFeatureValue's "value" attribute has been
   * set; otherwise, @c false is returned.
   */
  virtual bool isSetValue() const;


  /**
   * Sets the value of the "value" attribute of this SpeciesFeatureValue.
   *
   * @param value const std::string& value of the "value" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setValue(const std::string& value);


  /**
   * Unsets the value of the "value" attribute of this SpeciesFeatureValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetValue();


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
   * @return the name of this element, i.e. @c "speciesFeatureValue".
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
   * for this SpeciesFeatureValue object have been set.
   *
   * @note The required attributes for a SpeciesFeatureValue object are:
   * @li "value"
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

class LIBSBML_EXTERN ListOfSpeciesFeatureValues : public ListOf
{

public:

  /**
   * Creates a new ListOfSpeciesFeatureValues object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesFeatureValues(unsigned int level      = MultiExtension::getDefaultLevel(),
                             unsigned int version    = MultiExtension::getDefaultVersion(),
                             unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpeciesFeatureValues with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesFeatureValues(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this ListOfSpeciesFeatureValues
   * object.
   *
   * @return a (deep) copy of this ListOfSpeciesFeatureValues object.
   */
  virtual ListOfSpeciesFeatureValues* clone () const;


   /**
   * Get the nth SpeciesFeatureValue object from the
   * ListOfSpeciesFeatureValues.
   *
   * @param n the index number of the SpeciesFeatureValue to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual SpeciesFeatureValue* get(unsigned int n);


  /**
   * Get the nth SpeciesFeatureValue object from the
   * ListOfSpeciesFeatureValues.
   *
   * @param n the index number of the SpeciesFeatureValue to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual const SpeciesFeatureValue* get(unsigned int n) const;


  /**
   * Get the SpeciesFeatureValue object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * SpeciesFeatureValue to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual SpeciesFeatureValue* get(const std::string& sid);


  /**
   * Get the SpeciesFeatureValue object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * SpeciesFeatureValue to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const SpeciesFeatureValue* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesFeatureValue object from this
   * ListOfSpeciesFeatureValues.

   * @param n the index of the SpeciesFeatureValue to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see size()
   */
  virtual SpeciesFeatureValue* remove(unsigned int n);


  /**
   * Removes the SpeciesFeatureValue object with the given identifier @p sid.
   *
   * @param sid the identifier of the SpeciesFeatureValue to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  virtual SpeciesFeatureValue* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "listOfSpeciesFeatureValues".
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
   * Creates a new SpeciesFeatureValue in this ListOfSpeciesFeatureValues
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
 * Creates a new SpeciesFeatureValue_t using the given SBML Level, Version and
 * &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SpeciesFeatureValue_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesFeatureValue_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * SpeciesFeatureValue_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesFeatureValue_t
 */
LIBSBML_EXTERN
SpeciesFeatureValue_t *
SpeciesFeatureValue_create(unsigned int level, unsigned int version,
                           unsigned int pkgVersion);


/**
 * Frees this SpeciesFeatureValue_t object.
 *
 * @param sfv the SpeciesFeatureValue_t structure.
 *
 * @memberof SpeciesFeatureValue_t
 */
LIBSBML_EXTERN
void
SpeciesFeatureValue_free(SpeciesFeatureValue_t * sfv);


/**
 * Creates and returns a deep copy of this SpeciesFeatureValue_t object.
 *
 * @param sfv the SpeciesFeatureValue_t structure.
 *
 * @return a (deep) copy of this SpeciesFeatureValue_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesFeatureValue_t
 */
LIBSBML_EXTERN
SpeciesFeatureValue_t *
SpeciesFeatureValue_clone(SpeciesFeatureValue_t * sfv);


/**
 * Returns the value of the "value" attribute of this SpeciesFeatureValue_t.
 *
 * @param sfv the SpeciesFeatureValue_t structure whose value is sought.
 *
 * @return the value of the "value" attribute of this SpeciesFeatureValue_t as
 * a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesFeatureValue_t
 */
LIBSBML_EXTERN
char *
SpeciesFeatureValue_getValue(SpeciesFeatureValue_t * sfv);


/**
 * Predicate returning @c 1 (true) if this SpeciesFeatureValue_t's "value"
 * attribute is set.
 *
 * @param sfv the SpeciesFeatureValue_t structure.
 *
 * @return @c 1 (true) if this SpeciesFeatureValue_t's "value" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureValue_isSetValue(SpeciesFeatureValue_t * sfv);


/**
 * Sets the value of the "value" attribute of this SpeciesFeatureValue_t.
 *
 * @param sfv the SpeciesFeatureValue_t structure.
 *
 * @param value const char * value of the "value" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureValue_setValue(SpeciesFeatureValue_t * sfv, const char * value);


/**
 * Unsets the value of the "value" attribute of this SpeciesFeatureValue_t.
 *
 * @param sfv the SpeciesFeatureValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureValue_unsetValue(SpeciesFeatureValue_t * sfv);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SpeciesFeatureValue_t object have been set.
 *
 * @param sfv the SpeciesFeatureValue_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SpeciesFeatureValue_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the SpeciesFeatureValue_t object are:
 * @li "value"
 *
 * @memberof SpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureValue_hasRequiredAttributes(SpeciesFeatureValue_t * sfv);


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
 * @memberof ListOfSpeciesFeatureValues_t
 */
LIBSBML_EXTERN
SpeciesFeatureValue_t *
ListOfSpeciesFeatureValues_getById(ListOf_t * lo, const char * sid);


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
 * @memberof ListOfSpeciesFeatureValues_t
 */
LIBSBML_EXTERN
SpeciesFeatureValue_t *
ListOfSpeciesFeatureValues_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpeciesFeatureValue_H__  */

