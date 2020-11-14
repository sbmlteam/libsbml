/**
 * @file:   PossibleSpeciesFeatureValue.h
 * @brief:  Implementation of the PossibleSpeciesFeatureValue class
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
 * @class PossibleSpeciesFeatureValue
 * @sbmlbrief{multi} Defines one value of a SpeciesFeature.
 *
 * The PossibleSpeciesFeatureValue object is a child of a SpeciesFeatureType,
 * and defines one value (though its optional "numericValue" attribute) which
 * the parent SpeciesFeatureType can hold.
 *
 * @class ListOfPossibleSpeciesFeatureValues
 * @sbmlbrief{multi} A list of PossibleSpeciesFeatureValue objects.
 *
 * The ListOfPossibleSpeciesFeatureValues is a container for
 * PossibleSpeciesFeatureValue objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see PossibleSpeciesFeatureValue
 */


#ifndef PossibleSpeciesFeatureValue_H__
#define PossibleSpeciesFeatureValue_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN PossibleSpeciesFeatureValue : public SBase
{

protected:

  /** @cond doxygenLibsbmlInternal */

  ////  std::string   mId;
  ////  std::string   mName;
  std::string   mNumericValue;

  /** @endcond */


public:

  /**
   * Creates a new PossibleSpeciesFeatureValue object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  PossibleSpeciesFeatureValue(unsigned int level      = MultiExtension::getDefaultLevel(),
                              unsigned int version    = MultiExtension::getDefaultVersion(),
                              unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new PossibleSpeciesFeatureValue with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  PossibleSpeciesFeatureValue(MultiPkgNamespaces* multins);


  /**
   * Copy constructor for PossibleSpeciesFeatureValue.
   *
   * @param orig the PossibleSpeciesFeatureValue instance to copy.
   */
  PossibleSpeciesFeatureValue(const PossibleSpeciesFeatureValue& orig);


  /**
   * Assignment operator for PossibleSpeciesFeatureValue.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  PossibleSpeciesFeatureValue& operator=(const PossibleSpeciesFeatureValue& rhs);


  /**
   * Creates and returns a deep copy of this PossibleSpeciesFeatureValue object.
   *
   * @return a (deep) copy of this PossibleSpeciesFeatureValue object.
   */
  virtual PossibleSpeciesFeatureValue* clone () const;


  /**
   * Destructor for PossibleSpeciesFeatureValue.
   */
  virtual ~PossibleSpeciesFeatureValue();


   /**
   * Returns the value of the "id" attribute of this
   * PossibleSpeciesFeatureValue.
   *
   * @return the value of the "id" attribute of this
   * PossibleSpeciesFeatureValue as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns @c true if this PossibleSpeciesFeatureValue's "id" attribute has
   * been set.
   *
   * @return @c true if this PossibleSpeciesFeatureValue's "id" attribute has
   * been set; otherwise, @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this PossibleSpeciesFeatureValue.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this PossibleSpeciesFeatureValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this
   * PossibleSpeciesFeatureValue.
   *
   * @return the value of the "name" attribute of this
   * PossibleSpeciesFeatureValue as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this PossibleSpeciesFeatureValue's "name" attribute
   * has been set.
   *
   * @return @c true if this PossibleSpeciesFeatureValue's "name" attribute
   * has been set; otherwise, @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this PossibleSpeciesFeatureValue.
   *
   * @param name the new "name" attribute value.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this
   * PossibleSpeciesFeatureValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "numericValue" attribute of this
   * PossibleSpeciesFeatureValue.
   *
   * @return the value of the "numericValue" attribute of this
   * PossibleSpeciesFeatureValue as a string.
   */
  virtual const std::string& getNumericValue() const;


  /**
   * Returns @c true if this PossibleSpeciesFeatureValue's "numericValue"
   * attribute has been set.
   *
   * @return @c true if this PossibleSpeciesFeatureValue's "numericValue"
   * attribute has been set; otherwise, @c false is returned.
   */
  virtual bool isSetNumericValue() const;


  /**
   * Sets the value of the "numericValue" attribute of this
   * PossibleSpeciesFeatureValue.
   *
   * @param numericValue the new "numericValue" attribute value.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setNumericValue(const std::string& numericValue);


  /**
   * Unsets the value of the "numericValue" attribute of this
   * PossibleSpeciesFeatureValue.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetNumericValue();


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
   * @return the name of this element, i.e. @c "possibleSpeciesFeatureValue".
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
   * for this PossibleSpeciesFeatureValue object have been set.
   *
   * @note The required attributes for a PossibleSpeciesFeatureValue object are:
   * @li "id"
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

class LIBSBML_EXTERN ListOfPossibleSpeciesFeatureValues : public ListOf
{

public:

  /**
   * Creates a new ListOfPossibleSpeciesFeatureValues object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfPossibleSpeciesFeatureValues(unsigned int level      = MultiExtension::getDefaultLevel(),
                                     unsigned int version    = MultiExtension::getDefaultVersion(),
                                     unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfPossibleSpeciesFeatureValues with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfPossibleSpeciesFeatureValues(MultiPkgNamespaces* multins);


  /**
   * Creates and returns a deep copy of this
   * ListOfPossibleSpeciesFeatureValues object.
   *
   * @return a (deep) copy of this ListOfPossibleSpeciesFeatureValues object.
   */
  virtual ListOfPossibleSpeciesFeatureValues* clone () const;


  /**
   * Get the nth PossibleSpeciesFeatureValue object from the
   * ListOfPossibleSpeciesFeatureValues.
   *
   * @param n the index number of the PossibleSpeciesFeatureValue to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual PossibleSpeciesFeatureValue* get(unsigned int n);


  /**
   * Get the nth PossibleSpeciesFeatureValue object from the
   * ListOfPossibleSpeciesFeatureValues.
   *
   * @param n the index number of the PossibleSpeciesFeatureValue to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual const PossibleSpeciesFeatureValue* get(unsigned int n) const;


  /**
   * Get the PossibleSpeciesFeatureValue with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * PossibleSpeciesFeatureValue to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual PossibleSpeciesFeatureValue* get(const std::string& sid);


  /**
   * Get the PossibleSpeciesFeatureValue with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * PossibleSpeciesFeatureValue to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const PossibleSpeciesFeatureValue* get(const std::string& sid) const;


  /**
   * Removes the nth PossibleSpeciesFeatureValue object from the
   * ListOfPossibleSpeciesFeatureValues.
   *
   * @param n the index of the PossibleSpeciesFeatureValue to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see size()
   */
  virtual PossibleSpeciesFeatureValue* remove(unsigned int n);


  /**
   * Removes the PossibleSpeciesFeatureValue with the given identifier @p sid.
   *
   * @param sid the identifier of the PossibleSpeciesFeatureValue to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  virtual PossibleSpeciesFeatureValue* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "listOfPossibleSpeciesFeatureValues".
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
   * Creates a new PossibleSpeciesFeatureValue in this ListOfPossibleSpeciesFeatureValues
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
 * Creates a new PossibleSpeciesFeatureValue_t using the given SBML Level,
 * Version and &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * PossibleSpeciesFeatureValue_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
PossibleSpeciesFeatureValue_create(unsigned int level, unsigned int version,
                                   unsigned int pkgVersion);


/**
 * Frees this PossibleSpeciesFeatureValue_t object.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
void
PossibleSpeciesFeatureValue_free(PossibleSpeciesFeatureValue_t * psfv);


/**
 * Creates and returns a deep copy of this PossibleSpeciesFeatureValue_t
 * object.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @return a (deep) copy of this PossibleSpeciesFeatureValue_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
PossibleSpeciesFeatureValue_clone(PossibleSpeciesFeatureValue_t * psfv);


/**
 * Returns the value of the "id" attribute of this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this
 * PossibleSpeciesFeatureValue_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
char *
PossibleSpeciesFeatureValue_getId(const PossibleSpeciesFeatureValue_t * psfv);


/**
 * Returns the value of the "name" attribute of this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure whose name is
 * sought.
 *
 * @return the value of the "name" attribute of this
 * PossibleSpeciesFeatureValue_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
char *
PossibleSpeciesFeatureValue_getName(const PossibleSpeciesFeatureValue_t * psfv);


/**
 * Returns the value of the "numericValue" attribute of this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure whose numericValue
 * is sought.
 *
 * @return the value of the "numericValue" attribute of this
 * PossibleSpeciesFeatureValue_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
char *
PossibleSpeciesFeatureValue_getNumericValue(const PossibleSpeciesFeatureValue_t * psfv);


/**
 * Predicate returning @c 1 (true) if this PossibleSpeciesFeatureValue_t's "id"
 * attribute is set.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @return @c 1 (true) if this PossibleSpeciesFeatureValue_t's "id" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_isSetId(const PossibleSpeciesFeatureValue_t * psfv);


/**
 * Predicate returning @c 1 (true) if this PossibleSpeciesFeatureValue_t's
 * "name" attribute is set.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @return @c 1 (true) if this PossibleSpeciesFeatureValue_t's "name" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_isSetName(const PossibleSpeciesFeatureValue_t * psfv);


/**
 * Predicate returning @c 1 (true) if this PossibleSpeciesFeatureValue_t's
 * "numericValue" attribute is set.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @return @c 1 (true) if this PossibleSpeciesFeatureValue_t's "numericValue"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_isSetNumericValue(const PossibleSpeciesFeatureValue_t * psfv);


/**
 * Sets the value of the "id" attribute of this PossibleSpeciesFeatureValue_t.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling PossibleSpeciesFeatureValue_unsetId().
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_setId(PossibleSpeciesFeatureValue_t * psfv, const char * id);


/**
 * Sets the value of the "name" attribute of this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling PossibleSpeciesFeatureValue_unsetName().
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_setName(PossibleSpeciesFeatureValue_t * psfv, const char * name);


/**
 * Sets the value of the "numericValue" attribute of this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @param numericValue const char * value of the "numericValue" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_setNumericValue(PossibleSpeciesFeatureValue_t * psfv, const char * numericValue);


/**
 * Unsets the value of the "id" attribute of this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_unsetId(PossibleSpeciesFeatureValue_t * psfv);


/**
 * Unsets the value of the "name" attribute of this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_unsetName(PossibleSpeciesFeatureValue_t * psfv);


/**
 * Unsets the value of the "numericValue" attribute of this
 * PossibleSpeciesFeatureValue_t.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_unsetNumericValue(PossibleSpeciesFeatureValue_t * psfv);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * PossibleSpeciesFeatureValue_t object have been set.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * PossibleSpeciesFeatureValue_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required attributes for the PossibleSpeciesFeatureValue_t object
 * are:
 * @li "id"
 *
 * @memberof PossibleSpeciesFeatureValue_t
 */
LIBSBML_EXTERN
int
PossibleSpeciesFeatureValue_hasRequiredAttributes(PossibleSpeciesFeatureValue_t * psfv);


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
 * @memberof ListOfPossibleSpeciesFeatureValues_t
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
ListOfPossibleSpeciesFeatureValues_getById(ListOf_t * lo, const char * sid);


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
 * @memberof ListOfPossibleSpeciesFeatureValues_t
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
ListOfPossibleSpeciesFeatureValues_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  PossibleSpeciesFeatureValue_H__  */

