/**
 * @file:   SpeciesFeatureType.h
 * @brief:  Implementation of the SpeciesFeatureType class
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
 * @class SpeciesFeatureType
 * @sbmlbrief{multi} Defines a template for referencing SpeciesFeature objects.
 *
 * The SpeciesFeatureType object is a child of a MultiSpeciesType, and serves
 * to provide frameworks or templates to define the referencing
 * SpeciesFeature objects. SpeciesFeatureType has two required attributes
 * "id" and "occur", an optional attribute "name", and a required child
 * ListOfPossibleSpeciesFeatureValues. The multiple
 * PossibleSpeciesFeatureValue children of the
 * ListOfPossibleSpeciesFeatureValues object permit constructing multistate
 * species via its SpeciesFeature children of the ListOfSpeciesFeatures or
 * SubListOfSpeciesFeatures object.  The "occur" attribute is used to
 * indicate the number of instances of the SpeciesFeatureType. This attribute
 * can be used to infer the number of the instances in the "don"t care" state
 * in a referencing SpeciesFeature.
 *
 * @class ListOfSpeciesFeatureTypes
 * @sbmlbrief{multi} A list of SpeciesFeatureType objects.
 *
 * The ListOfSpeciesFeatureTypes is a container for SpeciesFeatureType objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see SpeciesFeatureType
 */


#ifndef SpeciesFeatureType_H__
#define SpeciesFeatureType_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>

#include <sbml/packages/multi/sbml/PossibleSpeciesFeatureValue.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpeciesFeatureType : public SBase
{

protected:

  /** @cond doxygenLibsbmlInternal */

  ////  std::string   mId;
  ////  std::string   mName;
  unsigned int  mOccur;
  bool          mIsSetOccur;
  ListOfPossibleSpeciesFeatureValues   mPossibleSpeciesFeatureValues;

  /** @endcond */


public:

  /**
   * Creates a new SpeciesFeatureType object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesFeatureType(unsigned int level      = MultiExtension::getDefaultLevel(),
                     unsigned int version    = MultiExtension::getDefaultVersion(),
                     unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpeciesFeatureType with the given MultiPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesFeatureType(MultiPkgNamespaces* multins);


  /**
   * Copy constructor for SpeciesFeatureType.
   *
   * @param orig the SpeciesFeatureType instance to copy.
   */
  SpeciesFeatureType(const SpeciesFeatureType& orig);


  /**
   * Assignment operator for SpeciesFeatureType.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  SpeciesFeatureType& operator=(const SpeciesFeatureType& rhs);


  /**
   * Creates and returns a deep copy of this SpeciesFeatureType object.
   *
   * @return a (deep) copy of this SpeciesFeatureType object.
   */
  virtual SpeciesFeatureType* clone () const;


  /**
   * Destructor for SpeciesFeatureType.
   */
  virtual ~SpeciesFeatureType();


  /**
   * Returns the value of the "id" attribute of this SpeciesFeatureType.
   *
   * @return the value of the "id" attribute of this SpeciesFeatureType as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns @c true if this SpeciesFeatureType's "id" attribute has been
   * set.
   *
   * @return @c true if this SpeciesFeatureType's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this SpeciesFeatureType.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this SpeciesFeatureType.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this SpeciesFeatureType.
   *
   * @return the value of the "name" attribute of this SpeciesFeatureType as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this SpeciesFeatureType's "name" attribute has been
   * set.
   *
   * @return @c true if this SpeciesFeatureType's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this SpeciesFeatureType.
   *
   * @param name const std::string& value of the "name" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this SpeciesFeatureType.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "occur" attribute of this SpeciesFeatureType.
   *
   * @return the value of the "occur" attribute of this SpeciesFeatureType as
   * a unsigned integer.
   */
  virtual unsigned int getOccur() const;


  /**
   * Returns @c true if this SpeciesFeatureType's "occur" attribute has been
   * set.
   *
   * @return @c true if this SpeciesFeatureType's "occur" attribute has been
   * set; otherwise, @c false is returned.
   */
  virtual bool isSetOccur() const;


  /**
   * Sets the value of the "occur" attribute of this SpeciesFeatureType.
   *
   * @param occur unsigned int value of the "occur" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setOccur(unsigned int occur);


  /**
   * Unsets the value of the "occur" attribute of this SpeciesFeatureType.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetOccur();


  /**
   * Returns the ListOfPossibleSpeciesFeatureValues in this
   * SpeciesFeatureType object.
   *
   * @return the ListOfPossibleSpeciesFeatureValues child of this
   * SpeciesFeatureType.
   */
  const ListOfPossibleSpeciesFeatureValues* getListOfPossibleSpeciesFeatureValues() const;


  /**
   * Returns the ListOfPossibleSpeciesFeatureValues in this
   * SpeciesFeatureType object.
   *
   * @return the ListOfPossibleSpeciesFeatureValues child of this
   * SpeciesFeatureType.
   */
  ListOfPossibleSpeciesFeatureValues* getListOfPossibleSpeciesFeatureValues();


  /**
   * Get the nth PossibleSpeciesFeatureValue object from the
   * ListOfPossibleSpeciesFeatureValues.
   *
   * @param n the index number of the PossibleSpeciesFeatureValue to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see getNumPossibleSpeciesFeatureValues()
   */
  PossibleSpeciesFeatureValue* getPossibleSpeciesFeatureValue(unsigned int n);


  /**
   * Get the nth PossibleSpeciesFeatureValue object from the
   * ListOfPossibleSpeciesFeatureValues.
   *
   * @param n the index number of the PossibleSpeciesFeatureValue to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see getNumPossibleSpeciesFeatureValues()
   */
  const PossibleSpeciesFeatureValue* getPossibleSpeciesFeatureValue(unsigned int n) const;


  /**
   * Get the PossibleSpeciesFeatureValue object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier
   * of the PossibleSpeciesFeatureValue to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see getPossibleSpeciesFeatureValue(unsigned int n)
   * @see getNumPossibleSpeciesFeatureValues()
   */
  PossibleSpeciesFeatureValue* getPossibleSpeciesFeatureValue(const std::string& sid);


  /**
   * Get the PossibleSpeciesFeatureValue object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier
   * of the PossibleSpeciesFeatureValue to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see getPossibleSpeciesFeatureValue(unsigned int n)
   * @see getNumPossibleSpeciesFeatureValues()
   */
  const PossibleSpeciesFeatureValue* getPossibleSpeciesFeatureValue(const std::string& sid) const;


  /**
   * Adds a copy the given "PossibleSpeciesFeatureValue" to this SpeciesFeatureType.
   *
   * @param psfv the PossibleSpeciesFeatureValue object to add
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  int addPossibleSpeciesFeatureValue(const PossibleSpeciesFeatureValue* psfv);


  /**
   * Get the number of PossibleSpeciesFeatureValue objects in this
   * SpeciesFeatureType.
   *
   * @return the number of PossibleSpeciesFeatureValue objects in this
   * SpeciesFeatureType
   */
  unsigned int getNumPossibleSpeciesFeatureValues() const;


  /**
   * Creates a new PossibleSpeciesFeatureValue object and adds it to this
   * SpeciesFeatureTypes.
   *
   * @return a new PossibleSpeciesFeatureValue object instance
   *
   * @see addPossibleSpeciesFeatureValue(const PossibleSpeciesFeatureValue* psfv)
   */
  PossibleSpeciesFeatureValue* createPossibleSpeciesFeatureValue();


  /**
   * Removes the nth PossibleSpeciesFeatureValue object from the
   * ListOfPossibleSpeciesFeatureValues.
   *
   * @param n the index of the PossibleSpeciesFeatureValue to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see getNumPossibleSpeciesFeatureValues()
   */
  PossibleSpeciesFeatureValue* removePossibleSpeciesFeatureValue(unsigned int n);


  /**
   * Removes the PossibleSpeciesFeatureValue object with the given identifier @p sid.
   *
   * @param sid the identifier of the PossibleSpeciesFeatureValue to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @return the PossibleSpeciesFeatureValue removed. As mentioned above, the caller owns the
   * returned item.
   */
  PossibleSpeciesFeatureValue* removePossibleSpeciesFeatureValue(const std::string& sid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitary depth.
   *
   * @param filter a pointer to an ElementFilter, which causes the function
   * to return only elements that match a particular set of constraints.
   * If NULL (the default), the function will return all child objects.
   *
   * @return a List of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "speciesFeatureType".
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
   * for this SpeciesFeatureType object have been set.
   *
   * @note The required attributes for a SpeciesFeatureType object are:
   * @li "id"
   * @li "occur"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this SpeciesFeatureType object have been set.
   *
   * @note A SpeciesFeatureType object has no required subelements.
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
   * Connects to child elements.
   */
  virtual void connectToChild ();
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

class LIBSBML_EXTERN ListOfSpeciesFeatureTypes : public ListOf
{

public:

  /**
   * Creates a new ListOfSpeciesFeatureTypes object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesFeatureTypes(unsigned int level      = MultiExtension::getDefaultLevel(),
                            unsigned int version    = MultiExtension::getDefaultVersion(),
                            unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpeciesFeatureTypes with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesFeatureTypes(MultiPkgNamespaces* multins);


  /**
   * Creates and returns a deep copy of this ListOfSpeciesFeatureTypes object.
   *
   * @return a (deep) copy of this ListOfSpeciesFeatureTypes object.
   */
  virtual ListOfSpeciesFeatureTypes* clone () const;


  /**
   * Get the nth SpeciesFeatureType object from the
   * ListOfSpeciesFeatureTypes.
   *
   * @param n the index number of the SpeciesFeatureType to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual SpeciesFeatureType* get(unsigned int n);


  /**
   * Get the nth SpeciesFeatureType object from the
   * ListOfSpeciesFeatureTypes.
   *
   * @param n the index number of the SpeciesFeatureType to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual const SpeciesFeatureType* get(unsigned int n) const;


  /**
   * Get the SpeciesFeatureType object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * SpeciesFeatureType to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual SpeciesFeatureType* get(const std::string& sid);


  /**
   * Get the SpeciesFeatureType object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * SpeciesFeatureType to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const SpeciesFeatureType* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesFeatureType object from this
   * ListOfSpeciesFeatureTypes.
   *
   * @param n the index of the SpeciesFeatureType to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see size()
   */
  virtual SpeciesFeatureType* remove(unsigned int n);


  /**
   * Removes the SpeciesFeatureType object with the given identifier @p sid.
   *
   * @param sid the identifier of the SpeciesFeatureType to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  virtual SpeciesFeatureType* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "listOfSpeciesFeatureTypes".
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
   * Creates a new SpeciesFeatureType in this ListOfSpeciesFeatureTypes
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
 * Creates a new SpeciesFeatureType_t using the given SBML Level, Version and
 * &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SpeciesFeatureType_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesFeatureType_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * SpeciesFeatureType_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
SpeciesFeatureType_t *
SpeciesFeatureType_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion);


/**
 * Frees this SpeciesFeatureType_t object.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
void
SpeciesFeatureType_free(SpeciesFeatureType_t * sft);


/**
 * Creates and returns a deep copy of this SpeciesFeatureType_t object.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @return a (deep) copy of this SpeciesFeatureType_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
SpeciesFeatureType_t *
SpeciesFeatureType_clone(SpeciesFeatureType_t * sft);


/**
 * Returns the value of the "id" attribute of this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this SpeciesFeatureType_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
char *
SpeciesFeatureType_getId(const SpeciesFeatureType_t * sft);


/**
 * Returns the value of the "name" attribute of this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this SpeciesFeatureType_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
char *
SpeciesFeatureType_getName(const SpeciesFeatureType_t * sft);


/**
 * Returns the value of the "occur" attribute of this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure whose occur is sought.
 *
 * @return the value of the "occur" attribute of this SpeciesFeatureType_t as a
 * unsigned integer.
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
unsigned int
SpeciesFeatureType_getOccur(const SpeciesFeatureType_t * sft);


/**
 * Predicate returning @c 1 (true) if this SpeciesFeatureType_t's "id"
 * attribute is set.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @return @c 1 (true) if this SpeciesFeatureType_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_isSetId(const SpeciesFeatureType_t * sft);


/**
 * Predicate returning @c 1 (true) if this SpeciesFeatureType_t's "name"
 * attribute is set.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @return @c 1 (true) if this SpeciesFeatureType_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_isSetName(const SpeciesFeatureType_t * sft);


/**
 * Predicate returning @c 1 (true) if this SpeciesFeatureType_t's "occur"
 * attribute is set.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @return @c 1 (true) if this SpeciesFeatureType_t's "occur" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_isSetOccur(const SpeciesFeatureType_t * sft);


/**
 * Sets the value of the "id" attribute of this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling SpeciesFeatureType_unsetId().
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_setId(SpeciesFeatureType_t * sft, const char * id);


/**
 * Sets the value of the "name" attribute of this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling SpeciesFeatureType_unsetName().
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_setName(SpeciesFeatureType_t * sft, const char * name);


/**
 * Sets the value of the "occur" attribute of this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @param occur unsigned int value of the "occur" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_setOccur(SpeciesFeatureType_t * sft, unsigned int occur);


/**
 * Unsets the value of the "id" attribute of this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_unsetId(SpeciesFeatureType_t * sft);


/**
 * Unsets the value of the "name" attribute of this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_unsetName(SpeciesFeatureType_t * sft);


/**
 * Unsets the value of the "occur" attribute of this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_unsetOccur(SpeciesFeatureType_t * sft);


/**
 * Adds a copy of the given PossibleSpeciesFeatureValue_t to this
 * SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure to which the
 * PossibleSpeciesFeatureValue_t should be added.
 *
 * @param psfv the PossibleSpeciesFeatureValue_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_addPossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft, PossibleSpeciesFeatureValue_t * psfv);


/**
 * Creates a new PossibleSpeciesFeatureValue_t object, adds it to this
 * SpeciesFeatureType_t object and returns the PossibleSpeciesFeatureValue_t
 * object created.
 *
 * @param sft the SpeciesFeatureType_t structure to which the
 * PossibleSpeciesFeatureValue_t should be added.
 *
 * @return a new PossibleSpeciesFeatureValue_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_createPossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft);


/**
 * Returns a ListOf_t * containing PossibleSpeciesFeatureValue_t objects from
 * this SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure whose
 * ListOfPossibleSpeciesFeatureValues is sought.
 *
 * @return the ListOfPossibleSpeciesFeatureValues from this
 * SpeciesFeatureType_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see SpeciesFeatureType_addPossibleSpeciesFeatureValue()
 * @see SpeciesFeatureType_createPossibleSpeciesFeatureValue()
 * @see SpeciesFeatureType_getPossibleSpeciesFeatureValueById()
 * @see SpeciesFeatureType_getPossibleSpeciesFeatureValue()
 * @see SpeciesFeatureType_getNumPossibleSpeciesFeatureValues()
 * @see SpeciesFeatureType_removePossibleSpeciesFeatureValueById()
 * @see SpeciesFeatureType_removePossibleSpeciesFeatureValue()
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
ListOf_t *
SpeciesFeatureType_getListOfPossibleSpeciesFeatureValues(SpeciesFeatureType_t * sft) ;


/**
 * Get a PossibleSpeciesFeatureValue_t from the SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * PossibleSpeciesFeatureValue_t to retrieve.
 *
 * @return the nth PossibleSpeciesFeatureValue_t in the
 * ListOfPossibleSpeciesFeatureValues within this SpeciesFeatureType.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_getPossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft, unsigned int n);


/**
 * Get a PossibleSpeciesFeatureValue_t from the SpeciesFeatureType_t based on
 * its identifier.
 *
 * @param sft the SpeciesFeatureType_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * PossibleSpeciesFeatureValue_t to retrieve.
 *
 * @return the PossibleSpeciesFeatureValue_t in the
 * ListOfPossibleSpeciesFeatureValues within this SpeciesFeatureType with the
 * given @p sid or @c NULL if no such PossibleSpeciesFeatureValue_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_getPossibleSpeciesFeatureValueById(SpeciesFeatureType_t * sft, const char * sid);


/**
 * Get the number of PossibleSpeciesFeatureValue_t objects in this
 * SpeciesFeatureType_t.
 *
 * @param sft the SpeciesFeatureType_t structure to query.
 *
 * @return the number of PossibleSpeciesFeatureValue_t objects in this
 * SpeciesFeatureType_t.
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
unsigned int
SpeciesFeatureType_getNumPossibleSpeciesFeatureValues(SpeciesFeatureType_t * sft);


/**
 * Removes the nth PossibleSpeciesFeatureValue_t from this SpeciesFeatureType_t
 * and returns a pointer to it.
 *
 * @param sft the SpeciesFeatureType_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * PossibleSpeciesFeatureValue_t to remove.
 *
 * @return a pointer to the nth PossibleSpeciesFeatureValue_t in this
 * SpeciesFeatureType_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_removePossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft, unsigned int n);


/**
 * Removes the PossibleSpeciesFeatureValue_t from this SpeciesFeatureType_t
 * based on its identifier and returns a pointer to it.
 *
 * @param sft the SpeciesFeatureType_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * PossibleSpeciesFeatureValue_t to remove.
 *
 * @return the PossibleSpeciesFeatureValue_t in this SpeciesFeatureType_t based
 * on the identifier or NULL if no such PossibleSpeciesFeatureValue_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_removePossibleSpeciesFeatureValueById(SpeciesFeatureType_t * sft, const char * sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SpeciesFeatureType_t object have been set.
 *
 * @param sft the SpeciesFeatureType_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SpeciesFeatureType_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the SpeciesFeatureType_t object are:
 * @li "id"
 * @li "occur"
 *
 * @memberof SpeciesFeatureType_t
 */
LIBSBML_EXTERN
int
SpeciesFeatureType_hasRequiredAttributes(SpeciesFeatureType_t * sft);


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
 * @memberof ListOfSpeciesFeatureTypes_t
 */
LIBSBML_EXTERN
SpeciesFeatureType_t *
ListOfSpeciesFeatureTypes_getById(ListOf_t * lo, const char * sid);


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
 * @memberof ListOfSpeciesFeatureTypes_t
 */
LIBSBML_EXTERN
SpeciesFeatureType_t *
ListOfSpeciesFeatureTypes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpeciesFeatureType_H__  */

