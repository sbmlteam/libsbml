/**
 * @file:   SpeciesFeatureType.h
 * @brief:  Implementation of the SpeciesFeatureType class
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
   * @return a List* of pointers to all child objects.
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
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
SpeciesFeatureType_t *
SpeciesFeatureType_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
void
SpeciesFeatureType_free(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
SpeciesFeatureType_t *
SpeciesFeatureType_clone(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
char *
SpeciesFeatureType_getId(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
char *
SpeciesFeatureType_getName(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
unsigned int
SpeciesFeatureType_getOccur(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_isSetId(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_isSetName(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_isSetOccur(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_setId(SpeciesFeatureType_t * sft, const char * id);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_setName(SpeciesFeatureType_t * sft, const char * name);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_setOccur(SpeciesFeatureType_t * sft, unsigned int occur);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_unsetId(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_unsetName(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_unsetOccur(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_addPossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft, PossibleSpeciesFeatureValue_t * psfv);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_createPossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
ListOf_t *
SpeciesFeatureType_getListOfPossibleSpeciesFeatureValues(SpeciesFeatureType_t * sft) ;


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_getPossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft, unsigned int n);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_getPossibleSpeciesFeatureValueById(SpeciesFeatureType_t * sft, const char * sid);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
unsigned int
SpeciesFeatureType_getNumPossibleSpeciesFeatureValues(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_removePossibleSpeciesFeatureValue(SpeciesFeatureType_t * sft, unsigned int n);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
PossibleSpeciesFeatureValue_t *
SpeciesFeatureType_removePossibleSpeciesFeatureValueById(SpeciesFeatureType_t * sft, const char * sid);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_hasRequiredAttributes(SpeciesFeatureType_t * sft);


/**
* @memberof SpeciesFeatureType_t
*/
LIBSBML_EXTERN
int
SpeciesFeatureType_hasRequiredElements(SpeciesFeatureType_t * sft);


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

