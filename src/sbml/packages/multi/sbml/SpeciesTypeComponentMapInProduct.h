/**
 * @file:   SpeciesTypeComponentMapInProduct.h
 * @brief:  Implementation of the SpeciesTypeComponentMapInProduct class
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
 * @class SpeciesTypeComponentMapInProduct
 * @sbmlbrief{multi} Distinguishes between components in reactants versus products.
 *
 * The SpeciesTypeComponentMapInProduct object is a child of a
 * SpeciesReference (via the MultiSpeciesReferencePlugin) in a Reaction, and
 * defines the mapping between a component in a reactant and a component in a
 * product. The identifications of a component and the SpeciesReference
 * should be sufficient to identify the component in the context of a
 * reaction. The attributes "reactant" and "reactantComponent" can identify
 * the component in a reactant, and the "productComponent" attribute and the
 * product storing the mapping information can identify the component in a
 * product.
 *
 * @class ListOfSpeciesTypeComponentMapInProducts
 * @sbmlbrief{multi} A list of SpeciesTypeComponentMapInProduct objects.
 *
 * The ListOfSpeciesTypeComponentMapInProducts is a container for
 * SpeciesTypeComponentMapInProduct objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see SpeciesTypeComponentMapInProduct
 */


#ifndef SpeciesTypeComponentMapInProduct_H__
#define SpeciesTypeComponentMapInProduct_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpeciesTypeComponentMapInProduct : public SBase
{

protected:

  /** @cond doxygenLibsbmlInternal */

  std::string   mReactant;
  std::string   mReactantComponent;
  std::string   mProductComponent;

  /** @endcond */


public:

  /**
   * Creates a new SpeciesTypeComponentMapInProduct object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesTypeComponentMapInProduct(unsigned int level      = MultiExtension::getDefaultLevel(),
                                   unsigned int version    = MultiExtension::getDefaultVersion(),
                                   unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpeciesTypeComponentMapInProduct with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesTypeComponentMapInProduct(MultiPkgNamespaces* multins);


  /**
   * Copy constructor for SpeciesTypeComponentMapInProduct.
   *
   * @param orig the SpeciesTypeComponentMapInProduct instance to copy.
   */
  SpeciesTypeComponentMapInProduct(const SpeciesTypeComponentMapInProduct& orig);


  /**
   * Assignment operator for SpeciesTypeComponentMapInProduct.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  SpeciesTypeComponentMapInProduct& operator=(const SpeciesTypeComponentMapInProduct& rhs);


  /**
   * Creates and returns a deep copy of this SpeciesTypeComponentMapInProduct
   * object.
   *
   * @return a (deep) copy of this SpeciesTypeComponentMapInProduct object.
   */
  virtual SpeciesTypeComponentMapInProduct* clone () const;


  /**
   * Destructor for SpeciesTypeComponentMapInProduct.
   */
  virtual ~SpeciesTypeComponentMapInProduct();


  /**
   * Returns the value of the "id" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @return the value of the "id" attribute of this
   * SpeciesTypeComponentMapInProduct as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns @c true if this SpeciesTypeComponentMapInProduct's "id" attribute
   * has been set.
   *
   * @return @c true if this SpeciesTypeComponentMapInProduct's "id" attribute
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @return the value of the "name" attribute of this
   * SpeciesTypeComponentMapInProduct as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this SpeciesTypeComponentMapInProduct's "name"
   * attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentMapInProduct's "name"
   * attribute has been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @param name const std::string& value of the "name" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "reactant" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @return the value of the "reactant" attribute of this
   * SpeciesTypeComponentMapInProduct as a string.
   */
  virtual const std::string& getReactant() const;


  /**
   * Returns @c true if this SpeciesTypeComponentMapInProduct's "reactant"
   * attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentMapInProduct's "reactant"
   * attribute has been set; otherwise, @c false is returned.
   */
  virtual bool isSetReactant() const;


  /**
   * Sets the value of the "reactant" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @param reactant const std::string& value of the "reactant" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setReactant(const std::string& reactant);


  /**
   * Unsets the value of the "reactant" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetReactant();


  /**
   * Returns the value of the "reactantComponent" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @return the value of the "reactantComponent" attribute of this
   * SpeciesTypeComponentMapInProduct as a string.
   */
  virtual const std::string& getReactantComponent() const;


  /**
   * Returns @c true if this SpeciesTypeComponentMapInProduct's
   * "reactantComponent" attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentMapInProduct's
   * "reactantComponent" attribute has been set; otherwise, @c false is
   * returned.
   */
  virtual bool isSetReactantComponent() const;


  /**
   * Sets the value of the "reactantComponent" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @param reactantComponent const std::string& value of the
   * "reactantComponent" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setReactantComponent(const std::string& reactantComponent);


  /**
   * Unsets the value of the "reactantComponent" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetReactantComponent();


  /**
   * Returns the value of the "productComponent" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @return the value of the "productComponent" attribute of this
   * SpeciesTypeComponentMapInProduct as a string.
   */
  virtual const std::string& getProductComponent() const;


  /**
   * Returns @c true if this SpeciesTypeComponentMapInProduct's
   * "productComponent" attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentMapInProduct's
   * "productComponent" attribute has been set, otherwise @c false is
   * returned.
   */
  virtual bool isSetProductComponent() const;


  /**
   * Sets the value of the "productComponent" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @param productComponent the new value of the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setProductComponent(const std::string& productComponent);


  /**
   * Unsets the value of the "productComponent" attribute of this
   * SpeciesTypeComponentMapInProduct.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetProductComponent();


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
   * @return the name of this element, i.e.
   * @c "speciesTypeComponentMapInProduct".
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
   * for this SpeciesTypeComponentMapInProduct object have been set.
   *
   * @note The required attributes for a SpeciesTypeComponentMapInProduct object are:
   * @li "reactant"
   * @li "reactantComponent"
   * @li "productComponent"
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

class LIBSBML_EXTERN ListOfSpeciesTypeComponentMapInProducts : public ListOf
{

public:

  /**
   * Creates a new ListOfSpeciesTypeComponentMapInProducts object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesTypeComponentMapInProducts(unsigned int level      = MultiExtension::getDefaultLevel(),
                                          unsigned int version    = MultiExtension::getDefaultVersion(),
                                          unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpeciesTypeComponentMapInProducts with the given
   * MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSpeciesTypeComponentMapInProducts(MultiPkgNamespaces* multins);


  /**
   * Creates and returns a deep copy of this
   * ListOfSpeciesTypeComponentMapInProducts object.
   *
   * @return a (deep) copy of this ListOfSpeciesTypeComponentMapInProducts object.
   */
  virtual ListOfSpeciesTypeComponentMapInProducts* clone () const;


  /**
   * Get the nth SpeciesTypeComponentMapInProduct object from the
   * ListOfSpeciesTypeComponentMapInProducts.
   *
   * @param n the index number of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual SpeciesTypeComponentMapInProduct* get(unsigned int n);


  /**
   * Get the nth SpeciesTypeComponentMapInProduct object from the
   * ListOfSpeciesTypeComponentMapInProducts.
   *
   * @param n the index number of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual const SpeciesTypeComponentMapInProduct* get(unsigned int n) const;


  /**
   * Get the SpeciesTypeComponentMapInProduct object with the given
   * identifier @p sid.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual SpeciesTypeComponentMapInProduct* get(const std::string& sid);


  /**
   * Get the SpeciesTypeComponentMapInProduct object with the given
   * identifier @p sid.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const SpeciesTypeComponentMapInProduct* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesTypeComponentMapInProduct object from this
   * ListOfSpeciesTypeComponentMapInProducts.
   *
   * @param n the index of the SpeciesTypeComponentMapInProduct to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see size()
   */
  virtual SpeciesTypeComponentMapInProduct* remove(unsigned int n);


  /**
   * Removes the SpeciesTypeComponentMapInProduct object with the given
   * identifier @p sid.
   *
   * @param sid the identifier of the SpeciesTypeComponentMapInProduct to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  virtual SpeciesTypeComponentMapInProduct* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e.
   * @c "listOfSpeciesTypeComponentMapInProducts".
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
   * Creates a new SpeciesTypeComponentMapInProduct in this ListOfSpeciesTypeComponentMapInProducts
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
 * Creates a new SpeciesTypeComponentMapInProduct_t using the given SBML Level,
 * Version and &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
SpeciesTypeComponentMapInProduct_create(unsigned int level, unsigned int version,
                                        unsigned int pkgVersion);


/**
 * Frees this SpeciesTypeComponentMapInProduct_t object.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
void
SpeciesTypeComponentMapInProduct_free(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Creates and returns a deep copy of this SpeciesTypeComponentMapInProduct_t
 * object.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @return a (deep) copy of this SpeciesTypeComponentMapInProduct_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
SpeciesTypeComponentMapInProduct_clone(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Returns the value of the "id" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure whose id is
 * sought.
 *
 * @return the value of the "id" attribute of this
 * SpeciesTypeComponentMapInProduct_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getId(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Returns the value of the "name" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure whose name is
 * sought.
 *
 * @return the value of the "name" attribute of this
 * SpeciesTypeComponentMapInProduct_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getName(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Returns the value of the "reactant" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure whose
 * reactant is sought.
 *
 * @return the value of the "reactant" attribute of this
 * SpeciesTypeComponentMapInProduct_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getReactant(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Returns the value of the "reactantComponent" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure whose
 * reactantComponent is sought.
 *
 * @return the value of the "reactantComponent" attribute of this
 * SpeciesTypeComponentMapInProduct_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Returns the value of the "productComponent" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure whose
 * productComponent is sought.
 *
 * @return the value of the "productComponent" attribute of this
 * SpeciesTypeComponentMapInProduct_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's
 * "id" attribute is set.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's "id"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetId(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's
 * "name" attribute is set.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's "name"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetName(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's
 * "reactant" attribute is set.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's "reactant"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetReactant(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's
 * "reactantComponent" attribute is set.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's
 * "reactantComponent" attribute has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Predicate returning @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's
 * "productComponent" attribute is set.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @return @c 1 (true) if this SpeciesTypeComponentMapInProduct_t's
 * "productComponent" attribute has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Sets the value of the "id" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling SpeciesTypeComponentMapInProduct_unsetId().
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setId(SpeciesTypeComponentMapInProduct_t * stcmip, const char * id);


/**
 * Sets the value of the "name" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling SpeciesTypeComponentMapInProduct_unsetName().
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setName(SpeciesTypeComponentMapInProduct_t * stcmip, const char * name);


/**
 * Sets the value of the "reactant" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @param reactant const char * value of the "reactant" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setReactant(SpeciesTypeComponentMapInProduct_t * stcmip, const char * reactant);


/**
 * Sets the value of the "reactantComponent" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @param reactantComponent const char * value of the "reactantComponent"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip, const char * reactantComponent);

/**
 * Sets the value of the "productComponent" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @param productComponent const char * value of the "productComponent"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip, const char * productComponent);


/**
 * Unsets the value of the "id" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetId(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Unsets the value of the "name" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetName(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Unsets the value of the "reactant" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetReactant(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Unsets the value of the "reactantComponent" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Unsets the value of the "productComponent" attribute of this
 * SpeciesTypeComponentMapInProduct_t.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SpeciesTypeComponentMapInProduct_t object have been set.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SpeciesTypeComponentMapInProduct_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required attributes for the SpeciesTypeComponentMapInProduct_t
 * object are:
 * @li "reactant"
 * @li "reactantComponent"
 * @li "productComponent"
 *
 * @memberof SpeciesTypeComponentMapInProduct_t
 */
LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_hasRequiredAttributes(SpeciesTypeComponentMapInProduct_t * stcmip);


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
 * @memberof ListOfSpeciesTypeComponentMapInProducts_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
ListOfSpeciesTypeComponentMapInProducts_getById(ListOf_t * lo, const char * sid);


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
 * @memberof ListOfSpeciesTypeComponentMapInProducts_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
ListOfSpeciesTypeComponentMapInProducts_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpeciesTypeComponentMapInProduct_H__  */

