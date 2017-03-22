/**
 * @file:   SpeciesTypeComponentMapInProduct.h
 * @brief:  Implementation of the SpeciesTypeComponentMapInProduct class
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

  std::string   mReactant;
  std::string   mReactantComponent;
  std::string   mProductComponent;


public:

  /**
   * Creates a new SpeciesTypeComponentMapInProduct with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SpeciesTypeComponentMapInProduct
   *
   * @param version an unsigned int, the SBML Version to assign to this SpeciesTypeComponentMapInProduct
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this SpeciesTypeComponentMapInProduct
   */
  SpeciesTypeComponentMapInProduct(unsigned int level      = MultiExtension::getDefaultLevel(),
                                   unsigned int version    = MultiExtension::getDefaultVersion(),
                                   unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpeciesTypeComponentMapInProduct with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  SpeciesTypeComponentMapInProduct(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for SpeciesTypeComponentMapInProduct.
   *
   * @param orig; the SpeciesTypeComponentMapInProduct instance to copy.
   */
  SpeciesTypeComponentMapInProduct(const SpeciesTypeComponentMapInProduct& orig);


   /**
   * Assignment operator for SpeciesTypeComponentMapInProduct.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SpeciesTypeComponentMapInProduct& operator=(const SpeciesTypeComponentMapInProduct& rhs);


   /**
   * Creates and returns a deep copy of this SpeciesTypeComponentMapInProduct object.
   *
   * @return a (deep) copy of this SpeciesTypeComponentMapInProduct object.
   */
  virtual SpeciesTypeComponentMapInProduct* clone () const;


   /**
   * Destructor for SpeciesTypeComponentMapInProduct.
   */
  virtual ~SpeciesTypeComponentMapInProduct();


  /**
  * Returns the value of the "id" attribute of this SpeciesTypeComponentMapInProduct.
  *
  * @return the value of the "id" attribute of this SpeciesTypeComponentMapInProduct as a string.
  */
 virtual const std::string& getId() const;


 /**
  * Predicate returning @c true or @c false depending on whether this
  * SpeciesTypeComponentMapInProduct's "id" attribute has been set.
  *
  * @return @c true if this SpeciesTypeComponentMapInProduct's "id" attribute has been set,
  * otherwise @c false is returned.
  */
 virtual bool isSetId() const;


 /**
  * Sets the value of the "id" attribute of this SpeciesTypeComponentMapInProduct.
  *
  * @param id; const std::string& value of the "id" attribute to be set
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
  */
 virtual int setId(const std::string& id);


 /**
  * Unsets the value of the "id" attribute of this SpeciesTypeComponentMapInProduct.
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_OPERATION_FAILED
  */
 virtual int unsetId();


 /**
  * Returns the value of the "name" attribute of this SpeciesTypeComponentMapInProduct.
  *
  * @return the value of the "name" attribute of this SpeciesTypeComponentMapInProduct as a string.
  */
 virtual const std::string& getName() const;


 /**
  * Predicate returning @c true or @c false depending on whether this
  * SpeciesTypeComponentMapInProduct's "name" attribute has been set.
  *
  * @return @c true if this SpeciesTypeComponentMapInProduct's "name" attribute has been set,
  * otherwise @c false is returned.
  */
 virtual bool isSetName() const;


 /**
  * Sets the value of the "name" attribute of this SpeciesTypeComponentMapInProduct.
  *
  * @param name; const std::string& value of the "name" attribute to be set
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
  */
 virtual int setName(const std::string& name);


 /**
  * Unsets the value of the "name" attribute of this SpeciesTypeComponentMapInProduct.
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_OPERATION_FAILED
  */
 virtual int unsetName();


   /**
   * Returns the value of the "reactant" attribute of this SpeciesTypeComponentMapInProduct.
   *
   * @return the value of the "reactant" attribute of this SpeciesTypeComponentMapInProduct as a string.
   */
  virtual const std::string& getReactant() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesTypeComponentMapInProduct's "reactant" attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentMapInProduct's "reactant" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetReactant() const;


  /**
   * Sets the value of the "reactant" attribute of this SpeciesTypeComponentMapInProduct.
   *
   * @param reactant; const std::string& value of the "reactant" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setReactant(const std::string& reactant);


  /**
   * Unsets the value of the "reactant" attribute of this SpeciesTypeComponentMapInProduct.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetReactant();


  /**
   * Returns the value of the "reactantComponent" attribute of this SpeciesTypeComponentMapInProduct.
   *
   * @return the value of the "reactantComponent" attribute of this SpeciesTypeComponentMapInProduct as a string.
   */
  virtual const std::string& getReactantComponent() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesTypeComponentMapInProduct's "reactantComponent" attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentMapInProduct's "reactantComponent" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetReactantComponent() const;


  /**
   * Sets the value of the "reactantComponent" attribute of this SpeciesTypeComponentMapInProduct.
   *
   * @param reactantComponent; const std::string& value of the "reactantComponent" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setReactantComponent(const std::string& reactantComponent);


  /**
   * Unsets the value of the "reactantComponent" attribute of this SpeciesTypeComponentMapInProduct.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetReactantComponent();


  /**
   * Returns the value of the "productComponent" attribute of this SpeciesTypeComponentMapInProduct.
   *
   * @return the value of the "productComponent" attribute of this SpeciesTypeComponentMapInProduct as a string.
   */
  virtual const std::string& getProductComponent() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesTypeComponentMapInProduct's "productComponent" attribute has been set.
   *
   * @return @c true if this SpeciesTypeComponentMapInProduct's "productComponent" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetProductComponent() const;


  /**
   * Sets the value of the "productComponent" attribute of this SpeciesTypeComponentMapInProduct.
   *
   * @param productComponent; const std::string& value of the "productComponent" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setProductComponent(const std::string& productComponent);


  /**
   * Unsets the value of the "productComponent" attribute of this SpeciesTypeComponentMapInProduct.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
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
   * Returns the XML element name of this object, which for SpeciesTypeComponentMapInProduct, is
   * always @c "speciesTypeComponentMapInProduct".
   *
   * @return the name of this element, i.e. @c "speciesTypeComponentMapInProduct".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
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


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};

class LIBSBML_EXTERN ListOfSpeciesTypeComponentMapInProducts : public ListOf
{

public:

  /**
   * Creates a new ListOfSpeciesTypeComponentMapInProducts with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfSpeciesTypeComponentMapInProducts
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfSpeciesTypeComponentMapInProducts
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this ListOfSpeciesTypeComponentMapInProducts
   */
  ListOfSpeciesTypeComponentMapInProducts(unsigned int level      = MultiExtension::getDefaultLevel(),
                                          unsigned int version    = MultiExtension::getDefaultVersion(),
                                          unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpeciesTypeComponentMapInProducts with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  ListOfSpeciesTypeComponentMapInProducts(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this ListOfSpeciesTypeComponentMapInProducts object.
   *
   * @return a (deep) copy of this ListOfSpeciesTypeComponentMapInProducts object.
   */
  virtual ListOfSpeciesTypeComponentMapInProducts* clone () const;


   /**
   * Get a SpeciesTypeComponentMapInProduct from the ListOfSpeciesTypeComponentMapInProducts.
   *
   * @param n the index number of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return the nth SpeciesTypeComponentMapInProduct in this ListOfSpeciesTypeComponentMapInProducts.
   *
   * @see size()
   */
  virtual SpeciesTypeComponentMapInProduct* get(unsigned int n);


  /**
   * Get a SpeciesTypeComponentMapInProduct from the ListOfSpeciesTypeComponentMapInProducts.
   *
   * @param n the index number of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return the nth SpeciesTypeComponentMapInProduct in this ListOfSpeciesTypeComponentMapInProducts.
   *
   * @see size()
   */
  virtual const SpeciesTypeComponentMapInProduct* get(unsigned int n) const;


  /**
   * Get a SpeciesTypeComponentMapInProduct from the ListOfSpeciesTypeComponentMapInProducts
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return SpeciesTypeComponentMapInProduct in this ListOfSpeciesTypeComponentMapInProducts
   * with the given id or NULL if no such
   * SpeciesTypeComponentMapInProduct exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual SpeciesTypeComponentMapInProduct* get(const std::string& sid);


  /**
   * Get a SpeciesTypeComponentMapInProduct from the ListOfSpeciesTypeComponentMapInProducts
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return SpeciesTypeComponentMapInProduct in this ListOfSpeciesTypeComponentMapInProducts
   * with the given id or NULL if no such
   * SpeciesTypeComponentMapInProduct exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SpeciesTypeComponentMapInProduct* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesTypeComponentMapInProduct from this ListOfSpeciesTypeComponentMapInProducts
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SpeciesTypeComponentMapInProduct to remove.
   *
   * @see size()
   */
  virtual SpeciesTypeComponentMapInProduct* remove(unsigned int n);


  /**
   * Removes the SpeciesTypeComponentMapInProduct from this ListOfSpeciesTypeComponentMapInProducts with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SpeciesTypeComponentMapInProduct to remove.
   *
   * @return the SpeciesTypeComponentMapInProduct removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual SpeciesTypeComponentMapInProduct* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfSpeciesTypeComponentMapInProducts, is
   * always @c "listOfSpeciesTypeComponentMapInProducts".
   *
   * @return the name of this element, i.e. @c "listOfSpeciesTypeComponentMapInProducts".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
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
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
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
   * Creates a new SpeciesTypeComponentMapInProduct in this ListOfSpeciesTypeComponentMapInProducts
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Multi package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
SpeciesTypeComponentMapInProduct_create(unsigned int level, unsigned int version,
                                        unsigned int pkgVersion);


LIBSBML_EXTERN
void
SpeciesTypeComponentMapInProduct_free(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
SpeciesTypeComponentMapInProduct_clone(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getId(SpeciesTypeComponentMapInProduct_t * cr);


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getName(SpeciesTypeComponentMapInProduct_t * cr);


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getReactant(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
char *
SpeciesTypeComponentMapInProduct_getProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetId(SpeciesTypeComponentMapInProduct_t * cr);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetName(SpeciesTypeComponentMapInProduct_t * cr);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetReactant(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_isSetProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setId(SpeciesTypeComponentMapInProduct_t * cr, const char * id);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setName(SpeciesTypeComponentMapInProduct_t * cr, const char * name);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setReactant(SpeciesTypeComponentMapInProduct_t * stcmip, const char * reactant);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip, const char * reactantComponent);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_setProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip, const char * productComponent);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetId(SpeciesTypeComponentMapInProduct_t * cr);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetName(SpeciesTypeComponentMapInProduct_t * cr);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetReactant(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetReactantComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_unsetProductComponent(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_hasRequiredAttributes(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
int
SpeciesTypeComponentMapInProduct_hasRequiredElements(SpeciesTypeComponentMapInProduct_t * stcmip);


LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
ListOfSpeciesTypeComponentMapInProducts_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t *
ListOfSpeciesTypeComponentMapInProducts_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpeciesTypeComponentMapInProduct_H__  */

