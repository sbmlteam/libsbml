/**
 * @file:   GeneProduct.h
 * @brief:  Implementation of the GeneProduct class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
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


#ifndef GeneProduct_H__
#define GeneProduct_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN GeneProduct : public SBase
{

protected:

  std::string   mId;
  std::string   mName;
  std::string   mLabel;
  std::string   mAssociatedSpecies;


public:

  /**
   * Creates a new GeneProduct with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this GeneProduct
   *
   * @param version an unsigned int, the SBML Version to assign to this GeneProduct
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this GeneProduct
   */
  GeneProduct(unsigned int level      = FbcExtension::getDefaultLevel(),
              unsigned int version    = FbcExtension::getDefaultVersion(),
              unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new GeneProduct with the given FbcPkgNamespaces object.
   *
   * @param fbcns the FbcPkgNamespaces object
   */
  GeneProduct(FbcPkgNamespaces* fbcns);


   /**
   * Copy constructor for GeneProduct.
   *
   * @param orig; the GeneProduct instance to copy.
   */
  GeneProduct(const GeneProduct& orig);


   /**
   * Assignment operator for GeneProduct.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  GeneProduct& operator=(const GeneProduct& rhs);


   /**
   * Creates and returns a deep copy of this GeneProduct object.
   *
   * @return a (deep) copy of this GeneProduct object.
   */
  virtual GeneProduct* clone () const;


   /**
   * Destructor for GeneProduct.
   */
  virtual ~GeneProduct();


   /**
   * Returns the value of the "id" attribute of this GeneProduct.
   *
   * @return the value of the "id" attribute of this GeneProduct as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this GeneProduct.
   *
   * @return the value of the "name" attribute of this GeneProduct as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "label" attribute of this GeneProduct.
   *
   * @return the value of the "label" attribute of this GeneProduct as a string.
   */
  virtual const std::string& getLabel() const;


  /**
   * Returns the value of the "associatedSpecies" attribute of this GeneProduct.
   *
   * @return the value of the "associatedSpecies" attribute of this GeneProduct as a string.
   */
  virtual const std::string& getAssociatedSpecies() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneProduct's "id" attribute has been set.
   *
   * @return @c true if this GeneProduct's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneProduct's "name" attribute has been set.
   *
   * @return @c true if this GeneProduct's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneProduct's "label" attribute has been set.
   *
   * @return @c true if this GeneProduct's "label" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetLabel() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneProduct's "associatedSpecies" attribute has been set.
   *
   * @return @c true if this GeneProduct's "associatedSpecies" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetAssociatedSpecies() const;


  /**
   * Sets the value of the "id" attribute of this GeneProduct.
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
   * Sets the value of the "name" attribute of this GeneProduct.
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
   * Sets the value of the "label" attribute of this GeneProduct.
   *
   * @param label; const std::string& value of the "label" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setLabel(const std::string& label);


  /**
   * Sets the value of the "associatedSpecies" attribute of this GeneProduct.
   *
   * @param associatedSpecies; const std::string& value of the "associatedSpecies" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setAssociatedSpecies(const std::string& associatedSpecies);


  /**
   * Unsets the value of the "id" attribute of this GeneProduct.
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
   * Unsets the value of the "name" attribute of this GeneProduct.
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
   * Unsets the value of the "label" attribute of this GeneProduct.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetLabel();


  /**
   * Unsets the value of the "associatedSpecies" attribute of this GeneProduct.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetAssociatedSpecies();


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
   * Returns the XML element name of this object, which for GeneProduct, is
   * always @c "geneProduct".
   *
   * @return the name of this element, i.e. @c "geneProduct".
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
   * for this GeneProduct object have been set.
   *
   * @note The required attributes for a GeneProduct object are:
   * @li "id"
   * @li "label"
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

class LIBSBML_EXTERN ListOfGeneProducts : public ListOf
{

public:

  /**
   * Creates a new ListOfGeneProducts with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfGeneProducts
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfGeneProducts
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this ListOfGeneProducts
   */
  ListOfGeneProducts(unsigned int level      = FbcExtension::getDefaultLevel(),
                     unsigned int version    = FbcExtension::getDefaultVersion(),
                     unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfGeneProducts with the given FbcPkgNamespaces object.
   *
   * @param fbcns the FbcPkgNamespaces object
   */
  ListOfGeneProducts(FbcPkgNamespaces* fbcns);


   /**
   * Creates and returns a deep copy of this ListOfGeneProducts object.
   *
   * @return a (deep) copy of this ListOfGeneProducts object.
   */
  virtual ListOfGeneProducts* clone () const;


   /**
   * Get a GeneProduct from the ListOfGeneProducts.
   *
   * @param n the index number of the GeneProduct to get.
   *
   * @return the nth GeneProduct in this ListOfGeneProducts.
   *
   * @see size()
   */
	virtual GeneProduct* get(unsigned int n);


  /**
   * Get a GeneProduct from the ListOfGeneProducts.
   *
   * @param n the index number of the GeneProduct to get.
   *
   * @return the nth GeneProduct in this ListOfGeneProducts.
   *
   * @see size()
   */
	virtual const GeneProduct* get(unsigned int n) const;


  /**
   * Get a GeneProduct from the ListOfGeneProducts
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeneProduct to get.
   *
   * @return GeneProduct in this ListOfGeneProducts
   * with the given id or NULL if no such
   * GeneProduct exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual GeneProduct* get(const std::string& sid);


  /**
   * Get a GeneProduct from the ListOfGeneProducts
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeneProduct to get.
   *
   * @return GeneProduct in this ListOfGeneProducts
   * with the given id or NULL if no such
   * GeneProduct exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const GeneProduct* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "GeneProduct" to this ListOfGeneProducts.
	 *
	 * @param gp; the GeneProduct object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addGeneProduct(const GeneProduct* gp);


	/**
	 * Get the number of GeneProduct objects in this ListOfGeneProducts.
	 *
	 * @return the number of GeneProduct objects in this ListOfGeneProducts
	 */
	unsigned int getNumGeneProducts() const;


	/**
	 * Creates a new GeneProduct object, adds it to the
	 * ListOfGeneProducts and returns the GeneProduct object created. 
	 *
	 * @return a new GeneProduct object instance
	 *
	 * @see addGeneProduct(const GeneProduct* gp)
	 */
	GeneProduct* createGeneProduct();


  /**
   * Removes the nth GeneProduct from this ListOfGeneProducts
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the GeneProduct to remove.
   *
   * @see size()
   */
	virtual GeneProduct* remove(unsigned int n);


  /**
   * Removes the GeneProduct from this ListOfGeneProducts with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the GeneProduct to remove.
   *
   * @return the GeneProduct removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual GeneProduct* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfGeneProducts, is
   * always @c "listOfGeneProducts".
   *
   * @return the name of this element, i.e. @c "listOfGeneProducts".
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
   * Creates a new GeneProduct in this ListOfGeneProducts
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Fbc package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new GeneProduct_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * GeneProduct_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * GeneProduct_t structure.
 *
 * @returns the newly-created GeneProduct_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof GeneProduct_t
 */
LIBSBML_EXTERN
GeneProduct_t *
GeneProduct_create(unsigned int level, unsigned int version,
                   unsigned int pkgVersion);


/**
 * Frees the given GeneProduct_t structure.
 * 
 * @param gp the GeneProduct_t structure to be freed.
 *
 * @memberof GeneProduct_t
 */
LIBSBML_EXTERN
void
GeneProduct_free(GeneProduct_t * gp);


/**
 * Creates a deep copy of the given GeneProduct_t structure.
 * 
 * @param gp the GeneProduct_t structure to be copied.
 *
 * @returns a (deep) copy of the given GeneProduct_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof GeneProduct_t
 */
LIBSBML_EXTERN
GeneProduct_t *
GeneProduct_clone(GeneProduct_t * gp);


/**
 * Returns the value of the "id" attribute of the given GeneProduct_t
 * structure.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return the id of this structure.
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
const char *
GeneProduct_getId(const GeneProduct_t * gp);


/**
 * Returns the value of the "name" attribute of the given GeneProduct_t
 * structure.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return the name of this structure.
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
const char *
GeneProduct_getName(const GeneProduct_t * gp);


/**
 * Returns the value of the "label" attribute of the given GeneProduct_t
 * structure.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return the label of this structure.
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
const char *
GeneProduct_getLabel(const GeneProduct_t * gp);


/**
 * Returns the value of the "associatedSpecies" attribute of the given GeneProduct_t
 * structure.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return the associatedSpecies of this structure.
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
const char *
GeneProduct_getAssociatedSpecies(const GeneProduct_t * gp);


/**
 * Predicate returning @c 1 if the given GeneProduct_t structure's "id"
 * is set.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return @c 1 if the "id" of this GeneProduct_t structure is
 * set, @c 0 otherwise.
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_isSetId(const GeneProduct_t * gp);


/**
 * Predicate returning @c 1 if the given GeneProduct_t structure's "name"
 * is set.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return @c 1 if the "name" of this GeneProduct_t structure is
 * set, @c 0 otherwise.
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_isSetName(const GeneProduct_t * gp);


/**
 * Predicate returning @c 1 if the given GeneProduct_t structure's "label"
 * is set.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return @c 1 if the "label" of this GeneProduct_t structure is
 * set, @c 0 otherwise.
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_isSetLabel(const GeneProduct_t * gp);


/**
 * Predicate returning @c 1 if the given GeneProduct_t structure's "associatedSpecies"
 * is set.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return @c 1 if the "associatedSpecies" of this GeneProduct_t structure is
 * set, @c 0 otherwise.
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_isSetAssociatedSpecies(const GeneProduct_t * gp);


/**
 * Sets the "id" attribute of the given GeneProduct_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs GeneProduct_unsetId() instead.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @param id the string to which the structures "id" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_setId(GeneProduct_t * gp, const char * id);


/**
 * Sets the "name" attribute of the given GeneProduct_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs GeneProduct_unsetName() instead.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @param name the string to which the structures "name" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_setName(GeneProduct_t * gp, const char * name);


/**
 * Sets the "label" attribute of the given GeneProduct_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs GeneProduct_unsetLabel() instead.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @param label the string to which the structures "label" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_setLabel(GeneProduct_t * gp, const char * label);


/**
 * Sets the "associatedSpecies" attribute of the given GeneProduct_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs GeneProduct_unsetAssociatedSpecies() instead.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @param associatedSpecies the string to which the structures "associatedSpecies" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_setAssociatedSpecies(GeneProduct_t * gp, const char * associatedSpecies);


/**
 * Unsets the value of the "id" attribute of the given 
 * GeneProduct_t structure.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_unsetId(GeneProduct_t * gp);


/**
 * Unsets the value of the "name" attribute of the given 
 * GeneProduct_t structure.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_unsetName(GeneProduct_t * gp);


/**
 * Unsets the value of the "label" attribute of the given 
 * GeneProduct_t structure.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_unsetLabel(GeneProduct_t * gp);


/**
 * Unsets the value of the "associatedSpecies" attribute of the given 
 * GeneProduct_t structure.
 *
 * @param gp the GeneProduct_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_unsetAssociatedSpecies(GeneProduct_t * gp);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given GeneProduct_t structure have been set.
 *
 * @param gp the GeneProduct_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of GeneProduct_t
 */
LIBSBML_EXTERN
int
GeneProduct_hasRequiredAttributes(const GeneProduct_t * gp);


LIBSBML_EXTERN
GeneProduct_t *
ListOfGeneProducts_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
GeneProduct_t *
ListOfGeneProducts_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  GeneProduct_H__  */

