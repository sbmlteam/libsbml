/**
 * @file:   FbcAssociation.h
 * @brief:  Implementation of the FbcAssociation class
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
 *
 * @class FbcAssociation
 * @sbmlbrief{fbc} The abstract base class for the FbcAnd, FbcOr, and 
 * GeneProductRef classes.
 *
 * The FbcAssociation class is the abstract base class for the classes that 
 * can be used as children of the GeneProductAssociation child of a Reaction.
 * The FbcAnd class is used when all of its children are definitely associated
 * with the Reaction; the FbcOr class is used when at least one of its children
 * are associated with the Reaction; and the GeneProductRef class is used to
 * denote a particular GeneProduct.
 */


#ifndef FbcAssociation_H__
#define FbcAssociation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN

class FbcAnd;
class FbcOr;
class GeneProductRef;
class FbcModelPlugin;


class LIBSBML_EXTERN FbcAssociation : public SBase
{

protected:
  /** @cond doxygenLibsbmlInternal */
  std::string   mElementName;
  /** @endcond */

public:

  /**
   * Creates a new FbcAssociation with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this FbcAssociation
   *
   * @param version an unsigned int, the SBML Version to assign to this FbcAssociation
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this FbcAssociation
   */
  FbcAssociation(unsigned int level      = FbcExtension::getDefaultLevel(),
                 unsigned int version    = FbcExtension::getDefaultVersion(),
                 unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new FbcAssociation with the given FbcPkgNamespaces object.
   *
   * @param fbcns the FbcPkgNamespaces object
   */
  FbcAssociation(FbcPkgNamespaces* fbcns);


   /**
   * Copy constructor for FbcAssociation.
   *
   * @param orig; the FbcAssociation instance to copy.
   */
  FbcAssociation(const FbcAssociation& orig);


   /**
   * Assignment operator for FbcAssociation.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  FbcAssociation& operator=(const FbcAssociation& rhs);


   /**
   * Creates and returns a deep copy of this FbcAssociation object.
   *
   * @return a (deep) copy of this FbcAssociation object.
   */
  virtual FbcAssociation* clone () const;


   /**
   * Destructor for FbcAssociation.
   */
  virtual ~FbcAssociation();


   /**
   * Returns @c true, if this abstract "FbcAssociation" is of type FbcAnd.
   *
   * @return @c true, if this abstract "FbcAssociation" is of type FbcAnd.
   *
   */
  virtual bool isFbcAnd() const;


  /**
   * Returns @c true, if this abstract "FbcAssociation" is of type FbcOr.
   *
   * @return @c true, if this abstract "FbcAssociation" is of type FbcOr.
   *
   */
  virtual bool isFbcOr() const;


  /**
   * Returns @c true, if this abstract "FbcAssociation" is of type GeneProductRef.
   *
   * @return @c true, if this abstract "FbcAssociation" is of type GeneProductRef.
   *
   */
  virtual bool isGeneProductRef() const;


  /**
   * Returns the XML element name of this object, which for FbcAssociation, is
   * always @c "fbcAssociation".
   *
   * @return the name of this element, i.e. @c "fbcAssociation".
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
   * for this FbcAssociation object have been set.
   *
   * @note The required attributes for a FbcAssociation object are:
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


  /**
  * Parses a gene association in infix format and returns a corresponding
  * Association object. It also creates all geneProducts, in case the unique
  * reference does not yet exist. 
  *
  * This parses a string that has a list of gene names and conjunctions
  * or disjunctions.  For example:
  * @verbatim
  (b2422) and (b2425) and (b2423) and (b2424) or (b2422) and (b2423) and (b2424) and (b2413) and (b3917)
  @endverbatim
  *
  * @param association the string to parse.
  * @param plugin the FbcModelPlugin on which to add the geneProduct elements
  *
  * @return the parsed association, or @c NULL in case of an error.
  *
  * @copydetails doc_note_static_methods
  */
  static FbcAssociation* parseFbcInfixAssociation(const std::string& association, FbcModelPlugin* plugin);


  /**
  * Converts this FbcAssociation object into an infix string representation.
  *
  * @return the association as infix string.
  */
  virtual std::string toInfix() const;


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


  virtual void setElementName(const std::string& name);


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

/**
 * @class ListOfFbcAssociations
 * @sbmlbrief{fbc} A list of FbcAssociation objects.
 * 
 * The ListOfFbcAssociations is a container of FbcAssociation objects 
 * for the FbcAnd and FbcOr classes.  Unlike the ListOf___ classes in
 * SBML, the ListOfFbcAssociations is not a class that appears in the 
 * &ldquo;fbc&rdquo; specification, and instead is used in libsbml 
 * as a convenience class to store arbitrary numbers of FbcAssociation
 * objects.  Also unlike other ListOf___ classes, the FbcAnd and FbcOr 
 * classes require at least two child FbcAssociation objects, so valid
 * ListOfFbcAssociations libsbml objects will always contain two or more 
 * children.
 *
 * In addition, these children will have element names associated with 
 * their derived class, not the base FbcAssociation class:  
 * &ldquo;fbc:and&rdquo;, &ldquo;fbc:or&rdquo;, and &ldquo;fbc:geneProductRef&rdquo;.
 *
 * @see FbcAssociation
 * @see FbcAnd
 * @see FbcOr
 * @see GeneProductRef
 */
class LIBSBML_EXTERN ListOfFbcAssociations : public ListOf
{

public:

  /**
   * Creates a new ListOfFbcAssociations with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfFbcAssociations
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfFbcAssociations
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this ListOfFbcAssociations
   */
  ListOfFbcAssociations(unsigned int level      = FbcExtension::getDefaultLevel(),
                        unsigned int version    = FbcExtension::getDefaultVersion(),
                        unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfFbcAssociations with the given FbcPkgNamespaces object.
   *
   * @param fbcns the FbcPkgNamespaces object
   */
  ListOfFbcAssociations(FbcPkgNamespaces* fbcns);


   /**
   * Creates and returns a deep copy of this ListOfFbcAssociations object.
   *
   * @return a (deep) copy of this ListOfFbcAssociations object.
   */
  virtual ListOfFbcAssociations* clone () const;


   /**
   * Get a FbcAssociation from the ListOfFbcAssociations.
   *
   * @param n the index number of the FbcAssociation to get.
   *
   * @return the nth FbcAssociation in this ListOfFbcAssociations.
   *
   * @see size()
   */
  virtual FbcAssociation* get(unsigned int n);


  /**
   * Get a FbcAssociation from the ListOfFbcAssociations.
   *
   * @param n the index number of the FbcAssociation to get.
   *
   * @return the nth FbcAssociation in this ListOfFbcAssociations.
   *
   * @see size()
   */
  virtual const FbcAssociation* get(unsigned int n) const;


  /**
   * Get a FbcAssociation from the ListOfFbcAssociations
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the FbcAssociation to get.
   *
   * @return FbcAssociation in this ListOfFbcAssociations
   * with the given id or NULL if no such
   * FbcAssociation exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual FbcAssociation* get(const std::string& sid);


  /**
   * Get a FbcAssociation from the ListOfFbcAssociations
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the FbcAssociation to get.
   *
   * @return FbcAssociation in this ListOfFbcAssociations
   * with the given id or NULL if no such
   * FbcAssociation exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const FbcAssociation* get(const std::string& sid) const;


  /**
   * Adds a copy the given "FbcAssociation" to this ListOfFbcAssociations.
   *
   * @param fa; the FbcAssociation object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSEDML_OPERATION_SUCCESS
   * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
   */
  int addFbcAssociation(const FbcAssociation* fa);


  /**
   * Get the number of FbcAssociation objects in this ListOfFbcAssociations.
   *
   * @return the number of FbcAssociation objects in this ListOfFbcAssociations
   */
  unsigned int getNumFbcAssociations() const;


  /**
   * Creates a new FbcAssociation object, adds it to the
   * ListOfFbcAssociations and returns the FbcAssociation object created. 
   *
   * @return a new FbcAssociation object instance
   *
   * @see addFbcAssociation(const FbcAssociation* fa)
   */
  FbcAnd* createAnd();


  /**
   * Creates a new FbcAssociation object, adds it to the
   * ListOfFbcAssociations and returns the FbcAssociation object created. 
   *
   * @return a new FbcAssociation object instance
   *
   * @see addFbcAssociation(const FbcAssociation* fa)
   */
  FbcOr* createOr();


  /**
   * Creates a new FbcAssociation object, adds it to the
   * ListOfFbcAssociations and returns the FbcAssociation object created. 
   *
   * @return a new FbcAssociation object instance
   *
   * @see addFbcAssociation(const FbcAssociation* fa)
   */
  GeneProductRef* createGeneProductRef();


  /**
   * Removes the nth FbcAssociation from this ListOfFbcAssociations
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the FbcAssociation to remove.
   *
   * @see size()
   */
  virtual FbcAssociation* remove(unsigned int n);


  /**
   * Removes the FbcAssociation from this ListOfFbcAssociations with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the FbcAssociation to remove.
   *
   * @return the FbcAssociation removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual FbcAssociation* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfFbcAssociations, is
   * always @c "listOfFbcAssociations".
   *
   * @return the name of this element, i.e. @c "listOfFbcAssociations".
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
   * Creates a new FbcAssociation in this ListOfFbcAssociations
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Fbc package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  virtual bool isValidTypeForList(SBase * item);


  friend class FbcAnd;
  friend class FbcOr;

};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new FbcAssociation_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * FbcAssociation_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * FbcAssociation_t structure.
 *
 * @returns the newly-created FbcAssociation_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof FbcAssociation_t
 */
LIBSBML_EXTERN
FbcAssociation_t *
FbcAssociation_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion);


/**
 * Frees the given FbcAssociation_t structure.
 * 
 * @param fa the FbcAssociation_t structure to be freed.
 *
 * @memberof FbcAssociation_t
 */
LIBSBML_EXTERN
void
FbcAssociation_free(FbcAssociation_t * fa);


/**
 * Creates a deep copy of the given FbcAssociation_t structure.
 * 
 * @param fa the FbcAssociation_t structure to be copied.
 *
 * @returns a (deep) copy of the given FbcAssociation_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof FbcAssociation_t
 */
LIBSBML_EXTERN
FbcAssociation_t *
FbcAssociation_clone(FbcAssociation_t * fa);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given FbcAssociation_t structure have been set.
 *
 * @param fa the FbcAssociation_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of FbcAssociation_t
 */
LIBSBML_EXTERN
int
FbcAssociation_hasRequiredAttributes(const FbcAssociation_t * fa);


LIBSBML_EXTERN
FbcAssociation_t *
ListOfFbcAssociations_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
FbcAssociation_t *
ListOfFbcAssociations_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  FbcAssociation_H__  */

