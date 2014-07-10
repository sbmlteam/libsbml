/**
 * @file:   CSGHomogeneousTransformation.h
 * @brief:  Implementation of the CSGHomogeneousTransformation class
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


#ifndef CSGHomogeneousTransformation_H__
#define CSGHomogeneousTransformation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGTransformation.h>

#include <sbml/packages/spatial/sbml/TransformationComponents.h>
#include <sbml/packages/spatial/sbml/TransformationComponents.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN CSGHomogeneousTransformation : public CSGTransformation
{

protected:

  TransformationComponents*      mForwardTransformation;
  TransformationComponents*      mReverseTransformation;


public:

  /**
   * Creates a new CSGHomogeneousTransformation with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGHomogeneousTransformation
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGHomogeneousTransformation
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGHomogeneousTransformation
   */
  CSGHomogeneousTransformation(unsigned int level      = SpatialExtension::getDefaultLevel(),
                               unsigned int version    = SpatialExtension::getDefaultVersion(),
                               unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGHomogeneousTransformation with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGHomogeneousTransformation(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGHomogeneousTransformation.
   *
   * @param orig; the CSGHomogeneousTransformation instance to copy.
   */
  CSGHomogeneousTransformation(const CSGHomogeneousTransformation& orig);


   /**
   * Assignment operator for CSGHomogeneousTransformation.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGHomogeneousTransformation& operator=(const CSGHomogeneousTransformation& rhs);


   /**
   * Creates and returns a deep copy of this CSGHomogeneousTransformation object.
   *
   * @return a (deep) copy of this CSGHomogeneousTransformation object.
   */
  virtual CSGHomogeneousTransformation* clone () const;


   /**
   * Destructor for CSGHomogeneousTransformation.
   */
  virtual ~CSGHomogeneousTransformation();


   /**
   * Returns the "forwardTransformation" element of this CSGHomogeneousTransformation.
   *
   * @return the "forwardTransformation" element of this CSGHomogeneousTransformation.
   */
  virtual const TransformationComponents* getForwardTransformation() const;


  /**
   * Returns the "forwardTransformation" element of this CSGHomogeneousTransformation.
   *
   * @return the "forwardTransformation" element of this CSGHomogeneousTransformation.
   */
  virtual TransformationComponents* getForwardTransformation();


  /**
   * Creates a new "TransformationComponents" and sets it for this CSGHomogeneousTransformation.
   *
   * @return the created "TransformationComponents" element of this CSGHomogeneousTransformation.
   */
  virtual TransformationComponents* createForwardTransformation();


  /**
   * Returns the "reverseTransformation" element of this CSGHomogeneousTransformation.
   *
   * @return the "reverseTransformation" element of this CSGHomogeneousTransformation.
   */
  virtual const TransformationComponents* getReverseTransformation() const;


  /**
   * Returns the "reverseTransformation" element of this CSGHomogeneousTransformation.
   *
   * @return the "reverseTransformation" element of this CSGHomogeneousTransformation.
   */
  virtual TransformationComponents* getReverseTransformation();


  /**
   * Creates a new "TransformationComponents" and sets it for this CSGHomogeneousTransformation.
   *
   * @return the created "TransformationComponents" element of this CSGHomogeneousTransformation.
   */
  virtual TransformationComponents* createReverseTransformation();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGHomogeneousTransformation's "forwardTransformation" element has been set.
   *
   * @return @c true if this CSGHomogeneousTransformation's "forwardTransformation" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetForwardTransformation() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGHomogeneousTransformation's "reverseTransformation" element has been set.
   *
   * @return @c true if this CSGHomogeneousTransformation's "reverseTransformation" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetReverseTransformation() const;


  /**
   * Sets the "forwardTransformation" element of this CSGHomogeneousTransformation.
   *
   * @param forwardTransformation; TransformationComponents* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setForwardTransformation(TransformationComponents* forwardTransformation);


  /**
   * Sets the "reverseTransformation" element of this CSGHomogeneousTransformation.
   *
   * @param reverseTransformation; TransformationComponents* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setReverseTransformation(TransformationComponents* reverseTransformation);


  /**
   * Unsets the "forwardTransformation" element of this CSGHomogeneousTransformation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetForwardTransformation();


  /**
   * Unsets the "reverseTransformation" element of this CSGHomogeneousTransformation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetReverseTransformation();


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for CSGHomogeneousTransformation, is
   * always @c "cSGHomogeneousTransformation".
   *
   * @return the name of this element, i.e. @c "cSGHomogeneousTransformation".
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
   * for this CSGHomogeneousTransformation object have been set.
   *
   * @note The required attributes for a CSGHomogeneousTransformation object are:
   * @li "forwardTransformation"
   * @li "reverseTransformation"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this CSGHomogeneousTransformation object have been set.
   *
   * @note The required elements for a CSGHomogeneousTransformation object are:
   * @li "forwardTransformation"
   * @li "reverseTransformation"
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
   * Connects to child elements.
   */
  virtual void connectToChild ();


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
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


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



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new CSGHomogeneousTransformation_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGHomogeneousTransformation_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGHomogeneousTransformation_t structure.
 *
 * @returns the newly-created CSGHomogeneousTransformation_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGHomogeneousTransformation_create(unsigned int level, unsigned int version,
                                    unsigned int pkgVersion);


/**
 * Frees the given CSGHomogeneousTransformation_t structure.
 * 
 * @param csght the CSGHomogeneousTransformation_t structure to be freed.
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
void
CSGHomogeneousTransformation_free(CSGHomogeneousTransformation_t * csght);


/**
 * Creates a deep copy of the given CSGHomogeneousTransformation_t structure.
 * 
 * @param csght the CSGHomogeneousTransformation_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGHomogeneousTransformation_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGHomogeneousTransformation_clone(CSGHomogeneousTransformation_t * csght);


LIBSBML_EXTERN
TransformationComponents_t*
CSGHomogeneousTransformation_getForwardTransformation(CSGHomogeneousTransformation_t * csght);


LIBSBML_EXTERN
TransformationComponents_t*
CSGHomogeneousTransformation_createForwardTransformation(CSGHomogeneousTransformation_t * csght);


LIBSBML_EXTERN
TransformationComponents_t*
CSGHomogeneousTransformation_getReverseTransformation(CSGHomogeneousTransformation_t * csght);


LIBSBML_EXTERN
TransformationComponents_t*
CSGHomogeneousTransformation_createReverseTransformation(CSGHomogeneousTransformation_t * csght);


/**
 * Predicate returning @c 1 if the given CSGHomogeneousTransformation_t structure's "forwardTransformation"
 * is set.
 *
 * @param csght the CSGHomogeneousTransformation_t structure.
 *
 * @return @c 1 if the "forwardTransformation" of this CSGHomogeneousTransformation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_isSetForwardTransformation(const CSGHomogeneousTransformation_t * csght);


/**
 * Predicate returning @c 1 if the given CSGHomogeneousTransformation_t structure's "reverseTransformation"
 * is set.
 *
 * @param csght the CSGHomogeneousTransformation_t structure.
 *
 * @return @c 1 if the "reverseTransformation" of this CSGHomogeneousTransformation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_isSetReverseTransformation(const CSGHomogeneousTransformation_t * csght);


LIBSBML_EXTERN
int
CSGHomogeneousTransformation_setForwardTransformation(CSGHomogeneousTransformation_t * csght, TransformationComponents_t* forwardTransformation);


LIBSBML_EXTERN
int
CSGHomogeneousTransformation_setReverseTransformation(CSGHomogeneousTransformation_t * csght, TransformationComponents_t* reverseTransformation);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGHomogeneousTransformation_t structure have been set.
 *
 * @param csght the CSGHomogeneousTransformation_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_hasRequiredAttributes(const CSGHomogeneousTransformation_t * csght);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given CSGHomogeneousTransformation_t structure have been set.
 *
 * @param csght the CSGHomogeneousTransformation_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_hasRequiredElements(const CSGHomogeneousTransformation_t * csght);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGHomogeneousTransformation_H__  */

