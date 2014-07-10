/**
 * @file:   CSGTransformation.h
 * @brief:  Implementation of the CSGTransformation class
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


#ifndef CSGTransformation_H__
#define CSGTransformation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>

#include <sbml/packages/spatial/sbml/CSGNode.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class CSGTranslation;
class CSGRotation;
class CSGScale;
class CSGHomogeneousTransformation;



class LIBSBML_EXTERN CSGTransformation : public CSGNode
{

protected:

  CSGNode*      mCsgNode;


public:

  /**
   * Creates a new CSGTransformation with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGTransformation
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGTransformation
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGTransformation
   */
  CSGTransformation(unsigned int level      = SpatialExtension::getDefaultLevel(),
                    unsigned int version    = SpatialExtension::getDefaultVersion(),
                    unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGTransformation with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGTransformation(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGTransformation.
   *
   * @param orig; the CSGTransformation instance to copy.
   */
  CSGTransformation(const CSGTransformation& orig);


   /**
   * Assignment operator for CSGTransformation.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGTransformation& operator=(const CSGTransformation& rhs);


   /**
   * Creates and returns a deep copy of this CSGTransformation object.
   *
   * @return a (deep) copy of this CSGTransformation object.
   */
  virtual CSGTransformation* clone () const;


   /**
   * Destructor for CSGTransformation.
   */
  virtual ~CSGTransformation();


   /**
   * Returns the "csgNode" element of this CSGTransformation.
   *
   * @return the "csgNode" element of this CSGTransformation.
   */
  virtual const CSGNode* getCsgNode() const;


  /**
   * Returns the "csgNode" element of this CSGTransformation.
   *
   * @return the "csgNode" element of this CSGTransformation.
   */
  virtual CSGNode* getCsgNode();


  /**
   * Creates a new "csgNode" and sets it for this CSGTransformation.
   */
  virtual CSGPrimitive* createCsgPrimitive();


  /**
   * Creates a new "csgNode" and sets it for this CSGTransformation.
   */
  virtual CSGTranslation* createCsgTranslation();


  /**
   * Creates a new "csgNode" and sets it for this CSGTransformation.
   */
  virtual CSGRotation* createCsgRotation();


  /**
   * Creates a new "csgNode" and sets it for this CSGTransformation.
   */
  virtual CSGScale* createCsgScale();


  /**
   * Creates a new "csgNode" and sets it for this CSGTransformation.
   */
  virtual CSGHomogeneousTransformation* createCsgHomogeneousTransformation();


  /**
   * Creates a new "csgNode" and sets it for this CSGTransformation.
   */
  virtual CSGPseudoPrimitive* createCsgPseudoPrimitive();


  /**
   * Creates a new "csgNode" and sets it for this CSGTransformation.
   */
  virtual CSGSetOperator* createCsgSetOperator();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTransformation's "csgNode" element has been set.
   *
   * @return @c true if this CSGTransformation's "csgNode" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCsgNode() const;


  /**
   * Sets the "csgNode" element of this CSGTransformation.
   *
   * @param csgNode; CSGNode* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCsgNode(CSGNode* csgNode);


  /**
   * Unsets the "csgNode" element of this CSGTransformation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCsgNode();


  /**
   * Returns @c true, if this abstract "CSGTransformation" is of type CSGTranslation.
   *
   * @return @c true, if this abstract "CSGTransformation" is of type CSGTranslation.
   *
   */
  virtual bool isCSGTranslation() const;


  /**
   * Returns @c true, if this abstract "CSGTransformation" is of type CSGRotation.
   *
   * @return @c true, if this abstract "CSGTransformation" is of type CSGRotation.
   *
   */
  virtual bool isCSGRotation() const;


  /**
   * Returns @c true, if this abstract "CSGTransformation" is of type CSGScale.
   *
   * @return @c true, if this abstract "CSGTransformation" is of type CSGScale.
   *
   */
  virtual bool isCSGScale() const;


  /**
   * Returns @c true, if this abstract "CSGTransformation" is of type CSGHomogeneousTransformation.
   *
   * @return @c true, if this abstract "CSGTransformation" is of type CSGHomogeneousTransformation.
   *
   */
  virtual bool isCSGHomogeneousTransformation() const;


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for CSGTransformation, is
   * always @c "cSGTransformation".
   *
   * @return the name of this element, i.e. @c "cSGTransformation".
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
   * for this CSGTransformation object have been set.
   *
   * @note The required attributes for a CSGTransformation object are:
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this CSGTransformation object have been set.
   *
   * @note The required elements for a CSGTransformation object are:
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
 * Creates a new CSGTransformation_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGTransformation_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGTransformation_t structure.
 *
 * @returns the newly-created CSGTransformation_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGTransformation_t *
CSGTransformation_create(unsigned int level, unsigned int version,
                         unsigned int pkgVersion);


/**
 * Frees the given CSGTransformation_t structure.
 * 
 * @param csgt the CSGTransformation_t structure to be freed.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
void
CSGTransformation_free(CSGTransformation_t * csgt);


/**
 * Creates a deep copy of the given CSGTransformation_t structure.
 * 
 * @param csgt the CSGTransformation_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGTransformation_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGTransformation_t *
CSGTransformation_clone(CSGTransformation_t * csgt);


LIBSBML_EXTERN
CSGNode_t*
CSGTransformation_getCsgNode(CSGTransformation_t * csgt);


LIBSBML_EXTERN
CSGPrimitive_t *
CSGTransformation_createCsgPrimitive(CSGTransformation_t * csgt);


LIBSBML_EXTERN
CSGTranslation_t *
CSGTransformation_createCsgTranslation(CSGTransformation_t * csgt);


LIBSBML_EXTERN
CSGRotation_t *
CSGTransformation_createCsgRotation(CSGTransformation_t * csgt);


LIBSBML_EXTERN
CSGScale_t *
CSGTransformation_createCsgScale(CSGTransformation_t * csgt);


LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGTransformation_createCsgHomogeneousTransformation(CSGTransformation_t * csgt);


LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGTransformation_createCsgPseudoPrimitive(CSGTransformation_t * csgt);


LIBSBML_EXTERN
CSGSetOperator_t *
CSGTransformation_createCsgSetOperator(CSGTransformation_t * csgt);


/**
 * Predicate returning @c 1 if the given CSGTransformation_t structure's "csgNode"
 * is set.
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @return @c 1 if the "csgNode" of this CSGTransformation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isSetCsgNode(const CSGTransformation_t * csgt);


LIBSBML_EXTERN
int
CSGTransformation_setCsgNode(CSGTransformation_t * csgt, CSGNode_t* csgNode);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGTransformation_t structure have been set.
 *
 * @param csgt the CSGTransformation_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_hasRequiredAttributes(const CSGTransformation_t * csgt);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given CSGTransformation_t structure have been set.
 *
 * @param csgt the CSGTransformation_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_hasRequiredElements(const CSGTransformation_t * csgt);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGTransformation_H__  */

