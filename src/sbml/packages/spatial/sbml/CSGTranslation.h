/*
 * @file    CSGTranslation.h
 * @brief   Definition of CSGTranslation, of spatial package.
 * @author  
 *
 * $Id: CSGTranslation.h  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGTranslation.h $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */


#ifndef CSGTranslation_H__
#define CSGTranslation_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGTransformation.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGTranslation : public CSGTransformation
{
protected:

  double mTranslateX;
  double mTranslateY;
  double mTranslateZ;

  bool  mIsSetTranslateX;
  bool  mIsSetTranslateY;
  bool  mIsSetTranslateZ;
  
public:

  /**
   * Creates a new CSGTranslation with the given level, version, and package version.
   */
   CSGTranslation(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGTranslation with the given SpatialPkgNamespaces object.
   */
   CSGTranslation(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   CSGTranslation(const CSGTranslation& source);


  /**
   * Assignment operator.
   */
   CSGTranslation& operator=(const CSGTranslation& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGTranslation ();

  /**
   * Get the translateX of this CSGTranslation object.
   * 
   * @return the value of the "translateX" attribute of this
   * CSGTranslation as an unsigned integer
   */
  double getTranslateX () const;


  /**
   * Get the spatial dimensions of this CSGTranslation object.
   * 
   * @return the value of the "translateY" attribute of this
   * CSGTranslation as an unsigned integer
   */
  double getTranslateY () const;


  /**
   * Get the spatial dimensions of this CSGTranslation object.
   * 
   * @return the value of the "translateZ" attribute of this
   * CSGTranslation as an unsigned integer
   */
  double getTranslateZ () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTranslation's "translateX" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "translateX" attribute of this CSGTranslation has
   * been set, @c false otherwise.
   */
  bool isSetTranslateX () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTranslation's "translateY" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "translateY" attribute of this CSGTranslation has
   * been set, @c false otherwise.
   */
  bool isSetTranslateY () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTranslation's "translateZ" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "translateZ" attribute of this CSGTranslation has
   * been set, @c false otherwise.
   */
  bool isSetTranslateZ () const;

  /**
   * Sets the "translateX" attribute of this CSGTranslation.
   *
   * 
   * @param value an double indicating the number of dimensions
   * of this compartment.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  int setTranslateX (double value);

  /**
   * Sets the "translateY" attribute of this CSGTranslation.
   *
   * 
   * @param value an double indicating the number of dimensions
   * of this compartment.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  int setTranslateY (double value);

  /**
   * Sets the "translateZ" attribute of this CSGTranslation.
   *
   * @param value an double indicating the number of dimensions
   * of this compartment.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  int setTranslateZ (double value);

  /**
   * Unsets the value of the "translateX" attribute of this CSGTranslation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   *
   * @note This function is only valid for SBML L3
   */
  int unsetTranslateX ();


  /**
   * Unsets the value of the "translateY" attribute of this CSGTranslation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   *
   * @note This function is only valid for SBML L3
   */
  int unsetTranslateY ();


  /**
   * Unsets the value of the "translateZ" attribute of this CSGTranslation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   *
   * @note This function is only valid for SBML L3
   */
  int unsetTranslateZ ();


  /**
   * @return a (deep) copy of this CSGTranslation.
   */
  virtual CSGTranslation* clone () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactans.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygenLibsbmlInternal */
 
  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument object to use
   */
  virtual void setSBMLDocument (SBMLDocument* d);

  /**
   * Enables/Disables the given package with this element and child
   * elements (if any).
   * (This is an internal implementation for enablePakcage function)
   *
   * @note Subclasses in which one or more child elements are defined
   * must override this function.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);
  /** @endcond doxygenLibsbmlInternal */


  protected:
  
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   
  virtual SBase*
  createObject (XMLInputStream& stream);
  */

  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes, 
                               const ExpectedAttributes& expectedAttributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.  For example:
   *
   *   SBase::writeAttributes(stream);
   *   stream.writeAttribute( "id"  , mId   );
   *   stream.writeAttribute( "name", mName );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;

  /* the validator classes need to be friends to access the 
   * protected constructor that takes no arguments
   */
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
/*  
  friend class L1CompatibilityValidator;
  friend class L2v1CompatibilityValidator;
  friend class L2v2CompatibilityValidator;
  friend class L2v3CompatibilityValidator;
  friend class L2v4CompatibilityValidator;
  friend class MathMLConsistencyValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;
*/
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;

 /** @endcond doxygenLibsbmlInternal */

};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

//
// C API will be added here.
//


LIBSBML_EXTERN
double
CSGTranslation_getTranslateX (const CSGTranslation_t *csgo);


LIBSBML_EXTERN
double
CSGTranslation_getTranslateY (const CSGTranslation_t *csgo);


LIBSBML_EXTERN
double
CSGTranslation_getTranslateZ (const CSGTranslation_t *csgo);


LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateX (const CSGTranslation_t *c);



LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateY (const CSGTranslation_t *c);



LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateZ (const CSGTranslation_t *c);



LIBSBML_EXTERN
int
CSGTranslation_setTranslateX (CSGTranslation_t *c, double val);



LIBSBML_EXTERN
int
CSGTranslation_setTranslateY (CSGTranslation_t *c, double val);



LIBSBML_EXTERN
int
CSGTranslation_setTranslateZ (CSGTranslation_t *c, double val);



LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateX (CSGTranslation_t *csgo);



LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateY (CSGTranslation_t *csgo);



LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateZ (CSGTranslation_t *csgo);



LIBSBML_EXTERN
CSGTranslation_t *
CSGTranslation_clone (const CSGTranslation_t* c);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGTranslation_H__ */
