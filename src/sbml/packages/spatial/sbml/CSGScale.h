/*
 * @file    CSGScale.h
 * @brief   Definition of CSGScale, of spatial package.
 * @author  
 *
 * $Id: CSGScale.h  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGScale.h $
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


#ifndef CSGScale_H__
#define CSGScale_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGTransformation.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGScale : public CSGTransformation
{
protected:

  double mScaleX;
  double mScaleY;
  double mScaleZ;

  bool  mIsSetScaleX;
  bool  mIsSetScaleY;
  bool  mIsSetScaleZ;
  
public:

  /**
   * Creates a new CSGScale with the given level, version, and package version.
   */
   CSGScale(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGScale with the given SpatialPkgNamespaces object.
   */
   CSGScale(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   CSGScale(const CSGScale& source);


  /**
   * Assignment operator.
   */
   CSGScale& operator=(const CSGScale& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGScale ();

  /**
   * Get the scaleX of this CSGScale object.
   * 
   * @return the value of the "scaleX" attribute of this
   * CSGScale as an unsigned integer
   */
  double getScaleX () const;


  /**
   * Get the spatial dimensions of this CSGScale object.
   * 
   * @return the value of the "scaleY" attribute of this
   * CSGScale as an unsigned integer
   */
  double getScaleY () const;


  /**
   * Get the spatial dimensions of this CSGScale object.
   * 
   * @return the value of the "scaleZ" attribute of this
   * CSGScale as an unsigned integer
   */
  double getScaleZ () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGScale's "scaleX" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "scaleX" attribute of this CSGScale has
   * been set, @c false otherwise.
   */
  bool isSetScaleX () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGScale's "scaleY" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "scaleY" attribute of this CSGScale has
   * been set, @c false otherwise.
   */
  bool isSetScaleY () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGScale's "scaleZ" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "scaleZ" attribute of this CSGScale has
   * been set, @c false otherwise.
   */
  bool isSetScaleZ () const;

  /**
   * Sets the "scaleX" attribute of this CSGScale.
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
  int setScaleX (double value);

  /**
   * Sets the "scaleY" attribute of this CSGScale.
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
  int setScaleY (double value);

  /**
   * Sets the "scaleZ" attribute of this CSGScale.
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
  int setScaleZ (double value);

  /**
   * Unsets the value of the "scaleX" attribute of this CSGScale.
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
  int unsetScaleX ();


  /**
   * Unsets the value of the "scaleY" attribute of this CSGScale.
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
  int unsetScaleY ();


  /**
   * Unsets the value of the "scaleZ" attribute of this CSGScale.
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
  int unsetScaleZ ();


  /**
   * @return a (deep) copy of this CSGScale.
   */
  virtual CSGScale* clone () const;


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
CSGScale_getScaleX (const CSGScale_t *csgo);


LIBSBML_EXTERN
double
CSGScale_getScaleY (const CSGScale_t *csgo);


LIBSBML_EXTERN
double
CSGScale_getScaleZ (const CSGScale_t *csgo);


LIBSBML_EXTERN
int
CSGScale_isSetScaleX (const CSGScale_t *c);



LIBSBML_EXTERN
int
CSGScale_isSetScaleY (const CSGScale_t *c);



LIBSBML_EXTERN
int
CSGScale_isSetScaleZ (const CSGScale_t *c);



LIBSBML_EXTERN
int
CSGScale_setScaleX (CSGScale_t *c, double val);



LIBSBML_EXTERN
int
CSGScale_setScaleY (CSGScale_t *c, double val);



LIBSBML_EXTERN
int
CSGScale_setScaleZ (CSGScale_t *c, double val);



LIBSBML_EXTERN
int
CSGScale_unsetScaleX (CSGScale_t *csgo);



LIBSBML_EXTERN
int
CSGScale_unsetScaleY (CSGScale_t *csgo);



LIBSBML_EXTERN
int
CSGScale_unsetScaleZ (CSGScale_t *csgo);



LIBSBML_EXTERN
CSGScale_t *
CSGScale_clone (const CSGScale_t* c);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGScale_H__ */
