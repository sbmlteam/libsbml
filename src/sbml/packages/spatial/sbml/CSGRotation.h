/*
 * @file    CSGRotation.h
 * @brief   Definition of CSGRotation, of spatial package.
 * @author  
 *
 * $Id: CSGRotation.h  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGRotation.h $
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


#ifndef CSGRotation_H__
#define CSGRotation_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGTransformation.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGRotation : public CSGTransformation
{
protected:

  double mRotationAxisX;
  double mRotationAxisY;
  double mRotationAxisZ;
  double mRotationAngleInRadians;

  bool  mIsSetRotationAxisX;
  bool  mIsSetRotationAxisY;
  bool  mIsSetRotationAxisZ;
  bool  mIsSetRotationAngleInRadians;
  
public:

  /**
   * Creates a new CSGRotation with the given level, version, and package version.
   */
   CSGRotation(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGRotation with the given SpatialPkgNamespaces object.
   */
   CSGRotation(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   CSGRotation(const CSGRotation& source);


  /**
   * Assignment operator.
   */
   CSGRotation& operator=(const CSGRotation& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGRotation ();

  /**
   * Get the rotationAxisX of this CSGRotation object.
   * 
   * @return the value of the "rotationAxisX" attribute of this
   * CSGRotation as an unsigned integer
   */
  double getRotationAxisX () const;


  /**
   * Get the rotation axis of this CSGRotation object.
   * 
   * @return the value of the "rotationAxisY" attribute of this
   * CSGRotation as an unsigned integer
   */
  double getRotationAxisY () const;


  /**
   * Get the rotation axis of this CSGRotation object.
   * 
   * @return the value of the "rotationAxisZ" attribute of this
   * CSGRotation as an unsigned integer
   */
  double getRotationAxisZ () const;


  /**
   * Get the rotation angle (in radians) of this CSGRotation object.
   * 
   * @return the value of the "rotationAxisZ" attribute of this
   * CSGRotation as an unsigned integer
   */
  double getRotationAngleInRadians () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGRotation's "rotationAxisX" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "rotationAxisX" attribute of this CSGRotation has
   * been set, @c false otherwise.
   */
  bool isSetRotationAxisX () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGRotation's "rotationAxisY" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "rotationAxisY" attribute of this CSGRotation has
   * been set, @c false otherwise.
   */
  bool isSetRotationAxisY () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGRotation's "rotationAxisZ" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "rotationAxisZ" attribute of this CSGRotation has
   * been set, @c false otherwise.
   */
  bool isSetRotationAxisZ () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGRotation's "rotationAngleInRadians" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "rotationAngleInRadians" attribute of this CSGRotation has
   * been set, @c false otherwise.
   */
  bool isSetRotationAngleInRadians () const;

  /**
   * Sets the "rotationAxisX" attribute of this CSGRotation.
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
  int setRotationAxisX (double value);

  /**
   * Sets the "rotationAxisY" attribute of this CSGRotation.
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
  int setRotationAxisY (double value);

  /**
   * Sets the "rotationAxisZ" attribute of this CSGRotation.
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
  int setRotationAxisZ (double value);

  /**
   * Sets the "rotationAngleInRadians" attribute of this CSGRotation.
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
  int setRotationAngleInRadians (double value);

  /**
   * Unsets the value of the "rotationAxisX" attribute of this CSGRotation.
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
  int unsetRotationAxisX ();


  /**
   * Unsets the value of the "rotationAxisY" attribute of this CSGRotation.
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
  int unsetRotationAxisY ();


  /**
   * Unsets the value of the "rotationAxisZ" attribute of this CSGRotation.
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
  int unsetRotationAxisZ ();

  /**
   * Unsets the value of the "rotationAngleInRadians" attribute of this CSGRotation.
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
  int unsetRotationAngleInRadians ();


  /**
   * @return a (deep) copy of this CSGRotation.
   */
  virtual CSGRotation* clone () const;


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
CSGRotation_getRotationAxisX (const CSGRotation_t *csgo);


LIBSBML_EXTERN
double
CSGRotation_getRotationAxisY (const CSGRotation_t *csgo);


LIBSBML_EXTERN
double
CSGRotation_getRotationAxisZ (const CSGRotation_t *csgo);


LIBSBML_EXTERN
double
CSGRotation_getRotationAngleInRadians (const CSGRotation_t *csgo);


LIBSBML_EXTERN
int
CSGRotation_isSetRotationAxisX (const CSGRotation_t *c);



LIBSBML_EXTERN
int
CSGRotation_isSetRotationAxisY (const CSGRotation_t *c);



LIBSBML_EXTERN
int
CSGRotation_isSetRotationAxisZ (const CSGRotation_t *c);



LIBSBML_EXTERN
int
CSGRotation_isSetRotationAngleInRadians (const CSGRotation_t *c);



LIBSBML_EXTERN
int
CSGRotation_setRotationAxisX (CSGRotation_t *c, double val);



LIBSBML_EXTERN
int
CSGRotation_setRotationAxisY (CSGRotation_t *c, double val);



LIBSBML_EXTERN
int
CSGRotation_setRotationAxisZ (CSGRotation_t *c, double val);



LIBSBML_EXTERN
int
CSGRotation_setRotationAngleInRadians (CSGRotation_t *c, double val);



LIBSBML_EXTERN
int
CSGRotation_unsetRotationAxisX (CSGRotation_t *csgo);



LIBSBML_EXTERN
int
CSGRotation_unsetRotationAxisY (CSGRotation_t *csgo);



LIBSBML_EXTERN
int
CSGRotation_unsetRotationAxisZ (CSGRotation_t *csgo);



LIBSBML_EXTERN
int
CSGRotation_unsetRotationAngleInRadians (CSGRotation_t *csgo);



LIBSBML_EXTERN
CSGRotation_t *
CSGRotation_clone (const CSGRotation_t* c);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGRotation_H__ */
