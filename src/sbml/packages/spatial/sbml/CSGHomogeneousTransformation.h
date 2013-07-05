/*
 * @file    CSGHomogeneousTransformation.h
 * @brief   Definition of CSGHomogeneousTransformation, of spatial package.
 * @author  
 *
 * $Id: CSGHomogeneousTransformation.h  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGHomogeneousTransformation.h $
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


#ifndef CSGHomogeneousTransformation_H__
#define CSGHomogeneousTransformation_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/TransformationComponents.h>
#include <sbml/packages/spatial/sbml/CSGTransformation.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGHomogeneousTransformation : public CSGTransformation
{
protected:

  TransformationComponents* mForwardTransform;
  TransformationComponents* mInverseTransform;
  
public:

  /**
   * Creates a new CSGHomogeneousTransformation with the given level, version, and package version.
   */
   CSGHomogeneousTransformation(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGHomogeneousTransformation with the given SpatialPkgNamespaces object.
   */
   CSGHomogeneousTransformation(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   CSGHomogeneousTransformation(const CSGHomogeneousTransformation& source);


  /**
   * Assignment operator.
   */
   CSGHomogeneousTransformation& operator=(const CSGHomogeneousTransformation& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGHomogeneousTransformation ();

  /**
   * Returns the "forwardTransform" attribute of this CSGHomogeneousTransformation.
   *
   * @return the "forwardTransform" attribute of this CSGHomogeneousTransformation.
   */
  virtual const TransformationComponents* getForwardTransform() const;

  /**
   * Returns the "inverseTransform" attribute of this CSGHomogeneousTransformation.
   *
   * @return the "inverseTransform" attribute of this CSGHomogeneousTransformation.
   */
  virtual const TransformationComponents* getInverseTransform() const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGHomogeneousTransformation's "forwardTransform" attribute has been set.
   *
   * @return @c true if this CSGHomogeneousTransformation's "forwardTransform" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetForwardTransform () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGHomogeneousTransformation's "inverseTransform" attribute has been set.
   *
   * @return @c true if this CSGHomogeneousTransformation's "inverseTransform" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetInverseTransform () const;
  
  /**
   * Sets the SIdRef string of the "forwardTransform" attribute of this CSGHomogeneousTransformation.
   *
   * @param transformationComponents 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setForwardTransform (const TransformationComponents* transformationComponents);

  /**
   * Sets the SIdRef string of the "inverseTransform" attribute of this CSGHomogeneousTransformation.
   *
   * @param transformationComponents 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setInverseTransform (const TransformationComponents* transformationComponents);

  /**
   * Creates a new forwardTransform object, installs it as this CSGHomogeneousTransformation's 
   * "forwardTransform" subelement, and returns it.
   *
   * If this CSGHomogeneousTransformation had a previous forwardTransform, it will be destroyed.
   *
   * @return the new forwardTransform object
   */
  TransformationComponents* createForwardTransform ();

  /**
   * Creates a new inverseTransform object, installs it as this CSGHomogeneousTransformation's 
   * "inverseTransform" subelement, and returns it.
   *
   * If this CSGHomogeneousTransformation had a previous inverseTransform, it will be destroyed.
   *
   * @return the new inverseTransform object
   */
  TransformationComponents* createInverseTransform ();

  /**
   * @return a (deep) copy of this CSGHomogeneousTransformation.
   */
  virtual CSGHomogeneousTransformation* clone () const;

  /** @cond doxygen-libsbml-internal */
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
   * Predicate returning @c true or @c false depending on whether
   * all the required elements for this CSGHomogeneousTransformation object
   * have been set.
   *
   * @note The required elements for a CSGHomogeneousTransformation object are:
   * transformationComponents
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const ;

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygen-libsbml-internal */
 
  /** @cond doxygen-libsbml-internal */
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
  /** @endcond doxygen-libsbml-internal */


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
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);

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

 /** @endcond doxygen-libsbml-internal */

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
const TransformationComponents_t *
CSGHomogeneousTransformation_getForwardTransform (const CSGHomogeneousTransformation_t *csgt);



LIBSBML_EXTERN
const TransformationComponents_t *
CSGHomogeneousTransformation_getInverseTransform (const CSGHomogeneousTransformation_t *csgt);



LIBSBML_EXTERN
int
CSGHomogeneousTransformation_isSetForwardTransform (const CSGHomogeneousTransformation_t *csgt);



LIBSBML_EXTERN
int
CSGHomogeneousTransformation_isSetInverseTransform (const CSGHomogeneousTransformation_t *csgt);



LIBSBML_EXTERN
int
CSGHomogeneousTransformation_setForwardTransform (CSGHomogeneousTransformation_t *csgt, const TransformationComponents_t *tc);



LIBSBML_EXTERN
int
CSGHomogeneousTransformation_setInverseTransform (CSGHomogeneousTransformation_t *csgt, const TransformationComponents_t *tc);



LIBSBML_EXTERN
TransformationComponents_t *
CSGHomogeneousTransformation_createForwardTransform (CSGHomogeneousTransformation_t *csgt);



LIBSBML_EXTERN
TransformationComponents_t *
CSGHomogeneousTransformation_createInverseTransform (CSGHomogeneousTransformation_t *csgt);



LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGHomogeneousTransformation_clone (const CSGHomogeneousTransformation_t* c);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGHomogeneousTransformation_H__ */
