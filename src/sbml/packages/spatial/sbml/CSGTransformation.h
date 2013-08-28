/*
 * @file    CSGTransformation.h
 * @brief   Definition of CSGTransformation, of spatial package.
 * @author  
 *
 * $Id: CSGTransformation.h  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGTransformation.h $
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


#ifndef CSGTransformation_H__
#define CSGTransformation_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


BEGIN_C_DECLS

typedef enum
{
    CSGTRANSFORMATION_TYPE_CSGTRANSLATION
  , CSGTRANSFORMATION_TYPE_CSGROTATION
  , CSGTRANSFORMATION_TYPE_CSGSCALE
  , CSGTRANSFORMATION_TYPE_CSGHOMOGENEOUSTRANSFORMATION
  , CSGTRANSFORMATION_TYPE_INVALID
} CSGTransformationType_t;

END_C_DECLS


#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN

class CSGPrimitive;
class CSGPseudoPrimitive;
class CSGSetOperator;
class CSGTranslation;
class CSGRotation;
class CSGScale;
class CSGHomogeneousTransformation;


class LIBSBML_EXTERN CSGTransformation : public CSGNode
{
protected:

  CSGNode*	mChild;
  SBMLSpatialTypeCode_t mType;
  
public:

  //
  //  Only subclasses may create CSGTransformation.
  // 
   CSGTransformation(SBMLSpatialTypeCode_t	type
		  , unsigned int level      = SpatialExtension::getDefaultLevel()
          , unsigned int version    = SpatialExtension::getDefaultVersion());


   CSGTransformation( SBMLSpatialTypeCode_t	type
					, SpatialPkgNamespaces* spatialns);

  /**
   * Assignment operator.
   */
   CSGTransformation& operator=(const CSGTransformation& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGTransformation ();

 /**
   * Returns the "child" attribute of this CSGTransformation.
   *
   * @return the "child" attribute of this CSGTransformation.
   */
  virtual const CSGNode* getChild() const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTransformation's "child" attribute has been set.
   *
   * @return @c true if this CSGTransformation's "child" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetChild () const;
  
  /**
   * Sets the SIdRef string of the "child" attribute of this CSGTransformation.
   *
   * @param child 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setChild (const CSGNode* csgNodeChild);

 /**
   * Unsets the "child" subelement of this CSGTransformation.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  int unsetChild ();

  /**
   * Creates a new CSGPrimitive object inside this CSGTransformation and returns it.
   *
   * @return the CSGPrimitive object created
   */
  CSGPrimitive* createCSGPrimitive ();

  /**
   * Creates a new CSGPseudoPrimitive object inside this CSGTransformation and returns it.
   *
   * @return the CSGPseudoPrimitive object created
   */
  CSGPseudoPrimitive* createCSGPseudoPrimitive ();

  /**
   * Creates a new CSGSetOperator object inside this CSGTransformation and returns it.
   *
   * @return the CSGSetOperator object created
   */
  CSGSetOperator* createCSGSetOperator ();

  /**
   * Creates a new CSGTranslation object inside this CSGTransformation and returns it.
   *
   * @return the CSGTranslation object created
   */
  CSGTranslation* createCSGTranslation ();

  /**
   * Creates a new CSGRotation object inside this CSGTransformation and returns it.
   *
   * @return the CSGRotation object created
   */
  CSGRotation* createCSGRotation ();

  /**
   * Creates a new CSGScale object inside this CSGTransformation and returns it.
   *
   * @return the CSGScale object created
   */
  CSGScale* createCSGScale ();

  /**
   * Creates a new CSGHomogeneousTransformation object inside this CSGTransformation and returns it.
   *
   * @return the CSGHomogeneousTransformation object created
   */
  CSGHomogeneousTransformation* createCSGHomogeneousTransformation ();

 /**
   * (SBML Level&nbsp;1) Get the type of CSGTransformation this is.
   * 
   * @return the CSGtransformation type (a value drawn from the enumeration <a
   * class="el" href="#CSGTransformation_t">CSGTransformation_t</a>) of this CSGtTransformation.
   * The value will be either @c CSGTRANSFORMATION_TYPE_CSGTRANSLATION
   * , CSGTRANSFORMATION_TYPE_CSGROTATION
   * , CSGTRANSFORMATION_TYPE_CSGSCALE
   * , CSGTRANSFORMATION_TYPE_CSGHOMOGENEOUSTRANSFORMATION
   * , CSGNODE_TYPE_INVALID.
   */
  CSGTransformationType_t getType () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTransformation is an CSGTranslation.
   * 
   * @return @c true if this CSGTransformation is an CSGTranslation, @c false otherwise.
   */
  bool isCSGTranslation () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTransformation is an CSGRotation.
   * 
   * @return @c true if this CSGTransformation is an CSGRotation, @c false otherwise.
   */
  bool isCSGRotation () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTransformation is an CSGScale.
   * 
   * @return @c true if this CSGTransformation is an CSGScale, @c false otherwise.
   */
  bool isCSGScale () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTransformation is an CSGHomogeneousTransformation.
   * 
   * @return @c true if this CSGTransformation is an CSGHomogeneousTransformation, @c false otherwise.
   */
  bool isCSGHomogeneousTransformation () const;


  /**
   * @return a (deep) copy of this CSGTransformation.
   */
  virtual CSGTransformation* clone () const;


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */ 
  virtual const std::string& getElementName () const ;
  

  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;
   

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
   * Predicate returning @c true or @c false depending on whether
   * all the required elements for this CSGTransformation object
   * have been set.
   * 
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const {
	  return true;
  };

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygenLibsbmlInternal */
 

  /**
   * Sets this SBML object to child SBML objects (if any).
   * (Creates a child-parent relationship by the parent)
   *
   * Subclasses must override this function if they define
   * one ore more child elements.
   * Basically, this function needs to be called in
   * constructor, copy constructor, assignment operator.
   *
   * @see setSBMLDocument
   * @see enablePackageInternal
   */
  void connectToChild ();


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
   */ 
  virtual SBase*
  createObject (XMLInputStream& stream);
  

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
const char *
CSGTransformation_getTransformationType (const CSGTransformation_t *csgt);



LIBSBML_EXTERN
int
CSGTransformation_isSetTransformationType (const CSGTransformation_t *c);



LIBSBML_EXTERN
int
CSGTransformation_setTransformationType (CSGTransformation_t *c, const char *sid);



LIBSBML_EXTERN
int
CSGTransformation_unsetTransformationType (CSGTransformation_t *csgt);



LIBSBML_EXTERN
CSGTransformationType_t
CSGTransformation_getType (const CSGTransformation_t *csgt);


LIBSBML_EXTERN
int
CSGNode_isCSGTranslate (const CSGTransformation_t *csgt);


LIBSBML_EXTERN
int
CSGNode_isCSGScale (const CSGTransformation_t *csgt);


LIBSBML_EXTERN
int
CSGNode_isCSGRotate (const CSGTransformation_t *csgt);


LIBSBML_EXTERN
int
CSGNode_isCSGHomogeneousTransformation (const CSGTransformation_t *csgt);


LIBSBML_EXTERN
CSGTransformation_t *
CSGTransformation_clone (const CSGTransformation_t* csgt);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGTransformation_H__ */
