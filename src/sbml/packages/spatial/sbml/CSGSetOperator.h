/*
 * @file    CSGSetOperator.h
 * @brief   Definition of CSGSetOperator, of spatial package.
 * @author  
 *
 * $Id: CSGSetOperator.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGSetOperator.h $
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


#ifndef CSGSetOperator_H__
#define CSGSetOperator_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

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


class LIBSBML_EXTERN CSGSetOperator : public CSGNode
{
protected:

  std::string		mOperationType;
  ListOfCSGNodes	mCSGNodeChildren;

public:

  /**
   * Creates a new CSGSetOperator with the given level, version, and package version.
   */
   CSGSetOperator(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGSetOperator with the given SpatialPkgNamespaces object.
   */
   CSGSetOperator(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   CSGSetOperator(const CSGSetOperator& source);


  /**
   * Assignment operator.
   */
   CSGSetOperator& operator=(const CSGSetOperator& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGSetOperator ();

  /**
   * Returns the string of the "operationType" attribute of this CSGSetOperator.
   *
   * @return the string of the "operationType" attribute of this CSGSetOperator.
   */
  virtual const std::string& getOperationType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGSetOperator's "operationType" attribute has been set.
   *
   * @return @c true if this CSGSetOperator's "operationType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetOperationType () const;
  
  /**
   * Sets the SIdRef string of the "operationType" attribute of this CSGSetOperator.
   *
   * @param operationType a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setOperationType (const std::string& operationType);

  /**
   * Unsets the value of the "operationType" attribute of this CSGSetOperator.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetOperationType ();

  /**
   * Adds a copy of the given CSGNode object to this CSGSetOperator.
   *
   * @param d the CSGNode to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_LEVEL_MISMATCH
   * @li LIBSBML_VERSION_MISMATCH
   * @li LIBSBML_DUPLICATE_OBJECT_ID
   * @li LIBSBML_OPERATION_FAILED
   * 
   * @note This method should be used with some caution.  The fact that
   * this method @em copies the object passed to it means that the caller
   * will be left holding a physically different object instance than the
   * one contained in this CSGSetoperator.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the CSGSetoperator</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see CSGSetoperator::createCSGNodeChild()
   * for a method that does not lead to these issues.
   *
   * @see createCSGNodeChild()
   */
  int addCSGNodeChild (const CSGNode* csgn);

  /**
   * Creates a new CSGPrimitive object inside this CSGSetOperator and returns it.
   *
   * @return the CSGPrimitive object created
   */
  CSGPrimitive* createCSGPrimitive ();

  /**
   * Creates a new CSGPseudoPrimitive object inside this CSGSetOperator and returns it.
   *
   * @return the CSGPseudoPrimitive object created
   */
  CSGPseudoPrimitive* createCSGPseudoPrimitive ();

  /**
   * Creates a new CSGSetOperator object inside this CSGSetOperator and returns it.
   *
   * @return the CSGSetOperator object created
   */
  CSGSetOperator* createCSGSetOperator ();

  /**
   * Creates a new CSGTranslation object inside this CSGSetOperator and returns it.
   *
   * @return the CSGTranslation object created
   */
  CSGTranslation* createCSGTranslation ();

  /**
   * Creates a new CSGRotation object inside this CSGSetOperator and returns it.
   *
   * @return the CSGRotation object created
   */
  CSGRotation* createCSGRotation ();

  /**
   * Creates a new CSGScale object inside this CSGSetOperator and returns it.
   *
   * @return the CSGScale object created
   */
  CSGScale* createCSGScale ();

  /**
   * Creates a new CSGHomogeneousTransformation object inside this CSGSetOperator and returns it.
   *
   * @return the CSGHomogeneousTransformation object created
   */
  CSGHomogeneousTransformation* createCSGHomogeneousTransformation ();

  /**
   * Get the ListOfCSGNodes object in this CSGSetOperator.
   * 
   * @return the list of CSGNode for this CSGSetOperator.
   */
  const ListOfCSGNodes* getListOfCSGNodeChildren () const;


  /**
   * Get the ListOfCSGNodes object in this CSGSetOperator.
   * 
   * @return the list of CSGNode for this CSGSetOperator.
   */
  ListOfCSGNodes* getListOfCSGNodeChildren ();


  /**
   * Get the nth CSGNode object in this CSGSetOperator.
   * 
   * @return the nth CSGNode of this CSGSetOperator.
   */
  const CSGNode* getCSGNodeChild (unsigned int n) const;


  /**
   * Get the nth CSGNode object in this CSGSetOperator.
   * 
   * @return the nth CSGNode of this CSGSetOperator.
   */
  CSGNode* getCSGNodeChild (unsigned int n);


  /**
   * Get a CSGNode object based on its identifier.
   * 
   * @return the CSGNode in this CSGSetOperator with the identifier
   * @p sid or NULL if no such CSGNode exists.
   */
  const CSGNode* getCSGNodeChild (const std::string& sid) const;


  /**
   * Get a CSGNode object based on its identifier.
   * 
   * @return the CSGNode in this CSGSetOperator with the identifier
   * @p sid or NULL if no such CSGNode exists.
   */
  CSGNode* getCSGNodeChild (const std::string& sid);


  /**
   * Get the number of CSGNode objects in this CSGSetOperator.
   * 
   * @return the number of CSGNode in this CSGSetOperator.
   */
  unsigned int getNumCSGNodeChildren () const;


 /**
   * Removes the nth CSGNode object from this CSGSetOperator object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the CSGNode object to remove
   *
   * @return the CSGNode object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  CSGNode* removeCSGNodeChild (unsigned int n);


  /**
   * Removes the CSGNode object with the given identifier from this CSGSetOperator 
   * object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   * If none of the CSGNode objects in this CSGSetOperator object have the identifier 
   * @p sid, then @c NULL is returned.
   *
   * @param sid the identifier of the CSGNode object to remove
   *
   * @return the CSGNode object removed.  As mentioned above, the 
   * caller owns the returned object. NULL is returned if no CSGNode
   * object with the identifier exists in this CSGSetOperator object.
   */
  CSGNode* removeCSGNodeChild (const std::string& sid);

  /**
   * @return a (deep) copy of this CSGSetOperator.
   */
  virtual CSGSetOperator* clone () const;


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.

  virtual const std::string& getElementName () const ;
   */

  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()

  int getTypeCode () const;
*/


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
CSGSetOperator_getOperationType (const CSGSetOperator_t *csgo);


LIBSBML_EXTERN
int
CSGSetOperator_isSetOperationType (const CSGSetOperator_t *csgo);


LIBSBML_EXTERN
int
CSGSetOperator_setOperationType (CSGSetOperator_t *csgo, const char *sid);


LIBSBML_EXTERN
int
CSGSetOperator_unsetOperationType (CSGSetOperator_t *csgo);


LIBSBML_EXTERN
int
CSGSetOperator_addCSGNodeChild (CSGSetOperator_t *csgso, const CSGNode_t *n);


LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_createCSGNodeChild (CSGSetOperator_t *csgso);


LIBSBML_EXTERN
ListOf_t *
CSGSetOperator_getListOfCSGNodeChildren (CSGSetOperator_t *g);


LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_getCSGNodeChild (CSGSetOperator_t *g, unsigned int n);


LIBSBML_EXTERN
CSGNode_t *
CSGSetOperator_getCSGNodeChildById (CSGSetOperator_t *g, const char *sid);


LIBSBML_EXTERN
unsigned int
CSGSetOperator_getNumCSGNodeChildren (const CSGSetOperator_t *g);


LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_removeCSGNodeChild (CSGSetOperator_t *g, unsigned int n);


LIBSBML_EXTERN
CSGNode_t*
CSGSetOperator_removeCSGNodeChildById (CSGSetOperator_t *g, const char* sid);


LIBSBML_EXTERN
CSGSetOperator_t *
CSGSetOperator_clone (const CSGSetOperator_t* csgo);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGSetOperator_H__ */
