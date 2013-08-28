/*
 * @file    CSGNode.h
 * @brief   Definition of CSGNode, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGNode.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGNode.h $
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


#ifndef CSGNode_H__
#define CSGNode_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


BEGIN_C_DECLS

typedef enum
{
    CSGNODE_TYPE_CSGPRIMITIVE
  , CSGNODE_TYPE_CSGPSEUDOPRIMITIVE
  , CSGNODE_TYPE_CSGSETOPERATOR
  , CSGNODE_TYPE_CSGTRANSFORMATION
  , CSGNODE_TYPE_INVALID
} CSGNodeType_t;

END_C_DECLS


#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class CSGPrimitive;
class CSGPseudoPrimitive;
class CSGSetOperator;
class CSGTranslation;
class CSGRotation;
class CSGScale;
class CSGHomogeneousTransformation;

class LIBSBML_EXTERN CSGNode : public SBase
{
protected:

  std::string mSpatialId;
  SBMLSpatialTypeCode_t mType;

public:

  //
  //  Only subclasses may create CSGNodes.
  // 
  CSGNode(SBMLSpatialTypeCode_t	type,
	      unsigned int level   = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion());

  CSGNode(SBMLSpatialTypeCode_t	type, SpatialPkgNamespaces* spatialns);


  /**
   * Assignment operator.
   */
   CSGNode& operator=(const CSGNode& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGNode ();

  /**
   * Returns the string of the "spatialId" attribute of this CSGObject.
   *
   * @return the string of the "spatialId" attribute of this CSGObject.
   */
  virtual const std::string& getSpatialId () const;


 /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGObject's "spatialId" attribute has been set.
   *
   * @return @c true if this CSGObject's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;
  
 /**
   * Sets the SIdRef string of the "spatialId" attribute of this CSGObject.
   *
   * @param spatialId a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSpatialId (const std::string& spatialId);

  /**
   * Unsets the value of the "id" attribute of this CSGObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSpatialId ();

 /**
   * (SBML Level&nbsp;1) Get the type of CSGNode.
   * 
   * @return the CSGNode type (a value drawn from the enumeration <a
   * class="el" href="#CSGNode">CSGNode</a>) of this CSGNode.
   * The value will be either @c GEOMETRICPRIMITIVE, @c CSGNODE_TYPE_CSGOPERATOR,
   * @c CSGNODE_TYPE_AFFINETRANSFORMATIONs
   */
  CSGNodeType_t getType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGNode's "csgPrimitive" attribute has been set.
   *
   * @return @c true if this CSGNode's "csgPrimitive" attribute has been set, 
   * otherwise @c false is returned.
   */
  bool isCSGPrimitive () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGNode's "csgPseudoPrimitive" attribute has been set.
   *
   * @return @c true if this CSGNode's "csgPseudoPrimitive" attribute has been set, 
   * otherwise @c false is returned.
   */
  bool isCSGPseudoPrimitive () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGNode's "csgSetOperator" attribute has been set.
   *
   * @return @c true if this CSGNode's "csgSetOperator" attribute has been set, 
   * otherwise @c false is returned.
   */
  bool isCSGSetOperator () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGNode's "csgTransformation" attribute has been set.
   *
   * @return @c true if this CSGNode's "csgTransformation" attribute has been set, 
   * otherwise @c false is returned.
   */
  bool isCSGTransformation () const;
  
  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this CSGNode.
   */
  virtual CSGNode* clone () const;


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
  virtual void writeElements (XMLOutputStream& stream) const {
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
  virtual void connectToChild (){ 
  };

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
  createObject (XMLInputStream& stream) {
	  return NULL;
  };


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
  virtual bool readOtherXML (XMLInputStream& stream) {
	  return false;
  };

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

    /**
   * Creates a new CSGPrimitive object inside this CSGNodeperator and returns it.
   *
   * @return the CSGPrimitive object created
   */
  CSGPrimitive* create_CSGPrimitive ();

  /**
   * Creates a new CSGPseudoPrimitive object inside this CSGNodeperator and returns it.
   *
   * @return the CSGPseudoPrimitive object created
   */
  CSGPseudoPrimitive* create_CSGPseudoPrimitive ();

  /**
   * Creates a new CSGNodeperator object inside this CSGNodeperator and returns it.
   *
   * @return the CSGNodeperator object created
   */
  CSGSetOperator* create_CSGSetOperator ();

  /**
   * Creates a new CSGTranslation object inside this CSGNodeperator and returns it.
   *
   * @return the CSGTranslation object created
   */
  CSGTranslation* create_CSGTranslation ();

  /**
   * Creates a new CSGRotation object inside this CSGNodeperator and returns it.
   *
   * @return the CSGRotation object created
   */
  CSGRotation* create_CSGRotation ();

  /**
   * Creates a new CSGScale object inside this CSGNodeperator and returns it.
   *
   * @return the CSGScale object created
   */
  CSGScale* create_CSGScale ();

  /**
   * Creates a new CSGHomogeneousTransformation object inside this CSGNodeperator and returns it.
   *
   * @return the CSGHomogeneousTransformation object created
   */
  CSGHomogeneousTransformation* create_CSGHomogeneousTransformation ();


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

class LIBSBML_EXTERN ListOfCSGNodes : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfCSGNodes.
   */
  virtual ListOfCSGNodes* clone () const;


  /**
   * Creates a new ListOfCSGNodes with the given level, version, and package version.
   */
   ListOfCSGNodes(unsigned int level  = SpatialExtension::getDefaultLevel(),
          unsigned int version			= SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion		= SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCSGNodes with the given spatialPkgNamespaces object.
   */
   ListOfCSGNodes(SpatialPkgNamespaces* spatialsns);


  /**
   * Get a CSGNode from the ListOfCSGNodes.
   *
   * @param n the index number of the CSGNode to get.
   * 
   * @return the nth CSGNode in this ListOfCSGNodes.
   *
   * @see size()
   */
  virtual CSGNode * get(unsigned int n); 


  /**
   * Get a CSGNode from the ListOfCSGNodes.
   *
   * @param n the index number of the CSGNode to get.
   * 
   * @return the nth CSGNode in this ListOfCSGNodes.
   *
   * @see size()
   */
  virtual const CSGNode * get(unsigned int n) const; 

  /**
   * Get a CSGNode from the ListOfCSGNodes
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the CSGNode to get.
   * 
   * @return CSGNode in this ListOfCSGNodes
   * with the given id or NULL if no such
   * CSGNode exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual CSGNode* get (const std::string& sid);


  /**
   * Get a CSGNode from the ListOfCSGNodes
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the CSGNode to get.
   * 
   * @return CSGNode in this ListOfCSGNodes
   * with the given id or NULL if no such
   * CSGNode exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const CSGNode* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfCSGNodes items and returns a pointer to
   * it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove
   * @return the item removed.  As mentioned above, the caller owns the
   * returned item.
   *
   * @see size()
   */
  virtual CSGNode* remove (unsigned int n);


  /**
   * Removes item in this ListOfCSGNodes items with the given identifier.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then @c
   * NULL is returned.
   *
   * @param sid the identifier of the item to remove
   *
   * @return the item removed.  As mentioned above, the caller owns the
   * returned item.
   */
  virtual CSGNode* remove (const std::string& sid);


  /**
   * @return the typecode (int) of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual int getItemTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const;


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  virtual bool isValidTypeForList(SBase * item);
};

/** @cond doxygenLibsbmlInternal */
/**
 * Used by ListOfCSGNodes::get() to lookup an SBase based by its 
 * componentType
 */
#ifndef SWIG
template<>
struct IdEq<CSGNode> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <CSGNode*> (sb)->getSpatialId() == id; }
};
#endif
/** @endcond doxygenLibsbmlInternal */

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
CSGNode_getSpatialId (const CSGNode_t *csgo);


LIBSBML_EXTERN
int
CSGNode_isSetSpatialId (const CSGNode_t *c);


LIBSBML_EXTERN
int
CSGNode_setSpatialId (CSGNode_t *c, const char *sid);


LIBSBML_EXTERN
int
CSGNode_unsetSpatialId (CSGNode_t *csgo);


LIBSBML_EXTERN
CSGNodeType_t
CSGNode_getType (const CSGNode_t *csgo);


LIBSBML_EXTERN
int
CSGNode_isCSGPrimitive (const CSGNode_t *csgo);


LIBSBML_EXTERN
int
CSGNode_isCSGSetOperator (const CSGNode_t *csgo);


LIBSBML_EXTERN
int
CSGNode_isCSGTransformation (const CSGNode_t *csgo);


LIBSBML_EXTERN
CSGNode_t *
CSGNode_clone (const CSGNode_t* c);


LIBSBML_EXTERN
CSGNode_t *
ListOfCSGNodes_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
CSGNode_t *
ListOfCSGNodes_removeById (ListOf_t *lo, const char *sid);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGNode_H__ */
