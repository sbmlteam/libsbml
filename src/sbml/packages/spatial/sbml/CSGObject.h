/*
 * @file    CSGObject.h
 * @brief   Definition of CSGObject, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGObject.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGObject.h $
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


#ifndef CSGObject_H__
#define CSGObject_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class CSGPrimitive;
class CSGPseudoPrimitive;
class CSGSetOperator;
class CSGTranslation;
class CSGRotation;
class CSGScale;
class CSGHomogeneousTransformation;


class LIBSBML_EXTERN CSGObject : public SBase
{
protected:

  std::string mSpatialId;
  std::string mDomainType;
  unsigned int mOrdinal;
  CSGNode* mCSGNodeRoot;

  bool  mIsSetOrdinal;

public:

  /**
   * Creates a new CSGObject with the given level, version, and package version.
   */

  CSGObject(unsigned int level   = SpatialExtension::getDefaultLevel(),
            unsigned int version    = SpatialExtension::getDefaultVersion(),
            unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());

  /**
   * Creates a new CSGObject with the given SpatialPkgNamespaces object.
   */

  CSGObject(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
  CSGObject(const CSGObject& source);

  /**
   * Assignment operator.
   */
   CSGObject& operator=(const CSGObject& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGObject ();

  /**
   * Returns the string of the "spatialId" attribute of this CSGObject.
   *
   * @return the string of the "spatialId" attribute of this CSGObject.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "domain" attribute of this CSGObject.
   *
   * @return the string of the "domain" attribute of this CSGObject.
   */
  virtual const std::string& getDomainType () const;

  /**
   * Returns the string of the "ordinal" attribute of this CSGObject.
   *
   * @return the string of the "ordinal" attribute of this CSGObject.
   */
  virtual unsigned int getOrdinal () const;

 /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGObject's "spatialId" attribute has been set.
   *
   * @return @c true if this CSGObject's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGObject's "domain" attribute has been set.
   *
   * @return @c true if this CSGObject's "domain" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDomainType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGObject's "ordinal" attribute has been set.
   *
   * @return @c true if this CSGObject's "ordinal" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetOrdinal () const;

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
   * Sets the SIdRef string of the "domain" attribute of this CSGObject.
   *
   * @param domain a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDomainType (const std::string& domain);

  /**
   * Sets the int of the "ordinal" attribute of this CSGObject.
   *
   * @param ordinal an int to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setOrdinal (unsigned int ordinal);

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
   * Unsets the value of the "domain" attribute of this CSGObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomainType ();

  /**
   * Unsets the value of the "ordinal" attribute of this CSGObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetOrdinal ();

  /**
   * Returns the "mCSGNodeRoot" object of this CSGNodeGeometry.
   *
   * @return the "mCSGNodeRoot" object of this CSGNodeGeometry.
   */
  const CSGNode* getCSGNodeRoot () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGNodeGeometry's "mCSGNodeRoot" object has been set.
   *
   * @return @c true if this CSGNodeGeometry's "mCSGNodeRoot" object has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCSGNodeRoot () const;
  
  /**
   * Sets the "mCSGNode" subelement of this CSGNodeGeometry to a copy of the 
   * given CSGNode object.
   *
   * @param sampledField an object to use.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setCSGNodeRoot (const CSGNode* csgn);

 /**
   * Unsets the "mCSGNode" subelement of this CSGNodeGeometry.
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
  int unsetCSGNodeRoot ();

  /**
   * Creates a new CSGPrimitive object inside this CSGObject and returns it.
   *
   * @return the CSGPrimitive object created
   */
  CSGPrimitive* createCSGPrimitive ();

  /**
   * Creates a new CSGPseudoPrimitive object inside this CSGObject and returns it.
   *
   * @return the CSGPseudoPrimitive object created
   */
  CSGPseudoPrimitive* createCSGPseudoPrimitive ();

  /**
   * Creates a new CSGSetOperator object inside this CSGObject and returns it.
   *
   * @return the CSGSetOperator object created
   */
  CSGSetOperator* createCSGSetOperator ();

  /**
   * Creates a new CSGTranslation object inside this CSGObject and returns it.
   *
   * @return the CSGTranslation object created
   */
  CSGTranslation* createCSGTranslation ();

  /**
   * Creates a new CSGRotation object inside this CSGObject and returns it.
   *
   * @return the CSGRotation object created
   */
  CSGRotation* createCSGRotation ();

  /**
   * Creates a new CSGScale object inside this CSGObject and returns it.
   *
   * @return the CSGScale object created
   */
  CSGScale* createCSGScale ();

  /**
   * Creates a new CSGHomogeneousTransformation object inside this CSGObject and returns it.
   *
   * @return the CSGHomogeneousTransformation object created
   */
  CSGHomogeneousTransformation* createCSGHomogeneousTransformation ();

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this CSGObject.
   */
  virtual CSGObject* clone () const;


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
  virtual void connectToChild ();

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

class LIBSBML_EXTERN ListOfCSGObjects : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfCSGObjects.
   */
  virtual ListOfCSGObjects* clone () const;


  /**
   * Creates a new ListOfCSGObjects with the given level, version, and package version.
   */
   ListOfCSGObjects(unsigned int level  = SpatialExtension::getDefaultLevel(),
          unsigned int version			= SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion		= SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCSGObjects with the given spatialPkgNamespaces object.
   */
   ListOfCSGObjects(SpatialPkgNamespaces* spatialsns);


  /**
   * Get a CSGObject from the ListOfCSGObjects.
   *
   * @param n the index number of the CSGObject to get.
   * 
   * @return the nth CSGObject in this ListOfCSGObjects.
   *
   * @see size()
   */
  virtual CSGObject * get(unsigned int n); 


  /**
   * Get a CSGObject from the ListOfCSGObjects.
   *
   * @param n the index number of the CSGObject to get.
   * 
   * @return the nth CSGObject in this ListOfCSGObjects.
   *
   * @see size()
   */
  virtual const CSGObject * get(unsigned int n) const; 

  /**
   * Get a CSGObject from the ListOfCSGObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the CSGObject to get.
   * 
   * @return CSGObject in this ListOfCSGObjects
   * with the given id or NULL if no such
   * CSGObject exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual CSGObject* get (const std::string& sid);


  /**
   * Get a CSGObject from the ListOfCSGObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the CSGObject to get.
   * 
   * @return CSGObject in this ListOfCSGObjects
   * with the given id or NULL if no such
   * CSGObject exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const CSGObject* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfCSGObjects items and returns a pointer to
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
  virtual CSGObject* remove (unsigned int n);


  /**
   * Removes item in this ListOfCSGObjects items with the given identifier.
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
  virtual CSGObject* remove (const std::string& sid);


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

};

/** @cond doxygenLibsbmlInternal */
/**
 * Used by ListOfCSGObjects::get() to lookup an SBase based by its 
 * componentType
 */
#ifndef SWIG
template<>
struct IdEq<CSGObject> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <CSGObject*> (sb)->getSpatialId() == id; }
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
CSGObject_getSpatialId (const CSGObject_t *csgo);


LIBSBML_EXTERN
const char *
CSGObject_getDomainType (const CSGObject_t *csgo);


LIBSBML_EXTERN
int
CSGObject_getOrdinal (const CSGObject_t *av);


LIBSBML_EXTERN
int
CSGObject_isSetSpatialId (const CSGObject_t *c);


LIBSBML_EXTERN
int
CSGObject_isSetDomainType (const CSGObject_t *sv);


LIBSBML_EXTERN
int
CSGObject_isSetOrdinal (const CSGObject_t *av);


LIBSBML_EXTERN
int
CSGObject_setSpatialId (CSGObject_t *c, const char *sid);


LIBSBML_EXTERN
int
CSGObject_setDomainType (CSGObject_t *sv, const char *dt);


LIBSBML_EXTERN
int
CSGObject_setOrdinal (CSGObject_t *av, int ord);


LIBSBML_EXTERN
int
CSGObject_unsetSpatialId (CSGObject_t *csgo);


LIBSBML_EXTERN
int
CSGObject_unsetDomainType (CSGObject_t *csgo);


LIBSBML_EXTERN
int
CSGObject_unsetOrdinal (CSGObject_t *av);


LIBSBML_EXTERN
CSGNode_t *
CSGObject_getCSGNodeRoot (const CSGObject_t *csgpp);


LIBSBML_EXTERN
int
CSGObject_isSetCSGNodeRoot (const CSGObject_t *csgpp);


LIBSBML_EXTERN
int
CSGObject_setCSGNodeRoot (CSGObject_t *csgpp, CSGNode_t *csgn);


LIBSBML_EXTERN
int
CSGObject_unsetCSGNodeRoot (CSGObject_t *csgpp);


LIBSBML_EXTERN
CSGObject_t *
CSGObject_clone (const CSGObject_t* c);


LIBSBML_EXTERN
CSGObject_t *
ListOfCSGObjects_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
CSGObject_t *
ListOfCSGObjects_removeById (ListOf_t *lo, const char *sid);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGObject_H__ */
