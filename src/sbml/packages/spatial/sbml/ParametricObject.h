/*
 * @file    ParametricObject.h
 * @brief   Definition of ParametricObject, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: ParametricObject.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/ParametricObject.h $
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


#ifndef ParametricObject_H__
#define ParametricObject_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ParametricObject : public SBase
{
protected:

  std::string mSpatialId;
  std::string mDomain;
  std::string mPolygonType;

  PolygonObject* mPolygonObject;

public:

  /**
   * Creates a new ParametricObject with the given level, version, and package version.
   */
   ParametricObject(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ParametricObject with the given SpatialPkgNamespaces object.
   */
   ParametricObject(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   ParametricObject(const ParametricObject& source);


  /**
   * Assignment operator.
   */
   ParametricObject& operator=(const ParametricObject& source);


  /**
   * Destructor.
   */ 
  virtual ~ParametricObject ();

  /**
   * Returns the string of the "spatialId" attribute of this ParametricObject.
   *
   * @return the string of the "spatialId" attribute of this ParametricObject.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "domain" attribute of this ParametricObject.
   *
   * @return the string of the "domain" attribute of this ParametricObject.
   */
  virtual const std::string& getDomain () const;

  /**
   * Returns the string of the "polygonType" attribute of this ParametricObject.
   *
   * @return the string of the "polygonType" attribute of this ParametricObject.
   */
  virtual const std::string& getPolygonType () const;

 /**
   * Returns the "polyObject" attribute of this ParametricObject.
   *
   * @return the "polyObject" attribute of this ParametricObject.
   */
  const PolygonObject* getPolygonObject() const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * ParametricObject's "spatialId" attribute has been set.
   *
   * @return @c true if this ParametricObject's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * ParametricObject's "domain" attribute has been set.
   *
   * @return @c true if this ParametricObject's "domain" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDomain () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * ParametricObject's "polygonType" attribute has been set.
   *
   * @return @c true if this ParametricObject's "polygonType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetPolygonType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * ParametricObject's "polyObject" attribute has been set.
   *
   * @return @c true if this ParametricObject's "polyObject" attribute has been set, 
   * otherwise @c false is returned.
   */
  bool isSetPolygonObject () const;
  
  /**
   * Sets the SIdRef string of the "spatialId" attribute of this ParametricObject.
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
   * Sets the SIdRef string of the "domain" attribute of this ParametricObject.
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
  virtual int setDomain (const std::string& domain);

  /**
   * Sets the SIdRef string of the "polygonType" attribute of this ParametricObject.
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
  virtual int setPolygonType (const std::string& polygonType);

  /**
   * Sets the SIdRef string of the "polyObject" attribute of this ParametricObject.
   *
   * @param polyObject 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setPolygonObject (const PolygonObject* polyObject);

  /**
   * Unsets the value of the "id" attribute of this ParametricObject.
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
   * Unsets the value of the "domain" attribute of this ParametricObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomain ();

 /**
   * Unsets the value of the "polygonType" attribute of this ParametricObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetPolygonType ();

  /**
   * Creates a new PolygonObject object, installs it as this ParametricObject's 
   * "polyObject" subelement, and returns it.
   *
   * If this ParametricObject had a previous PolygonObject, it will be destroyed.
   *
   * @return the new PolygonObject object
   */
  PolygonObject* createPolygonObject ();

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this ParametricObject.
   */
  virtual ParametricObject* clone () const;


  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;


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
   * all the required elements for this ParametricObject object
   * have been set.
   *
   * @note The required elements for a ParametricObject object are:
   * imageData
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

class LIBSBML_EXTERN ListOfParametricObjects : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfParametricObjects.
   */
  virtual ListOfParametricObjects* clone () const;


  /**
   * Creates a new ListOfParametricObjects with the given level, version, and package version.
   */
   ListOfParametricObjects(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfParametricObjects with the given spatialPkgNamespaces object.
   */
   ListOfParametricObjects(SpatialPkgNamespaces* spatialsns);


  /**
   * Get a ParametricObject from the ListOfParametricObjects.
   *
   * @param n the index number of the ParametricObject to get.
   * 
   * @return the nth ParametricObject in this ListOfParametricObjects.
   *
   * @see size()
   */
  virtual ParametricObject * get(unsigned int n); 


  /**
   * Get a ParametricObject from the ListOfParametricObjects.
   *
   * @param n the index number of the ParametricObject to get.
   * 
   * @return the nth ParametricObject in this ListOfParametricObjects.
   *
   * @see size()
   */
  virtual const ParametricObject * get(unsigned int n) const; 

  /**
   * Get a ParametricObject from the ListOfParametricObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the ParametricObject to get.
   * 
   * @return ParametricObject in this ListOfParametricObjects
   * with the given id or NULL if no such
   * ParametricObject exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual ParametricObject* get (const std::string& sid);


  /**
   * Get a ParametricObject from the ListOfParametricObjects
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the ParametricObject to get.
   * 
   * @return ParametricObject in this ListOfParametricObjects
   * with the given id or NULL if no such
   * ParametricObject exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const ParametricObject* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfParametricObjects items and returns a pointer to
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
  virtual ParametricObject* remove (unsigned int n);


  /**
   * Removes item in this ListOfParametricObjects items with the given identifier.
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
  virtual ParametricObject* remove (const std::string& sid);


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

/** @cond doxygen-libsbml-internal */
/**
 * Used by ListOfParametricObjects::get() to lookup an SBase based by its 
 * componentType
 */
#ifndef SWIG
template<>
struct IdEq<ParametricObject> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <ParametricObject*> (sb)->getSpatialId() == id; }
};
#endif
/** @endcond doxygen-libsbml-internal */

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
ParametricObject_getSpatialId (const ParametricObject_t *po);


LIBSBML_EXTERN
const char *
ParametricObject_getDomain (const ParametricObject_t *sv);


LIBSBML_EXTERN
const PolygonObject_t *
ParametricObject_getPolygonObject (const ParametricObject_t *sf);


LIBSBML_EXTERN
ParametricObject_t *
ParametricObject_clone (const ParametricObject_t* c);


LIBSBML_EXTERN
int
ParametricObject_isSetSpatialId (const ParametricObject_t *c);


LIBSBML_EXTERN
int
ParametricObject_isSetDomain (const ParametricObject_t *sv);


LIBSBML_EXTERN
int
ParametricObject_isSetPolygonObject (const ParametricObject_t *sf);


LIBSBML_EXTERN
int
ParametricObject_setSpatialId (ParametricObject_t *c, const char *sid);


LIBSBML_EXTERN
int
ParametricObject_setDomain (ParametricObject_t *sv, const char *dt);


LIBSBML_EXTERN
int
ParametricObject_setPolygonObject (ParametricObject_t *sf, const PolygonObject_t *polyObject);


LIBSBML_EXTERN
int
ParametricObject_unsetSpatialId (ParametricObject_t *c);


LIBSBML_EXTERN
int
ParametricObject_unsetDomain (ParametricObject_t *sv);


LIBSBML_EXTERN
ParametricObject_t *
ListOfParametricObjects_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
ParametricObject_t *
ListOfParametricObjects_removeById (ListOf_t *lo, const char *sid);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* ParametricObject_H__ */
