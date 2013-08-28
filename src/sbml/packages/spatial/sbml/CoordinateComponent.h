/*
 * @file    CoordinateComponent.h
 * @brief   Definition of CoordinateComponent, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CoordinateComponent.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CoordinateComponent.h $
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


#ifndef CoordinateComponent_H__
#define CoordinateComponent_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/packages/spatial/sbml/Boundary.h>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CoordinateComponent : public SBase
{
protected:

  std::string mSpatialId;
  std::string mComponentType;
  std::string  mSbmlUnit;

  unsigned int mIndex;

  BoundaryMin* mBoundaryMin;
  BoundaryMax* mBoundaryMax;

  bool  mIsSetIndex;

public:

  /**
   * Creates a new CoordinateComponent with the given level, version, and package version.
   */
   CoordinateComponent(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CoordinateComponent with the given SpatialPkgNamespaces object.
   */
   CoordinateComponent(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   CoordinateComponent(const CoordinateComponent& source);


  /**
   * Assignment operator.
   */
   CoordinateComponent& operator=(const CoordinateComponent& source);


  /**
   * Destructor.
   */ 
  virtual ~CoordinateComponent ();

  /**
   * Returns the string of the "spatialId" attribute of this CoordinateComponent.
   *
   * @return the string of the "spatialId" attribute of this CoordinateComponent.
   */
  virtual const std::string& getSpatialId () const;

 /**
   * Returns the string of the "componentType" attribute of this CoordinateComponent.
   *
   * @return the string of the "componentType" attribute of this CoordinateComponent.
   */
  virtual const std::string& getComponentType () const;

  /**
   * Returns the string of the "sbmUnit" attribute of this CoordinateComponent.
   *
   * @return the string of the "sbmUnit" attribute of this CoordinateComponent.
   */
  virtual const std::string& getSbmlUnit () const;

 /**
   * Returns the string of the "index" attribute of this CoordinateComponent.
   *
   * @return the string of the "index" attribute of this CoordinateComponent.
   */
  unsigned int getIndex () const;

 /**
   * Returns the "BoundaryMin" attribute of this CoordinateComponent.
   *
   * @return the "BoundaryMin" attribute of this CoordinateComponent.
   */
  BoundaryMin* getBoundaryMin() const;

 /**
   * Returns the "BoundaryMax" attribute of this CoordinateComponent.
   *
   * @return the "BoundaryMax" attribute of this CoordinateComponent.
   */
  BoundaryMax* getBoundaryMax() const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "spatialId" attribute has been set.
   *
   * @return @c true if this CoordinateComponent's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "componentType" attribute has been set.
   *
   * @return @c true if this CoordinateComponent's "componentType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetComponentType () const;

    /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "sbmUnit" attribute has been set.
   *
   * @return @c true if this CoordinateComponent's "sbmUnit" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSbmlUnit () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "index" attribute has been set.
   *
   * @return @c true if this CoordinateComponent's "index" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetIndex () const;
  
   /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "boundaryMin" attribute has been set.
   *
   * @return @c true if this CoordinateComponent's "boundaryMin" attribute has been set, 
   * otherwise @c false is returned.
   */
  bool isSetBoundaryMin () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "boundaryMax" attribute has been set.
   *
   * @return @c true if this CoordinateComponent's "boundaryMax" attribute has been set, 
   * otherwise @c false is returned.
   */
  bool isSetBoundaryMax () const;
  
  /**
   * Sets the SIdRef string of the "spatialId" attribute of this CoordinateComponent.
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
   * Sets the SIdRef string of the "componentType" attribute of this CoordinateComponent.
   *
   * @param componentType a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setComponentType (const std::string& componentType);

  /**
   * Sets the SIdRef string of the "sbmUnit" attribute of this CoordinateComponent.
   *
   * @param componentType a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSbmlUnit (const std::string& sbmUnit);

    /**
   * Sets the SIdRef string of the "index" attribute of this CoordinateComponent.
   *
   * @param componentType a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setIndex (unsigned int index);

  /**
   * Sets the SIdRef string of the "boundaryMin" attribute of this CoordinateComponent.
   *
   * @param boundaryMin 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setBoundaryMin (const BoundaryMin* boundaryMin);

  /**
   * Sets the SIdRef string of the "boundaryMax" attribute of this CoordinateComponent.
   *
   * @param boundaryMax 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setBoundaryMax (const BoundaryMax* boundaryMax);

  /**
   * Unsets the value of the "id" attribute of this CoordinateComponent.
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
   * Unsets the value of the "componentType" attribute of this CoordinateComponent.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetComponentType ();

  /**
   * Unsets the value of the "sbmUnit" attribute of this CoordinateComponent.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSbmlUnit ();

  /**
   * Unsets the value of the "index" attribute of this CoordinateComponent.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetIndex ();

  /**
   * Creates a new Boundary (min) object, installs it as this CoordinateComponent's 
   * "boundaryMin" subelement, and returns it.
   *
   * If this CoordinateComponent had a previous BoundaryMin, it will be destroyed.
   *
   * @return the new BoundaryMin object
   */
  BoundaryMin* createBoundaryMin ();

  /**
   * Creates a new Boundary (max) object, installs it as this CoordinateComponent's 
   * "boundaryMax" subelement, and returns it.
   *
   * If this CoordinateComponent had a previous BoundaryMax, it will be destroyed.
   *
   * @return the new BoundaryMax object
   */
  BoundaryMax* createBoundaryMax ();


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this CoordinateComponent.
   */
  virtual CoordinateComponent* clone () const;


  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;


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

class LIBSBML_EXTERN ListOfCoordinateComponents : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfCoordinateComponents.
   */
  virtual ListOfCoordinateComponents* clone () const;


  /**
   * Creates a new ListOfCoordinateComponents with the given level, version, and package version.
   */
   ListOfCoordinateComponents(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCoordinateComponents with the given spatialPkgNamespaces object.
   */
   ListOfCoordinateComponents(SpatialPkgNamespaces* spatialsns);


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents.
   *
   * @param n the index number of the CoordinateComponent to get.
   * 
   * @return the nth CoordinateComponent in this ListOfCoordinateComponents.
   *
   * @see size()
   */
  virtual CoordinateComponent * get(unsigned int n); 


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents.
   *
   * @param n the index number of the CoordinateComponent to get.
   * 
   * @return the nth CoordinateComponent in this ListOfCoordinateComponents.
   *
   * @see size()
   */
  virtual const CoordinateComponent * get(unsigned int n) const; 

  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the CoordinateComponent to get.
   * 
   * @return CoordinateComponent in this ListOfCoordinateComponents
   * with the given id or NULL if no such
   * CoordinateComponent exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual CoordinateComponent* get (const std::string& sid);


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the CoordinateComponent to get.
   * 
   * @return CoordinateComponent in this ListOfCoordinateComponents
   * with the given id or NULL if no such
   * CoordinateComponent exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const CoordinateComponent* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfCoordinateComponents items and returns a pointer to
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
  virtual CoordinateComponent* remove (unsigned int n);


  /**
   * Removes item in this ListOfCoordinateComponents items with the given identifier.
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
  virtual CoordinateComponent* remove (const std::string& sid);


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
 * Used by ListOfCoordinateComponents::get() to lookup an SBase based by its 
 * componentType
 */
#ifndef SWIG
template<>
struct IdEq<CoordinateComponent> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <CoordinateComponent*> (sb)->getSpatialId() == id; }
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
CoordinateComponent_getSpatialId (const CoordinateComponent_t *c);


LIBSBML_EXTERN
const char *
CoordinateComponent_getComponentType (const CoordinateComponent_t *c);


LIBSBML_EXTERN
const char *
CoordinateComponent_getSbmlUnit (const CoordinateComponent_t *c);


LIBSBML_EXTERN
unsigned int
CoordinateComponent_getIndex (const CoordinateComponent_t *c);


LIBSBML_EXTERN
const Boundary_t *
CoordinateComponent_getBoundaryMin (const CoordinateComponent_t *c);


LIBSBML_EXTERN
const Boundary_t *
CoordinateComponent_getBoundaryMax (const CoordinateComponent_t *c);


LIBSBML_EXTERN
CoordinateComponent_t *
CoordinateComponent_clone (const CoordinateComponent_t* c);


LIBSBML_EXTERN
int
CoordinateComponent_isSetSpatialId (const CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_isSetComponentType (const CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_isSetSbmlUnit (const CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_isSetIndex (const CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_isSetBoundaryMin(const CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_isSetBoundaryMax(const CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_setSpatialId (CoordinateComponent_t *c, const char *sid);


LIBSBML_EXTERN
int
CoordinateComponent_setComponentType (CoordinateComponent_t *c, const char *sid);


LIBSBML_EXTERN
int
CoordinateComponent_setSbmlUnit (CoordinateComponent_t *c, const char *sid);


LIBSBML_EXTERN
int
CoordinateComponent_setIndex (CoordinateComponent_t *c, unsigned int value);


LIBSBML_EXTERN
int
CoordinateComponent_setBoundaryMin (CoordinateComponent_t *c, const Boundary_t *bmin);


LIBSBML_EXTERN
int
CoordinateComponent_setBoundaryMax (CoordinateComponent_t *c, const Boundary_t *bmax);


LIBSBML_EXTERN
int
CoordinateComponent_unsetSpatialId (CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_unsetComponentType (CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_unsetSbmlUnit (CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_unsetIndex (CoordinateComponent_t *c);


LIBSBML_EXTERN
CoordinateComponent_t *
ListOfCoordinateComponents_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
CoordinateComponent_t *
ListOfCoordinateComponents_removeById (ListOf_t *lo, const char *sid);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CoordinateComponent_H__ */
