/*
 * @file    CompartmentMapping.h
 * @brief   Definition of CompartmentMapping, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CompartmentMapping.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CompartmentMapping.h $
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


#ifndef CompartmentMapping_H__
#define CompartmentMapping_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CompartmentMapping : public SBase
{
protected:

  std::string mSpatialId;
  std::string mCompartment;
  std::string mDomainType;

  double mUnitSize;

  bool mIsSetUnitSize;

public:

  /**
   * Creates a new CompartmentMapping with the given level, version, and package version.
   */
   CompartmentMapping(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CompartmentMapping with the given SpatialPkgNamespaces object.
   */
   CompartmentMapping(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   CompartmentMapping(const CompartmentMapping& source);


  /**
   * Assignment operator.
   */
   CompartmentMapping& operator=(const CompartmentMapping& source);


  /**
   * Destructor.
   */ 
  virtual ~CompartmentMapping ();

 /**
   * Returns the string of the "spatialId" attribute of this CompartmentMapping.
   *
   * @return the string of the "spatialId" attribute of this CompartmentMapping.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "domainType" attribute of this CompartmentMapping.
   *
   * @return the string of the "domainType" attribute of this CompartmentMapping.
   */
  virtual const std::string& getDomainType () const;

 /**
   * Returns the string of the "compartment" attribute of this CompartmentMapping.
   *
   * @return the string of the "compartment" attribute of this CompartmentMapping.
   */
  virtual const std::string& getCompartment () const;

  /**
   * Get the value of the "unitSize" attribute.
   * 
   * @return the unitSize value of this CoordinateComponent, as a float-point number.
   */
  double getUnitSize () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CompartmentMapping's "spatialId" attribute has been set.
   *
   * @return @c true if this CompartmentMapping's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;

    /**
   * Predicate returning @c true or @c false depending on whether this
   * CompartmentMapping's "domainType" attribute has been set.
   *
   * @return @c true if this CompartmentMapping's "domainType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDomainType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CompartmentMapping's "compartment" attribute has been set.
   *
   * @return @c true if this CompartmentMapping's "compartment" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCompartment () const;
  
 /**
   * Predicate returning @c true or @c false depending on whether this
   * CoordinateComponent's "unitSize" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "unitSize" attribute of this CoordinateComponent has
   * been set, @c false otherwise.
   *
   * @note In SBML Level&nbsp;1, CoordinateComponent' "unitSize" is required and
   * therefore <em>should always be set</em>.  (However, in Level&nbsp;1, the
   * attribute has no default value either, so this method will not return
   * @c true until a value has been assigned.)  In SBML Level&nbsp;2,
   * "unitSize" is optional and as such may or may not be set.
   */
  virtual bool isSetUnitSize () const;

  /**
   * Sets the SIdRef string of the "spatialId" attribute of this CompartmentMapping.
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
   * Sets the SIdRef string of the "domainType" attribute of this CompartmentMapping.
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
  virtual int setDomainType (const std::string& domainType);

    /**
   * Sets the SIdRef string of the "compartment" attribute of this CompartmentMapping.
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
  virtual int setCompartment (const std::string& compartment);

   /**
   * Sets the "unitSize" attribute of this CoordinateComponent and marks the field
   * as set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param value the value to which the "unitSize" attribute should
   * be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  virtual int setUnitSize (double value);

  /**
   * Unsets the value of the "spatialId" attribute of this CompartmentMapping.
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
   * Unsets the value of the "domainType" attribute of this CompartmentMapping.
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
   * Unsets the value of the "compartment" attribute of this CompartmentMapping.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCompartment ();

  /**
   * Unsets the "unitSize" attribute value of this CoordinateComponent.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  virtual int unsetUnitSize ();

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this CompartmentMapping.
   */
  virtual CompartmentMapping* clone () const;


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

class LIBSBML_EXTERN ListOfCompartmentMappings : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfCompartmentMappings.
   */
  virtual ListOfCompartmentMappings* clone () const;


  /**
   * Creates a new ListOfCompartmentMappings with the given level, version, and package version.
   */
   ListOfCompartmentMappings(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCompartmentMappings with the given spatialPkgNamespaces object.
   */
   ListOfCompartmentMappings(SpatialPkgNamespaces* spatialsns);


  /**
   * Get a CompartmentMapping from the ListOfCompartmentMappings.
   *
   * @param n the index number of the CompartmentMapping to get.
   * 
   * @return the nth CompartmentMapping in this ListOfCompartmentMappings.
   *
   * @see size()
   */
  virtual CompartmentMapping * get(unsigned int n); 


  /**
   * Get a CompartmentMapping from the ListOfCompartmentMappings.
   *
   * @param n the index number of the CompartmentMapping to get.
   * 
   * @return the nth CompartmentMapping in this ListOfCompartmentMappings.
   *
   * @see size()
   */
  virtual const CompartmentMapping * get(unsigned int n) const; 

  /**
   * Get a CompartmentMapping from the ListOfCompartmentMappings
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the CompartmentMapping to get.
   * 
   * @return CompartmentMapping in this ListOfCompartmentMappings
   * with the given id or NULL if no such
   * CompartmentMapping exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual CompartmentMapping* get (const std::string& sid);


  /**
   * Get a CompartmentMapping from the ListOfCompartmentMappings
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the CompartmentMapping to get.
   * 
   * @return CompartmentMapping in this ListOfCompartmentMappings
   * with the given id or NULL if no such
   * CompartmentMapping exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const CompartmentMapping* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfCompartmentMappings items and returns a pointer to
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
  virtual CompartmentMapping* remove (unsigned int n);


  /**
   * Removes item in this ListOfCompartmentMappings items with the given identifier.
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
  virtual CompartmentMapping* remove (const std::string& sid);


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
 * Used by ListOfCompartmentMappings::get() to lookup an SBase based by its 
 * spatialId
 */
#ifndef SWIG
template<>
struct IdEq<CompartmentMapping> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <CompartmentMapping*> (sb)->getSpatialId() == id; }
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
CompartmentMapping_getSpatialId (const CompartmentMapping_t *c);


LIBSBML_EXTERN
const char *
CompartmentMapping_getDomainType (const CompartmentMapping_t *c);


LIBSBML_EXTERN
const char *
CompartmentMapping_getCompartment (const CompartmentMapping_t *c);


LIBSBML_EXTERN
double
CoordinateComponent_getUnitSize (const CoordinateComponent_t *c);


LIBSBML_EXTERN
CompartmentMapping_t *
CompartmentMapping_clone (const CompartmentMapping_t* c);


LIBSBML_EXTERN
int
CompartmentMapping_isSetSpatialId (const CompartmentMapping_t *c);


LIBSBML_EXTERN
int
CompartmentMapping_isSetDomainType (const CompartmentMapping_t *c);


LIBSBML_EXTERN
int
CompartmentMapping_isSetCompartment (const CompartmentMapping_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_isSetUnitSize(const CoordinateComponent_t *c);


LIBSBML_EXTERN
int
CompartmentMapping_setSpatialId (CompartmentMapping_t *c, const char *sid);


LIBSBML_EXTERN
int
CompartmentMapping_setDomainType (CompartmentMapping_t *c, const char *sid);


LIBSBML_EXTERN
int
CompartmentMapping_setCompartment (CompartmentMapping_t *c, const char *sid);


LIBSBML_EXTERN
int
CoordinateComponent_setUnitSize (CoordinateComponent_t *c, double value);


LIBSBML_EXTERN
int
CompartmentMapping_unsetSpatialId (CompartmentMapping_t *c);


LIBSBML_EXTERN
int
CompartmentMapping_unsetDomainType (CompartmentMapping_t *c);


LIBSBML_EXTERN
int
CompartmentMapping_unsetCompartment (CompartmentMapping_t *c);


LIBSBML_EXTERN
int
CoordinateComponent_unsetUnitSize (CoordinateComponent_t *c);


LIBSBML_EXTERN
CompartmentMapping_t *
ListOfCompartmentMappings_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
CompartmentMapping_t *
ListOfCompartmentMappings_removeById (ListOf_t *lo, const char *sid);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CompartmentMapping_H__ */
