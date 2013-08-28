/*
 * @file    AdjacentDomains.h
 * @brief   Definition of AdjacentDomains, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: AdjacentDomains.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/AdjacentDomains.h $
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


#ifndef AdjacentDomains_H__
#define AdjacentDomains_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN AdjacentDomains : public SBase
{
protected:

  std::string mSpatialId;
  std::string mDomain1;
  std::string mDomain2;

public:

  /**
   * Creates a new AdjacentDomains with the given level, version, and package version.
   */
   AdjacentDomains(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AdjacentDomains with the given SpatialPkgNamespaces object.
   */
   AdjacentDomains(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   AdjacentDomains(const AdjacentDomains& source);


  /**
   * Assignment operator.
   */
   AdjacentDomains& operator=(const AdjacentDomains& source);


  /**
   * Destructor.
   */ 
  virtual ~AdjacentDomains ();

 /**
   * Returns the string of the "spatialId" attribute of this AdjacentDomains.
   *
   * @return the string of the "spatialId" attribute of this AdjacentDomains.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "domain1" attribute of this AdjacentDomains.
   *
   * @return the string of the "domain1" attribute of this AdjacentDomains.
   */
  virtual const std::string& getDomain1 () const;

 /**
   * Returns the string of the "domain2" attribute of this AdjacentDomains.
   *
   * @return the string of the "domain2" attribute of this AdjacentDomains.
   */
  virtual const std::string& getDomain2 () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * AdjacentDomains's "spatialId" attribute has been set.
   *
   * @return @c true if this AdjacentDomains's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;

    /**
   * Predicate returning @c true or @c false depending on whether this
   * AdjacentDomains's "domain1" attribute has been set.
   *
   * @return @c true if this AdjacentDomains's "domain1" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDomain1 () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * AdjacentDomains's "domain2" attribute has been set.
   *
   * @return @c true if this AdjacentDomains's "domain2" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDomain2 () const;
  
  /**
   * Sets the SIdRef string of the "spatialId" attribute of this AdjacentDomains.
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
   * Sets the SIdRef string of the "domain1" attribute of this AdjacentDomains.
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
  virtual int setDomain1 (const std::string& domain1);

    /**
   * Sets the SIdRef string of the "domain2" attribute of this AdjacentDomains.
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
  virtual int setDomain2 (const std::string& domain2);

  /**
   * Unsets the value of the "spatialId" attribute of this AdjacentDomains.
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
   * Unsets the value of the "domain1" attribute of this AdjacentDomains.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomain1 ();

  /**
   * Unsets the value of the "domain2" attribute of this AdjacentDomains.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomain2 ();


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this AdjacentDomains.
   */
  virtual AdjacentDomains* clone () const;


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

class LIBSBML_EXTERN ListOfAdjacentDomains : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfAdjacentDomains.
   */
  virtual ListOfAdjacentDomains* clone () const;


  /**
   * Creates a new ListOfAdjacentDomains with the given level, version, and package version.
   */
   ListOfAdjacentDomains(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfAdjacentDomains with the given spatialPkgNamespaces object.
   */
   ListOfAdjacentDomains(SpatialPkgNamespaces* spatialsns);


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains.
   *
   * @param n the index number of the AdjacentDomains to get.
   * 
   * @return the nth AdjacentDomains in this ListOfAdjacentDomains.
   *
   * @see size()
   */
  virtual AdjacentDomains * get(unsigned int n); 


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains.
   *
   * @param n the index number of the AdjacentDomains to get.
   * 
   * @return the nth AdjacentDomains in this ListOfAdjacentDomains.
   *
   * @see size()
   */
  virtual const AdjacentDomains * get(unsigned int n) const; 

  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the AdjacentDomains to get.
   * 
   * @return AdjacentDomains in this ListOfAdjacentDomains
   * with the given id or NULL if no such
   * AdjacentDomains exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual AdjacentDomains* get (const std::string& sid);


  /**
   * Get a AdjacentDomains from the ListOfAdjacentDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the AdjacentDomains to get.
   * 
   * @return AdjacentDomains in this ListOfAdjacentDomains
   * with the given id or NULL if no such
   * AdjacentDomains exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const AdjacentDomains* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfAdjacentDomains items and returns a pointer to
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
  virtual AdjacentDomains* remove (unsigned int n);


  /**
   * Removes item in this ListOfAdjacentDomains items with the given identifier.
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
  virtual AdjacentDomains* remove (const std::string& sid);


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
 * Used by ListOfAdjacentDomains::get() to lookup an SBase based by its 
 * spatialId
 */
#ifndef SWIG
template<>
struct IdEq<AdjacentDomains> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <AdjacentDomains*> (sb)->getSpatialId() == id; }
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
AdjacentDomains_getSpatialId (const AdjacentDomains_t *c);


LIBSBML_EXTERN
const char *
AdjacentDomains_getDomain1 (const AdjacentDomains_t *c);


LIBSBML_EXTERN
const char *
AdjacentDomains_getDomain2 (const AdjacentDomains_t *c);


LIBSBML_EXTERN
AdjacentDomains_t *
AdjacentDomains_clone (const AdjacentDomains_t* c);


LIBSBML_EXTERN
int
AdjacentDomains_isSetSpatialId (const AdjacentDomains_t *c);


LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain1 (const AdjacentDomains_t *c);


LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain2 (const AdjacentDomains_t *c);


LIBSBML_EXTERN
int
AdjacentDomains_setSpatialId (AdjacentDomains_t *c, const char *sid);


LIBSBML_EXTERN
int
AdjacentDomains_setDomain1 (AdjacentDomains_t *c, const char *sid);


LIBSBML_EXTERN
int
AdjacentDomains_setDomain2 (AdjacentDomains_t *c, const char *sid);


LIBSBML_EXTERN
int
AdjacentDomains_unsetSpatialId (AdjacentDomains_t *c);


LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain1 (AdjacentDomains_t *c);


LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain2 (AdjacentDomains_t *c);


LIBSBML_EXTERN
AdjacentDomains_t *
ListOfAdjacentDomains_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
AdjacentDomains_t *
ListOfAdjacentDomains_removeById (ListOf_t *lo, const char *sid);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* AdjacentDomains_H__ */
