/*
 * @file    SpatialPoint.h
 * @brief   Definition of SpatialPoint, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SpatialPoint.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SpatialPoint.h $
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


#ifndef SpatialPoint_H__
#define SpatialPoint_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpatialPoint : public SBase
{
protected:

  std::string mSpatialId;
  std::string mDomain;
  
  double mCoord1;
  double mCoord2;
  double mCoord3;

  bool  mIsSetCoord1;
  bool  mIsSetCoord2;
  bool  mIsSetCoord3;

public:

  /**
   * Creates a new SpatialPoint with the given level, version, and package version.
   */
   SpatialPoint(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpatialPoint with the given SpatialPkgNamespaces object.
   */
   SpatialPoint(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   SpatialPoint(const SpatialPoint& source);


  /**
   * Assignment operator.
   */
   SpatialPoint& operator=(const SpatialPoint& source);


  /**
   * Destructor.
   */ 
  virtual ~SpatialPoint ();


  /**
   * Returns the string of the "spatialId" attribute of this SpatialPoint.
   *
   * @return the string of the "spatialId" attribute of this SpatialPoint.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "domain" attribute of this SpatialPoint.
   *
   * @return the string of the "domain" attribute of this SpatialPoint.
   */
  virtual const std::string& getDomain () const;

  /**
   * Returns the double of the "coord1" attribute of this SpatialPoint.
   *
   * @return the double of the "coord1" attribute of this SpatialPoint.
   */
  virtual double getCoord1 () const;

  /**
   * Returns the double of the "coord2" attribute of this SpatialPoint.
   *
   * @return the double of the "coord2" attribute of this SpatialPoint.
   */
  virtual double getCoord2 () const;

  /**
   * Returns the double of the "coord3" attribute of this SpatialPoint.
   *
   * @return the double of the "coord3" attribute of this SpatialPoint.
   */
  virtual double getCoord3 () const;

 /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "spatialId" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "domain" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "domain" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDomain () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "coord1" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "coord1" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord1 () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "coord2" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "coord2" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord2 () const;

    /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoint's "coord2" attribute has been set.
   *
   * @return @c true if this SpatialPoint's "coord2" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord3 () const;

  /**
   * Sets the SIdRef string of the "spatialId" attribute of this SpatialPoint.
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
   * Sets the SIdRef string of the "domain" attribute of this SpatialPoint.
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
   * Sets the double value of the "coord1" attribute of this SpatialPoint.
   *
   * @param coord1 a double to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoord1 (double coord1);

  /**
   * Sets the double value of the "coord2" attribute of this SpatialPoint.
   *
   * @param coord2 a double to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoord2 (double coord2);

  /**
   * Sets the double value of the "coord3" attribute of this SpatialPoint.
   *
   * @param coord3 a double to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoord3 (double coord3);

  /**
   * Unsets the value of the "spatialId" attribute of this SpatialPoint.
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
   * Unsets the value of the "domain" attribute of this SpatialPoint.
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
   * Unsets the value of the "coord1" attribute of this SpatialPoint.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoord1 ();

  /**
   * Unsets the value of the "coord2" attribute of this SpatialPoint.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoord2 ();

  /**
   * Unsets the value of the "coord3" attribute of this SpatialPoint.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoord3 ();


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this SpatialPoint.
   */
  virtual SpatialPoint* clone () const;


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
//  friend class MathMLConsistencyValidator;
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
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;
*/
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;

  /** @endcond doxygen-libsbml-internal */

};

class LIBSBML_EXTERN ListOfSpatialPoints : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfSpatialPoints.
   */
  virtual ListOfSpatialPoints* clone () const;


  /**
   * Creates a new ListOfSpatialPoints with the given level, version, and package version.
   */
   ListOfSpatialPoints(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSpatialPoints with the given SpatialPkgNamespaces object.
   */
   ListOfSpatialPoints(SpatialPkgNamespaces* spatialns);


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints.
   *
   * @param n the index number of the SpatialPoint to get.
   * 
   * @return the nth SpatialPoint in this ListOfSpatialPoints.
   *
   * @see size()
   */
  virtual SpatialPoint * get(unsigned int n); 


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints.
   *
   * @param n the index number of the SpatialPoint to get.
   * 
   * @return the nth SpatialPoint in this ListOfSpatialPoints.
   *
   * @see size()
   */
  virtual const SpatialPoint * get(unsigned int n) const; 

  /**
   * Get a SpatialPoint from the ListOfSpatialPoints
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the SpatialPoint to get.
   * 
   * @return SpatialPoint in this ListOfSpatialPoints
   * with the given id or NULL if no such
   * SpatialPoint exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual SpatialPoint* get (const std::string& sid);


  /**
   * Get a SpatialPoint from the ListOfSpatialPoints
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the SpatialPoint to get.
   * 
   * @return SpatialPoint in this ListOfSpatialPoints
   * with the given id or NULL if no such
   * SpatialPoint exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const SpatialPoint* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfSpatialPoints items and returns a pointer to
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
  virtual SpatialPoint* remove (unsigned int n);


  /**
   * Removes item in this ListOfSpatialPoints items with the given identifier.
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
  virtual SpatialPoint* remove (const std::string& sid);


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
 * Used by ListOfSpatialPoints::get() to lookup an SBase based on its 
 * symbol
 */
#ifndef SWIG
template<>
struct IdEq<SpatialPoint> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <SpatialPoint*> (sb)->getSpatialId() == id; }
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
SpatialPoint_getSpatialId (const SpatialPoint_t *p);


LIBSBML_EXTERN
const char *
SpatialPoint_getDomain (const SpatialPoint_t *p);


LIBSBML_EXTERN
double
SpatialPoint_getCoord1 (const SpatialPoint_t *p);


LIBSBML_EXTERN
double
SpatialPoint_getCoord2 (const SpatialPoint_t *p);


LIBSBML_EXTERN
double
SpatialPoint_getCoord3 (const SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_isSetSpatialId (const SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_isSetDomain (const SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_isSetCoord1 (const SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_isSetCoord2 (const SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_isSetCoord3 (const SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_setSpatialId (SpatialPoint_t *p, const char *sid);


LIBSBML_EXTERN
int
SpatialPoint_setDomain (SpatialPoint_t *p, const char *sid);


LIBSBML_EXTERN
int
SpatialPoint_setCoord1 (SpatialPoint_t *p, double coord1);


LIBSBML_EXTERN
int
SpatialPoint_setCoord2 (SpatialPoint_t *p, double coord2);


LIBSBML_EXTERN
int
SpatialPoint_setCoord3 (SpatialPoint_t *p, double coord3);


LIBSBML_EXTERN
int
SpatialPoint_unsetSpatialId (SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_unsetDomain (SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_unsetCoord1 (SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_unsetCoord2 (SpatialPoint_t *p);


LIBSBML_EXTERN
int
SpatialPoint_unsetCoord3 (SpatialPoint_t *p);


LIBSBML_EXTERN
SpatialPoint_t *
SpatialPoint_clone (const SpatialPoint_t *p);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* SpatialPoint_H__ */
