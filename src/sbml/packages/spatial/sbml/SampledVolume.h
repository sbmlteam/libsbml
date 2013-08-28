/*
 * @file    SampledVolume.h
 * @brief   Definition of SampledVolume, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SampledVolume.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SampledVolume.h $
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


#ifndef SampledVolume_H__
#define SampledVolume_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SampledVolume : public SBase
{
protected:

  std::string mSpatialId;
  std::string mDomainType;

  double mMinValue;
  double mSampledValue;
  double mMaxValue;

  bool  mIsSetMinValue;
  bool  mIsSetSampledValue;
  bool  mIsSetMaxValue;


public:

  /**
   * Creates a new SampledVolume with the given level, version, and package version.
   */
   SampledVolume(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SampledVolume with the given SpatialPkgNamespaces object.
   */
   SampledVolume(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   SampledVolume(const SampledVolume& source);


  /**
   * Assignment operator.
   */
   SampledVolume& operator=(const SampledVolume& source);


  /**
   * Destructor.
   */ 
  virtual ~SampledVolume ();

  /**
   * Returns the string of the "spatialId" attribute of this SampledVolume.
   *
   * @return the string of the "spatialId" attribute of this SampledVolume.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "domainType" attribute of this SampledVolume.
   *
   * @return the string of the "domainType" attribute of this SampledVolume.
   */
  virtual const std::string& getDomainType () const;

  /**
   * Get the value of the "sampledValue" attribute.
   * 
   * @return the sampledValue value of this SampledVolume, as a float-point number.
   */
  double getSampledValue () const;

  /**
   * Get the value of the "minValue" attribute.
   * 
   * @return the minValue value of this SampledVolume, as a float-point number.
   */
  double getMinValue () const;

  /**
   * Get the value of the "maxValue" attribute.
   * 
   * @return the maxValue of this SampledVolume, as a float-point
   * number.
   */
  double getMaxValue () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "spatialId" attribute has been set.
   *
   * @return @c true if this SampledVolume's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "domainType" attribute has been set.
   *
   * @return @c true if this SampledVolume's "domainType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDomainType () const;

 /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "sampledValue" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "sampledValue" attribute of this SampledVolume has
   * been set, @c false otherwise.
   *
   * @note In SBML Level&nbsp;1, SampledVolume' "sampledValue" is required and
   * therefore <em>should always be set</em>.  (However, in Level&nbsp;1, the
   * attribute has no default value either, so this method will not return
   * @c true until a value has been assigned.)  In SBML Level&nbsp;2,
   * "sampledValue" is optional and as such may or may not be set.
   */
  bool isSetSampledValue () const;

 /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "minValue" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "minValue" attribute of this SampledVolume has
   * been set, @c false otherwise.
   *
   * @note In SBML Level&nbsp;1, SampledVolume' "minValue" is required and
   * therefore <em>should always be set</em>.  (However, in Level&nbsp;1, the
   * attribute has no default value either, so this method will not return
   * @c true until a value has been assigned.)  In SBML Level&nbsp;2,
   * "minValue" is optional and as such may or may not be set.
   */
  bool isSetMinValue () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledVolume's "maxValue" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "maxValue" attribute of this SampledVolume has
   * been set, @c false otherwise.
   */
  bool isSetMaxValue () const;

  /**
   * Sets the SIdRef string of the "spatialId" attribute of this SampledVolume.
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
   * Sets the SIdRef string of the "domainType" attribute of this SampledVolume.
   *
   * @param domainType a SIdRef string to be set.
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
   * Sets the "sampledValue" attribute of this SampledVolume and marks the field
   * as set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param value the value to which the "sampledValue" attribute should
   * be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int setSampledValue (double value);

   /**
   * Sets the "minValue" attribute of this SampledVolume and marks the field
   * as set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param value the value to which the "minValue" attribute should
   * be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int setMinValue (double value);


  /**
   * Sets the "maxValue" attribute of this SampledVolume and marks
   * the field as set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param value the value to which the "maxValue" attribute
   * should be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  int setMaxValue (double value);


  /**
   * Unsets the value of the "id" attribute of this SampledVolume.
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
   * Unsets the value of the "domainType" attribute of this SampledVolume.
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
   * Unsets the "sampledValue" attribute value of this SampledVolume.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int unsetSampledValue ();

  /**
   * Unsets the "minValue" attribute value of this SampledVolume.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int unsetMinValue ();

  /**
   * Unsets the "maxValue" attribute value of this SampledVolume.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int unsetMaxValue ();

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this SampledVolume.
   */
  virtual SampledVolume* clone () const;


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

class LIBSBML_EXTERN ListOfSampledVolumes : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfSampledVolumes.
   */
  virtual ListOfSampledVolumes* clone () const;


  /**
   * Creates a new ListOfSampledVolumes with the given level, version, and package version.
   */
   ListOfSampledVolumes(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSampledVolumes with the given spatialPkgNamespaces object.
   */
   ListOfSampledVolumes(SpatialPkgNamespaces* spatialsns);


  /**
   * Get a SampledVolume from the ListOfSampledVolumes.
   *
   * @param n the index number of the SampledVolume to get.
   * 
   * @return the nth SampledVolume in this ListOfSampledVolumes.
   *
   * @see size()
   */
  virtual SampledVolume * get(unsigned int n); 


  /**
   * Get a SampledVolume from the ListOfSampledVolumes.
   *
   * @param n the index number of the SampledVolume to get.
   * 
   * @return the nth SampledVolume in this ListOfSampledVolumes.
   *
   * @see size()
   */
  virtual const SampledVolume * get(unsigned int n) const; 

  /**
   * Get a SampledVolume from the ListOfSampledVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the SampledVolume to get.
   * 
   * @return SampledVolume in this ListOfSampledVolumes
   * with the given id or NULL if no such
   * SampledVolume exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual SampledVolume* get (const std::string& sid);


  /**
   * Get a SampledVolume from the ListOfSampledVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the SampledVolume to get.
   * 
   * @return SampledVolume in this ListOfSampledVolumes
   * with the given id or NULL if no such
   * SampledVolume exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const SampledVolume* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfSampledVolumes items and returns a pointer to
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
  virtual SampledVolume* remove (unsigned int n);


  /**
   * Removes item in this ListOfSampledVolumes items with the given identifier.
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
  virtual SampledVolume* remove (const std::string& sid);


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
 * Used by ListOfSampledVolumes::get() to lookup an SBase based by its 
 * componentType
 */
#ifndef SWIG
template<>
struct IdEq<SampledVolume> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <SampledVolume*> (sb)->getSpatialId() == id; }
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
SampledVolume_getSpatialId (const SampledVolume_t *c);


LIBSBML_EXTERN
const char *
SampledVolume_getDomainType (const SampledVolume_t *sv);


LIBSBML_EXTERN
double
SampledVolume_getSampledValue (const SampledVolume_t *c);


LIBSBML_EXTERN
double
SampledVolume_getMinValue (const SampledVolume_t *c);


LIBSBML_EXTERN
double
SampledVolume_getMaxValue (const SampledVolume_t *c);


LIBSBML_EXTERN
SampledVolume_t *
SampledVolume_clone (const SampledVolume_t* c);


LIBSBML_EXTERN
int
SampledVolume_isSetSpatialId (const SampledVolume_t *c);


LIBSBML_EXTERN
int
SampledVolume_isSetDomainType (const SampledVolume_t *sv);


LIBSBML_EXTERN
int
SampledVolume_isSetSampledValue(const SampledVolume_t *c);


LIBSBML_EXTERN
int
SampledVolume_isSetMinValue(const SampledVolume_t *c);


LIBSBML_EXTERN
int
SampledVolume_isSetMaxValue(const SampledVolume_t *c);


LIBSBML_EXTERN
int
SampledVolume_setSpatialId (SampledVolume_t *c, const char *sid);


LIBSBML_EXTERN
int
SampledVolume_setDomainType (SampledVolume_t *sv, const char *dt);


LIBSBML_EXTERN
int
SampledVolume_setSampledValue (SampledVolume_t *c, double value);


LIBSBML_EXTERN
int
SampledVolume_setMinValue (SampledVolume_t *c, double value);


LIBSBML_EXTERN
int
SampledVolume_setMaxValue (SampledVolume_t *c, double value);


LIBSBML_EXTERN
int
SampledVolume_unsetSpatialId (SampledVolume_t *c);


LIBSBML_EXTERN
int
SampledVolume_unsetDomainType (SampledVolume_t *sv);


LIBSBML_EXTERN
int
SampledVolume_unsetSampledValue (SampledVolume_t *c);


LIBSBML_EXTERN
int
SampledVolume_unsetMinValue (SampledVolume_t *c);


LIBSBML_EXTERN
int
SampledVolume_unsetMaxValue (SampledVolume_t *c);


LIBSBML_EXTERN
SampledVolume_t *
ListOfSampledVolumes_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
SampledVolume_t *
ListOfSampledVolumes_removeById (ListOf_t *lo, const char *sid);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* SampledVolume_H__ */
