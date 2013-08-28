/*
 * @file    DomainType.h
 * @brief   Definition of DomainType, the SBase derived class of spatial package.
 * @author  Akiya Jouraku
 *
 * $Id: DomainType.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/DomainType.h $
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


#ifndef DomainType_H__
#define DomainType_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DomainType : public SBase
{
protected:

  std::string mSpatialId;
  unsigned int  mSpatialDimensions;

  bool  mIsSetSpatialDimensions;

public:

  /**
   * Creates a new DomainType with the given level, version, and package version.
   */
   DomainType(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new DomainType with the given SpatialPkgNamespaces object.
   */
   DomainType(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   DomainType(const DomainType& source);


  /**
   * Assignment operator.
   */
   DomainType& operator=(const DomainType& source);


  /**
   * Destructor.
   */ 
  virtual ~DomainType ();


  /**
   * Returns the string of the "spatialId" attribute of this DomainType.
   *
   * @return the string of the "spatialId" attribute of this DomainType.
   */
  virtual const std::string& getSpatialId () const;


  /**
   * Get the spatial dimensions of this DomainType object.
   * 
   * @return the value of the "spatialDimensions" attribute of this
   * DomainType as an unsigned integer
   */
  unsigned int getSpatialDimensions () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * DomainType's "spatialId" attribute has been set.
   *
   * @return @c true if this DomainType's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * DomainType's "spatialDimensions" attribute has been set.
   * 
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @note This function only applies to SBML L3 where there are no
   * default values.
   *
   * @return @c true if the "spatialDimensions" attribute of this DomainType has
   * been set, @c false otherwise.
   */
  bool isSetSpatialDimensions () const;

  
  /**
   * Sets the SIdRef string of the "spatialId" attribute of this DomainType.
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
   * Sets the "spatialDimensions" attribute of this DomainType.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * If @p value is not one of @c 0, @c 1, @c 2, or @c 3, this method will
   * have no effect (i.e., the "spatialDimensions" attribute will not be
   * set).
   * 
   * @param value an unsigned integer indicating the number of dimensions
   * of this compartment.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   * @li LIBSBML_UNEXPECTED_ATTRIBUTE
   */
  int setSpatialDimensions (unsigned int value);


  /**
   * Unsets the value of the "id" attribute of this DomainType.
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
   * Unsets the value of the "spatialDimensions" attribute of this DomainType.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   *
   * @note This function is only valid for SBML L3
   */
  int unsetSpatialDimensions ();


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this DomainType.
   */
  virtual DomainType* clone () const;


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

class LIBSBML_EXTERN ListOfDomainTypes : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfDomainTypes.
   */
  virtual ListOfDomainTypes* clone () const;


  /**
   * Creates a new ListOfDomainTypes with the given level, version, and package version.
   */
   ListOfDomainTypes(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDomainTypes with the given SpatialPkgNamespaces object.
   */
   ListOfDomainTypes(SpatialPkgNamespaces* spatialns);


  /**
   * Get a DomainType from the ListOfDomainTypes.
   *
   * @param n the index number of the DomainType to get.
   * 
   * @return the nth DomainType in this ListOfDomainTypes.
   *
   * @see size()
   */
  virtual DomainType * get(unsigned int n); 

  /**
   * Get a DomainType from the ListOfDomainTypes.
   *
   * @param n the index number of the DomainType to get.
   * 
   * @return the nth DomainType in this ListOfDomainTypes.
   *
   * @see size()
   */
  virtual const DomainType * get(unsigned int n) const; 

  /**
   * Get a DomainType from the ListOfDomainTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the DomainType to get.
   * 
   * @return DomainType in this ListOfDomainTypes
   * with the given id or NULL if no such
   * DomainType exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual DomainType* get (const std::string& sid);


  /**
   * Get a DomainType from the ListOfDomainTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the DomainType to get.
   * 
   * @return DomainType in this ListOfDomainTypes
   * with the given id or NULL if no such
   * DomainType exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const DomainType* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfDomainTypes items and returns a pointer to
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
  virtual DomainType* remove (unsigned int n);


  /**
   * Removes item in this ListOfDomainTypes items with the given identifier.
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
  virtual DomainType* remove (const std::string& sid);


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
 * Used by ListOfDomainTypes::get() to lookup an SBase based on its 
 * spatialId
 */
#ifndef SWIG
template<>
struct IdEq<DomainType> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <DomainType*> (sb)->getSpatialId() == id; }
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
DomainType_getSpatialId (const DomainType_t *c);


LIBSBML_EXTERN
unsigned int
DomainType_getSpatialDimensions (const DomainType_t *c);


LIBSBML_EXTERN
DomainType_t *
DomainType_clone (const DomainType_t* c);


LIBSBML_EXTERN
int
DomainType_isSetSpatialId (const DomainType_t *c);


LIBSBML_EXTERN
int
DomainType_isSetSpatialDimensions (const DomainType_t *c);


LIBSBML_EXTERN
int
DomainType_setSpatialId (DomainType_t *c, const char *sid);


LIBSBML_EXTERN
int
DomainType_setSpatialDimensions (DomainType_t *c, unsigned int value);


LIBSBML_EXTERN
int
DomainType_unsetSpatialId (DomainType_t *c);


LIBSBML_EXTERN
int
DomainType_unsetSpatialDimensions (DomainType_t *c);


LIBSBML_EXTERN
DomainType_t *
ListOfDomainTypes_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
DomainType_t *
ListOfDomainTypes_removeById (ListOf_t *lo, const char *sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* DomainType_H__ */
