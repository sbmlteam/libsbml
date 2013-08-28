/*
 * @file    Domain.h
 * @brief   Definition of Domain, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: Domain.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/Domain.h $
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


#ifndef Domain_H__
#define Domain_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/sbml/InteriorPoint.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Domain : public SBase
{
protected:

  std::string mSpatialId;
  std::string mDomainType;

  ListOfInteriorPoints mInteriorPoints;

public:

  /**
   * Creates a new Domain with the given level, version, and package version.
   */
   Domain(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new Domain with the given SpatialPkgNamespaces object.
   */
   Domain(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   Domain(const Domain& source);


  /**
   * Assignment operator.
   */
   Domain& operator=(const Domain& source);


  /**
   * Destructor.
   */ 
  virtual ~Domain ();

 /**
   * Returns the string of the "spatialId" attribute of this Domain.
   *
   * @return the string of the "spatialId" attribute of this Domain.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "domainType" attribute of this Domain.
   *
   * @return the string of the "domainType" attribute of this Domain.
   */
  virtual const std::string& getDomainType () const;

  virtual bool isSetSpatialId () const;

    /**
   * Predicate returning @c true or @c false depending on whether this
   * Domain's "domainType" attribute has been set.
   *
   * @return @c true if this Domain's "domainType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDomainType () const;


  /**
   * Sets the SIdRef string of the "spatialId" attribute of this Domain.
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
   * Sets the SIdRef string of the "domainType" attribute of this Domain.
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
   * Unsets the value of the "spatialId" attribute of this Domain.
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
   * Unsets the value of the "domainType" attribute of this Domain.
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
   * Adds a copy of the given InteriorPoint object to this Domain.
   *
   * @param cc the InteriorPoint to add
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
   * one contained in this Domain.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the Domain</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see Domain::createInteriorPoint()
   * for a method that does not lead to these issues.
   *
   * @see createInteriorPoint()
   */
  int addInteriorPoint (const InteriorPoint* p);

  /**
   * Creates a new InteriorPoint inside this Domain and returns it.
   *
   * @return the InteriorPoint object created
   *
   * @see addInteriorPoint(const InteriorPoint* cc)
   */
  InteriorPoint* createInteriorPoint ();

  /**
   * Get the ListOfInteriorPoints object in this Domain.
   * 
   * @return the list of InteriorPoints for this Domain.
   */
  const ListOfInteriorPoints* getListOfInteriorPoints () const;

  /**
   * Get the ListOfInteriorPoints object in this Domain.
   * 
   * @return the list of InteriorPoints for this Domain.
   */
  ListOfInteriorPoints* getListOfInteriorPoints ();

 /**
   * Get the nth InteriorPoint object in this Domain.
   * 
   * @return the nth InteriorPoint of this Domain.
   */
  const InteriorPoint* getInteriorPoint (unsigned int n) const;


  /**
   * Get the nth InteriorPoints object in this Domain.
   * 
   * @return the nth InteriorPoint of this Domain.
   */
  InteriorPoint* getInteriorPoint (unsigned int n);

  /**
   * Get the number of InteriorPoint objects in this Domain.
   * 
   * @return the number of InteriorPoints in this Domain.
   */
  unsigned int getNumInteriorPoints () const;

  /**
   * Removes the nth InteriorPoint object from this Domain object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the InteriorPoint object to remove
   *
   * @return the InteriorPoint object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  InteriorPoint* removeInteriorPoint (unsigned int n);


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this Domain.
   */
  virtual Domain* clone () const;


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

class LIBSBML_EXTERN ListOfDomains : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfDomains.
   */
  virtual ListOfDomains* clone () const;


  /**
   * Creates a new ListOfDomains with the given level, version, and package version.
   */
   ListOfDomains(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDomains with the given spatialPkgNamespaces object.
   */
   ListOfDomains(SpatialPkgNamespaces* spatialsns);


  /**
   * Get a Domain from the ListOfDomains.
   *
   * @param n the index number of the Domain to get.
   * 
   * @return the nth Domain in this ListOfDomains.
   *
   * @see size()
   */
  virtual Domain * get(unsigned int n); 


  /**
   * Get a Domain from the ListOfDomains.
   *
   * @param n the index number of the Domain to get.
   * 
   * @return the nth Domain in this ListOfDomains.
   *
   * @see size()
   */
  virtual const Domain * get(unsigned int n) const; 

  /**
   * Get a Domain from the ListOfDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Domain to get.
   * 
   * @return Domain in this ListOfDomains
   * with the given id or NULL if no such
   * Domain exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual Domain* get (const std::string& sid);


  /**
   * Get a Domain from the ListOfDomains
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Domain to get.
   * 
   * @return Domain in this ListOfDomains
   * with the given id or NULL if no such
   * Domain exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const Domain* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfDomains items and returns a pointer to
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
  virtual Domain* remove (unsigned int n);


  /**
   * Removes item in this ListOfDomains items with the given identifier.
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
  virtual Domain* remove (const std::string& sid);


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
 * Used by ListOfDomains::get() to lookup an SBase based by its 
 * spatialId
 */
#ifndef SWIG
template<>
struct IdEq<Domain> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <Domain*> (sb)->getSpatialId() == id; }
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
Domain_getSpatialId (const Domain_t *c);


LIBSBML_EXTERN
const char *
Domain_getDomainType (const Domain_t *c);


LIBSBML_EXTERN
Domain_t *
Domain_clone (const Domain_t* c);


LIBSBML_EXTERN
int
Domain_isSetSpatialId (const Domain_t *c);


LIBSBML_EXTERN
int
Domain_isSetDomainType (const Domain_t *c);


LIBSBML_EXTERN
int
Domain_setSpatialId (Domain_t *c, const char *sid);


LIBSBML_EXTERN
int
Domain_setDomainType (Domain_t *c, const char *sid);


LIBSBML_EXTERN
int
Domain_unsetSpatialId (Domain_t *c);


LIBSBML_EXTERN
int
Domain_unsetDomainType (Domain_t *c);


LIBSBML_EXTERN
int
Domain_addInteriorPoint (Domain_t *d, const InteriorPoint_t *p);


LIBSBML_EXTERN
InteriorPoint_t *
Domain_createInteriorPoint (Domain_t *d);


LIBSBML_EXTERN
ListOf_t *
Domain_getListOfInteriorPoints (Domain_t *d);


LIBSBML_EXTERN
InteriorPoint_t *
Domain_getInteriorPoint (Domain_t *d, unsigned int n);


LIBSBML_EXTERN
unsigned int
Domain_getNumInteriorPoints (const Domain_t *d);


LIBSBML_EXTERN
InteriorPoint_t*
Domain_removeInteriorPoint (Domain_t *d, unsigned int n);


LIBSBML_EXTERN
Domain_t *
ListOfDomains_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
Domain_t *
ListOfDomains_removeById (ListOf_t *lo, const char *sid);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* Domain_H__ */
