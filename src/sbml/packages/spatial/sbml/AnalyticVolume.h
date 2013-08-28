/*
 * @file    AnalyticVolume.h
 * @brief   Definition of AnalyticVolume, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: AnalyticVolume.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/AnalyticVolume.h $
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


#ifndef AnalyticVolume_H__
#define AnalyticVolume_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN AnalyticVolume : public SBase
{
protected:

  std::string mSpatialId;
  std::string mDomainType;
  std::string mFunctionType;
  unsigned int mOrdinal;
  ASTNode* mMath;

  bool  mIsSetOrdinal;

public:

  /**
   * Creates a new AnalyticVolume with the given level, version, and package version.
   */
   AnalyticVolume(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AnalyticVolume with the given SpatialPkgNamespaces object.
   */
   AnalyticVolume(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   AnalyticVolume(const AnalyticVolume& source);


  /**
   * Assignment operator.
   */
   AnalyticVolume& operator=(const AnalyticVolume& source);


  /**
   * Destructor.
   */ 
  virtual ~AnalyticVolume ();


  /**
   * Returns the string of the "spatialId" attribute of this AnalyticVolume.
   *
   * @return the string of the "spatialId" attribute of this AnalyticVolume.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "domainType" attribute of this AnalyticVolume.
   *
   * @return the string of the "domainType" attribute of this AnalyticVolume.
   */
  virtual const std::string& getDomainType () const;

  /**
   * Returns the string of the "functionType" attribute of this AnalyticVolume.
   *
   * @return the string of the "functionType" attribute of this AnalyticVolume.
   */
  virtual const std::string& getFunctionType () const;

  /**
   * Returns the string of the "ordinal" attribute of this AnalyticVolume.
   *
   * @return the string of the "ordinal" attribute of this AnalyticVolume.
   */
  virtual unsigned int getOrdinal () const;

    /**
   * Returns the mathematical formula for this AnalyticVolume object as as an AST.
   *
   * This is fundamentally equivalent to getFormula().  The latter is
   * provided principally for compatibility with SBML Level
   * 1, which represented mathematical formulas in text-string form.
   * 
   * @return the ASTNode representation of the mathematical formula.
   *
   * @see getFormula()
   */
  const ASTNode* getMath () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "spatialId" attribute has been set.
   *
   * @return @c true if this AnalyticVolume's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "domainType" attribute has been set.
   *
   * @return @c true if this AnalyticVolume's "domainType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDomainType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "functionType" attribute has been set.
   *
   * @return @c true if this AnalyticVolume's "FunctionType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetFunctionType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "ordinal" attribute has been set.
   *
   * @return @c true if this AnalyticVolume's "ordinal" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetOrdinal () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * AnalyticVolume's "math" subelement contains a value.
   * 
   * @return @c true if the "math" for this AnalyticVolume has been set,
   * @c false otherwise.
   */
  bool isSetMath () const;

  
  /**
   * Sets the SIdRef string of the "spatialId" attribute of this AnalyticVolume.
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
   * Sets the SIdRef string of the "domainType" attribute of this AnalyticVolume.
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
   * Sets the SIdRef string of the "functionType" attribute of this AnalyticVolume.
   *
   * @param functionType a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setFunctionType (const std::string& functionType);

  /**
   * Sets the int of the "ordinal" attribute of this AnalyticVolume.
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
   * Sets the "math" subelement of this AnalyticVolume.
   *
   * The AST passed in @p math is copied.
   *
   * @param math an AST containing the mathematical expression to
   * be used as the formula for this AnalyticVolume.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_OBJECT
   */
  int setMath (const ASTNode* math);


  /**
   * Unsets the value of the "spatialId" attribute of this AnalyticVolume.
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
   * Unsets the value of the "domainType" attribute of this AnalyticVolume.
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
   * Unsets the value of the "functionType" attribute of this AnalyticVolume.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetFunctionType ();

  /**
   * Unsets the value of the "ordinal" attribute of this AnalyticVolume.
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
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this AnalyticVolume.
   */
  virtual AnalyticVolume* clone () const;


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
   * Predicate returning @c true or @c false depending on whether
   * all the required elements for this AnalyticVolume object
   * have been set.
   *
   * @note The required elements for a AnalyticVolume object are:
   * math
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
  friend class MathMLConsistencyValidator;
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

  /** @endcond doxygenLibsbmlInternal */
};

class LIBSBML_EXTERN ListOfAnalyticVolumes : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfAnalyticVolumes.
   */
  virtual ListOfAnalyticVolumes* clone () const;


  /**
   * Creates a new ListOfAnalyticVolumes with the given level, version, and package version.
   */
   ListOfAnalyticVolumes(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfAnalyticVolumes with the given SpatialPkgNamespaces object.
   */
   ListOfAnalyticVolumes(SpatialPkgNamespaces* spatialns);


  /**
   * Get an AnalyticVolume from the ListOfAnalyticVolumes.
   *
   * @param n the index number of the AnalyticVolume to get.
   * 
   * @return the nth AnalyticVolume in this ListOfAnalyticVolumes.
   *
   * @see size()
   */
  virtual AnalyticVolume * get(unsigned int n); 


  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes.
   *
   * @param n the index number of the AnalyticVolume to get.
   * 
   * @return the nth Member in this ListOfAnalyticVolumes.
   *
   * @see size()
   */
  virtual const AnalyticVolume * get(unsigned int n) const; 

  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the AnalyticVolume to get.
   * 
   * @return AnalyticVolume in this ListOfAnalyticVolumes
   * with the given id or NULL if no such
   * AnalyticVolume exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual AnalyticVolume* get (const std::string& sid);


  /**
   * Get a AnalyticVolume from the ListOfAnalyticVolumes
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the AnalyticVolume to get.
   * 
   * @return AnalyticVolume in this ListOfAnalyticVolumes
   * with the given id or NULL if no such
   * AnalyticVolume exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const AnalyticVolume* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfAnalyticVolumes items and returns a pointer to
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
  virtual AnalyticVolume* remove (unsigned int n);


  /**
   * Removes item in this ListOfAnalyticVolumes items with the given identifier.
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
  virtual AnalyticVolume* remove (const std::string& sid);


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
 * Used by ListOfAnalyticVolumes::get() to lookup an SBase based by its 
 * symbol
 */
#ifndef SWIG
template<>
struct IdEq<AnalyticVolume> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <AnalyticVolume*> (sb)->getSpatialId() == id; }
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
AnalyticVolume_t *
AnalyticVolume_clone (const AnalyticVolume_t* c);


LIBSBML_EXTERN
const char *
AnalyticVolume_getSpatialId (const AnalyticVolume_t *av);


LIBSBML_EXTERN
const char *
AnalyticVolume_getDomainType (const AnalyticVolume_t *av);


LIBSBML_EXTERN
const char *
AnalyticVolume_getFunctionType (const AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_getOrdinal (const AnalyticVolume_t *av);


LIBSBML_EXTERN
const ASTNode_t *
AnalyticVolume_getMath (const AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_isSetSpatialId (const AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_isSetDomainType (const AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_isSetFunctionType (const AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_isSetOrdinal (const AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_isSetMath (const AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_setSpatialId (AnalyticVolume_t *av, const char *sid);


LIBSBML_EXTERN
int
AnalyticVolume_setDomainType (AnalyticVolume_t *av, const char *dt);


LIBSBML_EXTERN
int
AnalyticVolume_setFunctionType (AnalyticVolume_t *av, const char *ft);


LIBSBML_EXTERN
int
AnalyticVolume_setOrdinal (AnalyticVolume_t *av, int ord);


LIBSBML_EXTERN
int
AnalyticVolume_setMath (AnalyticVolume_t *av, const ASTNode_t *math);


LIBSBML_EXTERN
int
AnalyticVolume_unsetSpatialId (AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_unsetDomainType (AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_unsetFunctionType (AnalyticVolume_t *av);


LIBSBML_EXTERN
int
AnalyticVolume_unsetOrdinal (AnalyticVolume_t *av);


LIBSBML_EXTERN
AnalyticVolume_t *
ListOfAnalyticVolumes_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
AnalyticVolume_t *
ListOfAnalyticVolumes_removeById (ListOf_t *lo, const char *sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* AnalyticVolume_H__ */
