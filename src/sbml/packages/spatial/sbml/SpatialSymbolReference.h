/*
 * @file    SpatialSymbolReference.h
 * @brief   Definition of SpatialSymbolReference, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SpatialSymbolReference.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SpatialSymbolReference.h $
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


#ifndef SpatialSymbolReference_H__
#define SpatialSymbolReference_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpatialSymbolReference : public SBase
{
protected:

  std::string mSpatialId;
  std::string mType;

public:

  /**
   * Creates a new SpatialSymbolReference with the given level, version, and package version.
   */
   SpatialSymbolReference(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpatialSymbolReference with the given SpatialPkgNamespaces object.
   */
   SpatialSymbolReference(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   SpatialSymbolReference(const SpatialSymbolReference& source);


  /**
   * Assignment operator.
   */
   SpatialSymbolReference& operator=(const SpatialSymbolReference& source);


  /**
   * Destructor.
   */ 
  virtual ~SpatialSymbolReference ();


  /**
   * Returns the string of the "spatialId" attribute of this SpatialSymbolReference.
   *
   * @return the string of the "spatialId" attribute of this SpatialSymbolReference.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "type" attribute of this SpatialSymbolReference.
   *
   * @return the string of the "type" attribute of this SpatialSymbolReference.
   */
  virtual const std::string& getType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialSymbolReference's "spatialId" attribute has been set.
   *
   * @return @c true if this SpatialSymbolReference's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;

 /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialSymbolReference's "type" attribute has been set.
   *
   * @return @c true if this SpatialSymbolReference's "type" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetType () const;
  
  /**
   * Sets the SIdRef string of the "spatialId" attribute of this SpatialSymbolReference.
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
   * Sets the SIdRef string of the "type" attribute of this SpatialSymbolReference.
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
  virtual int setType (const std::string& type);


  /**
   * Unsets the value of the "spatialId" attribute of this SpatialSymbolReference.
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
   * Unsets the value of the "type" attribute of this SpatialSymbolReference.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetType ();


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this SpatialSymbolReference.
   */
  virtual SpatialSymbolReference* clone () const;


  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;


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


/** @cond doxygen-libsbml-internal */
/**
 * Used by ListOfSpatialSymbolReferences::get() to lookup an SBase based on its 
 * spatialId
 */
#ifndef SWIG
template<>
struct IdEq<SpatialSymbolReference> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <SpatialSymbolReference*> (sb)->getSpatialId() == id; }
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
SpatialSymbolReference_t *
SpatialSymbolReference_clone (const SpatialSymbolReference_t* c);


LIBSBML_EXTERN
const char *
SpatialSymbolReference_getSpatialId (const SpatialSymbolReference_t *c);


LIBSBML_EXTERN
const char *
SpatialSymbolReference_getType (const SpatialSymbolReference_t *c);


LIBSBML_EXTERN
int
SpatialSymbolReference_isSetSpatialId (const SpatialSymbolReference_t *c);


LIBSBML_EXTERN
int
SpatialSymbolReference_isSetType (const SpatialSymbolReference_t *c);


LIBSBML_EXTERN
int
SpatialSymbolReference_setSpatialId (SpatialSymbolReference_t *c, const char *sid);


LIBSBML_EXTERN
int
SpatialSymbolReference_setType (SpatialSymbolReference_t *c, const char *type);


LIBSBML_EXTERN
int
SpatialSymbolReference_unsetSpatialId (SpatialSymbolReference_t *c);


LIBSBML_EXTERN
int
SpatialSymbolReference_unsetType (SpatialSymbolReference_t *c);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* SpatialSymbolReference_H__ */
