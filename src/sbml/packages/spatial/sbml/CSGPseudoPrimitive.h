/*
 * @file    CSGPseudoPrimitive.h
 * @brief   Definition of CSGPseudoPrimitive, of spatial package.
 * @author  
 *
 * $Id: CSGPseudoPrimitive.h $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGPseudoPrimitive.h $
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


#ifndef CSGPseudoPrimitive_H__
#define CSGPseudoPrimitive_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGPseudoPrimitive : public CSGNode
{
protected:

  std::string mcsgObjectRef;

public:

  /**
   * Creates a new CSGPseudoPrimitive with the given level, version, and package version.
   */
   CSGPseudoPrimitive(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGPseudoPrimitive with the given SpatialPkgNamespaces object.
   */
   CSGPseudoPrimitive(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   CSGPseudoPrimitive(const CSGPseudoPrimitive& source);


  /**
   * Assignment operator.
   */
   CSGPseudoPrimitive& operator=(const CSGPseudoPrimitive& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGPseudoPrimitive ();

  /**
   * Returns the string of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
   *
   * @return the string of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
   */
  virtual const std::string& getCSGObjectRef () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGPseudoPrimitive's "CSGObjectRef" attribute has been set.
   *
   * @return @c true if this CSGPseudoPrimitive's "CSGObjectRef" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCSGObjectRef () const;
  
  /**
   * Sets the SIdRef string of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
   *
   * @param CSGObjectRef a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCSGObjectRef (const std::string& csgObjectRef);

  /**
   * Unsets the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCSGObjectRef ();

/**
   * @return a (deep) copy of this CSGPseudoPrimitive.
   */
  virtual CSGPseudoPrimitive* clone () const;


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
  
  virtual const std::string& getElementName () const ;
   */

  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()

   int getTypeCode () const;
  */

  
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


  /** @cond doxygen-libsbml-internal */
  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument object to use
   */
  virtual void setSBMLDocument (SBMLDocument* d);

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
CSGPseudoPrimitive_getCSGObjectRef (const CSGPseudoPrimitive_t *csgpp);


LIBSBML_EXTERN
int
CSGPseudoPrimitive_isSetCSGObjectRef (const CSGPseudoPrimitive_t *csgpp);


LIBSBML_EXTERN
int
CSGPseudoPrimitive_setCSGObjectRef (CSGPseudoPrimitive_t *csgpp, const char *sid);


LIBSBML_EXTERN
int
CSGPseudoPrimitive_unsetCSGObjectRef (CSGPseudoPrimitive_t *csgpp);


LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGPseudoPrimitive_clone (const CSGPseudoPrimitive_t* csgpp);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGPseudoPrimitive_H__ */
