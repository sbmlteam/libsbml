/*
 * @file    TransformationComponents.h
 * @brief   Definition of TransformationComponents, of spatial package.
 * @author  
 *
 * $Id: TransformationComponents.h  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/TransformationComponents.h $
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


#ifndef TransformationComponents_H__
#define TransformationComponents_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN TransformationComponents
{
protected:

  double* mComponents;
  unsigned int mComponentsLength;
  
  std::string mURI;
  SBase* mParentSBMLObject;
  SBMLNamespaces* mSBMLNamespaces;

  bool  mIsSetComponents;

public:

  /**
   * Creates a new TransformationComponents with the given level, version, and package version.
   */
   TransformationComponents(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new TransformationComponents with the given SpatialPkgNamespaces object.
   */
   TransformationComponents(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   TransformationComponents(const TransformationComponents& source);


  /**
   * Assignment operator.
   */
   TransformationComponents& operator=(const TransformationComponents& source);


  /**
   * Destructor.
   */ 
  virtual ~TransformationComponents ();

 /**
   * The "components" attribute of this TransformationComponents is returned in a double array (pointer) 
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * @return void.
   */
  void getComponents(double* outputComponents) const;

 /**
   * Returns the "componentsLength" attribute of this TransformationComponents.
   *
   * @return the "componentsLength" attribute of this TransformationComponents.
   */
  unsigned int getComponentsLength() const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * TransformationComponents's "components" attribute has been set.
   *
   * @return @c true if this TransformationComponents's "components" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetComponents () const;
  
  /**
   * Sets the SIdRef string of the "components" attribute of this TransformationComponents.
   *
   * @param components an integer array  to be set.
   * @param componentsLength : length of integer array to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setComponents (double* components, int componentsLength);

  /**
   * Unsets the value of the "components" attribute of this TransformationComponents.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetComponents ();

  /**
   * Creates a copy of this TransformationComponents and all its children.
   * 
   * @return a copy of this TransformationComponents and all its children.  The caller owns
   * the returned TransformationComponents and is reponsible for deleting it.
   */
  TransformationComponents* deepCopy () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;

  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;
  
#ifndef SWIG
/**
 * Reads the TransformationComponents from the given XMLInputStream, 
 */
  static TransformationComponents* readTransformationComponents (XMLInputStream& stream);


  /** @cond doxygenLibsbmlInternal */
  /**
   * Write TransformationComponents to XMLOutputStream
   *
   */
 static void writeTransformationComponents (TransformationComponents* TransformationComponents, XMLOutputStream& stream);

  /** @cond doxygenLibsbmlInternal */
#endif

  /**
   * Sets the parent SBML object.
   * 
   * @param sb the parent SBML object of this ASTNode.
   */
  void setParentSBMLObject(SBase * sb);

  /** @endcond doxygenLibsbmlInternal */

  /**
   * Returns the parent SBML object.
   * 
   * @return the parent SBML object of this ASTNode.
   */
  SBase * getParentSBMLObject() const;

  protected:
  
  /* gets the SBMLnamespaces - internal use only*/
  SBMLNamespaces * getSBMLNamespaces() const;

  /* gets the URI - internal use only*/
  std::string getURI () const;
    
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
void
TransformationComponents_getComponents (const TransformationComponents_t *c, double* outputComponents);


LIBSBML_EXTERN
unsigned int
TransformationComponents_getComponentsLength (const TransformationComponents_t *c);


LIBSBML_EXTERN
int
TransformationComponents_isSetComponents (const TransformationComponents_t *c);


LIBSBML_EXTERN
int
TransformationComponents_setComponents (TransformationComponents_t *c, double* value, int length);


LIBSBML_EXTERN
int
TransformationComponents_unsetComponents (TransformationComponents_t *c);


LIBSBML_EXTERN
TransformationComponents_t *
TransformationComponents_deepCopy (const TransformationComponents_t *id);


LIBSBML_EXTERN
SBase_t * 
TransformationComponents_getParentSBMLObject(TransformationComponents_t* id);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* TransformationComponents_H__ */
