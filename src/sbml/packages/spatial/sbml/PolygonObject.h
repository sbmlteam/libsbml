/*
 * @file    PolygonObject.h
 * @brief   Definition of PolygonObject, of spatial package.
 * @author  
 *
 * $Id: PolygonObject.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/PolygonObject.h $
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


#ifndef PolygonObject_H__
#define PolygonObject_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN PolygonObject
{
protected:

  int* mPointIndices;
  unsigned int mIndicesLength;
  
  std::string mURI;
  SBase* mParentSBMLObject;
  SBMLNamespaces* mSBMLNamespaces;

  bool  mIsSetPointIndices;

public:

  /**
   * Creates a new PolygonObject with the given level, version, and package version.
   */
   PolygonObject(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new PolygonObject with the given SpatialPkgNamespaces object.
   */
   PolygonObject(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   PolygonObject(const PolygonObject& source);


  /**
   * Assignment operator.
   */
   PolygonObject& operator=(const PolygonObject& source);


  /**
   * Destructor.
   */ 
  virtual ~PolygonObject ();

 /**
   * The "pointIndices" attribute of this PolygonObject is returned in an int array (pointer) 
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * @return void.
   */
  void getPointIndices(int* outputPointIndices) const;

 /**
   * Returns the "indicesLength" attribute of this PolygonObject.
   *
   * @return the "indicesLength" attribute of this PolygonObject.
   */
  unsigned int getIndicesLength() const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * PolygonObject's "pointIndices" attribute has been set.
   *
   * @return @c true if this PolygonObject's "pointIndices" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetPointIndices () const;
  
  /**
   * Sets the SIdRef string of the "pointIndices" attribute of this PolygonObject.
   *
   * @param pointIndices an integer array  to be set.
   * @param indicesLength : length of integer array to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setPointIndices (int* pointIndices, int indicesLength);

  /**
   * Unsets the value of the "pointIndices" attribute of this PolygonObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetPointIndices ();

  /**
   * Creates a copy of this PolygonObject and all its children.
   * 
   * @return a copy of this PolygonObject and all its children.  The caller owns
   * the returned PolygonObject and is reponsible for deleting it.
   */
  PolygonObject* deepCopy () const;

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
 * Reads the PolygonObject from the given XMLInputStream, 
 */
  static PolygonObject* readPolygonObject (XMLInputStream& stream);


  /** @cond doxygenLibsbmlInternal */
  /**
   * Write PolygonObject to XMLOutputStream
   *
   */
 static void writePolygonObject (PolygonObject* PolygonObject, XMLOutputStream& stream);

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
PolygonObject_getPointIndices (const PolygonObject_t *c, int* outputPointIndices);


LIBSBML_EXTERN
unsigned int
PolygonObject_getIndicesLength (const PolygonObject_t *c);


LIBSBML_EXTERN
int
PolygonObject_isSetPointIndices (const PolygonObject_t *c);


LIBSBML_EXTERN
int
PolygonObject_setPointIndices (PolygonObject_t *c, int* value, int length);


LIBSBML_EXTERN
int
PolygonObject_unsetPointIndices (PolygonObject_t *c);


LIBSBML_EXTERN
PolygonObject_t *
PolygonObject_deepCopy (const PolygonObject_t *id);


LIBSBML_EXTERN
SBase_t * 
PolygonObject_getParentSBMLObject(PolygonObject_t* id);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* PolygonObject_H__ */
