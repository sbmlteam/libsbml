/*
 * @file    CSGeometry.h
 * @brief   Definition of CSGeometry, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGeometry.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGeometry.h $
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


#ifndef CSGeometry_H__
#define CSGeometry_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/CSGObject.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGeometry : public GeometryDefinition
{
protected:

  ListOfCSGObjects	mCSGObjects;

public:

  /**
   * Creates a new CSGeometry with the given level, version, and package version.
   */
   CSGeometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGeometry with the given SpatialPkgNamespaces object.
   */
   CSGeometry(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   CSGeometry(const CSGeometry& source);


  /**
   * Assignment operator.
   */
   CSGeometry& operator=(const CSGeometry& source);


  /**
   * Destructor.
   */ 
  virtual ~CSGeometry ();

  /**
   * Adds a copy of the given CSGObject object to this CSGeometry.
   *
   * @param d the CSGObject to add
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
   * one contained in this CSGeometry.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the CSGeometry</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see CSGeometry::createCSGObject()
   * for a method that does not lead to these issues.
   *
   * @see createCSGObject()
   */
  int addCSGObject (const CSGObject* csgo);

 /**
   * Creates a new CSGObject inside this CSGeometry and returns it.
   *
   * @return the CSGObject object created
   *
   * @see addCSGobject(const CSGObject* d)
   */
  CSGObject* createCSGObject ();

  /**
   * Get the ListOfCSGObjects object in this CSGeometry.
   * 
   * @return the list of CSGObject for this CSGeometry.
   */
  const ListOfCSGObjects* getListOfCSGObjects () const;

  /**
   * Get the ListOfCSGObjects object in this CSGeometry.
   * 
   * @return the list of CSGObject for this CSGeometry.
   */
  ListOfCSGObjects* getListOfCSGObjects ();

  /**
   * Get the nth CSGObject object in this CSGeometry.
   * 
   * @return the nth CSGObject of this CSGeometry.
   */
  const CSGObject* getCSGObject (unsigned int n) const;


  /**
   * Get the nth CSGObject object in this CSGeometry.
   * 
   * @return the nth CSGObject of this CSGeometry.
   */
  CSGObject* getCSGObject (unsigned int n);


  /**
   * Get a CSGObject object based on its identifier.
   * 
   * @return the CSGObject in this CSGeometry with the identifier
   * @p sid or NULL if no such CSGObject exists.
   */
  const CSGObject* getCSGObject (const std::string& sid) const;


  /**
   * Get a CSGObject object based on its identifier.
   * 
   * @return the CSGObject in this CSGeometry with the identifier
   * @p sid or NULL if no such CSGObject exists.
   */
  CSGObject* getCSGObject (const std::string& sid);

  /**
   * Get the number of CSGObject objects in this CSGeometry.
   * 
   * @return the number of CSGObject in this CSGeometry.
   */
  unsigned int getNumCSGObjects () const;

 /**
   * Removes the nth CSGObject object from this CSGeometry object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the CSGObject object to remove
   *
   * @return the CSGObject object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  CSGObject* removeCSGObject (unsigned int n);


  /**
   * Removes the CSGObject object with the given identifier from this CSGeometry 
   * object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   * If none of the CSGObject objects in this CSGeometry object have the identifier 
   * @p sid, then @c NULL is returned.
   *
   * @param sid the identifier of the CSGObject object to remove
   *
   * @return the CSGObject object removed.  As mentioned above, the 
   * caller owns the returned object. NULL is returned if no CSGObject
   * object with the identifier exists in this CSGeometry object.
   */
  CSGObject* removeCSGObject (const std::string& sid);

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.

  virtual const std::string& getElementName () const ;
   */

  /**
   * @return a (deep) copy of this CSGeometry.
   */
  virtual CSGeometry* clone () const;

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
  void writeElements (XMLOutputStream& stream) const;
 

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygen-libsbml-internal */
    

   /** @cond doxygen-libsbml-internal */
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
  /** @endcond doxygen-libsbml-internal */


protected:
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  SBase*
  createObject (XMLInputStream& stream);

  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  void addExpectedAttributes(ExpectedAttributes& attributes){
  };


  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  void readAttributes (const XMLAttributes& attributes, 
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


#ifndef SWIG
/*template<>
struct IdEq<CSGeometry> : public std::unary_function<SBase*, bool>
{
  const std::string& coordSystem;

  IdEq (const std::string& coordSystem) : id(coordSystem) { }
  bool operator() (SBase* sb) 
       { return static_cast <CSGeometry*> (sb)->getSpatialId() == coordSystem; }
};
*/
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
int
CSGeometry_addCSGObject (CSGeometry_t *csg, const CSGObject_t *csgo);


LIBSBML_EXTERN
CSGObject_t *
CSGeometry_createCSGObject (CSGeometry_t *csg);


LIBSBML_EXTERN
ListOf_t *
CSGeometry_getListOfCSGObjects (CSGeometry_t *csg);


LIBSBML_EXTERN
CSGeometry_t *
CSGeometry_clone (const CSGeometry_t *csg);


LIBSBML_EXTERN
CSGObject_t *
CSGeometry_getCSGObject (CSGeometry_t *csg, unsigned int n);


LIBSBML_EXTERN
CSGObject_t *
CSGeometry_getCSGObjectById (CSGeometry_t *csg, const char *sid);


LIBSBML_EXTERN
unsigned int
CSGeometry_getNumCSGObjects (const CSGeometry_t *csg);


LIBSBML_EXTERN
CSGObject_t*
CSGeometry_removeCSGObject (CSGeometry_t *csg, unsigned int n);


LIBSBML_EXTERN
CSGObject_t*
CSGeometry_removeCSGObjectById (CSGeometry_t *csg, const char* sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CSGeometry_H__ */
