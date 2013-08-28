/*
 * @file    AnalyticGeometry.h
 * @brief   Definition of AnalyticGeometry, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: AnalyticGeometry.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/AnalyticGeometry.h $
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


#ifndef AnalyticGeometry_H__
#define AnalyticGeometry_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/AnalyticVolume.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN AnalyticGeometry : public GeometryDefinition
{
protected:

  ListOfAnalyticVolumes			mAnalyticVolumes;

public:

  /**
   * Creates a new AnalyticGeometry with the given level, version, and package version.
   */
   AnalyticGeometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AnalyticGeometry with the given SpatialPkgNamespaces object.
   */
   AnalyticGeometry(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   AnalyticGeometry(const AnalyticGeometry& source);


  /**
   * Assignment operator.
   */
   AnalyticGeometry& operator=(const AnalyticGeometry& source);


  /**
   * Destructor.
   */ 
  virtual ~AnalyticGeometry ();

  /**
   * Adds a copy of the given AnalyticVolume object to this AnalyticGeometry.
   *
   * @param d the AnalyticVolume to add
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
   * one contained in this AnalyticGeometry.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the AnalyticGeometry</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see AnalyticGeometry::createAnalyticVolume()
   * for a method that does not lead to these issues.
   *
   * @see createAnalyticVolume()
   */
  int addAnalyticVolume (const AnalyticVolume* ad);


  /**
   * Creates a new AnalyticVolume object inside this AnalyticGeometry and returns it.
   *
   * @return the AnalyticVolume object created
   *
   * @see addAnalyticVolume(const AnalyticVolume* d)
   */
  AnalyticVolume* createAnalyticVolume ();

 
  /**
   * Get the ListOfAnalyticVolumes object in this AnalyticGeometry.
   * 
   * @return the list of AnalyticVolume for this AnalyticGeometry.
   */
  const ListOfAnalyticVolumes* getListOfAnalyticVolumes () const;


  /**
   * Get the ListOfAnalyticVolumes object in this AnalyticGeometry.
   * 
   * @return the list of AnalyticVolume for this AnalyticGeometry.
   */
  ListOfAnalyticVolumes* getListOfAnalyticVolumes ();


  /**
   * Get the nth AnalyticVolume object in this AnalyticGeometry.
   * 
   * @return the nth AnalyticVolume of this AnalyticGeometry.
   */
  const AnalyticVolume* getAnalyticVolume (unsigned int n) const;


  /**
   * Get the nth AnalyticVolume object in this AnalyticGeometry.
   * 
   * @return the nth AnalyticVolume of this AnalyticGeometry.
   */
  AnalyticVolume* getAnalyticVolume (unsigned int n);


  /**
   * Get a AnalyticVolume object based on its identifier.
   * 
   * @return the AnalyticVolume in this AnalyticGeometry with the identifier
   * @p sid or NULL if no such AnalyticVolume exists.
   */
  const AnalyticVolume* getAnalyticVolume (const std::string& sid) const;


  /**
   * Get a AnalyticVolume object based on its identifier.
   * 
   * @return the AnalyticVolume in this AnalyticGeometry with the identifier
   * @p sid or NULL if no such AnalyticVolume exists.
   */
  AnalyticVolume* getAnalyticVolume (const std::string& sid);


  /**
   * Get the number of AnalyticVolume objects in this AnalyticGeometry.
   * 
   * @return the number of AnalyticVolume in this AnalyticGeometry.
   */
  unsigned int getNumAnalyticVolumes () const;


 /**
   * Removes the nth AnalyticVolume object from this AnalyticGeometry object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the AnalyticVolume object to remove
   *
   * @return the AnalyticVolume object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  AnalyticVolume* removeAnalyticVolume (unsigned int n);


  /**
   * Removes the AnalyticVolume object with the given identifier from this AnalyticGeometry 
   * object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   * If none of the AnalyticVolume objects in this AnalyticGeometry object have the identifier 
   * @p sid, then @c NULL is returned.
   *
   * @param sid the identifier of the AnalyticVolume object to remove
   *
   * @return the AnalyticVolume object removed.  As mentioned above, the 
   * caller owns the returned object. NULL is returned if no AnalyticVolume
   * object with the identifier exists in this AnalyticGeometry object.
   */
  AnalyticVolume* removeAnalyticVolume (const std::string& sid);

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   *
  virtual const std::string& getElementName () const ;
   */

  /**
   * @return a (deep) copy of this AnalyticGeometry.
   */
  virtual AnalyticGeometry* clone () const;

  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   
  int getTypeCode () const;
*/

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
  void writeElements (XMLOutputStream& stream) const;
 

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygenLibsbmlInternal */
    

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

  /** @endcond doxygenLibsbmlInternal */


};


#ifndef SWIG
/*template<>
struct IdEq<AnalyticGeometry> : public std::unary_function<SBase*, bool>
{
  const std::string& coordSystem;

  IdEq (const std::string& coordSystem) : id(coordSystem) { }
  bool operator() (SBase* sb) 
       { return static_cast <AnalyticGeometry*> (sb)->getSpatialId() == coordSystem; }
};
*/
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
int
AnalyticGeometry_addAnalyticVolume (AnalyticGeometry_t *g, const AnalyticVolume_t *d);


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_createAnalyticVolume (AnalyticGeometry_t *g);


LIBSBML_EXTERN
ListOf_t *
AnalyticGeometry_getListOfAnalyticVolumes (AnalyticGeometry_t *g);


LIBSBML_EXTERN
AnalyticGeometry_t *
AnalyticGeometry_clone (const AnalyticGeometry_t *g);


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_getAnalyticVolume (AnalyticGeometry_t *g, unsigned int n);


LIBSBML_EXTERN
AnalyticVolume_t *
AnalyticGeometry_getAnalyticVolumeById (AnalyticGeometry_t *g, const char *sid);


LIBSBML_EXTERN
unsigned int
AnalyticGeometry_getNumAnalyticVolumes (const AnalyticGeometry_t *g);


LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_removeAnalyticVolume (AnalyticGeometry_t *g, unsigned int n);


LIBSBML_EXTERN
AnalyticVolume_t*
AnalyticGeometry_removeAnalyticVolumeById (AnalyticGeometry_t *g, const char* sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* AnalyticGeometry_H__ */
