/*
 * @file    SampledFieldGeometry.h
 * @brief   Definition of SampledFieldGeometry, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SampledFieldGeometry.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SampledFieldGeometry.h $
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


#ifndef SampledFieldGeometry_H__
#define SampledFieldGeometry_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/sbml/SampledField.h>
#include <sbml/packages/spatial/sbml/SampledVolume.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SampledFieldGeometry : public GeometryDefinition
{
protected:

  SampledField*			mSampledField;

  ListOfSampledVolumes	mSampledVolumes;

public:

  /**
   * Creates a new SampledFieldGeometry with the given level, version, and package version.
   */
   SampledFieldGeometry(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SampledFieldGeometry with the given SpatialPkgNamespaces object.
   */
   SampledFieldGeometry(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   SampledFieldGeometry(const SampledFieldGeometry& source);


  /**
   * Assignment operator.
   */
   SampledFieldGeometry& operator=(const SampledFieldGeometry& source);


  /**
   * Destructor.
   */ 
  virtual ~SampledFieldGeometry ();

  /**
   * Returns the "mSampledField" object of this SampledFieldGeometry.
   *
   * @return the "mSampledField" object of this SampledFieldGeometry.
   */
  SampledField* getSampledField ();

  /**
   * Returns the "mSampledField" object of this SampledFieldGeometry.
   *
   * @return the "mSampledField" object of this SampledFieldGeometry.
   */
  const SampledField* getSampledField () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledFieldGeometry's "mSampledField" object has been set.
   *
   * @return @c true if this SampledFieldGeometry's "mSampledField" object has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSampledField () const;
  
  /**
   * Sets the "mSampledField" subelement of this SampledFieldGeometry to a copy of the 
   * given SampledField object.
   *
   * @param sampledField an object to use.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setSampledField (const SampledField* sf);

 /**
   * Unsets the "mSampledField" subelement of this SampledFieldGeometry.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  int unsetSampledField ();

  /**
   * Adds a copy of the given SampledVolume object to this SampledFieldGeometry.
   *
   * @param d the SampledVolume to add
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
   * one contained in this SampledFieldGeometry.  Changes made to the original object
   * instance (such as resetting attribute values) will <em>not affect the
   * instance in the SampledFieldGeometry</em>.  In addition, the caller should make sure
   * to free the original object if it is no longer being used, or else a
   * memory leak will result.  Please see SampledFieldGeometry::createSampledVolume()
   * for a method that does not lead to these issues.
   *
   * @see createSampledVolume()
   */
  int addSampledVolume (const SampledVolume* sv);


  /**
   * Creates a new SampledVolume object inside this SampledFieldGeometry and returns it.
   *
   * @return the SampledVolume object created
   *
   * @see addSampledVolume(const SampledVolume* d)
   */
  SampledVolume* createSampledVolume ();

 
  /**
   * Creates a new SampledField object, installs it as this SampledFieldGeometry's
   * "sampledField" subelement, and returns it.
   *
   * If this SampledFieldGeometry had a previous SampledField, it will be destroyed.
   *
   * @return the new SampledField object
   */
  SampledField* createSampledField ();


  /**
   * Get the ListOfSampledVolumes object in this SampledFieldGeometry.
   * 
   * @return the list of SampledVolume for this SampledFieldGeometry.
   */
  const ListOfSampledVolumes* getListOfSampledVolumes () const;


  /**
   * Get the ListOfSampledVolumes object in this SampledFieldGeometry.
   * 
   * @return the list of SampledVolume for this SampledFieldGeometry.
   */
  ListOfSampledVolumes* getListOfSampledVolumes ();


  /**
   * Get the nth SampledVolume object in this SampledFieldGeometry.
   * 
   * @return the nth SampledVolume of this SampledFieldGeometry.
   */
  const SampledVolume* getSampledVolume (unsigned int n) const;


  /**
   * Get the nth SampledVolume object in this SampledFieldGeometry.
   * 
   * @return the nth SampledVolume of this SampledFieldGeometry.
   */
  SampledVolume* getSampledVolume (unsigned int n);


  /**
   * Get a SampledVolume object based on its identifier.
   * 
   * @return the SampledVolume in this SampledFieldGeometry with the identifier
   * @p sid or NULL if no such SampledVolume exists.
   */
  const SampledVolume* getSampledVolume (const std::string& sid) const;


  /**
   * Get a SampledVolume object based on its identifier.
   * 
   * @return the SampledVolume in this SampledFieldGeometry with the identifier
   * @p sid or NULL if no such SampledVolume exists.
   */
  SampledVolume* getSampledVolume (const std::string& sid);


  /**
   * Get the number of SampledVolume objects in this SampledFieldGeometry.
   * 
   * @return the number of SampledVolume in this SampledFieldGeometry.
   */
  unsigned int getNumSampledVolumes () const;


 /**
   * Removes the nth SampledVolume object from this SampledFieldGeometry object and 
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   *
   * @param n the index of the SampledVolume object to remove
   *
   * @return the SampledVolume object removed.  As mentioned above, 
   * the caller owns the returned item. NULL is returned if the given index 
   * is out of range.
   *
   */
  SampledVolume* removeSampledVolume (unsigned int n);


  /**
   * Removes the SampledVolume object with the given identifier from this SampledFieldGeometry 
   * object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for deleting it.
   * If none of the SampledVolume objects in this SampledFieldGeometry object have the identifier 
   * @p sid, then @c NULL is returned.
   *
   * @param sid the identifier of the SampledVolume object to remove
   *
   * @return the SampledVolume object removed.  As mentioned above, the 
   * caller owns the returned object. NULL is returned if no SampledVolume
   * object with the identifier exists in this SampledFieldGeometry object.
   */
  SampledVolume* removeSampledVolume (const std::string& sid);

  /**
   * @return a (deep) copy of this SampledFieldGeometry.
   */
  virtual SampledFieldGeometry* clone () const;

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
struct IdEq<SampledFieldGeometry> : public std::unary_function<SBase*, bool>
{
  const std::string& coordSystem;

  IdEq (const std::string& coordSystem) : id(coordSystem) { }
  bool operator() (SBase* sb) 
       { return static_cast <SampledFieldGeometry*> (sb)->getSpatialId() == coordSystem; }
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
SampledFieldGeometry_addSampledVolume (SampledFieldGeometry_t *sfg, const SampledVolume_t *sv);


LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_createSampledVolume (SampledFieldGeometry_t *sfg);


LIBSBML_EXTERN
ListOf_t *
SampledFieldGeometry_getListOfSampledVolumes (SampledFieldGeometry_t *sfg);


LIBSBML_EXTERN
SampledFieldGeometry_t *
SampledFieldGeometry_clone (const SampledFieldGeometry_t *sfg);


LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_getSampledVolume (SampledFieldGeometry_t *sfg, unsigned int n);


LIBSBML_EXTERN
SampledVolume_t *
SampledFieldGeometry_getSampledVolumeById (SampledFieldGeometry_t *sgg, const char *sid);


LIBSBML_EXTERN
unsigned int
SampledFieldGeometry_getNumSampledVolumes (const SampledFieldGeometry_t *sfg);


LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_removeSampledVolume (SampledFieldGeometry_t *sfg, unsigned int n);


LIBSBML_EXTERN
SampledVolume_t*
SampledFieldGeometry_removeSampledVolumeById (SampledFieldGeometry_t *sfg, const char* sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* SampledFieldGeometry_H__ */
