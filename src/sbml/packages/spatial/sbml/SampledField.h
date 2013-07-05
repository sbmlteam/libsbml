/*
 * @file    SampledField.h
 * @brief   Definition of SampledField, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SampledField.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SampledField.h $
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


#ifndef SampledField_H__
#define SampledField_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SampledField : public SBase
{
protected:

  std::string mSpatialId;
  std::string mDataType;
  std::string mInterpolationType;
  std::string mEncoding;

  ImageData* mImageData;
  
  long mNumSamples1;
  long mNumSamples2;
  long mNumSamples3;

  bool  mIsSetNumSamples1;
  bool  mIsSetNumSamples2;
  bool  mIsSetNumSamples3;

public:

  /**
   * Creates a new SampledField with the given level, version, and package version.
   */
   SampledField(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SampledField with the given SpatialPkgNamespaces object.
   */
   SampledField(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   SampledField(const SampledField& source);


  /**
   * Assignment operator.
   */
   SampledField& operator=(const SampledField& source);


  /**
   * Destructor.
   */ 
  virtual ~SampledField ();

  /**
   * Returns the string of the "spatialId" attribute of this SampledField.
   *
   * @return the string of the "spatialId" attribute of this SampledField.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Returns the string of the "dataType" attribute of this SampledField.
   *
   * @return the string of the "dataType" attribute of this SampledField.
   */
  virtual const std::string& getDataType () const;

  /**
   * Returns the string of the "interpolationType" attribute of this SampledField.
   *
   * @return the string of the "interpolationType" attribute of this SampledField.
   */
  virtual const std::string& getInterpolationType () const;

  /**
   * Returns the string of the "encoding" attribute of this SampledField.
   *
   * @return the string of the "encoding" attribute of this SampledField.
   */
  virtual const std::string& getEncoding () const;

 /**
   * Returns the string of the "numSamples1" attribute of this SampledField.
   *
   * @return the string of the "numSamples1" attribute of this SampledField.
   */
  long getNumSamples1() const;

 /**
   * Returns the string of the "numSamples2" attribute of this SampledField.
   *
   * @return the string of the "numSamples2" attribute of this SampledField.
   */
  long getNumSamples2() const;

 /**
   * Returns the string of the "numSamples3" attribute of this SampledField.
   *
   * @return the string of the "numSamples3" attribute of this SampledField.
   */
  long getNumSamples3() const;

 /**
   * Returns the "imageData" attribute of this SampledField.
   *
   * @return the "imageData" attribute of this SampledField.
   */
  const ImageData* getImageData() const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "spatialId" attribute has been set.
   *
   * @return @c true if this SampledField's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "dataType" attribute has been set.
   *
   * @return @c true if this SampledField's "dataType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDataType () const;

    /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "interpolationType" attribute has been set.
   *
   * @return @c true if this SampledField's "interpolationType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetInterpolationType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "encoding" attribute has been set.
   *
   * @return @c true if this SampledField's "encoding" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetEncoding () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "numSamples1" attribute has been set.
   *
   * @return @c true if this SampledField's "numSamples1" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetNumSamples1 () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "numSamples2" attribute has been set.
   *
   * @return @c true if this SampledField's "numSamples2" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetNumSamples2 () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "numSamples3" attribute has been set.
   *
   * @return @c true if this SampledField's "numSamples3" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetNumSamples3 () const;
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "imageData" attribute has been set.
   *
   * @return @c true if this SampledField's "imageData" attribute has been set, 
   * otherwise @c false is returned.
   */
  bool isSetImageData () const;
  
  /**
   * Sets the SIdRef string of the "spatialId" attribute of this SampledField.
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
   * Sets the SIdRef string of the "dataType" attribute of this SampledField.
   *
   * @param dataType a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDataType (const std::string& dataType);

  /**
   * Sets the SIdRef string of the "interpolationType" attribute of this SampledField.
   *
   * @param interpolationType a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setInterpolationType (const std::string& interpolationType);

  /**
   * Sets the SIdRef string of the "encoding" attribute of this SampledField.
   *
   * @param encoding a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setEncoding (const std::string& encoding);

  /**
   * Sets the SIdRef string of the "numSamples1" attribute of this SampledField.
   *
   * @param numSamples1 a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setNumSamples1 (long numSamples1);

  /**
   * Sets the SIdRef string of the "numSamples2" attribute of this SampledField.
   *
   * @param numSamples2 a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setNumSamples2 (long numSamples2);

  /**
   * Sets the SIdRef string of the "numSamples3" attribute of this SampledField.
   *
   * @param numSamples3 a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setNumSamples3 (long numSamples3);

  /**
   * Sets the SIdRef string of the "imageData" attribute of this SampledField.
   *
   * @param imageData 
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int setImageData (const ImageData* imageData);

  /**
   * Unsets the value of the "id" attribute of this SampledField.
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
   * Unsets the value of the "dataType" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDataType ();

  /**
   * Unsets the value of the "interpolationType" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetInterpolationType ();

  /**
   * Unsets the value of the "encoding" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetEncoding ();

  /**
   * Unsets the value of the "numSamples1" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetNumSamples1 ();

  /**
   * Unsets the value of the "numSamples2" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetNumSamples2 ();

  /**
   * Unsets the value of the "numSamples3" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetNumSamples3 ();

  /**
   * Creates a new ImageData object, installs it as this SampledField's 
   * "imageData" subelement, and returns it.
   *
   * If this SampledField had a previous ImageData, it will be destroyed.
   *
   * @return the new ImageData object
   */
  ImageData* createImageData ();


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this SampledField.
   */
  virtual SampledField* clone () const;


  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;


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
   * Predicate returning @c true or @c false depending on whether
   * all the required elements for this SampledField object
   * have been set.
   *
   * @note The required elements for a SampledField object are:
   * imageData
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
 * Used by ListOfSampledFields::get() to lookup an SBase based by its 
 * componentType
 */
#ifndef SWIG
template<>
struct IdEq<SampledField> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <SampledField*> (sb)->getSpatialId() == id; }
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
const char *
SampledField_getSpatialId (const SampledField_t *sf);


LIBSBML_EXTERN
const char *
SampledField_getDataType (const SampledField_t *sf);


LIBSBML_EXTERN
const char *
SampledField_getInterpolationType (const SampledField_t *sf);


LIBSBML_EXTERN
const char *
SampledField_getEncoding (const SampledField_t *sf);


LIBSBML_EXTERN
long
SampledField_getNumSamples1 (const SampledField_t *sf);


LIBSBML_EXTERN
long
SampledField_getNumSamples2 (const SampledField_t *sf);


LIBSBML_EXTERN
long
SampledField_getNumSamples3 (const SampledField_t *sf);


LIBSBML_EXTERN
const ImageData_t *
SampledField_getImageData (const SampledField_t *sf);


LIBSBML_EXTERN
long
SampledField_getNumSamples3 (const SampledField_t *sf);


LIBSBML_EXTERN
SampledField_t *
SampledField_clone (const SampledField_t* sf);


LIBSBML_EXTERN
int
SampledField_isSetSpatialId (const SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_isSetDataType (const SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_isSetInterpolationType (const SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_isSetEncoding (const SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_isSetNumSamples1 (const SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_isSetNumSamples2 (const SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_isSetNumSamples3 (const SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_isSetImageData (const SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_setSpatialId (SampledField_t *sf, const char *sid);


LIBSBML_EXTERN
int
SampledField_setDataType (SampledField_t *sf, const char *sid);


LIBSBML_EXTERN
int
SampledField_setInterpolationType (SampledField_t *sf, const char *sid);


LIBSBML_EXTERN
int
SampledField_setEncoding (SampledField_t *sf, const char *sid);


LIBSBML_EXTERN
int
SampledField_setNumSamples1 (SampledField_t *sf, long value);


LIBSBML_EXTERN
int
SampledField_setNumSamples2 (SampledField_t *sf, long value);


LIBSBML_EXTERN
int
SampledField_setNumSamples3 (SampledField_t *sf, long value);


LIBSBML_EXTERN
int
SampledField_setImageData (SampledField_t *sf, const ImageData_t *imageData);


LIBSBML_EXTERN
int
SampledField_unsetSpatialId (SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_unsetDataType (SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_unsetInterpolationType (SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_unsetEncoding (SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_unsetNumSamples1 (SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_unsetNumSamples2 (SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_unsetNumSamples3 (SampledField_t *sf);


LIBSBML_EXTERN
int
SampledField_unsetSamples (SampledField_t *sf);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* SampledField_H__ */
