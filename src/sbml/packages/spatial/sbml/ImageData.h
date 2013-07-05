/*
 * @file    ImageData.h
 * @brief   Definition of ImageData, of spatial package.
 * @author  
 *
 * $Id: ImageData.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/ImageData.h $
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


#ifndef ImageData_H__
#define ImageData_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ImageData : public SBase
{
protected:

  std::string mDataType;
  int* mSamples;
  unsigned int mSamplesLength;
  
  std::string mURI;
  SBase* mParentSBMLObject;
  SBMLNamespaces* mSBMLNamespaces;

  bool  mIsSetSamples;

public:

  /**
   * Creates a new ImageData with the given level, version, and package version.
   */
   ImageData(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ImageData with the given SpatialPkgNamespaces object.
   */
   ImageData(SpatialPkgNamespaces* spatialns);

  /**
   * Copy constructor.
   */
   ImageData(const ImageData& source);


  /**
   * Assignment operator.
   */
   ImageData& operator=(const ImageData& source);


  /**
   * Destructor.
   */ 
  virtual ~ImageData ();

  /**
   * Returns the string of the "dataType" attribute of this ImageData.
   *
   * @return the string of the "dataType" attribute of this ImageData.
   */
  virtual const std::string& getDataType () const;


 /**
   * The "samples" attribute of this ImageData is returned in an int array (pointer) 
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * @return void.
   */
  void getSamples(int* outputSamples) const;

 /**
   * Returns the "samplesLength" attribute of this ImageData.
   *
   * @return the "samplesLength" attribute of this ImageData.
   */
  unsigned int getSamplesLength() const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * ImageData's "dataType" attribute has been set.
   *
   * @return @c true if this ImageData's "dataType" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetDataType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * ImageData's "samples" attribute has been set.
   *
   * @return @c true if this ImageData's "samples" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSamples () const;
  
  /**
   * Sets the SIdRef string of the "dataType" attribute of this ImageData.
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
   * Sets the SIdRef string of the "samples" attribute of this ImageData.
   *
   * @param samples an integer array  to be set.
   * @param samplesLength : length of integer array to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSamples (int* samples, int samplesLength);

 /**
   * Unsets the value of the "dataType" attribute of this ImageData.
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
   * Unsets the value of the "samples" attribute of this ImageData.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSamples ();

  /**
   * Creates a copy of this imageData and all its children.
   * 
   * @return a copy of this ImageData and all its children.  The caller owns
   * the returned ImageData and is reponsible for deleting it.
   */
  ImageData* deepCopy () const;

    /**
   * Accepts the given SBMLVisitor for this SBase object.
   *
   * @param v the SBMLVisitor instance to be used
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this SBase object.
   * 
   * @return a (deep) copy of this SBase object.
   */
  virtual SBase* clone () const;

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

/**
 * Reads the imageData from the given XMLInputStream, 
 */
  static ImageData* readImageData (XMLInputStream& stream);


  /** @cond doxygen-libsbml-internal */
  /**
   * Write imageData to XMLOutputStream
   *
   */
 static void writeImageData (ImageData* imageData, XMLOutputStream& stream);

  /** @cond doxygen-libsbml-internal */

  /**
   * Sets the parent SBML object.
   * 
   * @param sb the parent SBML object of this ASTNode.
   */
  void setParentSBMLObject(SBase * sb);

  /** @endcond doxygen-libsbml-internal */

  /**
   * Returns the parent SBML object.
   * 
   * @return the parent SBML object of this ASTNode.
   */
  SBase * getParentSBMLObject() const;

  /**  
   *  Returns the data of this image as uncompressed array of integers
   * 
   * @param data the output array of integers (it will be allocated using
   *             malloc and will have to be freed using free)
   * @param length the output lenght of the array
   *
   */
  void getUncompressedData(int* &data, int& length) const;

  protected:

    static void uncompress_data(void *data, size_t length, int*& result, int& outLength);


  
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
const char *
ImageData_getDataType (const ImageData_t *id);


LIBSBML_EXTERN
void
ImageData_getSamples (const ImageData_t *id, int* outputSamples);


LIBSBML_EXTERN
unsigned int
ImageData_getSamplesLength (const ImageData_t *id);


LIBSBML_EXTERN
int
ImageData_isSetDataType (const ImageData_t *id);


LIBSBML_EXTERN
int
ImageData_isSetSamples (const ImageData_t *id);


LIBSBML_EXTERN
int
ImageData_setDataType (ImageData_t *id, const char *dt);


LIBSBML_EXTERN
int
ImageData_setSamples (ImageData_t *id, int* value, int length);


LIBSBML_EXTERN
int
ImageData_unsetDataType (ImageData_t *id);


LIBSBML_EXTERN
int
ImageData_unsetSamples (ImageData_t *id);


LIBSBML_EXTERN
ImageData_t *
ImageData_deepCopy (const ImageData_t *id);


LIBSBML_EXTERN
SBase_t * 
ImageData_getParentSBMLObject(ImageData_t* id);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* ImageData_H__ */
