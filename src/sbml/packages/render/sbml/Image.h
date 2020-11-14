/**
 * @file    Image.h
 * @brief Definition of the Image class.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2011-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright 2010 Ralph Gauges
 *     Group for the modeling of biological processes 
 *     University of Heidelberg
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class Image
 * @sbmlbrief{render} Representation of images.
 *
 * The image class represents a bitmap image representation.  It is derived
 * from Transformation2D and inherits all its attributes.
 *
 * There is an attribute that can be used to specify a file URL where that
 * specifies where the image data can be found. If the URL is a relative
 * path, it is considered to be relative to the document that contains the
 * render extension info.  The path should be the location of a JPEG or PNG
 * image, other image formats are currently not supported by the SBML
 * Level&nbsp;3 Render package.
 *
 * Additionally it provides an id attribute as well as attributes that
 * determine the dimensions and the position of the image relative to its
 * viewport.
 */

#ifndef Image_H__
#define Image_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/Transformation2D.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RelAbsVector.h>
#include <sbml/xml/XMLNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Image : public Transformation2D
{
protected:

  /** @cond doxygenLibsbmlInternal */

  RelAbsVector mX;
  RelAbsVector mY;
  RelAbsVector mZ;
  RelAbsVector mWidth;
  RelAbsVector mHeight;
  std::string mHref;

  /** @endcond */

public:

  /**
   * Creates a new Image using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Image.
   *
   * @param version an unsigned int, the SBML Version to assign to this Image.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this Image.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Image(unsigned int level = RenderExtension::getDefaultLevel(),
        unsigned int version = RenderExtension::getDefaultVersion(),
        unsigned int pkgVersion = RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new Image using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Image(RenderPkgNamespaces *renderns);


  /**
   * Creates a new Image object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * Image object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the Image
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  Image(const XMLNode& node, unsigned int l2version=4);




#ifndef OMIT_DEPRECATED
  /**
   * Instantiates an Image object with the given @p id.
   * The image reference is unset, the position and the dimensions
   * values of the image are set to 0.
   *
   * For the image to be valid, the reference has to be set and it has to 
   * have dimensions different from and larger than 0.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Image(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED

  /**
   * Copy constructor for Image.
   *
   * @param orig the Image instance to copy.
   */
  Image(const Image& orig);


  /**
   * Assignment operator for Image.
   *
   * @param rhs the Image object whose values are to be used as the basis of
   * the assignment.
   */
  Image& operator=(const Image& rhs);


  /**
   * Creates and returns a deep copy of this Image object.
   *
   * @return a (deep) copy of this Image object.
   */
  virtual Image* clone() const;


  /**
   * Destructor for Image.
   */
  virtual ~Image();


  /**
   * Returns the value of the "id" attribute of this Image.
   *
   * @return the value of the "id" attribute of this Image as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "href" attribute of this Image.
   *
   * @return the value of the "href" attribute of this Image as a string.
   */
  const std::string& getHref() const;

  /**
   * Returns the image reference URL string.
   *
   * @return The path to the image data as a string.
   */
  const std::string& getImageReference() const;

  /**
   * Predicate returning @c true if this Image's "id" attribute is set.
   *
   * @return @c true if this Image's "id" attribute has been set, otherwise
   * @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this Image's "href" attribute is set.
   *
   * @return @c true if this Image's "href" attribute has been set, otherwise
   * @c false is returned.
   */
  bool isSetHref() const;


  /**
   * Returns @c true if the image reference has been set.
   * The image reference is considered set if the string does not
   * only contain whitespace characters.
   *
   * @return @c true if the image reference has been set.
   */
  bool isSetImageReference() const;

  /**
   * Sets the value of the "id" attribute of this Image.
   *
   * @param id std::string& value of the "id" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * Calling this function with @p id = @c NULL or an empty string is
   * equivalent to calling unsetId().
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "href" attribute of this Image.
   *
   * @param href std::string& value of the "href" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p href = @c NULL or an empty string is
   * equivalent to calling unsetHref().
   */
  int setHref(const std::string& href);



  /**
   * Sets the reference to the image location.
   * Relative paths are relative to the document that contains the render information.
   * The path should be the location to a JPEG or PNG bitmap image; other formats are
   * currently not supported.
   *
   * @param ref A URL string that specifies where the image is located on the disk.
   */
  int setImageReference(const std::string& ref);

  /**
   * Unsets the value of the "id" attribute of this Image.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "href" attribute of this Image.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetHref();


  /**
   * Unsets the reference to the image location.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetImageReference();


  /**
   * Returns a const reference to the x coordinate of the image position.
   *
   * @return const reference to the x coordinate of the image position.
   */
  const RelAbsVector& getX() const;


  /**
   * Returns a reference to the x coordinate of the image position.
   *
   * @return reference to the x coordinate of the image position.
   */
  RelAbsVector& getX();


  /**
   * Returns a const reference to the y coordinate of the image position.
   *
   * @return const reference to the y coordinate of the image position.
   */
  const RelAbsVector& getY() const;


  /**
   * Returns a reference to the y coordinate of the image position.
   *
   * @return reference to the y coordinate of the image position.
   */
  RelAbsVector& getY();


  /**
   * Returns a const reference to the z coordinate of the image position.
   *
   * @return const reference to the z coordinate of the image position.
   */
  const RelAbsVector& getZ() const;


  /**
   * Returns a reference to the z coordinate of the image position.
   *
   * @return reference to the z coordinate of the image position.
   */
  RelAbsVector& getZ();


  /**
   * Returns a const reference to the width of the image.
   *
   * @return const reference to the width
   */
  const RelAbsVector& getWidth() const;


  /**
   * Returns a reference to the width of the image.
   *
   * @return reference to the width
   */
  RelAbsVector& getWidth();


  /**
   * Returns a const reference to the height of the image.
   *
   * @return const reference to the height
   */
  const RelAbsVector& getHeight() const;


  /**
   * Returns a reference to the height of the image.
   *
   * @return reference to the height
   */
  RelAbsVector& getHeight();


  /**
   * Predicate returning @c true if this Image's "x" element is set.
   *
   * @return @c true if this Image's "x" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetX() const;


  /**
   * Predicate returning @c true if this Image's "y" element is set.
   *
   * @return @c true if this Image's "y" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetY() const;


  /**
   * Predicate returning @c true if this Image's "z" element is set.
   *
   * @return @c true if this Image's "z" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetZ() const;


  /**
   * Predicate returning @c true if this Image's "width" element is set.
   *
   * @return @c true if this Image's "width" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetWidth() const;


  /**
   * Predicate returning @c true if this Image's "height" element is set.
   *
   * @return @c true if this Image's "height" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetHeight() const;


  /**
   * Sets the position of the image relative to its viewport.
   * The position can either be specified in relative or in absolute coordinates
   * or a combination of both.
   * The z coordinate can be omitted. In that case it is set to 0.
   *
   * @param x x coordinate of the image position
   * @param y y coordinate of the image position
   * @param z z coordinate of the image position
   */
  void setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z=RelAbsVector(0.0,0.0));


  /**
   * Sets the dimensions of the image.
   * The dimensions can be set as relative values or absolute values, or 
   * a combination of both.
   *
   * @param width the width of the image when rendered
   * @param height the height of the image when rendered
   */
  void setDimensions(const RelAbsVector& width,const RelAbsVector& height);


  /**
   * Sets the x coordinate of the image position.
   * The position can either be specified in relative or in absolute coordinates
   * or a combination of both.
   *
   * @param coord x-coordinate of the image position
   */
  int setX(const RelAbsVector& coord);


  /**
   * Sets the y coordinate of the image position.
   * The position can either be specified in relative or in absolute coordinates
   * or a combination of both.
   *
   * @param coord y-coordinate of the image position
   */
  int setY(const RelAbsVector& coord);


  /**
   * Sets the z coordinate of the image position.
   * The position can either be specified in relative or in absolute coordinates
   * or a combination of both.
   *
   * @param coord z-coordinate of the image position
   */
  int setZ(const RelAbsVector& coord);


  /**
   * Sets the width of the image when rendered.
   * The width can be set as relative values or absolute values, or 
   * a combination of both.
   *
   * @param width the width of the image when rendered
   */
  int setWidth(const RelAbsVector& width);


  /**
   * Sets the height of the image when rendered.
   * The height can be set as relative values or absolute values, or 
   * a combination of both.
   *
   * @param height the height of the image when rendered
   */
  int setHeight(const RelAbsVector& height);


  /**
   * Unsets the value of the "x" element of this Image.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetX();


  /**
   * Unsets the value of the "y" element of this Image.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetY();


  /**
   * Unsets the value of the "z" element of this Image.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetZ();


  /**
   * Unsets the value of the "width" element of this Image.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetWidth();


  /**
   * Unsets the value of the "height" element of this Image.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetHeight();


  /**
   * Returns the XML element name of this Image object.
   *
   * For Image, the XML element name is always @c "image".
   *
   * @return the name of this element, i.e. @c "image".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Image object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_IMAGE, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this Image
   * object have been set.
   *
   * @return @c true to indicate that all the required attributes of this Image
   * have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the Image object are:
   * @li "x"
   * @li "y"
   * @li "width"
   * @li "height"
   * @li "href"
   */
  virtual bool hasRequiredAttributes() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /**
   * Creates an XMLNode object from this Image object.
   *
   * @return the XMLNode with the XML representation for the 
   * Image object.
   */
  virtual XMLNode toXML() const;

protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds the expected attributes for this element
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */


};

LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new Image_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Image_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this Image_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Image_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
Image_t *
Image_create(unsigned int level,
             unsigned int version,
             unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Image_t object.
 *
 * @param i the Image_t structure.
 *
 * @return a (deep) copy of this Image_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
Image_t*
Image_clone(const Image_t* i);


/**
 * Frees this Image_t object.
 *
 * @param i the Image_t structure.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
void
Image_free(Image_t* i);


/**
 * Returns the value of the "id" attribute of this Image_t.
 *
 * @param i the Image_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this Image_t as a pointer to a
 * string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
char *
Image_getId(const Image_t * i);


/**
 * Returns the value of the "href" attribute of this Image_t.
 *
 * @param i the Image_t structure whose href is sought.
 *
 * @return the value of the "href" attribute of this Image_t as a pointer to a
 * string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
char *
Image_getHref(const Image_t * i);


/**
 * Predicate returning @c 1 (true) if this Image_t's "id" attribute is set.
 *
 * @param i the Image_t structure.
 *
 * @return @c 1 (true) if this Image_t's "id" attribute has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_isSetId(const Image_t * i);


/**
 * Predicate returning @c 1 (true) if this Image_t's "href" attribute is set.
 *
 * @param i the Image_t structure.
 *
 * @return @c 1 (true) if this Image_t's "href" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_isSetHref(const Image_t * i);


/**
 * Sets the value of the "id" attribute of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling Image_unsetId().
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_setId(Image_t * i, const char * id);


/**
 * Sets the value of the "href" attribute of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @param href const char * value of the "href" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p href = @c NULL or an empty string is
 * equivalent to calling Image_unsetHref().
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_setHref(Image_t * i, const char * href);


/**
 * Unsets the value of the "id" attribute of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_unsetId(Image_t * i);


/**
 * Unsets the value of the "href" attribute of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_unsetHref(Image_t * i);


/**
 * Returns the value of the "x" element of this Image_t.
 *
 * @param i the Image_t structure whose x is sought.
 *
 * @return the value of the "x" element of this Image_t as a RelAbsVector_t.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getX(const Image_t * i);


/**
 * Returns the value of the "y" element of this Image_t.
 *
 * @param i the Image_t structure whose y is sought.
 *
 * @return the value of the "y" element of this Image_t as a RelAbsVector_t.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getY(const Image_t * i);


/**
 * Returns the value of the "z" element of this Image_t.
 *
 * @param i the Image_t structure whose z is sought.
 *
 * @return the value of the "z" element of this Image_t as a RelAbsVector_t.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getZ(const Image_t * i);


/**
 * Returns the value of the "width" element of this Image_t.
 *
 * @param i the Image_t structure whose width is sought.
 *
 * @return the value of the "width" element of this Image_t as a RelAbsVector_t.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getWidth(const Image_t * i);


/**
 * Returns the value of the "height" element of this Image_t.
 *
 * @param i the Image_t structure whose height is sought.
 *
 * @return the value of the "height" element of this Image_t as a
 * RelAbsVector_t.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Image_getHeight(const Image_t * i);


/**
 * Predicate returning @c 1 (true) if this Image_t's "x" element is set.
 *
 * @param i the Image_t structure.
 *
 * @return @c 1 (true) if this Image_t's "x" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_isSetX(const Image_t * i);


/**
 * Predicate returning @c 1 (true) if this Image_t's "y" element is set.
 *
 * @param i the Image_t structure.
 *
 * @return @c 1 (true) if this Image_t's "y" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_isSetY(const Image_t * i);


/**
 * Predicate returning @c 1 (true) if this Image_t's "z" element is set.
 *
 * @param i the Image_t structure.
 *
 * @return @c 1 (true) if this Image_t's "z" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_isSetZ(const Image_t * i);


/**
 * Predicate returning @c 1 (true) if this Image_t's "width" element is set.
 *
 * @param i the Image_t structure.
 *
 * @return @c 1 (true) if this Image_t's "width" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_isSetWidth(const Image_t * i);


/**
 * Predicate returning @c 1 (true) if this Image_t's "height" element is set.
 *
 * @param i the Image_t structure.
 *
 * @return @c 1 (true) if this Image_t's "height" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_isSetHeight(const Image_t * i);


/**
 * Sets the value of the "x" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @param x RelAbsVector_t value of the "x" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_setX(Image_t * i, const RelAbsVector_t* x);


/**
 * Sets the value of the "y" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @param y RelAbsVector_t value of the "y" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_setY(Image_t * i, const RelAbsVector_t* y);


/**
 * Sets the value of the "z" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @param z RelAbsVector_t value of the "z" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_setZ(Image_t * i, const RelAbsVector_t* z);


/**
 * Sets the value of the "width" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @param width RelAbsVector_t value of the "width" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_setWidth(Image_t * i, const RelAbsVector_t* width);


/**
 * Sets the value of the "height" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @param height RelAbsVector_t value of the "height" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_setHeight(Image_t * i, const RelAbsVector_t* height);


/**
 * Unsets the value of the "x" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_unsetX(Image_t * i);


/**
 * Unsets the value of the "y" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_unsetY(Image_t * i);


/**
 * Unsets the value of the "z" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_unsetZ(Image_t * i);


/**
 * Unsets the value of the "width" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_unsetWidth(Image_t * i);


/**
 * Unsets the value of the "height" element of this Image_t.
 *
 * @param i the Image_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_unsetHeight(Image_t * i);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Image_t object have been set.
 *
 * @param i the Image_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Image_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the Image_t object are:
 * @li "href"
 * @li "x"
 * @li "y"
 * @li "width"
 * @li "height"
 *
 * @memberof Image_t
 */
LIBSBML_EXTERN
int
Image_hasRequiredAttributes(const Image_t * i);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Image_H__ */


