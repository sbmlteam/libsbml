/**
 * @file    Rectangle.h
 * @brief Definition of the Rectangle class.
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
 * @class Rectangle
 * @sbmlbrief{render} Representation of a rectangle.
 *
 * Rectangle allows the definition of rectangles with or without rounded
 * edges.  The Rectangle object derives from the GraphicalPrimitive2D class
 * and thus inherits any attributes and elements that are present on this
 * class.  In addition, the Rectangle object has the required attributes "x",
 * "y", "height", and "width" as well as the optional attributes "z", "rx", "ry" and
 * "ratio".
 */

#ifndef Rectangle_H__
#define Rectangle_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/GraphicalPrimitive2D.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RelAbsVector.h>
#include <sbml/xml/XMLNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN Rectangle : public GraphicalPrimitive2D
{
protected:

  /** @cond doxygenLibsbmlInternal */

  RelAbsVector mX;
  RelAbsVector mY;
  RelAbsVector mZ;
  RelAbsVector mWidth;
  RelAbsVector mHeight;
  RelAbsVector mRX;
  RelAbsVector mRY;
  static const std::string ELEMENT_NAME;
  double mRatio;
  bool mIsSetRatio;

  /** @endcond */

public:

  /**
   * Creates a new Rectangle using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Rectangle.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Rectangle.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this Rectangle.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Rectangle(unsigned int level = RenderExtension::getDefaultLevel(),
            unsigned int version = RenderExtension::getDefaultVersion(),
            unsigned int pkgVersion =
              RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new Rectangle using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Rectangle(RenderPkgNamespaces *renderns);


  /**
   * Creates a new Rectangle object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * Rectangle object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the Rectangle
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  Rectangle(const XMLNode& node, unsigned int l2version=4);



#ifndef OMIT_DEPRECATED
  /**
   * Instantiates a new Rectangle object.
   * All attributes are set as described for the default constructor
   * of GraphicalPrimitive2D.
   *
   * The id is set to the given string and all rectangle specific attributes are set to 0.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id id string for the rectangle
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Rectangle(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED


#ifndef OMIT_DEPRECATED
  /**
   * Instantiates a new Rectangle object.
   * All attributes are set as described for the default constructor
   * of GraphicalPrimitive2D.
   *
   * The id is set to the given string and all rectangle specific attributes
   * are set to the given values.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id id string for the rectangle
   * @param x x coordinate of the position 
   * @param y y coordinate of the position 
   * @param z z coordinate of the position 
   * @param w w width
   * @param h h height
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Rectangle(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z,const RelAbsVector& w,const RelAbsVector& h);
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
  /**
   * Instantiates a new Rectangle object.
   * All attributes are set as described for the default constructor
   * of GraphicalPrimitive2D.
   *
   * The id is set to the given string and all rectangle specific attributes
   * are set to the given values. The z coordinate of the position is set to 0.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id id string for the rectangle
   * @param x x coordinate of the position 
   * @param y y coordinate of the position 
   * @param w w width
   * @param h h height
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Rectangle(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& w,const RelAbsVector& h);
#endif // OMIT_DEPRECATED


  /**
   * Copy constructor for Rectangle.
   *
   * @param orig the Rectangle instance to copy.
   */
  Rectangle(const Rectangle& orig);


  /**
   * Assignment operator for Rectangle.
   *
   * @param rhs the Rectangle object whose values are to be used as the basis
   * of the assignment.
   */
  Rectangle& operator=(const Rectangle& rhs);


  /**
   * Creates and returns a deep copy of this Rectangle object.
   *
   * @return a (deep) copy of this Rectangle object.
   */
  virtual Rectangle* clone() const;


  /**
   * Destructor for Rectangle.
   */
  virtual ~Rectangle();


  /**
   * Returns the value of the "ratio" attribute of this Rectangle.
   *
   * @return the value of the "ratio" attribute of this Rectangle as a double.
   */
  double getRatio() const;


  /**
   * Predicate returning @c true if this Rectangle's "ratio" attribute is set.
   *
   * @return @c true if this Rectangle's "ratio" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetRatio() const;


  /**
   * Sets the value of the "ratio" attribute of this Rectangle.
   *
   * @param ratio double value of the "ratio" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * OperationReturnValues_t}
   */
  int setRatio(double ratio);


  /**
   * Unsets the value of the "ratio" attribute of this Rectangle.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRatio();


  /**
   * Returns the x coordinate of the rectangle's position
   *
   * @return const reference to RelAbsVector that represents the x position
   */
  const RelAbsVector& getX() const;

  /**
   * Returns the x coordinate of the rectangle's position
   *
   * @return reference to RelAbsVector that represents the x position
   */
  RelAbsVector& getX();


  /**
   * Returns the y coordinate of the rectangle's position
   *
   * @return const reference to RelAbsVector that represents the y position
   */
  const RelAbsVector& getY() const;


  /**
   * Returns the y coordinate of the rectangle's position
   *
   * @return reference to RelAbsVector that represents the y position
   */
  RelAbsVector& getY();

  /**
   * Returns the z coordinate of the rectangle's position
   *
   * @return const reference to RelAbsVector that represents the z position
   */
  const RelAbsVector& getZ() const;


  /**
   * Returns the z coordinate of the rectangle's position
   *
   * @return reference to RelAbsVector that represents the z position
   */
  RelAbsVector& getZ();

  /**
   * Returns the with of the rectangle
   *
   * @return const reference to the RelAbsVector that represents the width
   */
  const RelAbsVector& getWidth() const;


  /**
   * Returns the with of the rectangle
   *
   * @return reference to the RelAbsVector that represents the width
   */
  RelAbsVector& getWidth();

  /**
   * Returns the height of the rectangle
   *
   * @return const reference to the RelAbsVector that represents the height
   */
  const RelAbsVector& getHeight() const;

  /**
   * Returns the height of the rectangle
   *
   * @return reference to the RelAbsVector that represents the height
   */
  RelAbsVector& getHeight();


  /**
   * Returns the value of the "rX" element of this Rectangle.
   *
   * @return the value of the "rX" element of this Rectangle as a
   * RelAbsVector.
   */
  const RelAbsVector& getRX() const;


  /**
   * Returns the corner radius along the x axis
   *
   * @return const reference to the RelAbsVector that corner radius along the x axis
   */
  const RelAbsVector& getRadiusX() const;


  /**
   * Returns the value of the "rX" element of this Rectangle.
   *
   * @return the value of the "rX" element of this Rectangle as a
   * RelAbsVector.
   */
  RelAbsVector& getRX();


  /**
   * Returns the corner radius along the x axis
   *
   * @return reference to the RelAbsVector that corner radius along the x axis
   */
  RelAbsVector& getRadiusX();


  /**
  * Returns the value of the "rY" element of this Rectangle.
  *
  * @return the value of the "rY" element of this Rectangle as a
  * RelAbsVector.
  */
  const RelAbsVector& getRY() const;


  /**
   * Returns the corner radius along the y axis
   *
   * @return const reference to the RelAbsVector that corner radius along the y axis
   */
  const RelAbsVector& getRadiusY() const;

 
 /**
  * Returns the value of the "rY" element of this Rectangle.
  *
  * @return the value of the "rY" element of this Rectangle as a
  * RelAbsVector.
  */
  RelAbsVector& getRY();


  /**
   * Returns the corner radius along the y axis
   *
   * @return reference to the RelAbsVector that corner radius along the y axis
   */
  RelAbsVector& getRadiusY();

 
  /**
   * Predicate returning @c true if this Rectangle's "x" attribute is set.
   *
   * @return @c true if this Rectangle's "x" attribute has been set, otherwise
   * @c false is returned.
   */
  bool isSetX() const;


  /**
   * Predicate returning @c true if this Rectangle's "y" attribute is set.
   *
   * @return @c true if this Rectangle's "y" attribute has been set, otherwise
   * @c false is returned.
   */
  bool isSetY() const;


  /**
   * Predicate returning @c true if this Rectangle's "z" attribute is set.
   *
   * @return @c true if this Rectangle's "z" attribute has been set, otherwise
   * @c false is returned.
   */
  bool isSetZ() const;


  /**
   * Predicate returning @c true if this Rectangle's "width" attribute is set.
   *
   * @return @c true if this Rectangle's "width" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetWidth() const;


  /**
   * Predicate returning @c true if this Rectangle's "height" attribute is set.
   *
   * @return @c true if this Rectangle's "height" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetHeight() const;


  /**
   * Predicate returning @c true if this Rectangle's "rX" attribute is set.
   *
   * @return @c true if this Rectangle's "rX" attribute has been set, otherwise
   * @c false is returned.
   */
  bool isSetRX() const;


  /**
  * Predicate returning @c true if this Rectangle's "rX" attribute is set.
  *
  * @return @c true if this Rectangle's "rX" attribute has been set, otherwise
  * @c false is returned.
  */
  bool isSetRadiusX() const;


  /**
  * Predicate returning @c true if this Rectangle's "rY" attribute is set.
  *
  * @return @c true if this Rectangle's "rY" attribute has been set, otherwise
  * @c false is returned.
  */
  bool isSetRY() const;



  /**
   * Predicate returning @c true if this Rectangle's "rY" attribute is set.
   *
   * @return @c true if this Rectangle's "rY" attribute has been set, otherwise
   * @c false is returned.
   */
  bool isSetRadiusY() const;



  /**
   * Sets the position and the size of the Rectangle within the viewport.
   *
   * @param x x coordinate of the position 
   * @param y y coordinate of the position 
   * @param z z coordinate of the position 
   * @param w w width
   * @param h h height
   */
  void setCoordinatesAndSize(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z,const RelAbsVector& w,const RelAbsVector& h);

  /**
   * Sets the position of the Rectangle within the viewport.
   *
   * @param x x coordinate of the position 
   * @param y y coordinate of the position 
   * @param z z coordinate of the position 
   */
  void setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z);

  /**
   * Sets the size of the Rectangle 
   *
   * @param w w width
   * @param h h height
   */
  void setSize(const RelAbsVector& w,const RelAbsVector& h);

  /**
   * Sets the two corner radii of the rectangle
   *
   * @param rx corner radius along the x axis
   * @param ry corner radius along the y axis
   */
  void setRadii(const RelAbsVector& rx,const RelAbsVector& ry);

  /**
   * Sets the x position of the Rectangle within the viewport.
   *
   * @param x x coordinate of the position 
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setX(const RelAbsVector& x);

  /**
   * Sets the y position of the Rectangle within the viewport.
   *
   * @param y y coordinate of the position 
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setY(const RelAbsVector& y);

  /**
   * Sets the z position of the Rectangle within the viewport.
   *
   * @param z z coordinate of the position 
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setZ(const RelAbsVector& z);
  /**
   * Sets the width of the Rectangle 
   *
   * @param w w width
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setWidth(const RelAbsVector& w);

  /**
   * Sets the height of the Rectangle 
   *
   * @param h h height
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setHeight(const RelAbsVector& h);

  /**
   * Sets the corner radius along the x axis
   *
   * @param rx corner radius along the x axis
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadiusX(const RelAbsVector& rx);

  /**
  * Sets the corner radius along the x axis
  *
  * @param rx corner radius along the x axis
  *
  * @copydetails doc_returns_one_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  */
  int setRX(const RelAbsVector& rx);

  /**
   * Sets the corner radius along the y axis
   *
   * @param ry corner radius along the y axis
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadiusY(const RelAbsVector& ry);

  /**
  * Sets the corner radius along the y axis
  *
  * @param ry corner radius along the y axis
  *
  * @copydetails doc_returns_one_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  */
  int setRY(const RelAbsVector& ry);



  /**
   * Unsets the value of the "x" attribute of this Rectangle.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetX();


  /**
   * Unsets the value of the "y" attribute of this Rectangle.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetY();


  /**
   * Unsets the value of the "z" attribute of this Rectangle.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetZ();


  /**
   * Unsets the value of the "width" attribute of this Rectangle.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetWidth();


  /**
   * Unsets the value of the "height" attribute of this Rectangle.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetHeight();


  /**
   * Unsets the value of the "rX" attribute of this Rectangle.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetRadiusX();



  /**
  * Unsets the value of the "rX" attribute of this Rectangle.
  *
  * @copydetails doc_returns_one_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  */
  int unsetRX();


  /**
   * Unsets the value of the "rY" attribute of this Rectangle.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetRadiusY();


  /**
  * Unsets the value of the "rY" attribute of this Rectangle.
  *
  * @copydetails doc_returns_one_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  */
  int unsetRY();


  /**
   * Returns the XML element name of this Rectangle object.
   *
   * For Rectangle, the XML element name is always @c "rectangle".
   *
   * @return the name of this element, i.e. @c "rectangle".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Rectangle object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_RECTANGLE, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * Rectangle object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * Rectangle have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /**
   * Creates an XMLNode object from this Rectangle object.
   *
   * @return the XMLNode with the XML representation for the 
   * Rectangle object.
   */
  XMLNode toXML() const;

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
 * Creates a new Rectangle_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Rectangle_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Rectangle_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Rectangle_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
Rectangle_t *
Rectangle_create(unsigned int level,
                 unsigned int version,
                 unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Rectangle_t object.
 *
 * @param r the Rectangle_t structure.
 *
 * @return a (deep) copy of this Rectangle_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
Rectangle_t*
Rectangle_clone(const Rectangle_t* r);


/**
 * Frees this Rectangle_t object.
 *
 * @param r the Rectangle_t structure.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
void
Rectangle_free(Rectangle_t* r);


/**
 * Returns the value of the "ratio" attribute of this Rectangle_t.
 *
 * @param r the Rectangle_t structure whose ratio is sought.
 *
 * @return the value of the "ratio" attribute of this Rectangle_t as a double.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
double
Rectangle_getRatio(const Rectangle_t * r);


/**
 * Predicate returning @c 1 (true) if this Rectangle_t's "ratio" attribute is
 * set.
 *
 * @param r the Rectangle_t structure.
 *
 * @return @c 1 (true) if this Rectangle_t's "ratio" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_isSetRatio(const Rectangle_t * r);


/**
 * Sets the value of the "ratio" attribute of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @param ratio double value of the "ratio" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_setRatio(Rectangle_t * r, double ratio);


/**
 * Unsets the value of the "ratio" attribute of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_unsetRatio(Rectangle_t * r);


/**
 * Returns the value of the "x" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure whose x is sought.
 *
 * @return the value of the "x" element of this Rectangle_t as a RelAbsVector_t.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getX(const Rectangle_t * r);


/**
 * Returns the value of the "y" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure whose y is sought.
 *
 * @return the value of the "y" element of this Rectangle_t as a RelAbsVector_t.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getY(const Rectangle_t * r);


/**
 * Returns the value of the "z" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure whose z is sought.
 *
 * @return the value of the "z" element of this Rectangle_t as a RelAbsVector_t.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getZ(const Rectangle_t * r);


/**
 * Returns the value of the "width" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure whose width is sought.
 *
 * @return the value of the "width" element of this Rectangle_t as a
 * RelAbsVector_t.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getWidth(const Rectangle_t * r);


/**
 * Returns the value of the "height" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure whose height is sought.
 *
 * @return the value of the "height" element of this Rectangle_t as a
 * RelAbsVector_t.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getHeight(const Rectangle_t * r);


/**
 * Returns the value of the "rX" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure whose rX is sought.
 *
 * @return the value of the "rX" element of this Rectangle_t as a
 * RelAbsVector_t.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getRX(const Rectangle_t * r);


/**
 * Returns the value of the "rY" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure whose rY is sought.
 *
 * @return the value of the "rY" element of this Rectangle_t as a
 * RelAbsVector_t.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getRY(const Rectangle_t * r);


/**
 * Predicate returning @c 1 (true) if this Rectangle_t's "x" element is set.
 *
 * @param r the Rectangle_t structure.
 *
 * @return @c 1 (true) if this Rectangle_t's "x" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_isSetX(const Rectangle_t * r);


/**
 * Predicate returning @c 1 (true) if this Rectangle_t's "y" element is set.
 *
 * @param r the Rectangle_t structure.
 *
 * @return @c 1 (true) if this Rectangle_t's "y" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_isSetY(const Rectangle_t * r);


/**
 * Predicate returning @c 1 (true) if this Rectangle_t's "z" element is set.
 *
 * @param r the Rectangle_t structure.
 *
 * @return @c 1 (true) if this Rectangle_t's "z" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_isSetZ(const Rectangle_t * r);


/**
 * Predicate returning @c 1 (true) if this Rectangle_t's "width" element is
 * set.
 *
 * @param r the Rectangle_t structure.
 *
 * @return @c 1 (true) if this Rectangle_t's "width" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_isSetWidth(const Rectangle_t * r);


/**
 * Predicate returning @c 1 (true) if this Rectangle_t's "height" element is
 * set.
 *
 * @param r the Rectangle_t structure.
 *
 * @return @c 1 (true) if this Rectangle_t's "height" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_isSetHeight(const Rectangle_t * r);


/**
 * Predicate returning @c 1 (true) if this Rectangle_t's "rX" element is set.
 *
 * @param r the Rectangle_t structure.
 *
 * @return @c 1 (true) if this Rectangle_t's "rX" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_isSetRX(const Rectangle_t * r);


/**
 * Predicate returning @c 1 (true) if this Rectangle_t's "rY" element is set.
 *
 * @param r the Rectangle_t structure.
 *
 * @return @c 1 (true) if this Rectangle_t's "rY" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_isSetRY(const Rectangle_t * r);


/**
 * Sets the value of the "x" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @param x RelAbsVector_t value of the "x" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_setX(Rectangle_t * r, const RelAbsVector_t* x);


/**
 * Sets the value of the "y" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @param y RelAbsVector_t value of the "y" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_setY(Rectangle_t * r, const RelAbsVector_t* y);


/**
 * Sets the value of the "z" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @param z RelAbsVector_t value of the "z" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_setZ(Rectangle_t * r, const RelAbsVector_t* z);


/**
 * Sets the value of the "width" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @param width RelAbsVector_t value of the "width" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_setWidth(Rectangle_t * r, const RelAbsVector_t* width);


/**
 * Sets the value of the "height" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @param height RelAbsVector_t value of the "height" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_setHeight(Rectangle_t * r, const RelAbsVector_t* height);


/**
 * Sets the value of the "rX" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @param rX RelAbsVector_t value of the "rX" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_setRX(Rectangle_t * r, const RelAbsVector_t* rX);


/**
 * Sets the value of the "rY" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @param rY RelAbsVector_t value of the "rY" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_setRY(Rectangle_t * r, const RelAbsVector_t* rY);


/**
 * Unsets the value of the "x" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_unsetX(Rectangle_t * r);


/**
 * Unsets the value of the "y" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_unsetY(Rectangle_t * r);


/**
 * Unsets the value of the "z" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_unsetZ(Rectangle_t * r);


/**
 * Unsets the value of the "width" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_unsetWidth(Rectangle_t * r);


/**
 * Unsets the value of the "height" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_unsetHeight(Rectangle_t * r);


/**
 * Unsets the value of the "rX" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_unsetRX(Rectangle_t * r);


/**
 * Unsets the value of the "rY" element of this Rectangle_t.
 *
 * @param r the Rectangle_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_unsetRY(Rectangle_t * r);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Rectangle_t object have been set.
 *
 * @param r the Rectangle_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Rectangle_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Rectangle_t
 */
LIBSBML_EXTERN
int
Rectangle_hasRequiredAttributes(const Rectangle_t * r);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Rectangle_H__ */


