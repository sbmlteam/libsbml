/**
 * @file    LinearGradient.h
 * @brief Definition of the LinearGradient class.
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
 * @class LinearGradient
 * @sbmlbrief{render} Representation of a linear gradient.
 *
 * The concept of a linear gradient is more or or less taken from SVG.  A
 * linear gradient is defined by a vector which determines the direction and
 * the length of the gradient.  So for a valid gradient, this vector should
 * have a length different from 0.  Otherwise, all restrictions for the
 * GradientBase class apply.
 *
 * The vector for a linear gradient is defined by a start and an endpoint and
 * each point consists of three absolute-relative value pairs.
 *
 * For examples of LinearGradient objects, see the SBML Render package
 * specification and/or the SVG specification.
 *
 * @see GradientBase
 * @see RelAbsVector
 */

#ifndef LinearGradient_H__
#define LinearGradient_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>

#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/GradientBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RelAbsVector.h>


LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN LinearGradient : public GradientBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  RelAbsVector mX1;
  RelAbsVector mY1;
  RelAbsVector mZ1;
  RelAbsVector mX2;
  RelAbsVector mY2;
  RelAbsVector mZ2;
  /** @endcond */

public:

  /**
   * Creates a new LinearGradient using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * LinearGradient.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * LinearGradient.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this LinearGradient.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  LinearGradient(unsigned int level = RenderExtension::getDefaultLevel(),
                 unsigned int version = RenderExtension::getDefaultVersion(),
                 unsigned int pkgVersion =
                   RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new LinearGradient using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  LinearGradient(RenderPkgNamespaces *renderns);

  /**
   * Creates a new LinearGradient object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * LinearGradient object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the LinearGradient
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  LinearGradient(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a LinearGradient with no gradient stops.
   * The id is set to the given value.
   * The LinearGradient object is invalid until it has an id and at least two 
   * gradient stops.
   * The start and the end of the linear gradient vector are set to (0,0,0).
   * A linear gradient with a vector of length zero should also be considered invalid.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id the new id for the LinearGradient.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  LinearGradient(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED


  /**
   * Copy constructor for LinearGradient.
   *
   * @param orig the LinearGradient instance to copy.
   */
  LinearGradient(const LinearGradient& orig);


  /**
   * Assignment operator for LinearGradient.
   *
   * @param rhs the LinearGradient object whose values are to be used as the
   * basis of the assignment.
   */
  LinearGradient& operator=(const LinearGradient& rhs);


  /**
   * Creates and returns a deep copy of this LinearGradient object.
   *
   * @return a (deep) copy of this LinearGradient object.
   */
  virtual LinearGradient* clone() const;


  /**
   * Destructor for LinearGradient.
   */
  virtual ~LinearGradient();


  /**
   * Returns the value of the "x1" element of this LinearGradient.
   *
   * @return the value of the "x1" element of this LinearGradient as a
   * RelAbsVector.
   */
  const RelAbsVector& getX1() const;


  /**
   * Returns the value of the "x1" element of this LinearGradient.
   *
   * @return the value of the "x1" element of this LinearGradient as a
   * RelAbsVector.
   */
  RelAbsVector& getX1();


  /**
   * Returns the x coordinate for the start point as a const reference.
   *
   * @return RelAbsVector that represents the x value of the start point.
   */
  const RelAbsVector& getXPoint1() const;


  /**
  * Returns the x coordinate for the start point as a const reference.
  *
  * @return RelAbsVector that represents the x value of the start point.
  */
  RelAbsVector& getXPoint1();

  /**
   * Returns the value of the "y1" element of this LinearGradient.
   *
   * @return the value of the "y1" element of this LinearGradient as a
   * RelAbsVector.
   */
  const RelAbsVector& getY1() const;


  /**
   * Returns the value of the "y1" element of this LinearGradient.
   *
   * @return the value of the "y1" element of this LinearGradient as a
   * RelAbsVector.
   */
  RelAbsVector& getY1();


  /**
   * Returns the y coordinate for the start point as a const reference.
   *
   * @return RelAbsVector that represents the y value of the start point.
   */
  const RelAbsVector& getYPoint1() const;

  /**
  * Returns the y coordinate for the start point as a const reference.
  *
  * @return RelAbsVector that represents the y value of the start point.
  */
  RelAbsVector& getYPoint1();

  /**
   * Returns the value of the "z1" element of this LinearGradient.
   *
   * @return the value of the "z1" element of this LinearGradient as a
   * RelAbsVector.
   */
  const RelAbsVector& getZ1() const;


  /**
   * Returns the value of the "z1" element of this LinearGradient.
   *
   * @return the value of the "z1" element of this LinearGradient as a
   * RelAbsVector.
   */
  RelAbsVector& getZ1();


  /**
   * Returns the z coordinate for the start point as a const reference.
   *
   * @return RelAbsVector that represents the z value of the start point.
   */
  const RelAbsVector& getZPoint1() const;

  /**
  * Returns the z coordinate for the start point as a const reference.
  *
  * @return RelAbsVector that represents the z value of the start point.
  */
  RelAbsVector& getZPoint1();

  /**
  * Returns the value of the "x2" element of this LinearGradient.
  *
  * @return the value of the "x2" element of this LinearGradient as a
  * RelAbsVector.
  */
  const RelAbsVector& getX2() const;


  /**
  * Returns the value of the "x2" element of this LinearGradient.
  *
  * @return the value of the "x2" element of this LinearGradient as a
  * RelAbsVector.
  */
  RelAbsVector& getX2();


  /**
  * Returns the x coordinate for the end point as a const reference.
  *
  * @return RelAbsVector that represents the x value of the end point.
  */
  const RelAbsVector& getXPoint2() const;

  /**
  * Returns the x coordinate for the end point as a const reference.
  *
  * @return RelAbsVector that represents the x value of the end point.
  */
  RelAbsVector& getXPoint2();

  /**
  * Returns the value of the "y2" element of this LinearGradient.
  *
  * @return the value of the "y2" element of this LinearGradient as a
  * RelAbsVector.
  */
  const RelAbsVector& getY2() const;


  /**
  * Returns the value of the "y2" element of this LinearGradient.
  *
  * @return the value of the "y2" element of this LinearGradient as a
  * RelAbsVector.
  */
  RelAbsVector& getY2();


  /**
  * Returns the y coordinate for the end point as a const reference.
  *
  * @return RelAbsVector that represents the y value of the end point.
  */
  const RelAbsVector& getYPoint2() const;

  /**
  * Returns the y coordinate for the end point as a const reference.
  *
  * @return RelAbsVector that represents the y value of the end point.
  */
  RelAbsVector& getYPoint2();

  /**
  * Returns the value of the "z2" element of this LinearGradient.
  *
  * @return the value of the "z2" element of this LinearGradient as a
  * RelAbsVector.
  */
  const RelAbsVector& getZ2() const;


  /**
  * Returns the value of the "z2" element of this LinearGradient.
  *
  * @return the value of the "z2" element of this LinearGradient as a
  * RelAbsVector.
  */
  RelAbsVector& getZ2();


  /**
  * Returns the z coordinate for the end point as a const reference.
  *
  * @return RelAbsVector that represents the z value of the end point.
  */
  const RelAbsVector& getZPoint2() const;

  /**
  * Returns the z coordinate for the end point as a const reference.
  *
  * @return RelAbsVector that represents the z value of the end point.
  */
  RelAbsVector& getZPoint2();

  /**
   * Predicate returning @c true if this LinearGradient's "x1" element is set.
   *
   * @return @c true if this LinearGradient's "x1" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetX1() const;


  /**
   * Predicate returning @c true if this LinearGradient's "y1" element is set.
   *
   * @return @c true if this LinearGradient's "y1" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetY1() const;


  /**
   * Predicate returning @c true if this LinearGradient's "z1" element is set.
   *
   * @return @c true if this LinearGradient's "z1" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetZ1() const;


  /**
   * Predicate returning @c true if this LinearGradient's "x2" element is set.
   *
   * @return @c true if this LinearGradient's "x2" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetX2() const;


  /**
   * Predicate returning @c true if this LinearGradient's "y2" element is set.
   *
   * @return @c true if this LinearGradient's "y2" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetY2() const;


  /**
   * Predicate returning @c true if this LinearGradient's "z2" element is set.
   *
   * @return @c true if this LinearGradient's "z2" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetZ2() const;


  /**
   * Sets the value of the "x1" element of this LinearGradient.
   *
   * @param x1 RelAbsVector& value of the "x1" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setX1(const RelAbsVector& x1);


  /**
   * Sets the value of the "y1" element of this LinearGradient.
   *
   * @param y1 RelAbsVector& value of the "y1" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setY1(const RelAbsVector& y1);


  /**
   * Sets the value of the "z1" element of this LinearGradient.
   *
   * @param z1 RelAbsVector& value of the "z1" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setZ1(const RelAbsVector& z1);


  /**
   * Sets the value of the "x2" element of this LinearGradient.
   *
   * @param x2 RelAbsVector& value of the "x2" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setX2(const RelAbsVector& x2);


  /**
   * Sets the value of the "y2" element of this LinearGradient.
   *
   * @param y2 RelAbsVector& value of the "y2" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setY2(const RelAbsVector& y2);


  /**
   * Sets the value of the "z2" element of this LinearGradient.
   *
   * @param z2 RelAbsVector& value of the "z2" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setZ2(const RelAbsVector& z2);


  /**
   * Sets the 3D coordinates for the start and the end point of the linear gradient vector.
   * Each value can be a combination of absolute and relative value and is represented by 
   * a RelAbsVector object.
   *
   * @param x1 x value of the start point of the linear gradient vector
   * @param y1 y value of the start point of the linear gradient vector
   * @param z1 z value of the start point of the linear gradient vector
   * @param x2 x value of the end point of the linear gradient vector
   * @param y2 y value of the end point of the linear gradient vector
   * @param z2 z value of the end point of the linear gradient vector
   */
  void setCoordinates(const RelAbsVector& x1,const RelAbsVector& y1,const RelAbsVector& z1,const RelAbsVector& x2,const RelAbsVector& y2,const RelAbsVector& z2);

  /**
   * Sets the 2D coordinates for the start and the end point of the linear gradient vector.
   * The z values are automatically set to 0.
   * Each value can be a combination of absolute and relative value and is represented by 
   * a RelAbsVector object.
   *
   * @param x1 x value of the start point of the linear gradient vector
   * @param y1 y value of the start point of the linear gradient vector
   * @param x2 x value of the end point of the linear gradient vector
   * @param y2 y value of the end point of the linear gradient vector
   */
  void setCoordinates(const RelAbsVector& x1,const RelAbsVector& y1,const RelAbsVector& x2,const RelAbsVector& y2);

  /**
   * Sets the coordinates for the start point of the linear gradient vector.
   *
   * Each value can be a combination of absolute and relative value and is represented by 
   * a RelAbsVector object.
   *
   * The z value can be omitted. In that case it is set to 0.
   *
   * @param x x value of the start point of the linear gradient vector
   * @param y y value of the start point of the linear gradient vector
   * @param z z value of the start point of the linear gradient vector
   *
   */
  void setPoint1(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z=RelAbsVector(0.0,0.0));

  /**
   * Sets the coordinates for the end point of the linear gradient vector.
   *
   * Each value can be a combination of absolute and relative value and is represented by 
   * a RelAbsVector object.
   *
   * The z value can be omitted. In that case it is set to 0.
   *
   * @param x x value of the end point of the linear gradient vector
   * @param y y value of the end point of the linear gradient vector
   * @param z z value of the end point of the linear gradient vector
   *
   */
  void setPoint2(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z=RelAbsVector(0.0,0.0));

  /**
   * Unsets the value of the "x1" element of this LinearGradient.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetX1();


  /**
   * Unsets the value of the "y1" element of this LinearGradient.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetY1();


  /**
   * Unsets the value of the "z1" element of this LinearGradient.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetZ1();


  /**
   * Unsets the value of the "x2" element of this LinearGradient.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetX2();


  /**
   * Unsets the value of the "y2" element of this LinearGradient.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetY2();


  /**
   * Unsets the value of the "z2" element of this LinearGradient.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetZ2();


  /**
   * Returns the XML element name of this LinearGradient object.
   *
   * For LinearGradient, the XML element name is always @c "linearGradient".
   *
   * @return the name of this element, i.e. @c "linearGradient".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this LinearGradient object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_LINEARGRADIENT, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Accepts the given SBMLVisitor for this instance of LinearGradient.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * Creates an XMLNode object from this LinearGradient object.
   *
   * @return the XMLNode with the XML representation for the 
   * LinearGradient object.
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
 * Creates a new LinearGradient_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * LinearGradient_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * LinearGradient_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * LinearGradient_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
LinearGradient_t *
LinearGradient_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this LinearGradient_t object.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @return a (deep) copy of this LinearGradient_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
LinearGradient_t*
LinearGradient_clone(const LinearGradient_t* lg);


/**
 * Frees this LinearGradient_t object.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
void
LinearGradient_free(LinearGradient_t* lg);


/**
 * Returns the value of the "x1" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure whose x1 is sought.
 *
 * @return the value of the "x1" element of this LinearGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getX1(const LinearGradient_t * lg);


/**
 * Returns the value of the "y1" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure whose y1 is sought.
 *
 * @return the value of the "y1" element of this LinearGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getY1(const LinearGradient_t * lg);


/**
 * Returns the value of the "z1" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure whose z1 is sought.
 *
 * @return the value of the "z1" element of this LinearGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getZ1(const LinearGradient_t * lg);


/**
 * Returns the value of the "x2" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure whose x2 is sought.
 *
 * @return the value of the "x2" element of this LinearGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getX2(const LinearGradient_t * lg);


/**
 * Returns the value of the "y2" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure whose y2 is sought.
 *
 * @return the value of the "y2" element of this LinearGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getY2(const LinearGradient_t * lg);


/**
 * Returns the value of the "z2" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure whose z2 is sought.
 *
 * @return the value of the "z2" element of this LinearGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
LinearGradient_getZ2(const LinearGradient_t * lg);


/**
 * Predicate returning @c 1 (true) if this LinearGradient_t's "x1" element is
 * set.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @return @c 1 (true) if this LinearGradient_t's "x1" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_isSetX1(const LinearGradient_t * lg);


/**
 * Predicate returning @c 1 (true) if this LinearGradient_t's "y1" element is
 * set.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @return @c 1 (true) if this LinearGradient_t's "y1" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_isSetY1(const LinearGradient_t * lg);


/**
 * Predicate returning @c 1 (true) if this LinearGradient_t's "z1" element is
 * set.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @return @c 1 (true) if this LinearGradient_t's "z1" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_isSetZ1(const LinearGradient_t * lg);


/**
 * Predicate returning @c 1 (true) if this LinearGradient_t's "x2" element is
 * set.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @return @c 1 (true) if this LinearGradient_t's "x2" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_isSetX2(const LinearGradient_t * lg);


/**
 * Predicate returning @c 1 (true) if this LinearGradient_t's "y2" element is
 * set.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @return @c 1 (true) if this LinearGradient_t's "y2" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_isSetY2(const LinearGradient_t * lg);


/**
 * Predicate returning @c 1 (true) if this LinearGradient_t's "z2" element is
 * set.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @return @c 1 (true) if this LinearGradient_t's "z2" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_isSetZ2(const LinearGradient_t * lg);


/**
 * Sets the value of the "x1" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @param x1 RelAbsVector_t value of the "x1" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_setX1(LinearGradient_t * lg, const RelAbsVector_t* x1);


/**
 * Sets the value of the "y1" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @param y1 RelAbsVector_t value of the "y1" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_setY1(LinearGradient_t * lg, const RelAbsVector_t* y1);


/**
 * Sets the value of the "z1" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @param z1 RelAbsVector_t value of the "z1" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_setZ1(LinearGradient_t * lg, const RelAbsVector_t* z1);


/**
 * Sets the value of the "x2" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @param x2 RelAbsVector_t value of the "x2" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_setX2(LinearGradient_t * lg, const RelAbsVector_t* x2);


/**
 * Sets the value of the "y2" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @param y2 RelAbsVector_t value of the "y2" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_setY2(LinearGradient_t * lg, const RelAbsVector_t* y2);


/**
 * Sets the value of the "z2" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @param z2 RelAbsVector_t value of the "z2" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_setZ2(LinearGradient_t * lg, const RelAbsVector_t* z2);


/**
 * Unsets the value of the "x1" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_unsetX1(LinearGradient_t * lg);


/**
 * Unsets the value of the "y1" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_unsetY1(LinearGradient_t * lg);


/**
 * Unsets the value of the "z1" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_unsetZ1(LinearGradient_t * lg);


/**
 * Unsets the value of the "x2" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_unsetX2(LinearGradient_t * lg);


/**
 * Unsets the value of the "y2" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_unsetY2(LinearGradient_t * lg);


/**
 * Unsets the value of the "z2" element of this LinearGradient_t.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_unsetZ2(LinearGradient_t * lg);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * LinearGradient_t object have been set.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * LinearGradient_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_hasRequiredAttributes(const LinearGradient_t * lg);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * LinearGradient_t object have been set.
 *
 * @param lg the LinearGradient_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * LinearGradient_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the LinearGradient_t object are:
 *
 * @memberof LinearGradient_t
 */
LIBSBML_EXTERN
int
LinearGradient_hasRequiredElements(const LinearGradient_t * lg);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !LinearGradient_H__ */


