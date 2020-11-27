/**
 * @file    RadialGradient.h
 * @brief Definition of the RadialGradient class.
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
 * @class RadialGradient
 * @sbmlbrief{render} Representation of a radial gradient.
 *
 * The concept of a radial gradient is more or or less taken from SVG.  A
 * radial gradient is defined by a center point, a radius and an optional
 * focal point.  A valid gradient will have a positive radius 
 * greater than 0, and the focal point should be within the circle
 * defined by the center point and the radius.  Otherwise all restrictions
 * for the GradientBase class apply.
 *
 * The center and the focal point of a radial gradient are defined by three
 * pairs of absolute-relative value.  The radius is also defined as an
 * absolute-relative value pair.  For examples of RadialGradient object
 * definitions, see the SBML Render package specification and/or the SVG
 * specification.
 *
 * @see GradientBase
 * @see RelAbsVector
 */


#ifndef RadialGradient_H__
#define RadialGradient_H__


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


class LIBSBML_EXTERN RadialGradient : public GradientBase
{
protected:

  /** @cond doxygenLibsbmlInternal */
  RelAbsVector mCX;
  RelAbsVector mCY;
  RelAbsVector mCZ;
  RelAbsVector mRadius;
  RelAbsVector mFX;
  RelAbsVector mFY;
  RelAbsVector mFZ;
  /** @endcond */

public:

  /**
   * Creates a new RadialGradient using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * RadialGradient.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * RadialGradient.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this RadialGradient.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RadialGradient(unsigned int level = RenderExtension::getDefaultLevel(),
                 unsigned int version = RenderExtension::getDefaultVersion(),
                 unsigned int pkgVersion =
                   RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new RadialGradient using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RadialGradient(RenderPkgNamespaces *renderns);

  /**
   * Creates a new RadialGradient object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * RadialGradient object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the RadialGradient
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  RadialGradient(const XMLNode& node, unsigned int l2version);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a RadialGradient with no gradient stops.
   * The id is set to the given value.
   * The RadialGradient object is invalid until it has an id and at least two 
   * gradient stops.
   * The start and the end of the linear gradient vector are set to (0,0,0).
   * A linear gradient with a vector of length zero should also be considered invalid.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id the new id for the RadialGradient.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  RadialGradient(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED


  /**
   * Copy constructor for RadialGradient.
   *
   * @param orig the RadialGradient instance to copy.
   */
  RadialGradient(const RadialGradient& orig);


  /**
   * Assignment operator for RadialGradient.
   *
   * @param rhs the RadialGradient object whose values are to be used as the
   * basis of the assignment.
   */
  RadialGradient& operator=(const RadialGradient& rhs);


  /**
   * Creates and returns a deep copy of this RadialGradient object.
   *
   * @return a (deep) copy of this RadialGradient object.
   */
  virtual RadialGradient* clone() const;


  /**
   * Destructor for RadialGradient.
   */
  virtual ~RadialGradient();


  /**
   * Returns the value of the "cx" element of this RadialGradient.
   *
   * @return the value of the "cx" element of this RadialGradient as a
   * RelAbsVector.
   */
  const RelAbsVector& getCx() const;


  /**
   * Returns the value of the "cx" element of this RadialGradient.
   *
   * @return the value of the "cx" element of this RadialGradient as a
   * RelAbsVector.
   */
  RelAbsVector& getCx();


  /**
   * Returns the x coordinate for the start point as a const reference.
   *
   * @return RelAbsVector that represents the x value of the start point.
   */
  const RelAbsVector& getCenterX() const;


  /**
  * Returns the x coordinate for the start point as a reference.
  *
  * @return RelAbsVector that represents the x value of the start point.
  */
  RelAbsVector& getCenterX();

  /**
   * Returns the value of the "cy" element of this RadialGradient.
   *
   * @return the value of the "cy" element of this RadialGradient as a
   * RelAbsVector.
   */
  const RelAbsVector& getCy() const;


  /**
   * Returns the value of the "cy" element of this RadialGradient.
   *
   * @return the value of the "cy" element of this RadialGradient as a
   * RelAbsVector.
   */
  RelAbsVector& getCy();


  /**
   * Returns the y coordinate for the start point as a const reference.
   *
   * @return RelAbsVector that represents the y value of the start point.
   */
  const RelAbsVector& getCenterY() const;


  /**
  * Returns the y coordinate for the start point as a reference.
  *
  * @return RelAbsVector that represents the y value of the start point.
  */
  RelAbsVector& getCenterY();

  /**
   * Returns the value of the "cz" element of this RadialGradient.
   *
   * @return the value of the "cz" element of this RadialGradient as a
   * RelAbsVector.
   */
  const RelAbsVector& getCz() const;


  /**
   * Returns the value of the "cz" element of this RadialGradient.
   *
   * @return the value of the "cz" element of this RadialGradient as a
   * RelAbsVector.
   */
  RelAbsVector& getCz();


  /**
   * Returns the z coordinate for the start point as a const reference.
   *
   * @return RelAbsVector that represents the z value of the start point.
   */
  const RelAbsVector& getCenterZ() const;

  /**
  * Returns the z coordinate for the start point as a reference.
  *
  * @return RelAbsVector that represents the z value of the start point.
  */
  RelAbsVector& getCenterZ();

  /**
  * Returns the value of the "r" element of this RadialGradient.
  *
  * @return the value of the "r" element of this RadialGradient as a
  * RelAbsVector.
  */
  const RelAbsVector& getR() const;


  /**
  * Returns the value of the "r" element of this RadialGradient.
  *
  * @return the value of the "r" element of this RadialGradient as a
  * RelAbsVector.
  */
  RelAbsVector& getR();


  /**
   * Returns the radius as a const reference.
   *
   * @return const reference to the radius 
   */
  const RelAbsVector& getRadius() const;
  
  /**
   * Returns the radius as a reference.
   *
   * @return reference to the radius 
   */
  RelAbsVector& getRadius();

  /**
  * Returns the value of the "fx" element of this RadialGradient.
  *
  * @return the value of the "fx" element of this RadialGradient as a
  * RelAbsVector.
  */
  const RelAbsVector& getFx() const;


  /**
  * Returns the value of the "fx" element of this RadialGradient.
  *
  * @return the value of the "fx" element of this RadialGradient as a
  * RelAbsVector.
  */
  RelAbsVector& getFx();


  /**
  * Returns the x coordinate for the start point as a const reference.
  *
  * @return RelAbsVector that represents the x value of the start point.
  */
  const RelAbsVector& getFocalPointX() const;

  /**
  * Returns the x coordinate for the start point as a reference.
  *
  * @return RelAbsVector that represents the x value of the start point.
  */
  RelAbsVector& getFocalPointX();

  /**
  * Returns the value of the "fy" element of this RadialGradient.
  *
  * @return the value of the "fy" element of this RadialGradient as a
  * RelAbsVector.
  */
  const RelAbsVector& getFy() const;


  /**
  * Returns the value of the "fy" element of this RadialGradient.
  *
  * @return the value of the "fy" element of this RadialGradient as a
  * RelAbsVector.
  */
  RelAbsVector& getFy();


  /**
  * Returns the y coordinate for the start point as a const reference.
  *
  * @return RelAbsVector that represents the y value of the start point.
  */
  const RelAbsVector& getFocalPointY() const;

  /**
  * Returns the y coordinate for the start point as a reference.
  *
  * @return RelAbsVector that represents the y value of the start point.
  */
  RelAbsVector& getFocalPointY();

  /**
  * Returns the value of the "fz" element of this RadialGradient.
  *
  * @return the value of the "fz" element of this RadialGradient as a
  * RelAbsVector.
  */
  const RelAbsVector& getFz() const;


  /**
  * Returns the value of the "fz" element of this RadialGradient.
  *
  * @return the value of the "fz" element of this RadialGradient as a
  * RelAbsVector.
  */
  RelAbsVector& getFz();


  /**
  * Returns the z coordinate for the start point as a const reference.
  *
  * @return RelAbsVector that represents the z value of the start point.
  */
  const RelAbsVector& getFocalPointZ() const;

  /**
  * Returns the z coordinate for the start point as a reference.
  *
  * @return RelAbsVector that represents the z value of the start point.
  */
  RelAbsVector& getFocalPointZ();

  /**
   * Predicate returning @c true if this RadialGradient's "cx" element is set.
   *
   * @return @c true if this RadialGradient's "cx" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetCx() const;


  /**
   * Predicate returning @c true if this RadialGradient's "cy" element is set.
   *
   * @return @c true if this RadialGradient's "cy" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetCy() const;


  /**
   * Predicate returning @c true if this RadialGradient's "cz" element is set.
   *
   * @return @c true if this RadialGradient's "cz" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetCz() const;


  /**
   * Predicate returning @c true if this RadialGradient's "r" element is set.
   *
   * @return @c true if this RadialGradient's "r" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetR() const;


  /**
   * Predicate returning @c true if this RadialGradient's "fx" element is set.
   *
   * @return @c true if this RadialGradient's "fx" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetFx() const;


  /**
   * Predicate returning @c true if this RadialGradient's "fy" element is set.
   *
   * @return @c true if this RadialGradient's "fy" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetFy() const;


  /**
   * Predicate returning @c true if this RadialGradient's "fz" element is set.
   *
   * @return @c true if this RadialGradient's "fz" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetFz() const;


  /**
   * Sets the value of the "cx" element of this RadialGradient.
   *
   * @param cx RelAbsVector& value of the "cx" element to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setCx(const RelAbsVector& cx);


  /**
   * Sets the value of the "cy" element of this RadialGradient.
   *
   * @param cy RelAbsVector& value of the "cy" element to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setCy(const RelAbsVector& cy);


  /**
   * Sets the value of the "cz" element of this RadialGradient.
   *
   * @param cz RelAbsVector& value of the "cz" element to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setCz(const RelAbsVector& cz);


  /**
   * Sets the value of the "r" element of this RadialGradient.
   *
   * @param r RelAbsVector value of the "r" element to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setR(const RelAbsVector& r);


  /**
   * Sets the radius of the radial gradient.
   *
   * @param r radius of the radial gradient vector.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadius(const RelAbsVector& r);

  /**
   * Sets the value of the "fx" element of this RadialGradient.
   *
   * @param fx RelAbsVector& value of the "fx" element to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setFx(const RelAbsVector& fx);


  /**
   * Sets the value of the "fy" element of this RadialGradient.
   *
   * @param fy RelAbsVector& value of the "fy" element to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setFy(const RelAbsVector& fy);


  /**
   * Sets the value of the "fz" element of this RadialGradient.
   *
   * @param fz RelAbsVector& value of the "fz" element to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setFz(const RelAbsVector& fz);


  /**
   * Sets the 3D coordinates for the center and the focal
   * point as well as the radius.
   * Each value can be a combination of absolute and relative value and is represented by 
   * a RelAbsVector object.
   *
   * @param x x value of the center point of the radial gradient vector
   * @param y y value of the center point of the radial gradient vector
   * @param z z value of the center point of the radial gradient vector
   * @param r x value of the radius of the radial gradient vector
   * @param fx x value of the focal point of the radial gradient vector
   * @param fy y value of the focal point of the radial gradient vector
   * @param fz z value of the focal point of the radial gradient vector
   */
  void setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z,const RelAbsVector& r,const RelAbsVector& fx,const RelAbsVector& fy,const RelAbsVector& fz);

  /**
   * Sets the 2D coordinates for the center and the focal
   * point as well as the radius.
   * The z values are automatically set to 0.
   * Each value can be a combination of absolute and relative value and is represented by 
   * a RelAbsVector object.
   *
   * @param x x value of the center point of the radial gradient vector
   * @param y y value of the center point of the radial gradient vector
   * @param r x value of the radius of the radial gradient vector
   * @param fx x value of the focal point of the radial gradient vector
   * @param fy y value of the focal point of the radial gradient vector
   */
  void setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& r,const RelAbsVector& fx,const RelAbsVector& fy);

  /**
   * Sets the coordinates for the center point.
   *
   * @param x x value of the center point of the radial gradient vector
   * @param y y value of the center point of the radial gradient vector
   * @param z z value of the center point of the radial gradient vector
   * The z argument can be omitted. In that case it is set to 0.
   */
  void setCenter(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z=RelAbsVector(0.0,0.0));

  /**
   * Sets the coordinates for the focal point.
   *
   * @param x x value of the focal point of the radial gradient vector
   * @param y y value of the focal point of the radial gradient vector
   * @param z z value of the focal point of the radial gradient vector.
   * The z argument can be omitted. In that case it is set to 0.
   */
  void setFocalPoint(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z=RelAbsVector(0.0,0.0));


  /**
   * Unsets the value of the "cx" element of this RadialGradient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetCx();


  /**
   * Unsets the value of the "cy" element of this RadialGradient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetCy();


  /**
   * Unsets the value of the "cz" element of this RadialGradient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetCz();


  /**
   * Unsets the value of the "r" element of this RadialGradient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetR();


  /**
   * Unsets the value of the "fx" element of this RadialGradient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetFx();


  /**
   * Unsets the value of the "fy" element of this RadialGradient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetFy();


  /**
   * Unsets the value of the "fz" element of this RadialGradient.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetFz();


  /**
   * Returns the XML element name of this RadialGradient object.
   *
   * For RadialGradient, the XML element name is always @c "radialGradient".
   *
   * @return the name of this element, i.e. @c "radialGradient".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this RadialGradient object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_RADIALGRADIENT, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /**
   * Creates an XMLNode object from this RadialGradient object.
   *
   * @return the XMLNode with the XML representation for the 
   * RadialGradient object.
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
 * Creates a new RadialGradient_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * RadialGradient_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * RadialGradient_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * RadialGradient_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
RadialGradient_t *
RadialGradient_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this RadialGradient_t object.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return a (deep) copy of this RadialGradient_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
RadialGradient_t*
RadialGradient_clone(const RadialGradient_t* rg);


/**
 * Frees this RadialGradient_t object.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
void
RadialGradient_free(RadialGradient_t* rg);


/**
 * Returns the value of the "cx" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure whose cx is sought.
 *
 * @return the value of the "cx" element of this RadialGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getCx(const RadialGradient_t * rg);


/**
 * Returns the value of the "cy" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure whose cy is sought.
 *
 * @return the value of the "cy" element of this RadialGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getCy(const RadialGradient_t * rg);


/**
 * Returns the value of the "cz" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure whose cz is sought.
 *
 * @return the value of the "cz" element of this RadialGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getCz(const RadialGradient_t * rg);


/**
 * Returns the value of the "r" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure whose r is sought.
 *
 * @return the value of the "r" element of this RadialGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getR(const RadialGradient_t * rg);


/**
 * Returns the value of the "fx" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure whose fx is sought.
 *
 * @return the value of the "fx" element of this RadialGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getFx(const RadialGradient_t * rg);


/**
 * Returns the value of the "fy" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure whose fy is sought.
 *
 * @return the value of the "fy" element of this RadialGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getFy(const RadialGradient_t * rg);


/**
 * Returns the value of the "fz" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure whose fz is sought.
 *
 * @return the value of the "fz" element of this RadialGradient_t as a
 * RelAbsVector_t.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RadialGradient_getFz(const RadialGradient_t * rg);


/**
 * Predicate returning @c 1 (true) if this RadialGradient_t's "cx" element is
 * set.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return @c 1 (true) if this RadialGradient_t's "cx" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_isSetCx(const RadialGradient_t * rg);


/**
 * Predicate returning @c 1 (true) if this RadialGradient_t's "cy" element is
 * set.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return @c 1 (true) if this RadialGradient_t's "cy" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_isSetCy(const RadialGradient_t * rg);


/**
 * Predicate returning @c 1 (true) if this RadialGradient_t's "cz" element is
 * set.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return @c 1 (true) if this RadialGradient_t's "cz" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_isSetCz(const RadialGradient_t * rg);


/**
 * Predicate returning @c 1 (true) if this RadialGradient_t's "r" element is
 * set.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return @c 1 (true) if this RadialGradient_t's "r" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_isSetR(const RadialGradient_t * rg);


/**
 * Predicate returning @c 1 (true) if this RadialGradient_t's "fx" element is
 * set.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return @c 1 (true) if this RadialGradient_t's "fx" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_isSetFx(const RadialGradient_t * rg);


/**
 * Predicate returning @c 1 (true) if this RadialGradient_t's "fy" element is
 * set.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return @c 1 (true) if this RadialGradient_t's "fy" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_isSetFy(const RadialGradient_t * rg);


/**
 * Predicate returning @c 1 (true) if this RadialGradient_t's "fz" element is
 * set.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return @c 1 (true) if this RadialGradient_t's "fz" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_isSetFz(const RadialGradient_t * rg);


/**
 * Sets the value of the "cx" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @param cx RelAbsVector_t value of the "cx" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_setCx(RadialGradient_t * rg, const RelAbsVector_t* cx);


/**
 * Sets the value of the "cy" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @param cy RelAbsVector_t value of the "cy" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_setCy(RadialGradient_t * rg, const RelAbsVector_t* cy);


/**
 * Sets the value of the "cz" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @param cz RelAbsVector_t value of the "cz" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_setCz(RadialGradient_t * rg, const RelAbsVector_t* cz);


/**
 * Sets the value of the "r" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @param r RelAbsVector_t value of the "r" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_setR(RadialGradient_t * rg, const RelAbsVector_t* r);


/**
 * Sets the value of the "fx" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @param fx RelAbsVector_t value of the "fx" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_setFx(RadialGradient_t * rg, const RelAbsVector_t* fx);


/**
 * Sets the value of the "fy" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @param fy RelAbsVector_t value of the "fy" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_setFy(RadialGradient_t * rg, const RelAbsVector_t* fy);


/**
 * Sets the value of the "fz" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @param fz RelAbsVector_t value of the "fz" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_setFz(RadialGradient_t * rg, const RelAbsVector_t* fz);


/**
 * Unsets the value of the "cx" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_unsetCx(RadialGradient_t * rg);


/**
 * Unsets the value of the "cy" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_unsetCy(RadialGradient_t * rg);


/**
 * Unsets the value of the "cz" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_unsetCz(RadialGradient_t * rg);


/**
 * Unsets the value of the "r" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_unsetR(RadialGradient_t * rg);


/**
 * Unsets the value of the "fx" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_unsetFx(RadialGradient_t * rg);


/**
 * Unsets the value of the "fy" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_unsetFy(RadialGradient_t * rg);


/**
 * Unsets the value of the "fz" element of this RadialGradient_t.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_unsetFz(RadialGradient_t * rg);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RadialGradient_t object have been set.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * RadialGradient_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_hasRequiredAttributes(const RadialGradient_t * rg);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * RadialGradient_t object have been set.
 *
 * @param rg the RadialGradient_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * RadialGradient_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the RadialGradient_t object are:
 *
 * @memberof RadialGradient_t
 */
LIBSBML_EXTERN
int
RadialGradient_hasRequiredElements(const RadialGradient_t * rg);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !RadialGradient_H__ */


