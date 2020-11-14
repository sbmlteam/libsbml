/**
 * @file    RenderCubicBezier.h
 * @brief Definition of the RenderCubicBezier class.
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
 * @class RenderCubicBezier
 * @sbmlbrief{render} Representation of cubic Bezier curves.
 *
 * The RenderCubicBezier is derived from RenderPoint and is the second
 * element needed to represent arbitrary curves with relative coordinates as
 * they can appear in RenderCurve and Polygon objects.  In addition to the
 * attributes inherited from RenderPoint, RenderCubicBezier has two
 * additional attributes for the two base points that define a cubic bezier
 * curve.
 *
 * Segments in a RenderCurve or a Polygon are always defined by two
 * consecutive RenderPoint or RenderCubicBezier elements. The first element
 * in a list of RenderPoints has to be a RenderPoint object, all following
 * elements can either be RenderPoint or RenderCubicBezier elements.  If the
 * second element is a RenderPoint, the two elements represent a straight
 * line segement; if the second element if a RenderCubicBezier, the two
 * elements represent a cubic bezier curve segment.
 *
 * For further details please have a look at the SBML Level&nbsp;3 Render
 * package specification.
 */

#ifndef RenderCubicBezier_H__
#define RenderCubicBezier_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>

#include <sbml/packages/layout/util/LayoutUtilities.h>

#include <sbml/packages/render/common/renderfwd.h>
#include <sbml/xml/XMLNode.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/RenderPoint.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RelAbsVector.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN RenderCubicBezier : public RenderPoint
{
protected:

  /** @cond doxygenLibsbmlInternal */

  RelAbsVector mBasePoint1_X;
  RelAbsVector mBasePoint1_Y;
  RelAbsVector mBasePoint1_Z;
  RelAbsVector mBasePoint2_X;
  RelAbsVector mBasePoint2_Y;
  RelAbsVector mBasePoint2_Z;

  /** @endcond */

public:

  /**
   * Creates a new RenderCubicBezier using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * RenderCubicBezier.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * RenderCubicBezier.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this RenderCubicBezier.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RenderCubicBezier(unsigned int level = RenderExtension::getDefaultLevel(),
                    unsigned int version =
                      RenderExtension::getDefaultVersion(),
                    unsigned int pkgVersion =
                      RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new RenderCubicBezier using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RenderCubicBezier(RenderPkgNamespaces *renderns);



  /**
  * Creates a CubicBezier with the given points.
  *
  * @param renderns the RenderPkgNamespaces object.
  * @param bp1_x x coordinate of the first base point.
  * @param bp1_y y coordinate of the first base point.
  * @param bp1_z z coordinate of the first base point.
  * @param bp2_x x coordinate of the second base point.
  * @param bp2_y y coordinate of the second base point.
  * @param bp2_z z coordinate of the second base point.
  * @param end_x x coordinate of the end point.
  * @param end_y y coordinate of the end point.
  * @param end_z z coordinate of the end point.
  */
  RenderCubicBezier(RenderPkgNamespaces* renderns,
    const RelAbsVector& bp1_x,
    const RelAbsVector& bp1_y,
    const RelAbsVector& bp1_z,
    const RelAbsVector& bp2_x,
    const RelAbsVector& bp2_y,
    const RelAbsVector& bp2_z,
    const RelAbsVector& end_x,
    const RelAbsVector& end_y,
    const RelAbsVector& end_z);


  /**
  * Creates a new RenderCubicBezier object from the given XMLNode object.
  * The XMLNode object has to contain a valid XML representation of a
  * RenderCubicBezier object as defined in the render extension specification.
  * This method is normally called when render information is read from a file and
  * should normally not have to be called explicitly.
  *
  * @param node the XMLNode object reference that describes the RenderCubicBezier
  * object to be instantiated.
  * @param l2version the version of SBML Level&nbsp;2 to target.
  */
  RenderCubicBezier(const XMLNode& node, unsigned int l2version = 4);


  /**
   * Copy constructor for RenderCubicBezier.
   *
   * @param orig the RenderCubicBezier instance to copy.
   */
  RenderCubicBezier(const RenderCubicBezier& orig);


  /**
   * Assignment operator for RenderCubicBezier.
   *
   * @param rhs the RenderCubicBezier object whose values are to be used as the
   * basis of the assignment.
   */
  RenderCubicBezier& operator=(const RenderCubicBezier& rhs);


  /**
   * Comparison operator for RenderCubicBezier objects.
   */
  bool operator==(const RenderCubicBezier& left) const;


  /**
   * Creates and returns a deep copy of this RenderCubicBezier object.
   *
   * @return a (deep) copy of this RenderCubicBezier object.
   */
  virtual RenderCubicBezier* clone() const;


  /**
   * Destructor for RenderCubicBezier.
   */
  virtual ~RenderCubicBezier();


  /**
   * Returns the value of the "basePoint1_x" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint1_x" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  const RelAbsVector& getBasePoint1_x() const;


  /**
   * Returns the value of the "basePoint1_x" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint1_x" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  RelAbsVector& getBasePoint1_x();


  /**
   * Returns the x value of the first base point of the curve (the one closer to the
   * starting point) as a const reference.
   *
   * @return const reference to x value of first base point
   */ 
  const RelAbsVector& basePoint1_X() const;

  
  /**
  * Returns the x value of the first base point of the curve (the one closer to the
  * starting point) as a reference.
  *
  * @return reference to x value of first base point
  */
  RelAbsVector& basePoint1_X();

  /**
   * Returns the value of the "basePoint1_y" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint1_y" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  const RelAbsVector& getBasePoint1_y() const;


  /**
   * Returns the value of the "basePoint1_y" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint1_y" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  RelAbsVector& getBasePoint1_y();


  /**
   * Returns the y value of the first base point of the curve (the one closer to the
   * starting point) as a const reference.
   *
   * @return const reference to y value of first base point
   */ 
  const RelAbsVector& basePoint1_Y() const;

  /**
  * Returns the y value of the first base point of the curve (the one closer to the
  * starting point) as a reference.
  *
  * @return reference to y value of first base point
  */
  RelAbsVector& basePoint1_Y();


  /**
   * Returns the value of the "basePoint1_z" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint1_z" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  const RelAbsVector& getBasePoint1_z() const;


  /**
   * Returns the value of the "basePoint1_z" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint1_z" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  RelAbsVector& getBasePoint1_z();


  /**
   * Returns the z value of the first base point of the curve (the one closer to the
   * starting point) as a const reference.
   *
   * @return const reference to z value of first base point
   */ 
  const RelAbsVector& basePoint1_Z() const;


  /**
  * Returns the z value of the first base point of the curve (the one closer to the
  * starting point) as a reference.
  *
  * @return reference to z value of first base point
  */
  RelAbsVector& basePoint1_Z();


  /**
   * Returns the value of the "basePoint2_x" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint2_x" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  const RelAbsVector& getBasePoint2_x() const;


  /**
   * Returns the value of the "basePoint2_x" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint2_x" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  RelAbsVector& getBasePoint2_x();


  /**
   * Returns the x value of the second base point of the curve (the one further from the
   * starting point) as a const reference.
   *
   * @return const reference to x value of second base point
   */ 
  const RelAbsVector& basePoint2_X() const;

  /**
  * Returns the x value of the second base point of the curve (the one further from the
  * starting point) as a reference.
  *
  * @return reference to x value of second base point
  */
  RelAbsVector& basePoint2_X();

  /**
   * Returns the value of the "basePoint2_y" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint2_y" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  const RelAbsVector& getBasePoint2_y() const;


  /**
   * Returns the value of the "basePoint2_y" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint2_y" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  RelAbsVector& getBasePoint2_y();


  /**
   * Returns the y value of the second base point of the curve (the one further from the
   * starting point) as a const reference.
   *
   * @return const reference to y value of second base point
   */ 
  const RelAbsVector& basePoint2_Y() const;

  /**
  * Returns the y value of the second base point of the curve (the one further from the
  * starting point) as a reference.
  *
  * @return reference to y value of second base point
  */
  RelAbsVector& basePoint2_Y();

  /**
   * Returns the value of the "basePoint2_z" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint2_z" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  const RelAbsVector& getBasePoint2_z() const;


  /**
   * Returns the value of the "basePoint2_z" element of this RenderCubicBezier.
   *
   * @return the value of the "basePoint2_z" element of this RenderCubicBezier
   * as a RelAbsVector.
   */
  RelAbsVector& getBasePoint2_z();


  /**
   * Returns the z value of the second base point of the curve (the one further from the
   * starting point) as a const reference.
   *
   * @return const reference to z value of second base point
   */ 
  const RelAbsVector& basePoint2_Z() const;

  /**
   * Returns the z value of the second base point of the curve (the one further from the
   * starting point) as a reference.
   *
   * @return reference to z value of second base point
   */ 
  RelAbsVector& basePoint2_Z();

  /**
   * Predicate returning @c true if this RenderCubicBezier's "basePoint1_x"
   * element is set.
   *
   * @return @c true if this RenderCubicBezier's "basePoint1_x" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetBasePoint1_x() const;


  /**
   * Predicate returning @c true if this RenderCubicBezier's "basePoint1_y"
   * element is set.
   *
   * @return @c true if this RenderCubicBezier's "basePoint1_y" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetBasePoint1_y() const;


  /**
   * Predicate returning @c true if this RenderCubicBezier's "basePoint1_z"
   * element is set.
   *
   * @return @c true if this RenderCubicBezier's "basePoint1_z" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetBasePoint1_z() const;


  /**
   * Predicate returning @c true if this RenderCubicBezier's "basePoint2_x"
   * element is set.
   *
   * @return @c true if this RenderCubicBezier's "basePoint2_x" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetBasePoint2_x() const;


  /**
   * Predicate returning @c true if this RenderCubicBezier's "basePoint2_y"
   * element is set.
   *
   * @return @c true if this RenderCubicBezier's "basePoint2_y" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetBasePoint2_y() const;


  /**
   * Predicate returning @c true if this RenderCubicBezier's "basePoint2_z"
   * element is set.
   *
   * @return @c true if this RenderCubicBezier's "basePoint2_z" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetBasePoint2_z() const;


  /**
   * Sets the value of the "basePoint1_x" element of this RenderCubicBezier.
   *
   * @param basePoint1_x RelAbsVector& value of the "basePoint1_x" element to
   * be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setBasePoint1_x(const RelAbsVector& basePoint1_x);


  /**
   * Sets the x value of the first base point of the curve (the one closer to the
   * starting point).
   *
   * @param x x coordinate of first base point.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */ 
  int setBasePoint1_X(const RelAbsVector& x);

  /**
   * Sets the value of the "basePoint1_y" element of this RenderCubicBezier.
   *
   * @param basePoint1_y RelAbsVector& value of the "basePoint1_y" element to
   * be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setBasePoint1_y(const RelAbsVector& basePoint1_y);


  /**
   * Sets the value of the "basePoint1_y" element of this RenderCubicBezier.
   * Sets the y value of the first base point of the curve (the one closer to the
   * starting point).
   *
   * @param y y coordinate of first base point.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */ 
  int setBasePoint1_Y(const RelAbsVector& y);

  /**
   * Sets the value of the "basePoint1_z" element of this RenderCubicBezier.
   *
   * @param basePoint1_z RelAbsVector& value of the "basePoint1_z" element to
   * be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setBasePoint1_z(const RelAbsVector& basePoint1_z);


  /**
   * Sets the value of the "basePoint1_z" element of this RenderCubicBezier.
   * Sets the z value of the first base point of the curve (the one closer to the
   * starting point).
   *
   * @param z z coordinate of first base point.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */ 
  int setBasePoint1_Z(const RelAbsVector& z);


  /**
   * Sets the value of the "basePoint2_x" element of this RenderCubicBezier.
   *
   * @param basePoint2_x RelAbsVector& value of the "basePoint2_x" element to
   * be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setBasePoint2_x(const RelAbsVector& basePoint2_x);


  /**
   * Sets the x value of the second base point of the curve (the one further from the
   * starting point).
   *
   * @param x value of second base point.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */ 
  int setBasePoint2_X(const RelAbsVector& x);

  /**
   * Sets the value of the "basePoint2_y" element of this RenderCubicBezier.
   *
   * @param basePoint2_y RelAbsVector& value of the "basePoint2_y" element to
   * be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setBasePoint2_y(const RelAbsVector& basePoint2_y);


  /**
   * Sets the y value of the second base point of the curve (the one further from the
   * starting point).
   *
   * @param y value of second base point.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */ 
  int setBasePoint2_Y(const RelAbsVector& y);

  /**
   * Sets the value of the "basePoint2_z" element of this RenderCubicBezier.
   *
   * @param basePoint2_z RelAbsVector& value of the "basePoint2_z" element to
   * be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setBasePoint2_z(const RelAbsVector& basePoint2_z);


  /**
   * Sets the z value of the second base point of the curve (the one further from the
   * starting point).
   *
   * @param z value of second base point.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */ 
  int setBasePoint2_Z(const RelAbsVector& z);

  /**
   * Sets the first basepoint to the given coordinatees.
   *
   * @param x coordinate of second base point.
   * @param y coordinate of second base point.
   * @param z coordinate of second base point.
   * If the z coodinate is omitted, it is set to 0.
   */ 
  void setBasePoint1(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z = RelAbsVector(0.0,0.0));

  /**
   * Sets the second basepoint to the given coordinatees.
   *
   * @param x coordinate of second base point.
   * @param y coordinate of second base point.
   * @param z coordinate of second base point.
   * If the z coodinate is omitted, it is set to 0.
   */ 
  void setBasePoint2(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z = RelAbsVector(0.0,0.0));

  /**
   * Unsets the value of the "basePoint1_x" element of this RenderCubicBezier.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetBasePoint1_x();


  /**
   * Unsets the value of the "basePoint1_y" element of this RenderCubicBezier.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetBasePoint1_y();


  /**
   * Unsets the value of the "basePoint1_z" element of this RenderCubicBezier.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetBasePoint1_z();


  /**
   * Unsets the value of the "basePoint2_x" element of this RenderCubicBezier.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetBasePoint2_x();


  /**
   * Unsets the value of the "basePoint2_y" element of this RenderCubicBezier.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetBasePoint2_y();


  /**
   * Unsets the value of the "basePoint2_z" element of this RenderCubicBezier.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetBasePoint2_z();


  /**
   * Returns the XML element name of this RenderCubicBezier object.
   *
   * For RenderCubicBezier, the XML element name is always @c "element".
   *
   * @return the name of this element, i.e. @c "element".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this RenderCubicBezier object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_CUBICBEZIER, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * RenderCubicBezier object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * RenderCubicBezier have been set, otherwise @c false is returned.
   *
   * @note The required elements for the RenderCubicBezier object are:
   * @li "basePoint1_x"
   * @li "basePoint1_y"
   * @li "basePoint2_x"
   * @li "basePoint2_y"
  */
  virtual bool hasRequiredAttributes() const;


  /**
   * Creates an XMLNode object from this RenderCubicBezier object.
   *
   * @return the XMLNode with the XML representation for the 
   * RenderCubicBezier object.
   */
  virtual XMLNode toXML(const std::string& name) const;


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

  /**
   *
   * Subclasses should override this method to write their xmlns attriubutes
   * (if any) to the XMLOutputStream. 
   *
   */
  virtual void writeXMLNS (XMLOutputStream& stream) const;

};

LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new RenderCubicBezier_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * RenderCubicBezier_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * RenderCubicBezier_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * RenderCubicBezier_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
RenderCubicBezier_t *
RenderCubicBezier_create(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this RenderCubicBezier_t object.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @return a (deep) copy of this RenderCubicBezier_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
RenderCubicBezier_t*
RenderCubicBezier_clone(const RenderCubicBezier_t* rcb);


/**
 * Frees this RenderCubicBezier_t object.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
void
RenderCubicBezier_free(RenderCubicBezier_t* rcb);


/**
 * Returns the value of the "basePoint1_x" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure whose basePoint1_x is sought.
 *
 * @return the value of the "basePoint1_x" element of this RenderCubicBezier_t
 * as a RelAbsVector_t.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint1_x(const RenderCubicBezier_t * rcb);


/**
 * Returns the value of the "basePoint1_y" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure whose basePoint1_y is sought.
 *
 * @return the value of the "basePoint1_y" element of this RenderCubicBezier_t
 * as a RelAbsVector_t.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint1_y(const RenderCubicBezier_t * rcb);


/**
 * Returns the value of the "basePoint1_z" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure whose basePoint1_z is sought.
 *
 * @return the value of the "basePoint1_z" element of this RenderCubicBezier_t
 * as a RelAbsVector_t.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint1_z(const RenderCubicBezier_t * rcb);


/**
 * Returns the value of the "basePoint2_x" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure whose basePoint2_x is sought.
 *
 * @return the value of the "basePoint2_x" element of this RenderCubicBezier_t
 * as a RelAbsVector_t.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint2_x(const RenderCubicBezier_t * rcb);


/**
 * Returns the value of the "basePoint2_y" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure whose basePoint2_y is sought.
 *
 * @return the value of the "basePoint2_y" element of this RenderCubicBezier_t
 * as a RelAbsVector_t.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint2_y(const RenderCubicBezier_t * rcb);


/**
 * Returns the value of the "basePoint2_z" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure whose basePoint2_z is sought.
 *
 * @return the value of the "basePoint2_z" element of this RenderCubicBezier_t
 * as a RelAbsVector_t.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderCubicBezier_getBasePoint2_z(const RenderCubicBezier_t * rcb);


/**
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint1_x"
 * element is set.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @return @c 1 (true) if this RenderCubicBezier_t's "basePoint1_x" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint1_x(const RenderCubicBezier_t * rcb);


/**
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint1_y"
 * element is set.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @return @c 1 (true) if this RenderCubicBezier_t's "basePoint1_y" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint1_y(const RenderCubicBezier_t * rcb);


/**
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint1_z"
 * element is set.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @return @c 1 (true) if this RenderCubicBezier_t's "basePoint1_z" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint1_z(const RenderCubicBezier_t * rcb);


/**
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint2_x"
 * element is set.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @return @c 1 (true) if this RenderCubicBezier_t's "basePoint2_x" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint2_x(const RenderCubicBezier_t * rcb);


/**
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint2_y"
 * element is set.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @return @c 1 (true) if this RenderCubicBezier_t's "basePoint2_y" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint2_y(const RenderCubicBezier_t * rcb);


/**
 * Predicate returning @c 1 (true) if this RenderCubicBezier_t's "basePoint2_z"
 * element is set.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @return @c 1 (true) if this RenderCubicBezier_t's "basePoint2_z" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_isSetBasePoint2_z(const RenderCubicBezier_t * rcb);


/**
 * Sets the value of the "basePoint1_x" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @param basePoint1_x RelAbsVector_t value of the "basePoint1_x" element to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint1_x(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint1_x);


/**
 * Sets the value of the "basePoint1_y" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @param basePoint1_y RelAbsVector_t value of the "basePoint1_y" element to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint1_y(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint1_y);


/**
 * Sets the value of the "basePoint1_z" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @param basePoint1_z RelAbsVector_t value of the "basePoint1_z" element to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint1_z(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint1_z);


/**
 * Sets the value of the "basePoint2_x" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @param basePoint2_x RelAbsVector_t value of the "basePoint2_x" element to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint2_x(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint2_x);


/**
 * Sets the value of the "basePoint2_y" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @param basePoint2_y RelAbsVector_t value of the "basePoint2_y" element to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint2_y(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint2_y);


/**
 * Sets the value of the "basePoint2_z" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @param basePoint2_z RelAbsVector_t value of the "basePoint2_z" element to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_setBasePoint2_z(RenderCubicBezier_t * rcb,
                                  const RelAbsVector_t* basePoint2_z);


/**
 * Unsets the value of the "basePoint1_x" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint1_x(RenderCubicBezier_t * rcb);


/**
 * Unsets the value of the "basePoint1_y" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint1_y(RenderCubicBezier_t * rcb);


/**
 * Unsets the value of the "basePoint1_z" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint1_z(RenderCubicBezier_t * rcb);


/**
 * Unsets the value of the "basePoint2_x" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint2_x(RenderCubicBezier_t * rcb);


/**
 * Unsets the value of the "basePoint2_y" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint2_y(RenderCubicBezier_t * rcb);


/**
 * Unsets the value of the "basePoint2_z" element of this RenderCubicBezier_t.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_unsetBasePoint2_z(RenderCubicBezier_t * rcb);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RenderCubicBezier_t object have been set.
 *
 * @param rcb the RenderCubicBezier_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * RenderCubicBezier_t have been set, otherwise @c 0 (false) is returned.
 *
 * @note The required attributes for the RenderCubicBezier_t object are:
 * @li "basePoint1_x"
 * @li "basePoint1_y"
 * @li "basePoint2_x"
 * @li "basePoint2_y"
 *
 * @memberof RenderCubicBezier_t
 */
LIBSBML_EXTERN
int
RenderCubicBezier_hasRequiredAttributes(const RenderCubicBezier_t * rcb);



END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !RenderCubicBezier_H__ */


