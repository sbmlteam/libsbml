/**
 * @file    Ellipse.h
 * @brief Definition of the Ellipse class.
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
 * @class Ellipse
 * @sbmlbrief{render} Graphical representation of an ellipse.
 *
 * The Ellipse class is derived from GraphicalPrimitive2D, so it inherits all
 * the attributes and methods of the latter.  This means ellipses can have a
 * transformation, a stroke and a stroke with to draw the edge as well as a
 * fill style and fill style related settings.  Besides those inherited
 * attributes, an ellipse if defined by its center point which can be
 * specified as a combination of absolute and relative values and its radii
 * for the two axes. The radii can also be specified in terms absolute and/or
 * relative values.
 */

#ifndef Ellipse_H__
#define Ellipse_H__


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


class LIBSBML_EXTERN Ellipse : public GraphicalPrimitive2D
{
protected:
  /** @cond doxygenLibsbmlInternal */
  RelAbsVector mCX;
  RelAbsVector mCY;
  RelAbsVector mCZ;
  RelAbsVector mRX;
  RelAbsVector mRY;
  static const std::string ELEMENT_NAME;
  double mRatio;
  bool mIsSetRatio;
  /** @endcond */

public:

  /**
   * Creates a new Ellipse using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Ellipse.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Ellipse.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this Ellipse.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Ellipse(unsigned int level = RenderExtension::getDefaultLevel(),
          unsigned int version = RenderExtension::getDefaultVersion(),
          unsigned int pkgVersion =
            RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new Ellipse using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Ellipse(RenderPkgNamespaces *renderns);


  /**
   * Creates a new RadialGradient object from the given XMLNode object.
   *
   * The XMLNode object must contain a valid XML representation of a
   * RadialGradient object as defined in the render extension specification.
   * This method is normally called when render information is read from a
   * file and should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the RadialGradient
   * object to be instantiated.

   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Ellipse(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Instantiates a new ellipse object with the center set to 0,0,0
   * and the radii also set to 0.
   * The id is set to the given string.
   *
   * @param renderns SBMLNamespaces object for the SBML Render package
   * @param id the id of the ellipse.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Ellipse(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED


#ifndef OMIT_DEPRECATED
  /**
   * Constructor with 2D center and radius.
   * instantiates a new ellipse object with the center.
   * The z coordinate of the center is set to 0.
   * The id is unset and both radii are set to the given radius.
   *
   * @param renderns SBMLNamespaces object for the SBML Render package
   * @param cx x value of the center point 
   * @param cy y value of the center point 
   * @param r radius along both axis
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Ellipse(RenderPkgNamespaces* renderns, const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& r);
#endif // OMIT_DEPRECATED


#ifndef OMIT_DEPRECATED
  /**
   * Constructor with 2D center and radii.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Ellipse(RenderPkgNamespaces* renderns, const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& rx,const RelAbsVector& ry);
#endif // OMIT_DEPRECATED


#ifndef OMIT_DEPRECATED
  /**
   * Constructor with 3D center and radii.
   * instantiates a new ellipse object with the center and radii.
   * The id is unset.
   *
   * @param renderns SBMLNamespaces object for the SBML Render package
   * @param cx x value of the center point 
   * @param cy y value of the center point 
   * @param cz z value of the center point 
   * @param rx radius along the x axis
   * @param ry radius along the y axis
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Ellipse(RenderPkgNamespaces* renderns, const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& cz,const RelAbsVector& rx,const RelAbsVector& ry);
#endif // OMIT_DEPRECATED


#ifndef OMIT_DEPRECATED
  /**
   * Constructor with id, 2D center and radius.
   * instantiates a new ellipse object with the given @p id and center.
   * Both radii are set to the given radius r. This actually yields a circle.
   *
   * @param renderns SBMLNamespaces object for the SBML Render package
   * @param id id for the ellipse
   * @param cx x value of the center point 
   * @param cy y value of the center point 
   * @param r radius along both axis
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Ellipse(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& r);
#endif // OMIT_DEPRECATED


#ifndef OMIT_DEPRECATED
  /**
   * Constructor with id, 2D center and radii.
   * instantiates a new ellipse object with the given @p id, center and radii.
   *
   * @param renderns SBMLNamespaces object for the SBML Render package
   * @param id id for the ellipse
   * @param cx x value of the center point 
   * @param cy y value of the center point 
   * @param rx radius along the x axis
   * @param ry radius along the y axis
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Ellipse(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& rx,const RelAbsVector& ry);
#endif // OMIT_DEPRECATED


#ifndef OMIT_DEPRECATED
  /**
   * Constructor with id, 3D center and radii.
   * instantiates a new ellipse object with the given @p id, center and radii.
   *
   * @param renderns SBMLNamespaces object for the SBML Render package
   * @param id id for the ellipse
   * @param cx x value of the center point 
   * @param cy y value of the center point 
   * @param cz z value of the center point 
   * @param rx radius along the x axis
   * @param ry radius along the y axis
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Ellipse(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& cz,const RelAbsVector& rx,const RelAbsVector& ry);
#endif // OMIT_DEPRECATED


  /**
   * Copy constructor for Ellipse.
   *
   * @param orig the Ellipse instance to copy.
   */
  Ellipse(const Ellipse& orig);


  /**
   * Assignment operator for Ellipse.
   *
   * @param rhs the Ellipse object whose values are to be used as the basis
   * of the assignment.
   */
  Ellipse& operator=(const Ellipse& rhs);


  /**
   * Creates and returns a deep copy of this Ellipse object.
   *
   * @return a (deep) copy of this Ellipse object.
   */
  virtual Ellipse* clone() const;


  /**
   * Destructor for Ellipse.
   */
  virtual ~Ellipse();


  /**
   * Returns the value of the "ratio" attribute of this Ellipse.
   *
   * @return the value of the "ratio" attribute of this Ellipse as a double.
   */
  double getRatio() const;


  /**
   * Predicate returning @c true if this Ellipse's "ratio" attribute is set.
   *
   * @return @c true if this Ellipse's "ratio" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetRatio() const;


  /**
   * Sets the value of the "ratio" attribute of this Ellipse.
   *
   * @param ratio double value of the "ratio" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setRatio(double ratio);


  /**
   * Unsets the value of the "ratio" attribute of this Ellipse.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRatio();


  /**
   * Returns the x coordinate for the center point as a const reference.
   *
   * @return const reference to the x coordinate of the center point.
   */
  const RelAbsVector& getCX() const;


  /**
   * Returns the y coordinate for the center point as a const reference.
   *
   * @return const reference to the y coordinate of the center point.
   */
  const RelAbsVector& getCY() const;


  /**
   * Returns the z coordinate for the center point as a const reference.
   *
   * @return const reference to the z coordinate of the center point.
   */
  const RelAbsVector& getCZ() const;


  /**
   * Returns the radius along the x axis as a const reference.
   *
   * @return const reference to the radius along the x axis
   */
  const RelAbsVector& getRX() const;


  /**
   * Returns the radius along the y axis as a const reference.
   *
   * @return const reference to the radius along the y axis
   */
  const RelAbsVector& getRY() const;


  /**
   * Returns the x coordinate for the center point as a reference.
   *
   * @return reference to the x coordinate of the center point.
   */
  RelAbsVector& getCX();


  /**
   * Returns the y coordinate for the center point as a reference.
   *
   * @return reference to the y coordinate of the center point.
   */
  RelAbsVector& getCY();


  /**
   * Returns the z coordinate for the center point as a reference.
   *
   * @return reference to the z coordinate of the center point.
   */
  RelAbsVector& getCZ();


  /**
   * Returns the radius along the x axis as a reference.
   *
   * @return reference to the radius along the x axis
   */
  RelAbsVector& getRX();


  /**
   * Returns the radius along the y axis as a reference.
   *
   * @return reference to the radius along the y axis
   */
  RelAbsVector& getRY();


  /**
   * Predicate returning @c true if this Ellipse's "cx" element is set.
   *
   * @return @c true if this Ellipse's "cx" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetCX() const;


  /**
   * Predicate returning @c true if this Ellipse's "cy" element is set.
   *
   * @return @c true if this Ellipse's "cy" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetCY() const;


  /**
   * Predicate returning @c true if this Ellipse's "cz" element is set.
   *
   * @return @c true if this Ellipse's "cz" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetCZ() const;


  /**
   * Predicate returning @c true if this Ellipse's "rx" element is set.
   *
   * @return @c true if this Ellipse's "rx" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetRX() const;


  /**
   * Predicate returning @c true if this Ellipse's "ry" element is set.
   *
   * @return @c true if this Ellipse's "ry" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetRY() const;


  /**
   * Sets the x coordinates for the center point.
   *
   * @param cx x value of the center point 
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setCX(const RelAbsVector& cx);


  /**
   * Sets the y coordinates for the center point.
   *
   * @param cy y value of the center point 
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setCY(const RelAbsVector& cy);


  /**
   * Sets the z coordinates for the center point.
   *
   * @param cz z value of the center point 
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setCZ(const RelAbsVector& cz);


  /**
   * Sets the radius along the x axis
   *
   * @param rx radius along the x axis
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRX(const RelAbsVector& rx);


  /**
   * Sets the radius along the y axis
   *
   * @param ry radius along the y axis
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRY(const RelAbsVector& ry);


  /**
   * Sets the 2D coordinates for the center point.
   * The z coodintate is set to 50%
   *
   * @param cx x value of the center point 
   * @param cy y value of the center point 
   */
  void setCenter2D(const RelAbsVector& cx,const RelAbsVector& cy);


  /**
   * Sets the 3D coordinates for the center point.
   *
   * @param cx x value of the center point 
   * @param cy y value of the center point 
   * @param cz z value of the center point 
   */
  void setCenter3D(const RelAbsVector& cx,const RelAbsVector& cy,const RelAbsVector& cz);


  /**
   * Sets the radii of the ellipse
   *
   * @param rx radius along the x axis
   * @param ry radius along the y axis
   */
  void setRadii(const RelAbsVector& rx,const RelAbsVector& ry);


  /**
   * Unsets the value of the "cx" element of this Ellipse.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetCX();


  /**
   * Unsets the value of the "cy" element of this Ellipse.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetCY();


  /**
   * Unsets the value of the "cz" element of this Ellipse.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetCZ();


  /**
   * Unsets the value of the "rx" element of this Ellipse.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetRX();


  /**
   * Unsets the value of the "ry" element of this Ellipse.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetRY();


  /**
   * Returns the XML element name of this Ellipse object.
   *
   * For Ellipse, the XML element name is always @c "ellipse".
   *
   * @return the name of this element, i.e. @c "ellipse".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Ellipse object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_ELLIPSE, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * Ellipse object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * Ellipse have been set, otherwise @c false is returned.
   *
   * @note The required attributes for the Ellipse object are:
   * @li "cx"
   * @li "cy"
   * @li "rx"
   */
  virtual bool hasRequiredAttributes() const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;
  /** @endcond */


  /**
   * Creates an XMLNode object from this Ellipse object.
   *
   * @return the XMLNode with the XML representation for the 
   * Ellipse object.
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
 * Creates a new Ellipse_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Ellipse_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Ellipse_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Ellipse_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
Ellipse_t *
Ellipse_create(unsigned int level,
               unsigned int version,
               unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Ellipse_t object.
 *
 * @param e the Ellipse_t structure.
 *
 * @return a (deep) copy of this Ellipse_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
Ellipse_t*
Ellipse_clone(const Ellipse_t* e);


/**
 * Frees this Ellipse_t object.
 *
 * @param e the Ellipse_t structure.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
void
Ellipse_free(Ellipse_t* e);


/**
 * Returns the value of the "ratio" attribute of this Ellipse_t.
 *
 * @param e the Ellipse_t structure whose ratio is sought.
 *
 * @return the value of the "ratio" attribute of this Ellipse_t as a double.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
double
Ellipse_getRatio(const Ellipse_t * e);


/**
 * Predicate returning @c 1 (true) if this Ellipse_t's "ratio" attribute is
 * set.
 *
 * @param e the Ellipse_t structure.
 *
 * @return @c 1 (true) if this Ellipse_t's "ratio" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_isSetRatio(const Ellipse_t * e);


/**
 * Sets the value of the "ratio" attribute of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @param ratio double value of the "ratio" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_setRatio(Ellipse_t * e, double ratio);


/**
 * Unsets the value of the "ratio" attribute of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_unsetRatio(Ellipse_t * e);


/**
 * Returns the value of the "cx" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure whose cx is sought.
 *
 * @return the value of the "cx" element of this Ellipse_t as a RelAbsVector_t.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Ellipse_getCX(const Ellipse_t * e);


/**
 * Returns the value of the "cy" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure whose cy is sought.
 *
 * @return the value of the "cy" element of this Ellipse_t as a RelAbsVector_t.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Ellipse_getCY(const Ellipse_t * e);


/**
 * Returns the value of the "cz" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure whose cz is sought.
 *
 * @return the value of the "cz" element of this Ellipse_t as a RelAbsVector_t.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Ellipse_getCZ(const Ellipse_t * e);


/**
 * Returns the value of the "rx" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure whose rx is sought.
 *
 * @return the value of the "rx" element of this Ellipse_t as a RelAbsVector_t.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Ellipse_getRX(const Ellipse_t * e);


/**
 * Returns the value of the "ry" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure whose ry is sought.
 *
 * @return the value of the "ry" element of this Ellipse_t as a RelAbsVector_t.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Ellipse_getRY(const Ellipse_t * e);


/**
 * Predicate returning @c 1 (true) if this Ellipse_t's "cx" element is set.
 *
 * @param e the Ellipse_t structure.
 *
 * @return @c 1 (true) if this Ellipse_t's "cx" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_isSetCX(const Ellipse_t * e);


/**
 * Predicate returning @c 1 (true) if this Ellipse_t's "cy" element is set.
 *
 * @param e the Ellipse_t structure.
 *
 * @return @c 1 (true) if this Ellipse_t's "cy" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_isSetCY(const Ellipse_t * e);


/**
 * Predicate returning @c 1 (true) if this Ellipse_t's "cz" element is set.
 *
 * @param e the Ellipse_t structure.
 *
 * @return @c 1 (true) if this Ellipse_t's "cz" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_isSetCZ(const Ellipse_t * e);


/**
 * Predicate returning @c 1 (true) if this Ellipse_t's "rx" element is set.
 *
 * @param e the Ellipse_t structure.
 *
 * @return @c 1 (true) if this Ellipse_t's "rx" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_isSetRX(const Ellipse_t * e);


/**
 * Predicate returning @c 1 (true) if this Ellipse_t's "ry" element is set.
 *
 * @param e the Ellipse_t structure.
 *
 * @return @c 1 (true) if this Ellipse_t's "ry" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_isSetRY(const Ellipse_t * e);


/**
 * Sets the value of the "cx" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @param cx RelAbsVector&_t value of the "cx" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_setCX(Ellipse_t * e, const RelAbsVector_t* cx);


/**
 * Sets the value of the "cy" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @param cy RelAbsVector& value of the "cy" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_setCY(Ellipse_t * e, const RelAbsVector_t& cy);


/**
 * Sets the value of the "cz" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @param cz RelAbsVector& value of the "cz" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_setCZ(Ellipse_t * e, const RelAbsVector_t& cz);


/**
 * Sets the value of the "rx" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @param rx RelAbsVector& value of the "rx" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_setRX(Ellipse_t * e, const RelAbsVector_t& rx);


/**
 * Sets the value of the "ry" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @param ry RelAbsVector& value of the "ry" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_setRY(Ellipse_t * e, const RelAbsVector_t& ry);




/**
 * Unsets the value of the "cx" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_unsetCX(Ellipse_t * e);


/**
 * Unsets the value of the "cy" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_unsetCY(Ellipse_t * e);


/**
 * Unsets the value of the "cz" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_unsetCZ(Ellipse_t * e);


/**
 * Unsets the value of the "rx" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_unsetRX(Ellipse_t * e);


/**
 * Unsets the value of the "ry" element of this Ellipse_t.
 *
 * @param e the Ellipse_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_unsetRY(Ellipse_t * e);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Ellipse_t object have been set.
 *
 * @param e the Ellipse_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Ellipse_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Ellipse_t
 */
LIBSBML_EXTERN
int
Ellipse_hasRequiredAttributes(const Ellipse_t * e);






END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Ellipse_H__ */
