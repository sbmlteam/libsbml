/**
 * @file    Transformation.h
 * @brief   abstract class for representing 3D affine transformations
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
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class Transformation
 * @sbmlbrief{render} Implementation of a 3D transformation matrix.
 *
 * The Transformation class represents a 3D transformation which normally is
 * a 4x4 matrix.  Since the last row is always 0 0 0 1 for affine
 * transformations, we leave out those values and store the matrix as an
 * array of 4x3 columns
 */

#ifndef Transformation_H__
#define Transformation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class Image;
class Ellipse;
class Rectangle;
class Polygon;
class RenderGroup;
class LineEnding;
class Text;
class RenderCurve;

class LIBSBML_EXTERN Transformation : public SBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  double mMatrix[12];
  static const double IDENTITY3D[12];
  static const double NAN3D[12];
  int mTransformLength;
  /** @endcond */

protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * Creates a new Transformation object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * Transformation object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the Transformation
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  Transformation(const XMLNode& node, unsigned int l2version=4);
  /** @endcond */

public:

  /**
   * Creates a new Transformation using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * Transformation.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Transformation.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this Transformation.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Transformation(unsigned int level = RenderExtension::getDefaultLevel(),
                 unsigned int version = RenderExtension::getDefaultVersion(),
                 unsigned int pkgVersion =
                   RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new Transformation using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Transformation(RenderPkgNamespaces *renderns);


  /**
   * Copy constructor for Transformation.
   *
   * @param orig the Transformation instance to copy.
   */
  Transformation(const Transformation& orig);


  /**
   * Assignment operator for Transformation.
   *
   * @param rhs the Transformation object whose values are to be used as the
   * basis of the assignment.
   */
  Transformation& operator=(const Transformation& rhs);


  /**
   * Creates and returns a deep copy of this Transformation object.
   *
   * @return a (deep) copy of this Transformation object.
   */
  virtual Transformation* clone() const;


  /**
   * Destructor for Transformation.
   */
  virtual ~Transformation();


  /**
   * Returns the value of the "transform" attribute of this Transformation.
   *
   * @param outArray double* array that will be used to return the value of the
   * "transform" attribute of this Transformation.
   *
   * @note the value of the "transform" attribute of this Transformation is
   * returned in the argument array.
   */
  void getTransform(double* outArray) const;


  /**
   * Returns the value of the "name" attribute of this Transformation.
   *
   * @return the value of the "name" attribute of this Transformation as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "transform" attribute of this Transformation.
   * which is an array of double values of length 12.
   *
   * @return a pointer to the array of numbers for the transformation.
   */
  const double* getMatrix() const;

  /**
   * Returns a 3D identity matrix.
   * The matrix contains 12 double values.
   */
  static const double* getIdentityMatrix();


  /**
   * Predicate returning @c true if this Transformation's "transform" attribute
   * is set.
   *
   * @return @c true if this Transformation's "transform" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetTransform() const;


  /**
   * Predicate returning @c true if this Transformation's "name" attribute is
   * set.
   *
   * @return @c true if this Transformation's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Returns @c true if the matrix has been set or @c false otherwise.
   * The matrix is considered as set if none of the values in the matrix is NaN.
   *
   * @return @c true or @c false depending on whether a NaN was found.
   */
  bool isSetMatrix() const;
  


  /**
   * Sets the value of the "transform" attribute of this Transformation.
   *
   * @param inArray double* array value of the "transform" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTransform(double* inArray);


  /**
   * Sets the value of the "name" attribute of this Transformation.
   *
   * @param name std::string& value of the "name" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p name = @c NULL or an empty string is
   * equivalent to calling unsetName().
   */
  virtual int setName(const std::string& name);


  /**
   * Sets the matrix to the values given in the array.
   *
   * @param m array with new values to be set for this Transformation object.
   */
  void setMatrix(const double m[12]);

  /**
   * Unsets the value of the "transform" attribute of this Transformation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetTransform();


  /**
   * Unsets the value of the "name" attribute of this Transformation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
  * Unsets the value of the "transform" attribute of this Transformation.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
  */
  int unsetMatrix();


  /**
   * Predicate returning @c true if this abstract Transformation is of type
   * Image
   *
   * @return @c true if this abstract Transformation is of type Image,
   * @c false otherwise
   */
  virtual bool isImage() const;


  /**
   * Predicate returning @c true if this abstract Transformation is of type
   * Ellipse
   *
   * @return @c true if this abstract Transformation is of type Ellipse,
   * @c false otherwise
   */
  virtual bool isEllipse() const;


  /**
   * Predicate returning @c true if this abstract Transformation is of type
   * Rectangle
   *
   * @return @c true if this abstract Transformation is of type Rectangle,
   * @c false otherwise
   */
  virtual bool isRectangle() const;


  /**
   * Predicate returning @c true if this abstract Transformation is of type
   * Polygon
   *
   * @return @c true if this abstract Transformation is of type Polygon,
   * @c false otherwise
   */
  virtual bool isPolygon() const;


  /**
   * Predicate returning @c true if this abstract Transformation is of type
   * RenderGroup
   *
   * @return @c true if this abstract Transformation is of type RenderGroup,
   * @c false otherwise
   */
  virtual bool isRenderGroup() const;


  /**
   * Predicate returning @c true if this abstract Transformation is of type
   * LineEnding
   *
   * @return @c true if this abstract Transformation is of type LineEnding,
   * @c false otherwise
   */
  virtual bool isLineEnding() const;


  /**
   * Predicate returning @c true if this abstract Transformation is of type
   * Text
   *
   * @return @c true if this abstract Transformation is of type Text,
   * @c false otherwise
   */
  virtual bool isText() const;


  /**
   * Predicate returning @c true if this abstract Transformation is of type
   * RenderCurve
   *
   * @return @c true if this abstract Transformation is of type RenderCurve,
   * @c false otherwise
   */
  virtual bool isRenderCurve() const;


  /**
   * Returns the XML element name of this Transformation object.
   *
   * For Transformation, the XML element name is always @c "transformation".
   *
   * @return the name of this element, i.e. @c "transformation".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Transformation object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_TRANSFORMATION, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write any contained elements
   */
  virtual void writeElements(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */





  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, bool& value)
    const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           double& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           unsigned int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           std::string& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this Transformation's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this Transformation's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, bool value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, double value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           unsigned int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           const std::string& value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this Transformation.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */




  #endif /* !SWIG */


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
 * Creates a new Image (Transformation_t) using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Transformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Transformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Transformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createImage(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion);


/**
 * Creates a new Ellipse (Transformation_t) using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Transformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Transformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Transformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createEllipse(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion);


/**
 * Creates a new Rectangle (Transformation_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Transformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Transformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Transformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createRectangle(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion);


/**
 * Creates a new Polygon (Transformation_t) using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Transformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Transformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Transformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createPolygon(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion);


/**
 * Creates a new RenderGroup (Transformation_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Transformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Transformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Transformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createRenderGroup(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion);


/**
 * Creates a new LineEnding (Transformation_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Transformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Transformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Transformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createLineEnding(unsigned int level,
                                unsigned int version,
                                unsigned int pkgVersion);


/**
 * Creates a new Text (Transformation_t) using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Transformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Transformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Transformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createText(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion);


/**
 * Creates a new RenderCurve (Transformation_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Transformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Transformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Transformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
Transformation_t *
Transformation_createRenderCurve(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Transformation_t object.
 *
 * @param t the Transformation_t structure.
 *
 * @return a (deep) copy of this Transformation_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
Transformation_t*
Transformation_clone(const Transformation_t* t);


/**
 * Frees this Transformation_t object.
 *
 * @param t the Transformation_t structure.
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
void
Transformation_free(Transformation_t* t);


/**
 * Returns the value of the "name" attribute of this Transformation_t.
 *
 * @param t the Transformation_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this Transformation_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
char *
Transformation_getName(const Transformation_t * t);


/**
 * Predicate returning @c 1 (true) if this Transformation_t's "name" attribute
 * is set.
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 (true) if this Transformation_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_isSetName(const Transformation_t * t);


/**
* Predicate returning @c 1 (true) if this Transformation_t's "transform" attribute
* is set.
*
* @param t the Transformation_t structure.
*
* @return @c 1 (true) if this Transformation_t's "transform" attribute has been
* set, otherwise @c 0 (false) is returned.
*
* @memberof Transformation_t
*/
LIBSBML_EXTERN
int
Transformation_isSetTranform(const Transformation_t * t);


/**
 * Sets the value of the "name" attribute of this Transformation_t.
 *
 * @param t the Transformation_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling Transformation_unsetName().
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_setName(Transformation_t * t, const char * name);


/**
* Sets the value of the "transform" attribute of this Transformation_t.
*
* @param t the Transformation_t structure.
*
* @param transform double * array of the "transform" attribute to be set.
*
* @copydetails doc_returns_success_code
* @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
* @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
*
* @memberof Transformation_t
*/
LIBSBML_EXTERN
int
Transformation_setTransform(Transformation_t * t, double* transform);


/**
 * Unsets the value of the "name" attribute of this Transformation_t.
 *
 * @param t the Transformation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_unsetName(Transformation_t * t);


/**
 * Predicate returning @c 1 if this Transformation_t is of type Image_t
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 if this Transformation_t is of type Image_t, @c 0 otherwise
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_isImage(const Transformation_t * t);


/**
 * Predicate returning @c 1 if this Transformation_t is of type Ellipse_t
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 if this Transformation_t is of type Ellipse_t, @c 0 otherwise
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_isEllipse(const Transformation_t * t);


/**
 * Predicate returning @c 1 if this Transformation_t is of type Rectangle_t
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 if this Transformation_t is of type Rectangle_t, @c 0 otherwise
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_isRectangle(const Transformation_t * t);


/**
 * Predicate returning @c 1 if this Transformation_t is of type Polygon_t
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 if this Transformation_t is of type Polygon_t, @c 0 otherwise
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_isPolygon(const Transformation_t * t);


/**
 * Predicate returning @c 1 if this Transformation_t is of type RenderGroup_t
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 if this Transformation_t is of type RenderGroup_t, @c 0
 * otherwise
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_isRenderGroup(const Transformation_t * t);


/**
 * Predicate returning @c 1 if this Transformation_t is of type LineEnding_t
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 if this Transformation_t is of type LineEnding_t, @c 0
 * otherwise
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_isLineEnding(const Transformation_t * t);


/**
 * Predicate returning @c 1 if this Transformation_t is of type Text_t
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 if this Transformation_t is of type Text_t, @c 0 otherwise
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_isText(const Transformation_t * t);


/**
 * Predicate returning @c 1 if this Transformation_t is of type RenderCurve_t
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 if this Transformation_t is of type RenderCurve_t, @c 0
 * otherwise
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_isRenderCurve(const Transformation_t * t);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Transformation_t object have been set.
 *
 * @param t the Transformation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Transformation_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the Transformation_t object are:
 * @li "transform"
 *
 * @memberof Transformation_t
 */
LIBSBML_EXTERN
int
Transformation_hasRequiredAttributes(const Transformation_t * t);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Transformation_H__ */


