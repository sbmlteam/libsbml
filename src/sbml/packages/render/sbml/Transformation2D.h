/**
 * @file    Transformation2D.h
 * @brief   abstract base class for representing 2D affine transformations
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
 * @class Transformation2D
 * @sbmlbrief{render} Implementation of a 2D transformation matrix.
 *
 * The Transformation2D class represents a 2D transformation. it is derived
 * from Transformation and inherits all the attributes of a 3D
 * transformation. In addition is provides new methods to explicitly get and
 * set 2D transformation properties.  A 2D transformation normally consists
 * of a 3x3 matrix, but since the last row is always 0 0 1, this is reduced
 * to a 6 value array.
 *
 * Using one of the new 2D specific functions to set the matrix always
 * updates the 3D matrix automatically and vice versa, so the 2D data and the
 * 3D data inherited from Transformation should always be consistent.
 */

#ifndef Transformation2D_H__
#define Transformation2D_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/Transformation.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/xml/XMLNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class Image;
class Ellipse;
class Rectangle;
class Polygon;
class RenderGroup;
class LineEnding;
class Text;
class RenderCurve;

class LIBSBML_EXTERN Transformation2D : public Transformation
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mElementName;
  static const double IDENTITY2D[6];
  double mMatrix2D[6];

  /** @endcond */

public:

  /**
   * Creates a new Transformation2D using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * Transformation2D.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Transformation2D.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this Transformation2D.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Transformation2D(unsigned int level = RenderExtension::getDefaultLevel(),
                   unsigned int version = RenderExtension::getDefaultVersion(),
                   unsigned int pkgVersion =
                     RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new Transformation2D using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Transformation2D(RenderPkgNamespaces *renderns);

  /**
   * Creates a new Transformation2D object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * Transformation2D object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the Transformation2D
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  Transformation2D(const XMLNode& node, unsigned int l2version=4);


  /**
   * Copy constructor for Transformation2D.
   *
   * @param orig the Transformation2D instance to copy.
   */
  Transformation2D(const Transformation2D& orig);


  /**
   * Assignment operator for Transformation2D.
   *
   * @param rhs the Transformation2D object whose values are to be used as the
   * basis of the assignment.
   */
  Transformation2D& operator=(const Transformation2D& rhs);


  /**
   * Creates and returns a deep copy of this Transformation2D object.
   *
   * @return a (deep) copy of this Transformation2D object.
   */
  virtual Transformation2D* clone() const;


  /**
   * Destructor for Transformation2D.
   */
  virtual ~Transformation2D();


#ifndef OMIT_DEPRECATED
  /**
   * Instantiates a Transformation with all numerical values set to 
   * the values in the given array.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Transformation2D(RenderPkgNamespaces* renderns, const double m[6]);
#endif // OMIT_DEPRECATED

  /**
   * Predicate returning @c true if this abstract Transformation2D is of type
   * Image
   *
   * @return @c true if this abstract Transformation2D is of type Image,
   * @c false otherwise
   */
  virtual bool isImage() const;


  /**
   * Predicate returning @c true if this abstract Transformation2D is of type
   * Ellipse
   *
   * @return @c true if this abstract Transformation2D is of type Ellipse,
   * @c false otherwise
   */
  virtual bool isEllipse() const;


  /**
   * Predicate returning @c true if this abstract Transformation2D is of type
   * Rectangle
   *
   * @return @c true if this abstract Transformation2D is of type Rectangle,
   * @c false otherwise
   */
  virtual bool isRectangle() const;


  /**
   * Predicate returning @c true if this abstract Transformation2D is of type
   * Polygon
   *
   * @return @c true if this abstract Transformation2D is of type Polygon,
   * @c false otherwise
   */
  virtual bool isPolygon() const;


  /**
   * Predicate returning @c true if this abstract Transformation2D is of type
   * RenderGroup
   *
   * @return @c true if this abstract Transformation2D is of type
   * RenderGroup, @c false otherwise
   */
  virtual bool isRenderGroup() const;


  /**
   * Predicate returning @c true if this abstract Transformation2D is of type
   * LineEnding
   *
   * @return @c true if this abstract Transformation2D is of type LineEnding,
   * @c false otherwise
   */
  virtual bool isLineEnding() const;


  /**
   * Predicate returning @c true if this abstract Transformation2D is of type
   * Text
   *
   * @return @c true if this abstract Transformation2D is of type Text,
   * @c false otherwise
   */
  virtual bool isText() const;


  /**
   * Predicate returning @c true if this abstract Transformation2D is of type
   * RenderCurve
   *
   * @return @c true if this abstract Transformation2D is of type
   * RenderCurve, @c false otherwise
   */
  virtual bool isRenderCurve() const;


  /**
   * Returns a 2D identity matrix.
   * The matrix contains 6 double values.
   */
  static const double* getIdentityMatrix2D();

  /**
   * Sets the 2D matrix to the values given in the array.
   * The 3D matrix is updated accordingly.
   *
   * @param m array with new values to be set for this Transformation object.
   */
  void setMatrix2D(const double m[6]);

  /**
   * Sets the 2D matrix to the values given in the array.
   * The 2D matrix is updated accordingly.
   *
   * @param m array with new values to be set for this Transformation object.
   */
  void setMatrix(const double m[12]);

  /**
   * Returns the 2D matrix which is an array of double values of length 6.
   *
   * @return a pointer to the array of numbers for the 2D transformation.
   */
  const double* getMatrix2D() const;

  /**
   * Returns the XML element name of this Transformation2D object.
   *
   * For Transformation2D, the XML element name is always
   * @c "transformation2D".
   *
   * @return the name of this element, i.e. @c "transformation2D".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this Transformation2D object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this Transformation2D object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_TRANSFORMATION2D, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * Transformation2D object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * Transformation2D have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;



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



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument
   */
  virtual void setSBMLDocument(SBMLDocument* d);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this Transformation2D.
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
   * Returns the value of the "attributeName" attribute of this Transformation2D.
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
   * Returns the value of the "attributeName" attribute of this Transformation2D.
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
   * Returns the value of the "attributeName" attribute of this Transformation2D.
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
   * Returns the value of the "attributeName" attribute of this Transformation2D.
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
   * Predicate returning @c true if this Transformation2D's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this Transformation2D's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Transformation2D.
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
   * Sets the value of the "attributeName" attribute of this Transformation2D.
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
   * Sets the value of the "attributeName" attribute of this Transformation2D.
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
   * Sets the value of the "attributeName" attribute of this Transformation2D.
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
   * Sets the value of the "attributeName" attribute of this Transformation2D.
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
   * Unsets the value of the "attributeName" attribute of this
   * Transformation2D.
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


  /**
   * Creates an XMLNode object from this Transformation2D object.
   *
   * @return the XMLNode with the XML representation for the 
   * Transformation2D object.
   *
   * This method is purely virtual and has to be overwritten by derived classes.
   */
  virtual XMLNode toXML() const = 0;

protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new object from the next XMLToken on the XMLInputStream
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



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




  /** @cond doxygenLibsbmlInternal */
  /**
   * Tries to parse the numerical values from the given string
   * and fill the matrix with them. The method will accept strings
   * representing 6 or 12 numerical values and fill the 2D or 3D matrix
   * accordingly.
   * The other matrix is updated automatically.
   *
   * @param transformationString string representing 6 or 12 numerical values.
   */
  void parseTransformation(const std::string& transformationString);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Adds the transformation attribute to the given XMLAttributes object.
   *
   * @param transformation the transformation to add as attribute.
   *
   * @param att XMLAttributes where the attribute needs to be added to
   */
  static void addTransformation2DAttributes(const Transformation2D& transformation,XMLAttributes& att);
  /** @endcond */



  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns the transformation array as a string for storage in an XML
   * attribute.
   */
  std::string get2DTransformationString() const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the 3D matrix from the 2D matrix.
   */
  void updateMatrix3D();
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Fills the 2D matrix with data from the 3D matrix.
   */
  void updateMatrix2D();
  /** @endcond */

};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
* Creates a new Image (Transformation2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* Transformation2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* Transformation2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* Transformation2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof Transformation2D_t
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createImage(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new Ellipse (Transformation2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* Transformation2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* Transformation2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* Transformation2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof Transformation2D_t
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createEllipse(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new Rectangle (Transformation2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* Transformation2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* Transformation2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* Transformation2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof Transformation2D_t
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createRectangle(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new Polygon (Transformation2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* Transformation2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* Transformation2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* Transformation2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof Transformation2D_t
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createPolygon(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new RenderGroup (Transformation2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* Transformation2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* Transformation2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* Transformation2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof Transformation2D_t
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createRenderGroup(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new Text (Transformation2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* Transformation2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* Transformation2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* Transformation2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof Transformation2D_t
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createText(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new RenderCurve (Transformation2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* Transformation2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* Transformation2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* Transformation2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof Transformation2D_t
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createRenderCurve(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new LineEnding (Transformation2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* Transformation2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* Transformation2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* Transformation2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof Transformation2D_t
*/
LIBSBML_EXTERN
Transformation2D_t *
Transformation2D_createLineEnding(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Transformation2D_t object.
 *
 * @param td the Transformation2D_t structure.
 *
 * @return a (deep) copy of this Transformation2D_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
Transformation2D_t*
Transformation2D_clone(const Transformation2D_t* td);


/**
 * Frees this Transformation2D_t object.
 *
 * @param td the Transformation2D_t structure.
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
void
Transformation2D_free(Transformation2D_t* td);


/**
 * Predicate returning @c 1 if this Transformation2D_t is of type Image_t
 *
 * @param td the Transformation2D_t structure.
 *
 * @return @c 1 if this Transformation2D_t is of type Image_t, @c 0 otherwise
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
int
Transformation2D_isImage(const Transformation2D_t * td);


/**
 * Predicate returning @c 1 if this Transformation2D_t is of type Ellipse_t
 *
 * @param td the Transformation2D_t structure.
 *
 * @return @c 1 if this Transformation2D_t is of type Ellipse_t, @c 0 otherwise
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
int
Transformation2D_isEllipse(const Transformation2D_t * td);


/**
 * Predicate returning @c 1 if this Transformation2D_t is of type Rectangle_t
 *
 * @param td the Transformation2D_t structure.
 *
 * @return @c 1 if this Transformation2D_t is of type Rectangle_t, @c 0
 * otherwise
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
int
Transformation2D_isRectangle(const Transformation2D_t * td);


/**
 * Predicate returning @c 1 if this Transformation2D_t is of type Polygon_t
 *
 * @param td the Transformation2D_t structure.
 *
 * @return @c 1 if this Transformation2D_t is of type Polygon_t, @c 0 otherwise
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
int
Transformation2D_isPolygon(const Transformation2D_t * td);


/**
 * Predicate returning @c 1 if this Transformation2D_t is of type RenderGroup_t
 *
 * @param td the Transformation2D_t structure.
 *
 * @return @c 1 if this Transformation2D_t is of type RenderGroup_t, @c 0
 * otherwise
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
int
Transformation2D_isRenderGroup(const Transformation2D_t * td);


/**
 * Predicate returning @c 1 if this Transformation2D_t is of type LineEnding_t
 *
 * @param td the Transformation2D_t structure.
 *
 * @return @c 1 if this Transformation2D_t is of type LineEnding_t, @c 0
 * otherwise
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
int
Transformation2D_isLineEnding(const Transformation2D_t * td);


/**
 * Predicate returning @c 1 if this Transformation2D_t is of type Text_t
 *
 * @param td the Transformation2D_t structure.
 *
 * @return @c 1 if this Transformation2D_t is of type Text_t, @c 0 otherwise
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
int
Transformation2D_isText(const Transformation2D_t * td);


/**
 * Predicate returning @c 1 if this Transformation2D_t is of type RenderCurve_t
 *
 * @param td the Transformation2D_t structure.
 *
 * @return @c 1 if this Transformation2D_t is of type RenderCurve_t, @c 0
 * otherwise
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
int
Transformation2D_isRenderCurve(const Transformation2D_t * td);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Transformation2D_t object have been set.
 *
 * @param td the Transformation2D_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Transformation2D_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Transformation2D_t
 */
LIBSBML_EXTERN
int
Transformation2D_hasRequiredAttributes(const Transformation2D_t * td);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Transformation2D_H__ */


