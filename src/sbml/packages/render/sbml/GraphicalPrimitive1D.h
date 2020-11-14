/**
 * @file    GraphicalPrimitive1D.h
 * @brief Definition of the GraphicalPrimitive1D class.
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
 * @class GraphicalPrimitive1D
 * @sbmlbrief{render} Base class for graphical primitives implementing 1D attributes.
 *
 * The GraphicalPrimitive1D class implements attributes and methods necessary
 * for 1D objects like lines. The attributes that are implemented are a
 * stroke color, a stroke width and a stroke dasharray for dashed line
 * drawing.  Additionally, this class adds an id attribute with which all
 * graphical primitives can be referenced.
 *
 * The GraphicalPrimitive1D class is derived from Transformation2D and
 * inherits all its methods and attributes.
 */

#ifndef GraphicalPrimitive1D_H__
#define GraphicalPrimitive1D_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>

#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/Transformation2D.h>
#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus

#include <string>
#include <vector>

LIBSBML_CPP_NAMESPACE_BEGIN


class Ellipse;
class Rectangle;
class Polygon;
class RenderGroup;
class LineEnding;
class Text;
class RenderCurve;

class LIBSBML_EXTERN GraphicalPrimitive1D : public Transformation2D
{
protected:
  /** @cond doxygenLibsbmlInternal */

   std::string mStroke;
   double mStrokeWidth;
  bool mIsSetStrokeWidth;
   std::vector<unsigned int> mStrokeDashArray;

  /** @endcond */

public:

  /**
   * Creates a new GraphicalPrimitive1D using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * GraphicalPrimitive1D.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * GraphicalPrimitive1D.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this GraphicalPrimitive1D.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GraphicalPrimitive1D(unsigned int level = RenderExtension::getDefaultLevel(),
                       unsigned int version =
                         RenderExtension::getDefaultVersion(),
                       unsigned int pkgVersion =
                         RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new GraphicalPrimitive1D using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GraphicalPrimitive1D(RenderPkgNamespaces *renderns);

   /**
    * Creates a new GraphicalPrimitive1D object from the given XMLNode object.
    * The XMLNode object has to contain a valid XML representation of a 
    * GraphicalPrimitive1D object as defined in the render extension specification.
    * This method is normally called when render information is read from a file and 
    * should normally not have to be called explicitly.
    *
    * @param node the XMLNode object reference that describes the GraphicalPrimitive1D
    * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
    */
   GraphicalPrimitive1D(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a GraphicalPrimitive1D.
   * The transformation properties are not set, neither is the stroke or the stroke width.
   * The id is set to the given string.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id The id for the GraphicalPrimitive1D object
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  GraphicalPrimitive1D(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED


  /**
   * Copy constructor for GraphicalPrimitive1D.
   *
   * @param orig the GraphicalPrimitive1D instance to copy.
   */
  GraphicalPrimitive1D(const GraphicalPrimitive1D& orig);


  /**
   * Assignment operator for GraphicalPrimitive1D.
   *
   * @param rhs the GraphicalPrimitive1D object whose values are to be used as
   * the basis of the assignment.
   */
  GraphicalPrimitive1D& operator=(const GraphicalPrimitive1D& rhs);


  /**
   * Creates and returns a deep copy of this GraphicalPrimitive1D object.
   *
   * @return a (deep) copy of this GraphicalPrimitive1D object.
   */
  virtual GraphicalPrimitive1D* clone() const;


  /**
   * Destructor for GraphicalPrimitive1D.
   */
  virtual ~GraphicalPrimitive1D();


  /**
   * Returns the value of the "id" attribute of this GraphicalPrimitive1D.
   *
   * @return the value of the "id" attribute of this GraphicalPrimitive1D as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "stroke" attribute of this GraphicalPrimitive1D.
   *
   * @return the value of the "stroke" attribute of this GraphicalPrimitive1D
   * as a string.
   */
  const std::string& getStroke() const;


  /**
   * Returns the value of the "stroke-width" attribute of this
   * GraphicalPrimitive1D.
   *
   * @return the value of the "stroke-width" attribute of this
   * GraphicalPrimitive1D as a double.
   */
  double getStrokeWidth() const;


  /**
   * Returns the value of the "stroke-dasharray" attribute of this
   * GraphicalPrimitive1D.
   *
   * @note the value of the "stroke-dasharray" attribute of this
   * GraphicalPrimitive1D is returned in the argument array.
   */
  const std::vector<unsigned int>& getStrokeDashArray() const;


  /**
   * Returns a const reference to the stroke dasharray.
   *
   * @return const reference to stroke dash array
   */
  const std::vector<unsigned int>& getDashArray() const;
  
  
  /**
   * Returns a reference to the stroke dasharray.
   *
   * @return reference to stroke dash array
   */
  std::vector<unsigned int>& getDashArray();
  

  /**
   * Predicate returning @c true if this GraphicalPrimitive1D's "id" attribute
   * is set.
   *
   * @return @c true if this GraphicalPrimitive1D's "id" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this GraphicalPrimitive1D's "stroke"
   * attribute is set.
   *
   * @return @c true if this GraphicalPrimitive1D's "stroke" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetStroke() const;


  /**
   * Predicate returning @c true if this GraphicalPrimitive1D's "stroke-width"
   * attribute is set.
   *
   * @return @c true if this GraphicalPrimitive1D's "stroke-width" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetStrokeWidth() const;


  /**
   * Predicate returning @c true if this GraphicalPrimitive1D's
   * "stroke-dasharray" attribute is set.
   *
   * @return @c true if this GraphicalPrimitive1D's "stroke-dasharray"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetStrokeDashArray() const;


  /**
   * Returns @c true if the dash array has been set or @c false otherwise.
   * The array is considered set if it is not empty and if the first entry is
   * not @c NaN.
   *
   * @return @c true if the stroke-dasharray is set, @c false otherwise.
   */
  bool isSetDashArray() const;
  
  /**
   * Sets the value of the "id" attribute of this GraphicalPrimitive1D.
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
   * Sets the value of the "stroke" attribute of this GraphicalPrimitive1D.
   *
   * @param stroke std::string& value of the "stroke" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p stroke = @c NULL or an empty string is
   * equivalent to calling unsetStroke().
   */
  int setStroke(const std::string& stroke);


  /**
   * Sets the value of the "stroke-width" attribute of this
   * GraphicalPrimitive1D.
   *
   * @param strokeWidth double value of the "stroke-width" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setStrokeWidth(double strokeWidth);


  /**
   * Sets the value of the "stroke-dasharray" attribute of this
   * GraphicalPrimitive1D.
   *
   * @param array value of the "stroke-dasharray"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setStrokeDashArray(const std::vector<unsigned int>& array);
  
  
  /**
   * Sets the 'stroke-dasharray' attribute to have the values 
   * in the given array.
   *
   * @param array Array of alternating stroke and gap length values.
   */
  void setDashArray(const std::vector<unsigned int>& array);
  
  /**
   * Sets the 'stroke-dasharray' attribute from the given string.
   * If the string is not a valid dasharray string, @c false
   * is returned and the dasharray remains in the state is was
   * before the call.
   *
   * The individual numerical values in the string have to be separated by commas.
   *
   * @param arrayString a string with number representing a dash array.
   *
   * @return @c true if setting 'stroke-dasharray' from the string succeeds or @c false otherwise.
   */
  bool setDashArray(const std::string& arrayString);
  
  
  /** 
   * Returns the number of defined dashes in the 'stroke-dasharray' attribute.
   *
   * @return the number of dashes in the 'stroke-dasharray' attribute.
   */
  unsigned int getNumDashes() const;

  /**
   * Returns the dash at the given index of the 'stroke-dasharray' attribute. 
   *
   * @param index the index of the dash length to return from the 
   * 'stroke-dasharray' attribute.
   *
   * @return the value of the dash length from the 'stroke-dasharray'
   * attribute at the given index, or -1 (cast to an unsigned int)
   * if no such index exists.
   */
  unsigned int getDashByIndex(unsigned int index) const;
  
  /**
  * Adds a new length of a dash to the 'stroke-dasharray' attribute.
  *
  * @param dash the integer length of the dash to add to the end
  * of the 'stroke-dasharray' attribute.
  */
  void addDash(unsigned int dash);

  /**
  * Unsets the value of the "stroke-dasharray" attribute of this
  * GraphicalPrimitive1D.
  */
  void clearDashes();

  /** 
   * Sets the integer value of the dash at the given position.
   *
   * @param index the index of the dash length to replace in the 
   * 'stroke-dasharray' attribute.  The function will fail silently 
   * if no such index exists.
   *
   * @param dash the integer length to set the indexed dash to.
   */
  void setDashByIndex(unsigned int index, unsigned int dash);

  /** 
   * Inserts the given integer value for the dash length at the 
   * given position.
   *
   * @param index the index of the dash length at which the new dash is
   * to be inserted in the 'stroke-dasharray' attribute.  The function
   * will fail silently if no such index exists.
   *
   * @param dash the integer length to set the inserted dash to.
   */
  void insertDash(unsigned int index, unsigned int dash);


  /**
   * Removes the dash length at the given index.
   *
   * @param index the index of the dash length to remove from the
   * 'stroke-dasharray' attribute.
   * The function will fail silently if no such index exists.
   */
  void removeDash(unsigned int index);

  /**
   * Unsets the value of the "id" attribute of this GraphicalPrimitive1D.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "stroke" attribute of this GraphicalPrimitive1D.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStroke();


  /**
   * Unsets the value of the "stroke-width" attribute of this
   * GraphicalPrimitive1D.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStrokeWidth();


  /**
   * Unsets the value of the "stroke-dasharray" attribute of this
   * GraphicalPrimitive1D.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStrokeDashArray();


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
   * type Ellipse
   *
   * @return @c true if this abstract GraphicalPrimitive1D is of type
   * Ellipse, @c false otherwise
   */
  virtual bool isEllipse() const;


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
   * type Rectangle
   *
   * @return @c true if this abstract GraphicalPrimitive1D is of type
   * Rectangle, @c false otherwise
   */
  virtual bool isRectangle() const;


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
   * type Polygon
   *
   * @return @c true if this abstract GraphicalPrimitive1D is of type
   * Polygon, @c false otherwise
   */
  virtual bool isPolygon() const;


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
   * type RenderGroup
   *
   * @return @c true if this abstract GraphicalPrimitive1D is of type
   * RenderGroup, @c false otherwise
   */
  virtual bool isRenderGroup() const;


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
   * type LineEnding
   *
   * @return @c true if this abstract GraphicalPrimitive1D is of type
   * LineEnding, @c false otherwise
   */
  virtual bool isLineEnding() const;


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
   * type Text
   *
   * @return @c true if this abstract GraphicalPrimitive1D is of type Text,
   * @c false otherwise
   */
  virtual bool isText() const;


  /**
  * Predicate returning @c true if this abstract GraphicalPrimitive1D is of
  * type RenderCurve
  *
  * @return @c true if this abstract GraphicalPrimitive1D is of type Text,
  * @c false otherwise
  */
  virtual bool isRenderCurve() const;


  /**
   * Returns the libSBML type code for this GraphicalPrimitive1D object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_GRAPHICALPRIMITIVE1D, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * GraphicalPrimitive1D object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * GraphicalPrimitive1D have been set, otherwise @c false is returned.
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
   * Returns the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * Returns the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * Returns the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * Returns the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * Returns the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * Predicate returning @c true if this GraphicalPrimitive1D's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this GraphicalPrimitive1D's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * Sets the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * Sets the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * Sets the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * Sets the value of the "attributeName" attribute of this
   * GraphicalPrimitive1D.
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
   * GraphicalPrimitive1D.
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
   * Creates an XMLNode object from this GraphicalPrimitive1D object.
   *
   * @return the XMLNode with the XML representation for the 
   * GraphicalPrimitive1D object.
   *
   * This method is purely virtual and has to be implemented by subclasses.
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
   * Adds all set attributes specific to the given GraphicalPrimitive1D objects to the given
   * XMLAttributes object.
   */
  static void addGraphicalPrimitive1DAttributes(const GraphicalPrimitive1D& primitive,XMLAttributes& attributes);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   *  This method parses a dasharray string into the given vector.
   *  The vector is first cleared.
   *  If the dasharray is invalid, false is returned.
   */
  static bool parseDashArray(const std::string& s,std::vector<unsigned int>& array);
  /** @endcond */

};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
* Creates a new Ellipse (GraphicalPrimitive1D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive1D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive1D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive1D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive1D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createEllipse(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new Rectangle (GraphicalPrimitive1D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive1D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive1D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive1D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive1D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createRectangle(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new Polygon (GraphicalPrimitive1D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive1D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive1D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive1D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive1D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createPolygon(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new RenderGroup (GraphicalPrimitive1D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive1D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive1D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive1D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive1D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createRenderGroup(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new LineEnding (GraphicalPrimitive1D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive1D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive1D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive1D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive1D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createLineEnding(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new Text (GraphicalPrimitive1D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive1D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive1D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive1D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive1D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createText(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new RenderCurve (GraphicalPrimitive1D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive1D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive1D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive1D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive1D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive1D_t *
GraphicalPrimitive1D_createRenderCurve(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this GraphicalPrimitive1D_t object.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return a (deep) copy of this GraphicalPrimitive1D_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
GraphicalPrimitive1D_t*
GraphicalPrimitive1D_clone(const GraphicalPrimitive1D_t* gpd);


/**
 * Frees this GraphicalPrimitive1D_t object.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
void
GraphicalPrimitive1D_free(GraphicalPrimitive1D_t* gpd);


/**
 * Returns the value of the "id" attribute of this GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this GraphicalPrimitive1D_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
char *
GraphicalPrimitive1D_getId(const GraphicalPrimitive1D_t * gpd);


/**
 * Returns the value of the "stroke" attribute of this GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure whose stroke is sought.
 *
 * @return the value of the "stroke" attribute of this GraphicalPrimitive1D_t
 * as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
char *
GraphicalPrimitive1D_getStroke(const GraphicalPrimitive1D_t * gpd);


/**
 * Returns the value of the "stroke-width" attribute of this
 * GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure whose stroke-width is
 * sought.
 *
 * @return the value of the "stroke-width" attribute of this
 * GraphicalPrimitive1D_t as a double.
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
double
GraphicalPrimitive1D_getStrokeWidth(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 (true) if this GraphicalPrimitive1D_t's "id"
 * attribute is set.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 (true) if this GraphicalPrimitive1D_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isSetId(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 (true) if this GraphicalPrimitive1D_t's "stroke"
 * attribute is set.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 (true) if this GraphicalPrimitive1D_t's "stroke" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isSetStroke(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 (true) if this GraphicalPrimitive1D_t's
 * "stroke-width" attribute is set.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 (true) if this GraphicalPrimitive1D_t's "stroke-width"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isSetStrokeWidth(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 (true) if this GraphicalPrimitive1D_t's
 * "stroke-dasharray" attribute is set.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 (true) if this GraphicalPrimitive1D_t's "stroke-dasharray"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isSetStrokeDashArray(const GraphicalPrimitive1D_t * gpd);


/**
 * Sets the value of the "id" attribute of this GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling GraphicalPrimitive1D_unsetId().
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_setId(GraphicalPrimitive1D_t * gpd, const char * id);


/**
 * Sets the value of the "stroke" attribute of this GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @param stroke const char * value of the "stroke" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p stroke = @c NULL or an empty string is
 * equivalent to calling GraphicalPrimitive1D_unsetStroke().
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_setStroke(GraphicalPrimitive1D_t * gpd,
                               const char * stroke);


/**
 * Sets the value of the "stroke-width" attribute of this
 * GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @param strokeWidth double value of the "stroke-width" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_setStrokeWidth(GraphicalPrimitive1D_t * gpd,
                                    double strokeWidth);


/**
 * Sets the value of the "stroke-dasharray" attribute of this
 * GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @param strokeDash pointer value of the "stroke-dasharray" attribute to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_setStrokeDashArray(GraphicalPrimitive1D_t* gpd,
                                        const char* strokeDash);


/**
 * Unsets the value of the "id" attribute of this GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_unsetId(GraphicalPrimitive1D_t * gpd);


/**
 * Unsets the value of the "stroke" attribute of this GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_unsetStroke(GraphicalPrimitive1D_t * gpd);


/**
 * Unsets the value of the "stroke-width" attribute of this
 * GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_unsetStrokeWidth(GraphicalPrimitive1D_t * gpd);


/**
 * Unsets the value of the "stroke-dasharray" attribute of this
 * GraphicalPrimitive1D_t.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_unsetStrokeDashArray(GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type Ellipse_t
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive1D_t is of type Ellipse_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isEllipse(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type
 * Rectangle_t
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive1D_t is of type Rectangle_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isRectangle(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type Polygon_t
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive1D_t is of type Polygon_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isPolygon(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type
 * RenderGroup_t
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive1D_t is of type RenderGroup_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isRenderGroup(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type
 * LineEnding_t
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive1D_t is of type LineEnding_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isLineEnding(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type Text_t
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive1D_t is of type Text_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isText(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive1D_t is of type
 * RenderCurve_t
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive1D_t is of type RenderCurve_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_isRenderCurve(const GraphicalPrimitive1D_t * gpd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GraphicalPrimitive1D_t object have been set.
 *
 * @param gpd the GraphicalPrimitive1D_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * GraphicalPrimitive1D_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GraphicalPrimitive1D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive1D_hasRequiredAttributes(const GraphicalPrimitive1D_t * gpd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !GraphicalPrimitive1D_H__ */


