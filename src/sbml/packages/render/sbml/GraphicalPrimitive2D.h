/**
 * @file    GraphicalPrimitive2D.h
 * @brief Definition of the GraphicalPrimitive2D class.
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
 * @class GraphicalPrimitive2D
 * @sbmlbrief{render} Base class for graphical primitives implementing 2D attributes.
 *
 * The GraphicalPrimitive2D class implements attributes and methods necessary
 * for 2D objects like rectangles, polygons or ellipses. The attributes that
 * are implemented are a fill color and a fill rule that specifies how the
 * fill color is applied.
 *
 * The GraphicalPrimitive2D class is derived from GraphicalPrimitive1D and
 * inherits all its methods and attributes.
 */

#ifndef GraphicalPrimitive2D_H__
#define GraphicalPrimitive2D_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>

#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/GraphicalPrimitive1D.h>
#include <sbml/packages/render/extension/RenderExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class Ellipse;
class Rectangle;
class Polygon;
class RenderGroup;
class LineEnding;

class LIBSBML_EXTERN GraphicalPrimitive2D : public GraphicalPrimitive1D
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mFill;
  int mFillRule;

  /** @endcond */

public:

  /** @cond doxygenLibsbmlInternal */
  enum FILL_RULE
  {
    UNSET,
    NONZERO,
    EVENODD,
    INHERIT,
    INVALID
  };
  /** @endcond */


  /**
   * Creates a new GraphicalPrimitive2D using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * GraphicalPrimitive2D.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * GraphicalPrimitive2D.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this GraphicalPrimitive2D.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GraphicalPrimitive2D(unsigned int level = RenderExtension::getDefaultLevel(),
                       unsigned int version =
                         RenderExtension::getDefaultVersion(),
                       unsigned int pkgVersion =
                         RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new GraphicalPrimitive2D using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GraphicalPrimitive2D(RenderPkgNamespaces *renderns);


  /**
   * Creates a new GraphicalPrimitive2D object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * GraphicalPrimitive2D object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the GraphicalPrimitive2D
   * object to be instantiated.
   *
   * @param l2Version an integer indicating the version of SBML Level&nbsp;2
   */
  GraphicalPrimitive2D(const XMLNode& node, unsigned int l2Version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a GraphicalPrimitive2D.
   * The attributes inherited from GraphicalPrimitive1D are set as described
   * in the corresponding constructor for GraphicalPrimitive1D.
   *
   * The fill and the fill rule are unset.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id The id for the GraphicalPrimitive1D object
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  GraphicalPrimitive2D(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED


  /**
   * Copy constructor for GraphicalPrimitive2D.
   *
   * @param orig the GraphicalPrimitive2D instance to copy.
   */
  GraphicalPrimitive2D(const GraphicalPrimitive2D& orig);


  /**
   * Assignment operator for GraphicalPrimitive2D.
   *
   * @param rhs the GraphicalPrimitive2D object whose values are to be used as
   * the basis of the assignment.
   */
  GraphicalPrimitive2D& operator=(const GraphicalPrimitive2D& rhs);


  /**
   * Creates and returns a deep copy of this GraphicalPrimitive2D object.
   *
   * @return a (deep) copy of this GraphicalPrimitive2D object.
   */
  virtual GraphicalPrimitive2D* clone() const;


  /**
   * Destructor for GraphicalPrimitive2D.
   */
  virtual ~GraphicalPrimitive2D();


  /**
   * Returns the value of the "fill" attribute of this GraphicalPrimitive2D.
   *
   * @return the value of the "fill" attribute of this GraphicalPrimitive2D as
   * a string.
   */
  const std::string& getFill() const;


  /**
  * Returns the value of the "fill" attribute of this GraphicalPrimitive2D.
  *
   * @return the value of the "fill" attribute of this GraphicalPrimitive2D as
   * a string.
   */
  const std::string& getFillColor() const;


  /**
   * Returns the value of the "fill-rule" attribute of this
   * GraphicalPrimitive2D.
   *
   * @copydetails doc_render_fill_rule
   *
   * @return the value of the "fill-rule" attribute of this
   * GraphicalPrimitive2D as @if clike a FillRule_t @else an int@endif@~ value.
   * The possible values returned by this method are:
   * @li @sbmlconstant{FILL_RULE_UNSET, FillRule_t}
   * @li @sbmlconstant{FILL_RULE_NONZERO, FillRule_t}
   * @li @sbmlconstant{FILL_RULE_EVENODD, FillRule_t}
   * @li @sbmlconstant{FILL_RULE_INHERIT, FillRule_t}
   * @li @sbmlconstant{FILL_RULE_INVALID, FillRule_t}
   */
  int getFillRule() const;


  /**
   * Returns the value of the "fill-rule" attribute of this
   * GraphicalPrimitive2D.
   *
   * @copydetails doc_render_fill_rule
   *
   * @return the value of the "fill-rule" attribute of this
   * GraphicalPrimitive2D as a string. The possible values returned by this
   * method are:
   * @li @c "unset"
   * @li @c "nonzero"
   * @li @c "evenodd"
   * @li @c "inherit"
   * @li @c "(Unknown FillRule value)"
   */
  std::string getFillRuleAsString() const;


  /**
   * Predicate returning @c true if this GraphicalPrimitive2D's "fill"
   * attribute is set.
   *
   * @return @c true if this GraphicalPrimitive2D's "fill" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetFill() const;


  /**
   * Returns @c true if the fill attribute is set or @c false otherwise.
   * The fill attribute is considered set if the string is not empty.
   *
   * @return @c true if the fill color is set.
   */
  bool isSetFillColor() const;


  /**
   * Predicate returning @c true if this GraphicalPrimitive2D's "fill-rule"
   * attribute is set.
   *
   * @copydetails doc_render_fill_rule
   *
   * @return @c true if this GraphicalPrimitive2D's "fill-rule" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetFillRule() const;


  /**
   * Sets the value of the "fill" attribute of this GraphicalPrimitive2D.
   *
   * @param fill std::string& value of the "fill" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p fill = @c NULL or an empty string is
   * equivalent to calling unsetFill().
   */
  int setFill(const std::string& fill);


  /**
   * Set fill color to the id of a color definition, the id of a gradient
   * definition or a color value string.
   *
   * @param color the id of a color deifnition or a gradient or a color value string.
   */
  void setFillColor(const std::string& color);

 
  /**
  * Sets the value of the "fill-rule" attribute of this GraphicalPrimitive2D.
  *
  * @copydetails doc_render_fill_rule
  *
  * @param rule @if clike FillRule_t@else int@endif@~ value of the
  * "fill-rule" attribute to be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  */
  void setFillRule(FILL_RULE rule);


  /**
   * Sets the value of the "fill-rule" attribute of this GraphicalPrimitive2D.
   *
   * @copydetails doc_render_fill_rule
   *
   * @param fillRule @if clike FillRule_t@else int@endif@~ value of the
   * "fill-rule" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFillRule(const FillRule_t fillRule);


  /**
   * Sets the value of the "fill-rule" attribute of this GraphicalPrimitive2D.
   *
   * @copydetails doc_render_fill_rule
   *
   * @param fillRule std::string& of the "fill-rule" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFillRule(const std::string& fillRule);


  /**
   * Unsets the value of the "fill" attribute of this GraphicalPrimitive2D.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetFill();


  /**
   * Unsets the value of the "fill-rule" attribute of this
   * GraphicalPrimitive2D.
   *
   * @copydetails doc_render_fill_rule
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetFillRule();


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
   * type Ellipse
   *
   * @return @c true if this abstract GraphicalPrimitive2D is of type
   * Ellipse, @c false otherwise
   */
  virtual bool isEllipse() const;


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
   * type Rectangle
   *
   * @return @c true if this abstract GraphicalPrimitive2D is of type
   * Rectangle, @c false otherwise
   */
  virtual bool isRectangle() const;


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
   * type Polygon
   *
   * @return @c true if this abstract GraphicalPrimitive2D is of type
   * Polygon, @c false otherwise
   */
  virtual bool isPolygon() const;


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
   * type RenderGroup
   *
   * @return @c true if this abstract GraphicalPrimitive2D is of type
   * RenderGroup, @c false otherwise
   */
  virtual bool isRenderGroup() const;


  /**
   * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
   * type LineEnding
   *
   * @return @c true if this abstract GraphicalPrimitive2D is of type
   * LineEnding, @c false otherwise
   */
  virtual bool isLineEnding() const;


  /**
   * Returns the XML element name of this GraphicalPrimitive2D object.
   *
   * For GraphicalPrimitive2D, the XML element name is always
   * @c "graphicalPrimitive2D".
   *
   * @return the name of this element, i.e. @c "graphicalPrimitive2D".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this GraphicalPrimitive2D object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_GRAPHICALPRIMITIVE2D, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * GraphicalPrimitive2D object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * GraphicalPrimitive2D have been set, otherwise @c false is returned.
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
   * GraphicalPrimitive2D.
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
   * GraphicalPrimitive2D.
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
   * GraphicalPrimitive2D.
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
   * GraphicalPrimitive2D.
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
   * GraphicalPrimitive2D.
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
   * Predicate returning @c true if this GraphicalPrimitive2D's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this GraphicalPrimitive2D's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * GraphicalPrimitive2D.
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
   * GraphicalPrimitive2D.
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
   * GraphicalPrimitive2D.
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
   * GraphicalPrimitive2D.
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
   * GraphicalPrimitive2D.
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
   * GraphicalPrimitive2D.
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
   * Adds all set attributes specific to the given GraphicalPrimitive2D objects to the given
   * XMLAttributes object.
   */
  static void addGraphicalPrimitive2DAttributes(const GraphicalPrimitive2D& gp,XMLAttributes& attr);
  /** @endcond */

};

LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
* Creates a new Ellipse (GraphicalPrimitive2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive2D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createEllipse(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new Rectangle (GraphicalPrimitive2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive2D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createRectangle(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new Polygon (GraphicalPrimitive2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive2D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createPolygon(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new RenderGroup (GraphicalPrimitive2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive2D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createRenderGroup(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
* Creates a new LineEnding (GraphicalPrimitive2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*
* @param level an unsigned int, the SBML Level to assign to this
* GraphicalPrimitive2D_t.
*
* @param version an unsigned int, the SBML Version to assign to this
* GraphicalPrimitive2D_t.
*
* @param pkgVersion an unsigned int, the SBML Render Version to assign to this
* GraphicalPrimitive2D_t.
*
* @copydetails doc_note_setting_lv_pkg
*
* @copydetails doc_warning_returns_owned_pointer
*
* @memberof GraphicalPrimitive2D_t
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createLineEnding(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this GraphicalPrimitive2D_t object.
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @return a (deep) copy of this GraphicalPrimitive2D_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
GraphicalPrimitive2D_t*
GraphicalPrimitive2D_clone(const GraphicalPrimitive2D_t* gpd);


/**
 * Frees this GraphicalPrimitive2D_t object.
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
void
GraphicalPrimitive2D_free(GraphicalPrimitive2D_t* gpd);


/**
 * Returns the value of the "fill" attribute of this GraphicalPrimitive2D_t.
 *
 * @param gpd the GraphicalPrimitive2D_t structure whose fill is sought.
 *
 * @return the value of the "fill" attribute of this GraphicalPrimitive2D_t as
 * a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
char *
GraphicalPrimitive2D_getFill(const GraphicalPrimitive2D_t * gpd);


/**
 * Returns the value of the "fill-rule" attribute of this
 * GraphicalPrimitive2D_t.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param gpd the GraphicalPrimitive2D_t structure whose fill-rule is sought.
 *
 * @return the value of the "fill-rule" attribute of this
 * GraphicalPrimitive2D_t as @if clike a FillRule_t @else an int@endif@~ value.
 * The possible values returned by this method are:
 * @li @sbmlconstant{FILL_RULE_UNSET, FillRule_t}
 * @li @sbmlconstant{FILL_RULE_NONZERO, FillRule_t}
 * @li @sbmlconstant{FILL_RULE_EVENODD, FillRule_t}
 * @li @sbmlconstant{FILL_RULE_INHERIT, FillRule_t}
 * @li @sbmlconstant{FILL_RULE_INVALID, FillRule_t}
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_getFillRule(const GraphicalPrimitive2D_t * gpd);


/**
 * Returns the value of the "fill-rule" attribute of this
 * GraphicalPrimitive2D_t.
 *
 * @param gpd the GraphicalPrimitive2D_t structure whose fill-rule is sought.
 *
 * @return the value of the "fill-rule" attribute of this
 * GraphicalPrimitive2D_t as a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_render_fill_rule
 * The possible values returned by this method are:
 * @li @c "nonzero"
 * @li @c "evenodd"
 * @li @c "(Unknown FillRule value)"
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
char *
GraphicalPrimitive2D_getFillRuleAsString(const GraphicalPrimitive2D_t * gpd);


/**
 * Predicate returning @c 1 (true) if this GraphicalPrimitive2D_t's "fill"
 * attribute is set.
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @return @c 1 (true) if this GraphicalPrimitive2D_t's "fill" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isSetFill(const GraphicalPrimitive2D_t * gpd);


/**
 * Predicate returning @c 1 (true) if this GraphicalPrimitive2D_t's "fill-rule"
 * attribute is set.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @return @c 1 (true) if this GraphicalPrimitive2D_t's "fill-rule" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isSetFillRule(const GraphicalPrimitive2D_t * gpd);


/**
 * Sets the value of the "fill" attribute of this GraphicalPrimitive2D_t.
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @param fill const char * value of the "fill" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p fill = @c NULL or an empty string is
 * equivalent to calling GraphicalPrimitive2D_unsetFill().
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_setFill(GraphicalPrimitive2D_t * gpd, const char * fill);


/**
 * Sets the value of the "fill-rule" attribute of this GraphicalPrimitive2D_t.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @param fillRule @if clike FillRule_t @else int@endif@~ value of the
 * "fill-rule" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_setFillRule(GraphicalPrimitive2D_t * gpd,
                                 FillRule_t fillRule);


/**
 * Sets the value of the "fill-rule" attribute of this GraphicalPrimitive2D_t.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @param fillRule const char * of the "fill-rule" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_setFillRuleAsString(GraphicalPrimitive2D_t * gpd,
                                         const char * fillRule);


/**
 * Unsets the value of the "fill" attribute of this GraphicalPrimitive2D_t.
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_unsetFill(GraphicalPrimitive2D_t * gpd);


/**
 * Unsets the value of the "fill-rule" attribute of this
 * GraphicalPrimitive2D_t.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_unsetFillRule(GraphicalPrimitive2D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type Ellipse_t
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive2D_t is of type Ellipse_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isEllipse(const GraphicalPrimitive2D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type
 * Rectangle_t
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive2D_t is of type Rectangle_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isRectangle(const GraphicalPrimitive2D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type Polygon_t
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive2D_t is of type Polygon_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isPolygon(const GraphicalPrimitive2D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type
 * RenderGroup_t
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive2D_t is of type RenderGroup_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isRenderGroup(const GraphicalPrimitive2D_t * gpd);


/**
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type
 * LineEnding_t
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @return @c 1 if this GraphicalPrimitive2D_t is of type LineEnding_t, @c 0
 * otherwise
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isLineEnding(const GraphicalPrimitive2D_t * gpd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GraphicalPrimitive2D_t object have been set.
 *
 * @param gpd the GraphicalPrimitive2D_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * GraphicalPrimitive2D_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof GraphicalPrimitive2D_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_hasRequiredAttributes(const GraphicalPrimitive2D_t * gpd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */


#endif /* !GraphicalPrimitive2D_H__ */


