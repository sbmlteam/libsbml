/**
 * @file    GradientStop.h
 * @brief   class for representing a stop in a gradient definition
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
 * @class GradientStop
 * @sbmlbrief{render} A color at a certain location in a color gradient.
 *
 * The gradient stop concept was more or less taken from the corresponding
 * concept in SVG.  A GradientStop object represents the color at a certain
 * location in a linear or radial gradient.  Each gradient should contain two
 * or more gradient stops which mark the edges of a region within this region
 * color are interpolated based on the distance of the location to the edges
 * of the region.
 *
 * A gradient stop has two attributes. The first attribute is an offset which
 * determines the location for the gradient stop within the object the
 * gradient is applied to.  The offset can either be an absolute value or a
 * relative value or a combination of absolute and relative value.  For
 * example, a value of "50%" for the offset means that the gradient stop is
 * located at 50% of the gradient vector. For more information and examples,
 * see the render extension specification or the SVG specification.
 *
 * The second attribute defines the color for the gradient stop. The color
 * can either be defined be a color value string or by the id of a
 * ColorDefinition object.
 *
 * @see ColorDefinition
 */

#ifndef GradientStop_H__
#define GradientStop_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RelAbsVector.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN GradientStop : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  RelAbsVector mOffset;
  std::string mStopColor;

  /** @endcond */

public:

  /**
   * Creates a new GradientStop using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * GradientStop.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * GradientStop.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this GradientStop.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GradientStop(unsigned int level = RenderExtension::getDefaultLevel(),
               unsigned int version = RenderExtension::getDefaultVersion(),
               unsigned int pkgVersion =
                 RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new GradientStop using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GradientStop(RenderPkgNamespaces *renderns);


  /**
   * Creates a new GradientStop object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * GradientStop object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the GradientStop
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  GradientStop(const XMLNode& node, unsigned int l2version=4);

  
  /**
   * Copy constructor for GradientStop.
   *
   * @param orig the GradientStop instance to copy.
   */
  GradientStop(const GradientStop& orig);


  /**
   * Assignment operator for GradientStop.
   *
   * @param rhs the GradientStop object whose values are to be used as the
   * basis of the assignment.
   */
  GradientStop& operator=(const GradientStop& rhs);


  /**
   * Creates and returns a deep copy of this GradientStop object.
   *
   * @return a (deep) copy of this GradientStop object.
   */
  virtual GradientStop* clone() const;


  /**
   * Destructor for GradientStop.
   */
  virtual ~GradientStop();


  /**
   * Returns the value of the "stop-color" attribute of this GradientStop.
   *
   * @return the value of the "stop-color" attribute of this GradientStop as a
   * string.
   */
  const std::string& getStopColor() const;


  /**
   * Predicate returning @c true if this GradientStop's "stop-color" attribute
   * is set.
   *
   * @return @c true if this GradientStop's "stop-color" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetStopColor() const;


  /**
   * Sets the value of the "stop-color" attribute of this GradientStop.
   *
   * @param stopColor std::string& value of the "stop-color" attribute to be
   * set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p stopColor = @c NULL or an empty string is
   * equivalent to calling unsetStopColor().
   */
  int setStopColor(const std::string& stopColor);


  /**
   * Unsets the value of the "stop-color" attribute of this GradientStop.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStopColor();


  /**
   * Returns the value of the "offset" element of this GradientStop.
   *
   * @return the value of the "offset" element of this GradientStop as a
   * RelAbsVector.
   */
  const RelAbsVector& getOffset() const;


  /**
   * Returns the value of the "offset" element of this GradientStop.
   *
   * @return the value of the "offset" element of this GradientStop as a
   * RelAbsVector.
   */
  RelAbsVector& getOffset();


  /**
   * Predicate returning @c true if this GradientStop's "offset" element is
   * set.
   *
   * @return @c true if this GradientStop's "offset" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetOffset() const;


  /**
   * Sets the value of the "offset" element of this GradientStop.
   *
   * @param offset RelAbsVector value of the "offset" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setOffset(const RelAbsVector& offset);


  /**
   * Sets the offset for the gradient stop.
   *
   * @param abs the absolute value of the offset.
   *
   * @param rel the relative value of the offset.
   */
  void setOffset(double abs,double rel);

  /**
   * Sets the offset to the value specified by the given string.
   * The string has to represent a combination of an absolute 
   * and relative value.
   * Valid value string would e.g. be "45.0", "30%" or
   * "10+5%". If the value is a combination of both relative and 
   * absolute value, the absolute value has to come before the relative
   * value. Number can be given as integer values or floating point values
   * and the two components can be combined by '+' or '-'. Depending on
   * whethr the relative value should be added or subtracted from the 
   * absolute value.
   * If the given string is not valid, the offset will have an absolute 
   * and a relative value of NaN.
   *
   * @param co a string representing a valid offset value.
   */
  void setOffset(const std::string& co);


  /**
   * Unsets the value of the "offset" element of this GradientStop.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetOffset();


  /**
   * Returns the XML element name of this GradientStop object.
   *
   * For GradientStop, the XML element name is always @c "stop".
   *
   * @return the name of this element, i.e. @c "stop".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this GradientStop object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_GRADIENT_STOP, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * GradientStop object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * GradientStop have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the GradientStop object are:
   * @li "stop-color"
   */
  virtual bool hasRequiredAttributes() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this GradientStop.
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
   * Returns the value of the "attributeName" attribute of this GradientStop.
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
   * Returns the value of the "attributeName" attribute of this GradientStop.
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
   * Returns the value of the "attributeName" attribute of this GradientStop.
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
   * Returns the value of the "attributeName" attribute of this GradientStop.
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
   * Predicate returning @c true if this GradientStop's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this GradientStop's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this GradientStop.
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
   * Sets the value of the "attributeName" attribute of this GradientStop.
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
   * Sets the value of the "attributeName" attribute of this GradientStop.
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
   * Sets the value of the "attributeName" attribute of this GradientStop.
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
   * Sets the value of the "attributeName" attribute of this GradientStop.
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
   * Unsets the value of the "attributeName" attribute of this GradientStop.
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
   * Creates an XMLNode object from this GradientStop object.
   *
   * @return the XMLNode with the XML representation for the 
   * GradientStop object.
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
 * Creates a new GradientStop_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GradientStop_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GradientStop_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * GradientStop_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
GradientStop_t *
GradientStop_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this GradientStop_t object.
 *
 * @param gs the GradientStop_t structure.
 *
 * @return a (deep) copy of this GradientStop_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
GradientStop_t*
GradientStop_clone(const GradientStop_t* gs);


/**
 * Frees this GradientStop_t object.
 *
 * @param gs the GradientStop_t structure.
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
void
GradientStop_free(GradientStop_t* gs);


/**
 * Returns the value of the "stop-color" attribute of this GradientStop_t.
 *
 * @param gs the GradientStop_t structure whose stop-color is sought.
 *
 * @return the value of the "stop-color" attribute of this GradientStop_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
char *
GradientStop_getStopColor(const GradientStop_t * gs);


/**
 * Predicate returning @c 1 (true) if this GradientStop_t's "stop-color"
 * attribute is set.
 *
 * @param gs the GradientStop_t structure.
 *
 * @return @c 1 (true) if this GradientStop_t's "stop-color" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
int
GradientStop_isSetStopColor(const GradientStop_t * gs);


/**
 * Sets the value of the "stop-color" attribute of this GradientStop_t.
 *
 * @param gs the GradientStop_t structure.
 *
 * @param stopColor const char * value of the "stop-color" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p stopColor = @c NULL or an empty string is
 * equivalent to calling GradientStop_unsetStopColor().
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
int
GradientStop_setStopColor(GradientStop_t * gs, const char * stopColor);


/**
 * Unsets the value of the "stop-color" attribute of this GradientStop_t.
 *
 * @param gs the GradientStop_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
int
GradientStop_unsetStopColor(GradientStop_t * gs);


/**
 * Returns the value of the "offset" element of this GradientStop_t.
 *
 * @param gs the GradientStop_t structure whose offset is sought.
 *
 * @return the value of the "offset" element of this GradientStop_t as a
 * RelAbsVector_t.
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
const RelAbsVector_t *
GradientStop_getOffset(const GradientStop_t * gs);


/**
 * Predicate returning @c 1 (true) if this GradientStop_t's "offset" element is
 * set.
 *
 * @param gs the GradientStop_t structure.
 *
 * @return @c 1 (true) if this GradientStop_t's "offset" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
int
GradientStop_isSetOffset(const GradientStop_t * gs);


/**
 * Sets the value of the "offset" element of this GradientStop_t.
 *
 * @param gs the GradientStop_t structure.
 *
 * @param offset RelAbsVector_t value of the "offset" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
int
GradientStop_setOffset(GradientStop_t * gs, const RelAbsVector_t& offset);


/**
 * Unsets the value of the "offset" element of this GradientStop_t.
 *
 * @param gs the GradientStop_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
int
GradientStop_unsetOffset(GradientStop_t * gs);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GradientStop_t object have been set.
 *
 * @param gs the GradientStop_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * GradientStop_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the GradientStop_t object are:
 * @li "stop-color"
 *
 * @memberof GradientStop_t
 */
LIBSBML_EXTERN
int
GradientStop_hasRequiredAttributes(const GradientStop_t * gs);



END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !GradientStop_H__ */


