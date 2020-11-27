/**
 * @file    ColorDefinition.h
 * @brief   Definition of the ColorDefinition class.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 * @author  SBMLTeam
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
 * @class ColorDefinition
 * @sbmlbrief{render} Definition of an identifier for an RGBA color value.
 *
 * A @em ColorDefinition defines an identifier representing an RGBA value,
 * which can then be referenced in other render extension constructs.  For
 * example, an identifier can be defined for "lightyellow" and this may be
 * more descriptive to use in graphs of models than the corresponding RGBA
 * value.
 *
 * A ColorDefinition has two mandatory attributes: the id for the color
 * definition and the corresponding RGBA value. The RGBA value has the same
 * notation as in HTML files or CSS style sheets. It starts with the @c #
 * character followed by 8 digit hexadecimal string.  Optionally the alpha
 * part can be omitted, in which case it defaults to @c FF.
 *
 * Examples of valid values strings are @c "#000000" and @c "#000000FF" for
 * fully opaque black, and @c "#FF000010" for an almost completely
 * transparent red.
 *
 * Internally, the RGBA components are stored as integer values in the range
 * of 0 to 255 and most methods in the implementation of the libSBML
 * &ldquo;render&rdquo; extension use integer values instead of the
 * hexadecimal value string.
 */

#ifndef ColorDefinition_H__
#define ColorDefinition_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>

#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ColorDefinition : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

////  std::string mId;
  unsigned char mRed;
  unsigned char mGreen;
  unsigned char mBlue;
  unsigned char mAlpha;
  std::string mValue;

  static const std::string ELEMENT_NAME;


  /** @endcond */

public:

  /**
   * Creates a new ColorDefinition using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ColorDefinition.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ColorDefinition.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ColorDefinition.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ColorDefinition(unsigned int level = RenderExtension::getDefaultLevel(),
                  unsigned int version = RenderExtension::getDefaultVersion(),
                  unsigned int pkgVersion =
                  RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ColorDefinition using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ColorDefinition(RenderPkgNamespaces *renderns);


  /**
   * Creates a new ColorDefinition object from the given XMLNode object.
   *
   * The XMLNode object must contain a valid XML representation of a
   * ColorDefinition object as defined in the &ldquo;render&rdquo; package
   * specification.  This method is normally called when &ldquo;render&rdquo;
   * information is read from a file and should normally not have to be
   * called explicitly.
   *
   * (FOR BACKWARD COMPATIBILITY)
   *
   * @param node the XMLNode object reference that describes the ColorDefinition
   * object to be instantiated.
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  ColorDefinition(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which sets the ColorDefinition to the given RGBA values.
   *
   * @param renderns The namespace object for the Render package.
   * @param r Red component value. Has to be in the range of 0 to 255.
   * @param g Green component value. Has to be in the range of 0 to 255.
   * @param b Blue component value. Has to be in the range of 0 to 255.
   * @param a Alpha component value. Has to be in the range of 0 to 255. 
   * The alpha component can be omitted. In that case it has a default value of 255.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  ColorDefinition(RenderPkgNamespaces* renderns, unsigned char r,unsigned char g,unsigned char b,unsigned char a=255);
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
  /**
   * Constructor which sets the ColorDefinition to completely opaque
   * black and sets the id to the given string.
   *
   * @param renderns The namespace object for the Render package.
   * @param id the id of the color definition. The user has to make sure 
   * that the id is unique within the given set of color definitions. 
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  ColorDefinition(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
  /**
   * Constructor which sets the ColorDefinition to the given RGBA values
   * and sets the id.
   *
   * @param renderns The namespace object for the Render package.
   * @param id the id of the color definition. The user has to make sure 
   * that the id is unique within the given set of color definitions. 
   * @param r Red component value. Has to be in the range of 0 to 255.
   * @param g Green component value. Has to be in the range of 0 to 255.
   * @param b Blue component value. Has to be in the range of 0 to 255.
   * @param a Alpha component value. Has to be in the range of 0 to 255. 
   * The alpha component can be omitted. In that case it has a default value of 255.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  ColorDefinition(RenderPkgNamespaces* renderns, const std::string& id,unsigned char r,unsigned char g,unsigned char b,unsigned char a=255);
#endif // OMIT_DEPRECATED

  /**
   * Copy constructor for ColorDefinition.
   *
   * @param orig the ColorDefinition instance to copy.
   */
  ColorDefinition(const ColorDefinition& orig);


  /**
   * Assignment operator for ColorDefinition.
   *
   * @param rhs the ColorDefinition object whose values are to be used as the
   * basis of the assignment.
   */
  ColorDefinition& operator=(const ColorDefinition& rhs);


  /**
   * Creates and returns a deep copy of this ColorDefinition object.
   *
   * @return a (deep) copy of this ColorDefinition object.
   */
  virtual ColorDefinition* clone() const;


  /**
   * Destructor for ColorDefinition.
   */
  virtual ~ColorDefinition();


  /**
   * Returns the value of the "id" attribute of this ColorDefinition.
   *
   * @return the value of the "id" attribute of this ColorDefinition as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this ColorDefinition.
   *
   * @return the value of the "name" attribute of this ColorDefinition as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "value" attribute of this ColorDefinition.
   *
   * @return the value of the "value" attribute of this ColorDefinition as a
   * string.
   */
  const std::string& getValue() const;



  /**
   * Returns the red color component.
   *
   * @return the red color component for the ColorDefinition.
   */
  unsigned char getRed() const;


  /**
   * Returns the green color component.
   *
   * @return the green color component for the ColorDefinition.
   */
  unsigned char getGreen() const;


  /**
   * Returns the blue color component.
   *
   * @return the blue color component for the ColorDefinition.
   */
  unsigned char getBlue() const;


  /**
   * Returns the alpha color component.
   *
   * @return the alpha color component for the ColorDefinition.
   */
  unsigned char getAlpha() const;


  /**
   * Predicate returning @c true if this ColorDefinition's "id" attribute is
   * set.
   *
   * @return @c true if this ColorDefinition's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this ColorDefinition's "name" attribute is
   * set.
   *
   * @return @c true if this ColorDefinition's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this ColorDefinition's "value" attribute is
   * set.
   *
   * @return @c true if this ColorDefinition's "value" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetValue() const;


  /**
   * Sets the value of the "id" attribute of this ColorDefinition.
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
   * Sets the value of the "name" attribute of this ColorDefinition.
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
   * Sets the value of the "value" attribute of this ColorDefinition.
   *
   * @param value std::string& value of the "value" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p value = @c NULL or an empty string is
   * equivalent to calling unsetValue().
   */
  int setValue(const std::string& value);


  /**
   * Sets the red color component.
   *
   * @param c the new red component value for the color definition.
   */
  void setRed(unsigned char c);


  /**
   * Sets the green color component.
   *
   * @param c the new green component value for the color definition.
   */
  void setGreen(unsigned char c);


  /**
   * Sets the blue color component.
   *
   * @param c the new blue component value for the color definition.
   */
  void setBlue(unsigned char c);


  /**
   * Sets alpha red color component.
   *
   * @param c the new alpha component value for the color definition.
   */
  void setAlpha(unsigned char c);


  /**
   * Sets the red green, blue and alpha color component.
   * The alpha value is optional and defaults to 255 if not given.
   * @param r Red component value. Has to be in the range of 0 to 255.
   * @param g Green component value. Has to be in the range of 0 to 255.
   * @param b Blue component value. Has to be in the range of 0 to 255.
   * @param a Alpha component value. Has to be in the range of 0 to 255. 
   * The alpha component can be omitted. In that case it has a default value of 255.
   */
  void setRGBA(unsigned char r,unsigned char g,unsigned char b,unsigned char a=255);


  /**
   * Sets the color value from a given value string.
   *
   * If the string is not a valid value string, the color value is set to
   * black and @c false is returned.
   *
   * @param valueString A const reference to a string that represents a valid
   * color value, e.g. @c "#FFFFFFFF" for fully opaque white.
   *
   * @return @c true or @c false depending on whether setting the color value
   * from the string was successfull.
   */
  bool setColorValue(const std::string& valueString);


  /**
   * Creates a string that represents the current color value.
   *
   * @return The string representation of the color value.
   */
  std::string createValueString() const;


  /**
   * Unsets the value of the "id" attribute of this ColorDefinition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this ColorDefinition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "value" attribute of this ColorDefinition.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValue();


  /**
   * Returns the XML element name of this ColorDefinition object.
   *
   * For ColorDefinition, the XML element name is always @c "colorDefinition".
   *
   * @return the name of this element, i.e. @c "colorDefinition".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ColorDefinition object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_COLORDEFINITION, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * ColorDefinition object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * ColorDefinition have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the ColorDefinition object are:
   * @li "id"
   * @li "value"
   */
  virtual bool hasRequiredAttributes() const;



  /** @cond doxygenLibsbmlInternal */
  /**
  * Predicate returning @c true if all the required attributes for this
  * ColorDefinition object have been set, but not necessarily those with default.
  *
  * @return @c true to indicate that all the required attributes of this
  * ColorDefinition have been set, otherwise @c false is returned.
  *
  * @note The required attributes for the ColorDefinition object are:
  * @li "id"
  * @li "value" (default)
  */
  bool hasRequiredAttributesNoDefaults() const;
  /** @endcond */



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
   * Returns the value of the "attributeName" attribute of this ColorDefinition.
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
   * Returns the value of the "attributeName" attribute of this ColorDefinition.
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
   * Returns the value of the "attributeName" attribute of this ColorDefinition.
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
   * Returns the value of the "attributeName" attribute of this ColorDefinition.
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
   * Returns the value of the "attributeName" attribute of this ColorDefinition.
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
   * Predicate returning @c true if this ColorDefinition's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this ColorDefinition's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the value of the "attributeName" attribute of this ColorDefinition.
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
   * Sets the value of the "attributeName" attribute of this ColorDefinition.
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
   * Sets the value of the "attributeName" attribute of this ColorDefinition.
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
   * Sets the value of the "attributeName" attribute of this ColorDefinition.
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
   * Sets the value of the "attributeName" attribute of this ColorDefinition.
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
   * Unsets the value of the "attributeName" attribute of this ColorDefinition.
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
   * Creates an XMLNode object from this ColorDefinition object.
   *
   * @return the XMLNode with the XML representation for the 
   * ColorDefinition object.
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
 * Creates a new ColorDefinition_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * ColorDefinition_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * ColorDefinition_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * ColorDefinition_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
ColorDefinition_t *
ColorDefinition_create(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this ColorDefinition_t object.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @return a (deep) copy of this ColorDefinition_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
ColorDefinition_clone(const ColorDefinition_t* cd);


/**
 * Frees this ColorDefinition_t object.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
void
ColorDefinition_free(ColorDefinition_t* cd);


/**
 * Returns the value of the "id" attribute of this ColorDefinition_t.
 *
 * @param cd the ColorDefinition_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this ColorDefinition_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
char *
ColorDefinition_getId(const ColorDefinition_t * cd);


/**
 * Returns the value of the "name" attribute of this ColorDefinition_t.
 *
 * @param cd the ColorDefinition_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this ColorDefinition_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
char *
ColorDefinition_getName(const ColorDefinition_t * cd);


/**
 * Returns the value of the "value" attribute of this ColorDefinition_t.
 *
 * @param cd the ColorDefinition_t structure whose value is sought.
 *
 * @return the value of the "value" attribute of this ColorDefinition_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
char *
ColorDefinition_getValue(const ColorDefinition_t * cd);


/**
 * Predicate returning @c 1 (true) if this ColorDefinition_t's "id" attribute
 * is set.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @return @c 1 (true) if this ColorDefinition_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_isSetId(const ColorDefinition_t * cd);


/**
 * Predicate returning @c 1 (true) if this ColorDefinition_t's "name" attribute
 * is set.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @return @c 1 (true) if this ColorDefinition_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_isSetName(const ColorDefinition_t * cd);


/**
 * Predicate returning @c 1 (true) if this ColorDefinition_t's "value"
 * attribute is set.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @return @c 1 (true) if this ColorDefinition_t's "value" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_isSetValue(const ColorDefinition_t * cd);


/**
 * Sets the value of the "id" attribute of this ColorDefinition_t.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling ColorDefinition_unsetId().
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_setId(ColorDefinition_t * cd, const char * id);


/**
 * Sets the value of the "name" attribute of this ColorDefinition_t.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling ColorDefinition_unsetName().
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_setName(ColorDefinition_t * cd, const char * name);


/**
 * Sets the value of the "value" attribute of this ColorDefinition_t.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @param value const char * value of the "value" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p value = @c NULL or an empty string is
 * equivalent to calling ColorDefinition_unsetValue().
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_setValue(ColorDefinition_t * cd, const char * value);


/**
 * Unsets the value of the "id" attribute of this ColorDefinition_t.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_unsetId(ColorDefinition_t * cd);


/**
 * Unsets the value of the "name" attribute of this ColorDefinition_t.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_unsetName(ColorDefinition_t * cd);


/**
 * Unsets the value of the "value" attribute of this ColorDefinition_t.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_unsetValue(ColorDefinition_t * cd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * ColorDefinition_t object have been set.
 *
 * @param cd the ColorDefinition_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * ColorDefinition_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the ColorDefinition_t object are:
 * @li "id"
 * @li "value"
 *
 * @memberof ColorDefinition_t
 */
LIBSBML_EXTERN
int
ColorDefinition_hasRequiredAttributes(const ColorDefinition_t * cd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ColorDefinition_H__ */
