/**
 * @file    GradientBase.h
 * @brief   abstract base class for gradient definitions
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
 * @class GradientBase
 * @sbmlbrief{render} Abstract base class for linear and radial gradients.
 * 
 * The base class implements common structures to both gradient classes.
 * Both gradients have an id attribute which is used to reference a gradient
 * within other render extension constructs. The id of a gradient can be used
 * to define the fill style of 2D objects like e.g. rectangles.
 *
 * Further, both gradient classes have a ListOfGradientStops objects which holds
 * the GradientStop objects that define the gradient and both classes have an 
 * attribute called spreadMethod which defines how a gradient is applied to an
 * object.
 */

/**
 * <!-- ~ ~ ~ ~ ~ Start of common documentation strings ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 * The following text is used as common documentation blocks copied multiple
 * times elsewhere in this file. The use of @class is a hack needed because
 * Doxygen's @copydetails command has limited functionality. Symbols
 * beginning with "doc_" are marked as ignored in our Doxygen configuration.
 * ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ -->
 *
 *
 * @class doc_gradientbase_spreadMethod
 *
 * @par
 * The attribute "spreadMethod" on a GradientBase object is used by GradientBase 
 * elements to decide how gradients propagate over the whole element they are 
 * applied to.  The following are the allowable values for "spreadMethod":
 * <ul>
 * <li> @c "pad", the gradient color at the endpoint of the vector defines how 
 * the gradient is continued beyond that point (default value).
 *
 * <li> @c "reflect", the gradient continues from end to start and then from 
 * start to end again and again.
 *
 * <li> @c "repeat", the gradient pattern is repeated from start to end over 
 * and over again.
 *
 * </ul>
 */


#ifndef GradientBase_H__
#define GradientBase_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>

#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/ListOfGradientStops.h>
#include <sbml/packages/render/sbml/GradientStop.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LinearGradient;
class RadialGradient;

class LIBSBML_EXTERN GradientBase : public SBase
{
public:
  /** @cond doxygenLibsbmlInternal */
  enum SPREADMETHOD
  {
    PAD,
    REFLECT,
    REPEAT,
    INVALID
  };
  /** @endcond */

protected:
  /** @cond doxygenLibsbmlInternal */

  int mSpreadMethod;
  ListOfGradientStops mGradientStops;
  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new GradientBase using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * GradientBase.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * GradientBase.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this GradientBase.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GradientBase(unsigned int level = RenderExtension::getDefaultLevel(),
               unsigned int version = RenderExtension::getDefaultVersion(),
               unsigned int pkgVersion =
                 RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new GradientBase using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  GradientBase(RenderPkgNamespaces *renderns);


  /**
   * Creates a new GradientBase object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * GradientBase object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the GradientBase
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  GradientBase(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a GradientBase with no gradient stops.
   * The spreadMethod attribute is set to GradientBase::PAD and the id is
   * set to the given value.
   * This object is not valid until it gets at least two gradient stops.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id The id for the gradient definition object
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  GradientBase(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED

  /**
   * Copy constructor for GradientBase.
   *
   * @param orig the GradientBase instance to copy.
   */
  GradientBase(const GradientBase& orig);


  /**
   * Assignment operator for GradientBase.
   *
   * @param rhs the GradientBase object whose values are to be used as the
   * basis of the assignment.
   */
  GradientBase& operator=(const GradientBase& rhs);


  /**
   * Creates and returns a deep copy of this GradientBase object.
   *
   * @return a (deep) copy of this GradientBase object.
   */
  virtual GradientBase* clone() const = 0;


  /**
   * Destructor for GradientBase.
   */
  virtual ~GradientBase();


  /**
   * Returns the value of the "id" attribute of this GradientBase.
   *
   * @return the value of the "id" attribute of this GradientBase as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this GradientBase.
   *
   * @return the value of the "name" attribute of this GradientBase as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "spreadMethod" attribute of this GradientBase.
   *
   * @return the value of the "spreadMethod" attribute of this GradientBase as
   * a @if clike GradientSpreadMethod_t@else int@endif@~.
   *
   * @copydetails doc_gradientbase_spreadMethod
   * @if clike The value is drawn from the enumeration
   * GradientSpreadMethod_t.@endif@~
   * The possible values returned by this method are:
   * @li @sbmlconstant{GRADIENT_SPREADMETHOD_PAD, GradientSpreadMethod_t}
   * @li @sbmlconstant{GRADIENT_SPREADMETHOD_REFLECT, GradientSpreadMethod_t}
   * @li @sbmlconstant{GRADIENT_SPREADMETHOD_REPEAT, GradientSpreadMethod_t}
   * @li @sbmlconstant{GRADIENT_SPREAD_METHOD_INVALID, GradientSpreadMethod_t}
   */
  int getSpreadMethod() const;


  /**
   * Returns the value of the "spreadMethod" attribute of this GradientBase.
   *
   * @return the value of the "spreadMethod" attribute of this GradientBase as
   * a string.
   *
   * @copydetails doc_gradientbase_spreadMethod
   * The possible values returned by this method are:
   * @li @c "pad"
   * @li @c "reflect"
   * @li @c "repeat"
   * @li @c "invalid"
   * @li @c "(Unknown GradientSpreadMethod value)"
   */
  std::string getSpreadMethodAsString() const;


  /**
   * Returns the value of the "spreadMethod" attribute of this GradientBase.
   *
   * @return the value of the "spreadMethod" attribute of this GradientBase as
   * a string.
   *
   * @copydetails doc_gradientbase_spreadMethod
   * The possible values returned by this method are:
   * @li @c "pad"
   * @li @c "reflect"
   * @li @c "repeat"
   * @li @c "invalid"
   */
  std::string getSpreadMethodString() const;


  /**
   * Predicate returning @c true if this GradientBase's "id" attribute is set.
   *
   * @return @c true if this GradientBase's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this GradientBase's "name" attribute is
   * set.
   *
   * @return @c true if this GradientBase's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this GradientBase's "spreadMethod"
   * attribute is set.
   *
   * @return @c true if this GradientBase's "spreadMethod" attribute has been
   * set, otherwise @c false is returned.
   *
   * @copydetails doc_gradientbase_spreadMethod
   */
  bool isSetSpreadMethod() const;


  /**
   * Sets the value of the "id" attribute of this GradientBase.
   *
   * @param id the string value of the "id" attribute to be set.
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
   * Sets the value of the "name" attribute of this GradientBase.
   *
   * @param name the string value of the "name" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p name = @c NULL or an empty string is
   * equivalent to calling unsetName().
   */
  virtual int setName(const std::string& name);


  /**
   * Sets the value of the "spreadMethod" attribute of this GradientBase.
   *
   * @param spreadMethod @if clike GradientSpreadMethod_t@else int@endif@~ value
   * of the "spreadMethod" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_gradientbase_spreadMethod
   */
  void setSpreadMethod(SPREADMETHOD spreadMethod);


  /**
   * Sets the value of the "spreadMethod" attribute of this GradientBase.
   *
   * @param spreadMethod @if clike GradientSpreadMethod_t@else int@endif@~ value
   * of the "spreadMethod" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_gradientbase_spreadMethod
   */
  int setSpreadMethod(const GradientSpreadMethod_t spreadMethod);


  /**
   * Sets the value of the "spreadMethod" attribute of this GradientBase.
   *
   * @param spreadMethod std::string& of the "spreadMethod" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_gradientbase_spreadMethod
   */
  int setSpreadMethod(const std::string& spreadMethod);


  /**
   * Unsets the value of the "id" attribute of this GradientBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this GradientBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "spreadMethod" attribute of this GradientBase.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_gradientbase_spreadMethod
   */
  int unsetSpreadMethod();


  /**
   * Returns the ListOfGradientStops from this GradientBase.
   *
   * @return the ListOfGradientStops from this GradientBase.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see getGradientStop(const std::string& sid)
   * @see getGradientStop(unsigned int n)
   * @see getNumGradientStops()
   * @see removeGradientStop(const std::string& sid)
   * @see removeGradientStop(unsigned int n)
   */
  const ListOfGradientStops* getListOfGradientStops() const;


  /**
   * Returns the ListOfGradientStops from this GradientBase.
   *
   * @return the ListOfGradientStops from this GradientBase.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see getGradientStop(const std::string& sid)
   * @see getGradientStop(unsigned int n)
   * @see getNumGradientStops()
   * @see removeGradientStop(const std::string& sid)
   * @see removeGradientStop(unsigned int n)
   */
  ListOfGradientStops* getListOfGradientStops();


  /**
   * Get the nth GradientStop from the GradientBase.
   *
   * @param n an unsigned int representing the index of the GradientStop to
   * retrieve.
   *
   * @return the nth GradientStop in the ListOfGradientStops within this
   * GradientBase.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see getGradientStop(const std::string& sid)
   * @see getNumGradientStops()
   * @see removeGradientStop(const std::string& sid)
   * @see removeGradientStop(unsigned int n)
   */
  GradientStop* getGradientStop(unsigned int n);


  /**
  * Get the GradientStop with the given id from the GradientBase.
  *
  * @param sid the id of the GradientStop to retrieve.
  *
  * @return the GradientStop in the ListOfGradientStops with 
  * the given @p id from this GradientBase.
  * If no such GradientStop exists, @c NULL is returned.
  *
  * @copydetails doc_returned_unowned_pointer
  *
  * @see addGradientStop(const GradientStop* object)
  * @see createGradientStop()
  * @see getGradientStop(unsigned int n)
  * @see getNumGradientStops()
  * @see removeGradientStop(const std::string& sid)
  * @see removeGradientStop(unsigned int n)
  */
  GradientStop* getGradientStop(const std::string& sid);


  /**
   * Get a GradientStop from the GradientBase.
   *
   * @param n an unsigned int representing the index of the GradientStop to
   * retrieve.
   *
   * @return the nth GradientStop in the ListOfGradientStops within this
   * GradientBase.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see getGradientStop(const std::string& sid)
   * @see getNumGradientStops()
   * @see removeGradientStop(const std::string& sid)
   * @see removeGradientStop(unsigned int n)
   */
  const GradientStop* getGradientStop(unsigned int n) const;


  /**
   * Adds a copy of the given GradientStop to this GradientBase.
   *
   * @param gs the GradientStop object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createGradientStop()
   * @see getGradientStop(const std::string& sid)
   * @see getGradientStop(unsigned int n)
   * @see getNumGradientStops()
   * @see removeGradientStop(const std::string& sid)
   * @see removeGradientStop(unsigned int n)
   */
  int addGradientStop(const GradientStop* gs);


  /**
   * Get the number of GradientStop objects in this GradientBase.
   *
   * @return the number of GradientStop objects in this GradientBase.
   *
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see getGradientStop(const std::string& sid)
   * @see getGradientStop(unsigned int n)
   * @see removeGradientStop(const std::string& sid)
   * @see removeGradientStop(unsigned int n)
   */
  unsigned int getNumGradientStops() const;


  /**
   * Creates a new GradientStop object, adds it to this GradientBase object and
   * returns the GradientStop object created.
   *
   * @return a new GradientStop object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see getGradientStop(const std::string& sid)
   * @see getGradientStop(unsigned int n)
   * @see getNumGradientStops()
   * @see removeGradientStop(const std::string& sid)
   * @see removeGradientStop(unsigned int n)
   */
  GradientStop* createGradientStop();


  /**
   * Removes the nth GradientStop from this GradientBase and returns a pointer
   * to it.
   *
   * @param n an unsigned int representing the index of the GradientStop to
   * remove.
   *
   * @return a pointer to the nth GradientStop in this GradientBase.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see getGradientStop(const std::string& sid)
   * @see getGradientStop(unsigned int n)
   * @see getNumGradientStops()
   * @see removeGradientStop(const std::string& sid)
   */
  GradientStop* removeGradientStop(unsigned int n);


  /**
  * Removes the GradientStop with the given id from this GradientBase 
  * and returns a pointer to it.
  *
  * @param sid the id of the GradientStop to remove.
  *
  * @return a pointer to the nth GradientStop in this GradientBase.
  *
  * @copydetails doc_warning_returns_owned_pointer
  *
  * @see addGradientStop(const GradientStop* object)
  * @see createGradientStop()
  * @see getGradientStop(const std::string& sid)
  * @see getGradientStop(unsigned int n)
  * @see getNumGradientStops()
  * @see removeGradientStop(unsigned int n)
  */
  GradientStop* removeGradientStop(const std::string& sid);


  /**
   * Predicate returning @c true if this abstract GradientBase is of type
   * LinearGradient
   *
   * @return @c true if this abstract GradientBase is of type LinearGradient,
   * @c false otherwise
   */
  virtual bool isLinearGradient() const;


  /**
   * Predicate returning @c true if this abstract GradientBase is of type
   * RadialGradient
   *
   * @return @c true if this abstract GradientBase is of type RadialGradient,
   * @c false otherwise
   */
  virtual bool isRadialGradient() const;


  /**
   * Returns the XML element name of this GradientBase object.
   *
   * For GradientBase, the XML element name is always @c "gradientBase".
   *
   * @return the name of this element, i.e. @c "gradientBase".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this GradientBase object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this GradientBase object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_GRADIENTDEFINITION, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * GradientBase object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * GradientBase have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the GradientBase object are:
   * @li "id"
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
   * Connects to child elements
   */
  virtual void connectToChild();

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */


  /**
   * Creates an XMLNode object from this GradientBase object.
   *
   * @return the XMLNode with the XML representation for the 
   * GradientBase object.
   *
   * This method is purely abstract and needs to be implemented
   * by derived classes.
   */
  virtual XMLNode toXML() const=0;

  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this GradientBase.
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
   * Returns the value of the "attributeName" attribute of this GradientBase.
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
   * Returns the value of the "attributeName" attribute of this GradientBase.
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
   * Returns the value of the "attributeName" attribute of this GradientBase.
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
   * Returns the value of the "attributeName" attribute of this GradientBase.
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
   * Predicate returning @c true if this GradientBase's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this GradientBase's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this GradientBase.
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
   * Sets the value of the "attributeName" attribute of this GradientBase.
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
   * Sets the value of the "attributeName" attribute of this GradientBase.
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
   * Sets the value of the "attributeName" attribute of this GradientBase.
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
   * Sets the value of the "attributeName" attribute of this GradientBase.
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
   * Unsets the value of the "attributeName" attribute of this GradientBase.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this GradientBase.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this GradientBase.
   *
   * @param elementName, the name of the element to create.
   *
   * @param element, pointer to the element to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int addChildObject(const std::string& elementName,
                             const SBase* element);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Removes and returns the new "elementName" object with the given id in this
   * GradientBase.
   *
   * @param elementName, the name of the element to remove.
   *
   * @param id, the id of the element to remove.
   *
   * @return pointer to the element removed.
   */
  virtual SBase* removeChildObject(const std::string& elementName,
                                   const std::string& id);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this GradientBase.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this GradientBase.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int the index of the object to retrieve.
   *
   * @return pointer to the object.
   */
  virtual SBase* getObject(const std::string& elementName, unsigned int index);

  /** @endcond */




  #endif /* !SWIG */


  /**
   * Returns the first child element that has the given @p id in the model-wide
   * SId namespace, or @c NULL if no such object is found.
   *
   * @param id a string representing the id attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p id. If no such
   * object is found, this method returns @c NULL.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid. If no
   * such object is found this method returns @c NULL.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @param filter an ElementFilter that may impose restrictions on the objects
   * to be retrieved.
   *
   * @return a List pointer of pointers to all SBase child objects with any
   * restriction imposed.
   */
  virtual List* getAllElements(ElementFilter * filter = NULL);


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
   * Converts the given string into a spread method.
   * If the string does not represnt a valid spread method, PAD is
   * returned.
   */
  static int getSpreadMethodForString(const std::string& s);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * This method is used when writing out gradietns to XML.
   * I writes out the attributes and children that re common to linear and radial gradient.
   */
  static void addGradientAttributesAndChildren(const GradientBase& gradient,XMLAttributes& att,XMLNode& node);
  /** @endcond */


};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


/**
 * Creates a new LinearGradient (GradientBase_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GradientBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GradientBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * GradientBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
GradientBase_t *
GradientBase_createLinearGradient(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion);


/**
 * Creates a new RadialGradient (GradientBase_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * GradientBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GradientBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * GradientBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
GradientBase_t *
GradientBase_createRadialGradient(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this GradientBase_t object.
 *
 * @param gb the GradientBase_t structure.
 *
 * @return a (deep) copy of this GradientBase_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
GradientBase_t*
GradientBase_clone(const GradientBase_t* gb);


/**
 * Frees this GradientBase_t object.
 *
 * @param gb the GradientBase_t structure.
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
void
GradientBase_free(GradientBase_t* gb);


/**
 * Returns the value of the "id" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this GradientBase_t as a pointer
 * to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
char *
GradientBase_getId(const GradientBase_t * gb);


/**
 * Returns the value of the "name" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this GradientBase_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
char *
GradientBase_getName(const GradientBase_t * gb);


/**
 * Returns the value of the "spreadMethod" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure whose spreadMethod is sought.
 *
 * @return the value of the "spreadMethod" attribute of this GradientBase_t as
 * a @if clike GradientSpreadMethod_t@else int@endif@~.
 *
 * @copydetails doc_gradientbase_spreadMethod
 * @if clike The value is drawn from the enumeration
 * GradientSpreadMethod_t. @endif@~
 * The possible values returned by this method are:
 * @li @sbmlconstant{GRADIENT_SPREADMETHOD_PAD, GradientSpreadMethod_t}
 * @li @sbmlconstant{GRADIENT_SPREADMETHOD_REFLECT, GradientSpreadMethod_t}
 * @li @sbmlconstant{GRADIENT_SPREADMETHOD_REPEAT, GradientSpreadMethod_t}
 * @li @sbmlconstant{GRADIENT_SPREAD_METHOD_INVALID, GradientSpreadMethod_t}
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
GradientSpreadMethod_t
GradientBase_getSpreadMethod(const GradientBase_t * gb);


/**
 * Returns the value of the "spreadMethod" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure whose spreadMethod is sought.
 *
 * @return the value of the "spreadMethod" attribute of this GradientBase_t as
 * a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_gradientbase_spreadMethod
 * The possible values returned by this method are:
 * @li @c "pad"
 * @li @c "reflect"
 * @li @c "repeat"
 * @li @c "invalid"
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
char *
GradientBase_getSpreadMethodAsString(const GradientBase_t * gb);


/**
 * Predicate returning @c 1 (true) if this GradientBase_t's "id" attribute is
 * set.
 *
 * @param gb the GradientBase_t structure.
 *
 * @return @c 1 (true) if this GradientBase_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_isSetId(const GradientBase_t * gb);


/**
 * Predicate returning @c 1 (true) if this GradientBase_t's "name" attribute is
 * set.
 *
 * @param gb the GradientBase_t structure.
 *
 * @return @c 1 (true) if this GradientBase_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_isSetName(const GradientBase_t * gb);


/**
 * Predicate returning @c 1 (true) if this GradientBase_t's "spreadMethod"
 * attribute is set.
 *
 * @param gb the GradientBase_t structure.
 *
 * @return @c 1 (true) if this GradientBase_t's "spreadMethod" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_gradientbase_spreadMethod
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_isSetSpreadMethod(const GradientBase_t * gb);


/**
 * Sets the value of the "id" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling GradientBase_unsetId().
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_setId(GradientBase_t * gb, const char * id);


/**
 * Sets the value of the "name" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling GradientBase_unsetName().
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_setName(GradientBase_t * gb, const char * name);


/**
 * Sets the value of the "spreadMethod" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure.
 *
 * @param spreadMethod @if clike GradientSpreadMethod_t@else int@endif@~
 * value of the "spreadMethod" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_gradientbase_spreadMethod
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_setSpreadMethod(GradientBase_t * gb,
                             GradientSpreadMethod_t spreadMethod);


/**
 * Sets the value of the "spreadMethod" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure.
 *
 * @param spreadMethod const char * of the "spreadMethod" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_gradientbase_spreadMethod
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_setSpreadMethodAsString(GradientBase_t * gb,
                                     const char * spreadMethod);


/**
 * Unsets the value of the "id" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_unsetId(GradientBase_t * gb);


/**
 * Unsets the value of the "name" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_unsetName(GradientBase_t * gb);


/**
 * Unsets the value of the "spreadMethod" attribute of this GradientBase_t.
 *
 * @param gb the GradientBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_gradientbase_spreadMethod
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_unsetSpreadMethod(GradientBase_t * gb);


/**
 * Returns a ListOf_t * containing GradientStop_t objects from this
 * GradientBase_t.
 *
 * @param gb the GradientBase_t structure whose ListOfGradientStops is sought.
 *
 * @return the ListOfGradientStops from this GradientBase_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see GradientBase_addGradientStop()
 * @see GradientBase_createGradientStop()
 * @see GradientBase_getGradientStopById()
 * @see GradientBase_getGradientStop()
 * @see GradientBase_getNumGradientStops()
 * @see GradientBase_removeGradientStopById()
 * @see GradientBase_removeGradientStop()
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
ListOf_t*
GradientBase_getListOfGradientStops(GradientBase_t* gb);


/**
 * Get a GradientStop_t from the GradientBase_t.
 *
 * @param gb the GradientBase_t structure to search.
 *
 * @param n an unsigned int representing the index of the GradientStop_t to
 * retrieve.
 *
 * @return the nth GradientStop_t in the ListOfGradientStops within this
 * GradientBase.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
GradientStop_t*
GradientBase_getGradientStop(GradientBase_t* gb, unsigned int n);


/**
 * Adds a copy of the given GradientStop_t to this GradientBase_t.
 *
 * @param gb the GradientBase_t structure to which the GradientStop_t should be
 * added.
 *
 * @param gs the GradientStop_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_addGradientStop(GradientBase_t* gb, const GradientStop_t* gs);


/**
 * Get the number of GradientStop_t objects in this GradientBase_t.
 *
 * @param gb the GradientBase_t structure to query.
 *
 * @return the number of GradientStop_t objects in this GradientBase_t.
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
unsigned int
GradientBase_getNumGradientStops(GradientBase_t* gb);


/**
 * Creates a new GradientStop_t object, adds it to this GradientBase_t object
 * and returns the GradientStop_t object created.
 *
 * @param gb the GradientBase_t structure to which the GradientStop_t should be
 * added.
 *
 * @return a new GradientStop_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
GradientStop_t*
GradientBase_createGradientStop(GradientBase_t* gb);


/**
 * Removes the nth GradientStop_t from this GradientBase_t and returns a
 * pointer to it.
 *
 * @param gb the GradientBase_t structure to search.
 *
 * @param n an unsigned int representing the index of the GradientStop_t to
 * remove.
 *
 * @return a pointer to the nth GradientStop_t in this GradientBase_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
GradientStop_t*
GradientBase_removeGradientStop(GradientBase_t* gb, unsigned int n);


/**
 * Predicate returning @c 1 if this GradientBase_t is of type LinearGradient_t
 *
 * @param gb the GradientBase_t structure.
 *
 * @return @c 1 if this GradientBase_t is of type LinearGradient_t, @c 0
 * otherwise
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_isLinearGradient(const GradientBase_t * gb);


/**
 * Predicate returning @c 1 if this GradientBase_t is of type RadialGradient_t
 *
 * @param gb the GradientBase_t structure.
 *
 * @return @c 1 if this GradientBase_t is of type RadialGradient_t, @c 0
 * otherwise
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_isRadialGradient(const GradientBase_t * gb);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GradientBase_t object have been set.
 *
 * @param gb the GradientBase_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * GradientBase_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the GradientBase_t object are:
 * @li "id"
 *
 * @memberof GradientBase_t
 */
LIBSBML_EXTERN
int
GradientBase_hasRequiredAttributes(const GradientBase_t * gb);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
GradientBase::SPREADMETHOD
SpreadMethod_fromString(const char* name);

LIBSBML_EXTERN
const char*
SpreadMethod_toString(GradientBase::SPREADMETHOD method);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END



#endif /* !GradientBase_H__ */


