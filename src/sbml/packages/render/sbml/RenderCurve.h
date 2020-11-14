/**
 * @file    RenderCurve.h
 * @brief Definition of the RenderCurve class.
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
 * @class RenderCurve
 * @sbmlbrief{render} Representation of curves.
 *
 * The curve concept in the SBML Level&nbsp;3 Render package is similar to
 * the curves in the SBML layout.  Each curve consists of a number of either
 * straight line segments or cubic bezier elements.  The two element types
 * can also be mixed in a single curve object.
 *
 * In contrast to layout curves, render curves can not have gaps and the
 * individual coordinates of the curve elements can be specified as a
 * combination of absolute and relative values.
 *
 * Another difference to layout curves is the fact that render curves can
 * specify decorations to be applied to the start and/or the end of the
 * curve.
 *
 * Since RenderCurve is derived from GraphicalPrimitive1D, it inherits all
 * its attributes and methods.
 *
 * @see LineEnding
 */

#ifndef RenderCurve_H__
#define RenderCurve_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/GraphicalPrimitive1D.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/ListOfCurveElements.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN RenderCurve : public GraphicalPrimitive1D
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mStartHead;
  std::string mEndHead;
  ListOfCurveElements mRenderPoints;

  /** @endcond */

public:

  /**
   * Creates a new RenderCurve using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * RenderCurve.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * RenderCurve.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this RenderCurve.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RenderCurve(unsigned int level = RenderExtension::getDefaultLevel(),
              unsigned int version = RenderExtension::getDefaultVersion(),
              unsigned int pkgVersion =
                RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new RenderCurve using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RenderCurve(RenderPkgNamespaces *renderns);


  /**
   * Creates a new RenderCurve object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * RenderCurve object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the RenderCurve
   * object to be instantiated.
   * @param l2version the version of SBML Level&nbsp;2 to target.
   */
  RenderCurve(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Instantiates an empty curve object with the given @p id.
   * The decorations  are unset and there are no curve elements.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  RenderCurve(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED

  /**
   * Copy constructor for RenderCurve.
   *
   * @param orig the RenderCurve instance to copy.
   */
  RenderCurve(const RenderCurve& orig);


  /**
   * Assignment operator for RenderCurve.
   *
   * @param rhs the RenderCurve object whose values are to be used as the basis
   * of the assignment.
   */
  RenderCurve& operator=(const RenderCurve& rhs);


  /**
   * Creates and returns a deep copy of this RenderCurve object.
   *
   * @return a (deep) copy of this RenderCurve object.
   */
  virtual RenderCurve* clone() const;


  /**
   * Destructor for RenderCurve.
   */
  virtual ~RenderCurve();


  /**
   * Returns the value of the "startHead" attribute of this RenderCurve.
   *
   * @return the value of the "startHead" attribute of this RenderCurve as a
   * string.
   */
  const std::string& getStartHead() const;


  /**
   * Returns the value of the "endHead" attribute of this RenderCurve.
   *
   * @return the value of the "endHead" attribute of this RenderCurve as a
   * string.
   */
  const std::string& getEndHead() const;


  /**
   * Predicate returning @c true if this RenderCurve's "startHead" attribute is
   * set.
   *
   * @return @c true if this RenderCurve's "startHead" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetStartHead() const;


  /**
   * Predicate returning @c true if this RenderCurve's "endHead" attribute is
   * set.
   *
   * @return @c true if this RenderCurve's "endHead" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetEndHead() const;


  /**
   * Sets the value of the "startHead" attribute of this RenderCurve.
   *
   * @param startHead std::string& value of the "startHead" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setStartHead(const std::string& startHead);


  /**
   * Sets the value of the "endHead" attribute of this RenderCurve.
   *
   * @param endHead std::string& value of the "endHead" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setEndHead(const std::string& endHead);


  /**
   * Unsets the value of the "startHead" attribute of this RenderCurve.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStartHead();


  /**
   * Unsets the value of the "endHead" attribute of this RenderCurve.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetEndHead();


  /**
   * Returns the ListOfCurveElements from this RenderCurve.
   *
   * @return the ListOfCurveElements from this RenderCurve.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(unsigned int n)
   */
  const ListOfCurveElements* getListOfElements() const;


  /**
   * Returns the ListOfCurveElements from this RenderCurve.
   *
   * @return the ListOfCurveElements from this RenderCurve.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(unsigned int n)
   */
  ListOfCurveElements* getListOfElements();


  /**
   * Get a RenderPoint from the RenderCurve.
   *
   * @param n an unsigned int representing the index of the RenderPoint to
   * retrieve.
   *
   * @return the nth RenderPoint in the ListOfCurveElements within this
   * RenderCurve or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see getNumElements()
   * @see removeElement(unsigned int n)
   */
  RenderPoint* getElement(unsigned int n);


  /**
   * Get a RenderPoint from the RenderCurve.
   *
   * @param n an unsigned int representing the index of the RenderPoint to
   * retrieve.
   *
   * @return the nth RenderPoint in the ListOfCurveElements within this
   * RenderCurve or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see getNumElements()
   * @see removeElement(unsigned int n)
   */
  const RenderPoint* getElement(unsigned int n) const;


#ifndef OMIT_DEPRECATED
  /**
   * Returns a pointer to the curve segment with the given index or NULL if
   * the id is invalid.
   *
   * This method call is deprecated, please use getElement instead.
   *
   * @param index the index of the curve element to be returned
   *
   * @return a pointer to the curve element with the given index or NULL 
   * if the index was out of bounds.
   */
  RenderPoint* getCurveElement(unsigned int index);
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
  /**
   * Returns a const pointer to the curve segment with the given index or NULL if
   * the id is invalid.
   *
   * This method call is deprecated, please use getElement instead.
   *
   * @param index the index of the curve element to be returned
   *
   * @return a const pointer to the curve element with the given index or NULL 
   * if the index was out of bounds.
   */
  const RenderPoint* getCurveElement(unsigned int index) const;
#endif // OMIT_DEPRECATED


  /**
   * Adds a copy of the given RenderPoint to this RenderCurve.
   *
   * @param rp the RenderPoint object to add.
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
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(unsigned int n)
   */
  int addElement(const RenderPoint* rp);


  /**
   * Get the number of RenderPoint objects in this RenderCurve.
   *
   * @return the number of RenderPoint objects in this RenderCurve.
   *
   *
   * @see addElement(const RenderPoint* object)
   * @see getElement(unsigned int n)
   * @see removeElement(unsigned int n)
   */
  unsigned int getNumElements() const;


  /**
   * Creates a new RenderPoint object, adds it to this RenderCurve object and
   * returns the RenderPoint object created.
   *
   * @return a new RenderPoint object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(unsigned int n)
   */
  RenderPoint* createPoint();


  /**
   * Creates a new RenderCubicBezier object, adds it to this RenderCurve object
   * and returns the RenderCubicBezier object created.
   *
   * @return a new RenderCubicBezier object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(unsigned int n)
   */
  RenderCubicBezier* createCubicBezier();


  /**
   * Removes the nth RenderPoint from this RenderCurve and returns a pointer to
   * it.
   *
   * @param n an unsigned int representing the index of the RenderPoint to
   * remove.
   *
   * @return a pointer to the nth RenderPoint in this RenderCurve.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   */
  RenderPoint* removeElement(unsigned int n);


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this RenderCurve object.
   *
   * For RenderCurve, the XML element name is always @c "curve".
   *
   * @return the name of this element, i.e. @c "curve".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this RenderCurve object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_CURVE, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * RenderCurve object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * RenderCurve have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * RenderCurve object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * RenderCurve have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the RenderCurve object are:
   */
  virtual bool hasRequiredElements() const;



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


  /** @cond doxygenLibsbmlInternal */

 /**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
  virtual void setParentSBMLObject(SBase* sb);

  /** @endcond */

  
  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this RenderCurve.
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
   * Returns the value of the "attributeName" attribute of this RenderCurve.
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
   * Returns the value of the "attributeName" attribute of this RenderCurve.
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
   * Returns the value of the "attributeName" attribute of this RenderCurve.
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
   * Returns the value of the "attributeName" attribute of this RenderCurve.
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
   * Predicate returning @c true if this RenderCurve's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this RenderCurve's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this RenderCurve.
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
   * Sets the value of the "attributeName" attribute of this RenderCurve.
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
   * Sets the value of the "attributeName" attribute of this RenderCurve.
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
   * Sets the value of the "attributeName" attribute of this RenderCurve.
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
   * Sets the value of the "attributeName" attribute of this RenderCurve.
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
   * Unsets the value of the "attributeName" attribute of this RenderCurve.
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
   * Creates and returns an new "elementName" object in this RenderCurve.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this RenderCurve.
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
   * RenderCurve.
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
   * Returns the number of "elementName" in this RenderCurve.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this RenderCurve.
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



 /**
  * Creates an XMLNode object from this RenderCurve object.
  *
  * @return the XMLNode with the XML representation for the 
  * RenderCurve object.
  */
 virtual XMLNode toXML() const;

protected:

  friend class RenderGroup;

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


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new RenderCurve_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * RenderCurve_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * RenderCurve_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * RenderCurve_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
RenderCurve_t *
RenderCurve_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this RenderCurve_t object.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @return a (deep) copy of this RenderCurve_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
RenderCurve_t*
RenderCurve_clone(const RenderCurve_t* rc);


/**
 * Frees this RenderCurve_t object.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
void
RenderCurve_free(RenderCurve_t* rc);


/**
 * Returns the value of the "startHead" attribute of this RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure whose startHead is sought.
 *
 * @return the value of the "startHead" attribute of this RenderCurve_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
char *
RenderCurve_getStartHead(const RenderCurve_t * rc);


/**
 * Returns the value of the "endHead" attribute of this RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure whose endHead is sought.
 *
 * @return the value of the "endHead" attribute of this RenderCurve_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
char *
RenderCurve_getEndHead(const RenderCurve_t * rc);


/**
 * Predicate returning @c 1 (true) if this RenderCurve_t's "startHead"
 * attribute is set.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @return @c 1 (true) if this RenderCurve_t's "startHead" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
int
RenderCurve_isSetStartHead(const RenderCurve_t * rc);


/**
 * Predicate returning @c 1 (true) if this RenderCurve_t's "endHead" attribute
 * is set.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @return @c 1 (true) if this RenderCurve_t's "endHead" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
int
RenderCurve_isSetEndHead(const RenderCurve_t * rc);


/**
 * Sets the value of the "startHead" attribute of this RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @param startHead const char * value of the "startHead" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
int
RenderCurve_setStartHead(RenderCurve_t * rc, const char * startHead);


/**
 * Sets the value of the "endHead" attribute of this RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @param endHead const char * value of the "endHead" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
int
RenderCurve_setEndHead(RenderCurve_t * rc, const char * endHead);


/**
 * Unsets the value of the "startHead" attribute of this RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
int
RenderCurve_unsetStartHead(RenderCurve_t * rc);


/**
 * Unsets the value of the "endHead" attribute of this RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
int
RenderCurve_unsetEndHead(RenderCurve_t * rc);


/**
 * Returns a ListOf_t * containing RenderPoint_t objects from this
 * RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure whose ListOfCurveElements is sought.
 *
 * @return the ListOfCurveElements from this RenderCurve_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see RenderCurve_addElement()
 * @see RenderCurve_getElement()
 * @see RenderCurve_getNumElements()
 * @see RenderCurve_removeElement()
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
ListOf_t*
RenderCurve_getListOfElements(RenderCurve_t* rc);


/**
 * Get a RenderPoint_t from the RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure to search.
 *
 * @param n an unsigned int representing the index of the RenderPoint_t to
 * retrieve.
 *
 * @return the nth RenderPoint_t in the ListOfCurveElements within this
 * RenderCurve or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
RenderPoint_t*
RenderCurve_getElement(RenderCurve_t* rc, unsigned int n);


/**
 * Adds a copy of the given RenderPoint_t to this RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure to which the RenderPoint_t should be
 * added.
 *
 * @param rp the RenderPoint_t object to add.
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
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
int
RenderCurve_addElement(RenderCurve_t* rc, const RenderPoint_t* rp);


/**
 * Get the number of RenderPoint_t objects in this RenderCurve_t.
 *
 * @param rc the RenderCurve_t structure to query.
 *
 * @return the number of RenderPoint_t objects in this RenderCurve_t.
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
unsigned int
RenderCurve_getNumElements(RenderCurve_t* rc);


/**
 * Creates a new RenderPoint_t object, adds it to this RenderCurve_t object and
 * returns the RenderPoint_t object created.
 *
 * @param rc the RenderCurve_t structure to which the RenderPoint_t should be
 * added.
 *
 * @return a new RenderPoint_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
RenderPoint_t*
RenderCurve_createPoint(RenderCurve_t* rc);


/**
 * Creates a new RenderCubicBezier_t object, adds it to this RenderCurve_t
 * object and returns the RenderCubicBezier_t object created.
 *
 * @param rc the RenderCurve_t structure to which the RenderCubicBezier_t
 * should be added.
 *
 * @return a new RenderCubicBezier_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
RenderCubicBezier_t*
RenderCurve_createCubicBezier(RenderCurve_t* rc);


/**
 * Removes the nth RenderPoint_t from this RenderCurve_t and returns a pointer
 * to it.
 *
 * @param rc the RenderCurve_t structure to search.
 *
 * @param n an unsigned int representing the index of the RenderPoint_t to
 * remove.
 *
 * @return a pointer to the nth RenderPoint_t in this RenderCurve_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
RenderPoint_t*
RenderCurve_removeElement(RenderCurve_t* rc, unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RenderCurve_t object have been set.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * RenderCurve_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
int
RenderCurve_hasRequiredAttributes(const RenderCurve_t * rc);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * RenderCurve_t object have been set.
 *
 * @param rc the RenderCurve_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * RenderCurve_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the RenderCurve_t object are:
 *
 * @memberof RenderCurve_t
 */
LIBSBML_EXTERN
int
RenderCurve_hasRequiredElements(const RenderCurve_t * rc);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !RenderCurve_H__ */


