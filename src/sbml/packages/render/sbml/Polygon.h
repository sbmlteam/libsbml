/**
 * @file    Polygon.h
 * @brief   class representing a polygon
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
 * @class Polygon
 * @sbmlbrief{render} Representation of a Polygon
 *
 * The Polygon is very similar to the RenderCurve class. The only difference
 * is that in the polygon the end point of the last element in the curve
 * segment list is automatically connected to the start point of the first
 * element.
 *
 * Since a polygon is a closed shape and doesn't really have a start or an
 * end, it does not get decorations as the RenderCurve does.  So, a polygon
 * is always closed and can therefore have a fill style and fill style related
 * attributes.  Those attributes are inherited from the Polygon base class
 * GraphicalPrimitive2D.
 */

#ifndef Polygon_H__
#define Polygon_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/GraphicalPrimitive2D.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/ListOfCurveElements.h>
#include <sbml/xml/XMLNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Polygon : public GraphicalPrimitive2D
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfCurveElements mRenderPoints;

  /** @endcond */

public:

  /**
   * Creates a new Polygon using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Polygon.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Polygon.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this Polygon.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Polygon(unsigned int level = RenderExtension::getDefaultLevel(),
          unsigned int version = RenderExtension::getDefaultVersion(),
          unsigned int pkgVersion =
            RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new Polygon using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Polygon(RenderPkgNamespaces *renderns);


  /**
   * Creates a new Polygon object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * Polygon object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the Polygon
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  Polygon(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Instanciates a polygon with the given @p id and no elements.
   * All attributes inherited from GraphicalPrimitive are set as described
   * in the corresponding constructor of that class (@see GraphicalPrimitive2D)
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id id string for the polygon
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Polygon(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED

  /**
   * Copy constructor for Polygon.
   *
   * @param orig the Polygon instance to copy.
   */
  Polygon(const Polygon& orig);


  /**
   * Assignment operator for Polygon.
   *
   * @param rhs the Polygon object whose values are to be used as the basis of
   * the assignment.
   */
  Polygon& operator=(const Polygon& rhs);


  /**
   * Creates and returns a deep copy of this Polygon object.
   *
   * @return a (deep) copy of this Polygon object.
   */
  virtual Polygon* clone() const;


  /**
   * Destructor for Polygon.
   */
  virtual ~Polygon();


  /**
   * Returns the ListOfCurveElements from this Polygon.
   *
   * @return the ListOfCurveElements from this Polygon.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see createCubicBezier()
   * @see getElementBySId(const std::string& sid)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(const std::string& sid)
   * @see removeElement(unsigned int n)
   */
  const ListOfCurveElements* getListOfElements() const;


  /**
   * Returns the ListOfCurveElements from this Polygon.
   *
   * @return the ListOfCurveElements from this Polygon.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see createCubicBezier()
   * @see getElementBySId(const std::string& sid)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(const std::string& sid)
   * @see removeElement(unsigned int n)
   */
  ListOfCurveElements* getListOfElements();


  /**
   * Get a RenderPoint from the Polygon.
   *
   * @param n an unsigned int representing the index of the RenderPoint to
   * retrieve.
   *
   * @return the nth RenderPoint in the ListOfCurveElements within this
   * Polygon or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see createCubicBezier()
   * @see getElementBySId(const std::string& sid)
   * @see getNumElements()
   * @see removeElement(const std::string& sid)
   * @see removeElement(unsigned int n)
   */
  RenderPoint* getElement(unsigned int n);


  /**
   * Get a RenderPoint from the Polygon.
   *
   * @param n an unsigned int representing the index of the RenderPoint to
   * retrieve.
   *
   * @return the nth RenderPoint in the ListOfCurveElements within this
   * Polygon or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see createCubicBezier()
   * @see getElementBySId(const std::string& sid)
   * @see getNumElements()
   * @see removeElement(const std::string& sid)
   * @see removeElement(unsigned int n)
   */
  const RenderPoint* getElement(unsigned int n) const;


  /**
   * Adds a copy of the given RenderPoint to this Polygon.
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
   * @see createCubicBezier()
   * @see getElementBySId(const std::string& sid)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(const std::string& sid)
   * @see removeElement(unsigned int n)
   */
  int addElement(const RenderPoint* rp);


  /**
   * Get the number of RenderPoint objects in this Polygon.
   *
   * @return the number of RenderPoint objects in this Polygon.
   *
   *
   * @see addElement(const RenderPoint* object)
   * @see createCubicBezier()
   * @see getElementBySId(const std::string& sid)
   * @see getElement(unsigned int n)
   * @see removeElement(const std::string& sid)
   * @see removeElement(unsigned int n)
   */
  unsigned int getNumElements() const;


  /**
   * Creates a new RenderPoint object, adds it to this Polygon object and
   * returns the RenderPoint object created.
   *
   * @return a new RenderPoint object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see getElementBySId(const std::string& sid)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(const std::string& sid)
   * @see removeElement(unsigned int n)
   */
  RenderPoint* createPoint();


  /**
   * Creates a new RenderCubicBezier object, adds it to this Polygon object and
   * returns the RenderCubicBezier object created.
   *
   * @return a new RenderCubicBezier object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see getElementBySId(const std::string& sid)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(const std::string& sid)
   * @see removeElement(unsigned int n)
   */
  RenderCubicBezier* createCubicBezier();


  /**
   * Removes the nth RenderPoint from this Polygon and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the RenderPoint to
   * remove.
   *
   * @return a pointer to the nth RenderPoint in this Polygon.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addElement(const RenderPoint* object)
   * @see createCubicBezier()
   * @see getElementBySId(const std::string& sid)
   * @see getElement(unsigned int n)
   * @see getNumElements()
   * @see removeElement(const std::string& sid)
   */
  RenderPoint* removeElement(unsigned int n);


  /**
  * Removes the RenderPoint with the given id from this 
  * Polygon and returns a pointer to it.
  *
  * @param sid the ID of the RenderPoint to remove.
  *
  * @return a pointer to the removed RenderPoint.
  *
  * @copydetails doc_warning_returns_owned_pointer
  *
  * @see addElement(const RenderPoint* object)
  * @see createCubicBezier()
  * @see getElementBySId(const std::string& sid)
  * @see getElement(unsigned int n)
  * @see getNumElements()
  * @see removeElement(unsigned int n)
  */
  RenderPoint* removeElement(const std::string& sid);


  /**
   * Returns the XML element name of this Polygon object.
   *
   * For Polygon, the XML element name is always @c "polygon".
   *
   * @return the name of this element, i.e. @c "polygon".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Polygon object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_POLYGON, SBMLRenderTypeCode_t}.
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




  #ifndef SWIG


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this Polygon.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this Polygon.
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
   * Polygon.
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
   * Returns the number of "elementName" in this Polygon.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this Polygon.
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
   * Creates an XMLNode object from this Polygon object.
   *
   * @return the XMLNode with the XML representation for the 
   * Polygon object.
   */
  XMLNode toXML() const;



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


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new Polygon_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Polygon_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Polygon_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Polygon_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
Polygon_t *
Polygon_create(unsigned int level,
               unsigned int version,
               unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Polygon_t object.
 *
 * @param p the Polygon_t structure.
 *
 * @return a (deep) copy of this Polygon_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
Polygon_t*
Polygon_clone(const Polygon_t* p);


/**
 * Frees this Polygon_t object.
 *
 * @param p the Polygon_t structure.
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
void
Polygon_free(Polygon_t* p);


/**
 * Returns a ListOf_t * containing RenderPoint_t objects from this Polygon_t.
 *
 * @param p the Polygon_t structure whose ListOfCurveElements is sought.
 *
 * @return the ListOfCurveElements from this Polygon_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see Polygon_addElement()
 * @see Polygon_createElement()
 * @see Polygon_getElementById()
 * @see Polygon_getElement()
 * @see Polygon_getNumElements()
 * @see Polygon_removeElementById()
 * @see Polygon_removeElement()
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
ListOf_t*
Polygon_getListOfElements(Polygon_t* p);


/**
 * Get a RenderPoint_t from the Polygon_t.
 *
 * @param p the Polygon_t structure to search.
 *
 * @param n an unsigned int representing the index of the RenderPoint_t to
 * retrieve.
 *
 * @return the nth RenderPoint_t in the ListOfCurveElements within this
 * Polygon or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
RenderPoint_t*
Polygon_getElement(Polygon_t* p, unsigned int n);


/**
 * Adds a copy of the given RenderPoint_t to this Polygon_t.
 *
 * @param p the Polygon_t structure to which the RenderPoint_t should be added.
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
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
int
Polygon_addElement(Polygon_t* p, const RenderPoint_t* rp);


/**
 * Get the number of RenderPoint_t objects in this Polygon_t.
 *
 * @param p the Polygon_t structure to query.
 *
 * @return the number of RenderPoint_t objects in this Polygon_t.
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
unsigned int
Polygon_getNumElements(Polygon_t* p);


/**
 * Creates a new RenderPoint_t object, adds it to this Polygon_t object and
 * returns the RenderPoint_t object created.
 *
 * @param p the Polygon_t structure to which the RenderPoint_t should be added.
 *
 * @return a new RenderPoint_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
RenderPoint_t*
Polygon_createPoint(Polygon_t* p);


/**
 * Creates a new RenderCubicBezier_t object, adds it to this Polygon_t object
 * and returns the RenderCubicBezier_t object created.
 *
 * @param p the Polygon_t structure to which the RenderCubicBezier_t should be
 * added.
 *
 * @return a new RenderCubicBezier_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
RenderCubicBezier_t*
Polygon_createCubicBezier(Polygon_t* p);


/**
 * Removes the nth RenderPoint_t from this Polygon_t and returns a pointer to
 * it.
 *
 * @param p the Polygon_t structure to search.
 *
 * @param n an unsigned int representing the index of the RenderPoint_t to
 * remove.
 *
 * @return a pointer to the nth RenderPoint_t in this Polygon_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
RenderPoint_t*
Polygon_removeElement(Polygon_t* p, unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Polygon_t object have been set.
 *
 * @param p the Polygon_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Polygon_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
int
Polygon_hasRequiredAttributes(const Polygon_t * p);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * Polygon_t object have been set.
 *
 * @param p the Polygon_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * Polygon_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the Polygon_t object are:
 *
 * @memberof Polygon_t
 */
LIBSBML_EXTERN
int
Polygon_hasRequiredElements(const Polygon_t * p);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Polygon_H__ */


