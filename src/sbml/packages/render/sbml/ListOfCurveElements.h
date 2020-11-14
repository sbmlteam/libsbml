/**
 * @file    ListOfCurveElements.h
 * @brief Definition of the ListOfCurveElements class.
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
 * @class ListOfCurveElements
 * @sbmlbrief{render} A list of curve elements.
 *
 * The ListOfCurveElements is libSBML's implementation of what is called
 * %ListOfElements in the SBML Render specification.  It provides an
 * alternative way of specifying curves and polygons.
 *
 * @copydetails doc_what_is_listof
 *
 * @see RenderPoint
 * @see RenderCubicBezier
 */

#ifndef ListOfCurveElements_H__
#define ListOfCurveElements_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RenderPoint.h>
#include <sbml/packages/render/sbml/RenderCubicBezier.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class RenderCubicBezier;

class LIBSBML_EXTERN ListOfCurveElements : public ListOf
{

public:

  /**
   * Creates a new ListOfCurveElements using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * The ListOfCurveElements is libSBML's implementation of what is called
   * %ListOfElements in the SBML Render specification.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfCurveElements.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfCurveElements.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ListOfCurveElements.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCurveElements(unsigned int level = RenderExtension::getDefaultLevel(),
                      unsigned int version =
                        RenderExtension::getDefaultVersion(),
                      unsigned int pkgVersion =
                        RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCurveElements using the given RenderPkgNamespaces
   * object.
   *
   * The ListOfCurveElements is libSBML's implementation of what is called
   * %ListOfElements in the SBML Render specification.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCurveElements(RenderPkgNamespaces *renderns);


  /**
   * Creates a new ListOfCurveElements object from the given XMLNode object.
   *
   * This method is normally called when Render information is read from a
   * file and should normally not have to be called explicitly.  The XMLNode
   * object @p node must contain a valid XML representation of a
   * ListOfElements object as defined in the Render package specification.
   *
   * @param node the XMLNode object reference that describes the ListOfCurveElements
   * object to be instantiated.
   *
   * @param l2version The version of SBML Level&nbsp;2.
   */
  ListOfCurveElements(const XMLNode& node, unsigned int l2version=4);


  /**
   * Copy constructor for ListOfCurveElements.
   *
   * @param orig the ListOfCurveElements instance to copy.
   */
  ListOfCurveElements(const ListOfCurveElements& orig);


  /**
   * Assignment operator for ListOfCurveElements.
   *
   * @param rhs the ListOfCurveElements object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfCurveElements& operator=(const ListOfCurveElements& rhs);


  /**
   * Creates and returns a deep copy of this ListOfCurveElements object.
   *
   * @return a (deep) copy of this ListOfCurveElements object.
   */
  virtual ListOfCurveElements* clone() const;


  /**
   * Destructor for ListOfCurveElements.
   */
  virtual ~ListOfCurveElements();


  /**
   * Get a RenderPoint from the ListOfCurveElements.
   *
   * @param n an unsigned int representing the index of the RenderPoint to
   * retrieve.
   *
   * @return the nth RenderPoint in this ListOfCurveElements.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addRenderPoint(const RenderPoint* object)
   * @see createCubicBezier()
   * @see get(const std::string& sid)
   * @see getNumRenderPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual RenderPoint* get(unsigned int n);


  /**
   * Get a RenderPoint from the ListOfCurveElements.
   *
   * @param n an unsigned int representing the index of the RenderPoint to
   * retrieve.
   *
   * @return the nth RenderPoint in this ListOfCurveElements.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addRenderPoint(const RenderPoint* object)
   * @see createCubicBezier()
   * @see get(const std::string& sid)
   * @see getNumRenderPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const RenderPoint* get(unsigned int n) const;


  /**
   * Get a RenderPoint from the ListOfCurveElements based on its identifier.
   *
   * @param sid a string representing the identifier of the RenderPoint to
   * retrieve.
   *
   * @return the RenderPoint in this ListOfCurveElements with the given @p sid
   * or @c NULL if no such RenderPoint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addRenderPoint(const RenderPoint* object)
   * @see createCubicBezier()
   * @see get(unsigned int n)
   * @see getNumRenderPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual RenderPoint* get(const std::string& sid);


  /**
   * Get a RenderPoint from the ListOfCurveElements based on its identifier.
   *
   * @param sid a string representing the identifier of the RenderPoint to
   * retrieve.
   *
   * @return the RenderPoint in this ListOfCurveElements with the given @p sid
   * or @c NULL if no such RenderPoint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addRenderPoint(const RenderPoint* object)
   * @see createCubicBezier()
   * @see get(unsigned int n)
   * @see getNumRenderPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const RenderPoint* get(const std::string& sid) const;


  /**
   * Removes the nth RenderPoint from this ListOfCurveElements and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the RenderPoint to
   * remove.
   *
   * @return a pointer to the nth RenderPoint in this ListOfCurveElements.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addRenderPoint(const RenderPoint* object)
   * @see createCubicBezier()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumRenderPoints()
   * @see remove(const std::string& sid)
   */
  virtual RenderPoint* remove(unsigned int n);


  /**
   * Removes the RenderPoint from this ListOfCurveElements based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the RenderPoint to
   * remove.
   *
   * @return the RenderPoint in this ListOfCurveElements based on the
   * identifier or NULL if no such RenderPoint exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addRenderPoint(const RenderPoint* object)
   * @see createCubicBezier()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumRenderPoints()
   * @see remove(unsigned int n)
   */
  virtual RenderPoint* remove(const std::string& sid);


  /**
   * Adds a copy of the given RenderPoint to this ListOfCurveElements.
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
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumRenderPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addRenderPoint(const RenderPoint* rp);


  /**
   * Get the number of RenderPoint objects in this ListOfCurveElements.
   *
   * @return the number of RenderPoint objects in this ListOfCurveElements.
   *
   *
   * @see addRenderPoint(const RenderPoint* object)
   * @see createCubicBezier()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumRenderPoints() const;


  /**
   * Creates a new RenderCubicBezier object, adds it to this
   * ListOfCurveElements object and returns the RenderCubicBezier object
   * created.
   *
   * @return a new RenderCubicBezier object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addRenderPoint(const RenderPoint* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumRenderPoints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  RenderCubicBezier* createCubicBezier();


  /**
   * Returns the XML element name of this ListOfCurveElements object.
   *
   * For ListOfCurveElements, the XML element name is always
   * @c "listOfElements".
   *
   * @return the name of this element, i.e. @c "listOfElements".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfCurveElements object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_LIST_OF, SBMLTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   */
  virtual int getTypeCode() const;


  /**
   * Returns the libSBML type code for the SBML objects contained in this
   * ListOfCurveElements object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfCurveElements:
   * @sbmlconstant{SBML_RENDER_POINT, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * checks concrete types
   */
  virtual bool isValidTypeForList(SBase* item);
  /** @endcond */


  /**
   * Creates an XMLNode object from this ListOfCurveElements object.
   *
   * @return the XMLNode with the XML representation for the 
   * ListOfCurveElements object.
   */
  XMLNode toXML() const;


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new RenderPoint in this ListOfCurveElements
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Render package
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;

  /** @endcond */



};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Get a RenderPoint_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the RenderPoint_t to
 * retrieve.
 *
 * @return the nth RenderPoint_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCurveElements_t
 */
LIBSBML_EXTERN
RenderPoint_t*
ListOfCurveElements_getRenderPoint(ListOf_t* lo, unsigned int n);


/**
 * Get a RenderPoint_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the RenderPoint_t to
 * retrieve.
 *
 * @return the RenderPoint_t in this ListOf_t with the given @p sid or @c NULL
 * if no such RenderPoint_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCurveElements_t
 */
LIBSBML_EXTERN
RenderPoint_t*
ListOfCurveElements_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth RenderPoint_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the RenderPoint_t to
 * remove.
 *
 * @return a pointer to the nth RenderPoint_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfCurveElements_t
 */
LIBSBML_EXTERN
RenderPoint_t*
ListOfCurveElements_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the RenderPoint_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the RenderPoint_t to
 * remove.
 *
 * @return the RenderPoint_t in this ListOf_t based on the identifier or NULL
 * if no such RenderPoint_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfCurveElements_t
 */
LIBSBML_EXTERN
RenderPoint_t*
ListOfCurveElements_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfCurveElements_H__ */


