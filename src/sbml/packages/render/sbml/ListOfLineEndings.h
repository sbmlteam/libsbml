/**
 * @file ListOfLineEndings.h
 * @brief Definition of the ListOfLineEndings class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
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
 * @class ListOfLineEndings
 * @sbmlbrief{render} A list of LineEnding objects.
 *
 * The ListOfLineEndings is a container for the LineEnding elements 
 * of a RenderInformationBase object. Each RenderInformation object that 
 * inherits from RenderInformationBase can contain its own ListOfLineEndings object.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see LineEnding
 * @see RenderInformationBase
 */


#ifndef ListOfLineEndings_H__
#define ListOfLineEndings_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/LineEnding.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfLineEndings : public ListOf
{

public:

  /**
   * Creates a new ListOfLineEndings using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfLineEndings.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfLineEndings.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ListOfLineEndings.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfLineEndings(unsigned int level = RenderExtension::getDefaultLevel(),
                    unsigned int version =
                      RenderExtension::getDefaultVersion(),
                    unsigned int pkgVersion =
                      RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfLineEndings using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfLineEndings(RenderPkgNamespaces *renderns);


  /**
   * Creates a new ListOfLineEndings object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * ListOfLineEndings object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the ListOfLineEndings
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  ListOfLineEndings(const XMLNode& node, unsigned int l2version=4);


  /**
   * Copy constructor for ListOfLineEndings.
   *
   * @param orig the ListOfLineEndings instance to copy.
   */
  ListOfLineEndings(const ListOfLineEndings& orig);


  /**
   * Assignment operator for ListOfLineEndings.
   *
   * @param rhs the ListOfLineEndings object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfLineEndings& operator=(const ListOfLineEndings& rhs);


  /**
   * Creates and returns a deep copy of this ListOfLineEndings object.
   *
   * @return a (deep) copy of this ListOfLineEndings object.
   */
  virtual ListOfLineEndings* clone() const;


  /**
   * Destructor for ListOfLineEndings.
   */
  virtual ~ListOfLineEndings();


  /**
   * Get a LineEnding from the ListOfLineEndings.
   *
   * @param n an unsigned int representing the index of the LineEnding to
   * retrieve.
   *
   * @return the nth LineEnding in this ListOfLineEndings.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see get(const std::string& sid)
   * @see getNumLineEndings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual LineEnding* get(unsigned int n);


  /**
   * Get a LineEnding from the ListOfLineEndings.
   *
   * @param n an unsigned int representing the index of the LineEnding to
   * retrieve.
   *
   * @return the nth LineEnding in this ListOfLineEndings.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see get(const std::string& sid)
   * @see getNumLineEndings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const LineEnding* get(unsigned int n) const;


  /**
   * Get a LineEnding from the ListOfLineEndings based on its identifier.
   *
   * @param sid a string representing the identifier of the LineEnding to
   * retrieve.
   *
   * @return the LineEnding in this ListOfLineEndings with the given @p sid or
   * @c NULL if no such LineEnding exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see get(unsigned int n)
   * @see getNumLineEndings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual LineEnding* get(const std::string& sid);


  /**
   * Get a LineEnding from the ListOfLineEndings based on its identifier.
   *
   * @param sid a string representing the identifier of the LineEnding to
   * retrieve.
   *
   * @return the LineEnding in this ListOfLineEndings with the given @p sid or
   * @c NULL if no such LineEnding exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see get(unsigned int n)
   * @see getNumLineEndings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const LineEnding* get(const std::string& sid) const;


  /**
   * Removes the nth LineEnding from this ListOfLineEndings and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the LineEnding to
   * remove.
   *
   * @return a pointer to the nth LineEnding in this ListOfLineEndings.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumLineEndings()
   * @see remove(const std::string& sid)
   */
  virtual LineEnding* remove(unsigned int n);


  /**
   * Removes the LineEnding from this ListOfLineEndings based on its identifier
   * and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the LineEnding to
   * remove.
   *
   * @return the LineEnding in this ListOfLineEndings based on the identifier
   * or NULL if no such LineEnding exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumLineEndings()
   * @see remove(unsigned int n)
   */
  virtual LineEnding* remove(const std::string& sid);


  /**
   * Adds a copy of the given LineEnding to this ListOfLineEndings.
   *
   * @param le the LineEnding object to add.
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
   * @see createLineEnding()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumLineEndings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addLineEnding(const LineEnding* le);


  /**
   * Get the number of LineEnding objects in this ListOfLineEndings.
   *
   * @return the number of LineEnding objects in this ListOfLineEndings.
   *
   *
   * @see addLineEnding(const LineEnding* object)
   * @see createLineEnding()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumLineEndings() const;


  /**
   * Creates a new LineEnding object, adds it to this ListOfLineEndings object
   * and returns the LineEnding object created.
   *
   * @return a new LineEnding object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLineEnding(const LineEnding* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumLineEndings()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  LineEnding* createLineEnding();


  /**
   * Returns the XML element name of this ListOfLineEndings object.
   *
   * For ListOfLineEndings, the XML element name is always
   * @c "listOfLineEndings".
   *
   * @return the name of this element, i.e. @c "listOfLineEndings".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfLineEndings object.
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
   * ListOfLineEndings object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfLineEndings:
   * @sbmlconstant{SBML_RENDER_LINEENDING, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;


  /**
   * Creates an XMLNode object from this ListOfLineEndings object.
   *
   * @return the XMLNode with the XML representation for the 
   * ListOfLineEndings object.
   */
  XMLNode toXML() const;


  #ifndef SWIG




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new LineEnding in this ListOfLineEndings
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
 * Get a LineEnding_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the LineEnding_t to
 * retrieve.
 *
 * @return the nth LineEnding_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfLineEndings_t
 */
LIBSBML_EXTERN
LineEnding_t*
ListOfLineEndings_getLineEnding(ListOf_t* lo, unsigned int n);


/**
 * Get a LineEnding_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the LineEnding_t to
 * retrieve.
 *
 * @return the LineEnding_t in this ListOf_t with the given @p sid or @c NULL
 * if no such LineEnding_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfLineEndings_t
 */
LIBSBML_EXTERN
LineEnding_t*
ListOfLineEndings_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth LineEnding_t from this ListOf_t and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the LineEnding_t to
 * remove.
 *
 * @return a pointer to the nth LineEnding_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfLineEndings_t
 */
LIBSBML_EXTERN
LineEnding_t*
ListOfLineEndings_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the LineEnding_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the LineEnding_t to
 * remove.
 *
 * @return the LineEnding_t in this ListOf_t based on the identifier or NULL if
 * no such LineEnding_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfLineEndings_t
 */
LIBSBML_EXTERN
LineEnding_t*
ListOfLineEndings_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfLineEndings_H__ */


