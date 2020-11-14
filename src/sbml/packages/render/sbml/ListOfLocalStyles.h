/**
 * @file ListOfLocalStyles.h
 * @brief Definition of the ListOfLocalStyles class.
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
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class ListOfLocalStyles
 * @sbmlbrief{render} A list of LocalStyle objects.
 * 
 * The ListOfLocalStyles is a container for the LocalStyle elements 
 * of a LocalRenderInformation object.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see LocalStyle
 * @see LocalRenderInformation
 */


#ifndef ListOfLocalStyles_H__
#define ListOfLocalStyles_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/LocalStyle.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfLocalStyles : public ListOf
{

public:

  /**
   * Creates a new ListOfLocalStyles using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfLocalStyles.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfLocalStyles.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ListOfLocalStyles.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfLocalStyles(unsigned int level = RenderExtension::getDefaultLevel(),
                    unsigned int version =
                      RenderExtension::getDefaultVersion(),
                    unsigned int pkgVersion =
                      RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfLocalStyles using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfLocalStyles(RenderPkgNamespaces *renderns);


  /**
  * Creates a new ListOfLocalStyles object from the given XMLNode object.
  * The XMLNode object has to contain a valid XML representation of a
  * ListOfLocalStyles object as defined in the render extension specification.
  * This method is normally called when render information is read from a file and
  * should normally not have to be called explicitly.
  *
  * @param node the XMLNode object reference that describes the ListOfLocalStyles
  * object to be instantiated.
  */
  ListOfLocalStyles(const XMLNode& node);


  /**
   * Copy constructor for ListOfLocalStyles.
   *
   * @param orig the ListOfLocalStyles instance to copy.
   */
  ListOfLocalStyles(const ListOfLocalStyles& orig);


  /**
   * Assignment operator for ListOfLocalStyles.
   *
   * @param rhs the ListOfLocalStyles object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfLocalStyles& operator=(const ListOfLocalStyles& rhs);


  /**
   * Creates and returns a deep copy of this ListOfLocalStyles object.
   *
   * @return a (deep) copy of this ListOfLocalStyles object.
   */
  virtual ListOfLocalStyles* clone() const;


  /**
   * Destructor for ListOfLocalStyles.
   */
  virtual ~ListOfLocalStyles();


  /**
   * Get a LocalStyle from the ListOfLocalStyles.
   *
   * @param n an unsigned int representing the index of the LocalStyle to
   * retrieve.
   *
   * @return the nth LocalStyle in this ListOfLocalStyles.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see get(const std::string& sid)
   * @see getNumLocalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual LocalStyle* get(unsigned int n);


  /**
   * Get a LocalStyle from the ListOfLocalStyles.
   *
   * @param n an unsigned int representing the index of the LocalStyle to
   * retrieve.
   *
   * @return the nth LocalStyle in this ListOfLocalStyles.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see get(const std::string& sid)
   * @see getNumLocalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const LocalStyle* get(unsigned int n) const;


  /**
   * Get a LocalStyle from the ListOfLocalStyles based on its identifier.
   *
   * @param sid a string representing the identifier of the LocalStyle to
   * retrieve.
   *
   * @return the LocalStyle in this ListOfLocalStyles with the given @p sid or
   * @c NULL if no such LocalStyle exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see get(unsigned int n)
   * @see getNumLocalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual LocalStyle* get(const std::string& sid);


  /**
   * Get a LocalStyle from the ListOfLocalStyles based on its identifier.
   *
   * @param sid a string representing the identifier of the LocalStyle to
   * retrieve.
   *
   * @return the LocalStyle in this ListOfLocalStyles with the given @p sid or
   * @c NULL if no such LocalStyle exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see get(unsigned int n)
   * @see getNumLocalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const LocalStyle* get(const std::string& sid) const;


  /**
   * Removes the nth LocalStyle from this ListOfLocalStyles and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the LocalStyle to
   * remove.
   *
   * @return a pointer to the nth LocalStyle in this ListOfLocalStyles.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumLocalStyles()
   * @see remove(const std::string& sid)
   */
  virtual LocalStyle* remove(unsigned int n);


  /**
   * Removes the LocalStyle from this ListOfLocalStyles based on its identifier
   * and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the LocalStyle to
   * remove.
   *
   * @return the LocalStyle in this ListOfLocalStyles based on the identifier
   * or NULL if no such LocalStyle exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumLocalStyles()
   * @see remove(unsigned int n)
   */
  virtual LocalStyle* remove(const std::string& sid);


  /**
   * Adds a copy of the given LocalStyle to this ListOfLocalStyles.
   *
   * @param ls the LocalStyle object to add.
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
   * @see createLocalStyle()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumLocalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addLocalStyle(const LocalStyle* ls);


  /**
   * Get the number of LocalStyle objects in this ListOfLocalStyles.
   *
   * @return the number of LocalStyle objects in this ListOfLocalStyles.
   *
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see createLocalStyle()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumLocalStyles() const;


  /**
   * Creates a new LocalStyle object, adds it to this ListOfLocalStyles object
   * and returns the LocalStyle object created.
   *
   * @return a new LocalStyle object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addLocalStyle(const LocalStyle* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumLocalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  LocalStyle* createLocalStyle();


  /**
   * Returns the XML element name of this ListOfLocalStyles object.
   *
   * For ListOfLocalStyles, the XML element name is always
   * @c "listOfLocalStyles".
   *
   * @return the name of this element, i.e. @c "listOfLocalStyles".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfLocalStyles object.
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
   * ListOfLocalStyles object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfLocalStyles:
   * @sbmlconstant{SBML_RENDER_LOCALSTYLE, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;




  #ifndef SWIG




  #endif /* !SWIG */

  /**
  * Creates an XMLNode object from this ListOfLocalStyles object.
  *
  * @return the XMLNode with the XML representation for the
  * ListOfLocalStyles object.
  */
  XMLNode toXML() const;

protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new LocalStyle in this ListOfLocalStyles
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
 * Get a LocalStyle_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the LocalStyle_t to
 * retrieve.
 *
 * @return the nth LocalStyle_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfLocalStyles_t
 */
LIBSBML_EXTERN
LocalStyle_t*
ListOfLocalStyles_getLocalStyle(ListOf_t* lo, unsigned int n);


/**
 * Get a LocalStyle_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the LocalStyle_t to
 * retrieve.
 *
 * @return the LocalStyle_t in this ListOf_t with the given @p sid or @c NULL
 * if no such LocalStyle_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfLocalStyles_t
 */
LIBSBML_EXTERN
LocalStyle_t*
ListOfLocalStyles_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth LocalStyle_t from this ListOf_t and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the LocalStyle_t to
 * remove.
 *
 * @return a pointer to the nth LocalStyle_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfLocalStyles_t
 */
LIBSBML_EXTERN
LocalStyle_t*
ListOfLocalStyles_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the LocalStyle_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the LocalStyle_t to
 * remove.
 *
 * @return the LocalStyle_t in this ListOf_t based on the identifier or NULL if
 * no such LocalStyle_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfLocalStyles_t
 */
LIBSBML_EXTERN
LocalStyle_t*
ListOfLocalStyles_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfLocalStyles_H__ */


