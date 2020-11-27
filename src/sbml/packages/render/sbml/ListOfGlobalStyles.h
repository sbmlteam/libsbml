/**
 * @file ListOfGlobalStyles.h
 * @brief Definition of the ListOfGlobalStyles class.
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
 * @class ListOfGlobalStyles
 * @sbmlbrief{render} A list of GlobalStyle objects.
 *
 * The ListOfGlobalStyles is a container for the GlobalStyle elements of a
 * GlobalRenderInformation object.
 *
 * @copydetails doc_what_is_listof
 *
 * @see GlobalStyle
 */


#ifndef ListOfGlobalStyles_H__
#define ListOfGlobalStyles_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/GlobalStyle.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfGlobalStyles : public ListOf
{

public:

  /**
   * Creates a new ListOfGlobalStyles using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfGlobalStyles.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfGlobalStyles.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ListOfGlobalStyles.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGlobalStyles(unsigned int level = RenderExtension::getDefaultLevel(),
                     unsigned int version =
                       RenderExtension::getDefaultVersion(),
                     unsigned int pkgVersion =
                       RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfGlobalStyles using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGlobalStyles(RenderPkgNamespaces *renderns);


  /**
  * Creates a new ListOfGlobalStyles object from the given XMLNode object.
  * The XMLNode object has to contain a valid XML representation of a
  * ListOfGlobalStyles object as defined in the render extension specification.
  * This method is normally called when render information is read from a file and
  * should normally not have to be called explicitly.
  *
  * @param node the XMLNode object reference that describes the ListOfGlobalStyles
  * object to be instantiated.
  */
  ListOfGlobalStyles(const XMLNode& node);
  
  /**
   * Copy constructor for ListOfGlobalStyles.
   *
   * @param orig the ListOfGlobalStyles instance to copy.
   */
  ListOfGlobalStyles(const ListOfGlobalStyles& orig);


  /**
   * Assignment operator for ListOfGlobalStyles.
   *
   * @param rhs the ListOfGlobalStyles object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfGlobalStyles& operator=(const ListOfGlobalStyles& rhs);


  /**
   * Creates and returns a deep copy of this ListOfGlobalStyles object.
   *
   * @return a (deep) copy of this ListOfGlobalStyles object.
   */
  virtual ListOfGlobalStyles* clone() const;


  /**
   * Destructor for ListOfGlobalStyles.
   */
  virtual ~ListOfGlobalStyles();


  /**
   * Get a GlobalStyle from the ListOfGlobalStyles.
   *
   * @param n an unsigned int representing the index of the GlobalStyle to
   * retrieve.
   *
   * @return the nth GlobalStyle in this ListOfGlobalStyles.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see get(const std::string& sid)
   * @see getNumGlobalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GlobalStyle* get(unsigned int n);


  /**
   * Get a GlobalStyle from the ListOfGlobalStyles.
   *
   * @param n an unsigned int representing the index of the GlobalStyle to
   * retrieve.
   *
   * @return the nth GlobalStyle in this ListOfGlobalStyles.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see get(const std::string& sid)
   * @see getNumGlobalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GlobalStyle* get(unsigned int n) const;


  /**
   * Get a GlobalStyle from the ListOfGlobalStyles based on its identifier.
   *
   * @param sid a string representing the identifier of the GlobalStyle to
   * retrieve.
   *
   * @return the GlobalStyle in this ListOfGlobalStyles with the given @p sid
   * or @c NULL if no such GlobalStyle exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see get(unsigned int n)
   * @see getNumGlobalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GlobalStyle* get(const std::string& sid);


  /**
   * Get a GlobalStyle from the ListOfGlobalStyles based on its identifier.
   *
   * @param sid a string representing the identifier of the GlobalStyle to
   * retrieve.
   *
   * @return the GlobalStyle in this ListOfGlobalStyles with the given @p sid
   * or @c NULL if no such GlobalStyle exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see get(unsigned int n)
   * @see getNumGlobalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GlobalStyle* get(const std::string& sid) const;


  /**
   * Removes the nth GlobalStyle from this ListOfGlobalStyles and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the GlobalStyle to
   * remove.
   *
   * @return a pointer to the nth GlobalStyle in this ListOfGlobalStyles.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGlobalStyles()
   * @see remove(const std::string& sid)
   */
  virtual GlobalStyle* remove(unsigned int n);


  /**
   * Removes the GlobalStyle from this ListOfGlobalStyles based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the GlobalStyle to
   * remove.
   *
   * @return the GlobalStyle in this ListOfGlobalStyles based on the identifier
   * or NULL if no such GlobalStyle exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGlobalStyles()
   * @see remove(unsigned int n)
   */
  virtual GlobalStyle* remove(const std::string& sid);


  /**
   * Adds a copy of the given GlobalStyle to this ListOfGlobalStyles.
   *
   * @param gs the GlobalStyle object to add.
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
   * @see createGlobalStyle()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGlobalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addGlobalStyle(const GlobalStyle* gs);


  /**
   * Get the number of GlobalStyle objects in this ListOfGlobalStyles.
   *
   * @return the number of GlobalStyle objects in this ListOfGlobalStyles.
   *
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see createGlobalStyle()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumGlobalStyles() const;


  /**
   * Creates a new GlobalStyle object, adds it to this ListOfGlobalStyles
   * object and returns the GlobalStyle object created.
   *
   * @return a new GlobalStyle object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGlobalStyle(const GlobalStyle* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGlobalStyles()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  GlobalStyle* createGlobalStyle();


  /**
   * Returns the XML element name of this ListOfGlobalStyles object.
   *
   * For ListOfGlobalStyles, the XML element name is always
   * @c "listOfGlobalStyles".
   *
   * @return the name of this element, i.e. @c "listOfGlobalStyles".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfGlobalStyles object.
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
   * ListOfGlobalStyles object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfGlobalStyles:
   * @sbmlconstant{SBML_RENDER_GLOBALSTYLE, SBMLRenderTypeCode_t}.
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
  * Creates an XMLNode object from this ListOfGlobalStyles object.
  *
  * @return the XMLNode with the XML representation for the
  * ListOfGlobalStyles object.
  */
  XMLNode toXML() const;


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new GlobalStyle in this ListOfGlobalStyles
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
 * Get a GlobalStyle_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the GlobalStyle_t to
 * retrieve.
 *
 * @return the nth GlobalStyle_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGlobalStyles_t
 */
LIBSBML_EXTERN
GlobalStyle_t*
ListOfGlobalStyles_getGlobalStyle(ListOf_t* lo, unsigned int n);


/**
 * Get a GlobalStyle_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the GlobalStyle_t to
 * retrieve.
 *
 * @return the GlobalStyle_t in this ListOf_t with the given @p sid or @c NULL
 * if no such GlobalStyle_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGlobalStyles_t
 */
LIBSBML_EXTERN
GlobalStyle_t*
ListOfGlobalStyles_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth GlobalStyle_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the GlobalStyle_t to
 * remove.
 *
 * @return a pointer to the nth GlobalStyle_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfGlobalStyles_t
 */
LIBSBML_EXTERN
GlobalStyle_t*
ListOfGlobalStyles_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the GlobalStyle_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the GlobalStyle_t to
 * remove.
 *
 * @return the GlobalStyle_t in this ListOf_t based on the identifier or NULL
 * if no such GlobalStyle_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfGlobalStyles_t
 */
LIBSBML_EXTERN
GlobalStyle_t*
ListOfGlobalStyles_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfGlobalStyles_H__ */


