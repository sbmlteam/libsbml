/**
 * @file ListOfColorDefinitions.h
 * @brief Definition of the ListOfColorDefinitions class.
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
 * Copyright (C) 2013-2017 jointly by the following organizations:
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
 * @class ListOfColorDefinitions
 * @sbmlbrief{render} A list of ColorDefinition objects.
 * 
 * The ListOfColorDefinitions is a container for the ColorDefinition elements 
 * of a RenderInformationBase object.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see ColorDefinition
 */


#ifndef ListOfColorDefinitions_H__
#define ListOfColorDefinitions_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/ColorDefinition.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfColorDefinitions : public ListOf
{

public:
  /**
   * Creates a new ListOfColorDefinitions object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * ListOfColorDefinitions object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the ListOfColorDefinitions
   * object to be instantiated.
   */
  ListOfColorDefinitions(const XMLNode& node);  


  /**
   * Creates a new ListOfColorDefinitions using the given SBML Level, Version
   * and &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfColorDefinitions.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfColorDefinitions.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ListOfColorDefinitions.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfColorDefinitions(unsigned int level =
    RenderExtension::getDefaultLevel(),
                         unsigned int version =
                           RenderExtension::getDefaultVersion(),
                         unsigned int pkgVersion =
                           RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfColorDefinitions using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfColorDefinitions(RenderPkgNamespaces *renderns);


  /**
   * Copy constructor for ListOfColorDefinitions.
   *
   * @param orig the ListOfColorDefinitions instance to copy.
   */
  ListOfColorDefinitions(const ListOfColorDefinitions& orig);


  /**
   * Assignment operator for ListOfColorDefinitions.
   *
   * @param rhs the ListOfColorDefinitions object whose values are to be used
   * as the basis of the assignment.
   */
  ListOfColorDefinitions& operator=(const ListOfColorDefinitions& rhs);


  /**
   * Creates and returns a deep copy of this ListOfColorDefinitions object.
   *
   * @return a (deep) copy of this ListOfColorDefinitions object.
   */
  virtual ListOfColorDefinitions* clone() const;


  /**
   * Destructor for ListOfColorDefinitions.
   */
  virtual ~ListOfColorDefinitions();


  /**
   * Get a ColorDefinition from the ListOfColorDefinitions.
   *
   * @param n an unsigned int representing the index of the ColorDefinition to
   * retrieve.
   *
   * @return the nth ColorDefinition in this ListOfColorDefinitions.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see get(const std::string& sid)
   * @see getNumColorDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual ColorDefinition* get(unsigned int n);


  /**
   * Get a ColorDefinition from the ListOfColorDefinitions.
   *
   * @param n an unsigned int representing the index of the ColorDefinition to
   * retrieve.
   *
   * @return the nth ColorDefinition in this ListOfColorDefinitions.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see get(const std::string& sid)
   * @see getNumColorDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const ColorDefinition* get(unsigned int n) const;


  /**
   * Get a ColorDefinition from the ListOfColorDefinitions based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ColorDefinition to
   * retrieve.
   *
   * @return the ColorDefinition in this ListOfColorDefinitions with the given
   * @p sid or @c NULL if no such ColorDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see get(unsigned int n)
   * @see getNumColorDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual ColorDefinition* get(const std::string& sid);


  /**
   * Get a ColorDefinition from the ListOfColorDefinitions based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ColorDefinition to
   * retrieve.
   *
   * @return the ColorDefinition in this ListOfColorDefinitions with the given
   * @p sid or @c NULL if no such ColorDefinition exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see get(unsigned int n)
   * @see getNumColorDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const ColorDefinition* get(const std::string& sid) const;


  /**
   * Removes the nth ColorDefinition from this ListOfColorDefinitions and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the ColorDefinition to
   * remove.
   *
   * @return a pointer to the nth ColorDefinition in this
   * ListOfColorDefinitions.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumColorDefinitions()
   * @see remove(const std::string& sid)
   */
  virtual ColorDefinition* remove(unsigned int n);


  /**
   * Removes the ColorDefinition from this ListOfColorDefinitions based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the ColorDefinition to
   * remove.
   *
   * @return the ColorDefinition in this ListOfColorDefinitions based on the
   * identifier or NULL if no such ColorDefinition exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumColorDefinitions()
   * @see remove(unsigned int n)
   */
  virtual ColorDefinition* remove(const std::string& sid);


  /**
   * Adds a copy of the given ColorDefinition to this ListOfColorDefinitions.
   *
   * @param cd the ColorDefinition object to add.
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
   * @see createColorDefinition()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumColorDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addColorDefinition(const ColorDefinition* cd);


  /**
   * Get the number of ColorDefinition objects in this ListOfColorDefinitions.
   *
   * @return the number of ColorDefinition objects in this
   * ListOfColorDefinitions.
   *
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see createColorDefinition()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumColorDefinitions() const;


  /**
   * Creates a new ColorDefinition object, adds it to this
   * ListOfColorDefinitions object and returns the ColorDefinition object
   * created.
   *
   * @return a new ColorDefinition object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addColorDefinition(const ColorDefinition* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumColorDefinitions()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  ColorDefinition* createColorDefinition();


  /**
   * Returns the XML element name of this ListOfColorDefinitions object.
   *
   * For ListOfColorDefinitions, the XML element name is always
   * @c "listOfColorDefinitions".
   *
   * @return the name of this element, i.e. @c "listOfColorDefinitions".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfColorDefinitions object.
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
   * ListOfColorDefinitions object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfColorDefinitions:
   * @sbmlconstant{SBML_RENDER_COLORDEFINITION, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;



  /**
   * Creates an XMLNode object from this ListOfColorDefinitions object.
   *
   * @return the XMLNode with the XML representation for the 
   * ListOfColorDefinitions object.
   */
  XMLNode toXML() const;


  #ifndef SWIG




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new ColorDefinition in this ListOfColorDefinitions
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
 * Get a ColorDefinition_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the ColorDefinition_t to
 * retrieve.
 *
 * @return the nth ColorDefinition_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfColorDefinitions_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
ListOfColorDefinitions_getColorDefinition(ListOf_t* lo, unsigned int n);


/**
 * Get a ColorDefinition_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the ColorDefinition_t to
 * retrieve.
 *
 * @return the ColorDefinition_t in this ListOf_t with the given @p sid or
 * @c NULL if no such ColorDefinition_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfColorDefinitions_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
ListOfColorDefinitions_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth ColorDefinition_t from this ListOf_t and returns a pointer
 * to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the ColorDefinition_t to
 * remove.
 *
 * @return a pointer to the nth ColorDefinition_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfColorDefinitions_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
ListOfColorDefinitions_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the ColorDefinition_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the ColorDefinition_t to
 * remove.
 *
 * @return the ColorDefinition_t in this ListOf_t based on the identifier or
 * NULL if no such ColorDefinition_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfColorDefinitions_t
 */
LIBSBML_EXTERN
ColorDefinition_t*
ListOfColorDefinitions_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfColorDefinitions_H__ */


