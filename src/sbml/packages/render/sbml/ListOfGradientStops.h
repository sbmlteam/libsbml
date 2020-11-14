/**
 * @file ListOfGradientStops.h
 * @brief Definition of the ListOfGradientStops class.
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
 * @class ListOfGradientStops
 * @sbmlbrief{render} A list of one or more GradientStop objects.
 *
 * The ListOfGradientStops is used in linear and radial gradient objects to
 * store the GradientStop objects that define the gradient. A valid gradient
 * should have two or more gradient stops.
 *
 * The ListOfGradientStops is a container for the GradientStop elements of a
 * GradientBase object.
 *
 * Note that the ListOfGradientStops class is only defined in libsbml for
 * convenience: GradientStop objects are actually direct children of
 * GradientBase-derived objects (i.e., LinearGradient or RadialGradient
 * objects).
 *
 * @copydetails doc_what_is_listof
 *
 * @htmlinclude not-sbml-warning.html
 *
 * @see GradientStop
 * @see GradientBase
 * @see LinearGradient
 * @see RadialGradient
 */


#ifndef ListOfGradientStops_H__
#define ListOfGradientStops_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/GradientStop.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfGradientStops : public ListOf
{

public:

  /**
   * Creates a new ListOfGradientStops using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfGradientStops.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfGradientStops.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ListOfGradientStops.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGradientStops(unsigned int level = RenderExtension::getDefaultLevel(),
                      unsigned int version =
                        RenderExtension::getDefaultVersion(),
                      unsigned int pkgVersion =
                        RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfGradientStops using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGradientStops(RenderPkgNamespaces *renderns);


  /**
   * Creates a new ListOfGradientStops object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * ListOfGradientStops object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the ListOfGradientStops
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  ListOfGradientStops(const XMLNode& node, unsigned int l2version=4);

 
  /**
   * Copy constructor for ListOfGradientStops.
   *
   * @param orig the ListOfGradientStops instance to copy.
   */
  ListOfGradientStops(const ListOfGradientStops& orig);


  /**
   * Assignment operator for ListOfGradientStops.
   *
   * @param rhs the ListOfGradientStops object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfGradientStops& operator=(const ListOfGradientStops& rhs);


  /**
   * Creates and returns a deep copy of this ListOfGradientStops object.
   *
   * @return a (deep) copy of this ListOfGradientStops object.
   */
  virtual ListOfGradientStops* clone() const;


  /**
   * Destructor for ListOfGradientStops.
   */
  virtual ~ListOfGradientStops();


  /**
   * Get a GradientStop from the ListOfGradientStops.
   *
   * @param n an unsigned int representing the index of the GradientStop to
   * retrieve.
   *
   * @return the nth GradientStop in this ListOfGradientStops.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see get(const std::string& sid)
   * @see getNumGradientStops()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GradientStop* get(unsigned int n);


  /**
   * Get a GradientStop from the ListOfGradientStops.
   *
   * @param n an unsigned int representing the index of the GradientStop to
   * retrieve.
   *
   * @return the nth GradientStop in this ListOfGradientStops.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see get(const std::string& sid)
   * @see getNumGradientStops()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GradientStop* get(unsigned int n) const;


  /**
   * Get a GradientStop from the ListOfGradientStops based on its identifier.
   *
   * @param sid a string representing the identifier of the GradientStop to
   * retrieve.
   *
   * @return the GradientStop in this ListOfGradientStops with the given @p sid
   * or @c NULL if no such GradientStop exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see get(unsigned int n)
   * @see getNumGradientStops()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GradientStop* get(const std::string& sid);


  /**
   * Get a GradientStop from the ListOfGradientStops based on its identifier.
   *
   * @param sid a string representing the identifier of the GradientStop to
   * retrieve.
   *
   * @return the GradientStop in this ListOfGradientStops with the given @p sid
   * or @c NULL if no such GradientStop exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see get(unsigned int n)
   * @see getNumGradientStops()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GradientStop* get(const std::string& sid) const;


  /**
   * Removes the nth GradientStop from this ListOfGradientStops and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the GradientStop to
   * remove.
   *
   * @return a pointer to the nth GradientStop in this ListOfGradientStops.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGradientStops()
   * @see remove(const std::string& sid)
   */
  virtual GradientStop* remove(unsigned int n);


  /**
   * Removes the GradientStop from this ListOfGradientStops based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the GradientStop to
   * remove.
   *
   * @return the GradientStop in this ListOfGradientStops based on the
   * identifier or NULL if no such GradientStop exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGradientStops()
   * @see remove(unsigned int n)
   */
  virtual GradientStop* remove(const std::string& sid);


  /**
   * Adds a copy of the given GradientStop to this ListOfGradientStops.
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
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGradientStops()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addGradientStop(const GradientStop* gs);


  /**
   * Get the number of GradientStop objects in this ListOfGradientStops.
   *
   * @return the number of GradientStop objects in this ListOfGradientStops.
   *
   *
   * @see addGradientStop(const GradientStop* object)
   * @see createGradientStop()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumGradientStops() const;


  /**
   * Creates a new GradientStop object, adds it to this ListOfGradientStops
   * object and returns the GradientStop object created.
   *
   * @return a new GradientStop object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientStop(const GradientStop* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGradientStops()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  GradientStop* createGradientStop();


  /**
   * Returns the XML element name of this ListOfGradientStops object.
   *
   * For ListOfGradientStops, the XML element name is always
   * @c "listOfGradientStops".
   *
   * @return the name of this element, i.e. @c "listOfGradientStops".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfGradientStops object.
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
   * ListOfGradientStops object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfGradientStops:
   * @sbmlconstant{SBML_RENDER_GRADIENT_STOP, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;



  /**
   * Creates an XMLNode object from this ListOfGradientStops object.
   *
   * @return the XMLNode with the XML representation for the 
   * ListOfGradientStops object.
   */
  XMLNode toXML() const;


  #ifndef SWIG




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new GradientStop in this ListOfGradientStops
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */


  friend class GradientBase;

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
 * Get a GradientStop_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the GradientStop_t to
 * retrieve.
 *
 * @return the nth GradientStop_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGradientStops_t
 */
LIBSBML_EXTERN
GradientStop_t*
ListOfGradientStops_getGradientStop(ListOf_t* lo, unsigned int n);


/**
 * Get a GradientStop_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the GradientStop_t to
 * retrieve.
 *
 * @return the GradientStop_t in this ListOf_t with the given @p sid or @c NULL
 * if no such GradientStop_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGradientStops_t
 */
LIBSBML_EXTERN
GradientStop_t*
ListOfGradientStops_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth GradientStop_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the GradientStop_t to
 * remove.
 *
 * @return a pointer to the nth GradientStop_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfGradientStops_t
 */
LIBSBML_EXTERN
GradientStop_t*
ListOfGradientStops_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the GradientStop_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the GradientStop_t to
 * remove.
 *
 * @return the GradientStop_t in this ListOf_t based on the identifier or NULL
 * if no such GradientStop_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfGradientStops_t
 */
LIBSBML_EXTERN
GradientStop_t*
ListOfGradientStops_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfGradientStops_H__ */


