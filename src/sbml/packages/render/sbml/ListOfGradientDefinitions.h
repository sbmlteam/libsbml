/**
 * @file ListOfGradientDefinitions.h
 * @brief Definition of the ListOfGradientDefinitions class.
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
 * @class ListOfGradientDefinitions
 * @sbmlbrief{render} A list of GradientBase objects.
 * 
 * The ListOfGradientDefinitions is a container for the GradientBase elements 
 * of a RenderInformationBase object.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see GradientBase
 */


#ifndef ListOfGradientDefinitions_H__
#define ListOfGradientDefinitions_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/GradientBase.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LinearGradient;
class RadialGradient;

class LIBSBML_EXTERN ListOfGradientDefinitions : public ListOf
{

public:

  /**
   * Creates a new ListOfGradientDefinitions using the given SBML Level,
   * Version and &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfGradientDefinitions.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfGradientDefinitions.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ListOfGradientDefinitions.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGradientDefinitions(
                            unsigned int level =
                              RenderExtension::getDefaultLevel(),
                            unsigned int version =
                              RenderExtension::getDefaultVersion(),
                            unsigned int pkgVersion =
                              RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfGradientDefinitions using the given
   * RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfGradientDefinitions(RenderPkgNamespaces *renderns);


  /**
   * Creates a new ListOfGradientDefinitions object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * ListOfGradientDefinitions object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the ListOfGradientDefinitions
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  ListOfGradientDefinitions(const XMLNode& node, unsigned int l2version=4);


  /**
   * Copy constructor for ListOfGradientDefinitions.
   *
   * @param orig the ListOfGradientDefinitions instance to copy.
   */
  ListOfGradientDefinitions(const ListOfGradientDefinitions& orig);


  /**
   * Assignment operator for ListOfGradientDefinitions.
   *
   * @param rhs the ListOfGradientDefinitions object whose values are to be
   * used as the basis of the assignment.
   */
  ListOfGradientDefinitions& operator=(const ListOfGradientDefinitions& rhs);


  /**
   * Creates and returns a deep copy of this ListOfGradientDefinitions object.
   *
   * @return a (deep) copy of this ListOfGradientDefinitions object.
   */
  virtual ListOfGradientDefinitions* clone() const;


  /**
   * Destructor for ListOfGradientDefinitions.
   */
  virtual ~ListOfGradientDefinitions();


  /**
   * Get a GradientBase from the ListOfGradientDefinitions.
   *
   * @param n an unsigned int representing the index of the GradientBase to
   * retrieve.
   *
   * @return the nth GradientBase in this ListOfGradientDefinitions.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientBase(const GradientBase* object)
   * @see createLinearGradient() 
   * @see createRadialGradient
   * @see get(const std::string& sid)
   * @see getNumGradientBases()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GradientBase* get(unsigned int n);


  /**
   * Get a GradientBase from the ListOfGradientDefinitions.
   *
   * @param n an unsigned int representing the index of the GradientBase to
   * retrieve.
   *
   * @return the nth GradientBase in this ListOfGradientDefinitions.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientBase(const GradientBase* object)
   * @see createLinearGradient() 
   * @see createRadialGradient
   * @see get(const std::string& sid)
   * @see getNumGradientBases()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GradientBase* get(unsigned int n) const;


  /**
   * Get a GradientBase from the ListOfGradientDefinitions based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the GradientBase to
   * retrieve.
   *
   * @return the GradientBase in this ListOfGradientDefinitions with the given
   * @p sid or @c NULL if no such GradientBase exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientBase(const GradientBase* object)
   * @see createLinearGradient() 
   * @see createRadialGradient
   * @see get(unsigned int n)
   * @see getNumGradientBases()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual GradientBase* get(const std::string& sid);


  /**
   * Get a GradientBase from the ListOfGradientDefinitions based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the GradientBase to
   * retrieve.
   *
   * @return the GradientBase in this ListOfGradientDefinitions with the given
   * @p sid or @c NULL if no such GradientBase exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientBase(const GradientBase* object)
   * @see createLinearGradient() 
   * @see createRadialGradient
   * @see get(unsigned int n)
   * @see getNumGradientBases()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const GradientBase* get(const std::string& sid) const;


  /**
   * Removes the nth GradientBase from this ListOfGradientDefinitions and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the GradientBase to
   * remove.
   *
   * @return a pointer to the nth GradientBase in this
   * ListOfGradientDefinitions.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGradientBase(const GradientBase* object)
   * @see createLinearGradient() 
   * @see createRadialGradient
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGradientBases()
   * @see remove(const std::string& sid)
   */
  virtual GradientBase* remove(unsigned int n);


  /**
   * Removes the GradientBase from this ListOfGradientDefinitions based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the GradientBase to
   * remove.
   *
   * @return the GradientBase in this ListOfGradientDefinitions based on the
   * identifier or NULL if no such GradientBase exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGradientBase(const GradientBase* object)
   * @see createLinearGradient() 
   * @see createRadialGradient
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGradientBases()
   * @see remove(unsigned int n)
   */
  virtual GradientBase* remove(const std::string& sid);


  /**
   * Adds a copy of the given GradientBase to this ListOfGradientDefinitions.
   *
   * @param gb the GradientBase object to add.
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
   * @see createLinearGradient() 
   * @see createRadialGradient
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGradientBases()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addGradientBase(const GradientBase* gb);


  /**
   * Get the number of GradientBase objects in this ListOfGradientDefinitions.
   *
   * @return the number of GradientBase objects in this
   * ListOfGradientDefinitions.
   *
   *
   * @see addGradientBase(const GradientBase* object)
   * @see createLinearGradient() 
   * @see createRadialGradient
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumGradientBases() const;


  /**
   * Creates a new LinearGradient object, adds it to this
   * ListOfGradientDefinitions object and returns the LinearGradient object
   * created.
   *
   * @return a new LinearGradient object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientBase(const GradientBase* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGradientBases()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  LinearGradient* createLinearGradient();


  /**
   * Creates a new RadialGradient object, adds it to this
   * ListOfGradientDefinitions object and returns the RadialGradient object
   * created.
   *
   * @return a new RadialGradient object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGradientBase(const GradientBase* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumGradientBases()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  RadialGradient* createRadialGradient();


  /**
   * Returns the XML element name of this ListOfGradientDefinitions object.
   *
   * For ListOfGradientDefinitions, the XML element name is always
   * @c "listOfGradientDefinitions".
   *
   * @return the name of this element, i.e. @c "listOfGradientDefinitions".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfGradientDefinitions object.
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
   * ListOfGradientDefinitions object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfGradientDefinitions:
   * @sbmlconstant{SBML_RENDER_GRADIENTDEFINITION, SBMLRenderTypeCode_t}.
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
   * Creates an XMLNode object from this ListOfGradientDefinitions object.
   *
   * @return the XMLNode with the XML representation for the 
   * ListOfGradientDefinitions object.
   */
  XMLNode toXML() const;


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new GradientBase in this ListOfGradientDefinitions
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Render package
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * checks concrete types
   */
  virtual bool isValidTypeForList(SBase* item);

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Get a GradientBase_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the GradientBase_t to
 * retrieve.
 *
 * @return the nth GradientBase_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGradientDefinitions_t
 */
LIBSBML_EXTERN
GradientBase_t*
ListOfGradientDefinitions_getGradientBase(ListOf_t* lo, unsigned int n);


/**
 * Get a GradientBase_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the GradientBase_t to
 * retrieve.
 *
 * @return the GradientBase_t in this ListOf_t with the given @p sid or @c NULL
 * if no such GradientBase_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfGradientDefinitions_t
 */
LIBSBML_EXTERN
GradientBase_t*
ListOfGradientDefinitions_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth GradientBase_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the GradientBase_t to
 * remove.
 *
 * @return a pointer to the nth GradientBase_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfGradientDefinitions_t
 */
LIBSBML_EXTERN
GradientBase_t*
ListOfGradientDefinitions_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the GradientBase_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the GradientBase_t to
 * remove.
 *
 * @return the GradientBase_t in this ListOf_t based on the identifier or NULL
 * if no such GradientBase_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfGradientDefinitions_t
 */
LIBSBML_EXTERN
GradientBase_t*
ListOfGradientDefinitions_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfGradientDefinitions_H__ */


