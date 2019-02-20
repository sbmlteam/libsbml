/**
 * @file ListOfCoordinateComponents.h
 * @brief Definition of the ListOfCoordinateComponents class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
 * @class ListOfCoordinateComponents
 * @sbmlbrief{spatial} TODO:Definition of the ListOfCoordinateComponents class.
 */


#ifndef ListOfCoordinateComponents_H__
#define ListOfCoordinateComponents_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CoordinateComponent.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfCoordinateComponents : public ListOf
{

public:

  /**
   * Creates a new ListOfCoordinateComponents using the given SBML Level,
   * Version and &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfCoordinateComponents.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfCoordinateComponents.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfCoordinateComponents.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCoordinateComponents(
                             unsigned int level =
                               SpatialExtension::getDefaultLevel(),
                             unsigned int version =
                               SpatialExtension::getDefaultVersion(),
                             unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCoordinateComponents using the given
   * SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCoordinateComponents(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfCoordinateComponents.
   *
   * @param orig the ListOfCoordinateComponents instance to copy.
   */
  ListOfCoordinateComponents(const ListOfCoordinateComponents& orig);


  /**
   * Assignment operator for ListOfCoordinateComponents.
   *
   * @param rhs the ListOfCoordinateComponents object whose values are to be
   * used as the basis of the assignment.
   */
  ListOfCoordinateComponents& operator=(const ListOfCoordinateComponents& rhs);


  /**
   * Creates and returns a deep copy of this ListOfCoordinateComponents object.
   *
   * @return a (deep) copy of this ListOfCoordinateComponents object.
   */
  virtual ListOfCoordinateComponents* clone() const;


  /**
   * Destructor for ListOfCoordinateComponents.
   */
  virtual ~ListOfCoordinateComponents();


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents.
   *
   * @param n an unsigned int representing the index of the CoordinateComponent
   * to retrieve.
   *
   * @return the nth CoordinateComponent in this ListOfCoordinateComponents.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCoordinateComponent(const CoordinateComponent* object)
   * @see createCoordinateComponent()
   * @see get(const std::string& sid)
   * @see getNumCoordinateComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual CoordinateComponent* get(unsigned int n);


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents.
   *
   * @param n an unsigned int representing the index of the CoordinateComponent
   * to retrieve.
   *
   * @return the nth CoordinateComponent in this ListOfCoordinateComponents.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCoordinateComponent(const CoordinateComponent* object)
   * @see createCoordinateComponent()
   * @see get(const std::string& sid)
   * @see getNumCoordinateComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const CoordinateComponent* get(unsigned int n) const;


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the CoordinateComponent
   * to retrieve.
   *
   * @return the CoordinateComponent in this ListOfCoordinateComponents with
   * the given @p sid or @c NULL if no such CoordinateComponent exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCoordinateComponent(const CoordinateComponent* object)
   * @see createCoordinateComponent()
   * @see get(unsigned int n)
   * @see getNumCoordinateComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual CoordinateComponent* get(const std::string& sid);


  /**
   * Get a CoordinateComponent from the ListOfCoordinateComponents based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the CoordinateComponent
   * to retrieve.
   *
   * @return the CoordinateComponent in this ListOfCoordinateComponents with
   * the given @p sid or @c NULL if no such CoordinateComponent exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCoordinateComponent(const CoordinateComponent* object)
   * @see createCoordinateComponent()
   * @see get(unsigned int n)
   * @see getNumCoordinateComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const CoordinateComponent* get(const std::string& sid) const;


  /**
   * Removes the nth CoordinateComponent from this ListOfCoordinateComponents
   * and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the CoordinateComponent
   * to remove.
   *
   * @return a pointer to the nth CoordinateComponent in this
   * ListOfCoordinateComponents.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addCoordinateComponent(const CoordinateComponent* object)
   * @see createCoordinateComponent()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCoordinateComponents()
   * @see remove(const std::string& sid)
   */
  virtual CoordinateComponent* remove(unsigned int n);


  /**
   * Removes the CoordinateComponent from this ListOfCoordinateComponents based
   * on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the CoordinateComponent
   * to remove.
   *
   * @return the CoordinateComponent in this ListOfCoordinateComponents based
   * on the identifier or NULL if no such CoordinateComponent exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addCoordinateComponent(const CoordinateComponent* object)
   * @see createCoordinateComponent()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCoordinateComponents()
   * @see remove(unsigned int n)
   */
  virtual CoordinateComponent* remove(const std::string& sid);


  /**
   * Adds a copy of the given CoordinateComponent to this
   * ListOfCoordinateComponents.
   *
   * @param cc the CoordinateComponent object to add.
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
   * @see createCoordinateComponent()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCoordinateComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addCoordinateComponent(const CoordinateComponent* cc);


  /**
   * Get the number of CoordinateComponent objects in this
   * ListOfCoordinateComponents.
   *
   * @return the number of CoordinateComponent objects in this
   * ListOfCoordinateComponents.
   *
   * @see addCoordinateComponent(const CoordinateComponent* object)
   * @see createCoordinateComponent()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumCoordinateComponents() const;


  /**
   * Creates a new CoordinateComponent object, adds it to this
   * ListOfCoordinateComponents object and returns the CoordinateComponent
   * object created.
   *
   * @return a new CoordinateComponent object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCoordinateComponent(const CoordinateComponent* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCoordinateComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  CoordinateComponent* createCoordinateComponent();


  /**
   * Returns the XML element name of this ListOfCoordinateComponents object.
   *
   * For ListOfCoordinateComponents, the XML element name is always
   * @c "listOfCoordinateComponents".
   *
   * @return the name of this element, i.e. @c "listOfCoordinateComponents".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfCoordinateComponents object.
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
   * ListOfCoordinateComponents object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfCoordinateComponents:
   * @sbmlconstant{SBML_SPATIAL_COORDINATECOMPONENT, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;




  #ifndef SWIG




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new CoordinateComponent in this ListOfCoordinateComponents
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Spatial package
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
 * Get a CoordinateComponent_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the CoordinateComponent_t
 * to retrieve.
 *
 * @return the nth CoordinateComponent_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCoordinateComponents_t
 */
LIBSBML_EXTERN
CoordinateComponent_t*
ListOfCoordinateComponents_getCoordinateComponent(ListOf_t* lo,
                                                  unsigned int n);


/**
 * Get a CoordinateComponent_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the CoordinateComponent_t
 * to retrieve.
 *
 * @return the CoordinateComponent_t in this ListOf_t with the given @p sid or
 * @c NULL if no such CoordinateComponent_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCoordinateComponents_t
 */
LIBSBML_EXTERN
CoordinateComponent_t*
ListOfCoordinateComponents_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth CoordinateComponent_t from this ListOf_t and returns a
 * pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the CoordinateComponent_t
 * to remove.
 *
 * @return a pointer to the nth CoordinateComponent_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfCoordinateComponents_t
 */
LIBSBML_EXTERN
CoordinateComponent_t*
ListOfCoordinateComponents_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the CoordinateComponent_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the CoordinateComponent_t
 * to remove.
 *
 * @return the CoordinateComponent_t in this ListOf_t based on the identifier
 * or NULL if no such CoordinateComponent_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfCoordinateComponents_t
 */
LIBSBML_EXTERN
CoordinateComponent_t*
ListOfCoordinateComponents_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfCoordinateComponents_H__ */


