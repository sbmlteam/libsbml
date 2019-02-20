/**
 * @file ListOfParametricObjects.h
 * @brief Definition of the ListOfParametricObjects class.
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
 * @class ListOfParametricObjects
 * @sbmlbrief{spatial} TODO:Definition of the ListOfParametricObjects class.
 */


#ifndef ListOfParametricObjects_H__
#define ListOfParametricObjects_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/ParametricObject.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfParametricObjects : public ListOf
{

public:

  /**
   * Creates a new ListOfParametricObjects using the given SBML Level, Version
   * and &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfParametricObjects.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfParametricObjects.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfParametricObjects.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfParametricObjects(
                          unsigned int level =
                            SpatialExtension::getDefaultLevel(),
                          unsigned int version =
                            SpatialExtension::getDefaultVersion(),
                          unsigned int pkgVersion =
                            SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfParametricObjects using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfParametricObjects(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfParametricObjects.
   *
   * @param orig the ListOfParametricObjects instance to copy.
   */
  ListOfParametricObjects(const ListOfParametricObjects& orig);


  /**
   * Assignment operator for ListOfParametricObjects.
   *
   * @param rhs the ListOfParametricObjects object whose values are to be used
   * as the basis of the assignment.
   */
  ListOfParametricObjects& operator=(const ListOfParametricObjects& rhs);


  /**
   * Creates and returns a deep copy of this ListOfParametricObjects object.
   *
   * @return a (deep) copy of this ListOfParametricObjects object.
   */
  virtual ListOfParametricObjects* clone() const;


  /**
   * Destructor for ListOfParametricObjects.
   */
  virtual ~ListOfParametricObjects();


  /**
   * Get a ParametricObject from the ListOfParametricObjects.
   *
   * @param n an unsigned int representing the index of the ParametricObject to
   * retrieve.
   *
   * @return the nth ParametricObject in this ListOfParametricObjects.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see get(const std::string& sid)
   * @see getNumParametricObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual ParametricObject* get(unsigned int n);


  /**
   * Get a ParametricObject from the ListOfParametricObjects.
   *
   * @param n an unsigned int representing the index of the ParametricObject to
   * retrieve.
   *
   * @return the nth ParametricObject in this ListOfParametricObjects.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see get(const std::string& sid)
   * @see getNumParametricObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const ParametricObject* get(unsigned int n) const;


  /**
   * Get a ParametricObject from the ListOfParametricObjects based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ParametricObject to
   * retrieve.
   *
   * @return the ParametricObject in this ListOfParametricObjects with the
   * given @p sid or @c NULL if no such ParametricObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see get(unsigned int n)
   * @see getNumParametricObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual ParametricObject* get(const std::string& sid);


  /**
   * Get a ParametricObject from the ListOfParametricObjects based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ParametricObject to
   * retrieve.
   *
   * @return the ParametricObject in this ListOfParametricObjects with the
   * given @p sid or @c NULL if no such ParametricObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see get(unsigned int n)
   * @see getNumParametricObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const ParametricObject* get(const std::string& sid) const;


  /**
   * Removes the nth ParametricObject from this ListOfParametricObjects and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the ParametricObject to
   * remove.
   *
   * @return a pointer to the nth ParametricObject in this
   * ListOfParametricObjects.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumParametricObjects()
   * @see remove(const std::string& sid)
   */
  virtual ParametricObject* remove(unsigned int n);


  /**
   * Removes the ParametricObject from this ListOfParametricObjects based on
   * its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the ParametricObject to
   * remove.
   *
   * @return the ParametricObject in this ListOfParametricObjects based on the
   * identifier or NULL if no such ParametricObject exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumParametricObjects()
   * @see remove(unsigned int n)
   */
  virtual ParametricObject* remove(const std::string& sid);


  /**
   * Adds a copy of the given ParametricObject to this ListOfParametricObjects.
   *
   * @param po the ParametricObject object to add.
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
   * @see createParametricObject()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumParametricObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addParametricObject(const ParametricObject* po);


  /**
   * Get the number of ParametricObject objects in this
   * ListOfParametricObjects.
   *
   * @return the number of ParametricObject objects in this
   * ListOfParametricObjects.
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumParametricObjects() const;


  /**
   * Creates a new ParametricObject object, adds it to this
   * ListOfParametricObjects object and returns the ParametricObject object
   * created.
   *
   * @return a new ParametricObject object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumParametricObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  ParametricObject* createParametricObject();


  /**
   * Get a ParametricObject from the ListOfParametricObjects based on the
   * DomainType to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * ParametricObject object to retrieve.
   *
   * @return the first ParametricObject in this ListOfParametricObjects based
   * on the given domainType attribute or NULL if no such ParametricObject
   * exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const ParametricObject* getByDomainType(const std::string& sid) const;


  /**
   * Get a ParametricObject from the ListOfParametricObjects based on the
   * DomainType to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * ParametricObject object to retrieve.
   *
   * @return the first ParametricObject in this ListOfParametricObjects based
   * on the given domainType attribute or NULL if no such ParametricObject
   * exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  ParametricObject* getByDomainType(const std::string& sid);


  /**
   * Returns the XML element name of this ListOfParametricObjects object.
   *
   * For ListOfParametricObjects, the XML element name is always
   * @c "listOfParametricObjects".
   *
   * @return the name of this element, i.e. @c "listOfParametricObjects".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfParametricObjects object.
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
   * ListOfParametricObjects object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfParametricObjects:
   * @sbmlconstant{SBML_SPATIAL_PARAMETRICOBJECT, SBMLSpatialTypeCode_t}.
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
   * Creates a new ParametricObject in this ListOfParametricObjects
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
 * Get a ParametricObject_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the ParametricObject_t to
 * retrieve.
 *
 * @return the nth ParametricObject_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfParametricObjects_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ListOfParametricObjects_getParametricObject(ListOf_t* lo, unsigned int n);


/**
 * Get a ParametricObject_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the ParametricObject_t to
 * retrieve.
 *
 * @return the ParametricObject_t in this ListOf_t with the given @p sid or
 * @c NULL if no such ParametricObject_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfParametricObjects_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ListOfParametricObjects_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth ParametricObject_t from this ListOf_t and returns a pointer
 * to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the ParametricObject_t to
 * remove.
 *
 * @return a pointer to the nth ParametricObject_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfParametricObjects_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ListOfParametricObjects_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the ParametricObject_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the ParametricObject_t to
 * remove.
 *
 * @return the ParametricObject_t in this ListOf_t based on the identifier or
 * NULL if no such ParametricObject_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfParametricObjects_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ListOfParametricObjects_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfParametricObjects_H__ */


