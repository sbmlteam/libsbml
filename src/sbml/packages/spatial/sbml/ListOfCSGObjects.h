/**
 * @file ListOfCSGObjects.h
 * @brief Definition of the ListOfCSGObjects class.
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
 * @class ListOfCSGObjects
 * @sbmlbrief{spatial} TODO:Definition of the ListOfCSGObjects class.
 */


#ifndef ListOfCSGObjects_H__
#define ListOfCSGObjects_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGObject.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfCSGObjects : public ListOf
{

public:

  /**
   * Creates a new ListOfCSGObjects using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfCSGObjects.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfCSGObjects.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfCSGObjects.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCSGObjects(unsigned int level = SpatialExtension::getDefaultLevel(),
                   unsigned int version =
                     SpatialExtension::getDefaultVersion(),
                   unsigned int pkgVersion =
                     SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCSGObjects using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCSGObjects(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfCSGObjects.
   *
   * @param orig the ListOfCSGObjects instance to copy.
   */
  ListOfCSGObjects(const ListOfCSGObjects& orig);


  /**
   * Assignment operator for ListOfCSGObjects.
   *
   * @param rhs the ListOfCSGObjects object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfCSGObjects& operator=(const ListOfCSGObjects& rhs);


  /**
   * Creates and returns a deep copy of this ListOfCSGObjects object.
   *
   * @return a (deep) copy of this ListOfCSGObjects object.
   */
  virtual ListOfCSGObjects* clone() const;


  /**
   * Destructor for ListOfCSGObjects.
   */
  virtual ~ListOfCSGObjects();


  /**
   * Get a CSGObject from the ListOfCSGObjects.
   *
   * @param n an unsigned int representing the index of the CSGObject to
   * retrieve.
   *
   * @return the nth CSGObject in this ListOfCSGObjects.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see get(const std::string& sid)
   * @see getNumCSGObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual CSGObject* get(unsigned int n);


  /**
   * Get a CSGObject from the ListOfCSGObjects.
   *
   * @param n an unsigned int representing the index of the CSGObject to
   * retrieve.
   *
   * @return the nth CSGObject in this ListOfCSGObjects.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see get(const std::string& sid)
   * @see getNumCSGObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const CSGObject* get(unsigned int n) const;


  /**
   * Get a CSGObject from the ListOfCSGObjects based on its identifier.
   *
   * @param sid a string representing the identifier of the CSGObject to
   * retrieve.
   *
   * @return the CSGObject in this ListOfCSGObjects with the given @p sid or
   * @c NULL if no such CSGObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see get(unsigned int n)
   * @see getNumCSGObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual CSGObject* get(const std::string& sid);


  /**
   * Get a CSGObject from the ListOfCSGObjects based on its identifier.
   *
   * @param sid a string representing the identifier of the CSGObject to
   * retrieve.
   *
   * @return the CSGObject in this ListOfCSGObjects with the given @p sid or
   * @c NULL if no such CSGObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see get(unsigned int n)
   * @see getNumCSGObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const CSGObject* get(const std::string& sid) const;


  /**
   * Removes the nth CSGObject from this ListOfCSGObjects and returns a pointer
   * to it.
   *
   * @param n an unsigned int representing the index of the CSGObject to
   * remove.
   *
   * @return a pointer to the nth CSGObject in this ListOfCSGObjects.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGObjects()
   * @see remove(const std::string& sid)
   */
  virtual CSGObject* remove(unsigned int n);


  /**
   * Removes the CSGObject from this ListOfCSGObjects based on its identifier
   * and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the CSGObject to
   * remove.
   *
   * @return the CSGObject in this ListOfCSGObjects based on the identifier or
   * NULL if no such CSGObject exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGObjects()
   * @see remove(unsigned int n)
   */
  virtual CSGObject* remove(const std::string& sid);


  /**
   * Adds a copy of the given CSGObject to this ListOfCSGObjects.
   *
   * @param csgo the CSGObject object to add.
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
   * @see createCSGObject()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addCSGObject(const CSGObject* csgo);


  /**
   * Get the number of CSGObject objects in this ListOfCSGObjects.
   *
   * @return the number of CSGObject objects in this ListOfCSGObjects.
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumCSGObjects() const;


  /**
   * Creates a new CSGObject object, adds it to this ListOfCSGObjects object
   * and returns the CSGObject object created.
   *
   * @return a new CSGObject object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumCSGObjects()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  CSGObject* createCSGObject();


  /**
   * Get a CSGObject from the ListOfCSGObjects based on the DomainType to which
   * it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * CSGObject object to retrieve.
   *
   * @return the first CSGObject in this ListOfCSGObjects based on the given
   * domainType attribute or NULL if no such CSGObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const CSGObject* getByDomainType(const std::string& sid) const;


  /**
   * Get a CSGObject from the ListOfCSGObjects based on the DomainType to which
   * it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * CSGObject object to retrieve.
   *
   * @return the first CSGObject in this ListOfCSGObjects based on the given
   * domainType attribute or NULL if no such CSGObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  CSGObject* getByDomainType(const std::string& sid);


  /**
   * Returns the XML element name of this ListOfCSGObjects object.
   *
   * For ListOfCSGObjects, the XML element name is always
   * @c "listOfCSGObjects".
   *
   * @return the name of this element, i.e. @c "listOfCSGObjects".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfCSGObjects object.
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
   * ListOfCSGObjects object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfCSGObjects:
   * @sbmlconstant{SBML_SPATIAL_CSGOBJECT, SBMLSpatialTypeCode_t}.
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
   * Creates a new CSGObject in this ListOfCSGObjects
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
 * Get a CSGObject_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the CSGObject_t to
 * retrieve.
 *
 * @return the nth CSGObject_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCSGObjects_t
 */
LIBSBML_EXTERN
CSGObject_t*
ListOfCSGObjects_getCSGObject(ListOf_t* lo, unsigned int n);


/**
 * Get a CSGObject_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the CSGObject_t to
 * retrieve.
 *
 * @return the CSGObject_t in this ListOf_t with the given @p sid or @c NULL if
 * no such CSGObject_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCSGObjects_t
 */
LIBSBML_EXTERN
CSGObject_t*
ListOfCSGObjects_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth CSGObject_t from this ListOf_t and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the CSGObject_t to
 * remove.
 *
 * @return a pointer to the nth CSGObject_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfCSGObjects_t
 */
LIBSBML_EXTERN
CSGObject_t*
ListOfCSGObjects_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the CSGObject_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the CSGObject_t to
 * remove.
 *
 * @return the CSGObject_t in this ListOf_t based on the identifier or NULL if
 * no such CSGObject_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfCSGObjects_t
 */
LIBSBML_EXTERN
CSGObject_t*
ListOfCSGObjects_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfCSGObjects_H__ */


