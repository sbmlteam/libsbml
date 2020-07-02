/**
 * @file ListOfAnalyticVolumes.h
 * @brief Definition of the ListOfAnalyticVolumes class.
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
 * @class ListOfAnalyticVolumes
 * @sbmlbrief{spatial} TODO:Definition of the ListOfAnalyticVolumes class.
 */


#ifndef ListOfAnalyticVolumes_H__
#define ListOfAnalyticVolumes_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/AnalyticVolume.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfAnalyticVolumes : public ListOf
{

public:

  /**
   * Creates a new ListOfAnalyticVolumes using the given SBML Level, Version
   * and &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfAnalyticVolumes.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfAnalyticVolumes.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfAnalyticVolumes.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfAnalyticVolumes(unsigned int level =
    SpatialExtension::getDefaultLevel(),
                        unsigned int version =
                          SpatialExtension::getDefaultVersion(),
                        unsigned int pkgVersion =
                          SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfAnalyticVolumes using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfAnalyticVolumes(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfAnalyticVolumes.
   *
   * @param orig the ListOfAnalyticVolumes instance to copy.
   */
  ListOfAnalyticVolumes(const ListOfAnalyticVolumes& orig);


  /**
   * Assignment operator for ListOfAnalyticVolumes.
   *
   * @param rhs the ListOfAnalyticVolumes object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfAnalyticVolumes& operator=(const ListOfAnalyticVolumes& rhs);


  /**
   * Creates and returns a deep copy of this ListOfAnalyticVolumes object.
   *
   * @return a (deep) copy of this ListOfAnalyticVolumes object.
   */
  virtual ListOfAnalyticVolumes* clone() const;


  /**
   * Destructor for ListOfAnalyticVolumes.
   */
  virtual ~ListOfAnalyticVolumes();


  /**
   * Get an AnalyticVolume from the ListOfAnalyticVolumes.
   *
   * @param n an unsigned int representing the index of the AnalyticVolume to
   * retrieve.
   *
   * @return the nth AnalyticVolume in this ListOfAnalyticVolumes.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAnalyticVolume(const AnalyticVolume* object)
   * @see createAnalyticVolume()
   * @see get(const std::string& sid)
   * @see getNumAnalyticVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual AnalyticVolume* get(unsigned int n);


  /**
   * Get an AnalyticVolume from the ListOfAnalyticVolumes.
   *
   * @param n an unsigned int representing the index of the AnalyticVolume to
   * retrieve.
   *
   * @return the nth AnalyticVolume in this ListOfAnalyticVolumes.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAnalyticVolume(const AnalyticVolume* object)
   * @see createAnalyticVolume()
   * @see get(const std::string& sid)
   * @see getNumAnalyticVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const AnalyticVolume* get(unsigned int n) const;


  /**
   * Get an AnalyticVolume from the ListOfAnalyticVolumes based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the AnalyticVolume to
   * retrieve.
   *
   * @return the AnalyticVolume in this ListOfAnalyticVolumes with the given @p
   * sid or @c NULL if no such AnalyticVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAnalyticVolume(const AnalyticVolume* object)
   * @see createAnalyticVolume()
   * @see get(unsigned int n)
   * @see getNumAnalyticVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual AnalyticVolume* get(const std::string& sid);


  /**
   * Get an AnalyticVolume from the ListOfAnalyticVolumes based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the AnalyticVolume to
   * retrieve.
   *
   * @return the AnalyticVolume in this ListOfAnalyticVolumes with the given @p
   * sid or @c NULL if no such AnalyticVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAnalyticVolume(const AnalyticVolume* object)
   * @see createAnalyticVolume()
   * @see get(unsigned int n)
   * @see getNumAnalyticVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const AnalyticVolume* get(const std::string& sid) const;


  /**
   * Removes the nth AnalyticVolume from this ListOfAnalyticVolumes and returns
   * a pointer to it.
   *
   * @param n an unsigned int representing the index of the AnalyticVolume to
   * remove.
   *
   * @return a pointer to the nth AnalyticVolume in this ListOfAnalyticVolumes.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addAnalyticVolume(const AnalyticVolume* object)
   * @see createAnalyticVolume()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumAnalyticVolumes()
   * @see remove(const std::string& sid)
   */
  virtual AnalyticVolume* remove(unsigned int n);


  /**
   * Removes the AnalyticVolume from this ListOfAnalyticVolumes based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the AnalyticVolume to
   * remove.
   *
   * @return the AnalyticVolume in this ListOfAnalyticVolumes based on the
   * identifier or NULL if no such AnalyticVolume exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addAnalyticVolume(const AnalyticVolume* object)
   * @see createAnalyticVolume()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumAnalyticVolumes()
   * @see remove(unsigned int n)
   */
  virtual AnalyticVolume* remove(const std::string& sid);


  /**
   * Adds a copy of the given AnalyticVolume to this ListOfAnalyticVolumes.
   *
   * @param av the AnalyticVolume object to add.
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
   * @see createAnalyticVolume()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumAnalyticVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addAnalyticVolume(const AnalyticVolume* av);


  /**
   * Get the number of AnalyticVolume objects in this ListOfAnalyticVolumes.
   *
   * @return the number of AnalyticVolume objects in this
   * ListOfAnalyticVolumes.
   *
   * @see addAnalyticVolume(const AnalyticVolume* object)
   * @see createAnalyticVolume()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumAnalyticVolumes() const;


  /**
   * Creates a new AnalyticVolume object, adds it to this ListOfAnalyticVolumes
   * object and returns the AnalyticVolume object created.
   *
   * @return a new AnalyticVolume object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAnalyticVolume(const AnalyticVolume* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumAnalyticVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  AnalyticVolume* createAnalyticVolume();


  /**
   * Get an AnalyticVolume from the ListOfAnalyticVolumes based on the
   * DomainType to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * AnalyticVolume object to retrieve.
   *
   * @return the first AnalyticVolume in this ListOfAnalyticVolumes based on
   * the given domainType attribute or NULL if no such AnalyticVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const AnalyticVolume* getByDomainType(const std::string& sid) const;


  /**
   * Get an AnalyticVolume from the ListOfAnalyticVolumes based on the
   * DomainType to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * AnalyticVolume object to retrieve.
   *
   * @return the first AnalyticVolume in this ListOfAnalyticVolumes based on
   * the given domainType attribute or NULL if no such AnalyticVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  AnalyticVolume* getByDomainType(const std::string& sid);


  /**
   * Returns the XML element name of this ListOfAnalyticVolumes object.
   *
   * For ListOfAnalyticVolumes, the XML element name is always
   * @c "listOfAnalyticVolumes".
   *
   * @return the name of this element, i.e. @c "listOfAnalyticVolumes".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfAnalyticVolumes object.
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
   * ListOfAnalyticVolumes object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfAnalyticVolumes:
   * @sbmlconstant{SBML_SPATIAL_ANALYTICVOLUME, SBMLSpatialTypeCode_t}.
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
   * Creates a new AnalyticVolume in this ListOfAnalyticVolumes
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
 * Get an AnalyticVolume_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the AnalyticVolume_t to
 * retrieve.
 *
 * @return the nth AnalyticVolume_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfAnalyticVolumes_t
 */
LIBSBML_EXTERN
AnalyticVolume_t*
ListOfAnalyticVolumes_getAnalyticVolume(ListOf_t* lo, unsigned int n);


/**
 * Get an AnalyticVolume_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the AnalyticVolume_t to
 * retrieve.
 *
 * @return the AnalyticVolume_t in this ListOf_t with the given @p sid or
 * @c NULL if no such AnalyticVolume_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfAnalyticVolumes_t
 */
LIBSBML_EXTERN
AnalyticVolume_t*
ListOfAnalyticVolumes_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth AnalyticVolume_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the AnalyticVolume_t to
 * remove.
 *
 * @return a pointer to the nth AnalyticVolume_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfAnalyticVolumes_t
 */
LIBSBML_EXTERN
AnalyticVolume_t*
ListOfAnalyticVolumes_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the AnalyticVolume_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the AnalyticVolume_t to
 * remove.
 *
 * @return the AnalyticVolume_t in this ListOf_t based on the identifier or
 * NULL if no such AnalyticVolume_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfAnalyticVolumes_t
 */
LIBSBML_EXTERN
AnalyticVolume_t*
ListOfAnalyticVolumes_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfAnalyticVolumes_H__ */


