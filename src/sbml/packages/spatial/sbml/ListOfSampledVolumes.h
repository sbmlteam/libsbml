/**
 * @file ListOfSampledVolumes.h
 * @brief Definition of the ListOfSampledVolumes class.
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
 * @class ListOfSampledVolumes
 * @sbmlbrief{spatial} TODO:Definition of the ListOfSampledVolumes class.
 */


#ifndef ListOfSampledVolumes_H__
#define ListOfSampledVolumes_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/SampledVolume.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfSampledVolumes : public ListOf
{

public:

  /**
   * Creates a new ListOfSampledVolumes using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfSampledVolumes.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfSampledVolumes.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfSampledVolumes.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSampledVolumes(unsigned int level =
    SpatialExtension::getDefaultLevel(),
                       unsigned int version =
                         SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion =
                         SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSampledVolumes using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfSampledVolumes(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfSampledVolumes.
   *
   * @param orig the ListOfSampledVolumes instance to copy.
   */
  ListOfSampledVolumes(const ListOfSampledVolumes& orig);


  /**
   * Assignment operator for ListOfSampledVolumes.
   *
   * @param rhs the ListOfSampledVolumes object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfSampledVolumes& operator=(const ListOfSampledVolumes& rhs);


  /**
   * Creates and returns a deep copy of this ListOfSampledVolumes object.
   *
   * @return a (deep) copy of this ListOfSampledVolumes object.
   */
  virtual ListOfSampledVolumes* clone() const;


  /**
   * Destructor for ListOfSampledVolumes.
   */
  virtual ~ListOfSampledVolumes();


  /**
   * Get a SampledVolume from the ListOfSampledVolumes.
   *
   * @param n an unsigned int representing the index of the SampledVolume to
   * retrieve.
   *
   * @return the nth SampledVolume in this ListOfSampledVolumes.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see get(const std::string& sid)
   * @see getNumSampledVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual SampledVolume* get(unsigned int n);


  /**
   * Get a SampledVolume from the ListOfSampledVolumes.
   *
   * @param n an unsigned int representing the index of the SampledVolume to
   * retrieve.
   *
   * @return the nth SampledVolume in this ListOfSampledVolumes.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see get(const std::string& sid)
   * @see getNumSampledVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const SampledVolume* get(unsigned int n) const;


  /**
   * Get a SampledVolume from the ListOfSampledVolumes based on its identifier.
   *
   * @param sid a string representing the identifier of the SampledVolume to
   * retrieve.
   *
   * @return the SampledVolume in this ListOfSampledVolumes with the given @p
   * sid or @c NULL if no such SampledVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see get(unsigned int n)
   * @see getNumSampledVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual SampledVolume* get(const std::string& sid);


  /**
   * Get a SampledVolume from the ListOfSampledVolumes based on its identifier.
   *
   * @param sid a string representing the identifier of the SampledVolume to
   * retrieve.
   *
   * @return the SampledVolume in this ListOfSampledVolumes with the given @p
   * sid or @c NULL if no such SampledVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see get(unsigned int n)
   * @see getNumSampledVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const SampledVolume* get(const std::string& sid) const;


  /**
   * Removes the nth SampledVolume from this ListOfSampledVolumes and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the SampledVolume to
   * remove.
   *
   * @return a pointer to the nth SampledVolume in this ListOfSampledVolumes.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumSampledVolumes()
   * @see remove(const std::string& sid)
   */
  virtual SampledVolume* remove(unsigned int n);


  /**
   * Removes the SampledVolume from this ListOfSampledVolumes based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the SampledVolume to
   * remove.
   *
   * @return the SampledVolume in this ListOfSampledVolumes based on the
   * identifier or NULL if no such SampledVolume exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumSampledVolumes()
   * @see remove(unsigned int n)
   */
  virtual SampledVolume* remove(const std::string& sid);


  /**
   * Adds a copy of the given SampledVolume to this ListOfSampledVolumes.
   *
   * @param sv the SampledVolume object to add.
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
   * @see createSampledVolume()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumSampledVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addSampledVolume(const SampledVolume* sv);


  /**
   * Get the number of SampledVolume objects in this ListOfSampledVolumes.
   *
   * @return the number of SampledVolume objects in this ListOfSampledVolumes.
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see createSampledVolume()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumSampledVolumes() const;


  /**
   * Creates a new SampledVolume object, adds it to this ListOfSampledVolumes
   * object and returns the SampledVolume object created.
   *
   * @return a new SampledVolume object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addSampledVolume(const SampledVolume* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumSampledVolumes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  SampledVolume* createSampledVolume();


  /**
   * Get a SampledVolume from the ListOfSampledVolumes based on the DomainType
   * to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * SampledVolume object to retrieve.
   *
   * @return the first SampledVolume in this ListOfSampledVolumes based on the
   * given domainType attribute or NULL if no such SampledVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const SampledVolume* getByDomainType(const std::string& sid) const;


  /**
   * Get a SampledVolume from the ListOfSampledVolumes based on the DomainType
   * to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * SampledVolume object to retrieve.
   *
   * @return the first SampledVolume in this ListOfSampledVolumes based on the
   * given domainType attribute or NULL if no such SampledVolume exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  SampledVolume* getByDomainType(const std::string& sid);


  /**
   * Returns the XML element name of this ListOfSampledVolumes object.
   *
   * For ListOfSampledVolumes, the XML element name is always
   * @c "listOfSampledVolumes".
   *
   * @return the name of this element, i.e. @c "listOfSampledVolumes".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfSampledVolumes object.
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
   * ListOfSampledVolumes object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfSampledVolumes:
   * @sbmlconstant{SBML_SPATIAL_SAMPLEDVOLUME, SBMLSpatialTypeCode_t}.
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
   * Creates a new SampledVolume in this ListOfSampledVolumes
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
 * Get a SampledVolume_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the SampledVolume_t to
 * retrieve.
 *
 * @return the nth SampledVolume_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfSampledVolumes_t
 */
LIBSBML_EXTERN
SampledVolume_t*
ListOfSampledVolumes_getSampledVolume(ListOf_t* lo, unsigned int n);


/**
 * Get a SampledVolume_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the SampledVolume_t to
 * retrieve.
 *
 * @return the SampledVolume_t in this ListOf_t with the given @p sid or
 * @c NULL if no such SampledVolume_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfSampledVolumes_t
 */
LIBSBML_EXTERN
SampledVolume_t*
ListOfSampledVolumes_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth SampledVolume_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the SampledVolume_t to
 * remove.
 *
 * @return a pointer to the nth SampledVolume_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfSampledVolumes_t
 */
LIBSBML_EXTERN
SampledVolume_t*
ListOfSampledVolumes_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the SampledVolume_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the SampledVolume_t to
 * remove.
 *
 * @return the SampledVolume_t in this ListOf_t based on the identifier or NULL
 * if no such SampledVolume_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfSampledVolumes_t
 */
LIBSBML_EXTERN
SampledVolume_t*
ListOfSampledVolumes_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfSampledVolumes_H__ */


