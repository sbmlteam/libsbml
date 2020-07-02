/**
 * @file ListOfAdjacentDomains.h
 * @brief Definition of the ListOfAdjacentDomains class.
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
 * @class ListOfAdjacentDomains
 * @sbmlbrief{spatial} TODO:Definition of the ListOfAdjacentDomains class.
 */


#ifndef ListOfAdjacentDomains_H__
#define ListOfAdjacentDomains_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/AdjacentDomains.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfAdjacentDomains : public ListOf
{

public:

  /**
   * Creates a new ListOfAdjacentDomains using the given SBML Level, Version
   * and &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfAdjacentDomains.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfAdjacentDomains.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfAdjacentDomains.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfAdjacentDomains(unsigned int level =
    SpatialExtension::getDefaultLevel(),
                        unsigned int version =
                          SpatialExtension::getDefaultVersion(),
                        unsigned int pkgVersion =
                          SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfAdjacentDomains using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfAdjacentDomains(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfAdjacentDomains.
   *
   * @param orig the ListOfAdjacentDomains instance to copy.
   */
  ListOfAdjacentDomains(const ListOfAdjacentDomains& orig);


  /**
   * Assignment operator for ListOfAdjacentDomains.
   *
   * @param rhs the ListOfAdjacentDomains object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfAdjacentDomains& operator=(const ListOfAdjacentDomains& rhs);


  /**
   * Creates and returns a deep copy of this ListOfAdjacentDomains object.
   *
   * @return a (deep) copy of this ListOfAdjacentDomains object.
   */
  virtual ListOfAdjacentDomains* clone() const;


  /**
   * Destructor for ListOfAdjacentDomains.
   */
  virtual ~ListOfAdjacentDomains();


  /**
   * Get an AdjacentDomains from the ListOfAdjacentDomains.
   *
   * @param n an unsigned int representing the index of the AdjacentDomains to
   * retrieve.
   *
   * @return the nth AdjacentDomains in this ListOfAdjacentDomains.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAdjacentDomains(const AdjacentDomains* object)
   * @see createAdjacentDomains()
   * @see get(const std::string& sid)
   * @see getNumAdjacentDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual AdjacentDomains* get(unsigned int n);


  /**
   * Get an AdjacentDomains from the ListOfAdjacentDomains.
   *
   * @param n an unsigned int representing the index of the AdjacentDomains to
   * retrieve.
   *
   * @return the nth AdjacentDomains in this ListOfAdjacentDomains.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAdjacentDomains(const AdjacentDomains* object)
   * @see createAdjacentDomains()
   * @see get(const std::string& sid)
   * @see getNumAdjacentDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const AdjacentDomains* get(unsigned int n) const;


  /**
   * Get an AdjacentDomains from the ListOfAdjacentDomains based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the AdjacentDomains to
   * retrieve.
   *
   * @return the AdjacentDomains in this ListOfAdjacentDomains with the given
   * @p sid or @c NULL if no such AdjacentDomains exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAdjacentDomains(const AdjacentDomains* object)
   * @see createAdjacentDomains()
   * @see get(unsigned int n)
   * @see getNumAdjacentDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual AdjacentDomains* get(const std::string& sid);


  /**
   * Get an AdjacentDomains from the ListOfAdjacentDomains based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the AdjacentDomains to
   * retrieve.
   *
   * @return the AdjacentDomains in this ListOfAdjacentDomains with the given
   * @p sid or @c NULL if no such AdjacentDomains exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAdjacentDomains(const AdjacentDomains* object)
   * @see createAdjacentDomains()
   * @see get(unsigned int n)
   * @see getNumAdjacentDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const AdjacentDomains* get(const std::string& sid) const;


  /**
   * Removes the nth AdjacentDomains from this ListOfAdjacentDomains and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the AdjacentDomains to
   * remove.
   *
   * @return a pointer to the nth AdjacentDomains in this
   * ListOfAdjacentDomains.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addAdjacentDomains(const AdjacentDomains* object)
   * @see createAdjacentDomains()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumAdjacentDomains()
   * @see remove(const std::string& sid)
   */
  virtual AdjacentDomains* remove(unsigned int n);


  /**
   * Removes the AdjacentDomains from this ListOfAdjacentDomains based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the AdjacentDomains to
   * remove.
   *
   * @return the AdjacentDomains in this ListOfAdjacentDomains based on the
   * identifier or NULL if no such AdjacentDomains exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addAdjacentDomains(const AdjacentDomains* object)
   * @see createAdjacentDomains()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumAdjacentDomains()
   * @see remove(unsigned int n)
   */
  virtual AdjacentDomains* remove(const std::string& sid);


  /**
   * Adds a copy of the given AdjacentDomains to this ListOfAdjacentDomains.
   *
   * @param ad the AdjacentDomains object to add.
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
   * @see createAdjacentDomains()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumAdjacentDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addAdjacentDomains(const AdjacentDomains* ad);


  /**
   * Get the number of AdjacentDomains objects in this ListOfAdjacentDomains.
   *
   * @return the number of AdjacentDomains objects in this
   * ListOfAdjacentDomains.
   *
   * @see addAdjacentDomains(const AdjacentDomains* object)
   * @see createAdjacentDomains()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumAdjacentDomains() const;


  /**
   * Creates a new AdjacentDomains object, adds it to this
   * ListOfAdjacentDomains object and returns the AdjacentDomains object
   * created.
   *
   * @return a new AdjacentDomains object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addAdjacentDomains(const AdjacentDomains* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumAdjacentDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  AdjacentDomains* createAdjacentDomains();


  /**
   * Get an AdjacentDomains from the ListOfAdjacentDomains based on the Domain1
   * to which it refers.
   *
   * @param sid a string representing the "domain1" attribute of the
   * AdjacentDomains object to retrieve.
   *
   * @return the first AdjacentDomains in this ListOfAdjacentDomains based on
   * the given domain1 attribute or NULL if no such AdjacentDomains exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const AdjacentDomains* getByDomain1(const std::string& sid) const;


  /**
   * Get an AdjacentDomains from the ListOfAdjacentDomains based on the Domain1
   * to which it refers.
   *
   * @param sid a string representing the "domain1" attribute of the
   * AdjacentDomains object to retrieve.
   *
   * @return the first AdjacentDomains in this ListOfAdjacentDomains based on
   * the given domain1 attribute or NULL if no such AdjacentDomains exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  AdjacentDomains* getByDomain1(const std::string& sid);


  /**
   * Get an AdjacentDomains from the ListOfAdjacentDomains based on the Domain2
   * to which it refers.
   *
   * @param sid a string representing the "domain2" attribute of the
   * AdjacentDomains object to retrieve.
   *
   * @return the first AdjacentDomains in this ListOfAdjacentDomains based on
   * the given domain2 attribute or NULL if no such AdjacentDomains exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const AdjacentDomains* getByDomain2(const std::string& sid) const;


  /**
   * Get an AdjacentDomains from the ListOfAdjacentDomains based on the Domain2
   * to which it refers.
   *
   * @param sid a string representing the "domain2" attribute of the
   * AdjacentDomains object to retrieve.
   *
   * @return the first AdjacentDomains in this ListOfAdjacentDomains based on
   * the given domain2 attribute or NULL if no such AdjacentDomains exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  AdjacentDomains* getByDomain2(const std::string& sid);


  /**
   * Returns the XML element name of this ListOfAdjacentDomains object.
   *
   * For ListOfAdjacentDomains, the XML element name is always
   * @c "listOfAdjacentDomains".
   *
   * @return the name of this element, i.e. @c "listOfAdjacentDomains".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfAdjacentDomains object.
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
   * ListOfAdjacentDomains object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfAdjacentDomains:
   * @sbmlconstant{SBML_SPATIAL_ADJACENTDOMAINS, SBMLSpatialTypeCode_t}.
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
   * Creates a new AdjacentDomains in this ListOfAdjacentDomains
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
 * Get an AdjacentDomains_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the AdjacentDomains_t to
 * retrieve.
 *
 * @return the nth AdjacentDomains_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfAdjacentDomains_t
 */
LIBSBML_EXTERN
AdjacentDomains_t*
ListOfAdjacentDomains_getAdjacentDomains(ListOf_t* lo, unsigned int n);


/**
 * Get an AdjacentDomains_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the AdjacentDomains_t to
 * retrieve.
 *
 * @return the AdjacentDomains_t in this ListOf_t with the given @p sid or
 * @c NULL if no such AdjacentDomains_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfAdjacentDomains_t
 */
LIBSBML_EXTERN
AdjacentDomains_t*
ListOfAdjacentDomains_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth AdjacentDomains_t from this ListOf_t and returns a pointer
 * to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the AdjacentDomains_t to
 * remove.
 *
 * @return a pointer to the nth AdjacentDomains_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfAdjacentDomains_t
 */
LIBSBML_EXTERN
AdjacentDomains_t*
ListOfAdjacentDomains_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the AdjacentDomains_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the AdjacentDomains_t to
 * remove.
 *
 * @return the AdjacentDomains_t in this ListOf_t based on the identifier or
 * NULL if no such AdjacentDomains_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfAdjacentDomains_t
 */
LIBSBML_EXTERN
AdjacentDomains_t*
ListOfAdjacentDomains_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfAdjacentDomains_H__ */


