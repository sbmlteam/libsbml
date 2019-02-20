/**
 * @file ListOfDomains.h
 * @brief Definition of the ListOfDomains class.
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
 * @class ListOfDomains
 * @sbmlbrief{spatial} TODO:Definition of the ListOfDomains class.
 */


#ifndef ListOfDomains_H__
#define ListOfDomains_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/Domain.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfDomains : public ListOf
{

public:

  /**
   * Creates a new ListOfDomains using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfDomains.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfDomains.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfDomains.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDomains(unsigned int level = SpatialExtension::getDefaultLevel(),
                unsigned int version = SpatialExtension::getDefaultVersion(),
                unsigned int pkgVersion =
                  SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDomains using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDomains(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfDomains.
   *
   * @param orig the ListOfDomains instance to copy.
   */
  ListOfDomains(const ListOfDomains& orig);


  /**
   * Assignment operator for ListOfDomains.
   *
   * @param rhs the ListOfDomains object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfDomains& operator=(const ListOfDomains& rhs);


  /**
   * Creates and returns a deep copy of this ListOfDomains object.
   *
   * @return a (deep) copy of this ListOfDomains object.
   */
  virtual ListOfDomains* clone() const;


  /**
   * Destructor for ListOfDomains.
   */
  virtual ~ListOfDomains();


  /**
   * Get a Domain from the ListOfDomains.
   *
   * @param n an unsigned int representing the index of the Domain to retrieve.
   *
   * @return the nth Domain in this ListOfDomains.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomain(const Domain* object)
   * @see createDomain()
   * @see get(const std::string& sid)
   * @see getNumDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual Domain* get(unsigned int n);


  /**
   * Get a Domain from the ListOfDomains.
   *
   * @param n an unsigned int representing the index of the Domain to retrieve.
   *
   * @return the nth Domain in this ListOfDomains.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomain(const Domain* object)
   * @see createDomain()
   * @see get(const std::string& sid)
   * @see getNumDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const Domain* get(unsigned int n) const;


  /**
   * Get a Domain from the ListOfDomains based on its identifier.
   *
   * @param sid a string representing the identifier of the Domain to retrieve.
   *
   * @return the Domain in this ListOfDomains with the given @p sid or @c NULL
   * if no such Domain exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomain(const Domain* object)
   * @see createDomain()
   * @see get(unsigned int n)
   * @see getNumDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual Domain* get(const std::string& sid);


  /**
   * Get a Domain from the ListOfDomains based on its identifier.
   *
   * @param sid a string representing the identifier of the Domain to retrieve.
   *
   * @return the Domain in this ListOfDomains with the given @p sid or @c NULL
   * if no such Domain exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomain(const Domain* object)
   * @see createDomain()
   * @see get(unsigned int n)
   * @see getNumDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const Domain* get(const std::string& sid) const;


  /**
   * Removes the nth Domain from this ListOfDomains and returns a pointer to
   * it.
   *
   * @param n an unsigned int representing the index of the Domain to remove.
   *
   * @return a pointer to the nth Domain in this ListOfDomains.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDomain(const Domain* object)
   * @see createDomain()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDomains()
   * @see remove(const std::string& sid)
   */
  virtual Domain* remove(unsigned int n);


  /**
   * Removes the Domain from this ListOfDomains based on its identifier and
   * returns a pointer to it.
   *
   * @param sid a string representing the identifier of the Domain to remove.
   *
   * @return the Domain in this ListOfDomains based on the identifier or NULL
   * if no such Domain exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDomain(const Domain* object)
   * @see createDomain()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDomains()
   * @see remove(unsigned int n)
   */
  virtual Domain* remove(const std::string& sid);


  /**
   * Adds a copy of the given Domain to this ListOfDomains.
   *
   * @param d the Domain object to add.
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
   * @see createDomain()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addDomain(const Domain* d);


  /**
   * Get the number of Domain objects in this ListOfDomains.
   *
   * @return the number of Domain objects in this ListOfDomains.
   *
   * @see addDomain(const Domain* object)
   * @see createDomain()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumDomains() const;


  /**
   * Creates a new Domain object, adds it to this ListOfDomains object and
   * returns the Domain object created.
   *
   * @return a new Domain object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomain(const Domain* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDomains()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  Domain* createDomain();


  /**
   * Get a Domain from the ListOfDomains based on the DomainType to which it
   * refers.
   *
   * @param sid a string representing the "domainType" attribute of the Domain
   * object to retrieve.
   *
   * @return the first Domain in this ListOfDomains based on the given
   * domainType attribute or NULL if no such Domain exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const Domain* getByDomainType(const std::string& sid) const;


  /**
   * Get a Domain from the ListOfDomains based on the DomainType to which it
   * refers.
   *
   * @param sid a string representing the "domainType" attribute of the Domain
   * object to retrieve.
   *
   * @return the first Domain in this ListOfDomains based on the given
   * domainType attribute or NULL if no such Domain exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  Domain* getByDomainType(const std::string& sid);


  /**
   * Returns the XML element name of this ListOfDomains object.
   *
   * For ListOfDomains, the XML element name is always @c "listOfDomains".
   *
   * @return the name of this element, i.e. @c "listOfDomains".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfDomains object.
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
   * ListOfDomains object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this ListOfDomains:
   * @sbmlconstant{SBML_SPATIAL_DOMAIN, SBMLSpatialTypeCode_t}.
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
   * Creates a new Domain in this ListOfDomains
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
 * Get a Domain_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Domain_t to retrieve.
 *
 * @return the nth Domain_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDomains_t
 */
LIBSBML_EXTERN
Domain_t*
ListOfDomains_getDomain(ListOf_t* lo, unsigned int n);


/**
 * Get a Domain_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Domain_t to retrieve.
 *
 * @return the Domain_t in this ListOf_t with the given @p sid or @c NULL if no
 * such Domain_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDomains_t
 */
LIBSBML_EXTERN
Domain_t*
ListOfDomains_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth Domain_t from this ListOf_t and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Domain_t to remove.
 *
 * @return a pointer to the nth Domain_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfDomains_t
 */
LIBSBML_EXTERN
Domain_t*
ListOfDomains_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the Domain_t from this ListOf_t based on its identifier and returns
 * a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Domain_t to remove.
 *
 * @return the Domain_t in this ListOf_t based on the identifier or NULL if no
 * such Domain_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfDomains_t
 */
LIBSBML_EXTERN
Domain_t*
ListOfDomains_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfDomains_H__ */


