/**
 * @file ListOfDomainTypes.h
 * @brief Definition of the ListOfDomainTypes class.
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
 */


#ifndef ListOfDomainTypes_H__
#define ListOfDomainTypes_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/DomainType.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfDomainTypes : public ListOf
{

public:

  /**
   * Creates a new ListOfDomainTypes using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfDomainTypes.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfDomainTypes.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ListOfDomainTypes.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDomainTypes(unsigned int level = SpatialExtension::getDefaultLevel(),
                    unsigned int version =
                      SpatialExtension::getDefaultVersion(),
                    unsigned int pkgVersion =
                      SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDomainTypes using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDomainTypes(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ListOfDomainTypes.
   *
   * @param orig the ListOfDomainTypes instance to copy.
   */
  ListOfDomainTypes(const ListOfDomainTypes& orig);


  /**
   * Assignment operator for ListOfDomainTypes.
   *
   * @param rhs the ListOfDomainTypes object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfDomainTypes& operator=(const ListOfDomainTypes& rhs);


  /**
   * Creates and returns a deep copy of this ListOfDomainTypes object.
   *
   * @return a (deep) copy of this ListOfDomainTypes object.
   */
  virtual ListOfDomainTypes* clone() const;


  /**
   * Destructor for ListOfDomainTypes.
   */
  virtual ~ListOfDomainTypes();


  /**
   * Get a DomainType from the ListOfDomainTypes.
   *
   * @param n an unsigned int representing the index of the DomainType to
   * retrieve.
   *
   * @return the nth DomainType in this ListOfDomainTypes.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomainType(const DomainType* object)
   * @see createDomainType()
   * @see get(const std::string& sid)
   * @see getNumDomainTypes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual DomainType* get(unsigned int n);


  /**
   * Get a DomainType from the ListOfDomainTypes.
   *
   * @param n an unsigned int representing the index of the DomainType to
   * retrieve.
   *
   * @return the nth DomainType in this ListOfDomainTypes.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomainType(const DomainType* object)
   * @see createDomainType()
   * @see get(const std::string& sid)
   * @see getNumDomainTypes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const DomainType* get(unsigned int n) const;


  /**
   * Get a DomainType from the ListOfDomainTypes based on its identifier.
   *
   * @param sid a string representing the identifier of the DomainType to
   * retrieve.
   *
   * @return the DomainType in this ListOfDomainTypes with the given @p sid or
   * @c NULL if no such DomainType exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomainType(const DomainType* object)
   * @see createDomainType()
   * @see get(unsigned int n)
   * @see getNumDomainTypes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual DomainType* get(const std::string& sid);


  /**
   * Get a DomainType from the ListOfDomainTypes based on its identifier.
   *
   * @param sid a string representing the identifier of the DomainType to
   * retrieve.
   *
   * @return the DomainType in this ListOfDomainTypes with the given @p sid or
   * @c NULL if no such DomainType exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomainType(const DomainType* object)
   * @see createDomainType()
   * @see get(unsigned int n)
   * @see getNumDomainTypes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const DomainType* get(const std::string& sid) const;


  /**
   * Removes the nth DomainType from this ListOfDomainTypes and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the DomainType to
   * remove.
   *
   * @return a pointer to the nth DomainType in this ListOfDomainTypes.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDomainType(const DomainType* object)
   * @see createDomainType()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDomainTypes()
   * @see remove(const std::string& sid)
   */
  virtual DomainType* remove(unsigned int n);


  /**
   * Removes the DomainType from this ListOfDomainTypes based on its identifier
   * and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the DomainType to
   * remove.
   *
   * @return the DomainType in this ListOfDomainTypes based on the identifier
   * or NULL if no such DomainType exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDomainType(const DomainType* object)
   * @see createDomainType()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDomainTypes()
   * @see remove(unsigned int n)
   */
  virtual DomainType* remove(const std::string& sid);


  /**
   * Adds a copy of the given DomainType to this ListOfDomainTypes.
   *
   * @param dt the DomainType object to add.
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
   * @see createDomainType()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDomainTypes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addDomainType(const DomainType* dt);


  /**
   * Get the number of DomainType objects in this ListOfDomainTypes.
   *
   * @return the number of DomainType objects in this ListOfDomainTypes.
   *
   * @see addDomainType(const DomainType* object)
   * @see createDomainType()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumDomainTypes() const;


  /**
   * Creates a new DomainType object, adds it to this ListOfDomainTypes object
   * and returns the DomainType object created.
   *
   * @return a new DomainType object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDomainType(const DomainType* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDomainTypes()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  DomainType* createDomainType();


  /**
   * Returns the XML element name of this ListOfDomainTypes object.
   *
   * For ListOfDomainTypes, the XML element name is always
   * @c "listOfDomainTypes".
   *
   * @return the name of this element, i.e. @c "listOfDomainTypes".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfDomainTypes object.
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
   * ListOfDomainTypes object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfDomainTypes:
   * @sbmlconstant{SBML_SPATIAL_DOMAINTYPE, SBMLSpatialTypeCode_t}.
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
   * Creates a new DomainType in this ListOfDomainTypes
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
 * Get a DomainType_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the DomainType_t to
 * retrieve.
 *
 * @return the nth DomainType_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDomainTypes_t
 */
LIBSBML_EXTERN
DomainType_t*
ListOfDomainTypes_getDomainType(ListOf_t* lo, unsigned int n);


/**
 * Get a DomainType_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the DomainType_t to
 * retrieve.
 *
 * @return the DomainType_t in this ListOf_t with the given @p sid or @c NULL
 * if no such DomainType_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDomainTypes_t
 */
LIBSBML_EXTERN
DomainType_t*
ListOfDomainTypes_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth DomainType_t from this ListOf_t and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the DomainType_t to
 * remove.
 *
 * @return a pointer to the nth DomainType_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfDomainTypes_t
 */
LIBSBML_EXTERN
DomainType_t*
ListOfDomainTypes_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the DomainType_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the DomainType_t to
 * remove.
 *
 * @return the DomainType_t in this ListOf_t based on the identifier or NULL if
 * no such DomainType_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfDomainTypes_t
 */
LIBSBML_EXTERN
DomainType_t*
ListOfDomainTypes_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfDomainTypes_H__ */


