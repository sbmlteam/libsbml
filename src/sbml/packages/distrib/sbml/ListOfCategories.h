/**
 * @file ListOfCategories.h
 * @brief Definition of the ListOfCategories class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @class ListOfCategories
 * @sbmlbrief{distrib} TODO:Definition of the ListOfCategories class.
 */


#ifndef ListOfCategories_H__
#define ListOfCategories_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribListOfBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribCategory.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfCategories : public DistribListOfBase
{

public:

  /**
   * Creates a new ListOfCategories using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfCategories.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfCategories.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this ListOfCategories.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCategories(unsigned int level = DistribExtension::getDefaultLevel(),
                   unsigned int version =
                     DistribExtension::getDefaultVersion(),
                   unsigned int pkgVersion =
                     DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfCategories using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfCategories(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for ListOfCategories.
   *
   * @param orig the ListOfCategories instance to copy.
   */
  ListOfCategories(const ListOfCategories& orig);


  /**
   * Assignment operator for ListOfCategories.
   *
   * @param rhs the ListOfCategories object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfCategories& operator=(const ListOfCategories& rhs);


  /**
   * Creates and returns a deep copy of this ListOfCategories object.
   *
   * @return a (deep) copy of this ListOfCategories object.
   */
  virtual ListOfCategories* clone() const;


  /**
   * Destructor for ListOfCategories.
   */
  virtual ~ListOfCategories();


  /**
   * Get a DistribCategory from the ListOfCategories.
   *
   * @param n an unsigned int representing the index of the DistribCategory to
   * retrieve.
   *
   * @return the nth DistribCategory in this ListOfCategories.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see get(const std::string& sid)
   * @see getNumDistribCategories()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual DistribCategory* get(unsigned int n);


  /**
   * Get a DistribCategory from the ListOfCategories.
   *
   * @param n an unsigned int representing the index of the DistribCategory to
   * retrieve.
   *
   * @return the nth DistribCategory in this ListOfCategories.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see get(const std::string& sid)
   * @see getNumDistribCategories()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const DistribCategory* get(unsigned int n) const;


  /**
   * Get a DistribCategory from the ListOfCategories based on its identifier.
   *
   * @param sid a string representing the identifier of the DistribCategory to
   * retrieve.
   *
   * @return the DistribCategory in this ListOfCategories with the given @p sid
   * or @c NULL if no such DistribCategory exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see get(unsigned int n)
   * @see getNumDistribCategories()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual DistribCategory* get(const std::string& sid);


  /**
   * Get a DistribCategory from the ListOfCategories based on its identifier.
   *
   * @param sid a string representing the identifier of the DistribCategory to
   * retrieve.
   *
   * @return the DistribCategory in this ListOfCategories with the given @p sid
   * or @c NULL if no such DistribCategory exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see get(unsigned int n)
   * @see getNumDistribCategories()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const DistribCategory* get(const std::string& sid) const;


  /**
   * Removes the nth DistribCategory from this ListOfCategories and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the DistribCategory to
   * remove.
   *
   * @return a pointer to the nth DistribCategory in this ListOfCategories.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribCategories()
   * @see remove(const std::string& sid)
   */
  virtual DistribCategory* remove(unsigned int n);


  /**
   * Removes the DistribCategory from this ListOfCategories based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the DistribCategory to
   * remove.
   *
   * @return the DistribCategory in this ListOfCategories based on the
   * identifier or NULL if no such DistribCategory exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribCategories()
   * @see remove(unsigned int n)
   */
  virtual DistribCategory* remove(const std::string& sid);


  /**
   * Adds a copy of the given DistribCategory to this ListOfCategories.
   *
   * @param dc the DistribCategory object to add.
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
   * @see createDistribCategory()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribCategories()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addDistribCategory(const DistribCategory* dc);


  /**
   * Get the number of DistribCategory objects in this ListOfCategories.
   *
   * @return the number of DistribCategory objects in this ListOfCategories.
   *
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumDistribCategories() const;


  /**
   * Creates a new DistribCategory object, adds it to this ListOfCategories
   * object and returns the DistribCategory object created.
   *
   * @return a new DistribCategory object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumDistribCategories()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  DistribCategory* createDistribCategory();


  /**
   * Returns the XML element name of this ListOfCategories object.
   *
   * For ListOfCategories, the XML element name is always
   * @c "listOfCategories".
   *
   * @return the name of this element, i.e. @c "listOfCategories".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfCategories object.
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
   * ListOfCategories object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfCategories:
   * @sbmlconstant{SBML_DISTRIB_CATEGORY, SBMLDistribTypeCode_t}.
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
   * Creates a new DistribCategory in this ListOfCategories
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Distrib package
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
 * Get a DistribCategory_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the DistribCategory_t to
 * retrieve.
 *
 * @return the nth DistribCategory_t in this ListOf_t.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCategories_t
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfCategories_getDistribCategory(ListOf_t* lo, unsigned int n);


/**
 * Get a DistribCategory_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the DistribCategory_t to
 * retrieve.
 *
 * @return the DistribCategory_t in this ListOf_t with the given @p sid or
 * @c NULL if no such DistribCategory_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfCategories_t
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfCategories_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth DistribCategory_t from this ListOf_t and returns a pointer
 * to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the DistribCategory_t to
 * remove.
 *
 * @return a pointer to the nth DistribCategory_t in this ListOf_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfCategories_t
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfCategories_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the DistribCategory_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the DistribCategory_t to
 * remove.
 *
 * @return the DistribCategory_t in this ListOf_t based on the identifier or
 * NULL if no such DistribCategory_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ListOfCategories_t
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfCategories_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfCategories_H__ */


