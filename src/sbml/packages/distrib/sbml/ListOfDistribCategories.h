/**
 * @file ListOfDistribCategories.h
 * @brief Definition of the ListOfDistribCategories class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @class ListOfDistribCategories
 * @sbmlbrief{distrib} TODO:Definition of the ListOfDistribCategories class.
 */


#ifndef ListOfDistribCategories_H__
#define ListOfDistribCategories_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribCategory.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfDistribCategories : public ListOf
{
protected:

  /** @cond doxygenLibsbmlInternal */


  /** @endcond */

public:

  /**
   * Creates a new ListOfDistribCategories using the given SBML Level, Version
   * and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfDistribCategories.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfDistribCategories.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this ListOfDistribCategories.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDistribCategories(
                          unsigned int level =
                            DistribExtension::getDefaultLevel(),
                          unsigned int version =
                            DistribExtension::getDefaultVersion(),
                          unsigned int pkgVersion =
                            DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDistribCategories using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDistribCategories(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for ListOfDistribCategories.
   *
   * @param orig the ListOfDistribCategories instance to copy.
   */
  ListOfDistribCategories(const ListOfDistribCategories& orig);


  /**
   * Assignment operator for ListOfDistribCategories.
   *
   * @param rhs the ListOfDistribCategories object whose values are to be used
   * as the basis of the assignment.
   */
  ListOfDistribCategories& operator=(const ListOfDistribCategories& rhs);


  /**
   * Creates and returns a deep copy of this ListOfDistribCategories object.
   *
   * @return a (deep) copy of this ListOfDistribCategories object.
   */
  virtual ListOfDistribCategories* clone() const;


  /**
   * Destructor for ListOfDistribCategories.
   */
  virtual ~ListOfDistribCategories();


  /**
   * Returns the value of the "id" attribute of this ListOfDistribCategories.
   *
   * @return the value of the "id" attribute of this ListOfDistribCategories as
   * a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this ListOfDistribCategories.
   *
   * @return the value of the "name" attribute of this ListOfDistribCategories
   * as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this ListOfDistribCategories's "id"
   * attribute is set.
   *
   * @return @c true if this ListOfDistribCategories's "id" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this ListOfDistribCategories's "name"
   * attribute is set.
   *
   * @return @c true if this ListOfDistribCategories's "name" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this ListOfDistribCategories.
   *
   * @param id std::string& value of the "id" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * Calling this function with @p id = @c NULL or an empty string is
   * equivalent to calling unsetId().
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "name" attribute of this ListOfDistribCategories.
   *
   * @param name std::string& value of the "name" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p name = @c NULL or an empty string is
   * equivalent to calling unsetName().
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "id" attribute of this ListOfDistribCategories.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this ListOfDistribCategories.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Get a DistribCategory from the ListOfDistribCategories.
   *
   * @param n an unsigned int representing the index of the DistribCategory to
   * retrieve.
   *
   * @return the nth DistribCategory in this ListOfDistribCategories.
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
   * Get a DistribCategory from the ListOfDistribCategories.
   *
   * @param n an unsigned int representing the index of the DistribCategory to
   * retrieve.
   *
   * @return the nth DistribCategory in this ListOfDistribCategories.
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
   * Get a DistribCategory from the ListOfDistribCategories based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the DistribCategory to
   * retrieve.
   *
   * @return the DistribCategory in this ListOfDistribCategories with the given
   * @p sid or @c NULL if no such DistribCategory exists.
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
   * Get a DistribCategory from the ListOfDistribCategories based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the DistribCategory to
   * retrieve.
   *
   * @return the DistribCategory in this ListOfDistribCategories with the given
   * @p sid or @c NULL if no such DistribCategory exists.
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
   * Removes the nth DistribCategory from this ListOfDistribCategories and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the DistribCategory to
   * remove.
   *
   * @return a pointer to the nth DistribCategory in this
   * ListOfDistribCategories.
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
   * Removes the DistribCategory from this ListOfDistribCategories based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the DistribCategory to
   * remove.
   *
   * @return the DistribCategory in this ListOfDistribCategories based on the
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
   * Adds a copy of the given DistribCategory to this ListOfDistribCategories.
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
   * Get the number of DistribCategory objects in this ListOfDistribCategories.
   *
   * @return the number of DistribCategory objects in this
   * ListOfDistribCategories.
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
   * Creates a new DistribCategory object, adds it to this
   * ListOfDistribCategories object and returns the DistribCategory object
   * created.
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
   * Returns the XML element name of this ListOfDistribCategories object.
   *
   * For ListOfDistribCategories, the XML element name is always
   * @c "listOfDistribCategories".
   *
   * @return the name of this element, i.e. @c "listOfDistribCategories".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfDistribCategories object.
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
   * ListOfDistribCategories object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfDistribCategories:
   * @sbmlconstant{SBML_DISTRIB_CATEGORY, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * ListOfDistribCategories object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * ListOfDistribCategories have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;




  #ifndef SWIG




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new DistribCategory in this ListOfDistribCategories
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds the expected attributes for this element
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  void readL3V1V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  void readL3V2V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  void writeL3V1V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  void writeL3V2V1Attributes(XMLOutputStream& stream) const;

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
 * Returns the value of the "id" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this ListOf_t as a pointer to a
 * string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
char *
ListOfDistribCategories_getId(const ListOf_t * lo);


/**
 * Returns the value of the "name" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this ListOf_t as a pointer to a
 * string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
char *
ListOfDistribCategories_getName(const ListOf_t * lo);


/**
 * Predicate returning @c 1 (true) if this ListOf_t's "id" attribute is set.
 *
 * @param lo the ListOf_t structure.
 *
 * @return @c 1 (true) if this ListOf_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_isSetId(const ListOf_t * lo);


/**
 * Predicate returning @c 1 (true) if this ListOf_t's "name" attribute is set.
 *
 * @param lo the ListOf_t structure.
 *
 * @return @c 1 (true) if this ListOf_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_isSetName(const ListOf_t * lo);


/**
 * Sets the value of the "id" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling ListOfDistribCategories_unsetId().
 *
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_setId(ListOf_t * lo, const char * id);


/**
 * Sets the value of the "name" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling ListOfDistribCategories_unsetName().
 *
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_setName(ListOf_t * lo, const char * name);


/**
 * Unsets the value of the "id" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_unsetId(ListOf_t * lo);


/**
 * Unsets the value of the "name" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
int
ListOfDistribCategories_unsetName(ListOf_t * lo);


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
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfDistribCategories_getDistribCategory(ListOf_t* lo, unsigned int n);


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
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfDistribCategories_getById(ListOf_t* lo, const char *sid);


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
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfDistribCategories_remove(ListOf_t* lo, unsigned int n);


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
 * @memberof ListOfDistribCategories_t
 */
LIBSBML_EXTERN
DistribCategory_t*
ListOfDistribCategories_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfDistribCategories_H__ */


