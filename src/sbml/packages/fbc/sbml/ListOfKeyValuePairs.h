/**
 * @file ListOfKeyValuePairs.h
 * @brief Definition of the ListOfKeyValuePairs class.
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
 * @class ListOfKeyValuePairs
 * @sbmlbrief{fbc} TODO:Definition of the ListOfKeyValuePairs class.
 */


#ifndef ListOfKeyValuePairs_H__
#define ListOfKeyValuePairs_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/sbml/KeyValuePair.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfKeyValuePairs : public ListOf
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mXmlns;

  /** @endcond */

public:

  /**
   * Creates a new ListOfKeyValuePairs using the given SBML Level, Version and
   * &ldquo;fbc&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfKeyValuePairs.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfKeyValuePairs.
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this
   * ListOfKeyValuePairs.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfKeyValuePairs(unsigned int level = FbcExtension::getDefaultLevel(),
                      unsigned int version = FbcExtension::getDefaultVersion(),
                      unsigned int pkgVersion =
                        FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfKeyValuePairs using the given FbcPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param fbcns the FbcPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfKeyValuePairs(FbcPkgNamespaces *fbcns);


  /**
   * Copy constructor for ListOfKeyValuePairs.
   *
   * @param orig the ListOfKeyValuePairs instance to copy.
   */
  ListOfKeyValuePairs(const ListOfKeyValuePairs& orig);


  /**
   * Assignment operator for ListOfKeyValuePairs.
   *
   * @param rhs the ListOfKeyValuePairs object whose values are to be used as
   * the basis of the assignment.
   */
  ListOfKeyValuePairs& operator=(const ListOfKeyValuePairs& rhs);


  /**
   * Creates and returns a deep copy of this ListOfKeyValuePairs object.
   *
   * @return a (deep) copy of this ListOfKeyValuePairs object.
   */
  virtual ListOfKeyValuePairs* clone() const;


  /**
   * Destructor for ListOfKeyValuePairs.
   */
  virtual ~ListOfKeyValuePairs();


  /**
   * Returns the value of the "xmlns" attribute of this ListOfKeyValuePairs.
   *
   * @return the value of the "xmlns" attribute of this ListOfKeyValuePairs as
   * a string.
   */
  const std::string& getXmlns() const;


  /**
   * Predicate returning @c true if this ListOfKeyValuePairs's "xmlns"
   * attribute is set.
   *
   * @return @c true if this ListOfKeyValuePairs's "xmlns" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetXmlns() const;


  /**
   * Sets the value of the "xmlns" attribute of this ListOfKeyValuePairs.
   *
   * @param xmlns std::string& value of the "xmlns" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p xmlns = @c NULL or an empty string is
   * equivalent to calling unsetXmlns().
   */
  int setXmlns(const std::string& xmlns);

  /**
   * Sets the value of the "xmlns" attribute of this ListOfKeyValuePairs.
   *
   * @param xmlns XMLNamespaces* value of the "xmlns" attribute to be set.
   * @param prefix std::string& optional prefix (defaults to "")
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p xmlns = @c NULL or an empty string is
   * equivalent to calling unsetXmlns().
   */
  int setXmlns(const XMLNamespaces* xmlns, const std::string& prefix="");


  /**
   * Unsets the value of the "xmlns" attribute of this ListOfKeyValuePairs.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetXmlns();


  /**
   * Get a KeyValuePair from the ListOfKeyValuePairs.
   *
   * @param n an unsigned int representing the index of the KeyValuePair to
   * retrieve.
   *
   * @return the nth KeyValuePair in this ListOfKeyValuePairs or @c NULL if no
   * such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addKeyValuePair(const KeyValuePair* object)
   * @see createKeyValuePair()
   * @see get(const std::string& sid)
   * @see getNumKeyValuePairs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual KeyValuePair* get(unsigned int n);


  /**
   * Get a KeyValuePair from the ListOfKeyValuePairs.
   *
   * @param n an unsigned int representing the index of the KeyValuePair to
   * retrieve.
   *
   * @return the nth KeyValuePair in this ListOfKeyValuePairs or @c NULL if no
   * such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addKeyValuePair(const KeyValuePair* object)
   * @see createKeyValuePair()
   * @see get(const std::string& sid)
   * @see getNumKeyValuePairs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const KeyValuePair* get(unsigned int n) const;


  /**
   * Get a KeyValuePair from the ListOfKeyValuePairs based on its identifier.
   *
   * @param sid a string representing the identifier of the KeyValuePair to
   * retrieve.
   *
   * @return the KeyValuePair in this ListOfKeyValuePairs with the given @p sid
   * or @c NULL if no such KeyValuePair exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addKeyValuePair(const KeyValuePair* object)
   * @see createKeyValuePair()
   * @see get(unsigned int n)
   * @see getNumKeyValuePairs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual KeyValuePair* get(const std::string& sid);


  /**
   * Get a KeyValuePair from the ListOfKeyValuePairs based on its identifier.
   *
   * @param sid a string representing the identifier of the KeyValuePair to
   * retrieve.
   *
   * @return the KeyValuePair in this ListOfKeyValuePairs with the given @p sid
   * or @c NULL if no such KeyValuePair exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addKeyValuePair(const KeyValuePair* object)
   * @see createKeyValuePair()
   * @see get(unsigned int n)
   * @see getNumKeyValuePairs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const KeyValuePair* get(const std::string& sid) const;


  /**
   * Removes the nth KeyValuePair from this ListOfKeyValuePairs and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the KeyValuePair to
   * remove.
   *
   * @return a pointer to the nth KeyValuePair in this ListOfKeyValuePairs.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addKeyValuePair(const KeyValuePair* object)
   * @see createKeyValuePair()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumKeyValuePairs()
   * @see remove(const std::string& sid)
   */
  virtual KeyValuePair* remove(unsigned int n);


  /**
   * Removes the KeyValuePair from this ListOfKeyValuePairs based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the KeyValuePair to
   * remove.
   *
   * @return the KeyValuePair in this ListOfKeyValuePairs based on the
   * identifier or NULL if no such KeyValuePair exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addKeyValuePair(const KeyValuePair* object)
   * @see createKeyValuePair()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumKeyValuePairs()
   * @see remove(unsigned int n)
   */
  virtual KeyValuePair* remove(const std::string& sid);


  /**
   * Adds a copy of the given KeyValuePair to this ListOfKeyValuePairs.
   *
   * @param kvp the KeyValuePair object to add.
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
   * @see createKeyValuePair()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumKeyValuePairs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addKeyValuePair(const KeyValuePair* kvp);


  /**
   * Get the number of KeyValuePair objects in this ListOfKeyValuePairs.
   *
   * @return the number of KeyValuePair objects in this ListOfKeyValuePairs.
   *
   * @see addKeyValuePair(const KeyValuePair* object)
   * @see createKeyValuePair()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumKeyValuePairs() const;


  /**
   * Creates a new KeyValuePair object, adds it to this ListOfKeyValuePairs
   * object and returns the KeyValuePair object created.
   *
   * @return a new KeyValuePair object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addKeyValuePair(const KeyValuePair* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumKeyValuePairs()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  KeyValuePair* createKeyValuePair();


  /**
   * Returns the XML element name of this ListOfKeyValuePairs object.
   *
   * For ListOfKeyValuePairs, the XML element name is always
   * @c "listOfKeyValuePairs".
   *
   * @return the name of this element, i.e. @c "listOfKeyValuePairs".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfKeyValuePairs object.
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
   * ListOfKeyValuePairs object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfKeyValuePairs:
   * @sbmlconstant{SBML_FBC_KEYVALUEPAIR, SBMLFbcTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * ListOfKeyValuePairs object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * ListOfKeyValuePairs have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the ListOfKeyValuePairs object are:
   * @li "xmlns"
   */
  virtual bool hasRequiredAttributes() const;




  #ifndef SWIG




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new KeyValuePair in this ListOfKeyValuePairs
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
  void readL3V1V3Attributes(const XMLAttributes& attributes);

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
  void writeL3V1V3Attributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace
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
 * Returns the value of the "xmlns" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure whose xmlns is sought.
 *
 * @return the value of the "xmlns" attribute of this ListOf_t as a pointer to
 * a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof ListOfKeyValuePairs_t
 */
LIBSBML_EXTERN
char *
ListOfKeyValuePairs_getXmlns(const ListOf_t * lo);


/**
 * Predicate returning @c 1 (true) if this ListOf_t's "xmlns" attribute is set.
 *
 * @param lo the ListOf_t structure.
 *
 * @return @c 1 (true) if this ListOf_t's "xmlns" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof ListOfKeyValuePairs_t
 */
LIBSBML_EXTERN
int
ListOfKeyValuePairs_isSetXmlns(const ListOf_t * lo);


/**
 * Sets the value of the "xmlns" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @param xmlns const char * value of the "xmlns" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p xmlns = @c NULL or an empty string is
 * equivalent to calling ListOfKeyValuePairs_unsetXmlns().
 *
 * @memberof ListOfKeyValuePairs_t
 */
LIBSBML_EXTERN
int
ListOfKeyValuePairs_setXmlns(ListOf_t * lo, const char * xmlns);


/**
 * Unsets the value of the "xmlns" attribute of this ListOf_t.
 *
 * @param lo the ListOf_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ListOfKeyValuePairs_t
 */
LIBSBML_EXTERN
int
ListOfKeyValuePairs_unsetXmlns(ListOf_t * lo);


/**
 * Get a KeyValuePair_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the KeyValuePair_t to
 * retrieve.
 *
 * @return the nth KeyValuePair_t in this ListOf_t or @c NULL if no such object
 * exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfKeyValuePairs_t
 */
LIBSBML_EXTERN
KeyValuePair_t*
ListOfKeyValuePairs_getKeyValuePair(ListOf_t* lo, unsigned int n);


/**
 * Get a KeyValuePair_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the KeyValuePair_t to
 * retrieve.
 *
 * @return the KeyValuePair_t in this ListOf_t with the given @p sid or @c NULL
 * if no such KeyValuePair_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfKeyValuePairs_t
 */
LIBSBML_EXTERN
KeyValuePair_t*
ListOfKeyValuePairs_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth KeyValuePair_t from this ListOf_t and returns a pointer to
 * it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the KeyValuePair_t to
 * remove.
 *
 * @return a pointer to the nth KeyValuePair_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfKeyValuePairs_t
 */
LIBSBML_EXTERN
KeyValuePair_t*
ListOfKeyValuePairs_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the KeyValuePair_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the KeyValuePair_t to
 * remove.
 *
 * @return the KeyValuePair_t in this ListOf_t based on the identifier or NULL
 * if no such KeyValuePair_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfKeyValuePairs_t
 */
LIBSBML_EXTERN
KeyValuePair_t*
ListOfKeyValuePairs_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfKeyValuePairs_H__ */


