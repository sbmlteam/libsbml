/**
 * @file ListOfUserDefinedConstraints.h
 * @brief Definition of the ListOfUserDefinedConstraints class.
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
 * @class ListOfUserDefinedConstraints
 * @sbmlbrief{fbc} TODO:Definition of the ListOfUserDefinedConstraints class.
 */


#ifndef ListOfUserDefinedConstraints_H__
#define ListOfUserDefinedConstraints_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/sbml/UserDefinedConstraint.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfUserDefinedConstraints : public ListOf
{

public:

  /**
   * Creates a new ListOfUserDefinedConstraints using the given SBML Level,
   * Version and &ldquo;fbc&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfUserDefinedConstraints.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfUserDefinedConstraints.
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this
   * ListOfUserDefinedConstraints.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfUserDefinedConstraints(
                               unsigned int level =
                                 FbcExtension::getDefaultLevel(),
                               unsigned int version =
                                 FbcExtension::getDefaultVersion(),
                               unsigned int pkgVersion =
                                 FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfUserDefinedConstraints using the given
   * FbcPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param fbcns the FbcPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfUserDefinedConstraints(FbcPkgNamespaces *fbcns);


  /**
   * Copy constructor for ListOfUserDefinedConstraints.
   *
   * @param orig the ListOfUserDefinedConstraints instance to copy.
   */
  ListOfUserDefinedConstraints(const ListOfUserDefinedConstraints& orig);


  /**
   * Assignment operator for ListOfUserDefinedConstraints.
   *
   * @param rhs the ListOfUserDefinedConstraints object whose values are to be
   * used as the basis of the assignment.
   */
  ListOfUserDefinedConstraints& operator=(const ListOfUserDefinedConstraints&
    rhs);


  /**
   * Creates and returns a deep copy of this ListOfUserDefinedConstraints
   * object.
   *
   * @return a (deep) copy of this ListOfUserDefinedConstraints object.
   */
  virtual ListOfUserDefinedConstraints* clone() const;


  /**
   * Destructor for ListOfUserDefinedConstraints.
   */
  virtual ~ListOfUserDefinedConstraints();


  /**
   * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints.
   *
   * @param n an unsigned int representing the index of the
   * UserDefinedConstraint to retrieve.
   *
   * @return the nth UserDefinedConstraint in this ListOfUserDefinedConstraints
   * or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraint(const UserDefinedConstraint* object)
   * @see createUserDefinedConstraint()
   * @see get(const std::string& sid)
   * @see getNumUserDefinedConstraints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual UserDefinedConstraint* get(unsigned int n);


  /**
   * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints.
   *
   * @param n an unsigned int representing the index of the
   * UserDefinedConstraint to retrieve.
   *
   * @return the nth UserDefinedConstraint in this ListOfUserDefinedConstraints
   * or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraint(const UserDefinedConstraint* object)
   * @see createUserDefinedConstraint()
   * @see get(const std::string& sid)
   * @see getNumUserDefinedConstraints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const UserDefinedConstraint* get(unsigned int n) const;


  /**
   * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based
   * on its identifier.
   *
   * @param sid a string representing the identifier of the
   * UserDefinedConstraint to retrieve.
   *
   * @return the UserDefinedConstraint in this ListOfUserDefinedConstraints
   * with the given @p sid or @c NULL if no such UserDefinedConstraint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraint(const UserDefinedConstraint* object)
   * @see createUserDefinedConstraint()
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual UserDefinedConstraint* get(const std::string& sid);


  /**
   * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based
   * on its identifier.
   *
   * @param sid a string representing the identifier of the
   * UserDefinedConstraint to retrieve.
   *
   * @return the UserDefinedConstraint in this ListOfUserDefinedConstraints
   * with the given @p sid or @c NULL if no such UserDefinedConstraint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraint(const UserDefinedConstraint* object)
   * @see createUserDefinedConstraint()
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const UserDefinedConstraint* get(const std::string& sid) const;


  /**
   * Removes the nth UserDefinedConstraint from this
   * ListOfUserDefinedConstraints and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the
   * UserDefinedConstraint to remove.
   *
   * @return a pointer to the nth UserDefinedConstraint in this
   * ListOfUserDefinedConstraints.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUserDefinedConstraint(const UserDefinedConstraint* object)
   * @see createUserDefinedConstraint()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraints()
   * @see remove(const std::string& sid)
   */
  virtual UserDefinedConstraint* remove(unsigned int n);


  /**
   * Removes the UserDefinedConstraint from this ListOfUserDefinedConstraints
   * based on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the
   * UserDefinedConstraint to remove.
   *
   * @return the UserDefinedConstraint in this ListOfUserDefinedConstraints
   * based on the identifier or NULL if no such UserDefinedConstraint exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUserDefinedConstraint(const UserDefinedConstraint* object)
   * @see createUserDefinedConstraint()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraints()
   * @see remove(unsigned int n)
   */
  virtual UserDefinedConstraint* remove(const std::string& sid);


  /**
   * Adds a copy of the given UserDefinedConstraint to this
   * ListOfUserDefinedConstraints.
   *
   * @param udc the UserDefinedConstraint object to add.
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
   * @see createUserDefinedConstraint()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addUserDefinedConstraint(const UserDefinedConstraint* udc);


  /**
   * Get the number of UserDefinedConstraint objects in this
   * ListOfUserDefinedConstraints.
   *
   * @return the number of UserDefinedConstraint objects in this
   * ListOfUserDefinedConstraints.
   *
   * @see addUserDefinedConstraint(const UserDefinedConstraint* object)
   * @see createUserDefinedConstraint()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumUserDefinedConstraints() const;


  /**
   * Creates a new UserDefinedConstraint object, adds it to this
   * ListOfUserDefinedConstraints object and returns the UserDefinedConstraint
   * object created.
   *
   * @return a new UserDefinedConstraint object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraint(const UserDefinedConstraint* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraints()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  UserDefinedConstraint* createUserDefinedConstraint();


  /**
   * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based
   * on the LowerBound to which it refers.
   *
   * @param sid a string representing the "lowerBound" attribute of the
   * UserDefinedConstraint object to retrieve.
   *
   * @return the first UserDefinedConstraint in this
   * ListOfUserDefinedConstraints based on the given lowerBound attribute or
   * NULL if no such UserDefinedConstraint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const UserDefinedConstraint* getByLowerBound(const std::string& sid) const;


  /**
   * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based
   * on the LowerBound to which it refers.
   *
   * @param sid a string representing the "lowerBound" attribute of the
   * UserDefinedConstraint object to retrieve.
   *
   * @return the first UserDefinedConstraint in this
   * ListOfUserDefinedConstraints based on the given lowerBound attribute or
   * NULL if no such UserDefinedConstraint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  UserDefinedConstraint* getByLowerBound(const std::string& sid);


  /**
   * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based
   * on the UpperBound to which it refers.
   *
   * @param sid a string representing the "upperBound" attribute of the
   * UserDefinedConstraint object to retrieve.
   *
   * @return the first UserDefinedConstraint in this
   * ListOfUserDefinedConstraints based on the given upperBound attribute or
   * NULL if no such UserDefinedConstraint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const UserDefinedConstraint* getByUpperBound(const std::string& sid) const;


  /**
   * Get an UserDefinedConstraint from the ListOfUserDefinedConstraints based
   * on the UpperBound to which it refers.
   *
   * @param sid a string representing the "upperBound" attribute of the
   * UserDefinedConstraint object to retrieve.
   *
   * @return the first UserDefinedConstraint in this
   * ListOfUserDefinedConstraints based on the given upperBound attribute or
   * NULL if no such UserDefinedConstraint exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  UserDefinedConstraint* getByUpperBound(const std::string& sid);


  /**
   * Returns the XML element name of this ListOfUserDefinedConstraints object.
   *
   * For ListOfUserDefinedConstraints, the XML element name is always
   * @c "listOfUserDefinedConstraints".
   *
   * @return the name of this element, i.e. @c "listOfUserDefinedConstraints".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfUserDefinedConstraints
   * object.
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
   * ListOfUserDefinedConstraints object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfUserDefinedConstraints:
   * @sbmlconstant{SBML_FBC_USERDEFINEDCONSTRAINT, SBMLFbcTypeCode_t}.
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
   * Creates a new UserDefinedConstraint in this ListOfUserDefinedConstraints
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Get an UserDefinedConstraint_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * UserDefinedConstraint_t to retrieve.
 *
 * @return the nth UserDefinedConstraint_t in this ListOf_t or @c NULL if no
 * such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfUserDefinedConstraints_t
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
ListOfUserDefinedConstraints_getUserDefinedConstraint(ListOf_t* lo,
                                                      unsigned int n);


/**
 * Get an UserDefinedConstraint_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * UserDefinedConstraint_t to retrieve.
 *
 * @return the UserDefinedConstraint_t in this ListOf_t with the given @p sid
 * or @c NULL if no such UserDefinedConstraint_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfUserDefinedConstraints_t
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
ListOfUserDefinedConstraints_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth UserDefinedConstraint_t from this ListOf_t and returns a
 * pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * UserDefinedConstraint_t to remove.
 *
 * @return a pointer to the nth UserDefinedConstraint_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfUserDefinedConstraints_t
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
ListOfUserDefinedConstraints_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the UserDefinedConstraint_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * UserDefinedConstraint_t to remove.
 *
 * @return the UserDefinedConstraint_t in this ListOf_t based on the identifier
 * or NULL if no such UserDefinedConstraint_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfUserDefinedConstraints_t
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
ListOfUserDefinedConstraints_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfUserDefinedConstraints_H__ */


