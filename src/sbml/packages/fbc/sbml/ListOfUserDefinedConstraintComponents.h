/**
 * @file ListOfUserDefinedConstraintComponents.h
 * @brief Definition of the ListOfUserDefinedConstraintComponents class.
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
 * @class ListOfUserDefinedConstraintComponents
 * @sbmlbrief{fbc} TODO:Definition of the ListOfUserDefinedConstraintComponents
 * class.
 */


#ifndef ListOfUserDefinedConstraintComponents_H__
#define ListOfUserDefinedConstraintComponents_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/sbml/UserDefinedConstraintComponent.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ListOfUserDefinedConstraintComponents : public ListOf
{

public:

  /**
   * Creates a new ListOfUserDefinedConstraintComponents using the given SBML
   * Level, Version and &ldquo;fbc&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfUserDefinedConstraintComponents.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfUserDefinedConstraintComponents.
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this
   * ListOfUserDefinedConstraintComponents.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfUserDefinedConstraintComponents(
                                        unsigned int level =
                                          FbcExtension::getDefaultLevel(),
                                        unsigned int version =
                                          FbcExtension::getDefaultVersion(),
                                        unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfUserDefinedConstraintComponents using the given
   * FbcPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param fbcns the FbcPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfUserDefinedConstraintComponents(FbcPkgNamespaces *fbcns);


  /**
   * Copy constructor for ListOfUserDefinedConstraintComponents.
   *
   * @param orig the ListOfUserDefinedConstraintComponents instance to copy.
   */
  ListOfUserDefinedConstraintComponents(const
    ListOfUserDefinedConstraintComponents& orig);


  /**
   * Assignment operator for ListOfUserDefinedConstraintComponents.
   *
   * @param rhs the ListOfUserDefinedConstraintComponents object whose values
   * are to be used as the basis of the assignment.
   */
  ListOfUserDefinedConstraintComponents& operator=(const
    ListOfUserDefinedConstraintComponents& rhs);


  /**
   * Creates and returns a deep copy of this
   * ListOfUserDefinedConstraintComponents object.
   *
   * @return a (deep) copy of this ListOfUserDefinedConstraintComponents
   * object.
   */
  virtual ListOfUserDefinedConstraintComponents* clone() const;


  /**
   * Destructor for ListOfUserDefinedConstraintComponents.
   */
  virtual ~ListOfUserDefinedConstraintComponents();


  /**
   * Get an UserDefinedConstraintComponent from the
   * ListOfUserDefinedConstraintComponents.
   *
   * @param n an unsigned int representing the index of the
   * UserDefinedConstraintComponent to retrieve.
   *
   * @return the nth UserDefinedConstraintComponent in this
   * ListOfUserDefinedConstraintComponents or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see get(const std::string& sid)
   * @see getNumUserDefinedConstraintComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual UserDefinedConstraintComponent* get(unsigned int n);


  /**
   * Get an UserDefinedConstraintComponent from the
   * ListOfUserDefinedConstraintComponents.
   *
   * @param n an unsigned int representing the index of the
   * UserDefinedConstraintComponent to retrieve.
   *
   * @return the nth UserDefinedConstraintComponent in this
   * ListOfUserDefinedConstraintComponents or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see get(const std::string& sid)
   * @see getNumUserDefinedConstraintComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const UserDefinedConstraintComponent* get(unsigned int n) const;


  /**
   * Get an UserDefinedConstraintComponent from the
   * ListOfUserDefinedConstraintComponents based on its identifier.
   *
   * @param sid a string representing the identifier of the
   * UserDefinedConstraintComponent to retrieve.
   *
   * @return the UserDefinedConstraintComponent in this
   * ListOfUserDefinedConstraintComponents with the given @p sid or @c NULL if
   * no such UserDefinedConstraintComponent exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual UserDefinedConstraintComponent* get(const std::string& sid);


  /**
   * Get an UserDefinedConstraintComponent from the
   * ListOfUserDefinedConstraintComponents based on its identifier.
   *
   * @param sid a string representing the identifier of the
   * UserDefinedConstraintComponent to retrieve.
   *
   * @return the UserDefinedConstraintComponent in this
   * ListOfUserDefinedConstraintComponents with the given @p sid or @c NULL if
   * no such UserDefinedConstraintComponent exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const UserDefinedConstraintComponent* get(const std::string& sid)
    const;


  /**
   * Removes the nth UserDefinedConstraintComponent from this
   * ListOfUserDefinedConstraintComponents and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the
   * UserDefinedConstraintComponent to remove.
   *
   * @return a pointer to the nth UserDefinedConstraintComponent in this
   * ListOfUserDefinedConstraintComponents.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see remove(const std::string& sid)
   */
  virtual UserDefinedConstraintComponent* remove(unsigned int n);


  /**
   * Removes the UserDefinedConstraintComponent from this
   * ListOfUserDefinedConstraintComponents based on its identifier and returns
   * a pointer to it.
   *
   * @param sid a string representing the identifier of the
   * UserDefinedConstraintComponent to remove.
   *
   * @return the UserDefinedConstraintComponent in this
   * ListOfUserDefinedConstraintComponents based on the identifier or NULL if
   * no such UserDefinedConstraintComponent exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see remove(unsigned int n)
   */
  virtual UserDefinedConstraintComponent* remove(const std::string& sid);


  /**
   * Adds a copy of the given UserDefinedConstraintComponent to this
   * ListOfUserDefinedConstraintComponents.
   *
   * @param udcc the UserDefinedConstraintComponent object to add.
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
   * @see createUserDefinedConstraintComponent()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addUserDefinedConstraintComponent(const UserDefinedConstraintComponent*
    udcc);


  /**
   * Get the number of UserDefinedConstraintComponent objects in this
   * ListOfUserDefinedConstraintComponents.
   *
   * @return the number of UserDefinedConstraintComponent objects in this
   * ListOfUserDefinedConstraintComponents.
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumUserDefinedConstraintComponents() const;


  /**
   * Creates a new UserDefinedConstraintComponent object, adds it to this
   * ListOfUserDefinedConstraintComponents object and returns the
   * UserDefinedConstraintComponent object created.
   *
   * @return a new UserDefinedConstraintComponent object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  UserDefinedConstraintComponent* createUserDefinedConstraintComponent();


  /**
   * Get an UserDefinedConstraintComponent from the
   * ListOfUserDefinedConstraintComponents based on the Variable to which it
   * refers.
   *
   * @param sid a string representing the "variable" attribute of the
   * UserDefinedConstraintComponent object to retrieve.
   *
   * @return the first UserDefinedConstraintComponent in this
   * ListOfUserDefinedConstraintComponents based on the given variable
   * attribute or NULL if no such UserDefinedConstraintComponent exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const UserDefinedConstraintComponent* getByVariable(const std::string& sid)
    const;


  /**
   * Get an UserDefinedConstraintComponent from the
   * ListOfUserDefinedConstraintComponents based on the Variable to which it
   * refers.
   *
   * @param sid a string representing the "variable" attribute of the
   * UserDefinedConstraintComponent object to retrieve.
   *
   * @return the first UserDefinedConstraintComponent in this
   * ListOfUserDefinedConstraintComponents based on the given variable
   * attribute or NULL if no such UserDefinedConstraintComponent exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  UserDefinedConstraintComponent* getByVariable(const std::string& sid);


  /**
   * Returns the XML element name of this ListOfUserDefinedConstraintComponents
   * object.
   *
   * For ListOfUserDefinedConstraintComponents, the XML element name is always
   * @c "listOfUserDefinedConstraintComponents".
   *
   * @return the name of this element, i.e.
   * @c "listOfUserDefinedConstraintComponents".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this
   * ListOfUserDefinedConstraintComponents object.
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
   * ListOfUserDefinedConstraintComponents object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfUserDefinedConstraintComponents:
   * @sbmlconstant{SBML_FBC_USERDEFINEDCONSTRAINTCOMPONENT, SBMLFbcTypeCode_t}.
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
   * Creates a new UserDefinedConstraintComponent in this
   * ListOfUserDefinedConstraintComponents
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
 * Get an UserDefinedConstraintComponent_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * UserDefinedConstraintComponent_t to retrieve.
 *
 * @return the nth UserDefinedConstraintComponent_t in this ListOf_t or @c NULL
 * if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfUserDefinedConstraintComponents_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
ListOfUserDefinedConstraintComponents_getUserDefinedConstraintComponent(
                                                                        ListOf_t*
                                                                          lo,
                                                                        unsigned
                                                                          int n);


/**
 * Get an UserDefinedConstraintComponent_t from the ListOf_t based on its
 * identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * UserDefinedConstraintComponent_t to retrieve.
 *
 * @return the UserDefinedConstraintComponent_t in this ListOf_t with the given
 * @p sid or @c NULL if no such UserDefinedConstraintComponent_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfUserDefinedConstraintComponents_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
ListOfUserDefinedConstraintComponents_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth UserDefinedConstraintComponent_t from this ListOf_t and
 * returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * UserDefinedConstraintComponent_t to remove.
 *
 * @return a pointer to the nth UserDefinedConstraintComponent_t in this
 * ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfUserDefinedConstraintComponents_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
ListOfUserDefinedConstraintComponents_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the UserDefinedConstraintComponent_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * UserDefinedConstraintComponent_t to remove.
 *
 * @return the UserDefinedConstraintComponent_t in this ListOf_t based on the
 * identifier or NULL if no such UserDefinedConstraintComponent_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfUserDefinedConstraintComponents_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
ListOfUserDefinedConstraintComponents_removeById(ListOf_t* lo,
                                                 const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfUserDefinedConstraintComponents_H__ */


