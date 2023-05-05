/**
 * @file UserDefinedConstraint.h
 * @brief Definition of the UserDefinedConstraint class.
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
 * @class UserDefinedConstraint
 * @sbmlbrief{fbc} TODO:Definition of the UserDefinedConstraint class.
 */


#ifndef UserDefinedConstraint_H__
#define UserDefinedConstraint_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/sbml/ListOfUserDefinedConstraintComponents.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN UserDefinedConstraint : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mLowerBound;
  std::string mUpperBound;
  ListOfUserDefinedConstraintComponents mUserDefinedConstraintComponents;

  /** @endcond */

public:

  /**
   * Creates a new UserDefinedConstraint using the given SBML Level, Version
   * and &ldquo;fbc&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * UserDefinedConstraint.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * UserDefinedConstraint.
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this
   * UserDefinedConstraint.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UserDefinedConstraint(unsigned int level = FbcExtension::getDefaultLevel(),
                        unsigned int version =
                          FbcExtension::getDefaultVersion(),
                        unsigned int pkgVersion =
                          FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new UserDefinedConstraint using the given FbcPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param fbcns the FbcPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  UserDefinedConstraint(FbcPkgNamespaces *fbcns);


  /**
   * Copy constructor for UserDefinedConstraint.
   *
   * @param orig the UserDefinedConstraint instance to copy.
   */
  UserDefinedConstraint(const UserDefinedConstraint& orig);


  /**
   * Assignment operator for UserDefinedConstraint.
   *
   * @param rhs the UserDefinedConstraint object whose values are to be used as
   * the basis of the assignment.
   */
  UserDefinedConstraint& operator=(const UserDefinedConstraint& rhs);


  /**
   * Creates and returns a deep copy of this UserDefinedConstraint object.
   *
   * @return a (deep) copy of this UserDefinedConstraint object.
   */
  virtual UserDefinedConstraint* clone() const;


  /**
   * Destructor for UserDefinedConstraint.
   */
  virtual ~UserDefinedConstraint();


  /**
   * Returns the value of the "id" attribute of this UserDefinedConstraint.
   *
   * @return the value of the "id" attribute of this UserDefinedConstraint as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this UserDefinedConstraint.
   *
   * @return the value of the "name" attribute of this UserDefinedConstraint as
   * a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "lowerBound" attribute of this
   * UserDefinedConstraint.
   *
   * @return the value of the "lowerBound" attribute of this
   * UserDefinedConstraint as a string.
   */
  const std::string& getLowerBound() const;


  /**
   * Returns the value of the "upperBound" attribute of this
   * UserDefinedConstraint.
   *
   * @return the value of the "upperBound" attribute of this
   * UserDefinedConstraint as a string.
   */
  const std::string& getUpperBound() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraint's "id" attribute
   * is set.
   *
   * @return @c true if this UserDefinedConstraint's "id" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraint's "name"
   * attribute is set.
   *
   * @return @c true if this UserDefinedConstraint's "name" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraint's "lowerBound"
   * attribute is set.
   *
   * @return @c true if this UserDefinedConstraint's "lowerBound" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetLowerBound() const;


  /**
   * Predicate returning @c true if this UserDefinedConstraint's "upperBound"
   * attribute is set.
   *
   * @return @c true if this UserDefinedConstraint's "upperBound" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetUpperBound() const;


  /**
   * Sets the value of the "id" attribute of this UserDefinedConstraint.
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
   * Sets the value of the "name" attribute of this UserDefinedConstraint.
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
   * Sets the value of the "lowerBound" attribute of this
   * UserDefinedConstraint.
   *
   * @param lowerBound std::string& value of the "lowerBound" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setLowerBound(const std::string& lowerBound);


  /**
   * Sets the value of the "upperBound" attribute of this
   * UserDefinedConstraint.
   *
   * @param upperBound std::string& value of the "upperBound" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setUpperBound(const std::string& upperBound);


  /**
   * Unsets the value of the "id" attribute of this UserDefinedConstraint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this UserDefinedConstraint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "lowerBound" attribute of this
   * UserDefinedConstraint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLowerBound();


  /**
   * Unsets the value of the "upperBound" attribute of this
   * UserDefinedConstraint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetUpperBound();


  /**
   * Returns the ListOfUserDefinedConstraintComponents from this
   * UserDefinedConstraint.
   *
   * @return the ListOfUserDefinedConstraintComponents from this
   * UserDefinedConstraint.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see getUserDefinedConstraintComponent(const std::string& sid)
   * @see getUserDefinedConstraintComponent(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  const ListOfUserDefinedConstraintComponents*
    getListOfUserDefinedConstraintComponents() const;


  /**
   * Returns the ListOfUserDefinedConstraintComponents from this
   * UserDefinedConstraint.
   *
   * @return the ListOfUserDefinedConstraintComponents from this
   * UserDefinedConstraint.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see getUserDefinedConstraintComponent(const std::string& sid)
   * @see getUserDefinedConstraintComponent(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  ListOfUserDefinedConstraintComponents*
    getListOfUserDefinedConstraintComponents();


  /**
   * Get an UserDefinedConstraintComponent from the UserDefinedConstraint.
   *
   * @param n an unsigned int representing the index of the
   * UserDefinedConstraintComponent to retrieve.
   *
   * @return the nth UserDefinedConstraintComponent in the
   * ListOfUserDefinedConstraintComponents within this UserDefinedConstraint or
   * @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see getUserDefinedConstraintComponent(const std::string& sid)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  UserDefinedConstraintComponent* getUserDefinedConstraintComponent(unsigned
    int n);


  /**
   * Get an UserDefinedConstraintComponent from the UserDefinedConstraint.
   *
   * @param n an unsigned int representing the index of the
   * UserDefinedConstraintComponent to retrieve.
   *
   * @return the nth UserDefinedConstraintComponent in the
   * ListOfUserDefinedConstraintComponents within this UserDefinedConstraint or
   * @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see getUserDefinedConstraintComponent(const std::string& sid)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  const UserDefinedConstraintComponent*
    getUserDefinedConstraintComponent(unsigned int n) const;


  /**
   * Get an UserDefinedConstraintComponent from the UserDefinedConstraint based
   * on its identifier.
   *
   * @param sid a string representing the identifier of the
   * UserDefinedConstraintComponent to retrieve.
   *
   * @return the UserDefinedConstraintComponent in the
   * ListOfUserDefinedConstraintComponents within this UserDefinedConstraint
   * with the given @p sid or @c NULL if no such UserDefinedConstraintComponent
   * exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see getUserDefinedConstraintComponent(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  UserDefinedConstraintComponent* getUserDefinedConstraintComponent(const
    std::string& sid);


  /**
   * Get an UserDefinedConstraintComponent from the UserDefinedConstraint based
   * on its identifier.
   *
   * @param sid a string representing the identifier of the
   * UserDefinedConstraintComponent to retrieve.
   *
   * @return the UserDefinedConstraintComponent in the
   * ListOfUserDefinedConstraintComponents within this UserDefinedConstraint
   * with the given @p sid or @c NULL if no such UserDefinedConstraintComponent
   * exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see getUserDefinedConstraintComponent(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  const UserDefinedConstraintComponent* getUserDefinedConstraintComponent(const
    std::string& sid) const;


  /**
   * Get an UserDefinedConstraintComponent from the UserDefinedConstraint based
   * on the Variable to which it refers.
   *
   * @param sid a string representing the "variable" attribute of the
   * UserDefinedConstraintComponent object to retrieve.
   *
   * @return the first UserDefinedConstraintComponent in this
   * UserDefinedConstraint based on the given variable attribute or NULL if no
   * such UserDefinedConstraintComponent exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const UserDefinedConstraintComponent*
    getUserDefinedConstraintComponentByVariable(const std::string& sid) const;


  /**
   * Get an UserDefinedConstraintComponent from the UserDefinedConstraint based
   * on the Variable to which it refers.
   *
   * @param sid a string representing the "variable" attribute of the
   * UserDefinedConstraintComponent object to retrieve.
   *
   * @return the first UserDefinedConstraintComponent in this
   * UserDefinedConstraint based on the given variable attribute or NULL if no
   * such UserDefinedConstraintComponent exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  UserDefinedConstraintComponent*
    getUserDefinedConstraintComponentByVariable(const std::string& sid);


  /**
   * Adds a copy of the given UserDefinedConstraintComponent to this
   * UserDefinedConstraint.
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
   * @see getUserDefinedConstraintComponent(const std::string& sid)
   * @see getUserDefinedConstraintComponent(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  int addUserDefinedConstraintComponent(const UserDefinedConstraintComponent*
    udcc);


  /**
   * Get the number of UserDefinedConstraintComponent objects in this
   * UserDefinedConstraint.
   *
   * @return the number of UserDefinedConstraintComponent objects in this
   * UserDefinedConstraint.
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see getUserDefinedConstraintComponent(const std::string& sid)
   * @see getUserDefinedConstraintComponent(unsigned int n)
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  unsigned int getNumUserDefinedConstraintComponents() const;


  /**
   * Creates a new UserDefinedConstraintComponent object, adds it to this
   * UserDefinedConstraint object and returns the
   * UserDefinedConstraintComponent object created.
   *
   * @return a new UserDefinedConstraintComponent object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see getUserDefinedConstraintComponent(const std::string& sid)
   * @see getUserDefinedConstraintComponent(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  UserDefinedConstraintComponent* createUserDefinedConstraintComponent();


  /**
   * Removes the nth UserDefinedConstraintComponent from this
   * UserDefinedConstraint and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the
   * UserDefinedConstraintComponent to remove.
   *
   * @return a pointer to the nth UserDefinedConstraintComponent in this
   * UserDefinedConstraint.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see getUserDefinedConstraintComponent(const std::string& sid)
   * @see getUserDefinedConstraintComponent(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(const std::string& sid)
   */
  UserDefinedConstraintComponent* removeUserDefinedConstraintComponent(unsigned
    int n);


  /**
   * Removes the UserDefinedConstraintComponent from this UserDefinedConstraint
   * based on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the
   * UserDefinedConstraintComponent to remove.
   *
   * @return the UserDefinedConstraintComponent in this UserDefinedConstraint
   * based on the identifier or NULL if no such UserDefinedConstraintComponent
   * exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUserDefinedConstraintComponent(const
   * UserDefinedConstraintComponent* object)
   * @see createUserDefinedConstraintComponent()
   * @see getUserDefinedConstraintComponent(const std::string& sid)
   * @see getUserDefinedConstraintComponent(unsigned int n)
   * @see getNumUserDefinedConstraintComponents()
   * @see removeUserDefinedConstraintComponent(unsigned int n)
   */
  UserDefinedConstraintComponent* removeUserDefinedConstraintComponent(const
    std::string& sid);


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this UserDefinedConstraint object.
   *
   * For UserDefinedConstraint, the XML element name is always
   * @c "userDefinedConstraint".
   *
   * @return the name of this element, i.e. @c "userDefinedConstraint".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this UserDefinedConstraint object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_FBC_USERDEFINEDCONSTRAINT, SBMLFbcTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * UserDefinedConstraint object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * UserDefinedConstraint have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the UserDefinedConstraint object are:
   * @li "lowerBound"
   * @li "upperBound"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * UserDefinedConstraint object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * UserDefinedConstraint have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the UserDefinedConstraint object are:
   * @li "userDefinedConstraintComponent"
   */
  virtual bool hasRequiredElements() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Write any contained elements
   */
  virtual void writeElements(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument
   */
  virtual void setSBMLDocument(SBMLDocument* d);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements
   */
  virtual void connectToChild();

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Updates the namespaces when setLevelVersion is used
   */
  virtual void updateSBMLNamespace(const std::string& package,
                                   unsigned int level,
                                   unsigned int version);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, bool& value)
    const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           double& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           unsigned int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           std::string& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this UserDefinedConstraint's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this UserDefinedConstraint's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, bool value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, double value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           unsigned int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           const std::string& value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this
   * UserDefinedConstraint.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this
   * UserDefinedConstraint.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this UserDefinedConstraint.
   *
   * @param elementName, the name of the element to create.
   *
   * @param element, pointer to the element to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int addChildObject(const std::string& elementName,
                             const SBase* element);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Removes and returns the new "elementName" object with the given id in this
   * UserDefinedConstraint.
   *
   * @param elementName, the name of the element to remove.
   *
   * @param id, the id of the element to remove.
   *
   * @return pointer to the element removed.
   */
  virtual SBase* removeChildObject(const std::string& elementName,
                                   const std::string& id);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this UserDefinedConstraint.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this UserDefinedConstraint.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int the index of the object to retrieve.
   *
   * @return pointer to the object.
   */
  virtual SBase* getObject(const std::string& elementName, unsigned int index);

  /** @endcond */




  #endif /* !SWIG */


  /**
   * Returns the first child element that has the given @p id in the model-wide
   * SId namespace, or @c NULL if no such object is found.
   *
   * @param id a string representing the id attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p id. If no such
   * object is found, this method returns @c NULL.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid. If no
   * such object is found this method returns @c NULL.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @param filter an ElementFilter that may impose restrictions on the objects
   * to be retrieved.
   *
   * @return a List pointer of pointers to all SBase child objects with any
   * restriction imposed.
   */
  virtual List* getAllElements(ElementFilter * filter = NULL);


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new object from the next XMLToken on the XMLInputStream
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


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new UserDefinedConstraint_t using the given SBML Level, Version
 * and &ldquo;fbc&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * UserDefinedConstraint_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * UserDefinedConstraint_t.
 *
 * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this
 * UserDefinedConstraint_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
UserDefinedConstraint_t *
UserDefinedConstraint_create(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this UserDefinedConstraint_t object.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @return a (deep) copy of this UserDefinedConstraint_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
UserDefinedConstraint_clone(const UserDefinedConstraint_t* udc);


/**
 * Frees this UserDefinedConstraint_t object.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
void
UserDefinedConstraint_free(UserDefinedConstraint_t* udc);


/**
 * Returns the value of the "id" attribute of this UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this UserDefinedConstraint_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraint_getId(const UserDefinedConstraint_t * udc);


/**
 * Returns the value of the "name" attribute of this UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this UserDefinedConstraint_t as
 * a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraint_getName(const UserDefinedConstraint_t * udc);


/**
 * Returns the value of the "lowerBound" attribute of this
 * UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure whose lowerBound is sought.
 *
 * @return the value of the "lowerBound" attribute of this
 * UserDefinedConstraint_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraint_getLowerBound(const UserDefinedConstraint_t * udc);


/**
 * Returns the value of the "upperBound" attribute of this
 * UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure whose upperBound is sought.
 *
 * @return the value of the "upperBound" attribute of this
 * UserDefinedConstraint_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
char *
UserDefinedConstraint_getUpperBound(const UserDefinedConstraint_t * udc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraint_t's "id"
 * attribute is set.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraint_t's "id" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_isSetId(const UserDefinedConstraint_t * udc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraint_t's "name"
 * attribute is set.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraint_t's "name" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_isSetName(const UserDefinedConstraint_t * udc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraint_t's
 * "lowerBound" attribute is set.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraint_t's "lowerBound" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_isSetLowerBound(const UserDefinedConstraint_t * udc);


/**
 * Predicate returning @c 1 (true) if this UserDefinedConstraint_t's
 * "upperBound" attribute is set.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @return @c 1 (true) if this UserDefinedConstraint_t's "upperBound" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_isSetUpperBound(const UserDefinedConstraint_t * udc);


/**
 * Sets the value of the "id" attribute of this UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling UserDefinedConstraint_unsetId().
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_setId(UserDefinedConstraint_t * udc, const char * id);


/**
 * Sets the value of the "name" attribute of this UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling UserDefinedConstraint_unsetName().
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_setName(UserDefinedConstraint_t * udc,
                              const char * name);


/**
 * Sets the value of the "lowerBound" attribute of this
 * UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @param lowerBound const char * value of the "lowerBound" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_setLowerBound(UserDefinedConstraint_t * udc,
                                    const char * lowerBound);


/**
 * Sets the value of the "upperBound" attribute of this
 * UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @param upperBound const char * value of the "upperBound" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_setUpperBound(UserDefinedConstraint_t * udc,
                                    const char * upperBound);


/**
 * Unsets the value of the "id" attribute of this UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_unsetId(UserDefinedConstraint_t * udc);


/**
 * Unsets the value of the "name" attribute of this UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_unsetName(UserDefinedConstraint_t * udc);


/**
 * Unsets the value of the "lowerBound" attribute of this
 * UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_unsetLowerBound(UserDefinedConstraint_t * udc);


/**
 * Unsets the value of the "upperBound" attribute of this
 * UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_unsetUpperBound(UserDefinedConstraint_t * udc);


/**
 * Returns a ListOf_t * containing UserDefinedConstraintComponent_t objects
 * from this UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure whose
 * ListOfUserDefinedConstraintComponents is sought.
 *
 * @return the ListOfUserDefinedConstraintComponents from this
 * UserDefinedConstraint_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see UserDefinedConstraint_addUserDefinedConstraintComponent()
 * @see UserDefinedConstraint_createUserDefinedConstraintComponent()
 * @see UserDefinedConstraint_getUserDefinedConstraintComponentById()
 * @see UserDefinedConstraint_getUserDefinedConstraintComponent()
 * @see UserDefinedConstraint_getNumUserDefinedConstraintComponents()
 * @see UserDefinedConstraint_removeUserDefinedConstraintComponentById()
 * @see UserDefinedConstraint_removeUserDefinedConstraintComponent()
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
ListOf_t*
UserDefinedConstraint_getListOfUserDefinedConstraintComponents(UserDefinedConstraint_t*
  udc);


/**
 * Get an UserDefinedConstraintComponent_t from the UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * UserDefinedConstraintComponent_t to retrieve.
 *
 * @return the nth UserDefinedConstraintComponent_t in the
 * ListOfUserDefinedConstraintComponents within this UserDefinedConstraint or
 * @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_getUserDefinedConstraintComponent(
                                                        UserDefinedConstraint_t*
                                                          udc,
                                                        unsigned int n);


/**
 * Get an UserDefinedConstraintComponent_t from the UserDefinedConstraint_t
 * based on its identifier.
 *
 * @param udc the UserDefinedConstraint_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * UserDefinedConstraintComponent_t to retrieve.
 *
 * @return the UserDefinedConstraintComponent_t in the
 * ListOfUserDefinedConstraintComponents within this UserDefinedConstraint with
 * the given @p sid or @c NULL if no such UserDefinedConstraintComponent_t
 * exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_getUserDefinedConstraintComponentById(
                                                            UserDefinedConstraint_t*
                                                              udc,
                                                            const char *sid);


/**
 * Get an UserDefinedConstraintComponent_t from the UserDefinedConstraint_t
 * based on the Variable to which it refers.
 *
 * @param udc the UserDefinedConstraint_t structure to search.
 *
 * @param sid a string representing the "variable" attribute of the
 * UserDefinedConstraintComponent_t object to retrieve.
 *
 * @return the first UserDefinedConstraintComponent_t in this
 * UserDefinedConstraint_t based on the given variable attribute or NULL if no
 * such UserDefinedConstraintComponent_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_getUserDefinedConstraintComponentByVariable(
                                                                  UserDefinedConstraint_t*
                                                                    udc,
                                                                  const char
                                                                    *sid);


/**
 * Adds a copy of the given UserDefinedConstraintComponent_t to this
 * UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure to which the
 * UserDefinedConstraintComponent_t should be added.
 *
 * @param udcc the UserDefinedConstraintComponent_t object to add.
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
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_addUserDefinedConstraintComponent(
                                                        UserDefinedConstraint_t*
                                                          udc,
                                                        const UserDefinedConstraintComponent_t*
                                                          udcc);


/**
 * Get the number of UserDefinedConstraintComponent_t objects in this
 * UserDefinedConstraint_t.
 *
 * @param udc the UserDefinedConstraint_t structure to query.
 *
 * @return the number of UserDefinedConstraintComponent_t objects in this
 * UserDefinedConstraint_t.
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
unsigned int
UserDefinedConstraint_getNumUserDefinedConstraintComponents(UserDefinedConstraint_t*
  udc);


/**
 * Creates a new UserDefinedConstraintComponent_t object, adds it to this
 * UserDefinedConstraint_t object and returns the
 * UserDefinedConstraintComponent_t object created.
 *
 * @param udc the UserDefinedConstraint_t structure to which the
 * UserDefinedConstraintComponent_t should be added.
 *
 * @return a new UserDefinedConstraintComponent_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_createUserDefinedConstraintComponent(UserDefinedConstraint_t*
  udc);


/**
 * Removes the nth UserDefinedConstraintComponent_t from this
 * UserDefinedConstraint_t and returns a pointer to it.
 *
 * @param udc the UserDefinedConstraint_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * UserDefinedConstraintComponent_t to remove.
 *
 * @return a pointer to the nth UserDefinedConstraintComponent_t in this
 * UserDefinedConstraint_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_removeUserDefinedConstraintComponent(
                                                           UserDefinedConstraint_t*
                                                             udc,
                                                           unsigned int n);


/**
 * Removes the UserDefinedConstraintComponent_t from this
 * UserDefinedConstraint_t based on its identifier and returns a pointer to it.
 *
 * @param udc the UserDefinedConstraint_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * UserDefinedConstraintComponent_t to remove.
 *
 * @return the UserDefinedConstraintComponent_t in this UserDefinedConstraint_t
 * based on the identifier or NULL if no such UserDefinedConstraintComponent_t
 * exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
UserDefinedConstraint_removeUserDefinedConstraintComponentById(
                                                               UserDefinedConstraint_t*
                                                                 udc,
                                                               const char*
                                                                 sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * UserDefinedConstraint_t object have been set.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * UserDefinedConstraint_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the UserDefinedConstraint_t object are:
 * @li "lowerBound"
 * @li "upperBound"
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_hasRequiredAttributes(const UserDefinedConstraint_t *
  udc);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * UserDefinedConstraint_t object have been set.
 *
 * @param udc the UserDefinedConstraint_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * UserDefinedConstraint_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the UserDefinedConstraint_t object are:
 * @li "userDefinedConstraintComponent"
 *
 * @memberof UserDefinedConstraint_t
 */
LIBSBML_EXTERN
int
UserDefinedConstraint_hasRequiredElements(const UserDefinedConstraint_t * udc);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !UserDefinedConstraint_H__ */


