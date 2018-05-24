/**
 * @file DistribListOfBase.h
 * @brief Definition of the DistribListOfBase class.
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
 * @class DistribListOfBase
 * @sbmlbrief{distrib} TODO:Definition of the DistribListOfBase class.
 */


#ifndef DistribListOfBase_H__
#define DistribListOfBase_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribListOfBase : public ListOf
{
protected:

public:

  /**
   * Creates a new DistribListOfBase using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribListOfBase.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribListOfBase.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribListOfBase.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribListOfBase(unsigned int level = DistribExtension::getDefaultLevel(),
    unsigned int version = DistribExtension::getDefaultVersion(),
    unsigned int pkgVersion = DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribListOfBase using the given DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribListOfBase(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribListOfBase.
   *
   * @param orig the DistribListOfBase instance to copy.
   */
  DistribListOfBase(const DistribListOfBase& orig);


  /**
   * Assignment operator for DistribListOfBase.
   *
   * @param rhs the DistribListOfBase object whose values are to be used as the
   * basis of the assignment.
   */
  DistribListOfBase& operator=(const DistribListOfBase& rhs);


  /**
   * Creates and returns a deep copy of this DistribListOfBase object.
   *
   * @return a (deep) copy of this DistribListOfBase object.
   */
  virtual DistribListOfBase* clone() const;


  /**
   * Destructor for DistribListOfBase.
   */
  virtual ~DistribListOfBase();


  /**
   * Returns the value of the "id" attribute of this DistribListOfBase.
   *
   * @return the value of the "id" attribute of this DistribListOfBase as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DistribListOfBase.
   *
   * @return the value of the "name" attribute of this DistribListOfBase as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribListOfBase's "id" attribute is
   * set.
   *
   * @return @c true if this DistribListOfBase's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribListOfBase's "name" attribute is
   * set.
   *
   * @return @c true if this DistribListOfBase's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this DistribListOfBase.
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
   * Sets the value of the "name" attribute of this DistribListOfBase.
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
   * Unsets the value of the "id" attribute of this DistribListOfBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DistribListOfBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the XML element name of this DistribListOfBase object.
   *
   * For DistribListOfBase, the XML element name is always @c "Base".
   *
   * @return the name of this element, i.e. @c "Base".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribListOfBase object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_Base, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribListOfBase object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribListOfBase have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribListOfBase object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribListOfBase have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribListOfBase object are:
   * @li "value"
   */
  virtual bool hasRequiredElements() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Gets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Gets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Gets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Gets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Predicate returning @c true if this DistribListOfBase's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribListOfBase's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Sets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Sets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Sets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Sets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Unsets the value of the "attributeName" attribute of this DistribListOfBase.
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
   * Returns the number of "elementName" in this DistribListOfBase.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribListOfBase.
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


protected:


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
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */

};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new DistribListOfBase_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribListOfBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribListOfBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribListOfBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
DistribListOfBase_t *
DistribListOfBase_create(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribListOfBase_t object.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @return a (deep) copy of this DistribListOfBase_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
DistribListOfBase_t*
DistribListOfBase_clone(const DistribListOfBase_t* dc);


/**
 * Frees this DistribListOfBase_t object.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
void
DistribListOfBase_free(DistribListOfBase_t* dc);


/**
 * Returns the value of the "id" attribute of this DistribListOfBase_t.
 *
 * @param dc the DistribListOfBase_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribListOfBase_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
char *
DistribListOfBase_getId(const DistribListOfBase_t * dc);


/**
 * Returns the value of the "name" attribute of this DistribListOfBase_t.
 *
 * @param dc the DistribListOfBase_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DistribListOfBase_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
char *
DistribListOfBase_getName(const DistribListOfBase_t * dc);


/**
 * Predicate returning @c 1 (true) if this DistribListOfBase_t's "id" attribute
 * is set.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @return @c 1 (true) if this DistribListOfBase_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
int
DistribListOfBase_isSetId(const DistribListOfBase_t * dc);


/**
 * Predicate returning @c 1 (true) if this DistribListOfBase_t's "name" attribute
 * is set.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @return @c 1 (true) if this DistribListOfBase_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
int
DistribListOfBase_isSetName(const DistribListOfBase_t * dc);


/**
 * Sets the value of the "id" attribute of this DistribListOfBase_t.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribListOfBase_unsetId().
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
int
DistribListOfBase_setId(DistribListOfBase_t * dc, const char * id);


/**
 * Sets the value of the "name" attribute of this DistribListOfBase_t.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribListOfBase_unsetName().
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
int
DistribListOfBase_setName(DistribListOfBase_t * dc, const char * name);


/**
 * Unsets the value of the "id" attribute of this DistribListOfBase_t.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
int
DistribListOfBase_unsetId(DistribListOfBase_t * dc);


/**
 * Unsets the value of the "name" attribute of this DistribListOfBase_t.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
int
DistribListOfBase_unsetName(DistribListOfBase_t * dc);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribListOfBase_t object have been set.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribListOfBase_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
int
DistribListOfBase_hasRequiredAttributes(const DistribListOfBase_t * dc);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribListOfBase_t object have been set.
 *
 * @param dc the DistribListOfBase_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribListOfBase_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the DistribListOfBase_t object are:
 * @li "value"
 *
 * @memberof DistribListOfBase_t
 */
LIBSBML_EXTERN
int
DistribListOfBase_hasRequiredElements(const DistribListOfBase_t * dc);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribListOfBase_H__ */


