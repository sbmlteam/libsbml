/**
 * @file DistribBase.h
 * @brief Definition of the DistribBase class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
 * @class DistribBase
 * @sbmlbrief{distrib} TODO:Definition of the DistribBase class.
 */


#ifndef DistribBase_H__
#define DistribBase_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class Uncertainty;

class LIBSBML_EXTERN DistribBase : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new DistribBase using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribBase.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribBase.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribBase.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribBase(unsigned int level = DistribExtension::getDefaultLevel(),
              unsigned int version = DistribExtension::getDefaultVersion(),
              unsigned int pkgVersion =
                DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribBase using the given DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribBase(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribBase.
   *
   * @param orig the DistribBase instance to copy.
   */
  DistribBase(const DistribBase& orig);


  /**
   * Assignment operator for DistribBase.
   *
   * @param rhs the DistribBase object whose values are to be used as the basis
   * of the assignment.
   */
  DistribBase& operator=(const DistribBase& rhs);


  /**
   * Creates and returns a deep copy of this DistribBase object.
   *
   * @return a (deep) copy of this DistribBase object.
   */
  virtual DistribBase* clone() const;


  /**
   * Destructor for DistribBase.
   */
  virtual ~DistribBase();


  /**
   * Returns the value of the "id" attribute of this DistribBase.
   *
   * @return the value of the "id" attribute of this DistribBase as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DistribBase.
   *
   * @return the value of the "name" attribute of this DistribBase as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribBase's "id" attribute is set.
   *
   * @return @c true if this DistribBase's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribBase's "name" attribute is set.
   *
   * @return @c true if this DistribBase's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this DistribBase.
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
   * Sets the value of the "name" attribute of this DistribBase.
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
   * Unsets the value of the "id" attribute of this DistribBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DistribBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * Uncertainty
   *
   * @return @c true if this abstract "DistribBase" is of type Uncertainty,
   * @c false otherwise
   */
  virtual bool isUncertainty() const;


  /**
   * Returns the XML element name of this DistribBase object.
   *
   * For DistribBase, the XML element name is always @c "distribBase".
   *
   * @return the name of this element, i.e. @c "distribBase".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this DistribBase object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this DistribBase object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_DISTRIBBASE, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribBase object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribBase have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;



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
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Predicate returning @c true if this DistribBase's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribBase's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Unsets the value of the "attributeName" attribute of this DistribBase.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

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
 * Creates a new Uncertainty using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
Uncertainty_t *
DistribBase_createUncertainty(unsigned int level,
                              unsigned int version,
                              unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribBase_t object.
 *
 * @param db the DistribBase_t structure.
 *
 * @return a (deep) copy of this DistribBase_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t*
DistribBase_clone(const DistribBase_t* db);


/**
 * Frees this DistribBase_t object.
 *
 * @param db the DistribBase_t structure.
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
void
DistribBase_free(DistribBase_t* db);


/**
 * Returns the value of the "id" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribBase_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
char *
DistribBase_getId(const DistribBase_t * db);


/**
 * Returns the value of the "name" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DistribBase_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
char *
DistribBase_getName(const DistribBase_t * db);


/**
 * Predicate returning @c 1 (true) if this DistribBase_t's "id" attribute is
 * set.
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 (true) if this DistribBase_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isSetId(const DistribBase_t * db);


/**
 * Predicate returning @c 1 (true) if this DistribBase_t's "name" attribute is
 * set.
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 (true) if this DistribBase_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isSetName(const DistribBase_t * db);


/**
 * Sets the value of the "id" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribBase_unsetId().
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_setId(DistribBase_t * db, const char * id);


/**
 * Sets the value of the "name" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribBase_unsetName().
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_setName(DistribBase_t * db, const char * name);


/**
 * Unsets the value of the "id" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_unsetId(DistribBase_t * db);


/**
 * Unsets the value of the "name" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_unsetName(DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type Uncertainty_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type Uncertainty_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isUncertainty(const DistribBase_t * db);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribBase_t object have been set.
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribBase_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_hasRequiredAttributes(const DistribBase_t * db);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribBase_H__ */


