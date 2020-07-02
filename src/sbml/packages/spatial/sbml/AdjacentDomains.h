/**
 * @file AdjacentDomains.h
 * @brief Definition of the AdjacentDomains class.
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
 * @class AdjacentDomains
 * @sbmlbrief{spatial} TODO:Definition of the AdjacentDomains class.
 */


#ifndef AdjacentDomains_H__
#define AdjacentDomains_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN AdjacentDomains : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mDomain1;
  std::string mDomain2;

  /** @endcond */

public:

  /**
   * Creates a new AdjacentDomains using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * AdjacentDomains.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * AdjacentDomains.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this AdjacentDomains.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  AdjacentDomains(unsigned int level = SpatialExtension::getDefaultLevel(),
                  unsigned int version = SpatialExtension::getDefaultVersion(),
                  unsigned int pkgVersion =
                    SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AdjacentDomains using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  AdjacentDomains(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for AdjacentDomains.
   *
   * @param orig the AdjacentDomains instance to copy.
   */
  AdjacentDomains(const AdjacentDomains& orig);


  /**
   * Assignment operator for AdjacentDomains.
   *
   * @param rhs the AdjacentDomains object whose values are to be used as the
   * basis of the assignment.
   */
  AdjacentDomains& operator=(const AdjacentDomains& rhs);


  /**
   * Creates and returns a deep copy of this AdjacentDomains object.
   *
   * @return a (deep) copy of this AdjacentDomains object.
   */
  virtual AdjacentDomains* clone() const;


  /**
   * Destructor for AdjacentDomains.
   */
  virtual ~AdjacentDomains();


  /**
   * Returns the value of the "id" attribute of this AdjacentDomains.
   *
   * @return the value of the "id" attribute of this AdjacentDomains as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this AdjacentDomains.
   *
   * @return the value of the "name" attribute of this AdjacentDomains as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "domain1" attribute of this AdjacentDomains.
   *
   * @return the value of the "domain1" attribute of this AdjacentDomains as a
   * string.
   */
  const std::string& getDomain1() const;


  /**
   * Returns the value of the "domain2" attribute of this AdjacentDomains.
   *
   * @return the value of the "domain2" attribute of this AdjacentDomains as a
   * string.
   */
  const std::string& getDomain2() const;


  /**
   * Predicate returning @c true if this AdjacentDomains's "id" attribute is
   * set.
   *
   * @return @c true if this AdjacentDomains's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this AdjacentDomains's "name" attribute is
   * set.
   *
   * @return @c true if this AdjacentDomains's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this AdjacentDomains's "domain1" attribute
   * is set.
   *
   * @return @c true if this AdjacentDomains's "domain1" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetDomain1() const;


  /**
   * Predicate returning @c true if this AdjacentDomains's "domain2" attribute
   * is set.
   *
   * @return @c true if this AdjacentDomains's "domain2" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetDomain2() const;


  /**
   * Sets the value of the "id" attribute of this AdjacentDomains.
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
   * Sets the value of the "name" attribute of this AdjacentDomains.
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
   * Sets the value of the "domain1" attribute of this AdjacentDomains.
   *
   * @param domain1 std::string& value of the "domain1" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDomain1(const std::string& domain1);


  /**
   * Sets the value of the "domain2" attribute of this AdjacentDomains.
   *
   * @param domain2 std::string& value of the "domain2" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDomain2(const std::string& domain2);


  /**
   * Unsets the value of the "id" attribute of this AdjacentDomains.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this AdjacentDomains.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "domain1" attribute of this AdjacentDomains.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDomain1();


  /**
   * Unsets the value of the "domain2" attribute of this AdjacentDomains.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDomain2();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this AdjacentDomains object.
   *
   * For AdjacentDomains, the XML element name is always @c "adjacentDomains".
   *
   * @return the name of this element, i.e. @c "adjacentDomains".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this AdjacentDomains object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_ADJACENTDOMAINS, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * AdjacentDomains object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * AdjacentDomains have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the AdjacentDomains object are:
   * @li "id"
   * @li "domain1"
   * @li "domain2"
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
   * Gets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Gets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Gets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Gets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Gets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Predicate returning @c true if this AdjacentDomains's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this AdjacentDomains's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Sets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Sets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Sets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Sets the value of the "attributeName" attribute of this AdjacentDomains.
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
   * Unsets the value of the "attributeName" attribute of this AdjacentDomains.
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
 * Creates a new AdjacentDomains_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * AdjacentDomains_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * AdjacentDomains_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this AdjacentDomains_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
AdjacentDomains_t *
AdjacentDomains_create(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this AdjacentDomains_t object.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return a (deep) copy of this AdjacentDomains_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
AdjacentDomains_t*
AdjacentDomains_clone(const AdjacentDomains_t* ad);


/**
 * Frees this AdjacentDomains_t object.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
void
AdjacentDomains_free(AdjacentDomains_t* ad);


/**
 * Returns the value of the "id" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this AdjacentDomains_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
char *
AdjacentDomains_getId(const AdjacentDomains_t * ad);


/**
 * Returns the value of the "name" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this AdjacentDomains_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
char *
AdjacentDomains_getName(const AdjacentDomains_t * ad);


/**
 * Returns the value of the "domain1" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure whose domain1 is sought.
 *
 * @return the value of the "domain1" attribute of this AdjacentDomains_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
char *
AdjacentDomains_getDomain1(const AdjacentDomains_t * ad);


/**
 * Returns the value of the "domain2" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure whose domain2 is sought.
 *
 * @return the value of the "domain2" attribute of this AdjacentDomains_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
char *
AdjacentDomains_getDomain2(const AdjacentDomains_t * ad);


/**
 * Predicate returning @c 1 (true) if this AdjacentDomains_t's "id" attribute
 * is set.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return @c 1 (true) if this AdjacentDomains_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetId(const AdjacentDomains_t * ad);


/**
 * Predicate returning @c 1 (true) if this AdjacentDomains_t's "name" attribute
 * is set.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return @c 1 (true) if this AdjacentDomains_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetName(const AdjacentDomains_t * ad);


/**
 * Predicate returning @c 1 (true) if this AdjacentDomains_t's "domain1"
 * attribute is set.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return @c 1 (true) if this AdjacentDomains_t's "domain1" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain1(const AdjacentDomains_t * ad);


/**
 * Predicate returning @c 1 (true) if this AdjacentDomains_t's "domain2"
 * attribute is set.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return @c 1 (true) if this AdjacentDomains_t's "domain2" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain2(const AdjacentDomains_t * ad);


/**
 * Sets the value of the "id" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling AdjacentDomains_unsetId().
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_setId(AdjacentDomains_t * ad, const char * id);


/**
 * Sets the value of the "name" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling AdjacentDomains_unsetName().
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_setName(AdjacentDomains_t * ad, const char * name);


/**
 * Sets the value of the "domain1" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @param domain1 const char * value of the "domain1" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_setDomain1(AdjacentDomains_t * ad, const char * domain1);


/**
 * Sets the value of the "domain2" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @param domain2 const char * value of the "domain2" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_setDomain2(AdjacentDomains_t * ad, const char * domain2);


/**
 * Unsets the value of the "id" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetId(AdjacentDomains_t * ad);


/**
 * Unsets the value of the "name" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetName(AdjacentDomains_t * ad);


/**
 * Unsets the value of the "domain1" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain1(AdjacentDomains_t * ad);


/**
 * Unsets the value of the "domain2" attribute of this AdjacentDomains_t.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain2(AdjacentDomains_t * ad);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * AdjacentDomains_t object have been set.
 *
 * @param ad the AdjacentDomains_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * AdjacentDomains_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the AdjacentDomains_t object are:
 * @li "id"
 * @li "domain1"
 * @li "domain2"
 *
 * @memberof AdjacentDomains_t
 */
LIBSBML_EXTERN
int
AdjacentDomains_hasRequiredAttributes(const AdjacentDomains_t * ad);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !AdjacentDomains_H__ */


