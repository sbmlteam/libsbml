/**
 * @file DomainType.h
 * @brief Definition of the DomainType class.
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
 * @class DomainType
 * @sbmlbrief{spatial} TODO:Definition of the DomainType class.
 */


#ifndef DomainType_H__
#define DomainType_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DomainType : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  int mSpatialDimensions;
  bool mIsSetSpatialDimensions;

  /** @endcond */

public:

  /**
   * Creates a new DomainType using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this DomainType.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DomainType.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this DomainType.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DomainType(unsigned int level = SpatialExtension::getDefaultLevel(),
             unsigned int version = SpatialExtension::getDefaultVersion(),
             unsigned int pkgVersion =
               SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new DomainType using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DomainType(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for DomainType.
   *
   * @param orig the DomainType instance to copy.
   */
  DomainType(const DomainType& orig);


  /**
   * Assignment operator for DomainType.
   *
   * @param rhs the DomainType object whose values are to be used as the basis
   * of the assignment.
   */
  DomainType& operator=(const DomainType& rhs);


  /**
   * Creates and returns a deep copy of this DomainType object.
   *
   * @return a (deep) copy of this DomainType object.
   */
  virtual DomainType* clone() const;


  /**
   * Destructor for DomainType.
   */
  virtual ~DomainType();


  /**
   * Returns the value of the "id" attribute of this DomainType.
   *
   * @return the value of the "id" attribute of this DomainType as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DomainType.
   *
   * @return the value of the "name" attribute of this DomainType as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "spatialDimensions" attribute of this DomainType.
   *
   * @return the value of the "spatialDimensions" attribute of this DomainType
   * as a integer.
   */
  int getSpatialDimensions() const;


  /**
   * Predicate returning @c true if this DomainType's "id" attribute is set.
   *
   * @return @c true if this DomainType's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DomainType's "name" attribute is set.
   *
   * @return @c true if this DomainType's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this DomainType's "spatialDimensions"
   * attribute is set.
   *
   * @return @c true if this DomainType's "spatialDimensions" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetSpatialDimensions() const;


  /**
   * Sets the value of the "id" attribute of this DomainType.
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
   * Sets the value of the "name" attribute of this DomainType.
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
   * Sets the value of the "spatialDimensions" attribute of this DomainType.
   *
   * @param spatialDimensions int value of the "spatialDimensions" attribute to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSpatialDimensions(int spatialDimensions);


  /**
   * Unsets the value of the "id" attribute of this DomainType.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DomainType.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "spatialDimensions" attribute of this DomainType.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSpatialDimensions();


  /**
   * Returns the XML element name of this DomainType object.
   *
   * For DomainType, the XML element name is always @c "domainType".
   *
   * @return the name of this element, i.e. @c "domainType".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DomainType object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_DOMAINTYPE, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DomainType object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DomainType have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the DomainType object are:
   * @li "id"
   * @li "spatialDimensions"
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
   * Gets the value of the "attributeName" attribute of this DomainType.
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
   * Gets the value of the "attributeName" attribute of this DomainType.
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
   * Gets the value of the "attributeName" attribute of this DomainType.
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
   * Gets the value of the "attributeName" attribute of this DomainType.
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
   * Gets the value of the "attributeName" attribute of this DomainType.
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
   * Predicate returning @c true if this DomainType's attribute "attributeName"
   * is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DomainType's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DomainType.
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
   * Sets the value of the "attributeName" attribute of this DomainType.
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
   * Sets the value of the "attributeName" attribute of this DomainType.
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
   * Sets the value of the "attributeName" attribute of this DomainType.
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
   * Sets the value of the "attributeName" attribute of this DomainType.
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
   * Unsets the value of the "attributeName" attribute of this DomainType.
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
 * Creates a new DomainType_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this DomainType_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DomainType_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this DomainType_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
DomainType_t *
DomainType_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DomainType_t object.
 *
 * @param dt the DomainType_t structure.
 *
 * @return a (deep) copy of this DomainType_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
DomainType_t*
DomainType_clone(const DomainType_t* dt);


/**
 * Frees this DomainType_t object.
 *
 * @param dt the DomainType_t structure.
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
void
DomainType_free(DomainType_t* dt);


/**
 * Returns the value of the "id" attribute of this DomainType_t.
 *
 * @param dt the DomainType_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DomainType_t as a pointer to
 * a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
char *
DomainType_getId(const DomainType_t * dt);


/**
 * Returns the value of the "name" attribute of this DomainType_t.
 *
 * @param dt the DomainType_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DomainType_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
char *
DomainType_getName(const DomainType_t * dt);


/**
 * Returns the value of the "spatialDimensions" attribute of this DomainType_t.
 *
 * @param dt the DomainType_t structure whose spatialDimensions is sought.
 *
 * @return the value of the "spatialDimensions" attribute of this DomainType_t
 * as a integer.
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_getSpatialDimensions(const DomainType_t * dt);


/**
 * Predicate returning @c 1 (true) if this DomainType_t's "id" attribute is
 * set.
 *
 * @param dt the DomainType_t structure.
 *
 * @return @c 1 (true) if this DomainType_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_isSetId(const DomainType_t * dt);


/**
 * Predicate returning @c 1 (true) if this DomainType_t's "name" attribute is
 * set.
 *
 * @param dt the DomainType_t structure.
 *
 * @return @c 1 (true) if this DomainType_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_isSetName(const DomainType_t * dt);


/**
 * Predicate returning @c 1 (true) if this DomainType_t's "spatialDimensions"
 * attribute is set.
 *
 * @param dt the DomainType_t structure.
 *
 * @return @c 1 (true) if this DomainType_t's "spatialDimensions" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_isSetSpatialDimensions(const DomainType_t * dt);


/**
 * Sets the value of the "id" attribute of this DomainType_t.
 *
 * @param dt the DomainType_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DomainType_unsetId().
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_setId(DomainType_t * dt, const char * id);


/**
 * Sets the value of the "name" attribute of this DomainType_t.
 *
 * @param dt the DomainType_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DomainType_unsetName().
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_setName(DomainType_t * dt, const char * name);


/**
 * Sets the value of the "spatialDimensions" attribute of this DomainType_t.
 *
 * @param dt the DomainType_t structure.
 *
 * @param spatialDimensions int value of the "spatialDimensions" attribute to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_setSpatialDimensions(DomainType_t * dt, int spatialDimensions);


/**
 * Unsets the value of the "id" attribute of this DomainType_t.
 *
 * @param dt the DomainType_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_unsetId(DomainType_t * dt);


/**
 * Unsets the value of the "name" attribute of this DomainType_t.
 *
 * @param dt the DomainType_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_unsetName(DomainType_t * dt);


/**
 * Unsets the value of the "spatialDimensions" attribute of this DomainType_t.
 *
 * @param dt the DomainType_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_unsetSpatialDimensions(DomainType_t * dt);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DomainType_t object have been set.
 *
 * @param dt the DomainType_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DomainType_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the DomainType_t object are:
 * @li "id"
 * @li "spatialDimensions"
 *
 * @memberof DomainType_t
 */
LIBSBML_EXTERN
int
DomainType_hasRequiredAttributes(const DomainType_t * dt);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DomainType_H__ */


