/**
 * @file CSGRotation.h
 * @brief Definition of the CSGRotation class.
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
 * @class CSGRotation
 * @sbmlbrief{spatial} TODO:Definition of the CSGRotation class.
 */


#ifndef CSGRotation_H__
#define CSGRotation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/CSGTransformation.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGRotation : public CSGTransformation
{
protected:

  /** @cond doxygenLibsbmlInternal */

  double mRotateX;
  bool mIsSetRotateX;
  double mRotateY;
  bool mIsSetRotateY;
  double mRotateZ;
  bool mIsSetRotateZ;
  double mRotateAngleInRadians;
  bool mIsSetRotateAngleInRadians;

  /** @endcond */

public:

  /**
   * Creates a new CSGRotation using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CSGRotation.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGRotation.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGRotation.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGRotation(unsigned int level = SpatialExtension::getDefaultLevel(),
              unsigned int version = SpatialExtension::getDefaultVersion(),
              unsigned int pkgVersion =
                SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGRotation using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGRotation(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGRotation.
   *
   * @param orig the CSGRotation instance to copy.
   */
  CSGRotation(const CSGRotation& orig);


  /**
   * Assignment operator for CSGRotation.
   *
   * @param rhs the CSGRotation object whose values are to be used as the basis
   * of the assignment.
   */
  CSGRotation& operator=(const CSGRotation& rhs);


  /**
   * Creates and returns a deep copy of this CSGRotation object.
   *
   * @return a (deep) copy of this CSGRotation object.
   */
  virtual CSGRotation* clone() const;


  /**
   * Destructor for CSGRotation.
   */
  virtual ~CSGRotation();


  /**
   * Returns the value of the "rotateX" attribute of this CSGRotation.
   *
   * @return the value of the "rotateX" attribute of this CSGRotation as a
   * double.
   */
  double getRotateX() const;


  /**
   * Returns the value of the "rotateY" attribute of this CSGRotation.
   *
   * @return the value of the "rotateY" attribute of this CSGRotation as a
   * double.
   */
  double getRotateY() const;


  /**
   * Returns the value of the "rotateZ" attribute of this CSGRotation.
   *
   * @return the value of the "rotateZ" attribute of this CSGRotation as a
   * double.
   */
  double getRotateZ() const;


  /**
   * Returns the value of the "rotateAngleInRadians" attribute of this
   * CSGRotation.
   *
   * @return the value of the "rotateAngleInRadians" attribute of this
   * CSGRotation as a double.
   */
  double getRotateAngleInRadians() const;


  /**
   * Predicate returning @c true if this CSGRotation's "rotateX" attribute is
   * set.
   *
   * @return @c true if this CSGRotation's "rotateX" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetRotateX() const;


  /**
   * Predicate returning @c true if this CSGRotation's "rotateY" attribute is
   * set.
   *
   * @return @c true if this CSGRotation's "rotateY" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetRotateY() const;


  /**
   * Predicate returning @c true if this CSGRotation's "rotateZ" attribute is
   * set.
   *
   * @return @c true if this CSGRotation's "rotateZ" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetRotateZ() const;


  /**
   * Predicate returning @c true if this CSGRotation's "rotateAngleInRadians"
   * attribute is set.
   *
   * @return @c true if this CSGRotation's "rotateAngleInRadians" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetRotateAngleInRadians() const;


  /**
   * Sets the value of the "rotateX" attribute of this CSGRotation.
   *
   * @param rotateX double value of the "rotateX" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setRotateX(double rotateX);


  /**
   * Sets the value of the "rotateY" attribute of this CSGRotation.
   *
   * @param rotateY double value of the "rotateY" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setRotateY(double rotateY);


  /**
   * Sets the value of the "rotateZ" attribute of this CSGRotation.
   *
   * @param rotateZ double value of the "rotateZ" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setRotateZ(double rotateZ);


  /**
   * Sets the value of the "rotateAngleInRadians" attribute of this
   * CSGRotation.
   *
   * @param rotateAngleInRadians double value of the "rotateAngleInRadians"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setRotateAngleInRadians(double rotateAngleInRadians);


  /**
   * Unsets the value of the "rotateX" attribute of this CSGRotation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRotateX();


  /**
   * Unsets the value of the "rotateY" attribute of this CSGRotation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRotateY();


  /**
   * Unsets the value of the "rotateZ" attribute of this CSGRotation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRotateZ();


  /**
   * Unsets the value of the "rotateAngleInRadians" attribute of this
   * CSGRotation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRotateAngleInRadians();


  /**
   * Returns the XML element name of this CSGRotation object.
   *
   * For CSGRotation, the XML element name is always @c "csgRotation".
   *
   * @return the name of this element, i.e. @c "csgRotation".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CSGRotation object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_CSGROTATION, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGRotation object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGRotation have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CSGRotation object are:
   * @li "rotateX"
   * @li "rotateAngleInRadians"
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
   * Gets the value of the "attributeName" attribute of this CSGRotation.
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
   * Gets the value of the "attributeName" attribute of this CSGRotation.
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
   * Gets the value of the "attributeName" attribute of this CSGRotation.
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
   * Gets the value of the "attributeName" attribute of this CSGRotation.
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
   * Gets the value of the "attributeName" attribute of this CSGRotation.
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
   * Predicate returning @c true if this CSGRotation's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGRotation's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this CSGRotation.
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
   * Sets the value of the "attributeName" attribute of this CSGRotation.
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
   * Sets the value of the "attributeName" attribute of this CSGRotation.
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
   * Sets the value of the "attributeName" attribute of this CSGRotation.
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
   * Sets the value of the "attributeName" attribute of this CSGRotation.
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
   * Unsets the value of the "attributeName" attribute of this CSGRotation.
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
 * Creates a new CSGRotation_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CSGRotation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGRotation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGRotation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
CSGRotation_t *
CSGRotation_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CSGRotation_t object.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return a (deep) copy of this CSGRotation_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
CSGRotation_t*
CSGRotation_clone(const CSGRotation_t* csgr);


/**
 * Frees this CSGRotation_t object.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
void
CSGRotation_free(CSGRotation_t* csgr);


/**
 * Returns the value of the "rotateX" attribute of this CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure whose rotateX is sought.
 *
 * @return the value of the "rotateX" attribute of this CSGRotation_t as a
 * double.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateX(const CSGRotation_t * csgr);


/**
 * Returns the value of the "rotateY" attribute of this CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure whose rotateY is sought.
 *
 * @return the value of the "rotateY" attribute of this CSGRotation_t as a
 * double.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateY(const CSGRotation_t * csgr);


/**
 * Returns the value of the "rotateZ" attribute of this CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure whose rotateZ is sought.
 *
 * @return the value of the "rotateZ" attribute of this CSGRotation_t as a
 * double.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateZ(const CSGRotation_t * csgr);


/**
 * Returns the value of the "rotateAngleInRadians" attribute of this
 * CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure whose rotateAngleInRadians is
 * sought.
 *
 * @return the value of the "rotateAngleInRadians" attribute of this
 * CSGRotation_t as a double.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateAngleInRadians(const CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 (true) if this CSGRotation_t's "rotateX" attribute
 * is set.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return @c 1 (true) if this CSGRotation_t's "rotateX" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateX(const CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 (true) if this CSGRotation_t's "rotateY" attribute
 * is set.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return @c 1 (true) if this CSGRotation_t's "rotateY" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateY(const CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 (true) if this CSGRotation_t's "rotateZ" attribute
 * is set.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return @c 1 (true) if this CSGRotation_t's "rotateZ" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateZ(const CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 (true) if this CSGRotation_t's
 * "rotateAngleInRadians" attribute is set.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return @c 1 (true) if this CSGRotation_t's "rotateAngleInRadians" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateAngleInRadians(const CSGRotation_t * csgr);


/**
 * Sets the value of the "rotateX" attribute of this CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @param rotateX double value of the "rotateX" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateX(CSGRotation_t * csgr, double rotateX);


/**
 * Sets the value of the "rotateY" attribute of this CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @param rotateY double value of the "rotateY" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateY(CSGRotation_t * csgr, double rotateY);


/**
 * Sets the value of the "rotateZ" attribute of this CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @param rotateZ double value of the "rotateZ" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateZ(CSGRotation_t * csgr, double rotateZ);


/**
 * Sets the value of the "rotateAngleInRadians" attribute of this
 * CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @param rotateAngleInRadians double value of the "rotateAngleInRadians"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateAngleInRadians(CSGRotation_t * csgr,
                                    double rotateAngleInRadians);


/**
 * Unsets the value of the "rotateX" attribute of this CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateX(CSGRotation_t * csgr);


/**
 * Unsets the value of the "rotateY" attribute of this CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateY(CSGRotation_t * csgr);


/**
 * Unsets the value of the "rotateZ" attribute of this CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateZ(CSGRotation_t * csgr);


/**
 * Unsets the value of the "rotateAngleInRadians" attribute of this
 * CSGRotation_t.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateAngleInRadians(CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGRotation_t object have been set.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CSGRotation_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the CSGRotation_t object are:
 * @li "rotateX"
 * @li "rotateAngleInRadians"
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_hasRequiredAttributes(const CSGRotation_t * csgr);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGRotation_H__ */


