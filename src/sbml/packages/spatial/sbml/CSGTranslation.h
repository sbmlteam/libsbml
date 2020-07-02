/**
 * @file CSGTranslation.h
 * @brief Definition of the CSGTranslation class.
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
 * @class CSGTranslation
 * @sbmlbrief{spatial} TODO:Definition of the CSGTranslation class.
 */


#ifndef CSGTranslation_H__
#define CSGTranslation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/CSGTransformation.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGTranslation : public CSGTransformation
{
protected:

  /** @cond doxygenLibsbmlInternal */

  double mTranslateX;
  bool mIsSetTranslateX;
  double mTranslateY;
  bool mIsSetTranslateY;
  double mTranslateZ;
  bool mIsSetTranslateZ;

  /** @endcond */

public:

  /**
   * Creates a new CSGTranslation using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CSGTranslation.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGTranslation.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGTranslation.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGTranslation(unsigned int level = SpatialExtension::getDefaultLevel(),
                 unsigned int version = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion =
                   SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGTranslation using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGTranslation(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGTranslation.
   *
   * @param orig the CSGTranslation instance to copy.
   */
  CSGTranslation(const CSGTranslation& orig);


  /**
   * Assignment operator for CSGTranslation.
   *
   * @param rhs the CSGTranslation object whose values are to be used as the
   * basis of the assignment.
   */
  CSGTranslation& operator=(const CSGTranslation& rhs);


  /**
   * Creates and returns a deep copy of this CSGTranslation object.
   *
   * @return a (deep) copy of this CSGTranslation object.
   */
  virtual CSGTranslation* clone() const;


  /**
   * Destructor for CSGTranslation.
   */
  virtual ~CSGTranslation();


  /**
   * Returns the value of the "translateX" attribute of this CSGTranslation.
   *
   * @return the value of the "translateX" attribute of this CSGTranslation as
   * a double.
   */
  double getTranslateX() const;


  /**
   * Returns the value of the "translateY" attribute of this CSGTranslation.
   *
   * @return the value of the "translateY" attribute of this CSGTranslation as
   * a double.
   */
  double getTranslateY() const;


  /**
   * Returns the value of the "translateZ" attribute of this CSGTranslation.
   *
   * @return the value of the "translateZ" attribute of this CSGTranslation as
   * a double.
   */
  double getTranslateZ() const;


  /**
   * Predicate returning @c true if this CSGTranslation's "translateX"
   * attribute is set.
   *
   * @return @c true if this CSGTranslation's "translateX" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetTranslateX() const;


  /**
   * Predicate returning @c true if this CSGTranslation's "translateY"
   * attribute is set.
   *
   * @return @c true if this CSGTranslation's "translateY" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetTranslateY() const;


  /**
   * Predicate returning @c true if this CSGTranslation's "translateZ"
   * attribute is set.
   *
   * @return @c true if this CSGTranslation's "translateZ" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetTranslateZ() const;


  /**
   * Sets the value of the "translateX" attribute of this CSGTranslation.
   *
   * @param translateX double value of the "translateX" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTranslateX(double translateX);


  /**
   * Sets the value of the "translateY" attribute of this CSGTranslation.
   *
   * @param translateY double value of the "translateY" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTranslateY(double translateY);


  /**
   * Sets the value of the "translateZ" attribute of this CSGTranslation.
   *
   * @param translateZ double value of the "translateZ" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTranslateZ(double translateZ);


  /**
   * Unsets the value of the "translateX" attribute of this CSGTranslation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetTranslateX();


  /**
   * Unsets the value of the "translateY" attribute of this CSGTranslation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetTranslateY();


  /**
   * Unsets the value of the "translateZ" attribute of this CSGTranslation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetTranslateZ();


  /**
   * Returns the XML element name of this CSGTranslation object.
   *
   * For CSGTranslation, the XML element name is always @c "csgTranslation".
   *
   * @return the name of this element, i.e. @c "csgTranslation".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CSGTranslation object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_CSGTRANSLATION, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGTranslation object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGTranslation have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CSGTranslation object are:
   * @li "translateX"
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
   * Gets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Gets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Gets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Gets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Gets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Predicate returning @c true if this CSGTranslation's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGTranslation's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Sets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Sets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Sets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Sets the value of the "attributeName" attribute of this CSGTranslation.
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
   * Unsets the value of the "attributeName" attribute of this CSGTranslation.
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
 * Creates a new CSGTranslation_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CSGTranslation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGTranslation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGTranslation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
CSGTranslation_t *
CSGTranslation_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CSGTranslation_t object.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return a (deep) copy of this CSGTranslation_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
CSGTranslation_t*
CSGTranslation_clone(const CSGTranslation_t* csgt);


/**
 * Frees this CSGTranslation_t object.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
void
CSGTranslation_free(CSGTranslation_t* csgt);


/**
 * Returns the value of the "translateX" attribute of this CSGTranslation_t.
 *
 * @param csgt the CSGTranslation_t structure whose translateX is sought.
 *
 * @return the value of the "translateX" attribute of this CSGTranslation_t as
 * a double.
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
double
CSGTranslation_getTranslateX(const CSGTranslation_t * csgt);


/**
 * Returns the value of the "translateY" attribute of this CSGTranslation_t.
 *
 * @param csgt the CSGTranslation_t structure whose translateY is sought.
 *
 * @return the value of the "translateY" attribute of this CSGTranslation_t as
 * a double.
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
double
CSGTranslation_getTranslateY(const CSGTranslation_t * csgt);


/**
 * Returns the value of the "translateZ" attribute of this CSGTranslation_t.
 *
 * @param csgt the CSGTranslation_t structure whose translateZ is sought.
 *
 * @return the value of the "translateZ" attribute of this CSGTranslation_t as
 * a double.
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
double
CSGTranslation_getTranslateZ(const CSGTranslation_t * csgt);


/**
 * Predicate returning @c 1 (true) if this CSGTranslation_t's "translateX"
 * attribute is set.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return @c 1 (true) if this CSGTranslation_t's "translateX" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateX(const CSGTranslation_t * csgt);


/**
 * Predicate returning @c 1 (true) if this CSGTranslation_t's "translateY"
 * attribute is set.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return @c 1 (true) if this CSGTranslation_t's "translateY" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateY(const CSGTranslation_t * csgt);


/**
 * Predicate returning @c 1 (true) if this CSGTranslation_t's "translateZ"
 * attribute is set.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return @c 1 (true) if this CSGTranslation_t's "translateZ" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateZ(const CSGTranslation_t * csgt);


/**
 * Sets the value of the "translateX" attribute of this CSGTranslation_t.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @param translateX double value of the "translateX" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_setTranslateX(CSGTranslation_t * csgt, double translateX);


/**
 * Sets the value of the "translateY" attribute of this CSGTranslation_t.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @param translateY double value of the "translateY" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_setTranslateY(CSGTranslation_t * csgt, double translateY);


/**
 * Sets the value of the "translateZ" attribute of this CSGTranslation_t.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @param translateZ double value of the "translateZ" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_setTranslateZ(CSGTranslation_t * csgt, double translateZ);


/**
 * Unsets the value of the "translateX" attribute of this CSGTranslation_t.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateX(CSGTranslation_t * csgt);


/**
 * Unsets the value of the "translateY" attribute of this CSGTranslation_t.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateY(CSGTranslation_t * csgt);


/**
 * Unsets the value of the "translateZ" attribute of this CSGTranslation_t.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateZ(CSGTranslation_t * csgt);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGTranslation_t object have been set.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CSGTranslation_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the CSGTranslation_t object are:
 * @li "translateX"
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_hasRequiredAttributes(const CSGTranslation_t * csgt);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGTranslation_H__ */


