/**
 * @file CSGScale.h
 * @brief Definition of the CSGScale class.
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
 * @class CSGScale
 * @sbmlbrief{spatial} TODO:Definition of the CSGScale class.
 */


#ifndef CSGScale_H__
#define CSGScale_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/CSGTransformation.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGScale : public CSGTransformation
{
protected:

  /** @cond doxygenLibsbmlInternal */

  double mScaleX;
  bool mIsSetScaleX;
  double mScaleY;
  bool mIsSetScaleY;
  double mScaleZ;
  bool mIsSetScaleZ;

  /** @endcond */

public:

  /**
   * Creates a new CSGScale using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGScale.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGScale.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGScale.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGScale(unsigned int level = SpatialExtension::getDefaultLevel(),
           unsigned int version = SpatialExtension::getDefaultVersion(),
           unsigned int pkgVersion =
             SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGScale using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGScale(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGScale.
   *
   * @param orig the CSGScale instance to copy.
   */
  CSGScale(const CSGScale& orig);


  /**
   * Assignment operator for CSGScale.
   *
   * @param rhs the CSGScale object whose values are to be used as the basis of
   * the assignment.
   */
  CSGScale& operator=(const CSGScale& rhs);


  /**
   * Creates and returns a deep copy of this CSGScale object.
   *
   * @return a (deep) copy of this CSGScale object.
   */
  virtual CSGScale* clone() const;


  /**
   * Destructor for CSGScale.
   */
  virtual ~CSGScale();


  /**
   * Returns the value of the "scaleX" attribute of this CSGScale.
   *
   * @return the value of the "scaleX" attribute of this CSGScale as a double.
   */
  double getScaleX() const;


  /**
   * Returns the value of the "scaleY" attribute of this CSGScale.
   *
   * @return the value of the "scaleY" attribute of this CSGScale as a double.
   */
  double getScaleY() const;


  /**
   * Returns the value of the "scaleZ" attribute of this CSGScale.
   *
   * @return the value of the "scaleZ" attribute of this CSGScale as a double.
   */
  double getScaleZ() const;


  /**
   * Predicate returning @c true if this CSGScale's "scaleX" attribute is set.
   *
   * @return @c true if this CSGScale's "scaleX" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetScaleX() const;


  /**
   * Predicate returning @c true if this CSGScale's "scaleY" attribute is set.
   *
   * @return @c true if this CSGScale's "scaleY" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetScaleY() const;


  /**
   * Predicate returning @c true if this CSGScale's "scaleZ" attribute is set.
   *
   * @return @c true if this CSGScale's "scaleZ" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetScaleZ() const;


  /**
   * Sets the value of the "scaleX" attribute of this CSGScale.
   *
   * @param scaleX double value of the "scaleX" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setScaleX(double scaleX);


  /**
   * Sets the value of the "scaleY" attribute of this CSGScale.
   *
   * @param scaleY double value of the "scaleY" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setScaleY(double scaleY);


  /**
   * Sets the value of the "scaleZ" attribute of this CSGScale.
   *
   * @param scaleZ double value of the "scaleZ" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setScaleZ(double scaleZ);


  /**
   * Unsets the value of the "scaleX" attribute of this CSGScale.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetScaleX();


  /**
   * Unsets the value of the "scaleY" attribute of this CSGScale.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetScaleY();


  /**
   * Unsets the value of the "scaleZ" attribute of this CSGScale.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetScaleZ();


  /**
   * Returns the XML element name of this CSGScale object.
   *
   * For CSGScale, the XML element name is always @c "csgScale".
   *
   * @return the name of this element, i.e. @c "csgScale".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CSGScale object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_CSGSCALE, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGScale object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGScale have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CSGScale object are:
   * @li "scaleX"
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
   * Gets the value of the "attributeName" attribute of this CSGScale.
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
   * Gets the value of the "attributeName" attribute of this CSGScale.
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
   * Gets the value of the "attributeName" attribute of this CSGScale.
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
   * Gets the value of the "attributeName" attribute of this CSGScale.
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
   * Gets the value of the "attributeName" attribute of this CSGScale.
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
   * Predicate returning @c true if this CSGScale's attribute "attributeName"
   * is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGScale's attribute "attributeName" has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this CSGScale.
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
   * Sets the value of the "attributeName" attribute of this CSGScale.
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
   * Sets the value of the "attributeName" attribute of this CSGScale.
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
   * Sets the value of the "attributeName" attribute of this CSGScale.
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
   * Sets the value of the "attributeName" attribute of this CSGScale.
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
   * Unsets the value of the "attributeName" attribute of this CSGScale.
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
 * Creates a new CSGScale_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this CSGScale_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGScale_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGScale_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
CSGScale_t *
CSGScale_create(unsigned int level,
                unsigned int version,
                unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CSGScale_t object.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return a (deep) copy of this CSGScale_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
CSGScale_t*
CSGScale_clone(const CSGScale_t* csgs);


/**
 * Frees this CSGScale_t object.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
void
CSGScale_free(CSGScale_t* csgs);


/**
 * Returns the value of the "scaleX" attribute of this CSGScale_t.
 *
 * @param csgs the CSGScale_t structure whose scaleX is sought.
 *
 * @return the value of the "scaleX" attribute of this CSGScale_t as a double.
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
double
CSGScale_getScaleX(const CSGScale_t * csgs);


/**
 * Returns the value of the "scaleY" attribute of this CSGScale_t.
 *
 * @param csgs the CSGScale_t structure whose scaleY is sought.
 *
 * @return the value of the "scaleY" attribute of this CSGScale_t as a double.
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
double
CSGScale_getScaleY(const CSGScale_t * csgs);


/**
 * Returns the value of the "scaleZ" attribute of this CSGScale_t.
 *
 * @param csgs the CSGScale_t structure whose scaleZ is sought.
 *
 * @return the value of the "scaleZ" attribute of this CSGScale_t as a double.
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
double
CSGScale_getScaleZ(const CSGScale_t * csgs);


/**
 * Predicate returning @c 1 (true) if this CSGScale_t's "scaleX" attribute is
 * set.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return @c 1 (true) if this CSGScale_t's "scaleX" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_isSetScaleX(const CSGScale_t * csgs);


/**
 * Predicate returning @c 1 (true) if this CSGScale_t's "scaleY" attribute is
 * set.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return @c 1 (true) if this CSGScale_t's "scaleY" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_isSetScaleY(const CSGScale_t * csgs);


/**
 * Predicate returning @c 1 (true) if this CSGScale_t's "scaleZ" attribute is
 * set.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return @c 1 (true) if this CSGScale_t's "scaleZ" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_isSetScaleZ(const CSGScale_t * csgs);


/**
 * Sets the value of the "scaleX" attribute of this CSGScale_t.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @param scaleX double value of the "scaleX" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_setScaleX(CSGScale_t * csgs, double scaleX);


/**
 * Sets the value of the "scaleY" attribute of this CSGScale_t.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @param scaleY double value of the "scaleY" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_setScaleY(CSGScale_t * csgs, double scaleY);


/**
 * Sets the value of the "scaleZ" attribute of this CSGScale_t.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @param scaleZ double value of the "scaleZ" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_setScaleZ(CSGScale_t * csgs, double scaleZ);


/**
 * Unsets the value of the "scaleX" attribute of this CSGScale_t.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_unsetScaleX(CSGScale_t * csgs);


/**
 * Unsets the value of the "scaleY" attribute of this CSGScale_t.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_unsetScaleY(CSGScale_t * csgs);


/**
 * Unsets the value of the "scaleZ" attribute of this CSGScale_t.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_unsetScaleZ(CSGScale_t * csgs);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGScale_t object have been set.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CSGScale_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the CSGScale_t object are:
 * @li "scaleX"
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_hasRequiredAttributes(const CSGScale_t * csgs);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGScale_H__ */


