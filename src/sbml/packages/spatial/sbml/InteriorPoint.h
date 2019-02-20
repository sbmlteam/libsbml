/**
 * @file InteriorPoint.h
 * @brief Definition of the InteriorPoint class.
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
 * @class InteriorPoint
 * @sbmlbrief{spatial} TODO:Definition of the InteriorPoint class.
 */


#ifndef InteriorPoint_H__
#define InteriorPoint_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN InteriorPoint : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  double mCoord1;
  bool mIsSetCoord1;
  double mCoord2;
  bool mIsSetCoord2;
  double mCoord3;
  bool mIsSetCoord3;

  /** @endcond */

public:

  /**
   * Creates a new InteriorPoint using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * InteriorPoint.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * InteriorPoint.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this InteriorPoint.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  InteriorPoint(unsigned int level = SpatialExtension::getDefaultLevel(),
                unsigned int version = SpatialExtension::getDefaultVersion(),
                unsigned int pkgVersion =
                  SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new InteriorPoint using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  InteriorPoint(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for InteriorPoint.
   *
   * @param orig the InteriorPoint instance to copy.
   */
  InteriorPoint(const InteriorPoint& orig);


  /**
   * Assignment operator for InteriorPoint.
   *
   * @param rhs the InteriorPoint object whose values are to be used as the
   * basis of the assignment.
   */
  InteriorPoint& operator=(const InteriorPoint& rhs);


  /**
   * Creates and returns a deep copy of this InteriorPoint object.
   *
   * @return a (deep) copy of this InteriorPoint object.
   */
  virtual InteriorPoint* clone() const;


  /**
   * Destructor for InteriorPoint.
   */
  virtual ~InteriorPoint();


  /**
   * Returns the value of the "coord1" attribute of this InteriorPoint.
   *
   * @return the value of the "coord1" attribute of this InteriorPoint as a
   * double.
   */
  double getCoord1() const;


  /**
   * Returns the value of the "coord2" attribute of this InteriorPoint.
   *
   * @return the value of the "coord2" attribute of this InteriorPoint as a
   * double.
   */
  double getCoord2() const;


  /**
   * Returns the value of the "coord3" attribute of this InteriorPoint.
   *
   * @return the value of the "coord3" attribute of this InteriorPoint as a
   * double.
   */
  double getCoord3() const;


  /**
   * Predicate returning @c true if this InteriorPoint's "coord1" attribute is
   * set.
   *
   * @return @c true if this InteriorPoint's "coord1" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetCoord1() const;


  /**
   * Predicate returning @c true if this InteriorPoint's "coord2" attribute is
   * set.
   *
   * @return @c true if this InteriorPoint's "coord2" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetCoord2() const;


  /**
   * Predicate returning @c true if this InteriorPoint's "coord3" attribute is
   * set.
   *
   * @return @c true if this InteriorPoint's "coord3" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetCoord3() const;


  /**
   * Sets the value of the "coord1" attribute of this InteriorPoint.
   *
   * @param coord1 double value of the "coord1" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoord1(double coord1);


  /**
   * Sets the value of the "coord2" attribute of this InteriorPoint.
   *
   * @param coord2 double value of the "coord2" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoord2(double coord2);


  /**
   * Sets the value of the "coord3" attribute of this InteriorPoint.
   *
   * @param coord3 double value of the "coord3" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoord3(double coord3);


  /**
   * Unsets the value of the "coord1" attribute of this InteriorPoint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCoord1();


  /**
   * Unsets the value of the "coord2" attribute of this InteriorPoint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCoord2();


  /**
   * Unsets the value of the "coord3" attribute of this InteriorPoint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCoord3();


  /**
   * Returns the XML element name of this InteriorPoint object.
   *
   * For InteriorPoint, the XML element name is always @c "interiorPoint".
   *
   * @return the name of this element, i.e. @c "interiorPoint".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this InteriorPoint object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_INTERIORPOINT, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * InteriorPoint object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * InteriorPoint have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the InteriorPoint object are:
   * @li "coord1"
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
   * Gets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Gets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Gets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Gets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Gets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Predicate returning @c true if this InteriorPoint's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this InteriorPoint's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Sets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Sets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Sets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Sets the value of the "attributeName" attribute of this InteriorPoint.
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
   * Unsets the value of the "attributeName" attribute of this InteriorPoint.
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
 * Creates a new InteriorPoint_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * InteriorPoint_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * InteriorPoint_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this InteriorPoint_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
InteriorPoint_t *
InteriorPoint_create(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this InteriorPoint_t object.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return a (deep) copy of this InteriorPoint_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
InteriorPoint_t*
InteriorPoint_clone(const InteriorPoint_t* ip);


/**
 * Frees this InteriorPoint_t object.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
void
InteriorPoint_free(InteriorPoint_t* ip);


/**
 * Returns the value of the "coord1" attribute of this InteriorPoint_t.
 *
 * @param ip the InteriorPoint_t structure whose coord1 is sought.
 *
 * @return the value of the "coord1" attribute of this InteriorPoint_t as a
 * double.
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
double
InteriorPoint_getCoord1(const InteriorPoint_t * ip);


/**
 * Returns the value of the "coord2" attribute of this InteriorPoint_t.
 *
 * @param ip the InteriorPoint_t structure whose coord2 is sought.
 *
 * @return the value of the "coord2" attribute of this InteriorPoint_t as a
 * double.
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
double
InteriorPoint_getCoord2(const InteriorPoint_t * ip);


/**
 * Returns the value of the "coord3" attribute of this InteriorPoint_t.
 *
 * @param ip the InteriorPoint_t structure whose coord3 is sought.
 *
 * @return the value of the "coord3" attribute of this InteriorPoint_t as a
 * double.
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
double
InteriorPoint_getCoord3(const InteriorPoint_t * ip);


/**
 * Predicate returning @c 1 (true) if this InteriorPoint_t's "coord1" attribute
 * is set.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return @c 1 (true) if this InteriorPoint_t's "coord1" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_isSetCoord1(const InteriorPoint_t * ip);


/**
 * Predicate returning @c 1 (true) if this InteriorPoint_t's "coord2" attribute
 * is set.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return @c 1 (true) if this InteriorPoint_t's "coord2" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_isSetCoord2(const InteriorPoint_t * ip);


/**
 * Predicate returning @c 1 (true) if this InteriorPoint_t's "coord3" attribute
 * is set.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return @c 1 (true) if this InteriorPoint_t's "coord3" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_isSetCoord3(const InteriorPoint_t * ip);


/**
 * Sets the value of the "coord1" attribute of this InteriorPoint_t.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @param coord1 double value of the "coord1" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_setCoord1(InteriorPoint_t * ip, double coord1);


/**
 * Sets the value of the "coord2" attribute of this InteriorPoint_t.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @param coord2 double value of the "coord2" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_setCoord2(InteriorPoint_t * ip, double coord2);


/**
 * Sets the value of the "coord3" attribute of this InteriorPoint_t.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @param coord3 double value of the "coord3" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_setCoord3(InteriorPoint_t * ip, double coord3);


/**
 * Unsets the value of the "coord1" attribute of this InteriorPoint_t.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_unsetCoord1(InteriorPoint_t * ip);


/**
 * Unsets the value of the "coord2" attribute of this InteriorPoint_t.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_unsetCoord2(InteriorPoint_t * ip);


/**
 * Unsets the value of the "coord3" attribute of this InteriorPoint_t.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_unsetCoord3(InteriorPoint_t * ip);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * InteriorPoint_t object have been set.
 *
 * @param ip the InteriorPoint_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * InteriorPoint_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the InteriorPoint_t object are:
 * @li "coord1"
 *
 * @memberof InteriorPoint_t
 */
LIBSBML_EXTERN
int
InteriorPoint_hasRequiredAttributes(const InteriorPoint_t * ip);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !InteriorPoint_H__ */


