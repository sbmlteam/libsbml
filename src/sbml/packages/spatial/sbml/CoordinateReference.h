/**
 * @file CoordinateReference.h
 * @brief Definition of the CoordinateReference class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
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
 * @class CoordinateReference
 * @sbmlbrief{spatial} TODO:Definition of the CoordinateReference class.
 */


#ifndef CoordinateReference_H__
#define CoordinateReference_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CoordinateReference : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  CoordinateKind_t mCoordinate;

  /** @endcond */

public:

  /**
   * Creates a new CoordinateReference using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CoordinateReference.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CoordinateReference.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CoordinateReference.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  CoordinateReference(unsigned int level = SpatialExtension::getDefaultLevel(),
                      unsigned int version =
                        SpatialExtension::getDefaultVersion(),
                      unsigned int pkgVersion =
                        SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CoordinateReference using the given SpatialPkgNamespaces
   * object.
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  CoordinateReference(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CoordinateReference.
   *
   * @param orig the CoordinateReference instance to copy.
   */
  CoordinateReference(const CoordinateReference& orig);


  /**
   * Assignment operator for CoordinateReference.
   *
   * @param rhs the CoordinateReference object whose values are to be used as
   * the basis of the assignment.
   */
  CoordinateReference& operator=(const CoordinateReference& rhs);


  /**
   * Creates and returns a deep copy of this CoordinateReference object.
   *
   * @return a (deep) copy of this CoordinateReference object.
   */
  virtual CoordinateReference* clone() const;


  /**
   * Destructor for CoordinateReference.
   */
  virtual ~CoordinateReference();


  /**
   * Returns the value of the "coordinate" attribute of this
   * CoordinateReference.
   *
   * @return the value of the "coordinate" attribute of this
   * CoordinateReference as a CoordinateKind_t.
   */
  CoordinateKind_t getCoordinate() const;


  /**
   * Returns the value of the "coordinate" attribute of this
   * CoordinateReference.
   *
   * @return the value of the "coordinate" attribute of this
   * CoordinateReference as a string.
   */
  const std::string& getCoordinateAsString() const;


  /**
   * Predicate returning @c true if this CoordinateReference's "coordinate"
   * attribute is set.
   *
   * @return @c true if this CoordinateReference's "coordinate" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetCoordinate() const;


  /**
   * Sets the value of the "coordinate" attribute of this CoordinateReference.
   *
   * @param coordinate CoordinateKind_t value of the "coordinate" attribute to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoordinate(const CoordinateKind_t coordinate);


  /**
   * Sets the value of the "coordinate" attribute of this CoordinateReference.
   *
   * @param coordinate std::string& of the "coordinate" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoordinate(const std::string& coordinate);


  /**
   * Unsets the value of the "coordinate" attribute of this
   * CoordinateReference.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCoordinate();


  /**
   * Returns the XML element name of this CoordinateReference object.
   *
   * For CoordinateReference, the XML element name is always @c
   * "coordinateReference".
   *
   * @return the name of this element, i.e. @c "coordinateReference".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CoordinateReference object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   *
   * @sbmlconstant{SBML_SPATIAL_COORDINATEREFERENCE, SBMLSpatialTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CoordinateReference object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CoordinateReference have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CoordinateReference object are:
   * @li "coordinate"
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
   * Gets the value of the "attributeName" attribute of this
   * CoordinateReference.
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
   * CoordinateReference.
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
   * CoordinateReference.
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
   * CoordinateReference.
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
   * CoordinateReference.
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
   * Gets the value of the "attributeName" attribute of this
   * CoordinateReference.
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
                           const char* value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this CoordinateReference's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CoordinateReference's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * CoordinateReference.
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
   * CoordinateReference.
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
   * CoordinateReference.
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
   * CoordinateReference.
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
   * CoordinateReference.
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
   * Sets the value of the "attributeName" attribute of this
   * CoordinateReference.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, const char*
    value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this
   * CoordinateReference.
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
 * Creates a new CoordinateReference_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CoordinateReference_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CoordinateReference_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CoordinateReference_t.
 *
 * @throws SBMLConstructorException
 * Thrown if the given @p level and @p version combination, or this kind of
 * SBML object, are either invalid or mismatched with respect to the parent
 * SBMLDocument object.
 * @copydetails doc_note_setting_lv
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
CoordinateReference_t *
CoordinateReference_create(
                           unsigned int level =
                             SpatialExtension::getDefaultLevel(),
                           unsigned int version =
                             SpatialExtension::getDefaultVersion(),
                           unsigned int pkgVersion =
                             SpatialExtension::getDefaultPackageVersion());


/**
 * Creates and returns a deep copy of this CoordinateReference_t object.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @return a (deep) copy of this CoordinateReference_t object.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
CoordinateReference_t*
CoordinateReference_clone(const CoordinateReference_t* cr);


/**
 * Frees this CoordinateReference_t object.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
void
CoordinateReference_free(CoordinateReference_t* cr);


/**
 * Returns the value of the "coordinate" attribute of this
 * CoordinateReference_t.
 *
 * @param cr the CoordinateReference_t structure whose coordinate is sought.
 *
 * @return the value of the "coordinate" attribute of this
 * CoordinateReference_t as a CoordinateKind_t.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateReference_getCoordinate(const CoordinateReference_t * cr);


/**
 * Returns the value of the "coordinate" attribute of this
 * CoordinateReference_t.
 *
 * @param cr the CoordinateReference_t structure whose coordinate is sought.
 *
 * @return the value of the "coordinate" attribute of this
 * CoordinateReference_t as a const char *.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
const char *
CoordinateReference_getCoordinateAsString(const CoordinateReference_t * cr);


/**
 * Predicate returning @c 1 if this CoordinateReference_t's "coordinate"
 * attribute is set.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @return @c 1 if this CoordinateReference_t's "coordinate" attribute has been
 * set, otherwise @c 0 is returned.
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
int
CoordinateReference_isSetCoordinate(const CoordinateReference_t * cr);


/**
 * Sets the value of the "coordinate" attribute of this CoordinateReference_t.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @param coordinate CoordinateKind_t value of the "coordinate" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
int
CoordinateReference_setCoordinate(CoordinateReference_t * cr,
                                  CoordinateKind_t coordinate);


/**
 * Sets the value of the "coordinate" attribute of this CoordinateReference_t.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @param coordinate const char * of the "coordinate" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
int
CoordinateReference_setCoordinateAsString(CoordinateReference_t * cr,
                                          const char * coordinate);


/**
 * Unsets the value of the "coordinate" attribute of this
 * CoordinateReference_t.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
int
CoordinateReference_unsetCoordinate(CoordinateReference_t * cr);


/**
 * Predicate returning @c 1 if all the required attributes for this
 * CoordinateReference_t object have been set.
 *
 * @param cr the CoordinateReference_t structure.
 *
 * @return @c 1 to indicate that all the required attributes of this
 * CoordinateReference_t have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required attributes for the CoordinateReference_t object are:
 * @li "coordinate"
 *
 * @memberof CoordinateReference_t
 */
LIBSBML_EXTERN
int
CoordinateReference_hasRequiredAttributes(const CoordinateReference_t * cr);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CoordinateReference_H__ */


