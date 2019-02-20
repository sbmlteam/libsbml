/**
 * @file Boundary.h
 * @brief Definition of the Boundary class.
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
 * @class Boundary
 * @sbmlbrief{spatial} TODO:Definition of the Boundary class.
 */


#ifndef Boundary_H__
#define Boundary_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Boundary : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  double mValue;
  bool mIsSetValue;
  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new Boundary using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Boundary.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Boundary.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this Boundary.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Boundary(unsigned int level = SpatialExtension::getDefaultLevel(),
           unsigned int version = SpatialExtension::getDefaultVersion(),
           unsigned int pkgVersion =
             SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new Boundary using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Boundary(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for Boundary.
   *
   * @param orig the Boundary instance to copy.
   */
  Boundary(const Boundary& orig);


  /**
   * Assignment operator for Boundary.
   *
   * @param rhs the Boundary object whose values are to be used as the basis of
   * the assignment.
   */
  Boundary& operator=(const Boundary& rhs);


  /**
   * Creates and returns a deep copy of this Boundary object.
   *
   * @return a (deep) copy of this Boundary object.
   */
  virtual Boundary* clone() const;


  /**
   * Destructor for Boundary.
   */
  virtual ~Boundary();


  /**
   * Returns the value of the "id" attribute of this Boundary.
   *
   * @return the value of the "id" attribute of this Boundary as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this Boundary.
   *
   * @return the value of the "name" attribute of this Boundary as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "value" attribute of this Boundary.
   *
   * @return the value of the "value" attribute of this Boundary as a double.
   */
  double getValue() const;


  /**
   * Predicate returning @c true if this Boundary's "id" attribute is set.
   *
   * @return @c true if this Boundary's "id" attribute has been set, otherwise
   * @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this Boundary's "name" attribute is set.
   *
   * @return @c true if this Boundary's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this Boundary's "value" attribute is set.
   *
   * @return @c true if this Boundary's "value" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetValue() const;


  /**
   * Sets the value of the "id" attribute of this Boundary.
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
   * Sets the value of the "name" attribute of this Boundary.
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
   * Sets the value of the "value" attribute of this Boundary.
   *
   * @param value double value of the "value" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setValue(double value);


  /**
   * Unsets the value of the "id" attribute of this Boundary.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this Boundary.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "value" attribute of this Boundary.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetValue();


  /**
   * Returns the XML element name of this Boundary object.
   *
   * For Boundary, the XML element name is always @c "boundary".
   *
   * @return the name of this element, i.e. @c "boundary".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this Boundary object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this Boundary object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_BOUNDARY, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * Boundary object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * Boundary have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the Boundary object are:
   * @li "id"
   * @li "value"
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
   * Gets the value of the "attributeName" attribute of this Boundary.
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
   * Gets the value of the "attributeName" attribute of this Boundary.
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
   * Gets the value of the "attributeName" attribute of this Boundary.
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
   * Gets the value of the "attributeName" attribute of this Boundary.
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
   * Gets the value of the "attributeName" attribute of this Boundary.
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
   * Predicate returning @c true if this Boundary's attribute "attributeName"
   * is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this Boundary's attribute "attributeName" has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Boundary.
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
   * Sets the value of the "attributeName" attribute of this Boundary.
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
   * Sets the value of the "attributeName" attribute of this Boundary.
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
   * Sets the value of the "attributeName" attribute of this Boundary.
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
   * Sets the value of the "attributeName" attribute of this Boundary.
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
   * Unsets the value of the "attributeName" attribute of this Boundary.
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
 * Creates a new Boundary_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Boundary_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Boundary_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this Boundary_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
Boundary_t *
Boundary_create(unsigned int level,
                unsigned int version,
                unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Boundary_t object.
 *
 * @param b the Boundary_t structure.
 *
 * @return a (deep) copy of this Boundary_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
Boundary_t*
Boundary_clone(const Boundary_t* b);


/**
 * Frees this Boundary_t object.
 *
 * @param b the Boundary_t structure.
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
void
Boundary_free(Boundary_t* b);


/**
 * Returns the value of the "id" attribute of this Boundary_t.
 *
 * @param b the Boundary_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this Boundary_t as a pointer to a
 * string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
char *
Boundary_getId(const Boundary_t * b);


/**
 * Returns the value of the "name" attribute of this Boundary_t.
 *
 * @param b the Boundary_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this Boundary_t as a pointer to
 * a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
char *
Boundary_getName(const Boundary_t * b);


/**
 * Returns the value of the "value" attribute of this Boundary_t.
 *
 * @param b the Boundary_t structure whose value is sought.
 *
 * @return the value of the "value" attribute of this Boundary_t as a double.
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
double
Boundary_getValue(const Boundary_t * b);


/**
 * Predicate returning @c 1 (true) if this Boundary_t's "id" attribute is set.
 *
 * @param b the Boundary_t structure.
 *
 * @return @c 1 (true) if this Boundary_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_isSetId(const Boundary_t * b);


/**
 * Predicate returning @c 1 (true) if this Boundary_t's "name" attribute is
 * set.
 *
 * @param b the Boundary_t structure.
 *
 * @return @c 1 (true) if this Boundary_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_isSetName(const Boundary_t * b);


/**
 * Predicate returning @c 1 (true) if this Boundary_t's "value" attribute is
 * set.
 *
 * @param b the Boundary_t structure.
 *
 * @return @c 1 (true) if this Boundary_t's "value" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_isSetValue(const Boundary_t * b);


/**
 * Sets the value of the "id" attribute of this Boundary_t.
 *
 * @param b the Boundary_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling Boundary_unsetId().
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_setId(Boundary_t * b, const char * id);


/**
 * Sets the value of the "name" attribute of this Boundary_t.
 *
 * @param b the Boundary_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling Boundary_unsetName().
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_setName(Boundary_t * b, const char * name);


/**
 * Sets the value of the "value" attribute of this Boundary_t.
 *
 * @param b the Boundary_t structure.
 *
 * @param value double value of the "value" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_setValue(Boundary_t * b, double value);


/**
 * Unsets the value of the "id" attribute of this Boundary_t.
 *
 * @param b the Boundary_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_unsetId(Boundary_t * b);


/**
 * Unsets the value of the "name" attribute of this Boundary_t.
 *
 * @param b the Boundary_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_unsetName(Boundary_t * b);


/**
 * Unsets the value of the "value" attribute of this Boundary_t.
 *
 * @param b the Boundary_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_unsetValue(Boundary_t * b);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Boundary_t object have been set.
 *
 * @param b the Boundary_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Boundary_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the Boundary_t object are:
 * @li "id"
 * @li "value"
 *
 * @memberof Boundary_t
 */
LIBSBML_EXTERN
int
Boundary_hasRequiredAttributes(const Boundary_t * b);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Boundary_H__ */


