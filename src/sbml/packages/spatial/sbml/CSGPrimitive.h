/**
 * @file CSGPrimitive.h
 * @brief Definition of the CSGPrimitive class.
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
 * @class CSGPrimitive
 * @sbmlbrief{spatial} TODO:Definition of the CSGPrimitive class.
 */

/**
 * <!-- ~ ~ ~ ~ ~ Start of common documentation strings ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 * The following text is used as common documentation blocks copied multiple
 * times elsewhere in this file. The use of @class is a hack needed because
 * Doxygen's @copydetails command has limited functionality. Symbols
 * beginning with "doc_" are marked as ignored in our Doxygen configuration.
 * ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ -->
 *
 *
 * @class doc_csgprimitive_primitiveType
 *
 * @par
 * The attribute "primitiveType" on a CSGPrimitive object is used to TODO:add
 * explanation
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Spatial specification, the following are the
 * allowable values for "primitiveType":
 * <ul>
 * <li> @c "sphere", TODO:add description
 *
 * <li> @c "cube", TODO:add description
 *
 * <li> @c "cylinder", TODO:add description
 *
 * <li> @c "cone", TODO:add description
 *
 * <li> @c "circle", TODO:add description
 *
 * <li> @c "square", TODO:add description
 *
 * </ul>
 */


#ifndef CSGPrimitive_H__
#define CSGPrimitive_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/CSGNode.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGPrimitive : public CSGNode
{
protected:

  /** @cond doxygenLibsbmlInternal */

  PrimitiveKind_t mPrimitiveType;

  /** @endcond */

public:

  /**
   * Creates a new CSGPrimitive using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CSGPrimitive.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGPrimitive.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGPrimitive.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGPrimitive(unsigned int level = SpatialExtension::getDefaultLevel(),
               unsigned int version = SpatialExtension::getDefaultVersion(),
               unsigned int pkgVersion =
                 SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGPrimitive using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGPrimitive(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGPrimitive.
   *
   * @param orig the CSGPrimitive instance to copy.
   */
  CSGPrimitive(const CSGPrimitive& orig);


  /**
   * Assignment operator for CSGPrimitive.
   *
   * @param rhs the CSGPrimitive object whose values are to be used as the
   * basis of the assignment.
   */
  CSGPrimitive& operator=(const CSGPrimitive& rhs);


  /**
   * Creates and returns a deep copy of this CSGPrimitive object.
   *
   * @return a (deep) copy of this CSGPrimitive object.
   */
  virtual CSGPrimitive* clone() const;


  /**
   * Destructor for CSGPrimitive.
   */
  virtual ~CSGPrimitive();


  /**
   * Returns the value of the "primitiveType" attribute of this CSGPrimitive.
   *
   * @return the value of the "primitiveType" attribute of this CSGPrimitive as
   * a PrimitiveKind_t.
   *
   * @copydetails doc_csgprimitive_primitiveType
   * @if clike The value is drawn from the enumeration @ref PrimitiveKind_t
   * @endif
   * The possible values returned by this method are:
   * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_SPHERE, PrimitiveKind_t}
   * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_CUBE, PrimitiveKind_t}
   * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_CYLINDER, PrimitiveKind_t}
   * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_CONE, PrimitiveKind_t}
   * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_CIRCLE, PrimitiveKind_t}
   * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_SQUARE, PrimitiveKind_t}
   * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_INVALID, PrimitiveKind_t}
   */
  PrimitiveKind_t getPrimitiveType() const;


  /**
   * Returns the value of the "primitiveType" attribute of this CSGPrimitive.
   *
   * @return the value of the "primitiveType" attribute of this CSGPrimitive as
   * a string.
   *
   * @copydetails doc_csgprimitive_primitiveType
   * The possible values returned by this method are:
   * @li @c "sphere"
   * @li @c "cube"
   * @li @c "cylinder"
   * @li @c "cone"
   * @li @c "circle"
   * @li @c "square"
   * @li @c "invalid PrimitiveKind value"
   */
  const std::string& getPrimitiveTypeAsString() const;


  /**
   * Predicate returning @c true if this CSGPrimitive's "primitiveType"
   * attribute is set.
   *
   * @return @c true if this CSGPrimitive's "primitiveType" attribute has been
   * set, otherwise @c false is returned.
   *
   * @copydetails doc_csgprimitive_primitiveType
   */
  bool isSetPrimitiveType() const;


  /**
   * Sets the value of the "primitiveType" attribute of this CSGPrimitive.
   *
   * @param primitiveType @if clike PrimitiveKind_t@else int@endif value of the
   * "primitiveType" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_csgprimitive_primitiveType
   */
  int setPrimitiveType(const PrimitiveKind_t primitiveType);


  /**
   * Sets the value of the "primitiveType" attribute of this CSGPrimitive.
   *
   * @param primitiveType std::string& of the "primitiveType" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_csgprimitive_primitiveType
   */
  int setPrimitiveType(const std::string& primitiveType);


  /**
   * Unsets the value of the "primitiveType" attribute of this CSGPrimitive.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_csgprimitive_primitiveType
   */
  int unsetPrimitiveType();


  /**
   * Returns the XML element name of this CSGPrimitive object.
   *
   * For CSGPrimitive, the XML element name is always @c "csgPrimitive".
   *
   * @return the name of this element, i.e. @c "csgPrimitive".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CSGPrimitive object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_CSGPRIMITIVE, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGPrimitive object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGPrimitive have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CSGPrimitive object are:
   * @li "primitiveType"
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
   * Gets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Gets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Gets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Gets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Gets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Predicate returning @c true if this CSGPrimitive's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGPrimitive's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Sets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Sets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Sets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Sets the value of the "attributeName" attribute of this CSGPrimitive.
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
   * Unsets the value of the "attributeName" attribute of this CSGPrimitive.
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
 * Creates a new CSGPrimitive_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CSGPrimitive_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGPrimitive_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGPrimitive_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
CSGPrimitive_t *
CSGPrimitive_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CSGPrimitive_t object.
 *
 * @param csgp the CSGPrimitive_t structure.
 *
 * @return a (deep) copy of this CSGPrimitive_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
CSGPrimitive_t*
CSGPrimitive_clone(const CSGPrimitive_t* csgp);


/**
 * Frees this CSGPrimitive_t object.
 *
 * @param csgp the CSGPrimitive_t structure.
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
void
CSGPrimitive_free(CSGPrimitive_t* csgp);


/**
 * Returns the value of the "primitiveType" attribute of this CSGPrimitive_t.
 *
 * @param csgp the CSGPrimitive_t structure whose primitiveType is sought.
 *
 * @return the value of the "primitiveType" attribute of this CSGPrimitive_t as
 * a PrimitiveKind_t.
 *
 * @copydetails doc_csgprimitive_primitiveType
 * @if clike The value is drawn from the enumeration @ref PrimitiveKind_t
 * @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_SPHERE, PrimitiveKind_t}
 * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_CUBE, PrimitiveKind_t}
 * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_CYLINDER, PrimitiveKind_t}
 * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_CONE, PrimitiveKind_t}
 * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_CIRCLE, PrimitiveKind_t}
 * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_SQUARE, PrimitiveKind_t}
 * @li @sbmlconstant{SPATIAL_PRIMITIVEKIND_INVALID, PrimitiveKind_t}
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
PrimitiveKind_t
CSGPrimitive_getPrimitiveType(const CSGPrimitive_t * csgp);


/**
 * Returns the value of the "primitiveType" attribute of this CSGPrimitive_t.
 *
 * @param csgp the CSGPrimitive_t structure whose primitiveType is sought.
 *
 * @return the value of the "primitiveType" attribute of this CSGPrimitive_t as
 * a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_csgprimitive_primitiveType
 * The possible values returned by this method are:
 * @li @c "sphere"
 * @li @c "cube"
 * @li @c "cylinder"
 * @li @c "cone"
 * @li @c "circle"
 * @li @c "square"
 * @li @c "invalid PrimitiveKind value"
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
char *
CSGPrimitive_getPrimitiveTypeAsString(const CSGPrimitive_t * csgp);


/**
 * Predicate returning @c 1 (true) if this CSGPrimitive_t's "primitiveType"
 * attribute is set.
 *
 * @param csgp the CSGPrimitive_t structure.
 *
 * @return @c 1 (true) if this CSGPrimitive_t's "primitiveType" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_csgprimitive_primitiveType
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPrimitive_isSetPrimitiveType(const CSGPrimitive_t * csgp);


/**
 * Sets the value of the "primitiveType" attribute of this CSGPrimitive_t.
 *
 * @param csgp the CSGPrimitive_t structure.
 *
 * @param primitiveType PrimitiveKind_t value of the "primitiveType" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_csgprimitive_primitiveType
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPrimitive_setPrimitiveType(CSGPrimitive_t * csgp,
                              PrimitiveKind_t primitiveType);


/**
 * Sets the value of the "primitiveType" attribute of this CSGPrimitive_t.
 *
 * @param csgp the CSGPrimitive_t structure.
 *
 * @param primitiveType const char * of the "primitiveType" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_csgprimitive_primitiveType
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPrimitive_setPrimitiveTypeAsString(CSGPrimitive_t * csgp,
                                      const char * primitiveType);


/**
 * Unsets the value of the "primitiveType" attribute of this CSGPrimitive_t.
 *
 * @param csgp the CSGPrimitive_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_csgprimitive_primitiveType
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPrimitive_unsetPrimitiveType(CSGPrimitive_t * csgp);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGPrimitive_t object have been set.
 *
 * @param csgp the CSGPrimitive_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CSGPrimitive_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the CSGPrimitive_t object are:
 * @li "primitiveType"
 *
 * @memberof CSGPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPrimitive_hasRequiredAttributes(const CSGPrimitive_t * csgp);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGPrimitive_H__ */


