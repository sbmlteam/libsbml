/**
 * @file:   AdvectionCoefficient.h
 * @brief:  Implementation of the AdvectionCoefficient class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#ifndef AdvectionCoefficient_H__
#define AdvectionCoefficient_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN AdvectionCoefficient : public SBase
{

protected:

  std::string   mVariable;
  CoordinateKind_t   mCoordinate;


public:

  /**
   * Creates a new AdvectionCoefficient with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this AdvectionCoefficient
   *
   * @param version an unsigned int, the SBML Version to assign to this AdvectionCoefficient
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this AdvectionCoefficient
   */
  AdvectionCoefficient(unsigned int level      = SpatialExtension::getDefaultLevel(),
                       unsigned int version    = SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new AdvectionCoefficient with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  AdvectionCoefficient(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for AdvectionCoefficient.
   *
   * @param orig; the AdvectionCoefficient instance to copy.
   */
  AdvectionCoefficient(const AdvectionCoefficient& orig);


   /**
   * Assignment operator for AdvectionCoefficient.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  AdvectionCoefficient& operator=(const AdvectionCoefficient& rhs);


   /**
   * Creates and returns a deep copy of this AdvectionCoefficient object.
   *
   * @return a (deep) copy of this AdvectionCoefficient object.
   */
  virtual AdvectionCoefficient* clone () const;


   /**
   * Destructor for AdvectionCoefficient.
   */
  virtual ~AdvectionCoefficient();


   /**
   * Returns the value of the "variable" attribute of this AdvectionCoefficient.
   *
   * @return the value of the "variable" attribute of this AdvectionCoefficient as a string.
   */
  virtual const std::string& getVariable() const;


  /**
   * Returns the value of the "coordinate" attribute of this AdvectionCoefficient.
   *
   * @return the value of the "coordinate" attribute of this AdvectionCoefficient as a CoordinateKind_t.
   */
  virtual CoordinateKind_t getCoordinate() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AdvectionCoefficient's "variable" attribute has been set.
   *
   * @return @c true if this AdvectionCoefficient's "variable" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetVariable() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * AdvectionCoefficient's "coordinate" attribute has been set.
   *
   * @return @c true if this AdvectionCoefficient's "coordinate" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoordinate() const;


  /**
   * Sets the value of the "variable" attribute of this AdvectionCoefficient.
   *
   * @param variable; const std::string& value of the "variable" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setVariable(const std::string& variable);


  /**
   * Sets the value of the "coordinate" attribute of this AdvectionCoefficient.
   *
   * @param coordinate; CoordinateKind_t value of the "coordinate" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinate(CoordinateKind_t coordinate);


  /**
   * Sets the value of the "coordinate" attribute of this AdvectionCoefficient.
   *
   * @param coordinate; string value of the "coordinate" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinate(const std::string& coordinate);


  /**
   * Unsets the value of the "variable" attribute of this AdvectionCoefficient.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetVariable();


  /**
   * Unsets the value of the "coordinate" attribute of this AdvectionCoefficient.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoordinate();


  /**
   * Renames all the @c SIdRef attributes on this element, including any
   * found in MathML content (if such exists).
   *
   * This method works by looking at all attributes and (if appropriate)
   * mathematical formulas, comparing the identifiers to the value of @p
   * oldid.  If any matches are found, the matching identifiers are replaced
   * with @p newid.  The method does @em not descend into child elements.
   *
   * @param oldid the old identifier
   * @param newid the new identifier
   */
   virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
   * Returns the XML element name of this object, which for AdvectionCoefficient, is
   * always @c "advectionCoefficient".
   *
   * @return the name of this element, i.e. @c "advectionCoefficient".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Predicate returning @c true if all the required attributes
   * for this AdvectionCoefficient object have been set.
   *
   * @note The required attributes for a AdvectionCoefficient object are:
   * @li "variable"
   * @li "coordinate"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new AdvectionCoefficient_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * AdvectionCoefficient_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * AdvectionCoefficient_t structure.
 *
 * @returns the newly-created AdvectionCoefficient_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
AdvectionCoefficient_t *
AdvectionCoefficient_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion);


/**
 * Frees the given AdvectionCoefficient_t structure.
 * 
 * @param ac the AdvectionCoefficient_t structure to be freed.
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
void
AdvectionCoefficient_free(AdvectionCoefficient_t * ac);


/**
 * Creates a deep copy of the given AdvectionCoefficient_t structure.
 * 
 * @param ac the AdvectionCoefficient_t structure to be copied.
 *
 * @returns a (deep) copy of the given AdvectionCoefficient_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof AdvectionCoefficient_t
 */
LIBSBML_EXTERN
AdvectionCoefficient_t *
AdvectionCoefficient_clone(AdvectionCoefficient_t * ac);


/**
 * Returns the value of the "variable" attribute of the given AdvectionCoefficient_t
 * structure.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return the variable of this structure.
 *
 * @member of AdvectionCoefficient_t
 */
LIBSBML_EXTERN
const char *
AdvectionCoefficient_getVariable(const AdvectionCoefficient_t * ac);


/**
 * Returns the value of the "coordinate" attribute of the given AdvectionCoefficient_t
 * structure.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return the coordinate of this structure.
 *
 * @member of AdvectionCoefficient_t
 */
LIBSBML_EXTERN
CoordinateKind_t
AdvectionCoefficient_getCoordinate(const AdvectionCoefficient_t * ac);


/**
 * Predicate returning @c 1 if the given AdvectionCoefficient_t structure's "variable"
 * is set.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return @c 1 if the "variable" of this AdvectionCoefficient_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_isSetVariable(const AdvectionCoefficient_t * ac);


/**
 * Predicate returning @c 1 if the given AdvectionCoefficient_t structure's "coordinate"
 * is set.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return @c 1 if the "coordinate" of this AdvectionCoefficient_t structure is
 * set, @c 0 otherwise.
 *
 * @member of AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_isSetCoordinate(const AdvectionCoefficient_t * ac);


/**
 * Sets the "variable" attribute of the given AdvectionCoefficient_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs AdvectionCoefficient_unsetVariable() instead.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @param variable the string to which the structures "variable" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_setVariable(AdvectionCoefficient_t * ac, const char * variable);


/**
 * Sets the "coordinate" attribute of the given AdvectionCoefficient_t structure.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @param coordinate the string to which the structures "coordinate" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_setCoordinate(AdvectionCoefficient_t * ac, CoordinateKind_t coordinate);


/**
 * Unsets the value of the "variable" attribute of the given 
 *AdvectionCoefficient_t structure.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_unsetVariable(AdvectionCoefficient_t * ac);


/**
 * Unsets the value of the "coordinate" attribute of the given 
 *AdvectionCoefficient_t structure.
 *
 * @param ac the AdvectionCoefficient_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_unsetCoordinate(AdvectionCoefficient_t * ac);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given AdvectionCoefficient_t structure have been set.
 *
 * @param ac the AdvectionCoefficient_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of AdvectionCoefficient_t
 */
LIBSBML_EXTERN
int
AdvectionCoefficient_hasRequiredAttributes(const AdvectionCoefficient_t * ac);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  AdvectionCoefficient_H__  */

