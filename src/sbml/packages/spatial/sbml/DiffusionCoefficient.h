/**
 * @file:   DiffusionCoefficient.h
 * @brief:  Implementation of the DiffusionCoefficient class
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


#ifndef DiffusionCoefficient_H__
#define DiffusionCoefficient_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN DiffusionCoefficient : public SBase
{

protected:

  std::string   mVariable;
  DiffusionKind_t   mType;
  CoordinateKind_t   mCoordinateReference1;
  CoordinateKind_t   mCoordinateReference2;


public:

  /**
   * Creates a new DiffusionCoefficient with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this DiffusionCoefficient
   *
   * @param version an unsigned int, the SBML Version to assign to this DiffusionCoefficient
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this DiffusionCoefficient
   */
  DiffusionCoefficient(unsigned int level      = SpatialExtension::getDefaultLevel(),
                       unsigned int version    = SpatialExtension::getDefaultVersion(),
                       unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new DiffusionCoefficient with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  DiffusionCoefficient(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for DiffusionCoefficient.
   *
   * @param orig; the DiffusionCoefficient instance to copy.
   */
  DiffusionCoefficient(const DiffusionCoefficient& orig);


   /**
   * Assignment operator for DiffusionCoefficient.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  DiffusionCoefficient& operator=(const DiffusionCoefficient& rhs);


   /**
   * Creates and returns a deep copy of this DiffusionCoefficient object.
   *
   * @return a (deep) copy of this DiffusionCoefficient object.
   */
  virtual DiffusionCoefficient* clone () const;


   /**
   * Destructor for DiffusionCoefficient.
   */
  virtual ~DiffusionCoefficient();


   /**
   * Returns the value of the "variable" attribute of this DiffusionCoefficient.
   *
   * @return the value of the "variable" attribute of this DiffusionCoefficient as a string.
   */
  virtual const std::string& getVariable() const;


  /**
   * Returns the value of the "type" attribute of this DiffusionCoefficient.
   *
   * @return the value of the "type" attribute of this DiffusionCoefficient as a DiffusionKind_t.
   */
  virtual DiffusionKind_t getType() const;


  /**
   * Returns the value of the "coordinateReference1" attribute of this DiffusionCoefficient.
   *
   * @return the value of the "coordinateReference1" attribute of this DiffusionCoefficient as a CoordinateKind_t.
   */
  virtual CoordinateKind_t getCoordinateReference1() const;


  /**
   * Returns the value of the "coordinateReference2" attribute of this DiffusionCoefficient.
   *
   * @return the value of the "coordinateReference2" attribute of this DiffusionCoefficient as a CoordinateKind_t.
   */
  virtual CoordinateKind_t getCoordinateReference2() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * DiffusionCoefficient's "variable" attribute has been set.
   *
   * @return @c true if this DiffusionCoefficient's "variable" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetVariable() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * DiffusionCoefficient's "type" attribute has been set.
   *
   * @return @c true if this DiffusionCoefficient's "type" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * DiffusionCoefficient's "coordinateReference1" attribute has been set.
   *
   * @return @c true if this DiffusionCoefficient's "coordinateReference1" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoordinateReference1() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * DiffusionCoefficient's "coordinateReference2" attribute has been set.
   *
   * @return @c true if this DiffusionCoefficient's "coordinateReference2" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoordinateReference2() const;


  /**
   * Sets the value of the "variable" attribute of this DiffusionCoefficient.
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
   * Sets the value of the "type" attribute of this DiffusionCoefficient.
   *
   * @param type; DiffusionKind_t value of the "type" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setType(DiffusionKind_t type);


  /**
   * Sets the value of the "type" attribute of this DiffusionCoefficient.
   *
   * @param type; string value of the "type" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setType(const std::string& type);


  /**
   * Sets the value of the "coordinateReference1" attribute of this DiffusionCoefficient.
   *
   * @param coordinateReference1; CoordinateKind_t value of the "coordinateReference1" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinateReference1(CoordinateKind_t coordinateReference1);


  /**
   * Sets the value of the "coordinateReference1" attribute of this DiffusionCoefficient.
   *
   * @param coordinateReference1; string value of the "coordinateReference1" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinateReference1(const std::string& coordinateReference1);


  /**
   * Sets the value of the "coordinateReference2" attribute of this DiffusionCoefficient.
   *
   * @param coordinateReference2; CoordinateKind_t value of the "coordinateReference2" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinateReference2(CoordinateKind_t coordinateReference2);


  /**
   * Sets the value of the "coordinateReference2" attribute of this DiffusionCoefficient.
   *
   * @param coordinateReference2; string value of the "coordinateReference2" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinateReference2(const std::string& coordinateReference2);


  /**
   * Unsets the value of the "variable" attribute of this DiffusionCoefficient.
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
   * Unsets the value of the "type" attribute of this DiffusionCoefficient.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetType();


  /**
   * Unsets the value of the "coordinateReference1" attribute of this DiffusionCoefficient.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoordinateReference1();


  /**
   * Unsets the value of the "coordinateReference2" attribute of this DiffusionCoefficient.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoordinateReference2();


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
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for DiffusionCoefficient, is
   * always @c "diffusionCoefficient".
   *
   * @return the name of this element, i.e. @c "diffusionCoefficient".
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
   * for this DiffusionCoefficient object have been set.
   *
   * @note The required attributes for a DiffusionCoefficient object are:
   * @li "variable"
   * @li "type"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this DiffusionCoefficient object have been set.
   *
   * @note The required elements for a DiffusionCoefficient object are:
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const;


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
   * Connects to child elements.
   */
  virtual void connectToChild ();


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
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


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
 * Creates a new DiffusionCoefficient_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * DiffusionCoefficient_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * DiffusionCoefficient_t structure.
 *
 * @returns the newly-created DiffusionCoefficient_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
DiffusionCoefficient_t *
DiffusionCoefficient_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion);


/**
 * Frees the given DiffusionCoefficient_t structure.
 * 
 * @param dc the DiffusionCoefficient_t structure to be freed.
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
void
DiffusionCoefficient_free(DiffusionCoefficient_t * dc);


/**
 * Creates a deep copy of the given DiffusionCoefficient_t structure.
 * 
 * @param dc the DiffusionCoefficient_t structure to be copied.
 *
 * @returns a (deep) copy of the given DiffusionCoefficient_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof DiffusionCoefficient_t
 */
LIBSBML_EXTERN
DiffusionCoefficient_t *
DiffusionCoefficient_clone(DiffusionCoefficient_t * dc);


/**
 * Returns the value of the "variable" attribute of the given DiffusionCoefficient_t
 * structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return the variable of this structure.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
const char *
DiffusionCoefficient_getVariable(const DiffusionCoefficient_t * dc);


/**
 * Returns the value of the "type" attribute of the given DiffusionCoefficient_t
 * structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return the type of this structure.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
DiffusionKind_t
DiffusionCoefficient_getType(const DiffusionCoefficient_t * dc);


/**
 * Returns the value of the "coordinateReference1" attribute of the given DiffusionCoefficient_t
 * structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return the coordinateReference1 of this structure.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
CoordinateKind_t
DiffusionCoefficient_getCoordinateReference1(const DiffusionCoefficient_t * dc);


/**
 * Returns the value of the "coordinateReference2" attribute of the given DiffusionCoefficient_t
 * structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return the coordinateReference2 of this structure.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
CoordinateKind_t
DiffusionCoefficient_getCoordinateReference2(const DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 if the given DiffusionCoefficient_t structure's "variable"
 * is set.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return @c 1 if the "variable" of this DiffusionCoefficient_t structure is
 * set, @c 0 otherwise.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetVariable(const DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 if the given DiffusionCoefficient_t structure's "type"
 * is set.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return @c 1 if the "type" of this DiffusionCoefficient_t structure is
 * set, @c 0 otherwise.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetType(const DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 if the given DiffusionCoefficient_t structure's "coordinateReference1"
 * is set.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return @c 1 if the "coordinateReference1" of this DiffusionCoefficient_t structure is
 * set, @c 0 otherwise.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetCoordinateReference1(const DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 if the given DiffusionCoefficient_t structure's "coordinateReference2"
 * is set.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return @c 1 if the "coordinateReference2" of this DiffusionCoefficient_t structure is
 * set, @c 0 otherwise.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_isSetCoordinateReference2(const DiffusionCoefficient_t * dc);


/**
 * Sets the "variable" attribute of the given DiffusionCoefficient_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs DiffusionCoefficient_unsetVariable() instead.
 *
 * @param dc the DiffusionCoefficient_t structure.
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
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setVariable(DiffusionCoefficient_t * dc, const char * variable);


/**
 * Sets the "type" attribute of the given DiffusionCoefficient_t structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param type the string to which the structures "type" attribute should be
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
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setType(DiffusionCoefficient_t * dc, DiffusionKind_t type);


/**
 * Sets the "coordinateReference1" attribute of the given DiffusionCoefficient_t structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param coordinateReference1 the string to which the structures "coordinateReference1" attribute should be
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
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference1(DiffusionCoefficient_t * dc, CoordinateKind_t coordinateReference1);


/**
 * Sets the "coordinateReference2" attribute of the given DiffusionCoefficient_t structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @param coordinateReference2 the string to which the structures "coordinateReference2" attribute should be
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
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_setCoordinateReference2(DiffusionCoefficient_t * dc, CoordinateKind_t coordinateReference2);


/**
 * Unsets the value of the "variable" attribute of the given 
 *DiffusionCoefficient_t structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetVariable(DiffusionCoefficient_t * dc);


/**
 * Unsets the value of the "type" attribute of the given 
 *DiffusionCoefficient_t structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetType(DiffusionCoefficient_t * dc);


/**
 * Unsets the value of the "coordinateReference1" attribute of the given 
 *DiffusionCoefficient_t structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetCoordinateReference1(DiffusionCoefficient_t * dc);


/**
 * Unsets the value of the "coordinateReference2" attribute of the given 
 *DiffusionCoefficient_t structure.
 *
 * @param dc the DiffusionCoefficient_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_unsetCoordinateReference2(DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given DiffusionCoefficient_t structure have been set.
 *
 * @param dc the DiffusionCoefficient_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_hasRequiredAttributes(const DiffusionCoefficient_t * dc);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given DiffusionCoefficient_t structure have been set.
 *
 * @param dc the DiffusionCoefficient_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of DiffusionCoefficient_t
 */
LIBSBML_EXTERN
int
DiffusionCoefficient_hasRequiredElements(const DiffusionCoefficient_t * dc);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  DiffusionCoefficient_H__  */

