/**
 * @file:   TransformationComponents.h
 * @brief:  Implementation of the TransformationComponents class
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


#ifndef TransformationComponents_H__
#define TransformationComponents_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN TransformationComponents : public SBase
{

protected:

  double*         mComponents;
  int           mComponentsLength;
  bool          mIsSetComponentsLength;


  std::string   mElementName;

public:

  /**
   * Creates a new TransformationComponents with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this TransformationComponents
   *
   * @param version an unsigned int, the SBML Version to assign to this TransformationComponents
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this TransformationComponents
   */
  TransformationComponents(unsigned int level      = SpatialExtension::getDefaultLevel(),
                           unsigned int version    = SpatialExtension::getDefaultVersion(),
                           unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new TransformationComponents with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  TransformationComponents(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for TransformationComponents.
   *
   * @param orig; the TransformationComponents instance to copy.
   */
  TransformationComponents(const TransformationComponents& orig);


   /**
   * Assignment operator for TransformationComponents.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  TransformationComponents& operator=(const TransformationComponents& rhs);


   /**
   * Creates and returns a deep copy of this TransformationComponents object.
   *
   * @return a (deep) copy of this TransformationComponents object.
   */
  virtual TransformationComponents* clone () const;


   /**
   * Destructor for TransformationComponents.
   */
  virtual ~TransformationComponents();


   /**
   * The "components" attribute of this TransformationComponents is returned in an double* array (pointer)
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * NOTE: you have to pre-allocate the array with the correct length!   *
   * @return void.
   */
  void getComponents(double* outArray) const;


  /**
   * Returns the value of the "componentsLength" attribute of this TransformationComponents.
   *
   * @return the value of the "componentsLength" attribute of this TransformationComponents as a integer.
   */
  virtual int getComponentsLength() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * TransformationComponents's "components" attribute has been set.
   *
   * @return @c true if this TransformationComponents's "components" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetComponents() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * TransformationComponents's "componentsLength" attribute has been set.
   *
   * @return @c true if this TransformationComponents's "componentsLength" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetComponentsLength() const;


  /**
   * Sets the "components" element of this TransformationComponents.
   *
   * @param inArray; double* array to be set (it will be copied).
   * @param arrayLength; the length of the array.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setComponents(double* inArray, int arrayLength);


  /**
   * Sets the value of the "componentsLength" attribute of this TransformationComponents.
   *
   * @param componentsLength; int value of the "componentsLength" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setComponentsLength(int componentsLength);


  /**
   * Unsets the value of the "components" attribute of this TransformationComponents.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetComponents();


  /**
   * Unsets the value of the "componentsLength" attribute of this TransformationComponents.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetComponentsLength();


  /**
   * Returns the XML element name of this object, which for TransformationComponents, is
   * always @c "transformationComponents".
   *
   * @return the name of this element, i.e. @c "transformationComponents".
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
   * for this TransformationComponents object have been set.
   *
   * @note The required attributes for a TransformationComponents object are:
   * @li "components"
   * @li "componentsLength"
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


  virtual void write(XMLOutputStream& stream) const;


  virtual void setElementName(const std::string& name);


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


  virtual void setElementText(const std::string &text);



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new TransformationComponents_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * TransformationComponents_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * TransformationComponents_t structure.
 *
 * @returns the newly-created TransformationComponents_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof TransformationComponents_t
 */
LIBSBML_EXTERN
TransformationComponents_t *
TransformationComponents_create(unsigned int level, unsigned int version,
                                unsigned int pkgVersion);


/**
 * Frees the given TransformationComponents_t structure.
 * 
 * @param tc the TransformationComponents_t structure to be freed.
 *
 * @memberof TransformationComponents_t
 */
LIBSBML_EXTERN
void
TransformationComponents_free(TransformationComponents_t * tc);


/**
 * Creates a deep copy of the given TransformationComponents_t structure.
 * 
 * @param tc the TransformationComponents_t structure to be copied.
 *
 * @returns a (deep) copy of the given TransformationComponents_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof TransformationComponents_t
 */
LIBSBML_EXTERN
TransformationComponents_t *
TransformationComponents_clone(TransformationComponents_t * tc);


/**
 * Returns the value of the "components" attribute of the given TransformationComponents_t
 * structure.
 *
 * @param tc the TransformationComponents_t structure.
 *
 * @return the components of this structure.
 *
 * @member of TransformationComponents_t
 */
LIBSBML_EXTERN
double*
TransformationComponents_getComponents(const TransformationComponents_t * tc);


/**
 * Returns the value of the "componentsLength" attribute of the given TransformationComponents_t
 * structure.
 *
 * @param tc the TransformationComponents_t structure.
 *
 * @return the componentsLength of this structure.
 *
 * @member of TransformationComponents_t
 */
LIBSBML_EXTERN
int
TransformationComponents_getComponentsLength(const TransformationComponents_t * tc);


/**
 * Predicate returning @c 1 if the given TransformationComponents_t structure's "components"
 * is set.
 *
 * @param tc the TransformationComponents_t structure.
 *
 * @return @c 1 if the "components" of this TransformationComponents_t structure is
 * set, @c 0 otherwise.
 *
 * @member of TransformationComponents_t
 */
LIBSBML_EXTERN
int
TransformationComponents_isSetComponents(const TransformationComponents_t * tc);


/**
 * Predicate returning @c 1 if the given TransformationComponents_t structure's "componentsLength"
 * is set.
 *
 * @param tc the TransformationComponents_t structure.
 *
 * @return @c 1 if the "componentsLength" of this TransformationComponents_t structure is
 * set, @c 0 otherwise.
 *
 * @member of TransformationComponents_t
 */
LIBSBML_EXTERN
int
TransformationComponents_isSetComponentsLength(const TransformationComponents_t * tc);


/**
 * Sets the "components" attribute of the given TransformationComponents_t structure.
 *
 * @param tc the TransformationComponents_t structure.
 *
 * @param components the string to which the structures "components" attribute should be
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
 * @member of TransformationComponents_t
 */
LIBSBML_EXTERN
int
TransformationComponents_setComponents(TransformationComponents_t * tc, double* components);


/**
 * Sets the "componentsLength" attribute of the given TransformationComponents_t structure.
 *
 * @param tc the TransformationComponents_t structure.
 *
 * @param componentsLength the string to which the structures "componentsLength" attribute should be
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
 * @member of TransformationComponents_t
 */
LIBSBML_EXTERN
int
TransformationComponents_setComponentsLength(TransformationComponents_t * tc, int componentsLength);


/**
 * Unsets the value of the "components" attribute of the given 
 *TransformationComponents_t structure.
 *
 * @param tc the TransformationComponents_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of TransformationComponents_t
 */
LIBSBML_EXTERN
int
TransformationComponents_unsetComponents(TransformationComponents_t * tc);


/**
 * Unsets the value of the "componentsLength" attribute of the given 
 *TransformationComponents_t structure.
 *
 * @param tc the TransformationComponents_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of TransformationComponents_t
 */
LIBSBML_EXTERN
int
TransformationComponents_unsetComponentsLength(TransformationComponents_t * tc);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given TransformationComponents_t structure have been set.
 *
 * @param tc the TransformationComponents_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of TransformationComponents_t
 */
LIBSBML_EXTERN
int
TransformationComponents_hasRequiredAttributes(const TransformationComponents_t * tc);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  TransformationComponents_H__  */

