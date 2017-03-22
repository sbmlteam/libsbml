/**
 * @file:   BindingSiteSpeciesType.h
 * @brief:  Implementation of the BindingSiteSpeciesType class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2017 jointly by the following organizations:
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


#ifndef BindingSiteSpeciesType_H__
#define BindingSiteSpeciesType_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>
#include <sbml/packages/multi/sbml/MultiSpeciesType.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN BindingSiteSpeciesType : public MultiSpeciesType
{

protected:



public:

  /**
   * Creates a new BindingSiteSpeciesType with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this BindingSiteSpeciesType
   *
   * @param version an unsigned int, the SBML Version to assign to this BindingSiteSpeciesType
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this BindingSiteSpeciesType
   */
  BindingSiteSpeciesType(unsigned int level      = MultiExtension::getDefaultLevel(),
                         unsigned int version    = MultiExtension::getDefaultVersion(),
                         unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new BindingSiteSpeciesType with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  BindingSiteSpeciesType(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for BindingSiteSpeciesType.
   *
   * @param orig; the BindingSiteSpeciesType instance to copy.
   */
  BindingSiteSpeciesType(const BindingSiteSpeciesType& orig);


   /**
   * Assignment operator for BindingSiteSpeciesType.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  BindingSiteSpeciesType& operator=(const BindingSiteSpeciesType& rhs);


   /**
   * Creates and returns a deep copy of this BindingSiteSpeciesType object.
   *
   * @return a (deep) copy of this BindingSiteSpeciesType object.
   */
  virtual BindingSiteSpeciesType* clone () const;


   /**
   * Destructor for BindingSiteSpeciesType.
   */
  virtual ~BindingSiteSpeciesType();


   /**
   * Returns the XML element name of this object, which for BindingSiteSpeciesType, is
   * always @c "bindingSiteSpeciesType".
   *
   * @return the name of this element, i.e. @c "bindingSiteSpeciesType".
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
   * for this BindingSiteSpeciesType object have been set.
   *
   * @note The required attributes for a BindingSiteSpeciesType object are:
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
 * Creates a new BindingSiteSpeciesType_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * BindingSiteSpeciesType_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * BindingSiteSpeciesType_t structure.
 *
 * @returns the newly-created BindingSiteSpeciesType_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof BindingSiteSpeciesType_t
 */
LIBSBML_EXTERN
BindingSiteSpeciesType_t *
BindingSiteSpeciesType_create(unsigned int level, unsigned int version,
                              unsigned int pkgVersion);


/**
 * Frees the given BindingSiteSpeciesType_t structure.
 * 
 * @param bsst the BindingSiteSpeciesType_t structure to be freed.
 *
 * @memberof BindingSiteSpeciesType_t
 */
LIBSBML_EXTERN
void
BindingSiteSpeciesType_free(BindingSiteSpeciesType_t * bsst);


/**
 * Creates a deep copy of the given BindingSiteSpeciesType_t structure.
 * 
 * @param bsst the BindingSiteSpeciesType_t structure to be copied.
 *
 * @returns a (deep) copy of the given BindingSiteSpeciesType_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof BindingSiteSpeciesType_t
 */
LIBSBML_EXTERN
BindingSiteSpeciesType_t *
BindingSiteSpeciesType_clone(BindingSiteSpeciesType_t * bsst);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given BindingSiteSpeciesType_t structure have been set.
 *
 * @param bsst the BindingSiteSpeciesType_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of BindingSiteSpeciesType_t
 */
LIBSBML_EXTERN
int
BindingSiteSpeciesType_hasRequiredAttributes(const BindingSiteSpeciesType_t * bsst);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  BindingSiteSpeciesType_H__  */

