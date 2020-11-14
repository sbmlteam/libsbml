/**
 * @file:   BindingSiteSpeciesType.h
 * @brief:  Implementation of the BindingSiteSpeciesType class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
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
 *
 * @class BindingSiteSpeciesType
 * @sbmlbrief{multi} Extension of MultiSpeciesType to define a binding site.
 *
 * A BindingSiteSpeciesType inherits from MultiSpeciesType, and also is a
 * child of the extended Model (via the MultiModelPlugin). A
 * BindingSiteSpeciesType object defines a binding site, and therefore its
 * instance can further define the bindingStatus attribute and can
 * participate a binding internally and explicitly in an InSpeciesTypeBond
 * object, or externally and implicitly defined by an OutwardBindingSite
 * object. A binding site must be an atomic component which means that a
 * BindingSiteSpeciesType object cannot contain a ListOfSpeciesTypeInstances
 * subobject. Note: In the Multi package, a binding site can only participate
 * in one binding at a time. That means a binding site cannot bind two
 * partners at the same time. The binding relationship is one-to-one.
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
   * Creates a new BindingSiteSpeciesType object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  BindingSiteSpeciesType(unsigned int level      = MultiExtension::getDefaultLevel(),
                         unsigned int version    = MultiExtension::getDefaultVersion(),
                         unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new BindingSiteSpeciesType with the given MultiPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  BindingSiteSpeciesType(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for BindingSiteSpeciesType.
   *
   * @param orig the BindingSiteSpeciesType instance to copy.
   */
  BindingSiteSpeciesType(const BindingSiteSpeciesType& orig);


   /**
   * Assignment operator for BindingSiteSpeciesType.
   *
   * @param rhs the object whose values are used as the basis
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
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "bindingSiteSpeciesType".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_MULTI_BINDING_SITE_SPECIES_TYPE, SBMLMultiTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns @c true if this object has all the required elements.
   *
   * @return @c true if this object has all the elements required by the
   * package specification; otherwise, @c false will be returned.
   */
  virtual bool hasRequiredAttributes() const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);
  /** @endcond */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new BindingSiteSpeciesType_t structure using the given SBML @p level and
 * @p version, and the @p pkgVersion package version.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * BindingSiteSpeciesType_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * BindingSiteSpeciesType_t structure.
 *
 * @param pkgVersion an unsigned int, the version of the package to assign
 * to this BindingSiteSpeciesType_t structure.
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether all the required
 * attributes of the given BindingSiteSpeciesType_t structure have been set.
 *
 * @param bsst the BindingSiteSpeciesType_t structure to check.
 *
 * @return @c 1 (true) if all the required attributes for this
 * structure have been defined, @c 0 (false) otherwise.
 *
 * @memberof BindingSiteSpeciesType_t
 */
LIBSBML_EXTERN
int
BindingSiteSpeciesType_hasRequiredAttributes(const BindingSiteSpeciesType_t * bsst);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  BindingSiteSpeciesType_H__  */

