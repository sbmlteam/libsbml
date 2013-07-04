/**
 * @file    FbcSpeciesPlugin.h
 * @brief   Definition of FbcSpeciesPlugin, the plugin class of
 *          the fbc package for the Species element.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009-2013 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class FbcSpeciesPlugin
 * @ingroup FBC
 * @brief @htmlinclude pkg-marker-fbc.html
 * Implementation of the 'fbc' package extention to the %Species construct.
 */

#ifndef FbcSpeciesPlugin_h
#define FbcSpeciesPlugin_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>

#ifdef __cplusplus

#include <sbml/SBMLErrorLog.h>
#include <sbml/Model.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/fbc/sbml/FluxBound.h>
#include <sbml/packages/fbc/sbml/Objective.h>
#include <sbml/packages/fbc/sbml/GeneAssociation.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN FbcSpeciesPlugin : public SBasePlugin
{
public:

  /**
   * Constructor
   */
  FbcSpeciesPlugin  (const std::string &uri, const std::string &prefix,
                    FbcPkgNamespaces *fbcns);


  /**
   * Copy constructor. Creates a copy of this FbcSpeciesPlugin object.
   */
  FbcSpeciesPlugin (const FbcSpeciesPlugin & orig);


  /**
   * Destroy this object.
   */
  virtual ~FbcSpeciesPlugin  ();


  /**
   * Assignment operator for FbcSpeciesPlugin .
   */
  FbcSpeciesPlugin & operator=(const FbcSpeciesPlugin & orig);


  /**
   * Creates and returns a deep copy of this FbcSpeciesPlugin  object.
   * 
   * @return a (deep) copy of this FbcSpeciesPlugin object
   */
  virtual FbcSpeciesPlugin * clone () const;


  

  virtual bool isSetCharge() const;
  virtual int setCharge(int charge);
  virtual int getCharge() const;
  virtual int unsetCharge();

  virtual bool isSetChemicalFormula() const;
  virtual int setChemicalFormula(const std::string& chemicalFormula);
  virtual const std::string& getChemicalFormula() const;
  virtual int unsetChemicalFormula();
  

  // ---------------------------------------------------------
  //
  // virtual functions (internal implementation) which should
  // be overridden by subclasses.
  //
  // ---------------------------------------------------------

  /** @cond doxygen-libsbml-internal */
  /**
   * Sets the parent SBMLDocument of this plugin object.
   *
   * Subclasses which contain one or more SBase derived elements must
   * override this function.
   *
   * @param d the SBMLDocument object to use
   *
   * @see connectToParent
   * @see enablePackageInternal
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /**
   * Sets the parent SBML object of this plugin object to
   * this object and child elements (if any).
   * (Creates a child-parent relationship by this plugin object)
   *
   * This function is called when this object is created by
   * the parent element.
   * Subclasses must override this this function if they have one
   * or more child elements.Also, SBasePlugin::connectToParent()
   * must be called in the overridden function.
   *
   * @param sbase the SBase object to use
   *
   * @see setSBMLDocument
   * @see enablePackageInternal
   */
  virtual void connectToParent (SBase *sbase);
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /**
   * Enables/Disables the given package with child elements in this plugin
   * object (if any).
   * (This is an internal implementation invoked from
   *  SBase::enablePakcageInternal() function)
   *
   * @note Subclasses in which one or more SBase derived elements are
   * defined must override this function.
   *
   * @see setSBMLDocument
   * @see connectToParent
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);
  /** @endcond */

protected:

  // --------------------------------------------------------
  //
  // overridden virtual functions for reading/writing/checking 
  // elements
  //
  // --------------------------------------------------------

  
  /** @cond doxygen-libsbml-internal */
  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /**
   * Reads the attributes of corresponding package in SBMLDocument element.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygen-libsbml-internal */
  /**
   * Writes the attributes of corresponding package in SBMLDocument element.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;
  /** @endcond */


  /*-- data members --*/

  /** @cond doxygen-libsbml-internal */
  int mCharge;
  bool mIsSetCharge;
  std::string mChemicalFormula;
  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
int
FbcSpeciesPlugin_getCharge(SBasePlugin_t * fbc);


LIBSBML_EXTERN
int
FbcSpeciesPlugin_isSetCharge(SBasePlugin_t * fbc);


LIBSBML_EXTERN
int
FbcSpeciesPlugin_setCharge(SBasePlugin_t * fbc, int charge);


LIBSBML_EXTERN
int
FbcSpeciesPlugin_unsetCharge(SBasePlugin_t * fbc);


LIBSBML_EXTERN
char *
FbcSpeciesPlugin_getChemicalFormula(SBasePlugin_t * fbc);


LIBSBML_EXTERN
int
FbcSpeciesPlugin_isSetChemicalFormula(SBasePlugin_t * fbc);


LIBSBML_EXTERN
int
FbcSpeciesPlugin_setChemicalFormula(SBasePlugin_t * fbc, const char * chemform);


LIBSBML_EXTERN
int
FbcSpeciesPlugin_unsetChemicalFormula(SBasePlugin_t * fbc);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* FbcSpeciesPlugin_h */
