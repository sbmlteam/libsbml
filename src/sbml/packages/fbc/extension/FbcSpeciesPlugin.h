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
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class FbcSpeciesPlugin
 * @sbmlbrief{fbc} Extension of Species.
 *
 * The FbcSpeciesPlugin class codifies an extension of the core SBML Species
 * class defined in the SBML Level&nbsp;3 @ref fbc (&ldquo;fbc&rdquo;)
 * package.  The &ldquo;fbc&rdquo; package adds two attributes named "charge"
 * and "chemicalFormula" to Species.
 *
 * The optional attribute "charge" can contain a signed integer that refers
 * to the Species object's electrical charge (in terms of electrons, not the
 * SI unit of coulombs).  Note that this attribute is therefore defined as it
 * is in the SBML Level&nbsp;2 Version&nbsp;1 specification.  (The charge
 * attribute was removed in higher Versions and Levels of SBML, and is not an
 * attribute of SBML Species in core SBML Level&nbsp;3.  However, it is
 * useful for flux balance constraints models, and thus, the Level&nbsp;3
 * &ldquo;fbc&rdquo; package adds it.)
 *
 * The optional attribute "chemicalFormula" can contain a text string that
 * represents the elemental composition of the substance represented by the
 * Species object.  The purpose of the "chemicalFormula" attribute is to
 * allow balancing and validating reactions.  This is particularly important
 * in constraint-based models.  The format of "chemicalFormula" must consist
 * only of atomic names (as given in the Periodic Table of elements) or
 * user-defined compounds, either of which take the form of a single capital
 * letter followed by zero or more lowercase letters.  Where there is more
 * than a single atom present, this is indicated with an integer.  With
 * regards to order (and to enhance interoperability of models), users are
 * advised to employ the <a href="https://en.wikipedia.org/wiki/Hill_system">Hill system order</a>.
 * Using this notation, the number of carbon atoms in a molecule is indicated
 * first, followed by the number of hydrogen atoms, and then the number of
 * all other chemical elements in alphabetical order.  When the formula
 * contains no carbon, all elements including hydrogen are listed
 * alphabetically.
 *
 * Here is an example of the XML form of an extended Species definition with
 * these attributes:
 * @verbatim
<species metaid="meta_M_atp_c" id="M_atp_c" name="ATP" compartment="Cytosol"
boundaryCondition="false" initialConcentration="0" hasOnlySubstanceUnits="false"
fbc:charge="-4" fbc:chemicalFormula="C10H12N5O13P3"/>
@endverbatim
 */


#ifndef FbcSpeciesPlugin_H__
#define FbcSpeciesPlugin_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>

#ifdef __cplusplus

#include <sbml/SBMLErrorLog.h>
#include <sbml/Model.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/fbc/extension/FbcSBasePlugin.h>
#include <sbml/packages/fbc/sbml/FluxBound.h>
#include <sbml/packages/fbc/sbml/Objective.h>
#include <sbml/packages/fbc/sbml/GeneAssociation.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN FbcSpeciesPlugin : public FbcSBasePlugin
{
public:

  /**
   * Creates a new FbcSpeciesPlugin
   */
  FbcSpeciesPlugin(const std::string& uri, const std::string& prefix, 
                                 FbcPkgNamespaces* fbcns);


  /**
   * Copy constructor for FbcSpeciesPlugin.
   *
   * @param orig; the FbcSpeciesPlugin instance to copy.
   */
  FbcSpeciesPlugin(const FbcSpeciesPlugin& orig);


   /**
   * Assignment operator for FbcSpeciesPlugin.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  FbcSpeciesPlugin& operator=(const FbcSpeciesPlugin& rhs);


   /**
   * Creates and returns a deep copy of this FbcSpeciesPlugin object.
   *
   * @return a (deep) copy of this FbcSpeciesPlugin object.
   */
  virtual FbcSpeciesPlugin* clone () const;


   /**
   * Destructor for FbcSpeciesPlugin.
   */
  virtual ~FbcSpeciesPlugin();


   //---------------------------------------------------------------
  //
  // overridden virtual functions for read/write/check
  //
  //---------------------------------------------------------------

  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses must override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream if they have their specific elements.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses must override this method to write out their contained
   * SBML objects as XML elements if they have their specific elements.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  //---------------------------------------------------------------


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


  //---------------------------------------------------------------
  //
  // Functions for interacting with the members of the plugin
  //
  //---------------------------------------------------------------

  /**
   * Returns the value of the "charge" attribute of this FbcSpeciesPlugin.
   *
   * @return the value of the "charge" attribute of this FbcSpeciesPlugin as a integer.
   */
  virtual int getCharge() const;


  /**
  * Returns the value of the "charge" attribute of this FbcSpeciesPlugin.
  *
  * @return the value of the "charge" attribute of this FbcSpeciesPlugin as a double.
  */
  virtual double getChargeAsDouble() const;


  /**
   * Returns the value of the "chemicalFormula" attribute of this FbcSpeciesPlugin.
   *
   * @return the value of the "chemicalFormula" attribute of this FbcSpeciesPlugin as a string.
   */
  virtual const std::string& getChemicalFormula() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * FbcSpeciesPlugin's "charge" attribute has been set.
   *
   * @return @c true if this FbcSpeciesPlugin's "charge" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCharge() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * FbcSpeciesPlugin's "chemicalFormula" attribute has been set.
   *
   * @return @c true if this FbcSpeciesPlugin's "chemicalFormula" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetChemicalFormula() const;


  /**
   * Sets the value of the "charge" attribute of this FbcSpeciesPlugin.
   *
   * @param charge; int value of the "charge" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setCharge(int charge);


  /**
  * Sets the value of the "charge" attribute of this FbcSpeciesPlugin.
  *
  * @param charge; double value of the "charge" attribute to be set
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
  */
  virtual int setCharge(double charge);


  /**
   * Sets the value of the "chemicalFormula" attribute of this FbcSpeciesPlugin.
   *
   * @param chemicalFormula; const std::string& value of the "chemicalFormula" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setChemicalFormula(const std::string& chemicalFormula);


  /**
   * Unsets the value of the "charge" attribute of this FbcSpeciesPlugin.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetCharge();


  /**
   * Unsets the value of the "chemicalFormula" attribute of this FbcSpeciesPlugin.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetChemicalFormula();


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  virtual void connectToParent (SBase* sbase);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  virtual bool accept (SBMLVisitor& v) const;

  /** @endcond doxygenLibsbmlInternal */


  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Returns the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Predicate returning @c true if this FbcSpeciesPlugin's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this FbcSpeciesPlugin's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Sets the value of the "attributeName" attribute of this FbcSpeciesPlugin.
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
   * Unsets the value of the "attributeName" attribute of this
   * FbcSpeciesPlugin.
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

  int           mCharge;
  double        mChargeAsDouble;
  bool          mIsSetCharge;
  std::string   mChemicalFormula;

  /** @endcond doxygenLibsbmlInternal */


};




LIBSBML_CPP_NAMESPACE_END


#endif  /* __cplusplus */
#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Takes a FbcSpeciesPlugin_t structure and returns its charge.
 *
 * @param fbc the FbcSpeciesPlugin_t whose charge is sought.
 *
 * @return the charge attribute of the given FbcSpeciesPlugin_t, as an @c int.
 *
 * @memberof FbcSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
FbcSpeciesPlugin_getCharge(FbcSBasePlugin_t * fbc);


/**
* Takes a FbcSpeciesPlugin_t structure and returns its charge.
*
* @param fbc the FbcSpeciesPlugin_t whose charge is sought.
*
* @return the charge attribute of the given FbcSpeciesPlugin_t, as an @c double.
*
* @memberof FbcSpeciesPlugin_t
*/
LIBSBML_EXTERN
double
FbcSpeciesPlugin_getChargeAsDouble(FbcSBasePlugin_t * fbc);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the given
 * FbcSpeciesPlugin_t structure's charge is set.
 *
 * @param fbc the FbcSpeciesPlugin_t structure to query
 * 
 * @return @c 1 (true) if the "charge" attribute of the given
 * FbcSpeciesPlugin_t structure is set, @c 0 (false) otherwise.
 *
 * @memberof FbcSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
FbcSpeciesPlugin_isSetCharge(FbcSBasePlugin_t * fbc);


/**
 * Sets the "charge" attribute of the given FbcSpeciesPlugin_t
 * structure.
 *
 * @param fbc the FbcSpeciesPlugin_t structure
 * 
 * @param charge the value of charge to assign to the "charge" attribute
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_UNEXPECTED_ATTRIBUTE, OperationReturnValues_t}
 *
 * @memberof FbcSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
FbcSpeciesPlugin_setCharge(FbcSBasePlugin_t * fbc, int charge);


/**
* Sets the "charge" attribute of the given FbcSpeciesPlugin_t
* structure.
*
* @param fbc the FbcSpeciesPlugin_t structure
*
* @param charge the value of charge to assign to the "charge" attribute
*
* @copydetails doc_returns_success_code
* @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
* @li @sbmlconstant{LIBSBML_UNEXPECTED_ATTRIBUTE, OperationReturnValues_t}
*
* @memberof FbcSpeciesPlugin_t
*/
LIBSBML_EXTERN
int
FbcSpeciesPlugin_setChargeAsDouble(FbcSBasePlugin_t * fbc, double charge);


/**
 * Unsets the "charge" attribute of the given FbcSpeciesPlugin_t structure.
 *
 * @param fbc the FbcSpeciesPlugin_t structure to unset
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof FbcSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
FbcSpeciesPlugin_unsetCharge(FbcSBasePlugin_t * fbc);


/**
 * Takes a FbcSpeciesPlugin_t structure and returns its chemicalFormula.
 *
 * @param fbc the FbcSpeciesPlugin_t whose chemicalFormula is sought.
 *
 * @return the chemicalFormula of the given FbcSpeciesPlugin_t, as a pointer to a string.
 *
 * @memberof FbcSpeciesPlugin_t
 */
LIBSBML_EXTERN
char *
FbcSpeciesPlugin_getChemicalFormula(FbcSBasePlugin_t * fbc);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the given
 * FbcSpeciesPlugin_t structure's chemicalFormula is set.
 *
 * @param fbc the FbcSpeciesPlugin_t structure to query
 * 
 * @return @c 1 (true) if the "chemicalFormula" attribute of the given
 * FbcSpeciesPlugin_t structure is set, @c 0 (false) otherwise.
 *
 * @memberof FbcSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
FbcSpeciesPlugin_isSetChemicalFormula(FbcSBasePlugin_t * fbc);


/**
 * Sets the chemicalFormula of the given FbcSpeciesPlugin_t to a copy of @p chemicalFormula.
 *
 * @param fbc the FbcSpeciesPlugin_t structure to set
 * @param chemicalFormula the chemicalFormula to assign to the given FbcSpeciesPlugin_t's "chemicalFormula" attribute.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @note Using this function with the name set to NULL is equivalent to
 * unsetting the "chemicalFormula" attribute.
 *
 * @memberof FbcSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
FbcSpeciesPlugin_setChemicalFormula(FbcSBasePlugin_t * fbc, const char * chemicalFormula);


/**
 * Unsets the "chemicalFormula" attribute of the given FbcSpeciesPlugin_t structure.
 *
 * @param fbc the FbcSpeciesPlugin_t structure to unset
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof FbcSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
FbcSpeciesPlugin_unsetChemicalFormula(FbcSBasePlugin_t * fbc);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif /* FbcSpeciesPlugin_H__ */


