/**
 * @file    RateRule.h
 * @brief   Definitions of RateRule.
 * @author  Ben Bornstein
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
 * 
 * @class RateRule
 * @sbmlbrief{core} Implementation of %SBML's %RateRule construct.
 *
 * The rule type RateRule is derived from the parent class Rule.  It is
 * used to express equations that determine the rates of change of
 * variables.  The left-hand side (the "variable" attribute) can refer to
 * the identifier of a species, compartment, or parameter (but not a
 * reaction).  The entity identified must have its "constant" attribute set
 * to @c false.  The effects of a RateRule are in general terms the same,
 * but differ in the precise details depending on which variable is being
 * set:
 * 
 * <ul> <li> <em>In the case of a species</em>, a RateRule sets the rate of
 * change of the species' quantity (<em>concentration</em> or <em>amount of
 * substance</em>) to the value determined by the formula in the "math"
 * subelement of the RateRule object.  The overall units of the formula in
 * "math" @em should (in SBML Level&nbsp;2 Version&nbsp;4 and in SBML Level&nbsp;3) or @em
 * must (in SBML releases prior to Level&nbsp;2 version&nbsp;4) be equal to
 * the unit of <em>species quantity</em> divided by the model-wide unit of
 * <em>time</em>.  <em>Restrictions</em>: There must not be both a RateRule
 * "variable" attribute and a SpeciesReference "species" attribute having
 * the same value, unless that species has its "boundaryCondition"
 * attribute is set to @c true.  This means a rate rule cannot be defined
 * for a species that is created or destroyed in a reaction, unless that
 * species is defined as a boundary condition in the model.
 *
 * <li> (For SBML Level&nbsp;3 only) <em>In the case of a species
 * reference</em>, a RateRule sets the rate of change of the stoichiometry
 * of the referenced reactant or product to the value determined by the
 * formula in "math".  The unit associated with the value produced by the
 * "math" formula should be consistent with the unit "dimensionless"
 * divided by the model-wide unit of <em>time</em>.
 *
 * <li> <em>In the case of a compartment</em>, a RateRule sets the rate of
 * change of the compartment's size to the value determined by the formula
 * in the "math" subelement of the RateRule object.  The overall units of
 * the formula @em should (in SBML Level&nbsp;2 Version&nbsp;4 and in SBML
 * Level&nbsp;3) or @em must (in SBML releases prior to Level&nbsp;2
 * version&nbsp;4) be the units of the compartment's <em>size</em> divided
 * by the model-wide unit of <em>time</em>.
 *
 * <li> <em>In the case of a parameter</em>, a RateRule sets the rate of
 * change of the parameter's value to that determined by the formula in the
 * "math" subelement of the RateRule object.  The overall units of the
 * formula @em should (in SBML Level&nbsp;2 Version&nbsp;4 and in SBML
 * Level&nbsp;3) or @em must (in SBML releases prior to Level&nbsp;2
 * version&nbsp;4) be the Parameter object's "unit" attribute value divided
 * by the model-wide unit of <em>time</em>.  </ul>
 * 
 * In the context of a simulation, rate rules are in effect for simulation
 * time <em>t</em> &lt; <em>0</em>.  Please consult the relevant SBML
 * specification for additional information about the semantics of
 * assignments, rules, and entity values for simulation time <em>t</em>
 * \f$\leq\f$ <em>0</em>.
 *
 * As mentioned in the description of AssignmentRule, a model must not
 * contain more than one RateRule or AssignmentRule object having the same
 * value of "variable"; in other words, in the set of all assignment rules
 * and rate rules in an SBML model, each variable appearing in the
 * left-hand sides can only appear once.  This simply follows from the fact
 * that an indeterminate system would result if a model contained more than
 * one assignment rule for the same variable or both an assignment rule and
 * a rate rule for the same variable.
 * 
 * @copydetails doc_rules_general_summary
 */ 

/**
 * <!-- ~ ~ ~ ~ ~ Start of common documentation strings ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 * The following text is used as common documentation blocks copied multiple
 * times elsewhere in this file.  The use of @class is a hack needed because
 * Doxygen's @copydetails command has limited functionality.  Symbols
 * beginning with "doc_" are marked as ignored in our Doxygen configuration.
 * ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~  -->
 *
 * @class doc_note_raterule_setting_lv
 *
 * @note Upon the addition of an RateRule object to an SBMLDocument
 * (e.g., using Model::addRule(@if java Rule r@endif)), the SBML Level, SBML
 * Version and XML namespace of the document @em override the values used
 * when creating the RateRule object via the RateRule constructors.
 * This is necessary to ensure that an SBML document has a consistent overall
 * structure.  Nevertheless, the ability to supply the values at the time of
 * creation of a RateRule is an important aid to producing valid SBML.
 * Knowledge of the intented SBML Level and Version determine whether it is
 * valid to assign a particular value to an attribute, or whether it is valid
 * to add an object to an existing SBMLDocument.
 * 
 */

#ifndef RateRule_h
#define RateRule_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/Rule.h>
#include <sbml/SBMLVisitor.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLNamespaces;


class LIBSBML_EXTERN RateRule : public Rule
{
public:

  /**
   * Creates a new RateRule using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this RateRule
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * RateRule
   *
   * @throws @if python ValueError @else SBMLConstructorException @endif@~
   * Thrown if the given @p level and @p version combination, or this kind
   * of SBML object, are either invalid or mismatched with respect to the
   * parent SBMLDocument object.
   *
   * @copydetails doc_note_raterule_setting_lv
   */
  RateRule (unsigned int level, unsigned int version);


  /**
   * Creates a new RateRule using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * @copydetails doc_what_are_sbmlnamespaces 
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @throws @if python ValueError @else SBMLConstructorException @endif@~
   * Thrown if the given @p level and @p version combination, or this kind
   * of SBML object, are either invalid or mismatched with respect to the
   * parent SBMLDocument object.
   *
   * @copydetails doc_note_raterule_setting_lv
   *
   */
  RateRule (SBMLNamespaces* sbmlns);


  /**
   * Destroys this RateRule.
   */
  virtual ~RateRule ();

  /**
   * Creates and returns a deep copy of this Rule.
   * 
   * @return a (deep) copy of this Rule.
   */
  virtual RateRule* clone () const;


  /**
   * Accepts the given SBMLVisitor.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next RateRule object
   * in the list of rules within which @em the present object is embedded.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Predicate returning @c true if
   * all the required attributes for this RateRule object
   * have been set.
   *
   * @note In SBML Levels&nbsp;2&ndash;3, the only required attribute for a
   * RateRule object is "variable".  For Level&nbsp;1, where the equivalent
   * attribute is known by different names ("compartment", "species", or
   * "name", depending on the type of object), there is an additional
   * required attribute called "formula".
   *
   * @return @c true if the required attributes have been set, @c false
   * otherwise.
   */
  virtual bool hasRequiredAttributes() const ;


  /**
   * Renames all the @c SIdRef attributes on this element, including any
   * found in MathML.
   *
   * @copydetails doc_what_is_sidref
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


protected:
  /** @cond doxygenLibsbmlInternal */

  /* the validator classes need to be friends to access the 
   * protected constructor that takes no arguments
   */
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
  friend class L1CompatibilityValidator;
  friend class L2v1CompatibilityValidator;
  friend class L2v2CompatibilityValidator;
  friend class L2v3CompatibilityValidator;
  friend class L2v4CompatibilityValidator;
  friend class MathMLConsistencyValidator;
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;

  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG  */
#endif  /* RateRule_h */

