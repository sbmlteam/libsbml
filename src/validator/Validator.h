/**
 * \file    Validator.h
 * \brief   Base class for all SBML Validators
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef Validator_h
#define Validator_h


#ifdef __cplusplus


#include <list>

#include "xml/ParseMessage.h"
#include "ConstraintSet.h"


class Constraint;


class Validator
{
public:

  Validator ();
  virtual ~Validator ();

  /**
   * Initializes this Validator with a set of Constraints.
   *
   * When creating a subclass of Validator, override this method to add
   * your own Constraints.
   */
  virtual void init () = 0;

  /**
   * Adds the given Contraint to this validator.
   */
  void addConstraint (Constraint* c);

  /**
   * Clears the Validator's list of messages.
   *
   * If you are validating multiple SBML documents with the same Validator,
   * call this method after you have processed the list of messages from
   * the last Validation run and before validating the next document.
   */
  void clearMessages ();

  /**
   * @return a list of messages logged during validation.
   */
  const std::list<ParseMessage>& getMessages () const;

  /**
   * Adds the given message to this list of Validators messages.
   */
  void logMessage (const ParseMessage& msg);

  /**
   * Validates the given SBMLDocument.  Error messages logged during
   * validation may be retrieved via <code>getMessages()</code>.
   *
   * @return the number of validation errors that occurred.
   */
  unsigned int validate (const SBMLDocument& d);

  /**
   * Validates the given SBMLDocument.  Error messages logged during
   * validation may be retrieved via <code>getMessages()</code>.
   *
   * @return the number of validation errors that occurred.
   */
  unsigned int validate (const std::string& filename);


protected:


  /* Maintain a separate list of constraints for each SBML type.  This is
   * done so that constraints may be applied efficiently during the
   * validation process.
   */

  ConstraintSet<GlobalConstraint>         mGlobalConstraints;

  ConstraintSet<SBMLDocument>             mSBMLDocumentConstraints;
  ConstraintSet<Model>                    mModelConstraints;
  ConstraintSet<FunctionDefinition>       mFunctionDefinitionConstraints;
  ConstraintSet<UnitDefinition>           mUnitDefinitionConstraints;
  ConstraintSet<Unit>                     mUnitConstraints;
  ConstraintSet<Compartment>              mCompartmentConstraints;
  ConstraintSet<Species>                  mSpeciesConstraints;
  ConstraintSet<Parameter>                mParameterConstraints;
  ConstraintSet<Rule>                     mRuleConstraints;
  ConstraintSet<AlgebraicRule>            mAlgebraicRuleConstraints;
  ConstraintSet<AssignmentRule>           mAssignmentRuleConstraints;
  ConstraintSet<SpeciesConcentrationRule> mSpeciesConcentrationRuleConstraints;
  ConstraintSet<CompartmentVolumeRule>    mCompartmentVolumeRuleConstraints;
  ConstraintSet<ParameterRule>            mParameterRuleConstraints;
  ConstraintSet<RateRule>                 mRateRuleConstraints;
  ConstraintSet<Reaction>                 mReactionConstraints;
  ConstraintSet<KineticLaw>               mKineticLawConstraints;
  ConstraintSet<SimpleSpeciesReference>   mSimpleSpeciesReferenceConstraints;
  ConstraintSet<SpeciesReference>         mSpeciesReferenceConstraints;
  ConstraintSet<ModifierSpeciesReference> mModifierSpeciesReferenceConstraints;
  ConstraintSet<Event>                    mEventConstraints;
  ConstraintSet<EventAssignment>          mEventAssignmentConstraints;

  std::list<ParseMessage> mMessages;


  friend class ValidatingVisitor;
};


#endif  /* __cplusplus */
#endif  /* Validator_h */
