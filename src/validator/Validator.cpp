/**
 * \file    Validator.cpp
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


#include <algorithm>
#include <functional>

#include "sbml/SBMLTypes.h"
#include "sbml/SBMLVisitor.h"

#include "LocalConstraint.h"
#include "GlobalConstraint.h"

#include "Validator.h"


using namespace std;


//
// NOTE: ConstraintSet, ValidatorConstraints, and ValidatingVisitor used to
// be in separate .cpp and .h files, but in order to link under MSVC6 (the
// compiler doesn't instantiate templates (i.e. generate code), even when
// told explicitly to do so), the classes needed to be combined in a single
// file.
//


// ----------------------------------------------------------------------
// ConstraintSet
// ----------------------------------------------------------------------


/**
 * Logs a ParseMessage to the given Validator.  It's used by
 * Apply<GlobalConstraints>() to log multiple messages on constraint
 * failure.
 */
struct LogMessageTo : public unary_function<const ParseMessage&, void>
{
  LogMessageTo (Validator& v) : validator(v) { }


  void operator() (const ParseMessage& msg)
  {
    validator.logMessage(msg);
  }


  Validator& validator;
};


/**
 * Applies a LocalConstraint<T> to an SBML object of type T.
 */
template <typename T>
struct Apply : public unary_function<LocalConstraint<T>*, void>
{
  Apply (const Model& m, const T& obj, Validator& v) :
    model(m), object(obj), validator(v) { }


  void operator() (LocalConstraint<T>* constraint)
  {
    if (!constraint->holds(model, object))
      validator.logMessage
      (
        ParseMessage(  constraint->getId     ()
                     , constraint->getMessage()
                     , object.getLine        ()
                     , object.getColumn      () )
      );
  }


  const Model& model;
  const T&     object;
  Validator&   validator;
};


/**
 * Applies a GlobalConstraint to an SBML Model.
 */
template <>
struct Apply<GlobalConstraint>: public unary_function<GlobalConstraint*, void>
{
  Apply (const Model& m, Validator& v) : model(m), validator(v) { }


  void operator() (GlobalConstraint* constraint)
  {
    if (!constraint->holds(model))
    {
      const list<ParseMessage>& messages = constraint->getMessages();
      for_each(messages.begin(), messages.end(), LogMessageTo(validator));

      constraint->reset();
    }
  }


  const Model& model;
  Validator&   validator;
};


template <typename T>
class ConstraintSet
{
public:

   ConstraintSet () { }
  ~ConstraintSet () { }


  /**
   * Adds a Constraint to this ConstraintSet.
   */
  void add (LocalConstraint<T>* c)
  {
    mConstraints.push_back(c);
  }

  /**
   * Applies all Constraints in this ConstraintSet to the given SBML object
   * of type T.  Constraint violations are logged to Validator.
   */
  void applyTo (const Model& m, const T& x, Validator& v)
  {
    for_each(mConstraints.begin(), mConstraints.end(), Apply<T>(m, x, v));
  }

  /**
   * @return true if this ConstraintSet is empty, false otherwise.
   */
  bool empty () const
  {
    return mConstraints.empty();
  }


protected:

  std::list< LocalConstraint<T>* > mConstraints;


  friend class Validator;
};


template <>
class ConstraintSet<GlobalConstraint>
{
public:

   ConstraintSet () { }
  ~ConstraintSet () { }


  /**
   * Adds a GlobalConstraint to this ConstraintSet.
   */
  void add (GlobalConstraint* c)
  {
    mConstraints.push_back(c);
  }

  /**
   * Applies all GlobalConstraints in this ConstraintSet to the given SBML
   * Model.  Constraint violations are logged to Validator.
   */
  void applyTo (const Model& m, Validator& v)
  {
    Apply<GlobalConstraint> apply(m, v);
    for_each( mConstraints.begin(), mConstraints.end(), apply);
  }

  /**
   * @return true if this ConstraintSet is empty, false otherwise.
   */
  bool empty () const
  {
    return mConstraints.empty();
  }


protected:

  std::list<GlobalConstraint*> mConstraints;


  friend class Validator;
};

// ----------------------------------------------------------------------




// ----------------------------------------------------------------------
// ValidatorConstraints
// ----------------------------------------------------------------------

/**
 * ValidatorConstraints maintain a separate list of constraints for each
 * SBML type.  This is done so that constraints may be applied efficiently
 * during the validation process.
 */
struct ValidatorConstraints
{
  ConstraintSet<GlobalConstraint>         mGlobal;
  ConstraintSet<SBMLDocument>             mSBMLDocument;
  ConstraintSet<Model>                    mModel;
  ConstraintSet<FunctionDefinition>       mFunctionDefinition;
  ConstraintSet<UnitDefinition>           mUnitDefinition;
  ConstraintSet<Unit>                     mUnit;
  ConstraintSet<Compartment>              mCompartment;
  ConstraintSet<Species>                  mSpecies;
  ConstraintSet<Parameter>                mParameter;
  ConstraintSet<Rule>                     mRule;
  ConstraintSet<AlgebraicRule>            mAlgebraicRule;
  ConstraintSet<AssignmentRule>           mAssignmentRule;
  ConstraintSet<SpeciesConcentrationRule> mSpeciesConcentrationRule;
  ConstraintSet<CompartmentVolumeRule>    mCompartmentVolumeRule;
  ConstraintSet<ParameterRule>            mParameterRule;
  ConstraintSet<RateRule>                 mRateRule;
  ConstraintSet<Reaction>                 mReaction;
  ConstraintSet<KineticLaw>               mKineticLaw;
  ConstraintSet<SimpleSpeciesReference>   mSimpleSpeciesReference;
  ConstraintSet<SpeciesReference>         mSpeciesReference;
  ConstraintSet<ModifierSpeciesReference> mModifierSpeciesReference;
  ConstraintSet<Event>                    mEvent;
  ConstraintSet<EventAssignment>          mEventAssignment;

  void add (Constraint* c);
};


/**
 * Adds the given Contraint to the appropriate ConstraintSet.
 */
void
ValidatorConstraints::add (Constraint* c)
{
  if (dynamic_cast<GlobalConstraint*>(c))
  {
    mGlobal.add( static_cast<GlobalConstraint*>(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<SBMLDocument>* >(c))
  {
    mSBMLDocument.add( static_cast< LocalConstraint<SBMLDocument>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<Model>* >(c))
  {
    mModel.add( static_cast< LocalConstraint<Model>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<FunctionDefinition>* >(c))
  {
    mFunctionDefinition.add
    (
      static_cast< LocalConstraint<FunctionDefinition>* >(c)
    );
    return;
  }

  if (dynamic_cast< LocalConstraint<UnitDefinition>* >(c))
  {
    mUnitDefinition.add( static_cast< LocalConstraint<UnitDefinition>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<Unit>* >(c))
  {
    mUnit.add( static_cast< LocalConstraint<Unit>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<Compartment>* >(c))
  {
    mCompartment.add( static_cast< LocalConstraint<Compartment>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<Species>* >(c))
  {
    mSpecies.add( static_cast< LocalConstraint<Species>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<Parameter>* >(c))
  {
    mParameter.add( static_cast< LocalConstraint<Parameter>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<Rule>* >(c))
  {
    mRule.add( static_cast< LocalConstraint<Rule>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<AlgebraicRule>* >(c))
  {
    mAlgebraicRule.add( static_cast< LocalConstraint<AlgebraicRule>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<AssignmentRule>* >(c))
  {
    mAssignmentRule.add( static_cast< LocalConstraint<AssignmentRule>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<SpeciesConcentrationRule>* >(c))
  {
    mSpeciesConcentrationRule.add
    (
      static_cast< LocalConstraint<SpeciesConcentrationRule>* >(c)
    );
    return;
  }

  if (dynamic_cast< LocalConstraint<CompartmentVolumeRule>* >(c))
  {
    mCompartmentVolumeRule.add
    (
      static_cast< LocalConstraint<CompartmentVolumeRule>* >(c)
    );
    return;
  }

  if (dynamic_cast< LocalConstraint<ParameterRule>* >(c))
  {
    mParameterRule.add( static_cast< LocalConstraint<ParameterRule>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<RateRule>* >(c))
  {
    mRateRule.add( static_cast< LocalConstraint<RateRule>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<Reaction>* >(c))
  {
    mReaction.add( static_cast< LocalConstraint<Reaction>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<KineticLaw>* >(c))
  {
    mKineticLaw.add( static_cast< LocalConstraint<KineticLaw>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<SimpleSpeciesReference>* >(c))
  {
    mSimpleSpeciesReference.add
    (
      static_cast< LocalConstraint<SimpleSpeciesReference>* >(c)
    );
    return;
  }

  if (dynamic_cast< LocalConstraint<SpeciesReference>* >(c))
  {
    mSpeciesReference.add
    (
      static_cast< LocalConstraint<SpeciesReference>* >(c)
    );
    return;
  }

  if (dynamic_cast< LocalConstraint<ModifierSpeciesReference>* >(c))
  {
    mModifierSpeciesReference.add
    (
      static_cast< LocalConstraint<ModifierSpeciesReference>* >(c)
    );
    return;
  }

  if (dynamic_cast< LocalConstraint<Event>* >(c))
  {
    mEvent.add( static_cast< LocalConstraint<Event>* >(c) );
    return;
  }

  if (dynamic_cast< LocalConstraint<EventAssignment>* >(c))
  {
    mEventAssignment.add( static_cast< LocalConstraint<EventAssignment>* >(c) );
    return;
  }
}

// ----------------------------------------------------------------------




// ----------------------------------------------------------------------
// ValidatingVisitor
// ----------------------------------------------------------------------


/**
 * An SBMLVisitor visits each object in an SBML object tree, calling the
 * appropriate visit() method for the object visited.
 *
 * A ValidatingVisitor overrides each visit method to validate the given
 * SBML object.
 */
class ValidatingVisitor: public SBMLVisitor
{
public:

  ValidatingVisitor (Validator& v, const Model& m) : v(v), m(m) { }


  void visit (const SBMLDocument& x)
  {
    v.mConstraints->mGlobal      .applyTo(m, v);
    v.mConstraints->mSBMLDocument.applyTo(m, x, v);
  }


  void visit (const Model& x)
  {
    v.mConstraints->mModel.applyTo(m, x, v);
  }


  void visit (const KineticLaw& x)
  {
    v.mConstraints->mKineticLaw.applyTo(m, x, v);
  }


  /*
   * Visit methods should return true if the more objects of the given type
   * should be visited and false otherwise.
   *
   * For example, it doesn't make sense to keep visiting additional
   * FunctionDefinition objects in the SBML object tree if no
   * FunctionDefinition constraints have been added to the validator. ...
   */

  bool visit (const FunctionDefinition& x)
  {
    v.mConstraints->mFunctionDefinition.applyTo(m, x, v);
    return !v.mConstraints->mFunctionDefinition.empty();
  }


  /*
   * ... In this case, even if there are no UnitDefinitionConstraints,
   * UnitDefinitions will still need to be visited to ensure that
   * subordinate Unit objects are visited.  If no UnitDefinition or Unit
   * constraints exist, then its okay to skip further UnitDefinitions.
   */

  bool visit (const UnitDefinition& x)
  {
    v.mConstraints->mUnitDefinition.applyTo(m, x, v);

    return
      !v.mConstraints->mUnitDefinition.empty() ||
      !v.mConstraints->mUnit          .empty();
  }


  bool visit (const Unit& x)
  {
    v.mConstraints->mUnit.applyTo(m, x, v);
    return !v.mConstraints->mUnit.empty();
  }


  bool visit (const Compartment &x)
  {
    v.mConstraints->mCompartment.applyTo(m, x, v);
    return !v.mConstraints->mCompartment.empty();
  }


  bool visit (const Species& x)
  {
    v.mConstraints->mSpecies.applyTo(m, x, v);
    return !v.mConstraints->mSpecies.empty();
  }


  bool visit (const Parameter& x)
  {
    v.mConstraints->mParameter.applyTo(m, x, v);
    return !v.mConstraints->mParameter.empty();
  }


  bool visit (const AlgebraicRule& x)
  {
    const Rule& r = static_cast<const Rule&>(x);


    v.mConstraints->mRule         .applyTo(m, r, v);
    v.mConstraints->mAlgebraicRule.applyTo(m, x, v);

    return true;
  }


  bool visit (const AssignmentRule& x)
  {
    const Rule& r = static_cast<const Rule&>(x);


    v.mConstraints->mRule          .applyTo(m, r, v);
    v.mConstraints->mAssignmentRule.applyTo(m, x, v);

    return true;
  }


  bool visit (const SpeciesConcentrationRule& x)
  {
    const Rule& r = static_cast<const Rule&>(x);


    v.mConstraints->mRule                    .applyTo(m, r, v);
    v.mConstraints->mSpeciesConcentrationRule.applyTo(m, x, v);

    return true;
  }


  bool visit (const CompartmentVolumeRule& x)
  {
    const Rule& r = static_cast<const Rule&>(x);


    v.mConstraints->mRule                 .applyTo(m, r, v);
    v.mConstraints->mCompartmentVolumeRule.applyTo(m, x, v);

    return true;
  }


  bool visit (const ParameterRule& x)
  {
    const Rule& r = static_cast<const Rule&>(x);


    v.mConstraints->mRule         .applyTo(m, r, v);
    v.mConstraints->mParameterRule.applyTo(m, x, v);

    return true;
  }


  bool visit (const RateRule& x)
  {
    const Rule& r = static_cast<const Rule&>(x);


    v.mConstraints->mRule    .applyTo(m, r, v);
    v.mConstraints->mRateRule.applyTo(m, x, v);

    return true;
  }


  bool visit (const Reaction& x)
  {
    v.mConstraints->mReaction.applyTo(m, x, v);
    return true;
  }


  bool visit (const SpeciesReference& x)
  {
    const SimpleSpeciesReference& s =
      static_cast<const SimpleSpeciesReference&>(x);

    v.mConstraints->mSimpleSpeciesReference.applyTo(m, s, v);
    v.mConstraints->mSpeciesReference      .applyTo(m, x, v);

    return
      !v.mConstraints->mSimpleSpeciesReference.empty() ||
      !v.mConstraints->mSpeciesReference      .empty();
  }


  bool visit (const ModifierSpeciesReference& x)
  {
    const SimpleSpeciesReference& s =
      static_cast<const SimpleSpeciesReference&>(x);


    v.mConstraints->mSimpleSpeciesReference  .applyTo(m, s, v);
    v.mConstraints->mModifierSpeciesReference.applyTo(m, x, v);

    return
      !v.mConstraints->mSimpleSpeciesReference  .empty() ||
      !v.mConstraints->mModifierSpeciesReference.empty();
  }


  bool visit (const Event& x)
  {
    v.mConstraints->mEvent.applyTo(m, x, v);

    return
      !v.mConstraints->mEvent          .empty() ||
      !v.mConstraints->mEventAssignment.empty();
  }


  bool visit (const EventAssignment& x)
  {
    v.mConstraints->mEventAssignment.applyTo(m, x, v);
    return !v.mConstraints->mEventAssignment.empty();
  }


protected:

  Validator&   v;
  const Model& m;
};


// ----------------------------------------------------------------------




// ----------------------------------------------------------------------
// Validator
// ----------------------------------------------------------------------


Validator::Validator ()
{
  mConstraints = new ValidatorConstraints();
}


Validator::~Validator ()
{
  delete mConstraints;
}


/**
 * Adds the given Contraint to this validator.
 */
void
Validator::addConstraint (Constraint* c)
{
  mConstraints->add(c);
}


/**
 * Clears the Validator's list of messages.
 *
 * If you are validating multiple SBML documents with the same Validator,
 * call this method after you have processed the list of messages from the
 * last Validation run and before validating the next document.
 */
void
Validator::clearMessages ()
{
  mMessages.clear();
}


/**
 * @return a list of messages logged during validation.
 */
const std::list<ParseMessage>&
Validator::getMessages () const
{
  return mMessages;
}


/**
 * Adds the given message to this list of Validators messages.
 */
void
Validator::logMessage (const ParseMessage& msg)
{
  mMessages.push_back(msg);
}


/**
 * Validates the given SBMLDocument.  Error messages logged during
 * validation may be retrieved via <code>getMessages()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
Validator::validate (const SBMLDocument& d)
{
  using namespace std;
  Model* m = const_cast<SBMLDocument&>(d).getModel();

  if (m != NULL)
  {
    ValidatingVisitor vv(*this, *m);
    d.accept(vv);
  }

  return mMessages.size();
}


/**
 * Validates the given SBMLDocument.  Error messages logged during
 * validation may be retrieved via <code>getMessages()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
Validator::validate (const std::string& filename)
{
  SBMLReader    reader;
  SBMLDocument& d = *reader.readSBML(filename);

  return validate(d);
}

// ----------------------------------------------------------------------
