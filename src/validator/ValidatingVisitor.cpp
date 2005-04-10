/**
 * \file    ValidatingVisitor.cpp
 * \brief   Visits (and validates) each object in an SBML object tree
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


#include "sbml/SBMLTypes.h"

#include "Validator.h"
#include "ValidatingVisitor.h"


ValidatingVisitor::ValidatingVisitor (Validator& v, const Model& m) :
  mValidator(v), mModel(m)
{
}


ValidatingVisitor::~ValidatingVisitor ()
{
}


void
ValidatingVisitor::visit (const SBMLDocument& x)
{
  mValidator.mGlobalConstraints      .applyTo(mModel, mValidator);
  mValidator.mSBMLDocumentConstraints.applyTo(mModel, x, mValidator);
}


void
ValidatingVisitor::visit (const Model& x)
{
  mValidator.mModelConstraints.applyTo(mModel, x, mValidator);
}


void
ValidatingVisitor::visit (const KineticLaw& x)
{
  mValidator.mKineticLawConstraints.applyTo(mModel, x, mValidator);
}


/*
 * Visit methods should return true if the more objects of the given type
 * should be visited and false otherwise.
 *
 * For example, it doesn't make sense to keep visiting additional
 * FunctionDefinition objects in the SBML object tree if no
 * FunctionDefinition constraints have been added to the validator. ...
 */

bool
ValidatingVisitor::visit (const FunctionDefinition& x)
{
  mValidator.mFunctionDefinitionConstraints.applyTo(mModel, x, mValidator);

  return
    !mValidator.mFunctionDefinitionConstraints.empty();
}


/*
 * ... In this case, even if there are no UnitDefinitionConstraints,
 * UnitDefinitions will still need to be visited to ensure that subordinate
 * Unit objects are visited.  If no UnitDefinition or Unit constraints
 * exist, then its okay to skip further UnitDefinitions.
 */

bool
ValidatingVisitor::visit (const UnitDefinition& x)
{
  mValidator.mUnitDefinitionConstraints.applyTo(mModel, x, mValidator);

  return
    !mValidator.mUnitDefinitionConstraints.empty() ||
    !mValidator.mUnitConstraints          .empty();
}


bool
ValidatingVisitor::visit (const Unit& x)
{
  mValidator.mUnitConstraints.applyTo(mModel, x, mValidator);

  return
    !mValidator.mUnitConstraints.empty();
}


bool
ValidatingVisitor::visit (const Compartment& x)
{
  mValidator.mCompartmentConstraints.applyTo(mModel, x, mValidator);

  return
    !mValidator.mCompartmentConstraints.empty();
}


bool
ValidatingVisitor::visit (const Species& x)
{
  mValidator.mSpeciesConstraints.applyTo(mModel, x, mValidator);

  return
    !mValidator.mSpeciesConstraints.empty();
}


bool
ValidatingVisitor::visit (const Parameter& x)
{
  mValidator.mParameterConstraints.applyTo(mModel, x, mValidator);

  return
    !mValidator.mParameterConstraints.empty();
}


bool
ValidatingVisitor::visit (const AlgebraicRule& x)
{
  const Rule& r = static_cast<const Rule&>(x);


  mValidator.mRuleConstraints         .applyTo(mModel, r, mValidator);
  mValidator.mAlgebraicRuleConstraints.applyTo(mModel, x, mValidator);

  return true;
}


bool
ValidatingVisitor::visit (const AssignmentRule& x)
{
  const Rule& r = static_cast<const Rule&>(x);


  mValidator.mRuleConstraints          .applyTo(mModel, r, mValidator);
  mValidator.mAssignmentRuleConstraints.applyTo(mModel, x, mValidator);

  return true;
}


bool
ValidatingVisitor::visit (const SpeciesConcentrationRule& x)
{
  const Rule& r = static_cast<const Rule&>(x);


  mValidator.mRuleConstraints
    .applyTo(mModel, r, mValidator);

  mValidator.mSpeciesConcentrationRuleConstraints
    .applyTo(mModel, x, mValidator);

  return true;
}


bool
ValidatingVisitor::visit (const CompartmentVolumeRule& x)
{
  const Rule& r = static_cast<const Rule&>(x);


  mValidator.mRuleConstraints                 .applyTo(mModel, r, mValidator);
  mValidator.mCompartmentVolumeRuleConstraints.applyTo(mModel, x, mValidator);

  return true;
}


bool
ValidatingVisitor::visit (const ParameterRule& x)
{
  const Rule& r = static_cast<const Rule&>(x);


  mValidator.mRuleConstraints         .applyTo(mModel, r, mValidator);
  mValidator.mParameterRuleConstraints.applyTo(mModel, x, mValidator);

  return true;
}


bool
ValidatingVisitor::visit (const RateRule& x)
{
  const Rule& r = static_cast<const Rule&>(x);


  mValidator.mRuleConstraints    .applyTo(mModel, r, mValidator);
  mValidator.mRateRuleConstraints.applyTo(mModel, x, mValidator);

  return true;
}


bool
ValidatingVisitor::visit (const Reaction& x)
{
  mValidator.mReactionConstraints.applyTo(mModel, x, mValidator);

  return true;
}


bool
ValidatingVisitor::visit (const SpeciesReference& x)
{
  const SimpleSpeciesReference& s =
    static_cast<const SimpleSpeciesReference&>(x);

  mValidator.mSimpleSpeciesReferenceConstraints.applyTo(mModel, s, mValidator);
  mValidator.mSpeciesReferenceConstraints      .applyTo(mModel, x, mValidator);

  return
    !mValidator.mSimpleSpeciesReferenceConstraints.empty() ||
    !mValidator.mSpeciesReferenceConstraints      .empty();
}


bool
ValidatingVisitor::visit (const ModifierSpeciesReference& x)
{
  const SimpleSpeciesReference& s =
    static_cast<const SimpleSpeciesReference&>(x);


  mValidator.mSimpleSpeciesReferenceConstraints
    .applyTo(mModel, s, mValidator);

  mValidator.mModifierSpeciesReferenceConstraints
    .applyTo(mModel, x, mValidator);

  return
    !mValidator.mSimpleSpeciesReferenceConstraints  .empty() ||
    !mValidator.mModifierSpeciesReferenceConstraints.empty();
}


bool
ValidatingVisitor::visit (const Event& x)
{
  mValidator.mEventConstraints.applyTo(mModel, x, mValidator);

  return
    !mValidator.mEventConstraints          .empty() ||
    !mValidator.mEventAssignmentConstraints.empty();
}


bool
ValidatingVisitor::visit (const EventAssignment& x)
{
  mValidator.mEventAssignmentConstraints.applyTo(mModel, x, mValidator);

  return
    !mValidator.mEventAssignmentConstraints.empty();
}
