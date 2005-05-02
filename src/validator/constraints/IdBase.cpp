/**
 * \file    IdBase.h
 * \brief   Base class for Id constraints
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


#include <string>

#include "sbml/SBMLTypes.h"
#include "IdBase.h"


using namespace std;


/**
 * Creates a new IdBase with the given constraint id.
 */
IdBase::IdBase (unsigned int id) : GlobalConstraint(id)
{
  mHolds = false;
}


/**
 * Destroys this Constraint.
 */
IdBase::~IdBase ()
{
}


/**
 * @return the fieldname to use logging constraint violations.  If not
 * overridden, "id" is returned.
 */
const char*
IdBase::getFieldname ()
{
  return "id";
}


/**
 * @return the preamble to use when logging constraint violations.  The
 * preamble will be prepended to each log message.  If not overriden,
 * returns an empty string.
 */
const char*
IdBase::getPreamble ()
{
  return "";
}


/**
 * Checks that all ids for some given subset of the Model adhere to this
 * Constraint.  Override the doCheck() method to define your own subset.
 *
 * @return true if at all ids adhere to this constriant, false otherwise.
 */
bool
IdBase::check (const Model& m)
{
  doCheck(m);
  return mHolds;
}


/*
 * These convenience methods simply wrap (delegate to) doCheckId(const
 * std::string&, const SBase* object).  This is necessary because getId()
 * is not (yet) defined on SBase for SBML objects.
 *
 * For Rules and EventAssignments, it calls getVariable() instead.
 */


void
IdBase::checkId (const Model* x)
{
  if (x->isSetId()) doCheckId(x->getId(), x);
}


void
IdBase::checkId (const FunctionDefinition* x)
{
  if (x->isSetId()) doCheckId(x->getId(), x);
}


void
IdBase::checkId (const UnitDefinition* x)
{
  if (x->isSetId()) doCheckId(x->getId(), x);
}


void
IdBase::checkId (const Compartment* x)
{
  if (x->isSetId()) doCheckId(x->getId(), x);
}


void
IdBase::checkId (const Species* x)
{
  if (x->isSetId()) doCheckId(x->getId(), x);
}


void
IdBase::checkId (const Parameter* x)
{
  if (x->isSetId()) doCheckId(x->getId(), x);
}


void
IdBase::checkId (const Rule* x)
{
  switch (x->getTypeCode())
  {
    case SBML_ASSIGNMENT_RULE:
      doCheckId(static_cast<const AssignmentRule*>(x)->getVariable(), x);
      break;

    case SBML_RATE_RULE:
      doCheckId(static_cast<const RateRule*>(x)->getVariable(), x);
      break;

    case SBML_SPECIES_CONCENTRATION_RULE:
      doCheckId
      (
        static_cast<const SpeciesConcentrationRule*>(x)->getSpecies(), x
      );
      break;

    case SBML_COMPARTMENT_VOLUME_RULE:
      doCheckId(static_cast<const CompartmentVolumeRule*>(x)->getCompartment(),
              x);
      break;

    case SBML_PARAMETER_RULE:
      doCheckId(static_cast<const ParameterRule*>(x)->getName(), x);
      break;

    default:
      break;
  }
}


void
IdBase::checkId (const Reaction* x)
{
  if (x->isSetId()) doCheckId(x->getId(), x);
}


void
IdBase::checkId (const Event* x)
{
  if (x->isSetId()) doCheckId(x->getId(), x);
}


void
IdBase::checkId (const EventAssignment* x)
{
  if (x->isSetVariable()) doCheckId(x->getVariable(), x);
}


/**
 * @return the typename of the given SBase object.
 */
const char*
IdBase::getTypename (const SBase* object)
{
  return SBMLTypeCode_toString( object->getTypeCode() );
}


/**
 * Logs a message that the given id (and its corresponding object) have
 * failed to satisfy this constraint.
 */
void
IdBase::logFailure (const std::string& id, const SBase* object)
{
  const string& msg = getMessage(id, object);


  //
  // Here getId() refers to this Constraints unique identifier.
  //
  logMessage
  (
    ParseMessage(getId(), msg, object->getLine(), object->getColumn())
  );

  mHolds = false;
}
