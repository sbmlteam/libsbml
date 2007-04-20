/**
 * @file    UniqueMetaId.cpp
 * @brief   Base class for Id constraints
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <sbml/SBMLTypes.h>
#include "UniqueMetaId.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


static const char* PREAMBLE =
    "Every 'metaid' attribute value must be unique across the set of all "
    "'metaid' values in a model. (References: L2V2 Sections 3.3.1 and "
    "3.1.6.)";


/**
 * Creates a new UniqueMetaId with the given constraint id.
 */
UniqueMetaId::UniqueMetaId (unsigned int id, Validator& v) : TConstraint<Model>(id, v)
{
}


/**
 * Destroys this Constraint.
 */
UniqueMetaId::~UniqueMetaId ()
{
}


/**
 * @return the fieldname to use logging constraint violations.  If not
 * overridden, "metaid" is returned.
 */
const char*
UniqueMetaId::getFieldname ()
{
  return "metaid";
}


/**
 * @return the preamble to use when logging constraint violations.  The
 * preamble will be prepended to each log message.  If not overriden,
 * returns an empty string.
 */
const char*
UniqueMetaId::getPreamble ()
{
  return PREAMBLE;
}


/**
 * Checks that all ids for some given subset of the Model adhere to this
 * Constraint.  Override the doCheck() method to define your own subset.
 */
void
UniqueMetaId::check_ (const Model& m, const Model& object)
{
  doCheck(m);
}


/**
 * @return the typename of the given SBase object.
 */
const char*
UniqueMetaId::getTypename (const SBase& object)
{
  return SBMLTypeCode_toString( object.getTypeCode() );
}


/**
 * Logs a message that the given id (and its corresponding object) have
 * failed to satisfy this constraint.
 */
void
UniqueMetaId::logIdConflict (const std::string& id, const SBase& object)
{
  logFailure(object, getMessage(id, object));
}

/**
 * Resets the state of this GlobalConstraint by clearing its internal
 * list of error messages.
 */
void
UniqueMetaId::reset ()
{
  mMetaIdObjectMap.clear();
}


/**
 * Checks that the id associated with the given object is unique.  If it
 * is not, logIdConflict is called.
 */
void
UniqueMetaId::doCheckMetaId (const SBase& object)
{ 
  if (object.isSetMetaId())
  {
    const string& id = object.getMetaId();

    if (mMetaIdObjectMap.insert( make_pair(id, &object) ).second == false)
    {
      logIdConflict(id, object);
    }
  }
}


/**
 * @return the error message to use when logging constraint violations.
 * This method is called by logFailure.
 *
 * Returns a message that the given id and its corresponding object are
 * in  conflict with an object previously defined.
 */
const string
UniqueMetaId::getMessage (const string& id, const SBase& object)
{
  IdObjectMap::iterator iter = mMetaIdObjectMap.find(id);


  if (iter == mMetaIdObjectMap.end())
  {
    return
      "Internal (but non-fatal) Validator error in "
      "UniqueMetaId::getMessage().  The SBML object with duplicate id was "
      "not found when it came time to construct a descriptive error message.";
  }


  ostringstream msg;
  const SBase&  previous = *(iter->second);


  msg << getPreamble();

  //
  // Example message: 
  //
  // The Compartment id 'cell' conflicts with the previously defined
  // Parameter id 'cell' at line 10.
  //

  msg << "  The " << getTypename(object) << " " << getFieldname()
      << " '" << id << "' conflicts with the previously defined "
      << getTypename(previous) << ' ' << getFieldname()
      << " '" << id << "'";

  if (previous.getLine() != 0)
  {
    msg << " at line " << previous.getLine();
  }

  msg << '.';

  return msg.str();
}
/**
 * Checks that all ids on the following Model objects are unique:
 * FunctionDefinitions, Species, Compartments, global Parameters,
 * Reactions, and Events.
 */
void
UniqueMetaId::doCheck (const Model& m)
{
  unsigned int n, size;

  doCheckMetaId( m );

  size = m.getNumFunctionDefinitions();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getFunctionDefinition(n) );

  size = m.getNumUnitDefinitions();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getUnitDefinition(n) );

  size = m.getNumCompartmentTypes();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getCompartmentType(n) );

  size = m.getNumSpeciesTypes();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getSpeciesType(n) );

  size = m.getNumCompartments();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getCompartment(n) );

  size = m.getNumSpecies();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getSpecies(n) );

  size = m.getNumParameters();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getParameter(n) );

  size = m.getNumInitialAssignments();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getInitialAssignment(n) );

  size = m.getNumRules();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getRule(n) );

  size = m.getNumConstraints();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getConstraint(n) );

  size = m.getNumReactions();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getReaction(n) );

  size = m.getNumEvents();
  for (n = 0; n < size; ++n) doCheckMetaId( *m.getEvent(n) );

  reset();
}
