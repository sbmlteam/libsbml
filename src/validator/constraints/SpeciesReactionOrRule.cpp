/**
 * @file    SpeciesReactionOrRule.cpp
 * @brief   Ensures unique variables assigned by rules and events
 * @author  Sarah Keating
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

#include <sbml/Model.h>
#include <sbml/Rule.h>
#include <sbml/Reaction.h>
#include <sbml/Species.h>

#include "SpeciesReactionOrRule.h"
#include "IdList.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */




/**
 * Creates a new Constraint with the given constraint id.
 */
SpeciesReactionOrRule::SpeciesReactionOrRule (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/**
 * Destroys this Constraint.
 */
SpeciesReactionOrRule::~SpeciesReactionOrRule ()
{
}


/**
  * Checks that any species with boundary condition false
  * is not set by reaction and rules
  */
void
SpeciesReactionOrRule::check_ (const Model& m, const Model& object)
{
  unsigned int n, nr, nsr;
  const Species * s;

  /* populate lists */
  for (n = 0; n < m.getNumRules(); n++)
  {
    const Rule * r = m.getRule(n);
    if (r->isAssignment() || r->isRate())
    {
      mRules.append(r->getVariable());
    }
  }
  /* redid this to get better information in the error message 
  for (n = 0; n < m.getNumReactions(); n++)
  {
    const Reaction * react = m.getReaction(n);

    for (nsr = 0; nsr < react->getNumReactants(); nsr++)
    {
      mReactions.append(react->getReactant(nsr)->getSpecies());
    }

    for (nsr = 0; nsr < react->getNumProducts(); nsr++)
    {
      mReactions.append(react->getProduct(nsr)->getSpecies());
    }
  }
*/
  for (n = 0; n < m.getNumSpecies(); ++n)
  {
    s = m.getSpecies(n);
    const string& id = s->getId();

    if (!s->getBoundaryCondition())
    {
      if (mRules.contains(id))
      {
        for (nr = 0; nr < m.getNumReactions(); nr++)
        {
          const Reaction * react = m.getReaction(nr);

          for (nsr = 0; nsr < react->getNumReactants(); nsr++)
          {
            if (!strcmp(id.c_str(), react->getReactant(nsr)->getSpecies().c_str()))
            {
              logConflict(*s, *react);
            }
          }

          for (nsr = 0; nsr < react->getNumProducts(); nsr++)
          {
            if (!strcmp(id.c_str(), react->getProduct(nsr)->getSpecies().c_str()))
            {
              logConflict(*s, *react);
            }
          }
        }
      }
    }
  }
}

/**
  * Logs a message about species with boundary condition false
  * being set by reaction and rules
  */
void
SpeciesReactionOrRule::logConflict (const Species& s, const Reaction& r)
{
  msg =
    //"A <species>'s quantity cannot be determined simultaneously by both "
    //"reactions and rules. More formally, if the identifier of a <species> "
    //"definition having 'boundaryCondition'='false' and 'constant'='false' is "
    //"referenced by a <speciesReference> anywhere in a model, then this "
    //"identifier cannot also appear as the value of a 'variable' in an "
    //"<assignmentRule> or a <rateRule>. (References: L2V1 Section 4.6.5; L2V2 "
    //"Section 4.8.6; L2V3 Section 4.8.6.) 
    "The species '";

  msg += s.getId();
  msg += "' occurs in both a rule and reaction '";
  msg += r.getId();
  msg += "'.";

  
  logFailure(s);
}
