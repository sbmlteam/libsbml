/**
 * \file    UniqueIdsInKineticLaw.cpp
 * \brief   Ensures the ids for all Parameters in a KineticLaw are unique
 * \author  Ben Bornstein
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


#include <string>

#include <sbml/Model.h>
#include <sbml/Reaction.h>
#include <sbml/KineticLaw.h>
#include <sbml/Parameter.h>

#include "UniqueIdsInKineticLaw.h"


using namespace std;


static const char* PREAMBLE =
    "The value of the 'id' field of each parameter defined locally within a "
    "<kineticLaw> must be unique across the set of all such parameter "
    "definitions in that <kineticLaw>. (References: L2V2 Sections 3.4.1 and "
    "4.13.9; L2V1 Sections 3.4.1 and 4.13.5.)";


/**
 * Creates a new Constraint with the given constraint id.
 */
UniqueIdsInKineticLaw::UniqueIdsInKineticLaw (unsigned int id, Validator& v) :
  UniqueIdBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
UniqueIdsInKineticLaw::~UniqueIdsInKineticLaw ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
UniqueIdsInKineticLaw::getPreamble ()
{
  return PREAMBLE;
}


/**
 * Checks that all ids on KineticLawParameters are unique.
 */
void
UniqueIdsInKineticLaw::doCheck (const Model& m)
{
  for (unsigned int r = 0; r < m.getNumReactions(); ++r)
  {
    const KineticLaw* kl = m.getReaction(r)->getKineticLaw();
    if (!kl) continue;

    for (unsigned int p = 0; p < kl->getNumParameters(); ++p)
    {
      checkId( *kl->getParameter(p) );
    }

    reset();
  }
}
