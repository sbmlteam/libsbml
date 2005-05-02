/**
 * \file    UniqueIdsInKineticLaw.cpp
 * \brief   Ensures the ids for all Parameters in a KineticLaw are unique
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

#include "sbml/Model.h"
#include "sbml/Reaction.h"
#include "sbml/KineticLaw.h"
#include "sbml/Parameter.h"

#include "UniqueIdsInKineticLaw.h"


using namespace std;


static const char* PREAMBLE =
  "The ids of all Parameters within a KineticLaw must be unique "
  "(L2v1 Sections 3.5 and 4.9.7).";


/**
 * Creates a new Constraint with the given constraint id.
 */
UniqueIdsInKineticLaw::UniqueIdsInKineticLaw (unsigned int id) :
  UniqueIdBase(id)
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
    KineticLaw* kl = m.getReaction(r)->getKineticLaw();
    if (!kl) continue;

    for (unsigned int p = 0; p < kl->getNumParameters(); ++p)
    {
      checkId( kl->getParameter(p) );
    }
  }
}
