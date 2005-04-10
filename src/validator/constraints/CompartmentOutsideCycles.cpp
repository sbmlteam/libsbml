/**
 * \file    CompartmentOutsideCycles.cpp
 * \brief   Ensures no cycles exist via a Compartment's 'outside' attribute.
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
#include <vector>

#include <algorithm>
#include <functional>

#include "sbml/Model.h"
#include "sbml/Compartment.h"
#include "xml/ParseMessage.h"

#include "IdList.h"
#include "CompartmentOutsideCycles.h"


using namespace std;


/**
 * Creates a new Constraint with the given id.
 */
CompartmentOutsideCycles::CompartmentOutsideCycles (unsigned int id) :
  GlobalConstraint(id)
{
}


/**
 * Destroys this Constraint.
 */
CompartmentOutsideCycles::~CompartmentOutsideCycles ()
{
}


/**
 * Resets the state of this GlobalConstraint by clearing its internal
 * list of error messages.
 */
void
CompartmentOutsideCycles::reset ()
{
  GlobalConstraint::reset();
  mCycles.clear();
}


/**
 * Checks that no Compartments in Model form a cycle via their 'outside'
 * attribute.
 *
 * @return true if at least one cycle is found, false otherwise.
 */
bool
CompartmentOutsideCycles::check (const Model& m)
{
  for (int n = 0; n < m.getNumCompartments(); n++)
  {
    checkForCycle(m, m.getCompartment(n));
  }

  return mCycles.empty();
}


/**
 * Checks for a cycle by following Compartment c's 'outside' attribute.  If
 * a cycle is found, it is added to the list of found cycles, mCycles.
 */
void
CompartmentOutsideCycles::checkForCycle (const Model& m, const Compartment* c)
{
  IdList visited;


  while (c && !isInCycle(c))
  {
    const string& id = c->getId();

    if ( visited.contains(id) )
    {
      visited.removeIdsBefore(id);

      mCycles.push_back(visited);
      logCycle(c, visited);

      break;
    }

    visited.append(id);
    c = c->isSetOutside() ? m.getCompartment( c->getOutside() ) : 0;
  }
}


/**
 * Function Object: Returns true if Compartment c is contained in the given
 * IdList cycle.
 */
struct CycleContains : public unary_function<IdList, bool>
{
  CycleContains (const Compartment* c) : id(c->getId()) { }

  bool operator() (const IdList& lst) const
  {
    return lst.contains(id);
  }

  const string& id;
};


/**
 * @return true if Compartment c is contained in one of the already found
 * cycles, false otherwise.
 */
bool
CompartmentOutsideCycles::isInCycle (const Compartment* c)
{
  vector<IdList>::iterator end = mCycles.end();
  return find_if(mCycles.begin(), end, CycleContains(c)) != end;
}


/**
 * Logs a message about a cycle found starting at Compartment c.
 */
void
CompartmentOutsideCycles::logCycle (const Compartment* c, const IdList& cycle)
{
  string msg;


  msg  = "A Compartment may not enclose itself via its 'outside' attribute, ";
  msg += "but Compartment '" + c->getId() + "' encloses itself";

  if (cycle.size() > 1)
  {
    IdList::const_iterator iter = cycle.begin();
    IdList::const_iterator end  = cycle.end();

    while (iter != end) msg += " -> '" + *iter++ + "'";
    msg += " -> '" + c->getId() + "'";
  }

  msg += '.';

  logMessage( ParseMessage(getId(), msg, c->getLine(), c->getColumn()) );
}
