/**
 * \file    CompartmentOutsideCycles.h
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


#ifndef CompartmentOutsideCycles_h
#define CompartmentOutsideCycles_h


#ifdef __cplusplus


#include <string>
#include <vector>

#include "IdList.h"
#include "validator/GlobalConstraint.h"


class Compartment;


class CompartmentOutsideCycles: public GlobalConstraint
{
public:

  /**
   * Creates a new Constraint with the given id.
   */
  CompartmentOutsideCycles (unsigned int id);

  /**
   * Destroys this Constraint.
   */
  virtual ~CompartmentOutsideCycles ();


  /**
   * Resets the state of this GlobalConstraint by clearing its internal
   * list of error messages.
   */
  virtual void reset ();


protected:

  /**
   * Checks that no Compartments in Model have a cycle via their 'outside'
   * attribute.
   *
   * @return true if no cycles are found, false otherwise.
   */
  virtual bool check (const Model& m);

  /**
   * Checks for a cycle by following Compartment c's 'outside' attribute.
   * If a cycle is found, it is added to the list of found cycles, mCycles.
   */
  void checkForCycle (const Model& m, const Compartment* c);

  /**
   * @return true if Compartment c is contained in one of the already found
   * cycles, false otherwise.
   */
  bool isInCycle (const Compartment* c);

  /**
   * Logs a message about a cycle found starting at Compartment c.
   */
  void logCycle (const Compartment* c, const IdList& cycle);


  std::vector<IdList> mCycles;
};


#endif  /* __cplusplus */
#endif  /* CompartmentOutsideCycles_h */
