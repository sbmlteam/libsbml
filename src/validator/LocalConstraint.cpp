/**
 * \file    LocalConstraint.cpp
 * \brief   Applies to a single SBML Object and provides a single message.
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
 *     http://www.sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/SBMLTypes.h"
#include "LocalConstraint.h"


/**
 * @return true if this contraint holds when applied to the given SBML
 * object of type T, false otherwise.
 *
 * The Model object should contain x and is passed in to provide
 * additional context information, should the contraint need it.
 */
template <typename T>
bool
LocalConstraint<T>::holds (const Model& m, const T& x)
{
  mHolds = true;

  check(m, x);

  bool result = mHolds;
  mHolds      = true;

  return result;
}


/*
 * Generate the following LocalConstraints immediately so linking succeeds.
 */
template class LocalConstraint< SBMLDocument             >;
template class LocalConstraint< Model                    >;
template class LocalConstraint< FunctionDefinition       >;
template class LocalConstraint< UnitDefinition           >;
template class LocalConstraint< Unit                     >;
template class LocalConstraint< Compartment              >;
template class LocalConstraint< Species                  >;
template class LocalConstraint< Parameter                >;
template class LocalConstraint< Rule                     >;
template class LocalConstraint< AlgebraicRule            >;
template class LocalConstraint< AssignmentRule           >;
template class LocalConstraint< SpeciesConcentrationRule >;
template class LocalConstraint< CompartmentVolumeRule    >;
template class LocalConstraint< ParameterRule            >;
template class LocalConstraint< RateRule                 >;
template class LocalConstraint< Reaction                 >;
template class LocalConstraint< KineticLaw               >;
template class LocalConstraint< SimpleSpeciesReference   >;
template class LocalConstraint< SpeciesReference         >;
template class LocalConstraint< ModifierSpeciesReference >;
template class LocalConstraint< Event                    >;
template class LocalConstraint< EventAssignment          >;
