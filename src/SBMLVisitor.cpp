/**
 * \file   SBMLVisitor.cpp
 * \brief  Visitor Design Pattern for the SBML object tree  
 * \author Ben Bornstein
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


#include "sbml/SBMLTypes.hpp"
#include "sbml/SBMLVisitor.hpp"


SBMLVisitor::~SBMLVisitor ()
{
}


void
SBMLVisitor::visit (const SBMLDocument& x)
{
  visit( static_cast<const SBase&>(x) );
}


void
SBMLVisitor::visit (const Model& x)
{
  visit( static_cast<const SBase&>(x) );
}


void
SBMLVisitor::visit (const KineticLaw& x)
{
  visit( static_cast<const SBase&>(x) );
}


void
SBMLVisitor::visit (const ListOf& x, SBMLTypeCode_t type)
{
  visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const SBase& sb)
{
  return false;
}


bool
SBMLVisitor::visit (const FunctionDefinition& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const UnitDefinition& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const Unit& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const Compartment& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const Species& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const Parameter& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const Rule& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const AlgebraicRule& x)
{
  return visit( static_cast<const Rule&>(x) );
}


bool
SBMLVisitor::visit (const AssignmentRule& x)
{
  return visit( static_cast<const Rule&>(x) );
}


bool
SBMLVisitor::visit (const SpeciesConcentrationRule& x)
{
  return visit( static_cast<const Rule&>(x) );
}


bool
SBMLVisitor::visit (const CompartmentVolumeRule& x)
{
  return visit( static_cast<const Rule&>(x) );
}


bool
SBMLVisitor::visit (const ParameterRule& x)
{
  return visit( static_cast<const Rule&>(x) );
}


bool
SBMLVisitor::visit (const RateRule& x)
{
  return visit( static_cast<const Rule&>(x) );
}


bool
SBMLVisitor::visit (const Reaction& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const SimpleSpeciesReference& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const SpeciesReference& x)
{
  return visit( static_cast<const SimpleSpeciesReference&>(x) );
}


bool
SBMLVisitor::visit (const ModifierSpeciesReference& x)
{
  return visit( static_cast<const SimpleSpeciesReference&>(x) );
}


bool
SBMLVisitor::visit (const Event& x)
{
  return visit( static_cast<const SBase&>(x) );
}


bool
SBMLVisitor::visit (const EventAssignment& x)
{
  return visit( static_cast<const SBase&>(x) );
}


void
SBMLVisitor::leave (const SBMLDocument& x)
{
}


void
SBMLVisitor::leave (const Model& x)
{
}


void
SBMLVisitor::leave (const KineticLaw& x)
{
}


void
SBMLVisitor::leave (const Reaction& x)
{
}


void
SBMLVisitor::leave (const ListOf& x, SBMLTypeCode_t type)
{
}
