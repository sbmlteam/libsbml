/**
 * \file    L2v2CompatibilityConstraints.cpp
 * \brief   L1 compatibility for conversion from L2
 * \author  Sarah Keating
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


#ifndef AddingConstraintsToValidator
#include <sbml/SBMLTypes.h>
#include <sbml/SBase.h>
#include <sbml/validator/Constraint.h>
#include <math.h>
#endif


#include <sbml/validator/ConstraintMacros.h>


using namespace std;


START_CONSTRAINT (93001, UnitDefinition, ud)
{
  msg =
    "In Level 2 Version 2 an sboTerm can only occur on the following elements: "
    "Model, FunctionDefinition, Parameter, InitialAssignment, Rule, Constraint, "
    "Reaction, SpeciesReference, KineticLaw, Event and EventAssignment.";

  inv( ud.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (93001, Unit, u)
{
  msg =
    "In Level 2 Version 2 an sboTerm can only occur on the following elements: "
    "Model, FunctionDefinition, Parameter, InitialAssignment, Rule, Constraint, "
    "Reaction, SpeciesReference, KineticLaw, Event and EventAssignment.";

  inv( u.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (93001, CompartmentType, ct)
{
  msg =
    "In Level 2 Version 2 an sboTerm can only occur on the following elements: "
    "Model, FunctionDefinition, Parameter, InitialAssignment, Rule, Constraint, "
    "Reaction, SpeciesReference, KineticLaw, Event and EventAssignment.";

  inv( ct.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (93001, SpeciesType, st)
{
  msg =
    "In Level 2 Version 2 an sboTerm can only occur on the following elements: "
    "Model, FunctionDefinition, Parameter, InitialAssignment, Rule, Constraint, "
    "Reaction, SpeciesReference, KineticLaw, Event and EventAssignment.";

  inv( st.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (93001, Compartment, c)
{
  msg =
    "In Level 2 Version 2 an sboTerm can only occur on the following elements: "
    "Model, FunctionDefinition, Parameter, InitialAssignment, Rule, Constraint, "
    "Reaction, SpeciesReference, KineticLaw, Event and EventAssignment.";

  inv( c.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (93001, Species, s)
{
  msg =
    "In Level 2 Version 2 an sboTerm can only occur on the following elements: "
    "Model, FunctionDefinition, Parameter, InitialAssignment, Rule, Constraint, "
    "Reaction, SpeciesReference, KineticLaw, Event and EventAssignment.";

  inv( s.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (93001, Trigger, t)
{
  msg =
    "In Level 2 Version 2 an sboTerm can only occur on the following elements: "
    "Model, FunctionDefinition, Parameter, InitialAssignment, Rule, Constraint, "
    "Reaction, SpeciesReference, KineticLaw, Event and EventAssignment.";

  inv( t.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (93001, Delay, d)
{
  msg =
    "In Level 2 Version 2 an sboTerm can only occur on the following elements: "
    "Model, FunctionDefinition, Parameter, InitialAssignment, Rule, Constraint, "
    "Reaction, SpeciesReference, KineticLaw, Event and EventAssignment.";

  inv( d.getSBOTerm() == -1 );
}
END_CONSTRAINT
