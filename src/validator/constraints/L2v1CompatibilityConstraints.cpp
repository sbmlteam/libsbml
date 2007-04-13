/**
 * \file    L2v1CompatibilityConstraints.cpp
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

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */



START_CONSTRAINT (92001, Model, x)
{
  msg =
    "Conversion of a model with Constraints to Level 2 Version 1 is not yet "
    "supported.";

  inv( m.getNumConstraints() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (92002, Model, x)
{
  msg =
    "Conversion of a model with InitialAssignments to Level 2 Version 1 is not yet "
    "supported.";

  inv( m.getNumInitialAssignments() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (92003, Model, x)
{
  msg =
    "SpeciesType information is not represented in Level 2 Version 1.";

  inv( m.getNumSpeciesTypes() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (92004, Model, x)
{
  msg =
    "CompartmentType information is not represented in Level 2 Version 1.";

  inv( m.getNumCompartmentTypes() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Model, x)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( m.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, FunctionDefinition, fd)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( fd.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, UnitDefinition, ud)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( ud.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Unit, u)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( u.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, CompartmentType, ct)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( ct.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, SpeciesType, st)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( st.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Compartment, c)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( c.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Species, s)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( s.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Parameter, p)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( p.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, InitialAssignment, ia)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( ia.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, AssignmentRule, r)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( r.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, RateRule, r)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( r.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, AlgebraicRule, r)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( r.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Constraint, c)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( c.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Reaction, r)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( r.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, SpeciesReference, sr)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( sr.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, KineticLaw, kl)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( kl.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Event, e)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( e.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, EventAssignment, ea)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( ea.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Trigger, t)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( t.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92005, Delay, d)
{
  msg =
    "No sboTerm information is represented in Level 2 Version 1.";

  inv( d.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (92006, SpeciesReference, sr) 
{
  msg =
    "A SpeciesReference containing an 'id' field cannot be "
    "represented in Level 2 Version 1.";

  inv( sr.isSetId() == 0  );
}
END_CONSTRAINT
