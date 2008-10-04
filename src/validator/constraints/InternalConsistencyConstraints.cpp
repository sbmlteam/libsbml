/**
 * @file    InternalConsistencyConstraints.cpp
 * @brief   Consistency check constraints.  See SBML Wiki
 * @author  Sarah Keating
 *
 * $Id:  $
 * $HeadURL:  $
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

//#include <string>
#include <cstring>

#include <sbml/SBMLTypes.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBO.h>
#include <sbml/validator/VConstraint.h>
#include <sbml/units/UnitFormulaFormatter.h>
#include <sbml/units/FormulaUnitsData.h>

#include <sbml/util/List.h>


#endif


#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */



// Compartment validation

START_CONSTRAINT (99901, Compartment, c)
{
  // level 1 compartment spatial dimensions should be 3
  pre( c.getLevel() == 1);
  
  inv( c.getSpatialDimensions() == 3 );
}
END_CONSTRAINT


START_CONSTRAINT (99902, Compartment, c)
{
  // level 1 and L2V1 compartment shouldnt have compartmentType
  pre( c.getLevel() == 1 || (c.getLevel() == 2 && c.getVersion() == 1));
  
  inv( c.isSetCompartmentType() == false );
}
END_CONSTRAINT

// 99903 constant not valid attribute
// assume it should default to false
START_CONSTRAINT (99903, Compartment, c)
{
  // level 1 compartment constant didnt exist
  // if compartment appears as the variable in a rule it should be false
  // otherwise it can be either
  pre( c.getLevel() == 1);
  
  const Rule *r = m.getRule(c.getId());

  if (r != NULL)
  {
    inv( c.getConstant() == false );
  }
}
END_CONSTRAINT

START_CONSTRAINT (99903, Parameter, p)
{
  // level 1 parameter constant didnt exist
  // if parameter appears as the variable in a rule it should be false
  // otherwise it can be either
  // BUT a local parameter must be true
  pre( p.getLevel() == 1);
  
  SBase *sb = const_cast <Parameter *> (&p)->getParentSBMLObject();
  if (sb->getParentSBMLObject()->getTypeCode() == SBML_KINETIC_LAW)
  {
    // local parameter
    inv (p.getConstant() == true);
  }
  else
  {
    const Rule *r = m.getRule(p.getId());

    if (r != NULL)
    {
      inv( p.getConstant() == false );
    }
  }
}
END_CONSTRAINT

// 99904 - metaid did not exist in l1
// this constraint applies to any component that did exist in l1
START_CONSTRAINT (99904, Compartment, c)
{
  // level 1 metaid didnt exist
  pre( c.getLevel() == 1);
  
  inv( c.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, KineticLaw, kl)
{
  // level 1 metaid didnt exist
  pre( kl.getLevel() == 1);
  
  inv( kl.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, Model, x)
{
  // level 1 metaid didnt exist
  pre( x.getLevel() == 1);
  
  inv( x.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, Parameter, p)
{
  // level 1 metaid didnt exist
  pre( p.getLevel() == 1);
  
  inv( p.isSetMetaId() == false );
}
END_CONSTRAINT

// 99905 SBOTerm not valid before l2v3
// this constraint applies to any component that existed in l2v2
// but did not have an sboterm
START_CONSTRAINT (99905, Compartment, c)
{
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( c.getLevel() == 1 || (c.getLevel() == 2 && c.getVersion() < 3));
  
  inv( c.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99905, CompartmentType, ct)
{
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( ct.getLevel() == 1 || (ct.getLevel() == 2 && ct.getVersion() < 3));
  
  inv( ct.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99905, Delay, d)
{
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( d.getLevel() == 1 || (d.getLevel() == 2 && d.getVersion() < 3));
  
  inv( d.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99906, Compartment, c)
{
  // level 1 units check gets missed in check consistency
  pre( c.getLevel() == 1 && c.isSetUnits())
  
  const string&         units = c.getUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "volume" );
  inv_or( units == "litre"  );
  inv_or( units == "liter"  );
  inv_or( defn  != NULL && defn->isVariantOfVolume() );
}
END_CONSTRAINT


START_CONSTRAINT (99907, Compartment, c)
{
  // level 1 version 1 volume required
  pre( c.getLevel() == 1 && c.getVersion() == 1)
  
  inv( c.isSetVolume() == true );
}
END_CONSTRAINT


START_CONSTRAINT (99908, Model, x)
{
  // compartmentType not valid in L1 or L2v1
  pre( x.getLevel() == 1 ||(x.getLevel() == 2 && x.getVersion() == 1));
  
  inv( x.getNumCompartmentTypes() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (99909, Model, x)
{
  // constraint not valid in L1 or L2v1
  pre( x.getLevel() == 1 ||(x.getLevel() == 2 && x.getVersion() == 1));
  
  inv( x.getNumConstraints() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (99910, Model, x)
{
  // event not valid in L1
  pre( x.getLevel() == 1 );
  
  inv( x.getNumEvents() == 0 );
}
END_CONSTRAINT

// 99911 SBOTerm not valid before l2v2
// this constraint applies to any component that existed in l2v1 and earlier
// but did not have an sboterm
START_CONSTRAINT (99911, Event, e)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( e.getLevel() == 1 || (e.getLevel() == 2 && e.getVersion() == 1));
  
  inv( e.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99911, EventAssignment, ea)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( ea.getLevel() == 1 || (ea.getLevel() == 2 && ea.getVersion() == 1));
  
  inv( ea.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99911, FunctionDefinition, fd)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( fd.getLevel() == 1 || (fd.getLevel() == 2 && fd.getVersion() == 1));
  
  inv( fd.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99911, KineticLaw, kl)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( kl.getLevel() == 1 || (kl.getLevel() == 2 && kl.getVersion() == 1));
  
  inv( kl.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99911, Model, m1)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( m1.getLevel() == 1 || (m1.getLevel() == 2 && m1.getVersion() == 1));
  
  inv( m1.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99911, Parameter, p)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( p.getLevel() == 1 || (p.getLevel() == 2 && p.getVersion() == 1));
  
  inv( p.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99912, Model, x)
{
  // functionDefinition not valid in L1
  pre( x.getLevel() == 1 );
  
  inv( x.getNumFunctionDefinitions() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (99913, Model, x)
{
  // initial assignment not valid in L1 or L2v1
  pre( x.getLevel() == 1 ||(x.getLevel() == 2 && x.getVersion() == 1));
  
  inv( x.getNumInitialAssignments() == 0 );
}
END_CONSTRAINT


