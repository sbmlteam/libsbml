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


START_CONSTRAINT (99904, Reaction, r)
{
  // level 1 metaid didnt exist
  pre( r.getLevel() == 1);
  
  inv( r.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, AssignmentRule, r)
{
  // level 1 metaid didnt exist
  pre( r.getLevel() == 1);
  
  inv( r.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, RateRule, r)
{
  // level 1 metaid didnt exist
  pre( r.getLevel() == 1);
  
  inv( r.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, AlgebraicRule, r)
{
  // level 1 metaid didnt exist
  pre( r.getLevel() == 1);
  
  inv( r.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, Species, s)
{
  // level 1 metaid didnt exist
  pre( s.getLevel() == 1);
  
  inv( s.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, SpeciesReference, sr)
{
  // level 1 metaid didnt exist
  pre( sr.getLevel() == 1);
  
  inv( sr.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, Unit, u)
{
  // level 1 metaid didnt exist
  pre( u.getLevel() == 1);
  
  inv( u.isSetMetaId() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99904, UnitDefinition, u)
{
  // level 1 metaid didnt exist
  pre( u.getLevel() == 1);
  
  inv( u.isSetMetaId() == false );
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


START_CONSTRAINT (99905, Species, s)
{
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( s.getLevel() == 1 || (s.getLevel() == 2 && s.getVersion() < 3));
  
  inv( s.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99905, SpeciesType, s)
{
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( s.getLevel() == 1 || (s.getLevel() == 2 && s.getVersion() < 3));
  
  inv( s.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99905, SpeciesReference, sr)
{
  // testing the stoichiometrymath element
  pre( sr.isSetStoichiometryMath());
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( sr.getLevel() == 1 || (sr.getLevel() == 2 && sr.getVersion() < 3));

  const StoichiometryMath * sm = sr.getStoichiometryMath();
  
  inv( sm->isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99905, Trigger, t)
{
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( t.getLevel() == 1 || (t.getLevel() == 2 && t.getVersion() < 3));
  
  inv( t.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99905, Unit, u)
{
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( u.getLevel() == 1 || (u.getLevel() == 2 && u.getVersion() < 3));
  
  inv( u.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99905, UnitDefinition, u)
{
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( u.getLevel() == 1 || (u.getLevel() == 2 && u.getVersion() < 3));
  
  inv( u.isSetSBOTerm() == false );
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


START_CONSTRAINT (99911, Reaction, r)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( r.getLevel() == 1 || (r.getLevel() == 2 && r.getVersion() == 1));
  
  inv( r.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99911, AssignmentRule, r)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( r.getLevel() == 1 || (r.getLevel() == 2 && r.getVersion() == 1));
  
  inv( r.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99911, RateRule, r)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( r.getLevel() == 1 || (r.getLevel() == 2 && r.getVersion() == 1));
  
  inv( r.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99911, AlgebraicRule, r)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( r.getLevel() == 1 || (r.getLevel() == 2 && r.getVersion() == 1));
  
  inv( r.isSetSBOTerm() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99911, SpeciesReference, sr)
{
  // level 1; l2v1 sboTerm didnt exist
  pre( sr.getLevel() == 1 || (sr.getLevel() == 2 && sr.getVersion() == 1));
  
  inv( sr.isSetSBOTerm() == false );
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


START_CONSTRAINT (99914, AlgebraicRule, ar)
{
  // might have been set internally 
  pre (ar.getInternalIdOnly() == false);
  // algebraic rule shouldnt have a variable 
  inv( ar.isSetVariable() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99915, AssignmentRule, r)
{
  // units only valid for l1 ParameterRule
  pre(r.isSetUnits());

  inv(r.getLevel() == 1 && r.getL1TypeCode() == SBML_PARAMETER_RULE );
}
END_CONSTRAINT


START_CONSTRAINT (99915, RateRule, r)
{
  // units only valid for l1 ParameterRule
  pre(r.isSetUnits());

  inv(r.getLevel() == 1 && r.getL1TypeCode() == SBML_PARAMETER_RULE );
}
END_CONSTRAINT


START_CONSTRAINT (99915, AlgebraicRule, r)
{
  // units only valid for l1 ParameterRule
  inv(r.isSetUnits() == false);
}
END_CONSTRAINT


START_CONSTRAINT (99916, Species, s)
{
  // level 1 species constant didnt exist
  // if species appears as the variable in a rule it should be false
  // if the species is product/reactant in a reaction 
  // with boundaryCondition false it should be false
  // otherwise it can be either

  //NOTE: the product/reactant check is easier to do on a speciesreference
  pre( s.getLevel() == 1);
  
  const Rule *r = m.getRule(s.getId());

  if (r != NULL)
  {
    inv( s.getConstant() == false );
  }
}
END_CONSTRAINT


START_CONSTRAINT (99916, SpeciesReference, sr)
{
  // level 1 species constant didnt exist
  // if species appears as the variable in a rule it should be false
  // if the species is product/reactant in a reaction 
  // with boundaryCondition false it should be false
  // otherwise it can be either
  pre( sr.getLevel() == 1);
  
  const Species* s = m.getSpecies( sr.getSpecies() );

  pre( s != NULL );
  
  inv( ! (s->getConstant() == true && s->getBoundaryCondition() == false) ); 
}
END_CONSTRAINT


START_CONSTRAINT (99917, Species, s)
{
  // level 1; spatialSizeUnits didnt exist
  pre( s.getLevel() == 1);
  
  inv( s.isSetSpatialSizeUnits() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99918, Species, s)
{
  // level 1 and L2V1 species shouldnt have speciesType
  pre( s.getLevel() == 1 || (s.getLevel() == 2 && s.getVersion() == 1));
  
  inv( s.isSetSpeciesType() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99919, Species, s)
{
  // level 1 species shouldnt have hasOnlySubstanceUnits
  pre( s.getLevel() == 1);
  
  inv( s.getHasOnlySubstanceUnits() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99920, SpeciesReference, sr)
{
  // level 1 and L2V1 speciesReference shouldnt have id
  pre( sr.getLevel() == 1 || (sr.getLevel() == 2 && sr.getVersion() == 1));
  
  inv( sr.isSetId() == false ); 
}
END_CONSTRAINT


START_CONSTRAINT (99921, SpeciesReference, sr)
{
  // level 1 and L2V1 speciesReference shouldnt have name
  pre( sr.getLevel() == 1 || (sr.getLevel() == 2 && sr.getVersion() == 1));
  
  inv( sr.isSetName() == false ); 
}
END_CONSTRAINT


START_CONSTRAINT (99922, Model, x)
{
  // speciesType not valid in L1 or L2v1
  pre( x.getLevel() == 1 ||(x.getLevel() == 2 && x.getVersion() == 1));
  
  inv( x.getNumSpeciesTypes() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (99923, SpeciesReference, sr)
{
  // testing the stoichiometrymath element
  pre( sr.isSetStoichiometryMath());
  // level 1 stoichiometryMath didnt exist
  pre( sr.getLevel() == 1);

  inv( sr.isSetStoichiometryMath() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99924, Unit, u)
{
  // multiplier not valid in L1
  // a value of 1 will not alter the unit
  pre( u.getLevel() == 1);

  inv (u.getMultiplier() == 1.0)  
}
END_CONSTRAINT


START_CONSTRAINT (99925, Unit, u)
{
  // offset not valid in L1
  // a value of 0 will not alter the unit
  pre( u.getLevel() == 1);
  
  inv( u.getOffset() == 0 );
}
END_CONSTRAINT



