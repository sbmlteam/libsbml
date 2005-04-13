/**
 * \file    ConsistencyConstraints.cpp
 * \brief   Consistency check constraints.  See SBML Wiki
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


#ifndef AddingConstraintsToValidator
#include <string>
#include "sbml/SBMLTypes.h"
#include "validator/LocalConstraint.h"
#include "CompartmentOutsideCycles.h"
#endif


#include "validator/ConstraintMacros.h"


using namespace std;


START_CONSTRAINT (1002, Model, x)
{
  msg =
    "A Model that has a Species must also have at least one Compartment "
    "(L2v1 Section 4.5).";

  pre( m.getNumSpecies()      > 0 );
  inv( m.getNumCompartments() > 0 );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models (replacing name with id).
START_CONSTRAINT (1201, UnitDefinition, ud)
{
  msg =
    "The id of a UnitDefinition must not be a predefined kind of unit "
    "(L2v1 erratum).";
    

  inv( Unit::isUnitKind( ud.getId() ) == false );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1202, UnitDefinition, ud)
{
  msg =
    "A 'substance' UnitDefinition must simplify to a single Unit of kind "
    "'mole' or 'item' with an exponent of '1' (L2v1 Section 4.4.3).";

  pre( ud.getId() == "substance" );

  inv( ud.getNumUnits() == 1                              );
  inv( ud.getUnit(0)->isMole() || ud.getUnit(0)->isItem() );
  inv( ud.getUnit(0)->getExponent() == 1                  );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1203, UnitDefinition, ud)
{
  msg =
    "A 'length' UnitDefinition must simplify to a single Unit of kind "
    "'metre' with an exponent of '1' (L2v1 Section 4.4.3).";

  pre( ud.getId() == "length" );

  inv( ud.getNumUnits() == 1             );
  inv( ud.getUnit(0)->isMetre()          );
  inv( ud.getUnit(0)->getExponent() == 1 );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1204, UnitDefinition, ud)
{
  msg =
    "An 'area' UnitDefinition must simplify to a single Unit of kind "
    "'metre' with an exponent of '2' (L2v1 Section 4.4.3).";

  pre( ud.getId() == "area" );

  inv( ud.getNumUnits() == 1             );
  inv( ud.getUnit(0)->isMetre()          );
  inv( ud.getUnit(0)->getExponent() == 2 );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1205, UnitDefinition, ud)
{
  msg =
    "A 'volume' UnitDefinition must simplify to a single Unit of kind "
    "'litre' or 'metre' (L2v1 Section 4.4.3).";

  pre( ud.getId() == "volume" );

  inv( ud.getNumUnits() == 1 );
  inv( ud.getUnit(0)->isLitre() || ud.getUnit(0)->isMetre() );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1206, UnitDefinition, ud)
{
  msg =
    "A 'volume' UnitDefinition that simplifies to a single Unit of kind "
    "'litre' must also have an exponent of '1' (L2v1 Section 4.4.3).";

  pre( ud.getId()       == "volume" );
  pre( ud.getNumUnits() == 1        );
  pre( ud.getUnit(0)->isLitre()     );

  inv( ud.getUnit(0)->getExponent() == 1 );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1207, UnitDefinition, ud)
{
  msg =
    "A 'volume' UnitDefinition that simplifies to a single Unit of kind "
    "'metre' must also have an exponent of '3' (L2v1 Section 4.4.3).";

  pre( ud.getId()       == "volume" );
  pre( ud.getNumUnits() == 1        );
  pre( ud.getUnit(0)->isMetre()     );

  inv( ud.getUnit(0)->getExponent() == 3 );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1208, UnitDefinition, ud)
{
  msg =
    "A 'time' UnitDefinition must simplify to a single Unit of kind "
    "'second' with an exponent of '1' (L2v1 Section 4.4.3).";

  pre( ud.getId() == "time" );

  inv( ud.getNumUnits() == 1             );
  inv( ud.getUnit(0)->isSecond()         );
  inv( ud.getUnit(0)->getExponent() == 1 );
}
END_CONSTRAINT




START_CONSTRAINT (1300, Compartment, c)
{
  msg =
    "Compartment size must not be set if spatialDimensions is zero "
    "(L2v1 Section 4.5.3).";

  pre( c.getSpatialDimensions() == 0 );
  inv( c.isSetSize() == false );
}
END_CONSTRAINT


START_CONSTRAINT (1301, Compartment, c)
{
  msg =
    "Compartment units must not be set if spatialDimensions is zero "
    "(L2v1 Section 4.5.4).";

  pre( c.getSpatialDimensions() == 0 );
  inv( c.isSetUnits() == false       );
}
END_CONSTRAINT


START_CONSTRAINT (1302, Compartment, c)
{
  msg =
    "A Compartment must be constant if spatialDimensions is zero "
    "(L2v1 Section 4.5.5).";

  pre( c.getSpatialDimensions() == 0 );
  inv( c.getConstant() == true       );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1303, Compartment, c)
{
  msg =
    "A Compartment's 'outside' must be the id of another Compartment "
    "(L2v1 Section 4.5.6).";

  pre( c.isSetOutside() );
  inv( m.getCompartment( c.getOutside() ) != NULL );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
EXTERN_CONSTRAINT(1304, CompartmentOutsideCycles)


START_CONSTRAINT (1305, Compartment, c)
{
  msg =
    "A Compartment with spatialDimensions='1' must have units of 'length', "
    "'metre', or the id of a UnitDefinition that defines a variant of "
    "'metre' with exponent='1' (L2v1 Section 4.5.4).";

  pre( c.getSpatialDimensions() == 1 );
  pre( c.isSetUnits()                );

  const string&         units = c.getUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "length" );
  inv_or( units == "metre"  );
  inv_or( defn  != NULL && defn->isVariantOfLength() );
}
END_CONSTRAINT


START_CONSTRAINT (1306, Compartment, c)
{
  msg =
    "A Compartment with spatialDimensions='2' must have units of 'area' "
    "or the id of a UnitDefinition that defines a variant of 'metre' "
    "with exponent='2' (L2v1 Section 4.5.4).";

  pre( c.getSpatialDimensions() == 2 );
  pre( c.isSetUnits()                );

  const string&         units = c.getUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "area" );
  inv_or( defn  != NULL && defn->isVariantOfArea() );
}
END_CONSTRAINT


START_CONSTRAINT (1307, Compartment, c)
{
  msg =
    "A Compartment with spatialDimensions='3' must have units of 'volume', "
    "'litre', or the id of a UnitDefinition that defines a variant of "
    "'metre' with exponent='3' or a variant of 'litre' (L2v1 Section 4.5.4).";

  pre( c.getSpatialDimensions() == 3 );
  pre( c.isSetUnits()                );

  const string&         units = c.getUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "volume" );
  inv_or( units == "litre"  );
  inv_or( defn  != NULL && defn->isVariantOfVolume() );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1400, Species, s)
{
  msg =
    "Compartment '" + s.getCompartment() + "' is undefined.  If a Species "
    "refers to a Compartment it must exist in the Model (L2v1 Section 4.6.2).";

  pre( s.isSetCompartment() );
  inv( m.getCompartment( s.getCompartment() ) != NULL );
}
END_CONSTRAINT


START_CONSTRAINT (1401, Species, s)
{
  msg =
    "A Species with hasOnlySubstanceUnits='true' must not have "
    "spatialSizeUnits (L2v1 Section 4.6.4).";

  pre( s.getHasOnlySubstanceUnits() == true );
  inv( !s.isSetSpatialSizeUnits()           );
}
END_CONSTRAINT


START_CONSTRAINT (1402, Species, s)
{
  msg =
    "A Species must not have spatialSizeUnits if its Compartment has "
    "spatialDimensions='0' (L2v1 Section 4.6.4).";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 0 );
  inv( !s.isSetSpatialSizeUnits()                  );
}
END_CONSTRAINT


START_CONSTRAINT (1403, Species, s)
{
  msg =
    "A Species with hasOnlySubstanceUnits='true' must not have an "
    "initialConcentration (L2v1 Section 4.6.3).";

  pre( s.getHasOnlySubstanceUnits() == true );
  inv( !s.isSetInitialConcentration()       );
}
END_CONSTRAINT


START_CONSTRAINT (1404, Species, s)
{
  msg =
    "A Species whose Compartment has spatialDimensions='0' must not have an "
    "initialConcentration (L2v1 Section 4.6.3).";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 0 );
  inv( !s.isSetInitialConcentration()              );
}
END_CONSTRAINT


START_CONSTRAINT (1405, Species, s)
{
  msg =
    "A Species whose Compartment has spatialDimensions='1' must have "
    "spatialSizeUnits of 'length', 'metre', or the id of a UnitDefinition "
    "that defines a variant of 'metre' with exponent='1' "
    "(L2v1 Section 4.6.4).";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 1 );
  pre( s.isSetSpatialSizeUnits() );

  const string&         units = s.getSpatialSizeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "length" );
  inv_or( units == "metre"  );
  inv_or( defn  != NULL && defn->isVariantOfLength() );
}
END_CONSTRAINT


START_CONSTRAINT (1406, Species, s)
{
  msg =
    "A Species whose Compartment has spatialDimensions='2' must have "
    "spatialSizeUnits of 'area' or the id of a unitDefinition that "
    "defines a variant of 'metre' with exponent='2' "
    "(L2v1 Section 4.6.4).";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 2 );
  pre( s.isSetSpatialSizeUnits() );

  const string&         units = s.getSpatialSizeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "area" );
  inv_or( defn  != NULL && defn->isVariantOfArea() );
}
END_CONSTRAINT


START_CONSTRAINT (1407, Species, s)
{
  msg =
    "A Species whose Compartment has spatialDimensions=3 must have "
    "spatialSizeUnits of 'volume' or 'litre' or the id of a UnitDefinition "
    "that defines a variant of 'metre' with exponent='3' or a variant of "
    "'litre' (L2v1 Section 4.6.4).";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 3 );
  pre( s.isSetSpatialSizeUnits() );

  const string&         units = s.getSpatialSizeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "volume" );
  inv_or( units == "litre"  );
  inv_or( defn  != NULL && defn->isVariantOfVolume() );
}
END_CONSTRAINT


START_CONSTRAINT (1408, Species, s)
{
  msg =
    "A Species' substanceUnits must be 'substance', 'item', 'mole', or the "
    "id of a UnitDefinition that defines a variant of 'item' or 'mole' "
    "(L2v1 Section 4.6.4)";


  pre( s.isSetSubstanceUnits() );

  const string&         units = s.getSubstanceUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "substance" );
  inv_or( units == "item"      );
  inv_or( units == "mole"      );
  inv_or( defn  != NULL && defn->isVariantOfSubstance() );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1500, Parameter, p)
{
  msg =
    "A Parameter's 'units' must be a UnitKind, a built-in unit, or the id "
    "of a UnitDefinition. (L2v1 Section 4.7.3)";


  pre( p.isSetUnits() );

  const string& units = p.getUnits();

  inv_or( Unit::isUnitKind(units)    );
  inv_or( Unit::isBuiltIn(units)     );
  inv_or( m.getUnitDefinition(units) );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (1600, Reaction, r)
{
  msg =
    "A Reaction must contain at least one SpeciesReference in its list "
    "of reactants or products.";

  inv( r.getNumReactants() > 0 || r.getNumProducts() > 0 );
}
END_CONSTRAINT


START_CONSTRAINT (1601, SimpleSpeciesReference, sr)
{
  msg =
    "Species '" + sr.getSpecies() + "' is undefined.  A SpeciesReference "
    "must refer to a Species (L2v1 Section 4.9.5).";

  inv( m.getSpecies( sr.getSpecies() ) != NULL );
}
END_CONSTRAINT


START_CONSTRAINT (1602, SpeciesReference, sr)
{
  msg =
    "A SpeciesReference may not refer to a Species with constant='true' "
    "and boundaryCondition='false' (L2v1 Section 4.6.5).";


  const Species* s = m.getSpecies( sr.getSpecies() );

  pre( s != NULL );
  inv( ! (s->getConstant() == true && s->getBoundaryCondition() == false) ); 
}
END_CONSTRAINT


START_CONSTRAINT (1603, SpeciesReference, sr)
{
  msg =
    "A SpeciesReference may not contain both a 'stoichiometry' attribute "
    "and a 'stoichiometryMath' subelement (L2v1 Section 4.9.5).";


  pre( sr.isSetStoichiometryMath()  );
  inv( sr.getStoichiometry() == 1.0 );
}
END_CONSTRAINT


START_CONSTRAINT (1604, KineticLaw, kl)
{
  msg =
    "A KineticLaw's substanceUnits must be 'substance', 'item', 'mole', or "
    "the id of a UnitDefinition that defines a variant of 'item' or 'mole' "
    "(L2v1 Section 4.9.7).";


  pre( kl.isSetSubstanceUnits() );

  const string&         units = kl.getSubstanceUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "substance" );
  inv_or( units == "item"      );
  inv_or( units == "mole"      );
  inv_or( defn  != NULL && defn->isVariantOfSubstance() );
}
END_CONSTRAINT


START_CONSTRAINT (1605, KineticLaw, kl)
{
  msg =
    "A KineticLaw's timeUnits must be 'time', 'second', or the id of a "
    "UnitDefnition that defines a variant of 'second' with exponent='1' "
    "(L2v1 Section 4.9.7).";


  pre( kl.isSetTimeUnits() );

  const string&         units = kl.getTimeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "time"   );
  inv_or( units == "second" );
  inv_or( defn  != NULL && defn->isVariantOfTime() );
}
END_CONSTRAINT


START_CONSTRAINT (1700, AssignmentRule, r)
{
  msg =
    "An AssignmentRule's variable must be the id of a Compartment, Species, "
    "or Parameter (L2v1 Section 4.8.2).";


  pre( r.isSetVariable() );

  const string& id = r.getVariable();

  inv_or( m.getCompartment(id) );
  inv_or( m.getSpecies    (id) );
  inv_or( m.getParameter  (id) );
}
END_CONSTRAINT


START_CONSTRAINT (1701, RateRule, r)
{
  msg =
    "A RateRule's variable must be the id of a Compartment, Species, "
    "or Parameter (L2v1 Section 4.8.3).";


  pre( r.isSetVariable() );

  const string& id = r.getVariable();

  inv_or( m.getCompartment(id) );
  inv_or( m.getSpecies    (id) );
  inv_or( m.getParameter  (id) );
}
END_CONSTRAINT


START_CONSTRAINT (1702, AssignmentRule, r)
{
  msg =
    "A Compartment, Species, or Parameter referenced by an AssignmentRule "
    "must have constant='false' (L2v1 Section 4.8.4).";


  pre( r.isSetVariable() );

  const string& id = r.getVariable();

  const Compartment* c = m.getCompartment(id);
  const Species*     s = m.getSpecies    (id);
  const Parameter*   p = m.getParameter  (id);

  pre( c || s || p );

  inv_or( c && c->getConstant() == false );
  inv_or( s && s->getConstant() == false );
  inv_or( p && p->getConstant() == false );
}
END_CONSTRAINT


START_CONSTRAINT (1703, RateRule, r)
{
  msg =
    "A Compartment, Species, or Parameter referenced by a RateRule "
    "must have constant='false' (L2v1 Section 4.8.4).";


  pre( r.isSetVariable() );

  const string& id = r.getVariable();

  const Compartment* c = m.getCompartment(id);
  const Species*     s = m.getSpecies    (id);
  const Parameter*   p = m.getParameter  (id);

  pre( c || s || p );

  inv_or( c && c->getConstant() == false );
  inv_or( s && s->getConstant() == false );
  inv_or( p && p->getConstant() == false );
}
END_CONSTRAINT




START_CONSTRAINT (1800, Event, e)
{
  msg =
    "An Event's timeUnits must be 'time', 'second', or the id of a "
    "UnitDefinition that defines a variant of 'second' with exponent='1' "
    "(L2v1 Section 4.10.4).";

  pre( e.isSetTimeUnits() );

  const string&         units = e.getTimeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "time"   );
  inv_or( units == "second" );
  inv_or( defn  != NULL && defn->isVariantOfTime() );
}
END_CONSTRAINT


// 1801 - The top-level MathML operator in the trigger subelement must be a
// return boolean.


START_CONSTRAINT (1802, EventAssignment, ea)
{
  msg = 
    "An EventAssignment's variable must be the id of a Compartment, Species, "
    "or Parameter (L2v1 Section 4.10.5).";


  pre( ea.isSetVariable() );

  const string& id = ea.getVariable();

  inv_or( m.getCompartment(id) );
  inv_or( m.getSpecies    (id) );
  inv_or( m.getParameter  (id) );
}
END_CONSTRAINT


START_CONSTRAINT (1803, EventAssignment, ea)
{
  msg =
    "A Compartment, Species, or Parameter referenced by an EventAssignment "
    "must have constant='false' (L2v1 Section 4.10.5).";


  pre( ea.isSetVariable() );

  const string& id = ea.getVariable();

  const Compartment* c = m.getCompartment(id);
  const Species*     s = m.getSpecies    (id);
  const Parameter*   p = m.getParameter  (id);

  pre( c || s || p );

  inv_or( c && c->getConstant() == false );
  inv_or( s && s->getConstant() == false );
  inv_or( p && p->getConstant() == false );
}
END_CONSTRAINT
