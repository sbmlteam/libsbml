/**
 * \file    ConsistencyConstraints.cpp
 * \brief   Consistency check constraints.  See SBML Wiki
 * \author  Ben Bornstein
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

#include <string>

#include <sbml/SBMLTypes.h>
#include <sbml/validator/Constraint.h>
#include <sbml/units/UnitFormulaFormatter.h>

#include <sbml/util/List.h>

#include "CompartmentOutsideCycles.h"
#include "FunctionDefinitionVars.h"

#include "UniqueIdsForUnitDefinitions.h"
#include "UniqueIdsInKineticLaw.h"
#include "UniqueIdsInModel.h"
#include "UniqueVarsInEventAssignments.h"
#include "UniqueVarsInRules.h"
#include "UniqueVarsInEventsAndRules.h"
#include "UniqueMetaId.h"

#include "FunctionReferredToExists.h"
#include "SpeciesReactionOrRule.h"
#include "UniqueSpeciesTypesInCompartment.h"
#include "UniqueSymbolsInInitialAssignments.h"
#include "UniqueVarsInInitialAssignmentsAndRules.h"
#include "StoichiometryMathVars.h"
#include "KineticLawVars.h"
#include "AssignmentCycles.h"

//#include "FormulaUnitsCheck.h"

#include "PowerUnitsCheck.h"
#include "ExponentUnitsCheck.h"
#include "ArgumentsUnitsCheck.h"

#include "LogicalArgsMathCheck.h"
#include "NumericArgsMathCheck.h"
#include "PieceBooleanMathCheck.h"
#include "PiecewiseValueMathCheck.h"
#include "EqualityArgsMathCheck.h"
#include "FunctionApplyMathCheck.h"
#include "CiElementMathCheck.h"
#include "LambdaMathCheck.h"
#include "NumericReturnMathCheck.h"

#endif


#include <sbml/validator/ConstraintMacros.h>


using namespace std;

EXTERN_CONSTRAINT( 2006, LambdaMathCheck)
EXTERN_CONSTRAINT( 2007, FunctionApplyMathCheck)
EXTERN_CONSTRAINT( 2008, CiElementMathCheck)
EXTERN_CONSTRAINT( 2011, LogicalArgsMathCheck)
EXTERN_CONSTRAINT( 2012, PieceBooleanMathCheck)
EXTERN_CONSTRAINT( 2015, NumericArgsMathCheck)
EXTERN_CONSTRAINT( 2017, PiecewiseValueMathCheck)
EXTERN_CONSTRAINT( 2018, EqualityArgsMathCheck)
EXTERN_CONSTRAINT( 2019, NumericReturnMathCheck)

EXTERN_CONSTRAINT( 900, UniqueIdsInModel             )
EXTERN_CONSTRAINT( 901, UniqueIdsForUnitDefinitions  )
EXTERN_CONSTRAINT( 902, UniqueIdsInKineticLaw        )
EXTERN_CONSTRAINT( 903, UniqueVarsInRules            )
EXTERN_CONSTRAINT( 904, UniqueVarsInEventAssignments )
EXTERN_CONSTRAINT( 905, UniqueVarsInEventsAndRules   )
EXTERN_CONSTRAINT( 907, UniqueMetaId                 )



//
// Constraint 1000 is: No Model Present and is caught before validation
// begins.  This is because the validator framework assumes a Model.
//

START_CONSTRAINT (1001, Model, x)
{
  msg =
    "A Model that has a Species must also have at least one Compartment "
    "(L2v1 Section 4.5).";

  pre( m.getNumSpecies()      > 0 );
  inv( m.getNumCompartments() > 0 );
}
END_CONSTRAINT



START_CONSTRAINT (1100, FunctionDefinition, fd)
{
  msg =
    "<lambda> must be the top-level element of a FunctionDefinition "
    "(L2v1 Section 4.3.2).";

  pre( fd.isSetMath()           );
  inv( fd.getMath()->isLambda() );
}
END_CONSTRAINT

EXTERN_CONSTRAINT(1101, FunctionReferredToExists)


START_CONSTRAINT (1102, FunctionDefinition, fd)
{
  msg =
    "Inside the lambda of a FunctionDefinition, the identifier of that "
    "FunctionDefinition cannot appear as the value of a ci element. "
    "SBML functions are not permitted to be recursive (L2V2 Section 4.3.2).";

  pre( fd.isSetMath()            );
  pre( fd.getBody() != NULL      );
  pre( fd.getNumArguments() != 0 );
  
  const string  id = fd.getId();

  List* variables = fd.getBody()->getListOfNodes( ASTNode_isFunction );
  for (unsigned int n = 0; n < variables->getSize(); ++n)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(n) );
    const char *   name = node->getName() ? node->getName() : "";

    inv(strcmp(name, id.c_str()));
 }

}
END_CONSTRAINT

// This was 1101 but the new spec rephrases it
EXTERN_CONSTRAINT(1103, FunctionDefinitionVars)

START_CONSTRAINT (1104, FunctionDefinition, fd)
{
  msg =
    "The value type returned by a FunctionDefinition's lambda "
    "must be either boolean or numeric (L2V2 Section 3.5.6).";

  pre( fd.isSetMath()           );
  pre( fd.getBody() != NULL      );

  inv_or( fd.getBody()->isBoolean() );
  inv_or( fd.getBody()->isNumber()  );
  inv_or( fd.getBody()->isFunction());
  inv_or( fd.getBody()->isOperator());
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

  /* dimensionless is allowable in L2V2 */
  if (  ud.getLevel() == 2 
    &&  ud.getVersion() == 2
    &&  !ud.getUnit(0)->isMetre())
  {
    inv(ud.getUnit(0)->isDimensionless());
  }
  else
  {
    inv( ud.getUnit(0)->isMetre()          );
    inv( ud.getUnit(0)->getExponent() == 1 );
  }
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

  /* dimensionless is allowable in L2V2 */
  if (  ud.getLevel() == 2 
    &&  ud.getVersion() == 2
    &&  !ud.getUnit(0)->isMetre())
  {
    inv(ud.getUnit(0)->isDimensionless());
  }
  else
  {
    inv( ud.getUnit(0)->isMetre()          );
    inv( ud.getUnit(0)->getExponent() == 2 );
  }
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
  
  /* dimensionless is allowable in L2V2 */
  if (  ud.getLevel() == 2 
    &&  ud.getVersion() == 2)
  {
    inv( ud.getUnit(0)->isLitre() 
      || ud.getUnit(0)->isMetre() 
      || ud.getUnit(0)->isDimensionless() );
  }
  else
  {
    inv( ud.getUnit(0)->isLitre() || ud.getUnit(0)->isMetre() );
  }
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

  /* dimensionless is allowable in L2V2 */
  if (  ud.getLevel() == 2 
    &&  ud.getVersion() == 2
    &&  !ud.getUnit(0)->isSecond())
  {
    inv(ud.getUnit(0)->isDimensionless());
  }
  else
  {
    inv( ud.getUnit(0)->isSecond()         );
    inv( ud.getUnit(0)->getExponent() == 1 );
  }
}
END_CONSTRAINT


START_CONSTRAINT (1209, UnitDefinition, ud)
{
  msg =
    "The offset field in Unit is deprecated as of SBML Level 2 Version 2. "
    "Software tools should not generate models containing deprecated features. "
    "(L2V2 Section 4.4).";

  pre( ud.getLevel() == 2 && ud.getVersion() == 2 );

  for (unsigned int n = 0; n < ud.getNumUnits(); ++n)
  {
    inv(ud.getUnit(n)->getOffset() == 0);
  }
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
  
  /* dimensionless is allowable in L2V2 */
  if (  c.getLevel() == 2 
    &&  c.getVersion() == 2
    &&  c.isSetUnits() )
  {
    const string&         units = c.getUnits();
    const UnitDefinition* defn  = m.getUnitDefinition(units);
 
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv( c.isSetUnits() == false       );
  }
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

  /* dimensionless is allowable in L2V2 */
  if (  c.getLevel() == 2 
    &&  c.getVersion() == 2)
  {
    inv_or( units == "length" );
    inv_or( units == "metre"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfLength() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "length" );
    inv_or( units == "metre"  );
    inv_or( defn  != NULL && defn->isVariantOfLength() );
  }
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

  /* dimensionless is allowable in L2V2 */
  if (  c.getLevel() == 2 
    &&  c.getVersion() == 2)
  {
    inv_or( units == "area" );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfArea() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "area" );
    inv_or( defn  != NULL && defn->isVariantOfArea() );
  }
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

  /* dimensionless is allowable in L2V2 */
  if (  c.getLevel() == 2 
    &&  c.getVersion() == 2)
  {
    inv_or( units == "volume" );
    inv_or( units == "litre"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfVolume() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "volume" );
    inv_or( units == "litre"  );
    inv_or( defn  != NULL && defn->isVariantOfVolume() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (1308, Compartment, c)
{
  msg =
    "CompartmentType '" + c.getCompartmentType() + "' is undefined. "
    "If the compartmentType field is given a value in a Compartment definition "
    "it must contain the identifier of an existing compartmentType. "
    "(L2V2 Section 4.7.2).";

  pre( c.getLevel() == 2 && c.getVersion() == 2 );
  pre( c.isSetCompartmentType());

  inv( m.getCompartmentType( c.getCompartmentType() ) != NULL );
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

  /* dimensionless is allowable in L2V2 */
  if (  s.getLevel() == 2 
    &&  s.getVersion() == 2)
  {
    inv_or( units == "length" );
    inv_or( units == "metre"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfLength() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "length" );
    inv_or( units == "metre"  );
    inv_or( defn  != NULL && defn->isVariantOfLength() );
  }
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

  /* dimensionless is allowable in L2V2 */
  if (  s.getLevel() == 2 
    &&  s.getVersion() == 2)
  {
    inv_or( units == "area" );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfArea() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "area" );
    inv_or( defn  != NULL && defn->isVariantOfArea() );
  }
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

  /* dimensionless is allowable in L2V2 */
  if (  s.getLevel() == 2 
    &&  s.getVersion() == 2)
  {
    inv_or( units == "volume" );
    inv_or( units == "litre"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfVolume() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "volume" );
    inv_or( units == "litre"  );
    inv_or( defn  != NULL && defn->isVariantOfVolume() );
  }
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

  /* dimensionless is allowable in L2V2 */
  if (  s.getLevel() == 2 
    &&  s.getVersion() == 2)
  {
    inv_or( units == "substance"      );
    inv_or( units == "item"           );
    inv_or( units == "mole"           );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfSubstance()     );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "substance" );
    inv_or( units == "item"      );
    inv_or( units == "mole"      );
    inv_or( defn  != NULL && defn->isVariantOfSubstance() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (1409, Species, s)
{
  msg =
    "A Species cannot set values for both initialConcentration and "
    "initial amount "
    "(L2v1 Section 4.6.3)";

  pre(s.isSetInitialAmount());

  inv (!s.isSetInitialConcentration());
}
END_CONSTRAINT


EXTERN_CONSTRAINT(1410, SpeciesReactionOrRule)


START_CONSTRAINT (1411, Species, s)
{
  msg =
    "SpeciesType '" + s.getSpeciesType() + "' is undefined. "
    "If the SpeciesType field is given a value in a Species definition "
    "it must contain the identifier of an existing SpeciesType. "
    "(L2V2 Section 4.7.2).";

  pre( s.getLevel() == 2 && s.getVersion() == 2 );
  pre( s.isSetSpeciesType());

  inv( m.getSpeciesType( s.getSpeciesType() ) != NULL );
}
END_CONSTRAINT


EXTERN_CONSTRAINT(1412, UniqueSpeciesTypesInCompartment)


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


START_CONSTRAINT (1601, SpeciesReference, sr)
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

  /* doesnt apply if the SpeciesReference is a modifier */
  pre(!sr.isModifier());

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

  /* doesnt apply if the SpeciesReference is a modifier */
  pre(!sr.isModifier());

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

  /* dimensionless is allowable in L2V2 */
  if (  kl.getLevel() == 2 
    &&  kl.getVersion() == 2)
  {
    inv_or( units == "substance" );
    inv_or( units == "item"  );
    inv_or( units == "mole"      );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfSubstance() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "substance" );
    inv_or( units == "item"      );
    inv_or( units == "mole"      );
    inv_or( defn  != NULL && defn->isVariantOfSubstance() );
  }
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

  /* dimensionless is allowable in L2V2 */
  if (  kl.getLevel() == 2 
    &&  kl.getVersion() == 2)
  {
    inv_or( units == "time" );
    inv_or( units == "second"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfTime() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "time"   );
    inv_or( units == "second" );
    inv_or( defn  != NULL && defn->isVariantOfTime() );
  }
}
END_CONSTRAINT


/*
START_CONSTRAINT (1606, Reaction, r)
{
  msg =
    "All Species referenced in a Reaction's KineticLaw must be listed as a "
    "reactant, product, or modifier of the Reaction (L2v1 Section x.y.z)";


  pre( r.isSetKineticLaw()            );
  pre( r.getKineticLaw()->isSetMath() );


  ASTNode* math  = r.getKineticLaw()->getMath();
  List*    names = math->getListOfNodes( ASTNode_isName );

  for (unsigned int n = 0; n < names.getSize(); ++n)
  {
    ASTNode*    node = static_cast<ASTNode*>( names.get(n) );
    const char* name = node->getName();

    if ( m.getSpecies(name) )
    {
      inv_or( r.getReactant(name) != NULL );
      inv_or( r.getProduct (name) != NULL );
      inv_or( r.getModifier(name) != NULL );
    }
  }
}
END_CONSTRAINT
*/

EXTERN_CONSTRAINT(1606, KineticLawVars)

EXTERN_CONSTRAINT(1607, StoichiometryMathVars)


START_CONSTRAINT (1610, KineticLaw, kl)
{
  msg =
    "In a Level 1 model only predefined functions are permitted "
     "within the KineticLaw formula. (L1V2 Appendix C)";

  pre (m.getLevel() == 1);

  pre (kl.isSetFormula() == 1);

  FormulaTokenizer_t * ft = FormulaTokenizer_create (kl.getFormula().c_str());
  Token_t * t = FormulaTokenizer_nextToken (ft);

  const Compartment * c;
  const Species * s;
  const Parameter * p, * p1;

  /* loop through each token of the formula
   * if it has type TT_NAME then it is either the id of some component
   * of the model or the name of a function in which case 
   * need to check whether it is defined
   */
  while (t->type != TT_END)
  {
    if (t->type == TT_NAME)
    {
      c = m.getCompartment(t->value.name);
      s = m.getSpecies    (t->value.name);
      p = m.getParameter  (t->value.name);
      p1 = kl.getParameter(t->value.name);

      if (!c && !s && !p && !p1)
      {
        inv_or (strcmp(t->value.name, "abs") == 0);
        inv_or (strcmp(t->value.name, "acos") == 0);
        inv_or (strcmp(t->value.name, "asin") == 0);
        inv_or (strcmp(t->value.name, "atan") == 0);
        inv_or (strcmp(t->value.name, "ceil") == 0);
        inv_or (strcmp(t->value.name, "cos") == 0);
        inv_or (strcmp(t->value.name, "exp") == 0);
        inv_or (strcmp(t->value.name, "floor") == 0);
        inv_or (strcmp(t->value.name, "log") == 0);
        inv_or (strcmp(t->value.name, "log10") == 0);
        inv_or (strcmp(t->value.name, "pow") == 0);
        inv_or (strcmp(t->value.name, "sqr") == 0);
        inv_or (strcmp(t->value.name, "sqrt") == 0);
        inv_or (strcmp(t->value.name, "sin") == 0);
        inv_or (strcmp(t->value.name, "tan") == 0);
      }
    }

    t = FormulaTokenizer_nextToken(ft);
  }
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
  pre( r.getLevel() == 2);

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

EXTERN_CONSTRAINT(1705, AssignmentCycles)

START_CONSTRAINT (1800, Event, e)
{
  msg =
    "An Event's timeUnits must be 'time', 'second', or the id of a "
    "UnitDefinition that defines a variant of 'second' with exponent='1' "
    "(L2v1 Section 4.10.4).";

  pre( e.isSetTimeUnits() );

  const string&         units = e.getTimeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (  e.getLevel() == 2 
    &&  e.getVersion() == 2)
  {
    inv_or( units == "time" );
    inv_or( units == "second"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfTime() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "time"   );
    inv_or( units == "second" );
    inv_or( defn  != NULL && defn->isVariantOfTime() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (1801, Event, e)
{
  msg =
    "An Event trigger must return a boolean value (L2v1 Section 4.10.2).";

  pre( e.isSetTrigger() );
  inv( m.isBoolean( e.getTrigger() ) );
}
END_CONSTRAINT


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


START_CONSTRAINT (1900, InitialAssignment, ia)
{
  msg =
    "The value of symbol in an InitialAssignment must be the id of a Compartment, Species, "
    "or Parameter (L2V2 Section 4.10).";


  pre( ia.isSetSymbol() );

  const string& id = ia.getSymbol();

  inv_or( m.getCompartment(id) );
  inv_or( m.getSpecies    (id) );
  inv_or( m.getParameter  (id) );
}
END_CONSTRAINT


EXTERN_CONSTRAINT(1901, UniqueSymbolsInInitialAssignments)
EXTERN_CONSTRAINT(1902, UniqueVarsInInitialAssignmentsAndRules)


START_CONSTRAINT (2100, Constraint, c)
{
  msg =
    "A Constraint math expression must return a boolean value (L2V2 Section 4.12).";

  pre( c.isSetMath() );
  inv( m.isBoolean( c.getMath() ) );
}
END_CONSTRAINT


START_CONSTRAINT (3000, AssignmentRule, ar)
{
  msg =
    "When the 'variable' field of an assignment rule refers to a "
    "compartment, the units of the rule's right-hand side must be "
    "consistent with either the units declared for that "
    "compartment, or (in the absence of explicit units declared "
    "for the compartment) the default units for that compartment.";


  const string& variable = ar.getVariable();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( ar.isSetMath() == 1 );

  // get the unitDefinition from the compartment and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ar.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromCompartment(c);
  
  pre (unitFormat->hasUndeclaredUnits(ar.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT


START_CONSTRAINT (3001, AssignmentRule, ar)
{
  msg =
    "When the 'variable' field of an assignment rule refers to a "
    "species, the units of the rule's right-hand side must be "
    "consistent with either the units declared for that "
    "species, or (in the absence of explicit units declared "
    "for the species) the default units for that species.";
   

  const string& variable = ar.getVariable();
  const Species * s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( ar.isSetMath() == 1 );

  // get the unitDefinition from the species and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ar.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromSpecies(s);
  
  pre (unitFormat->hasUndeclaredUnits(ar.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT


START_CONSTRAINT (3002, AssignmentRule, ar)
{
  msg =
    "When the 'variable' field of an assignment rule refers to a "
    "parameter, and the parameter's definition includes explicit "
    "units, then the units of the rule's right-hand side must be "
    "consistent the units declared for that parameter.";
   

  const string& variable = ar.getVariable();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( ar.isSetMath() == 1 );

  // get the unitDefinition from the parameter and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ar.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromParameter(p);

  // special case where no units have been declared for parameter
  pre (variableUnits->getNumUnits() != 0);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT




START_CONSTRAINT (3003, InitialAssignment, ia)
{
  msg =
    "When the 'variable' field of an initial assignment refers to a "
    "compartment, the units of the rule's right-hand side must be "
    "consistent with either the units declared for that "
    "compartment, or (in the absence of explicit units declared "
    "for the compartment) the default units for that compartment.";


  const string& variable = ia.getSymbol();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( ia.isSetMath() == 1 );

  // get the unitDefinition from the compartment and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ia.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromCompartment(c);
  
  pre (unitFormat->hasUndeclaredUnits(ia.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT


START_CONSTRAINT (3004, InitialAssignment, ia)
{
  msg =
    "When the 'variable' field of an initial assignment refers to a "
    "species, the units of the rule's right-hand side must be "
    "consistent with either the units declared for that "
    "species, or (in the absence of explicit units declared "
    "for the species) the default units for that species.";
   

  const string& variable = ia.getSymbol();
  const Species * s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( ia.isSetMath() == 1 );

  // get the unitDefinition from the species and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ia.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromSpecies(s);
  
  pre (unitFormat->hasUndeclaredUnits(ia.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT


START_CONSTRAINT (3005, InitialAssignment, ia)
{
  msg =
    "When the 'variable' field of an initial assignment refers to a "
    "parameter, and the parameter's definition includes explicit "
    "units, then the units of the rule's right-hand side must be "
    "consistent the units declared for that parameter.";
   

  const string& variable = ia.getSymbol();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( ia.isSetMath() == 1 );

  // get the unitDefinition from the parameter and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ia.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromParameter(p);

  // special case where no units have been declared for parameter
  pre (variableUnits->getNumUnits() != 0);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT


START_CONSTRAINT (3100, RateRule, rr)
{
  msg =
    "When the 'variable' field of a rate rule refers to a "
    "compartment, the units of the rule's right-hand side must be "
    "of the form 'x' per 'time', where 'x' refers to either the units "
    "declared for that compartment, or (in the absence of "
    "explicit units declared for the compartment) the default "
    "units for that compartment, and 'time' refers to the units "
    "of time for the model.";
   

  const string& variable = rr.getVariable();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( rr.isSetMath() == 1 );

  // get the unitDefinition from the compartment and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();
  Unit * time = new Unit("second", -1);

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(rr.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromCompartment(c);
  
  pre (unitFormat->hasUndeclaredUnits(rr.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  // add per time to the units from the compartment
  variableUnits->addUnit(time);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT

START_CONSTRAINT (3101, RateRule, rr)
{
  msg =
    "When the 'variable' field of a rate rule refers to a "
    "species, the units of the rule's right-hand side must be "
    "of the form 'x' per 'time', where 'x' refers to either the units "
    "declared for that species, or (in the absence of "
    "explicit units declared for the species) the default "
    "units for that species, and 'time' refers to the units "
    "of time for the model.";
 
  const string& variable = rr.getVariable();
  const Species* s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( rr.isSetMath() == 1 );

  // get the unitDefinition from the species and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();
  Unit * time = new Unit("second", -1);

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(rr.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromSpecies(s);
  
  pre (unitFormat->hasUndeclaredUnits(rr.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);


  // add per time to the units from the species
  variableUnits->addUnit(time);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)
  
  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT

START_CONSTRAINT (3102, RateRule, rr)
{
  msg =
    "When the 'variable' field of a rate rule refers to a "
    "parameter, and the parameter's definition includes explicit "
    "units, the units of the rule's right-hand side must be "
    "of the form 'x' per 'time', where 'x' refers to the units "
    "declared for that parameter, and 'time' refers to "
    "the units of time for the model.";
   

  const string& variable = rr.getVariable();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( rr.isSetMath() == 1 );

  // get the unitDefinition from the parameter and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();
  Unit * time = new Unit("second", -1);

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(rr.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromParameter(p);

  // special case where no units have been declared for parameter
  pre (variableUnits->getNumUnits() != 0);
  
  // add per time to the units from the parameter
  variableUnits->addUnit(time);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)
  
  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT

START_CONSTRAINT (3200, KineticLaw, kl)
{
  msg =
    "The units of the formula of a kinetic law "
    "must be 'substance per time'.";

  pre ( kl.isSetMath() == 1 );

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  pre (unitFormat->hasUndeclaredUnits(kl.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  UnitDefinition * formulaUnits = new UnitDefinition();
  
  UnitDefinition * SubsTime = new UnitDefinition();
  Unit * subs = new Unit("mole");
  Unit * time = new Unit("second", -1);
  SubsTime->addUnit(subs);
  SubsTime->addUnit(time);


  formulaUnits  = unitFormat->getUnitDefinition(kl.getMath());

  inv (areEquivalent(formulaUnits, SubsTime) == 1)

  delete unitFormat;
}
END_CONSTRAINT

EXTERN_CONSTRAINT(3302, PowerUnitsCheck)
EXTERN_CONSTRAINT(3303, ExponentUnitsCheck)
EXTERN_CONSTRAINT(3304, ArgumentsUnitsCheck)
 
START_CONSTRAINT (3400, Event, e)
{
  msg =
    "When a delay is specified within an event the units "
    "of the delay formula must correspond to the timeUnits "
    "specified within the event or, if no timeUnits are "
    "specified, correspond to the builtin unit 'time'.";

  pre ( e.isSetDelay() == 1 );

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  pre (unitFormat->hasUndeclaredUnits(e.getDelay()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);
  
  UnitDefinition * formulaUnits = new UnitDefinition();
  formulaUnits  = unitFormat->getUnitDefinition(e.getDelay());

  UnitDefinition * timeUnits = 
    unitFormat->getUnitDefinitionFromEventTime(&e);

    inv (areIdentical(formulaUnits, timeUnits) == 1)

  delete unitFormat;
}
END_CONSTRAINT



