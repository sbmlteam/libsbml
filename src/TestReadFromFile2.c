/**
 * Filename    : TestReadFromFile2.c
 * Description : Reads tests/l1v1-units.xml into memory and tests it.
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-24
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <sbml/common.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>


extern char *TestDataDirectory;


START_TEST (test_read_l1v1_units)
{
  SBMLDocument_t     *d;
  Model_t            *m;
  Compartment_t      *c;
  KineticLaw_t       *kl;
  Parameter_t        *p;
  Reaction_t         *r;
  Species_t          *s;
  SpeciesReference_t *sr;
  Unit_t             *u;
  UnitDefinition_t   *ud;

  char *filename = safe_strcat(TestDataDirectory, "l1v1-units.xml");


  d = readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"l1v1-units.xml\") returned a NULL pointer.");
  }

  safe_free(filename);


  /**
   * <sbml level="1" version="1">
   */
  fail_unless( d->level   == 1, NULL );
  fail_unless( d->version == 1, NULL );

  m = d->model;


  /**
   * <listOfUnitDefinitions>
   *   <unitDefinition name="substance"> ... </unitDefinition>
   *   <unitDefinition name="mls">       ... </unitDefinition>
   * </listOfUnitDefinitions>
   */
  fail_unless( Model_getNumUnitDefinitions(m) == 2, NULL );

  ud = Model_getUnitDefinition(m, 0);
  fail_unless ( !strcmp(ud->name, "substance"), NULL );

  ud = Model_getUnitDefinition(m, 1);
  fail_unless ( !strcmp(ud->name, "mls"), NULL );


  /**
   * <unitDefinition name="substance">
   *   <listOfUnits>
   *     <unit kind="mole" scale="-3"/>
   *   </listOfUnits>
   * </unitDefinition>
   */
  ud = Model_getUnitDefinition(m, 0);
  fail_unless( UnitDefinition_getNumUnits(ud) == 1, NULL );

  u = UnitDefinition_getUnit(ud, 0);
  fail_unless( u->kind     == UNIT_KIND_MOLE, NULL );
  fail_unless( u->exponent ==  1, NULL );
  fail_unless( u->scale    == -3, NULL );


  /**
   * <unitDefinition name="mls">
   *   <listOfUnits>
   *     <unit kind="mole"   scale="-3"/>
   *     <unit kind="liter"  exponent="-1"/>
   *     <unit kind="second" exponent="-1"/>
   *   </listOfUnits>
   * </unitDefinition>
   */
  ud = Model_getUnitDefinition(m, 1);
  fail_unless( UnitDefinition_getNumUnits(ud) == 3, NULL );

  u = UnitDefinition_getUnit(ud, 0);
  fail_unless( u->kind     == UNIT_KIND_MOLE, NULL );
  fail_unless( u->exponent ==  1, NULL );
  fail_unless( u->scale    == -3, NULL );

  u = UnitDefinition_getUnit(ud, 1);
  fail_unless( u->kind     == UNIT_KIND_LITER, NULL );
  fail_unless( u->exponent == -1, NULL );
  fail_unless( u->scale    ==  0, NULL );

  u = UnitDefinition_getUnit(ud, 2);
  fail_unless( u->kind     == UNIT_KIND_SECOND, NULL );
  fail_unless( u->exponent == -1, NULL );
  fail_unless( u->scale    ==  0, NULL );

  /**
   * <listOfCompartments>
   *   <compartment name="cell"/>
   * </listOfCompartments>
   */
  fail_unless( Model_getNumCompartments(m) == 1, NULL );

  c = Model_getCompartment(m, 0);
  fail_unless( !strcmp(c->name, "cell"), NULL );


  /**
   * <listOfSpecies>
   *   <specie name="x0" compartment="cell" initialAmount="1"/>
   *   <specie name="x1" compartment="cell" initialAmount="1"/>
   *   <specie name="s1" compartment="cell" initialAmount="1"/>
   *   <specie name="s2" compartment="cell" initialAmount="1"/>
   * </listOfSpecies>
   */
  fail_unless( Model_getNumSpecies(m) == 4, NULL );

  s = Model_getSpecies(m, 0);
  fail_unless( !strcmp( s->name       , "x0"   ), NULL );
  fail_unless( !strcmp( s->compartment, "cell" ), NULL );
  fail_unless( s->initialAmount     == 1, NULL );
  fail_unless( s->boundaryCondition == 0, NULL );
  
  s = Model_getSpecies(m, 1);
  fail_unless( !strcmp( s->name       , "x1"   ), NULL );
  fail_unless( !strcmp( s->compartment, "cell" ), NULL );
  fail_unless( s->initialAmount     == 1, NULL );
  fail_unless( s->boundaryCondition == 0, NULL );

  s = Model_getSpecies(m, 2);
  fail_unless( !strcmp( s->name       , "s1"   ), NULL );
  fail_unless( !strcmp( s->compartment, "cell" ), NULL );
  fail_unless( s->initialAmount     == 1, NULL );
  fail_unless( s->boundaryCondition == 0, NULL );

  s = Model_getSpecies(m, 3);
  fail_unless( !strcmp( s->name       , "s2"   ), NULL );
  fail_unless( !strcmp( s->compartment, "cell" ), NULL );
  fail_unless( s->initialAmount     == 1, NULL );
  fail_unless( s->boundaryCondition == 0, NULL );


  /**
   * <listOfParameters>
   *   <parameter name="vm" value="2" units="mls"/>
   *   <parameter name="km" value="2"/>
   * </listOfParameters>
   */
  fail_unless( Model_getNumParameters(m) == 2, NULL );

  p = Model_getParameter(m, 0);
  fail_unless( !strcmp( p->name , "vm"  ), NULL );
  fail_unless( !strcmp( p->units, "mls" ), NULL );
  fail_unless( p->value == 2, NULL );

  p = Model_getParameter(m, 1);
  fail_unless( !strcmp( p->name , "km"  ), NULL );
  fail_unless( p->value == 2, NULL );

  /**
   * <listOfReactions>
   *   <reaction name="v1"> ... </reaction>
   *   <reaction name="v2"> ... </reaction>
   *   <reaction name="v3"> ... </reaction>
   * </listOfReactions>
   */
  fail_unless( Model_getNumReactions(m) == 3, NULL );

  r = Model_getReaction(m, 0);
  fail_unless( !strcmp(r->name , "v1"), NULL );
  fail_unless( r->reversible != 0, NULL );
  fail_unless( r->fast       == 0, NULL );

  r = Model_getReaction(m, 1);
  fail_unless( !strcmp(r->name , "v2"), NULL );
  fail_unless( r->reversible != 0, NULL );
  fail_unless( r->fast       == 0, NULL );

  r = Model_getReaction(m, 2);
  fail_unless( !strcmp(r->name , "v3"), NULL );
  fail_unless( r->reversible != 0, NULL );
  fail_unless( r->fast       == 0, NULL );


  /**
   * <reaction name="v1">
   *   <listOfReactants>
   *     <specieReference specie="x0"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="s1"/>
   *   </listOfProducts>
   *   <kineticLaw formula="(vm * s1)/(km + s1)"/>
   * </reaction>
   */
  r = Model_getReaction(m, 0);

  fail_unless( Reaction_getNumReactants(r) == 1, NULL );
  fail_unless( Reaction_getNumProducts(r)  == 1, NULL );

  sr = Reaction_getReactant(r, 0);
  fail_unless( !strcmp(sr->species, "x0"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(sr->species, "s1"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  kl = r->kineticLaw;
  fail_unless( !strcmp(kl->formula, "(vm * s1)/(km + s1)"), NULL );


  /**
   * <reaction name="v2">
   *   <listOfReactants>
   *     <specieReference specie="s1"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="s2"/>
   *   </listOfProducts>
   *   <kineticLaw formula="(vm * s2)/(km + s2)"/>
   * </reaction>
   */
  r = Model_getReaction(m, 1);

  fail_unless( Reaction_getNumReactants(r) == 1, NULL );
  fail_unless( Reaction_getNumProducts(r)  == 1, NULL );

  sr = Reaction_getReactant(r, 0);
  fail_unless( !strcmp(sr->species, "s1"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(sr->species, "s2"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  kl = r->kineticLaw;
  fail_unless( !strcmp(kl->formula, "(vm * s2)/(km + s2)"), NULL );


  /**
   * <reaction name="v3">
   *   <listOfReactants>
   *     <specieReference specie="s2"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="x1"/>
   *   </listOfProducts>
   *   <kineticLaw formula="(vm * s1)/(km + s1)"/>
   * </reaction>
   */
  r = Model_getReaction(m, 2);

  fail_unless( Reaction_getNumReactants(r) == 1, NULL );
  fail_unless( Reaction_getNumProducts(r)  == 1, NULL );

  sr = Reaction_getReactant(r, 0);
  fail_unless( !strcmp(sr->species, "s2"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(sr->species, "x1"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  kl = r->kineticLaw;
  fail_unless( !strcmp(kl->formula, "(vm * s1)/(km + s1)"), NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_create_l1v1_units)
{
  SBMLDocument_t     *d;
  Model_t            *m;
  Compartment_t      *c;
  KineticLaw_t       *kl;
  Parameter_t        *p;
  Reaction_t         *r;
  Species_t          *s;
  SpeciesReference_t *sr;
  Unit_t             *u;
  UnitDefinition_t   *ud;


  /**
   * <sbml level="1" version="1">
   */
  d = SBMLDocument_create();
  m = Model_create();

  d->model = m;


  /**
   * <unitDefinition name="substance">
   *   <listOfUnits>
   *     <unit kind="mole" scale="-3"/>
   *   </listOfUnits>
   * </unitDefinition>
   */
  ud = Model_createUnitDefinition(m);
  UnitDefinition_setName(ud, "substance");

  u = Model_createUnit(m);
  u->kind  = UNIT_KIND_MOLE;
  u->scale = -3;

  /**
   * <unitDefinition name="mls">
   *   <listOfUnits>
   *     <unit kind="mole"   scale="-3"/>
   *     <unit kind="liter"  exponent="-1"/>
   *     <unit kind="second" exponent="-1"/>
   *   </listOfUnits>
   * </unitDefinition>
   */
  ud = Model_createUnitDefinition(m);
  UnitDefinition_setName(ud, "mls");

  u = Model_createUnit(m);
  u->kind  = UNIT_KIND_MOLE;
  u->scale = -3;

  u = Model_createUnit(m);
  u->kind     = UNIT_KIND_LITER;
  u->exponent = -1;

  u = Model_createUnit(m);
  u->kind     = UNIT_KIND_SECOND;
  u->exponent = -1;

  /**
   * <listOfCompartments>
   *   <compartment name="cell"/>
   * </listOfCompartments>
   */
  c = Model_createCompartment(m);
  Compartment_setName(c, "cell");

  /**
   * <listOfSpecies>
   *   <specie name="x0" compartment="cell" initialAmount="1"/>
   *   <specie name="x1" compartment="cell" initialAmount="1"/>
   *   <specie name="s1" compartment="cell" initialAmount="1"/>
   *   <specie name="s2" compartment="cell" initialAmount="1"/>
   * </listOfSpecies>
   */
  s = Model_createSpecies(m);
  Species_setName(s, "x0");
  Species_setCompartment(s, "cell");
  s->initialAmount = 1;

  s = Model_createSpecies(m);
  Species_setName(s, "x1");
  Species_setCompartment(s, "cell");
  s->initialAmount = 1;

  s = Model_createSpecies(m);
  Species_setName(s, "s1");
  Species_setCompartment(s, "cell");
  s->initialAmount = 1;

  s = Model_createSpecies(m);
  Species_setName(s, "s2");
  Species_setCompartment(s, "cell");
  s->initialAmount = 1;

  /**
   * <listOfParameters>
   *   <parameter name="vm" value="2" units="mls"/>
   *   <parameter name="km" value="2"/>
   * </listOfParameters>
   */
  p = Model_createParameter(m);
  Parameter_setName(p, "vm");
  Parameter_setUnits(p, "mls");
  p->value = 2;

  p = Model_createParameter(m);
  Parameter_setName(p, "km");
  p->value = 2;

  /**
   * <reaction name="v1">
   *   <listOfReactants>
   *     <specieReference specie="x0"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="s1"/>
   *   </listOfProducts>
   *   <kineticLaw formula="(vm * s1)/(km + s1)"/>
   * </reaction>
   */
  r = Model_createReaction(m);
  Reaction_setName(r, "v1");

  sr = Model_createReactant(m);
  SpeciesReference_setSpecies(sr, "x0");

  sr = Model_createProduct(m);
  SpeciesReference_setSpecies(sr, "s1");

  kl = Model_createKineticLaw(m);
  KineticLaw_setFormula(kl, "(vm * s1)/(km + s1)");

  /**
   * <reaction name="v2">
   *   <listOfReactants>
   *     <specieReference specie="s1"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="s2"/>
   *   </listOfProducts>
   *   <kineticLaw formula="(vm * s2)/(km + s2)"/>
   * </reaction>
   */
  r = Model_createReaction(m);
  Reaction_setName(r, "v2");

  sr = Model_createReactant(m);
  SpeciesReference_setSpecies(sr, "s1");

  sr = Model_createProduct(m);
  SpeciesReference_setSpecies(sr, "s2");

  kl = Model_createKineticLaw(m);
  KineticLaw_setFormula(kl, "(vm * s2)/(km + s2)");

  /**
   * <reaction name="v3">
   *   <listOfReactants>
   *     <specieReference specie="s2"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="x1"/>
   *   </listOfProducts>
   *   <kineticLaw formula="(vm * s1)/(km + s1)"/>
   * </reaction>
   */
  r = Model_createReaction(m);
  Reaction_setName(r, "v3");

  sr = Model_createReactant(m);
  SpeciesReference_setSpecies(sr, "s2");

  sr = Model_createProduct(m);
  SpeciesReference_setSpecies(sr, "x1");

  kl = Model_createKineticLaw(m);
  KineticLaw_setFormula(kl, "(vm * s1)/(km + s1)");

  SBMLDocument_free(d);
}
END_TEST


Suite *
create_suite_TestReadFromFile2 (void) 
{ 
  Suite *suite = suite_create("test-data/l1v1-units.xml");
  TCase *tcase = tcase_create("test-data/l1v1-units.xml");


  tcase_add_test(tcase, test_read_l1v1_units);
  tcase_add_test(tcase, test_create_l1v1_units);

  suite_add_tcase(suite, tcase);

  return suite;
}
