/**
 * Filename    : TestReadFromFile3.c
 * Description : Reads tests/l1v1-rules.xml into memory and tests it.
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-25
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


START_TEST (test_read_l1v1_rules)
{
  SBMLDocument_t             *d;
  Model_t                    *m;
  Compartment_t              *c;
  KineticLaw_t               *kl;
  Parameter_t                *p;
  ParameterRule_t            *pr;
  Reaction_t                 *r;
  Species_t                  *s;
  SpeciesConcentrationRule_t *scr;
  SpeciesReference_t         *sr;

  char *filename = safe_strcat(TestDataDirectory, "l1v1-rules.xml");


  d = readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"l1v1-rules.xml\") returned a NULL pointer.");
  }

  safe_free(filename);


  /**
   * <sbml level="1" version="1" ...>
   */
  fail_unless( d->level   == 1, NULL );
  fail_unless( d->version == 1, NULL );


  /**
   * <model>
   */
  m = d->model;

  /**
   * <listOfCompartments>
   *  <compartment name="cell" volume="1"/>
   * </listOfCompartments>
   */
  fail_unless( Model_getNumCompartments(m) == 1, NULL );

  c = Model_getCompartment(m, 0);
  fail_unless( !strcmp(c->name, "cell"), NULL );
  fail_unless( c->volume == 1, NULL );


  /**
   * <listOfSpecies>
   *   <specie name="s1" compartment="cell" initialAmount="4"/>
   *   <specie name="s2" compartment="cell" initialAmount="2"/>
   *   <specie name="x0" compartment="cell" initialAmount="1"/>
   *   <specie name="x1" compartment="cell" initialAmount="0"/>
   * </listOfSpecies>
   */
  fail_unless( Model_getNumSpecies(m) == 4, NULL );

  s = Model_getSpecies(m, 0);
  fail_unless( !strcmp( s->name       , "s1"   ), NULL );
  fail_unless( !strcmp( s->compartment, "cell" ), NULL );
  fail_unless( s->initialAmount     == 4, NULL );
  fail_unless( s->boundaryCondition == 0, NULL );

  s = Model_getSpecies(m, 1);
  fail_unless( !strcmp( s->name       , "s2"   ), NULL );
  fail_unless( !strcmp( s->compartment, "cell" ), NULL );
  fail_unless( s->initialAmount     == 2, NULL );
  fail_unless( s->boundaryCondition == 0, NULL );

  s = Model_getSpecies(m, 2);
  fail_unless( !strcmp( s->name       , "x0"   ), NULL );
  fail_unless( !strcmp( s->compartment, "cell" ), NULL );
  fail_unless( s->initialAmount     == 1, NULL );
  fail_unless( s->boundaryCondition == 0, NULL );

  s = Model_getSpecies(m, 3);
  fail_unless( !strcmp( s->name       , "x1"   ), NULL );
  fail_unless( !strcmp( s->compartment, "cell" ), NULL );
  fail_unless( s->initialAmount     == 0, NULL );
  fail_unless( s->boundaryCondition == 0, NULL );


  /**
   * <listOfParameters>
   *   <parameter name="k1" value="1.2"/>
   *   <parameter name="k2" value="1000"/>
   *   <parameter name="k3" value="3000"/>
   *   <parameter name="k4" value="4.5"/>
   * </listOfParameters>
   */
  fail_unless( Model_getNumParameters(m) == 4, NULL );

  p = Model_getParameter(m, 0);
  fail_unless( !strcmp(p->name, "k1"), NULL );
  fail_unless( p->value == 1.2, NULL );

  p = Model_getParameter(m, 1);
  fail_unless( !strcmp(p->name, "k2"), NULL );
  fail_unless( p->value == 1000, NULL );

  p = Model_getParameter(m, 2);
  fail_unless( !strcmp(p->name, "k3"), NULL );
  fail_unless( p->value == 3000, NULL );

  p = Model_getParameter(m, 3);
  fail_unless( !strcmp(p->name, "k4"), NULL );
  fail_unless( p->value == 4.5, NULL );


  /**
   * <listOfRules>
   *   <parameterRule name="t" formula="s1 + s2"/>
   *   <parameterRule name="k" formula="k3/k2"/>
   *   <specieConcentrationRule specie="s2" formula="k * t/(1 + k)"/>
   *   <specieConcentrationRule specie="s1" formula="t - s2"/>
   * </listOfRules>
   */
  fail_unless( Model_getNumRules(m) == 4, NULL );

  pr = (ParameterRule_t *) Model_getRule(m, 0);
  fail_unless( !strcmp(pr->name   , "t"      ), NULL );
  fail_unless( !strcmp(pr->formula, "s1 + s2"), NULL );

  pr = (ParameterRule_t *) Model_getRule(m, 1);
  fail_unless( !strcmp(pr->name   , "k"    ), NULL );
  fail_unless( !strcmp(pr->formula, "k3/k2"), NULL );

  scr = (SpeciesConcentrationRule_t *) Model_getRule(m, 2);
  fail_unless( !strcmp(scr->species, "s2"           ), NULL );
  fail_unless( !strcmp(scr->formula, "k * t/(1 + k)"), NULL );

  scr = (SpeciesConcentrationRule_t *) Model_getRule(m, 3);
  fail_unless( !strcmp(scr->species, "s1"    ), NULL );
  fail_unless( !strcmp(scr->formula, "t - s2"), NULL );


  /**
   * <listOfReactions>
   *   <reaction name="j1" > ... </reaction>
   *   <reaction name="j3" > ... </reaction>
   * </listOfReactions>
   */
  fail_unless( Model_getNumReactions(m) == 2, NULL );

  r = Model_getReaction(m, 0);
  fail_unless( !strcmp(r->name, "j1"), NULL );
  fail_unless( r->reversible != 0, NULL );
  fail_unless( r->fast       == 0, NULL );

  r = Model_getReaction(m, 1);
  fail_unless( !strcmp(r->name, "j3"), NULL );
  fail_unless( r->reversible != 0, NULL );
  fail_unless( r->fast       == 0, NULL );


  /**
   * <reaction name="j1">
   *   <listOfReactants>
   *     <specieReference specie="x0"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="s1"/>
   *   </listOfProducts>
   *   <kineticLaw formula="k1 * x0"/>
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
  fail_unless( !strcmp(kl->formula, "k1 * x0"), NULL );


  /**
   * <reaction name="j3">
   *   <listOfReactants>
   *     <specieReference specie="s2"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="x1"/>
   *   </listOfProducts>
   *   <kineticLaw formula="k4 * s2"/>
   * </reaction>
   */
  r = Model_getReaction(m, 1);

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
  fail_unless( !strcmp(kl->formula, "k4 * s2"), NULL );

  SBMLDocument_free(d);
}
END_TEST


Suite *
create_suite_TestReadFromFile3 (void)
{ 
  Suite *suite = suite_create("tests/l1v1-rules.xml");
  TCase *tcase = tcase_create("tests/l1v1-rules.xml");


  tcase_add_test(tcase, test_read_l1v1_rules);

  suite_add_tcase(suite, tcase);

  return suite;
}
