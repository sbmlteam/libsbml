/**
 * Filename    : TestReadFromFile1.c
 * Description : Reads tests/l1v1-branch.xml into memory and tests it.
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


START_TEST (test_read_l1v1_branch)
{
  SBMLDocument_t     *d;
  Model_t            *m;
  Compartment_t      *c;
  KineticLaw_t       *kl;
  Parameter_t        *p;
  Reaction_t         *r;
  Species_t          *s;
  SpeciesReference_t *sr;

  char notes[] = 
    "\n"
    "      <body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
    "        <p>Simple branch system.</p>\n"
    "        <p>The reaction looks like this:</p>\n"
    "        <p>reaction-1:   X0 -> S1; k1*X0;</p>\n"
    "        <p>reaction-2:   S1 -> X1; k2*S1;</p>\n"
    "        <p>reaction-3:   S1 -> X2; k3*S1;</p>\n"
    "      </body>\n"
    "    ";

  char *filename = safe_strcat(TestDataDirectory, "l1v1-branch.xml");


  d = readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"l1v1-branch.xml\") returned a NULL pointer.");
  }

  safe_free(filename);


  /**
   * <sbml level="1" version="1" ...>
   */
  fail_unless( d->level   == 1, NULL );
  fail_unless( d->version == 1, NULL );


  /**
   * <model name="Branch">
   */
  m = d->model;

  fail_unless( !strcmp( m->name , "Branch"), NULL );
  fail_unless( !strcmp( m->notes, notes   ), NULL );


  /**
   * <listOfCompartments>
   *  <compartment name="compartmentOne" volume="1"/>
   * </listOfCompartments>
   */
  fail_unless( Model_getNumCompartments(m) == 1, NULL );

  c = Model_getCompartment(m, 0);
  fail_unless( !strcmp(c->name, "compartmentOne"), NULL );
  fail_unless( c->volume == 1, NULL );


  /**
   * <listOfSpecies>
   *   <specie name="S1" initialAmount="0" compartment="compartmentOne"
   *           boundaryCondition="false"/>
   *   <specie name="X0" initialAmount="0" compartment="compartmentOne"
   *           boundaryCondition="true"/>
   *   <specie name="X1" initialAmount="0" compartment="compartmentOne"
   *           boundaryCondition="true"/>
   *   <specie name="X2" initialAmount="0" compartment="compartmentOne"
   *           boundaryCondition="true"/>
   * </listOfSpecies>
   */
  fail_unless( Model_getNumSpecies(m) == 4, NULL );

  s = Model_getSpecies(m, 0);
  fail_unless( !strcmp( s->name       , "S1"             ), NULL );
  fail_unless( !strcmp( s->compartment, "compartmentOne" ), NULL );
  fail_unless( s->initialAmount     == 0, NULL );
  fail_unless( s->boundaryCondition == 0, NULL );

  s = Model_getSpecies(m, 1);
  fail_unless( !strcmp( s->name       , "X0"             ), NULL );
  fail_unless( !strcmp( s->compartment, "compartmentOne" ), NULL );
  fail_unless( s->initialAmount     == 0, NULL );
  fail_unless( s->boundaryCondition == 1, NULL );

  s = Model_getSpecies(m, 2);
  fail_unless( !strcmp( s->name       , "X1"             ), NULL );
  fail_unless( !strcmp( s->compartment, "compartmentOne" ), NULL );
  fail_unless( s->initialAmount     == 0, NULL );
  fail_unless( s->boundaryCondition == 1, NULL );

  s = Model_getSpecies(m, 3);
  fail_unless( !strcmp( s->name       , "X2"             ), NULL );
  fail_unless( !strcmp( s->compartment, "compartmentOne" ), NULL );
  fail_unless( s->initialAmount     == 0, NULL );
  fail_unless( s->boundaryCondition == 1, NULL );


  /**
   * <listOfReactions>
   *   <reaction name="reaction_1" reversible="false"> ... </reaction>
   *   <reaction name="reaction_2" reversible="false"> ... </reaction>
   *   <reaction name="reaction_3" reversible="false"> ... </reaction>
   * </listOfReactions>
   */
  fail_unless( Model_getNumReactions(m) == 3, NULL );

  r = Model_getReaction(m, 0);
  fail_unless( !strcmp(r->name, "reaction_1"), NULL );
  fail_unless( r->reversible == 0, NULL );
  fail_unless( r->fast       == 0, NULL );

  r = Model_getReaction(m, 1);
  fail_unless( !strcmp(r->name, "reaction_2"), NULL );
  fail_unless( r->reversible == 0, NULL );
  fail_unless( r->fast       == 0, NULL );

  r = Model_getReaction(m, 2);
  fail_unless( !strcmp(r->name, "reaction_3"), NULL );
  fail_unless( r->reversible == 0, NULL );
  fail_unless( r->fast       == 0, NULL );

  /**
   * <reaction name="reaction_1" reversible="false">
   *   <listOfReactants>
   *     <specieReference specie="X0" stoichiometry="1"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="S1" stoichiometry="1"/>
   *   </listOfProducts>
   *   <kineticLaw formula="k1 * X0">
   *     <listOfParameters>
   *       <parameter name="k1" value="0"/>
   *     </listOfParameters>
   *   </kineticLaw>
   * </reaction>
   */
  r = Model_getReaction(m, 0);

  fail_unless( Reaction_getNumReactants(r) == 1, NULL );
  fail_unless( Reaction_getNumProducts(r)  == 1, NULL );

  sr = Reaction_getReactant(r, 0);
  fail_unless( !strcmp(sr->species, "X0"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(sr->species, "S1"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  kl = r->kineticLaw;
  fail_unless( !strcmp(kl->formula, "k1 * X0"), NULL );
  fail_unless( KineticLaw_getNumParameters(kl) == 1, NULL );

  p = KineticLaw_getParameter(kl, 0);
  fail_unless( !strcmp(p->name, "k1"), NULL );
  fail_unless( p->value == 0, NULL );


  /**
   * <reaction name="reaction_2" reversible="false">
   *   <listOfReactants>
   *     <specieReference specie="S1" stoichiometry="1"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="X1" stoichiometry="1"/>
   *   </listOfProducts>
   *   <kineticLaw formula="k2 * S1">
   *     <listOfParameters>
   *       <parameter name="k2" value="0"/>
   *     </listOfParameters>
   *   </kineticLaw>
   * </reaction>
   */
  r = Model_getReaction(m, 1);
  fail_unless( Reaction_getNumReactants(r) == 1, NULL );
  fail_unless( Reaction_getNumProducts(r)  == 1, NULL );

  sr = Reaction_getReactant(r, 0);
  fail_unless( !strcmp(sr->species, "S1"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(sr->species, "X1"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  kl = r->kineticLaw;
  fail_unless( !strcmp(kl->formula, "k2 * S1"), NULL );
  fail_unless( KineticLaw_getNumParameters(kl) == 1, NULL );

  p = KineticLaw_getParameter(kl, 0);
  fail_unless( !strcmp(p->name, "k2"), NULL );
  fail_unless( p->value == 0, NULL );


  /**
   * <reaction name="reaction_3" reversible="false">
   *   <listOfReactants>
   *     <specieReference specie="S1" stoichiometry="1"/>
   *   </listOfReactants>
   *   <listOfProducts>
   *     <specieReference specie="X2" stoichiometry="1"/>
   *   </listOfProducts>
   *   <kineticLaw formula="k3 * S1">
   *     <listOfParameters>
   *       <parameter name="k3" value="0"/>
   *     </listOfParameters>
   *   </kineticLaw>
   * </reaction>
   */
  r = Model_getReaction(m, 2);
  fail_unless( Reaction_getNumReactants(r) == 1, NULL );
  fail_unless( Reaction_getNumProducts(r)  == 1, NULL );

  sr = Reaction_getReactant(r, 0);
  fail_unless( !strcmp(sr->species, "S1"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(sr->species, "X2"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );

  kl = r->kineticLaw;
  fail_unless( !strcmp(kl->formula, "k3 * S1"), NULL );
  fail_unless( KineticLaw_getNumParameters(kl) == 1, NULL );

  p = KineticLaw_getParameter(kl, 0);
  fail_unless( !strcmp(p->name, "k3"), NULL );
  fail_unless( p->value == 0, NULL );

  SBMLDocument_free(d);
}
END_TEST


Suite *
create_suite_TestReadFromFile1 (void)
{ 
  Suite *suite = suite_create("test-data/l1v1-branch.xml");
  TCase *tcase = tcase_create("test-data/l1v1-branch.xml");


  tcase_add_test(tcase, test_read_l1v1_branch);

  suite_add_tcase(suite, tcase);

  return suite;
}
