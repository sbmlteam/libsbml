/**
 * \file    TestReadFromFile1.c
 * \brief   Reads tests/l1v1-branch.xml into memory and tests it.
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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


#include <check.h>

#include "common/common.h"
#include "SBMLReader.h"
#include "SBMLTypes.h"


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

  char notes1[] = 
    "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
    "        <p>Simple branch system.</p>\n"
    "        <p>The reaction looks like this:</p>\n"
    "        <p>reaction-1:   X0 -> S1; k1*X0;</p>\n"
    "        <p>reaction-2:   S1 -> X1; k2*S1;</p>\n"
    "        <p>reaction-3:   S1 -> X2; k3*S1;</p>\n"
    "      </body>";

  /* Xerces-C 2.4.0 escapes the '>' with '&gt;'. */
  char notes2[] = 
    "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n"
    "        <p>Simple branch system.</p>\n"
    "        <p>The reaction looks like this:</p>\n"
    "        <p>reaction-1:   X0 -&gt; S1; k1*X0;</p>\n"
    "        <p>reaction-2:   S1 -&gt; X1; k2*S1;</p>\n"
    "        <p>reaction-3:   S1 -&gt; X2; k3*S1;</p>\n"
    "      </body>";

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
  fail_unless( SBMLDocument_getLevel  (d) == 1, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );


  /**
   * <model name="Branch">
   */
  m = SBMLDocument_getModel(d);

  fail_unless( !strcmp( Model_getName(m) , "Branch"), NULL );
  fail_unless( !strcmp( SBase_getNotes((SBase_t *) m), notes1) ||
               !strcmp( SBase_getNotes((SBase_t *) m), notes2), NULL );


  /**
   * <listOfCompartments>
   *  <compartment name="compartmentOne" volume="1"/>
   * </listOfCompartments>
   */
  fail_unless( Model_getNumCompartments(m) == 1, NULL );

  c = Model_getCompartment(m, 0);
  fail_unless( !strcmp(Compartment_getName(c), "compartmentOne"), NULL );
  fail_unless( Compartment_getVolume(c) == 1, NULL );


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
  fail_unless( !strcmp( Species_getName       (s), "S1"             ), NULL );
  fail_unless( !strcmp( Species_getCompartment(s), "compartmentOne" ), NULL );
  fail_unless( Species_getInitialAmount    (s) == 0, NULL );
  fail_unless( Species_getBoundaryCondition(s) == 0, NULL );

  s = Model_getSpecies(m, 1);
  fail_unless( !strcmp( Species_getName       (s), "X0"             ), NULL );
  fail_unless( !strcmp( Species_getCompartment(s), "compartmentOne" ), NULL );
  fail_unless( Species_getInitialAmount    (s) == 0, NULL );
  fail_unless( Species_getBoundaryCondition(s) == 1, NULL );

  s = Model_getSpecies(m, 2);
  fail_unless( !strcmp( Species_getName       (s), "X1"             ), NULL );
  fail_unless( !strcmp( Species_getCompartment(s), "compartmentOne" ), NULL );
  fail_unless( Species_getInitialAmount    (s) == 0, NULL );
  fail_unless( Species_getBoundaryCondition(s) == 1, NULL );

  s = Model_getSpecies(m, 3);
  fail_unless( !strcmp( Species_getName       (s), "X2"             ), NULL );
  fail_unless( !strcmp( Species_getCompartment(s), "compartmentOne" ), NULL );
  fail_unless( Species_getInitialAmount    (s) == 0, NULL );
  fail_unless( Species_getBoundaryCondition(s) == 1, NULL );


  /**
   * <listOfReactions>
   *   <reaction name="reaction_1" reversible="false"> ... </reaction>
   *   <reaction name="reaction_2" reversible="false"> ... </reaction>
   *   <reaction name="reaction_3" reversible="false"> ... </reaction>
   * </listOfReactions>
   */
  fail_unless( Model_getNumReactions(m) == 3, NULL );

  r = Model_getReaction(m, 0);
  fail_unless( !strcmp(Reaction_getName(r), "reaction_1"), NULL );
  fail_unless( Reaction_getReversible(r) == 0, NULL );
  fail_unless( Reaction_getFast      (r) == 0, NULL );

  r = Model_getReaction(m, 1);
  fail_unless( !strcmp(Reaction_getName(r), "reaction_2"), NULL );
  fail_unless( Reaction_getReversible(r) == 0, NULL );
  fail_unless( Reaction_getFast      (r) == 0, NULL );

  r = Model_getReaction(m, 2);
  fail_unless( !strcmp(Reaction_getName(r), "reaction_3"), NULL );
  fail_unless( Reaction_getReversible(r) == 0, NULL );
  fail_unless( Reaction_getFast      (r) == 0, NULL );

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
  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "X0"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "S1"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );

  kl = Reaction_getKineticLaw(r);
  fail_unless( !strcmp(KineticLaw_getFormula(kl), "k1 * X0"), NULL );
  fail_unless( KineticLaw_getNumParameters(kl) == 1, NULL );

  p = KineticLaw_getParameter(kl, 0);
  fail_unless( !strcmp(Parameter_getName(p), "k1"), NULL );
  fail_unless( Parameter_getValue(p) == 0, NULL );


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
  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "S1"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "X1"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );

  kl = Reaction_getKineticLaw(r);
  fail_unless( !strcmp(KineticLaw_getFormula(kl), "k2 * S1"), NULL );
  fail_unless( KineticLaw_getNumParameters(kl) == 1, NULL );

  p = KineticLaw_getParameter(kl, 0);
  fail_unless( !strcmp(Parameter_getName(p), "k2"), NULL );
  fail_unless( Parameter_getValue(p) == 0, NULL );


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
  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "S1"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "X2"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );

  kl = Reaction_getKineticLaw(r);
  fail_unless( !strcmp(KineticLaw_getFormula(kl), "k3 * S1"), NULL );
  fail_unless( KineticLaw_getNumParameters(kl) == 1, NULL );

  p = KineticLaw_getParameter(kl, 0);
  fail_unless( !strcmp(Parameter_getName(p), "k3"), NULL );
  fail_unless( Parameter_getValue(p) == 0, NULL );

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
