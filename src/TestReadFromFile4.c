/**
 * \file    TestReadFromFile4.c
 * \brief   Reads tests/l1v1-minimal.xml into memory and tests it.
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

#include "common.h"
#include "SBMLReader.h"
#include "SBMLTypes.h"


extern char *TestDataDirectory;


START_TEST (test_read_l1v1_minimal)
{
  SBMLDocument_t     *d;
  Model_t            *m;
  Compartment_t      *c;
  Reaction_t         *r;
  Species_t          *s;
  SpeciesReference_t *sr;

  char *filename = safe_strcat(TestDataDirectory, "l1v1-minimal.xml");


  d = readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"l1v1-minimal.xml\") returned a NULL pointer.");
  }

  safe_free(filename);


  /**
   * <sbml level="1" version="1" ...>
   */
  fail_unless( SBMLDocument_getLevel  (d) == 1, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );


  /**
   * <model>
   */
  m = SBMLDocument_getModel(d);

  /**
   * <listOfCompartments>
   *  <compartment name="x"/>
   * </listOfCompartments>
   */
  fail_unless( Model_getNumCompartments(m) == 1, NULL );

  c = Model_getCompartment(m, 0);
  fail_unless( !strcmp(Compartment_getName(c), "x"), NULL );


  /**
   * <listOfSpecies>
   *   <specie name="y" compartment="x" initialAmount="1"/>
   * </listOfSpecies>
   */
  fail_unless( Model_getNumSpecies(m) == 1, NULL );

  s = Model_getSpecies(m, 0);
  fail_unless( !strcmp( Species_getName(s)       , "y" )  , NULL );
  fail_unless( !strcmp( Species_getCompartment(s), "x" )  , NULL );
  fail_unless( Species_getInitialAmount    (s) == 1, NULL );
  fail_unless( Species_getBoundaryCondition(s) == 0, NULL );


  /**
   * <listOfReactions>
   *   <reaction name="x">
   *     <listOfReactants>
   *       <specieReference specie="y"/>
   *     </listOfReactants>
   *     <listOfProducts>
   *       <specieReference specie="y"/>
   *     </listOfProducts>
   *   </reaction>
   * </listOfReactions>
   */
  fail_unless( Model_getNumReactions(m) == 1, NULL );

  r = Model_getReaction(m, 0);
  fail_unless( !strcmp(Reaction_getName(r), "x"), NULL );
  fail_unless( Reaction_getReversible(r) != 0, NULL );
  fail_unless( Reaction_getFast(r)       == 0, NULL );

  fail_unless( Reaction_getNumReactants(r) == 1, NULL );
  fail_unless( Reaction_getNumProducts(r)  == 1, NULL );

  sr = Reaction_getReactant(r, 0);
  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "y"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );

  sr = Reaction_getProduct(r, 0);
  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "y"), NULL );
  fail_unless( SpeciesReference_getStoichiometry(sr) == 1, NULL );
  fail_unless( SpeciesReference_getDenominator  (sr) == 1, NULL );

  SBMLDocument_free(d);
}
END_TEST


Suite *
create_suite_TestReadFromFile4 (void)
{ 
  Suite *suite = suite_create("test-data/l1v1-minimal.xml");
  TCase *tcase = tcase_create("test-data/l1v1-minimal.xml");


  tcase_add_test(tcase, test_read_l1v1_minimal);

  suite_add_tcase(suite, tcase);

  return suite;
}
