/**
 * Filename    : TestReaction.c
 * Description : SBML Reaction unit tests
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


#include "sbml/common.h"
#include "sbml/Reaction.h"


Reaction_t *R;


void
ReactionTest_setup (void)
{
  R = Reaction_create();

  if (R == NULL)
  {
    fail("Reaction_create() returned a NULL pointer.");
  }
}


void
ReactionTest_teardown (void)
{
  Reaction_free(R);
}


START_TEST (test_Reaction_create)
{
  fail_unless( R->typecode   == SBML_REACTION, NULL );
  fail_unless( R->metaid     == NULL, NULL );
  fail_unless( R->notes      == NULL, NULL );
  fail_unless( R->annotation == NULL, NULL );

  fail_unless( R->id         == NULL, NULL );
  fail_unless( R->name       == NULL, NULL );
  fail_unless( R->kineticLaw == NULL, NULL );
  fail_unless( R->reversible != 0   , NULL );
  fail_unless( R->fast       == 0   , NULL );

  fail_unless( !Reaction_isSetId        (R), NULL );
  fail_unless( !Reaction_isSetName      (R), NULL );
  fail_unless( !Reaction_isSetKineticLaw(R), NULL );

  fail_unless( Reaction_getNumReactants(R) == 0, NULL );
  fail_unless( Reaction_getNumProducts (R) == 0, NULL );
  fail_unless( Reaction_getNumModifiers(R) == 0, NULL );
}
END_TEST


START_TEST (test_Reaction_createWith)
{
  KineticLaw_t *kl = KineticLaw_create();
  Reaction_t   *r  = Reaction_createWith("r1", kl, 0, 1);


  fail_unless( r->typecode   == SBML_REACTION, NULL );
  fail_unless( r->metaid     == NULL, NULL );
  fail_unless( r->notes      == NULL, NULL );
  fail_unless( r->annotation == NULL, NULL );
  fail_unless( r->name       == NULL, NULL );

  fail_unless( !strcmp(r->id, "r1"), NULL );

  fail_unless( r->kineticLaw == kl, NULL );
  fail_unless( r->reversible ==  0, NULL );
  fail_unless( r->fast       ==  1, NULL );

  fail_unless( Reaction_isSetId        (r), NULL );
  fail_unless( !Reaction_isSetName     (r), NULL );
  fail_unless( Reaction_isSetKineticLaw(r), NULL );

  fail_unless( Reaction_getNumReactants(r) == 0, NULL );
  fail_unless( Reaction_getNumProducts (r) == 0, NULL );
  fail_unless( Reaction_getNumModifiers(r) == 0, NULL );

  Reaction_free(r);
}
END_TEST


START_TEST (test_Reaction_free_NULL)
{
  Reaction_free(NULL);
}
END_TEST


START_TEST (test_Reaction_setId)
{
  char *id = "J1";


  Reaction_setId(R, id);

  fail_unless( !strcmp(R->id, id) , NULL );
  fail_unless( Reaction_isSetId(R), NULL );

  if (R->id == id)
  {
    fail("Reaction_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Reaction_setId(R, R->id);
  fail_unless( !strcmp(R->id, id), NULL );

  Reaction_setId(R, NULL);
  fail_unless( !Reaction_isSetId(R), NULL );

  if (R->id != NULL)
  {
    fail("Reaction_setId(R, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Reaction_setName)
{
  char *name = "MapK Cascade";


  Reaction_setName(R, name);

  fail_unless( !strcmp(R->name, name), NULL );
  fail_unless( Reaction_isSetName(R) , NULL );

  if (R->name == name)
  {
    fail("Reaction_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Reaction_setName(R, R->name);
  fail_unless( !strcmp(R->name, name), NULL );

  Reaction_setName(R, NULL);
  fail_unless( !Reaction_isSetName(R), NULL );

  if (R->name != NULL)
  {
    fail("Reaction_setName(R, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Reaction_addReactant)
{
  Reaction_addReactant(R, SpeciesReference_create());

  fail_unless( Reaction_getNumReactants(R) == 1, NULL );
  fail_unless( Reaction_getNumProducts (R) == 0, NULL );
  fail_unless( Reaction_getNumModifiers(R) == 0, NULL );
}
END_TEST


START_TEST (test_Reaction_addProduct)
{
  Reaction_addProduct(R, SpeciesReference_create());

  fail_unless( Reaction_getNumReactants(R) == 0, NULL );
  fail_unless( Reaction_getNumProducts (R) == 1, NULL );
  fail_unless( Reaction_getNumModifiers(R) == 0, NULL );
}
END_TEST


START_TEST (test_Reaction_addModifier)
{
  Reaction_addModifier(R, ModifierSpeciesReference_create());

  fail_unless( Reaction_getNumReactants(R) == 0, NULL );
  fail_unless( Reaction_getNumProducts (R) == 0, NULL );
  fail_unless( Reaction_getNumModifiers(R) == 1, NULL );
}
END_TEST


START_TEST (test_Reaction_getReactant)
{
  SpeciesReference_t *sr1 = SpeciesReference_create();
  SpeciesReference_t *sr2 = SpeciesReference_create();


  SpeciesReference_setSpecies(sr1, "R1");
  SpeciesReference_setSpecies(sr2, "R2");

  Reaction_addReactant(R, sr1);
  Reaction_addReactant(R, sr2);

  fail_unless( Reaction_getNumReactants(R) == 2, NULL );
  fail_unless( Reaction_getNumProducts (R) == 0, NULL );
  fail_unless( Reaction_getNumModifiers(R) == 0, NULL );

  sr1 = Reaction_getReactant(R, 0);
  sr2 = Reaction_getReactant(R, 1);

  fail_unless( !strcmp(sr1->species, "R1"), NULL );
  fail_unless( !strcmp(sr2->species, "R2"), NULL );

}
END_TEST


START_TEST (test_Reaction_getProduct)
{
  SpeciesReference_t *sr1 = SpeciesReference_create();
  SpeciesReference_t *sr2 = SpeciesReference_create();


  SpeciesReference_setSpecies(sr1, "P1");
  SpeciesReference_setSpecies(sr2, "P2");

  Reaction_addProduct(R, sr1);
  Reaction_addProduct(R, sr2);

  fail_unless( Reaction_getNumReactants(R) == 0, NULL );
  fail_unless( Reaction_getNumProducts (R) == 2, NULL );
  fail_unless( Reaction_getNumModifiers(R) == 0, NULL );

  sr1 = Reaction_getProduct(R, 0);
  sr2 = Reaction_getProduct(R, 1);

  fail_unless( !strcmp(sr1->species, "P1"), NULL );
  fail_unless( !strcmp(sr2->species, "P2"), NULL );
}
END_TEST


START_TEST (test_Reaction_getModifier)
{
  ModifierSpeciesReference_t *msr1 = ModifierSpeciesReference_create();
  ModifierSpeciesReference_t *msr2 = ModifierSpeciesReference_create();


  ModifierSpeciesReference_setSpecies(msr1, "M1");
  ModifierSpeciesReference_setSpecies(msr2, "M2");

  Reaction_addModifier(R, msr1);
  Reaction_addModifier(R, msr2);

  fail_unless( Reaction_getNumReactants(R) == 0, NULL );
  fail_unless( Reaction_getNumProducts (R) == 0, NULL );
  fail_unless( Reaction_getNumModifiers(R) == 2, NULL );

  msr1 = Reaction_getModifier(R, 0);
  msr2 = Reaction_getModifier(R, 1);

  fail_unless( !strcmp(msr1->species, "M1"), NULL );
  fail_unless( !strcmp(msr2->species, "M2"), NULL );
}
END_TEST


Suite *
create_suite_Reaction (void)
{
  Suite *suite = suite_create("Reaction");
  TCase *tcase = tcase_create("Reaction");


  tcase_add_checked_fixture(tcase, ReactionTest_setup, ReactionTest_teardown);

  tcase_add_test( tcase, test_Reaction_create      );
  tcase_add_test( tcase, test_Reaction_createWith  );
  tcase_add_test( tcase, test_Reaction_free_NULL   );
  tcase_add_test( tcase, test_Reaction_setId       );
  tcase_add_test( tcase, test_Reaction_setName     );
  tcase_add_test( tcase, test_Reaction_addReactant );
  tcase_add_test( tcase, test_Reaction_addProduct  );
  tcase_add_test( tcase, test_Reaction_addModifier );
  tcase_add_test( tcase, test_Reaction_getReactant );
  tcase_add_test( tcase, test_Reaction_getProduct  );
  tcase_add_test( tcase, test_Reaction_getModifier );

  suite_add_tcase(suite, tcase);

  return suite;
}
