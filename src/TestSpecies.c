/**
 * Filename    : TestSpecies.c
 * Description : Species unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-18
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
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
#include "sbml/Species.h"


static Species_t *S;


void
SpeciesTest_setup (void)
{
  S = Species_create();

  if (S == NULL)
  {
    fail("Species_create() returned a NULL pointer.");
  }
}


void
SpeciesTest_teardown (void)
{
  Species_free(S);
}


START_TEST (test_Species_create)
{
  fail_unless( S->typecode   == SBML_SPECIES, NULL );
  fail_unless( S->metaid     == NULL, NULL );
  fail_unless( S->notes      == NULL, NULL );
  fail_unless( S->annotation == NULL, NULL );

  fail_unless( S->id                    == NULL, NULL );
  fail_unless( S->name                  == NULL, NULL );
  fail_unless( S->compartment           == NULL, NULL );
  fail_unless( S->initial.Amount        == 0.0 , NULL );
  fail_unless( S->initial.Concentration == 0.0 , NULL );
  fail_unless( S->substanceUnits        == NULL, NULL );
  fail_unless( S->spatialSizeUnits      == NULL, NULL );
  fail_unless( S->hasOnlySubstanceUnits == 0   , NULL );
  fail_unless( S->boundaryCondition     == 0   , NULL );
  fail_unless( S->charge                == 0   , NULL );
  fail_unless( S->constant              == 0   , NULL );

  fail_unless( !Species_isSetId                  (S), NULL );
  fail_unless( !Species_isSetName                (S), NULL );
  fail_unless( !Species_isSetCompartment         (S), NULL );
  fail_unless( !Species_isSetInitialAmount       (S), NULL );
  fail_unless( !Species_isSetInitialConcentration(S), NULL );
  fail_unless( !Species_isSetSubstanceUnits      (S), NULL );
  fail_unless( !Species_isSetSpatialSizeUnits    (S), NULL );
  fail_unless( !Species_isSetUnits               (S), NULL );
  fail_unless( !Species_isSetCharge              (S), NULL );
}
END_TEST


START_TEST (test_Species_createWith)
{
  Species_t *s = Species_createWith("Ca", "cell", 5.7, "mole", 1, 1);


  fail_unless( s->typecode   == SBML_SPECIES, NULL );
  fail_unless( s->metaid     == NULL, NULL );
  fail_unless( s->notes      == NULL, NULL );
  fail_unless( s->annotation == NULL, NULL );

  fail_unless( s->name                  == NULL, NULL );
  fail_unless( s->spatialSizeUnits      == NULL, NULL );
  fail_unless( s->hasOnlySubstanceUnits == 0, NULL );
  fail_unless( s->constant              == 0, NULL );

  fail_unless( !strcmp(s->id            , "Ca"  ), NULL );
  fail_unless( !strcmp(s->compartment   , "cell"), NULL );
  fail_unless( !strcmp(s->substanceUnits, "mole"), NULL );

  fail_unless( s->initial.Amount    == 5.7 , NULL );
  fail_unless( s->boundaryCondition == 1   , NULL );
  fail_unless( s->charge            == 1   , NULL );

  fail_unless(   Species_isSetId                   (s), NULL );
  fail_unless( ! Species_isSetName                 (s), NULL );
  fail_unless(   Species_isSetCompartment          (s), NULL );
  fail_unless(   Species_isSetSubstanceUnits       (s), NULL );
  fail_unless( ! Species_isSetSpatialSizeUnits     (s), NULL );
  fail_unless(   Species_isSetUnits                (s), NULL );
  fail_unless(   Species_isSetInitialAmount        (s), NULL );
  fail_unless( ! Species_isSetInitialConcentration (s), NULL );
  fail_unless(   Species_isSetCharge               (s), NULL );

  Species_free(s);
}
END_TEST


START_TEST (test_Species_free_NULL)
{
  Species_free(NULL);
}
END_TEST


START_TEST (test_Species_setId)
{
  char *id = "Glucose";


  Species_setId(S, id);

  fail_unless( !strcmp(S->id, id), NULL );
  fail_unless( Species_isSetId(S)  , NULL );

  if (S->id == id)
  {
    fail("Species_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setId(S, S->id);
  fail_unless( !strcmp(S->id, id), NULL );

  Species_setId(S, NULL);
  fail_unless( !Species_isSetId(S), NULL );

  if (S->id != NULL)
  {
    fail("Species_setId(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setName)
{
  char *name = "So Sweet";


  Species_setName(S, name);

  fail_unless( !strcmp(S->name, name), NULL );
  fail_unless( Species_isSetName(S)  , NULL );

  if (S->name == name)
  {
    fail("Species_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setName(S, S->name);
  fail_unless( !strcmp(S->name, name), NULL );

  Species_setName(S, NULL);
  fail_unless( !Species_isSetName(S), NULL );

  if (S->name != NULL)
  {
    fail("Species_setName(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setCompartment)
{
  char *compartment = "cell";


  Species_setCompartment(S, compartment);

  fail_unless( !strcmp(S->compartment, compartment), NULL );
  fail_unless( Species_isSetCompartment(S), NULL );

  if (S->compartment == compartment)
  {
    fail("Species_setCompartment(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setCompartment(S, S->compartment);
  fail_unless( !strcmp(S->compartment, compartment), NULL );

  Species_setCompartment(S, NULL);
  fail_unless( !Species_isSetCompartment(S), NULL );

  if (S->compartment != NULL)
  {
    fail("Species_setComartment(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setSubstanceUnits)
{
  char *units = "item";


  Species_setSubstanceUnits(S, units);

  fail_unless( !strcmp(S->substanceUnits, units), NULL );
  fail_unless( Species_isSetSubstanceUnits(S), NULL );

  if (S->substanceUnits == units)
  {
    fail("Species_setSubstanceUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setSubstanceUnits(S, S->substanceUnits);
  fail_unless( !strcmp(S->substanceUnits, units), NULL );

  Species_setSubstanceUnits(S, NULL);
  fail_unless( !Species_isSetSubstanceUnits(S), NULL );

  if (S->substanceUnits != NULL)
  {
    fail("Species_setSubstanceUnits(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setSpatialSizeUnits)
{
  char *units = "volume";


  Species_setSpatialSizeUnits(S, units);

  fail_unless( !strcmp(S->spatialSizeUnits, units), NULL );
  fail_unless( Species_isSetSpatialSizeUnits(S), NULL );

  if (S->spatialSizeUnits == units)
  {
    fail("Species_setSpatialSizeUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setSpatialSizeUnits(S, S->spatialSizeUnits);
  fail_unless( !strcmp(S->spatialSizeUnits, units), NULL );

  Species_setSpatialSizeUnits(S, NULL);
  fail_unless( !Species_isSetSpatialSizeUnits(S), NULL );

  if (S->spatialSizeUnits != NULL)
  {
    fail("Species_setSpatialSizeUnits(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setUnits)
{
  char *units = "mole";


  Species_setUnits(S, units);

  fail_unless( !strcmp(Species_getUnits(S), units), NULL );
  fail_unless( Species_isSetUnits(S), NULL );

  if (S->substanceUnits == units)
  {
    fail("Species_setUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setUnits(S, S->substanceUnits);
  fail_unless( !strcmp(Species_getUnits(S), units), NULL );

  Species_setUnits(S, NULL);
  fail_unless( !Species_isSetUnits(S), NULL );

  if (S->substanceUnits != NULL)
  {
    fail("Species_setUnits(S, NULL) did not clear string.");
  }
}
END_TEST


Suite *
create_suite_Species (void)
{
  Suite *suite = suite_create("Species");
  TCase *tcase = tcase_create("Species");


  tcase_add_checked_fixture( tcase,
                             SpeciesTest_setup,
                             SpeciesTest_teardown );

  tcase_add_test( tcase, test_Species_create              );
  tcase_add_test( tcase, test_Species_createWith          );
  tcase_add_test( tcase, test_Species_free_NULL           );
  tcase_add_test( tcase, test_Species_setId               );
  tcase_add_test( tcase, test_Species_setName             );
  tcase_add_test( tcase, test_Species_setCompartment      );
  tcase_add_test( tcase, test_Species_setSubstanceUnits   );
  tcase_add_test( tcase, test_Species_setSpatialSizeUnits );
  tcase_add_test( tcase, test_Species_setUnits            );

  suite_add_tcase(suite, tcase);

  return suite;
}
