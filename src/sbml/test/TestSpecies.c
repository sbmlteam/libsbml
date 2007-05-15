/**
 * \file    TestSpecies.c
 * \brief   Species unit tests
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


#include "common/common.h"

#include "SBase.h"
#include "Species.h"

#include <check.h>


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
  fail_unless( SBase_getTypeCode  ((SBase_t *) S) == SBML_SPECIES );
  fail_unless( SBase_getMetaId    ((SBase_t *) S) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) S) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) S) == NULL );

  fail_unless( Species_getId                   (S) == NULL );
  fail_unless( Species_getName                 (S) == NULL );
  fail_unless( Species_getCompartment          (S) == NULL );
  fail_unless( Species_getInitialAmount        (S) == 0.0  );
  fail_unless( Species_getInitialConcentration (S) == 0.0  );
  fail_unless( Species_getSubstanceUnits       (S) == NULL );
  fail_unless( Species_getSpatialSizeUnits     (S) == NULL );
  fail_unless( Species_getHasOnlySubstanceUnits(S) == 0    );
  fail_unless( Species_getBoundaryCondition    (S) == 0    );
  fail_unless( Species_getCharge               (S) == 0    );
  fail_unless( Species_getConstant             (S) == 0    );

  fail_unless( !Species_isSetId                  (S) );
  fail_unless( !Species_isSetName                (S) );
  fail_unless( !Species_isSetCompartment         (S) );
  fail_unless( !Species_isSetInitialAmount       (S) );
  fail_unless( !Species_isSetInitialConcentration(S) );
  fail_unless( !Species_isSetSubstanceUnits      (S) );
  fail_unless( !Species_isSetSpatialSizeUnits    (S) );
  fail_unless( !Species_isSetUnits               (S) );
  fail_unless( !Species_isSetCharge              (S) );
}
END_TEST


START_TEST (test_Species_createWith)
{
  Species_t *s = Species_createWith("Ca", "Calcium");


  fail_unless( SBase_getTypeCode  ((SBase_t *) s) == SBML_SPECIES );
  fail_unless( SBase_getMetaId    ((SBase_t *) s) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) s) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) s) == NULL );

  fail_unless( !strcmp(Species_getName            (s), "Calcium"  ) );
  fail_unless( Species_getSpatialSizeUnits     (s) == NULL );
  fail_unless( Species_getHasOnlySubstanceUnits(s) == 0 );
  fail_unless( Species_getConstant             (s) == 0 );

  fail_unless( !strcmp(Species_getId            (s), "Ca"  ) );
  //fail_unless( !strcmp(Species_getCompartment   (s), "cell") );
  //fail_unless( !strcmp(Species_getSubstanceUnits(s), "mole") );

  //fail_unless( Species_getInitialAmount    (s) == 5.7  );
  //fail_unless( Species_getBoundaryCondition(s) == 1    );
  //fail_unless( Species_getCharge           (s) == 1    );

  fail_unless(   Species_isSetId                   (s) );
  fail_unless(   Species_isSetName                 (s) );
  fail_unless( ! Species_isSetCompartment          (s) );
  fail_unless( ! Species_isSetSubstanceUnits       (s) );
  fail_unless( ! Species_isSetSpatialSizeUnits     (s) );
  fail_unless( ! Species_isSetUnits                (s) );
  fail_unless( ! Species_isSetInitialAmount        (s) );
  fail_unless( ! Species_isSetInitialConcentration (s) );
  fail_unless( ! Species_isSetCharge               (s) );

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

  fail_unless( !strcmp(Species_getId(S), id) );
  fail_unless( Species_isSetId(S) );

  if (Species_getId(S) == id)
  {
    fail("Species_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setId(S, Species_getId(S));
  fail_unless( !strcmp(Species_getId(S), id) );

  Species_setId(S, NULL);
  fail_unless( !Species_isSetId(S) );

  if (Species_getId(S) != NULL)
  {
    fail("Species_setId(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setName)
{
  char *name = "So Sweet";


  Species_setName(S, name);

  fail_unless( !strcmp(Species_getName(S), name) );
  fail_unless( Species_isSetName(S) );

  if (Species_getName(S) == name)
  {
    fail("Species_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setName(S, Species_getName(S));
  fail_unless( !strcmp(Species_getName(S), name) );

  Species_setName(S, NULL);
  fail_unless( !Species_isSetName(S) );

  if (Species_getName(S) != NULL)
  {
    fail("Species_setName(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setCompartment)
{
  char *compartment = "cell";


  Species_setCompartment(S, compartment);

  fail_unless( !strcmp(Species_getCompartment(S), compartment) );
  fail_unless( Species_isSetCompartment(S) );

  if (Species_getCompartment(S) == compartment)
  {
    fail("Species_setCompartment(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setCompartment(S, Species_getCompartment(S));
  fail_unless( !strcmp(Species_getCompartment(S), compartment) );

  Species_setCompartment(S, NULL);
  fail_unless( !Species_isSetCompartment(S) );

  if (Species_getCompartment(S) != NULL)
  {
    fail("Species_setComartment(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setInitialAmount)
{
  fail_unless( !Species_isSetInitialAmount       (S) );
  fail_unless( !Species_isSetInitialConcentration(S) );

  Species_setInitialAmount(S, 1.2);

  fail_unless(  Species_isSetInitialAmount       (S) );
  fail_unless( !Species_isSetInitialConcentration(S) );

  fail_unless( Species_getInitialAmount(S) == 1.2 );
}
END_TEST


START_TEST (test_Species_setInitialConcentration)
{
  fail_unless( !Species_isSetInitialAmount       (S) );
  fail_unless( !Species_isSetInitialConcentration(S) );

  Species_setInitialConcentration(S, 3.4);

  fail_unless( !Species_isSetInitialAmount       (S) );
  fail_unless(  Species_isSetInitialConcentration(S) );

  fail_unless( Species_getInitialConcentration(S) == 3.4 );
}
END_TEST


START_TEST (test_Species_setSubstanceUnits)
{
  char *units = "item";


  Species_setSubstanceUnits(S, units);

  fail_unless( !strcmp(Species_getSubstanceUnits(S), units) );
  fail_unless( Species_isSetSubstanceUnits(S) );

  if (Species_getSubstanceUnits(S) == units)
  {
    fail("Species_setSubstanceUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setSubstanceUnits(S, Species_getSubstanceUnits(S));
  fail_unless( !strcmp(Species_getSubstanceUnits(S), units) );

  Species_setSubstanceUnits(S, NULL);
  fail_unless( !Species_isSetSubstanceUnits(S) );

  if (Species_getSubstanceUnits(S) != NULL)
  {
    fail("Species_setSubstanceUnits(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setSpatialSizeUnits)
{
  char *units = "volume";


  Species_setSpatialSizeUnits(S, units);

  fail_unless( !strcmp(Species_getSpatialSizeUnits(S), units) );
  fail_unless( Species_isSetSpatialSizeUnits(S) );

  if (Species_getSpatialSizeUnits(S) == units)
  {
    fail("Species_setSpatialSizeUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setSpatialSizeUnits(S, Species_getSpatialSizeUnits(S));
  fail_unless( !strcmp(Species_getSpatialSizeUnits(S), units) );

  Species_setSpatialSizeUnits(S, NULL);
  fail_unless( !Species_isSetSpatialSizeUnits(S) );

  if (Species_getSpatialSizeUnits(S) != NULL)
  {
    fail("Species_setSpatialSizeUnits(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Species_setUnits)
{
  char *units = "mole";


  Species_setUnits(S, units);

  fail_unless( !strcmp(Species_getUnits(S), units) );
  fail_unless( Species_isSetUnits(S) );

  if (Species_getSubstanceUnits(S) == units)
  {
    fail("Species_setUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Species_setUnits(S, Species_getSubstanceUnits(S));
  fail_unless( !strcmp(Species_getUnits(S), units) );

  Species_setUnits(S, NULL);
  fail_unless( !Species_isSetUnits(S) );

  if (Species_getSubstanceUnits(S) != NULL)
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

  tcase_add_test( tcase, test_Species_create                  );
  tcase_add_test( tcase, test_Species_createWith              );
  tcase_add_test( tcase, test_Species_free_NULL               );
  tcase_add_test( tcase, test_Species_setId                   );
  tcase_add_test( tcase, test_Species_setName                 );
  tcase_add_test( tcase, test_Species_setCompartment          );
  tcase_add_test( tcase, test_Species_setInitialAmount        );
  tcase_add_test( tcase, test_Species_setInitialConcentration );
  tcase_add_test( tcase, test_Species_setSubstanceUnits       );
  tcase_add_test( tcase, test_Species_setSpatialSizeUnits     );
  tcase_add_test( tcase, test_Species_setUnits                );

  suite_add_tcase(suite, tcase);

  return suite;
}
