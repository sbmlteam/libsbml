/**
 * \file    TestSBMLDocument.c
 * \brief   SBMLDocument unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
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


#include <sbml/common/common.h>

#include <sbml/SBase.h>
#include <sbml/Model.h>
#include <sbml/Unit.h>
#include <sbml/UnitKind.h>
#include <sbml/UnitDefinition.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Compartment.h>
#include <sbml/Parameter.h>
#include <sbml/Rule.h>

#include <check.h>


START_TEST (test_SBMLDocument_create)
{
  SBMLDocument_t *d = SBMLDocument_create();


  fail_unless( SBase_getTypeCode  ((SBase_t *) d) == SBML_DOCUMENT );
  fail_unless( SBase_getNotes     ((SBase_t *) d) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) d) == NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 3);
  fail_unless( SBMLDocument_getVersion(d) == 1);

  fail_unless( SBMLDocument_getNumErrors  (d) == 0 );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_createWith)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(1, 2);


  fail_unless( SBase_getTypeCode  ((SBase_t *) d) == SBML_DOCUMENT );
  fail_unless( SBase_getNotes     ((SBase_t *) d) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) d) == NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 1);
  fail_unless( SBMLDocument_getVersion(d) == 2);

  fail_unless( SBMLDocument_getNumErrors  (d) == 0 );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_free_NULL)
{
  SBMLDocument_free(NULL);
}
END_TEST


START_TEST (test_SBMLDocument_setModel)
{
  SBMLDocument_t *d  = SBMLDocument_create();
  Model_t        *m1 = Model_create(2, 4);
  Model_t        *m2 = Model_create(2, 4);


  fail_unless(SBMLDocument_getModel(d) == NULL);

  SBMLDocument_setModel(d, m1);
  fail_unless(SBMLDocument_getModel(d) != m1);

  /* Reflexive case (pathological) */
  SBMLDocument_setModel(d, SBMLDocument_getModel(d));
  fail_unless(SBMLDocument_getModel(d) != m1);

  SBMLDocument_setModel(d, m2);
  fail_unless(SBMLDocument_getModel(d) != m2);

  SBMLDocument_free(d);
  /* m1 is freed by SBMLDocument_setModel(d, m2); */
}
END_TEST


START_TEST (test_SBMLDocument_setLevelAndVersion)
{
  SBMLDocument_t *d  = SBMLDocument_create();
  SBMLDocument_setLevelAndVersion(d, 2, 2);
  
  Model_t        *m1 = Model_create(2, 2);

  SBMLDocument_setModel(d, m1);

  fail_unless(SBMLDocument_setLevelAndVersion(d,2,3) == 1);
  fail_unless(SBMLDocument_setLevelAndVersion(d,2,1) == 1);
  fail_unless(SBMLDocument_setLevelAndVersion(d,1,2) == 1);
  fail_unless(SBMLDocument_setLevelAndVersion(d,1,1) == 0);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_setLevelAndVersion_Warning)
{
  SBMLDocument_t *d  = SBMLDocument_create();
  SBMLDocument_setLevelAndVersion(d, 2, 2);
  
  Model_t        *m1 = Model_create(2, 2);
  SBase_setSBOTerm((SBase_t*)(m1), 2);

  SBMLDocument_setModel(d, m1);

  fail_unless(SBMLDocument_setLevelAndVersion(d,2,3) == 1);
  fail_unless(SBMLDocument_setLevelAndVersion(d,2,1) == 1);
  fail_unless(SBMLDocument_setLevelAndVersion(d,1,2) == 1);
  fail_unless(SBMLDocument_setLevelAndVersion(d,1,1) == 0);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_setLevelAndVersion_Error)
{
  SBMLDocument_t *d  = SBMLDocument_create();
  SBMLDocument_setLevelAndVersion(d, 2, 1);
  
  Model_t        *m1 = Model_create(2, 1);

  /* add unitDefinition */
  Unit_t * u = Unit_create(2, 1);
  Unit_setKind(u, UnitKind_forName("mole"));
  Unit_setOffset(u, 3.2);

  UnitDefinition_t *ud = 
    UnitDefinition_create(2, 1);
  UnitDefinition_setId(ud, "ud");
  UnitDefinition_addUnit(ud, u);

  Model_addUnitDefinition(m1, ud);
  SBMLDocument_setModel(d, m1);

  fail_unless(SBMLDocument_setLevelAndVersion(d,2,2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d,2,3) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d,1,2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d,1,1) == 0);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_setModel1)
{
  SBMLDocument_t *d  = SBMLDocument_create();
  SBMLDocument_setLevelAndVersion(d, 2, 2);
  
  Model_t        *m1 = Model_create(2, 1);

  int i = SBMLDocument_setModel(d, m1);

  fail_unless ( i == LIBSBML_VERSION_MISMATCH);
  fail_unless (SBMLDocument_getModel(d) == 0);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_setModel2)
{
  SBMLDocument_t *d  = SBMLDocument_create();
  SBMLDocument_setLevelAndVersion(d, 2, 2);
  
  Model_t        *m1 = Model_create(1, 2);

  int i = SBMLDocument_setModel(d, m1);

  fail_unless ( i == LIBSBML_LEVEL_MISMATCH);
  fail_unless (SBMLDocument_getModel(d) == 0);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_setModel3)
{
  SBMLDocument_t *d  = SBMLDocument_create();
  SBMLDocument_setLevelAndVersion(d, 2, 2);
  
  Model_t        *m1 = Model_create(2, 2);

  int i = SBMLDocument_setModel(d, m1);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless (SBMLDocument_getModel(d) != 0);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLDocument_setLevelAndVersion_UnitsError)
{
  SBMLDocument_t *d  = SBMLDocument_create();
  SBMLDocument_setLevelAndVersion(d, 2, 4);
  
  Model_t        *m1 = SBMLDocument_createModel(d);
  
  Compartment_t  *c = Model_createCompartment(m1);
  Compartment_setId(c, "c");
  
  Parameter_t *p = Model_createParameter(m1);
  Parameter_setId(p, "p");
  Parameter_setUnits(p, "mole");

  Rule_t * r = Model_createAssignmentRule(m1);
  Rule_setVariable(r, "c");
  Rule_setFormula(r, "p*p");

  fail_unless(SBMLDocument_setLevelAndVersion(d,2,2) == 1);
  fail_unless(SBMLDocument_setLevelAndVersion(d,2,3) == 1);
  fail_unless(SBMLDocument_setLevelAndVersion(d,1,2) == 1);
  fail_unless(SBMLDocument_setLevelAndVersion(d,1,1) == 0);

  SBMLDocument_free(d);
}
END_TEST


Suite *
create_suite_SBMLDocument (void) 
{ 
  Suite *suite = suite_create("SBMLDocument");
  TCase *tcase = tcase_create("SBMLDocument");
 

  tcase_add_test(tcase, test_SBMLDocument_create     );
  tcase_add_test(tcase, test_SBMLDocument_createWith );
  tcase_add_test(tcase, test_SBMLDocument_free_NULL  );
  tcase_add_test(tcase, test_SBMLDocument_setModel   );

  tcase_add_test(tcase, test_SBMLDocument_setLevelAndVersion         );
  tcase_add_test(tcase, test_SBMLDocument_setLevelAndVersion_Warning );
  tcase_add_test(tcase, test_SBMLDocument_setLevelAndVersion_Error   );

  tcase_add_test(tcase, test_SBMLDocument_setModel1   );
  tcase_add_test(tcase, test_SBMLDocument_setModel2   );
  tcase_add_test(tcase, test_SBMLDocument_setModel3   );

  tcase_add_test(tcase, test_SBMLDocument_setLevelAndVersion_UnitsError   );

  suite_add_tcase(suite, tcase);

  return suite;
}
