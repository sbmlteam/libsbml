/**
 * Filename    : TestSBMLConvert.c
 * Description : SBMLConvert unit tests
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-07-27
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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

#include "sbml/common.h"
#include "sbml/SBMLConvert.h"
#include "sbml/SBMLTypes.h"


START_TEST (test_SBMLConvert_convertToL2_SBMLDocument)
{
  SBMLDocument_t *d = SBMLDocument_createWith(1, 2);


  SBML_convertToL2( (SBase_t *) d);

  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Model)
{
  const char *id = "Model";
  Model_t    *m  = Model_create();


  fail_unless( !Model_isSetId  (m), NULL );
  fail_unless( !Model_isSetName(m), NULL );

  Model_setName(m, id);

  fail_unless( !Model_isSetId  (m), NULL );
  fail_unless(  Model_isSetName(m), NULL );

  SBML_convertNameToId( (SBase_t *) m);

  fail_unless(  Model_isSetId  (m), NULL );
  fail_unless( !Model_isSetName(m), NULL );

  fail_unless( !strcmp(Model_getId(m), id), NULL );

  Model_free(m);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_UnitDefinition)
{
  const char       *id = "bps";
  UnitDefinition_t *ud = UnitDefinition_create();


  fail_unless( !UnitDefinition_isSetId  (ud), NULL );
  fail_unless( !UnitDefinition_isSetName(ud), NULL );

  UnitDefinition_setName(ud, id);

  fail_unless( !UnitDefinition_isSetId  (ud), NULL );
  fail_unless(  UnitDefinition_isSetName(ud), NULL );

  SBML_convertNameToId( (SBase_t *) ud);

  fail_unless(  UnitDefinition_isSetId  (ud), NULL );
  fail_unless( !UnitDefinition_isSetName(ud), NULL );

  fail_unless( !strcmp(UnitDefinition_getId(ud), id), NULL );

  UnitDefinition_free(ud);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Compartment)
{
  const char    *id = "cell2";
  Compartment_t *c  = Compartment_create();


  fail_unless( !Compartment_isSetId  (c), NULL );
  fail_unless( !Compartment_isSetName(c), NULL );

  Compartment_setName(c, id);

  fail_unless( !Compartment_isSetId  (c), NULL );
  fail_unless(  Compartment_isSetName(c), NULL );

  SBML_convertNameToId( (SBase_t *) c);

  fail_unless(  Compartment_isSetId  (c), NULL );
  fail_unless( !Compartment_isSetName(c), NULL );

  fail_unless( !strcmp(Compartment_getId(c), id), NULL );

  Compartment_free(c);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Species)
{
  const char *id = "X1";
  Species_t  *s  = Species_create();


  fail_unless( !Species_isSetId  (s), NULL );
  fail_unless( !Species_isSetName(s), NULL );

  Species_setName(s, id);

  fail_unless( !Species_isSetId  (s), NULL );
  fail_unless(  Species_isSetName(s), NULL );

  SBML_convertNameToId( (SBase_t *) s);

  fail_unless(  Species_isSetId  (s), NULL );
  fail_unless( !Species_isSetName(s), NULL );

  fail_unless( !strcmp(Species_getId(s), id), NULL );

  Species_free(s);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Parameter)
{
  const char  *id = "x";
  Parameter_t *p  = Parameter_create();


  fail_unless( !Parameter_isSetId  (p), NULL );
  fail_unless( !Parameter_isSetName(p), NULL );

  Parameter_setName(p, id);

  fail_unless( !Parameter_isSetId  (p), NULL );
  fail_unless(  Parameter_isSetName(p), NULL );

  SBML_convertNameToId( (SBase_t *) p);

  fail_unless(  Parameter_isSetId  (p), NULL );
  fail_unless( !Parameter_isSetName(p), NULL );

  fail_unless( !strcmp(Parameter_getId(p), id), NULL );

  Parameter_free(p);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Reaction)
{
  const char *id = "r1";
  Reaction_t *r  = Reaction_create();


  fail_unless( !Reaction_isSetId  (r), NULL );
  fail_unless( !Reaction_isSetName(r), NULL );

  Reaction_setName(r, id);

  fail_unless( !Reaction_isSetId  (r), NULL );
  fail_unless(  Reaction_isSetName(r), NULL );

  SBML_convertNameToId( (SBase_t *) r);

  fail_unless(  Reaction_isSetId  (r), NULL );
  fail_unless( !Reaction_isSetName(r), NULL );

  fail_unless( !strcmp(Reaction_getId(r), id), NULL );

  Reaction_free(r);
}
END_TEST


START_TEST (test_SBMLConvert_addModifiersToReaction)
{
  Model_t      *m  = Model_create();
  KineticLaw_t *kl = KineticLaw_createWith("k1*S1*S2*S3*S4*S5", NULL, NULL);
  Reaction_t   *r  = Reaction_createWith("R", kl, 1, 0);

  SimpleSpeciesReference_t *ssr1;
  SimpleSpeciesReference_t *ssr2;


  Model_addSpecies( m, Species_createWith("S1", "cell", 2.0, "amount", 0, 0) );
  Model_addSpecies( m, Species_createWith("S2", "cell", 2.0, "amount", 0, 0) );
  Model_addSpecies( m, Species_createWith("S3", "cell", 1.0, "amount", 0, 0) );
  Model_addSpecies( m, Species_createWith("S4", "cell", 1.0, "amount", 0, 0) );
  Model_addSpecies( m, Species_createWith("S5", "cell", 0.0, "amount", 0, 0) );

  Reaction_addReactant( r, SpeciesReference_createWith("S1", 1, 1) );
  Reaction_addReactant( r, SpeciesReference_createWith("S2", 1, 1) );
  Reaction_addProduct ( r, SpeciesReference_createWith("S5", 1, 1) );

  Model_addReaction(m, r);

  fail_unless( Reaction_getNumModifiers(r) == 0, NULL );

  SBML_addModifiersToReaction(r, m);

  fail_unless( Reaction_getNumModifiers(r) == 2, NULL );

  ssr1 = (SimpleSpeciesReference_t *) Reaction_getModifier(r, 0);
  ssr2 = (SimpleSpeciesReference_t *) Reaction_getModifier(r, 1);

  fail_unless( !strcmp(SimpleSpeciesReference_getSpecies(ssr1), "S3"), NULL );
  fail_unless( !strcmp(SimpleSpeciesReference_getSpecies(ssr2), "S4"), NULL );

  Model_free(m);
}
END_TEST


Suite *
create_suite_SBMLConvert (void) 
{ 
  Suite *suite = suite_create("SBMLConvert");
  TCase *tcase = tcase_create("SBMLConvert");


  tcase_add_test( tcase, test_SBMLConvert_convertToL2_SBMLDocument       );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Model          );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_UnitDefinition );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Compartment    );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Species        );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Parameter      );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Reaction       );
  tcase_add_test( tcase, test_SBMLConvert_addModifiersToReaction         );


  suite_add_tcase(suite, tcase);

  return suite;
}
