/**
 * \file    TestSBMLConvert.c
 * \brief   SBMLConvert unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLTypes.h>
#include <sbml/SpeciesReference.h>

#include <check.h>

#if __cplusplus
CK_CPPSTART
#endif

START_TEST (test_SBMLConvert_convertToL2_SBMLDocument)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(1, 2);


  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 2, 1) == 1, NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 2, 2) == 1, NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 2, NULL );

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 2, 3) == 1, NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 3, NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_addModifiersToReaction)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(1, 2);
  Model_t        *m = SBMLDocument_createModel(d);
  Reaction_t     *r = Model_createReaction(m);
  KineticLaw_t  *kl = Reaction_createKineticLaw(r);
  KineticLaw_setFormula(kl, "k1*S1*S2*S3*S4*S5");

  SpeciesReference_t *ssr1;
  SpeciesReference_t *ssr2;


  Species_t *s1 = Model_createSpecies( m ); 
  Species_setId( s1, "S1" );
  Species_t *s2 = Model_createSpecies( m ); 
  Species_setId( s2, "S2");
  Species_t *s3 = Model_createSpecies( m ); 
  Species_setId( s3, "S3");
  Species_t *s4 = Model_createSpecies( m ); 
  Species_setId( s4, "S4");
  Species_t *s5 = Model_createSpecies( m ); 
  Species_setId( s5, "S5");

  SpeciesReference_t *sr1 = Reaction_createReactant( r );
  SpeciesReference_t *sr2 = Reaction_createReactant( r );
  SpeciesReference_t *sr3 = Reaction_createProduct ( r );

  SpeciesReference_setSpecies(sr1, "S1");
  SpeciesReference_setSpecies(sr2, "S2");
  SpeciesReference_setSpecies(sr3, "S5");

  fail_unless( Reaction_getNumModifiers(r) == 0, NULL );

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 2, 1) == 1, NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );


  fail_unless( Reaction_getNumModifiers(Model_getReaction(m, 0)) == 2, NULL );

  ssr1 = (SpeciesReference_t *) Reaction_getModifier(Model_getReaction(m, 0), 0);
  ssr2 = (SpeciesReference_t *) Reaction_getModifier(Model_getReaction(m, 0), 1);

  fail_unless( !strcmp(SpeciesReference_getSpecies(ssr1), "S3"), NULL );
  fail_unless( !strcmp(SpeciesReference_getSpecies(ssr2), "S4"), NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL1_SBMLDocument)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 1);

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 1, 2) == 1, NULL);

  fail_unless( SBMLDocument_getLevel  (d) == 1, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 2, NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL1_Species_Amount)
{
  SBMLDocument_t *d   = SBMLDocument_createWithLevelAndVersion(2, 1);
  Model_t        *m   = SBMLDocument_createModel(d);
  const char     *sid = "C";
  Compartment_t  *c   = Compartment_create(2, 4);
  Species_t      *s   = Species_create(2, 4);


  Compartment_setId   ( c, sid );
  Model_addCompartment( m, c   );

  Species_setCompartment  ( s, sid  ); 
  Species_setInitialAmount( s, 2.34 );
  Model_addSpecies        ( m, s    );
  
  fail_unless( SBMLDocument_setLevelAndVersion(d, 1, 2) == 1, NULL );

  fail_unless( Species_getInitialAmount(s) == 2.34, NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL1_Species_Concentration)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 1);
  Model_t        *m = SBMLDocument_createModel(d);
  const char   *sid = "C";
  Compartment_t  *c = 
    Compartment_create(2, 1);
  Species_t      *s = 
    Species_create(2, 1);


  Compartment_setId   ( c, sid );
  Compartment_setSize ( c, 1.2 ); 
  Model_addCompartment( m, c   );

  Species_setId                  ( s, "s"  );
  Species_setCompartment         ( s, sid  ); 
  Species_setInitialConcentration( s, 2.34 );
  Model_addSpecies               ( m, s    );
  
  fail_unless( SBMLDocument_setLevelAndVersion(d, 1, 2) == 1, NULL);

  /**
   * These tests will fail under Cygwin because of a minimal
   * setlocale() implementation (see setlocale manpage).
   */
#ifndef CYGWIN
  fail_unless( Species_getInitialAmount(Model_getSpecies(m, 0)) == 2.808, NULL );
#endif

  Species_t * s1 = Model_getSpecies(m, 0);
  fail_unless (s1 != NULL);
  fail_unless (!strcmp(Species_getCompartment(s1), "C"));
  fail_unless(Compartment_getSize(Model_getCompartmentById(m, "C")) == 1.2);
  fail_unless(Species_getInitialConcentration(s1) == 2.34);
  fail_unless(Species_isSetInitialConcentration(s1) == 1);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL2v4_DuplicateAnnotations_doc)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 1);
  SBMLDocument_createModel(d);

  char * annotation = "<rdf/>\n<rdf/>";

  int i = SBase_setAnnotationString((SBase_t *) (d), annotation);
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );
  fail_unless( XMLNode_getNumChildren(SBase_getAnnotation((SBase_t *) (d))) == 2);

  fail_unless( SBMLDocument_setLevelAndVersion(d, 2, 4) == 1, NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 4, NULL );

  fail_unless( XMLNode_getNumChildren(SBase_getAnnotation((SBase_t *) (d))) == 1);


  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL2v4_DuplicateAnnotations_model)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 1);
  Model_t * m = SBMLDocument_createModel(d);

  char * annotation = "<rdf/>\n<rdf/>";

  int i = SBase_setAnnotationString((SBase_t *) (m), annotation);
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );
  fail_unless( XMLNode_getNumChildren(SBase_getAnnotation((SBase_t *) (m))) == 2);

  fail_unless( SBMLDocument_setLevelAndVersion(d, 2, 4) == 1, NULL );

  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 4, NULL );

  m = SBMLDocument_getModel(d);
  fail_unless( XMLNode_getNumChildren(SBase_getAnnotation((SBase_t *) (m))) == 1);


  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_defaultUnits)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(1, 2);
  Model_t        *m = SBMLDocument_createModel(d);

  const char   *sid = "C";
  Compartment_t  *c = Model_createCompartment(m);

  Compartment_setId   ( c, sid );
  Compartment_setSize ( c, 1.2 ); 
  Compartment_setUnits( c, "volume");

  fail_unless(Model_getNumUnitDefinitions(m) == 0);
  
  fail_unless( SBMLDocument_setLevelAndVersion(d, 3, 1) == 1, NULL);


  fail_unless(Model_getNumUnitDefinitions(m) == 2);

  UnitDefinition_t *ud = Model_getUnitDefinition(m, 0);

  fail_unless (ud != NULL);
  fail_unless (!strcmp(UnitDefinition_getId( ud), "volume"));
  fail_unless(UnitDefinition_getNumUnits(ud) == 1);

  Unit_t * u = UnitDefinition_getUnit(ud, 0);

  fail_unless(Unit_getKind(u) == UNIT_KIND_LITRE);
  fail_unless(Unit_getExponent(u) == 1);
  fail_unless(Unit_getMultiplier(u) == 1);
  fail_unless(Unit_getScale(u) == 0);

  ud = Model_getUnitDefinition(m, 1);

  fail_unless (ud != NULL);
  fail_unless (!strcmp(UnitDefinition_getId( ud), "time"));
  fail_unless(UnitDefinition_getNumUnits(ud) == 1);
  
  u = UnitDefinition_getUnit(ud, 0);

  fail_unless(Unit_getKind(u) == UNIT_KIND_SECOND);
  fail_unless(Unit_getExponent(u) == 1);
  fail_unless(Unit_getMultiplier(u) == 1);
  fail_unless(Unit_getScale(u) == 0);

  fail_unless(!strcmp(Model_getTimeUnits(m), "time"));

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertFromL3)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(3, 1);
  Model_t        *m = SBMLDocument_createModel(d);

  const char   *sid = "C";
  Compartment_t  *c = Model_createCompartment(m);

  Compartment_setId   ( c, sid );
  Compartment_setSize ( c, 1.2 ); 
  Compartment_setConstant( c, 1);
  Compartment_setSpatialDimensionsAsDouble(c, 3.4);

  fail_unless(SBMLDocument_setLevelAndVersion(d, 1, 1) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 1, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 2, 1) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 2, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 2, 3) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 2, 4) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 3, 1) == 1);

}
END_TEST


START_TEST (test_SBMLConvert_invalidLevelVersion)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 1);
  Model_t        *m = SBMLDocument_createModel(d);

  const char   *sid = "C";
  Compartment_t  *c = Model_createCompartment(m);

  Compartment_setId   ( c, sid );
  Compartment_setSize ( c, 1.2 ); 
  Compartment_setUnits( c, "volume");

  fail_unless(SBMLDocument_setLevelAndVersion(d, 1, 3) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 2, 5) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 3, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 4, 1) == 0);

}
END_TEST

START_TEST (test_SBMLConvert_convertToL3_localParameters)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(1, 2);
  Model_t        *m = SBMLDocument_createModel(d);

  Compartment_t  *c = Model_createCompartment(m);
  Compartment_setId   ( c, "c" );

  Species_t *s = Model_createSpecies(m);
  Species_setId(s, "s");
  Species_setCompartment(s, "c");

  Reaction_t * r = Model_createReaction(m);
  SpeciesReference_t *sr = Reaction_createReactant(r);
  SpeciesReference_setSpecies(sr, "s");

  KineticLaw_t *kl = Reaction_createKineticLaw(r);

  KineticLaw_setFormula(kl, "s*k");
  Parameter_t *p = KineticLaw_createParameter(kl);
  Parameter_setId(p, "k");

  fail_unless(KineticLaw_getNumLocalParameters(kl) == 0);
  
  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1 );

  m = SBMLDocument_getModel(d);
  r = Model_getReaction(m,0);
  kl = Reaction_getKineticLaw(r);


  fail_unless(KineticLaw_getNumLocalParameters(kl) == 1);

  LocalParameter_t *lp = KineticLaw_getLocalParameter(kl, 0);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_stoichiometryMath)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 1);
  Model_t        *m = SBMLDocument_createModel(d);

  Compartment_t  *c = Model_createCompartment(m);
  Compartment_setId   ( c, "c" );

  Species_t *s = Model_createSpecies(m);
  Species_setId(s, "s");
  Species_setCompartment(s, "c");

  Reaction_t * r = Model_createReaction(m);
  SpeciesReference_t *sr = Reaction_createReactant(r);
  SpeciesReference_setSpecies(sr, "s");
  StoichiometryMath_t *sm = SpeciesReference_createStoichiometryMath(sr);

  ASTNode_t * ast = SBML_parseFormula("c*2");
  StoichiometryMath_setMath(sm, ast);

  fail_unless(Model_getNumRules(m) == 0);
  fail_unless(SpeciesReference_isSetId(sr) == 0);
  
  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1);

  m = SBMLDocument_getModel(d);
  r = Model_getReaction(m, 0);
  sr = Reaction_getReactant(r, 0);

  fail_unless(Model_getNumRules(m) == 1);
  fail_unless(SpeciesReference_isSetId(sr) == 1);
  
  Rule_t *rule = Model_getRule(m, 0);

  fail_unless( strcmp(SpeciesReference_getId(sr), Rule_getVariable(rule)) == 0 );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_compartment)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 2);
  Model_t        *m = SBMLDocument_createModel(d);

  const char   *sid = "C";
  Compartment_t  *c = Model_createCompartment(m);
  Compartment_t *c1;

  Compartment_setId   ( c, sid );

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1, NULL);

  c1 = Model_getCompartment(m, 0);

  fail_unless(Compartment_hasRequiredAttributes(c1) == 1);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_unit)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 2);
  Model_t        *m = SBMLDocument_createModel(d);

  const char   *sid = "C";
  UnitDefinition_t  *ud = Model_createUnitDefinition(m);
  UnitDefinition_setId   ( ud, sid );
  Unit_t *u = UnitDefinition_createUnit(ud);
  Unit_setKind(u, UNIT_KIND_MOLE);

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1, NULL);

  Unit_t *u1 = UnitDefinition_getUnit(Model_getUnitDefinition(m, 0), 0);

  fail_unless(Unit_hasRequiredAttributes(u1) == 1);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_reaction)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 2);
  Model_t        *m = SBMLDocument_createModel(d);

  const char   *sid = "C";
  Reaction_t  *r = Model_createReaction(m);
  Reaction_setId   ( r, sid );

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1, NULL);

  Reaction_t *r1 = Model_getReaction(m, 0);

  fail_unless(Reaction_hasRequiredAttributes(r1) == 1);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_species)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 2);
  Model_t        *m = SBMLDocument_createModel(d);

  const char   *sid = "C";
  Species_t  *s = Model_createSpecies(m);
  Species_t *s1;

  Species_setId   ( s, sid );
  Species_setCompartment( s, "comp");

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1, NULL);

  s1 = Model_getSpecies(m, 0);

  fail_unless(Species_hasRequiredAttributes(s1) == 1);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_parameter)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 2);
  Model_t        *m = SBMLDocument_createModel(d);

  const char   *sid = "C";
  Parameter_t  *p = Model_createParameter(m);
  Parameter_t *p1;

  Parameter_setId   ( p, sid );

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1, NULL);

  p1 = Model_getParameter(m, 0);

  fail_unless(Parameter_hasRequiredAttributes(p1) == 1);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_reactant)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 2);
  Model_t        *m = SBMLDocument_createModel(d);
  Reaction_t     *r = Model_createReaction(m);

  SpeciesReference_t  *sr = Reaction_createReactant(r);
  SpeciesReference_t *sr1;

  SpeciesReference_setSpecies   ( sr, "s" );

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1, NULL);

  sr1 = Reaction_getReactant(Model_getReaction(m, 0), 0);

  fail_unless(SpeciesReference_hasRequiredAttributes(sr1) == 1);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_product)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 2);
  Model_t        *m = SBMLDocument_createModel(d);
  Reaction_t     *r = Model_createReaction(m);

  SpeciesReference_t  *sr = Reaction_createProduct(r);
  SpeciesReference_t *sr1;

  SpeciesReference_setSpecies   ( sr, "s" );

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1, NULL);

  sr1 = Reaction_getProduct(Model_getReaction(m, 0), 0);

  fail_unless(SpeciesReference_hasRequiredAttributes(sr1) == 1);

  SBMLDocument_free(d);
}
END_TEST

START_TEST (test_SBMLConvert_convertToL3_event)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 2);
  Model_t        *m = SBMLDocument_createModel(d);

  Event_t  *e = Model_createEvent(m);
  Event_t *e1;

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1, NULL);

  e1 = Model_getEvent(m, 0);

  fail_unless(Event_hasRequiredAttributes(e1) == 1);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertToL3_trigger)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 2);
  Model_t        *m = SBMLDocument_createModel(d);

  Event_t  *e = Model_createEvent(m);
  Trigger_t *t = Event_createTrigger(e);

  Trigger_t *t1;

  fail_unless( SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1, NULL);

  t1 = Event_getTrigger(Model_getEvent(m, 0));

  fail_unless(Trigger_hasRequiredAttributes(t1) == 1);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertFromL3_modelUnits)
{
  UnitDefinition_t *ud;
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(3, 1);
  Model_t        *m = SBMLDocument_createModel(d);
  Model_setVolumeUnits(m, "litre");

  fail_unless(Model_getNumUnitDefinitions(m) == 0);
  
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 1, 2) == 1);

  m = SBMLDocument_getModel(d);

  fail_unless(Model_getNumUnitDefinitions(m) == 1);

  ud = Model_getUnitDefinition(m, 0);

  fail_unless(!strcmp(UnitDefinition_getId(ud), "volume"));
  fail_unless(UnitDefinition_getNumUnits(ud) == 1);
  fail_unless(Unit_getKind(UnitDefinition_getUnit(ud, 0)) == UNIT_KIND_LITRE );
}
END_TEST


START_TEST (test_SBMLConvert_convertFromL3_conversionFactor)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(3, 1);
  Model_t        *m = SBMLDocument_createModel(d);
  const char   *sid = "P";

  Model_setConversionFactor(m, sid);
  Parameter_t  *c = Model_createParameter(m);

  Parameter_setId   ( c, sid );
  Parameter_setConstant( c, 1);

  fail_unless(SBMLDocument_setLevelAndVersion(d, 1, 1) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 1, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 2, 1) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 2, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 2, 3) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 2, 4) == 0);
  fail_unless(SBMLDocument_setLevelAndVersion(d, 3, 1) == 1);

}
END_TEST


START_TEST (test_SBMLConvert_convertFromL3_priority)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(3, 1);
  Model_t        *m = SBMLDocument_createModel(d);

  Event_t * e = Model_createEvent(m);
  Priority_t * p = Event_createPriority(e);

  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 1, 1) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 1, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 1) == 1);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 2) == 1);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 3) == 1);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 4) == 1);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1);

}
END_TEST


START_TEST (test_SBMLConvert_convertFromL3_persistent)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(3, 1);
  Model_t        *m = SBMLDocument_createModel(d);

  Event_t * e = Model_createEvent(m);
  Trigger_t * t = Event_createTrigger(e);
  Trigger_setPersistent(t, 0);

  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 1, 1) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 1, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 1) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 3) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 4) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1);

}
END_TEST


START_TEST (test_SBMLConvert_convertFromL3_initialValue)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(3, 1);
  Model_t        *m = SBMLDocument_createModel(d);

  Event_t * e = Model_createEvent(m);
  Trigger_t * t = Event_createTrigger(e);
  Trigger_setInitialValue(t, 0);

  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 1, 1) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 1, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 1) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 2) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 3) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 2, 4) == 0);
  fail_unless(SBMLDocument_setLevelAndVersionNonStrict(d, 3, 1) == 1);

}
END_TEST


Suite *
create_suite_SBMLConvert (void) 
{ 
  Suite *suite = suite_create("SBMLConvert");
  TCase *tcase = tcase_create("SBMLConvert");


  tcase_add_test( tcase, test_SBMLConvert_convertToL2_SBMLDocument       );
  tcase_add_test( tcase, test_SBMLConvert_addModifiersToReaction         );
  tcase_add_test( tcase, test_SBMLConvert_convertToL1_SBMLDocument          );
  tcase_add_test( tcase, test_SBMLConvert_convertToL1_Species_Amount        );
  tcase_add_test( tcase, test_SBMLConvert_convertToL1_Species_Concentration );
  tcase_add_test( tcase, test_SBMLConvert_convertToL2v4_DuplicateAnnotations_doc );
  tcase_add_test( tcase, test_SBMLConvert_convertToL2v4_DuplicateAnnotations_model );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_defaultUnits );
  tcase_add_test( tcase, test_SBMLConvert_convertFromL3 );
  tcase_add_test( tcase, test_SBMLConvert_invalidLevelVersion );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_localParameters );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_stoichiometryMath );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_compartment );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_unit );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_reaction );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_species );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_species );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_parameter );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_reactant );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_product );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_event );
  tcase_add_test( tcase, test_SBMLConvert_convertToL3_trigger );
  tcase_add_test( tcase, test_SBMLConvert_convertFromL3_modelUnits );
  tcase_add_test( tcase, test_SBMLConvert_convertFromL3_conversionFactor );
  tcase_add_test( tcase, test_SBMLConvert_convertFromL3_priority );
  tcase_add_test( tcase, test_SBMLConvert_convertFromL3_persistent );
  tcase_add_test( tcase, test_SBMLConvert_convertFromL3_initialValue );

  suite_add_tcase(suite, tcase);

  return suite;
}

#if __cplusplus
CK_CPPEND
#endif
