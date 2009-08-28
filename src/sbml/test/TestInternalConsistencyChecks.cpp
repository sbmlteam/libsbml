/**
 * \file    TestInternalConsistencyChecks.cpp
 * \brief   Tests the internal consistency validation.
 * \author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 */
/* Copyright 2004 California Institute of Technology and
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

#include <sbml/SBMLReader.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS



START_TEST (test_internal_consistency_check_99901)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setSpatialDimensions(2);
  c->setId("c");
  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99901);
  */
  /* this will give schema error as level 1 models
   * required a compartment
   * which wont have been added
   */
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 10103);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99902)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setCompartmentType("hh");
  c->setId("c");
  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99902);
  */
  /* this will give schema error as level 1 models
   * required a compartment
   * which wont have been added
   */
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 10103);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99903)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setConstant(true);
  c->setId("c");
  m->addCompartment(c);

  Rule * r = m->createAssignmentRule();
  r->setVariable("c");
  r->setFormula("2*3");


  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99903);
  */
  /* this will give several errors as level 1 models
   * required a compartment
   * which wont have been added
   * which means the rule cant work out what type of rule
   */
  fail_unless(errors == 3);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99903_param)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Parameter *p = new Parameter(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");

  p->setConstant(true);
  p->setId("c");
  m->addParameter(p);

  Rule * r = m->createAssignmentRule();
  r->setVariable("c");
  r->setFormula("2*3");

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99903);
  */
  /* this will give several errors
   * parameter wont have been added
   * the rule cant work out what type of rule
   */
  fail_unless(errors == 2);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99903_localparam)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Parameter *p = new Parameter(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");
  Reaction *r = m->createReaction();
  r->setId("r");
  KineticLaw *kl = r->createKineticLaw();
  kl->setFormula("2");

  p->setId("p");
  p->setConstant(false);
  kl->addParameter(p);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99903);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setId("c");
  c->setMetaId("mmm");
  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  /* this will give schema error as level 1 models
   * required a compartment
   * which wont have been added
   */
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 10103);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_kl)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  KineticLaw *kl = new KineticLaw(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(1, 2);
  Compartment *c = m->createCompartment();
  c->setId("cc");
  Reaction *r = m->createReaction();
  r->setId("r");

  kl->setFormula("2");
  kl->setMetaId("mmm");
  r->setKineticLaw(kl);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_model)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  d->setLevelAndVersion(1, 2);
  Model * m = new Model(2, 4);
  Compartment *c = m->createCompartment();
  c->setId("cc");

  m->setMetaId("mmm");
  d->setModel(m);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  /* this will have error because the model is not added
  */
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 20201);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_param)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Parameter *p = new Parameter(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");

  p->setId("p");
  p->setMetaId("mmm");
  m->addParameter(p);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_react)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Reaction *r = new Reaction(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");

  r->setId("r");
  r->setMetaId("mmm");
  m->addReaction(r);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_rule_assign)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Rule *r = new AssignmentRule(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");
  c->setConstant(false);

  r->setVariable("cc");
  r->setFormula("2");
  r->setMetaId("mmm");
  m->addRule(r);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_rule_rate)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Rule *r = new RateRule(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");
  c->setConstant(false);

  r->setVariable("cc");
  r->setFormula("2");
  r->setMetaId("mmm");
  m->addRule(r);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_rule_alg)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Rule *r = new AlgebraicRule(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");

  r->setMetaId("mmm");
  r->setFormula("2");
  m->addRule(r);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_species)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Species *s = new Species(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");

  s->setCompartment("c");
  s->setId("s");
  s->setMetaId("mmm");
  m->addSpecies(s);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_speciesRef)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  SpeciesReference *sr = new SpeciesReference(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");
  Species * s = m->createSpecies();
  s->setId("s");
  Reaction *r = m->createReaction();
  r->setId("r");

  s->setCompartment("c");
  sr->setSpecies("s");
  sr->setMetaId("mmm");
  r->addProduct(sr);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_unit)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Unit *u = new Unit(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");
  UnitDefinition *ud = m->createUnitDefinition();
  ud->setId("ud");

  u->setMetaId("mmm");
  u->setKind(UNIT_KIND_MOLE);
  ud->addUnit(u);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99904_unitdef)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  UnitDefinition *u = new UnitDefinition(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");

  u->setId("ud");
  u->setMetaId("mmm");
  u->createUnit();
  m->addUnitDefinition(u);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99904);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setId("c");
  c->setSBOTerm(2);
  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);
  */
  /* this will give schema error as level 1 models
   * required a compartment
   * which wont have been added
   */
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 10103);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_ct)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  CompartmentType *ct = new CompartmentType(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(2, 2);
  
  ct->setId("ct");
  ct->setSBOTerm(5);
  m->addCompartmentType(ct);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_delay)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Delay *delay = new Delay(2, 4);
  Event *e = new Event(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(2, 2);
  delay->setSBOTerm(5);
  e->setDelay(delay);
  m->addEvent(e);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_species)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Species *s = new Species(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");

  s->setId("s");
  s->setCompartment("c");
  s->setSBOTerm(2);
  m->addSpecies(s);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_st)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  SpeciesType *ct = new SpeciesType(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(2, 2);
  
  ct->setId("st");
  ct->setSBOTerm(5);
  m->addSpeciesType(ct);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_stoichmath)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  StoichiometryMath *sm = new StoichiometryMath(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(2, 2);
  Species *s = m->createSpecies();
  s->setId("s");
  Compartment *c = m->createCompartment();
  c->setId("c");
  s->setCompartment("c");
  Reaction *r = m->createReaction();
  r->setId("r");
  SpeciesReference *sr = r->createProduct();
  sr->setSpecies("s");
  
  sm->setSBOTerm(5);
  sr->setStoichiometryMath(sm);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_trigger)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Trigger *trigger = new Trigger(2, 4);
  Event *e = new Event(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(2, 2);
  trigger->setSBOTerm(5);
  e->setTrigger(trigger);
  m->addEvent(e);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_unit)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Unit *u = new Unit(2, 4);
  d->setLevelAndVersion(2, 2);
  Model *m = d->createModel();
  UnitDefinition *ud = m->createUnitDefinition();
  ud->setId("ud");

  u->setKind(UNIT_KIND_MOLE);
  u->setSBOTerm(9);
  ud->addUnit(u);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99905_unitdef)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  UnitDefinition *u = new UnitDefinition(2, 4);
  d->setLevelAndVersion(2, 2);
  Model *m = d->createModel();

  u->setId("ud");
  u->setSBOTerm(9);
  u->createUnit();
  m->addUnitDefinition(u);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99905);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99906)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setId("c");
  c->setUnits("mole");
  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99906);
  */
  /* this will give schema error as level 1 models
   * required a compartment
   * which wont have been added
   */
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 10103);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99907)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Compartment *c = new Compartment(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();

  c->setId("c");
  /* note - it is impossible to create the situation where a l1 model
   * has no volume set as the code doesnt let you !!!
   */
  c->unsetVolume();

  m->addCompartment(c);

  errors = d->checkInternalConsistency();

  /* this will give schema error as level 1 models
   * required a compartment
   * which wont have been added
   */
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 10103);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99908)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  CompartmentType *ct = new CompartmentType(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(2, 1);

  ct->setId("ct");
  m->addCompartmentType(ct);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99908);
  */
  fail_unless(errors == 0);


  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99909)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Constraint *ct = new Constraint(2, 4);
  Model *m = d->createModel();

  d->setLevelAndVersion(2, 1);
  m->addConstraint(ct);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99909);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99910)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Event *e = new Event(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(1, 2);
  Compartment *c = m->createCompartment();
  c->setId("cc");
  c->setConstant(false);
  m->addEvent(e);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99910);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_event)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Event *e = new Event(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(2, 1);

  e->setSBOTerm(2);
  m->addEvent(e);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_ea)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");
  c->setConstant(false);
  Event *e = m->createEvent();
  EventAssignment *ea = new EventAssignment(2, 4);
  d->setLevelAndVersion(2, 1);

  ea->setVariable("c");
  ea->setSBOTerm(2);
  e->addEventAssignment(ea);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_fd)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  FunctionDefinition *fd = new FunctionDefinition(2, 4);
  d->setLevelAndVersion(2, 1);

  fd->setId("fd");
  fd->setSBOTerm(2);
  m->addFunctionDefinition(fd );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_kl)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  Reaction *r = m->createReaction();
  r->setId("r");
  KineticLaw *kl = new KineticLaw(2, 4);
  d->setLevelAndVersion(2, 1);

  kl->setSBOTerm(2);
  Parameter *p = kl->createParameter();
  p->setId("p");
  r->setKineticLaw(kl );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_model)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  d->setLevelAndVersion(2, 1);
  Model * m = new Model(2, 4);

  m->setSBOTerm(2);
  d->setModel(m );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  /* this will have error as model wont have been added
   */
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 20201);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_param)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  Parameter *p = new Parameter(2, 4);
  d->setLevelAndVersion(2, 1);

  p->setId("p");
  p->setSBOTerm(2);
  m->addParameter(p );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_react)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  Reaction *r = new Reaction(2, 4);
  d->setLevelAndVersion(2, 1);

  r->setId("r");
  r->setSBOTerm(2);
  m->addReaction(r );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_rule_assign)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  Parameter *p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  Rule *r = new AssignmentRule(2, 4);
  d->setLevelAndVersion(2, 1);

  r->setVariable("p");
  r->setSBOTerm(2);
  m->addRule(r );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_rule_rate)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  Parameter *p = m->createParameter();
  p->setId("p");
  p->setConstant(false);
  Rule *r = new RateRule(2, 4);
  d->setLevelAndVersion(2, 1);

  r->setVariable("p");
  r->setSBOTerm(2);
  m->addRule(r );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_rule_alg)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  Rule *r = new AlgebraicRule(2, 4);
  d->setLevelAndVersion(2, 1);

  r->setSBOTerm(2);
  m->addRule(r );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99911_speciesRef)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  SpeciesReference *sr = new SpeciesReference(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");
  Species * s = m->createSpecies();
  s->setId("s");
  Reaction *r = m->createReaction();
  r->setId("r");

  s->setCompartment("c");
  sr->setSpecies("s");
  sr->setSBOTerm(4);
  r->addReactant(sr);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99911);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99912)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  FunctionDefinition *fd = new FunctionDefinition(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(1, 2);
  Compartment *c = m->createCompartment();
  c->setId("cc");
  c->setConstant(false);
 
  m->addFunctionDefinition(fd );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99912);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99913)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  InitialAssignment *ia = new InitialAssignment(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(1, 2);
  Compartment *c = m->createCompartment();
  c->setId("cc");
  c->setConstant(false);
  m->addInitialAssignment(ia );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99913);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99914)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  Rule *r = new AlgebraicRule(2, 4);
  d->setLevelAndVersion(2, 1);

  r->setVariable("kk");
  m->addRule(r );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99914);
  */

  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99915_alg)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Model *m = d->createModel();
  Rule *r = new AlgebraicRule(2, 4);
  d->setLevelAndVersion(2, 1);

  r->setUnits("kk");
  m->addRule(r );

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99915);
  */

  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99915_assign)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");
  c->setConstant(false);
  AssignmentRule *r = m->createAssignmentRule();
  r->setL1TypeCode(SBML_SPECIES_CONCENTRATION_RULE);

  r->setVariable("c");
  r->setFormula("2");
  r->setUnits("mmm");

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99915);
  */

  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99915_rate)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");
  c->setConstant(false);
  RateRule *r = m->createRateRule();
  r->setL1TypeCode(SBML_SPECIES_CONCENTRATION_RULE);

  r->setFormula("2");
  r->setVariable("c");
  r->setUnits("mmm");

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99915);
  */

  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99916_rule)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Species *s = new Species(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment * c = m->createCompartment();
  c->setId("c");

  s->setId("s");
  s->setCompartment("c");
  s->setConstant(true);
  m->addSpecies(s);

  Rule * r = m->createAssignmentRule();
  r->setVariable("s");
  r->setFormula("2");


  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99916);
  */
  /* this will give several errors
   * species wont have been added
   * the rule cant work out what type of rule
   */
  fail_unless(errors == 2);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99916_reaction)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Species *s = new Species(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment * c = m->createCompartment();
  c->setId("c");
  Reaction * r = m->createReaction();
  r->setId("r");
  SpeciesReference *sr = r->createReactant();

  s->setId("s");
  s->setCompartment("c");
  s->setConstant(true);
  sr->setSpecies("s");
  m->addSpecies(s);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99916);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99917)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Species *s = new Species(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment * c = m->createCompartment();
  c->setId("c");

  s->setId("s");
  s->setCompartment("c");
  s->setSpatialSizeUnits("kkk");
  m->addSpecies(s);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99917);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99918)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Species *s = new Species(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment * c = m->createCompartment();
  c->setId("c");

  s->setId("s");
  s->setCompartment("c");
  s->setSpeciesType("kkk");
  m->addSpecies(s);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99918);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99919)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Species *s = new Species(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment * c = m->createCompartment();
  c->setId("c");

  s->setId("s");
  s->setCompartment("c");
  s->setHasOnlySubstanceUnits(true);
  m->addSpecies(s);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99919);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99920)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  SpeciesReference *sr = new SpeciesReference(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");
  Species * s = m->createSpecies();
  s->setId("s");
  Reaction *r = m->createReaction();
  r->setId("r");

  s->setCompartment("c");
  sr->setSpecies("s");
  sr->setId("mmm");
  r->addProduct(sr);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 2);
  fail_unless(d->getError(0)->getErrorId() == 99920);
  fail_unless(d->getError(1)->getErrorId() == 99921);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99921)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  SpeciesReference *sr = new SpeciesReference(2, 4);
  d->setLevelAndVersion(2, 1);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");
  Species * s = m->createSpecies();
  s->setId("s");
  Reaction *r = m->createReaction();
  r->setId("r");

  s->setCompartment("c");
  sr->setSpecies("s");
  sr->setName("mmm");
  r->addReactant(sr);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(1)->getErrorId() == 99921);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99922)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  SpeciesType *ct = new SpeciesType(2, 4);
  Model *m = d->createModel();

  ct->setId("st");
  d->setLevelAndVersion(2, 1);
  m->addSpeciesType(ct);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(1)->getErrorId() == 99922);
  */
  fail_unless(errors == 0);


  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99923)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  StoichiometryMath *sm = new StoichiometryMath(2, 4);
  Model *m = d->createModel();
  d->setLevelAndVersion(1, 2);
  Species *s = m->createSpecies();
  s->setId("s");
  Compartment *c = m->createCompartment();
  c->setId("c");
  s->setCompartment("c");
  Reaction *r = m->createReaction();
  r->setId("r");
  SpeciesReference *sr = r->createProduct();
  sr->setSpecies("s");
  
  sr->setStoichiometryMath(sm);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99923);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99924)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Unit *u = new Unit(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");
  UnitDefinition *ud = m->createUnitDefinition();
  ud->setId("ud");

  u->setKind(UNIT_KIND_MOLE);
  u->setMultiplier(9);
  ud->addUnit(u);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(1)->getErrorId() == 99924);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


START_TEST (test_internal_consistency_check_99925)
{
  SBMLDocument*     d = new SBMLDocument();
  unsigned int errors;
  Unit *u = new Unit(2, 4);
  d->setLevelAndVersion(1, 2);
  Model *m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("cc");
  UnitDefinition *ud = m->createUnitDefinition();
  ud->setId("ud");

  u->setKind(UNIT_KIND_MOLE);
  u->setOffset(9);
  ud->addUnit(u);

  errors = d->checkInternalConsistency();

  /* as I change the set functions these should become 
   * impossible to create
  fail_unless(errors == 1);
  fail_unless(d->getError(0)->getErrorId() == 99925);
  */
  fail_unless(errors == 0);

  delete d;
}
END_TEST


Suite *
create_suite_TestInternalConsistencyChecks (void)
{ 
  Suite *suite = suite_create("InternalConsistencyChecks");
  TCase *tcase = tcase_create("InternalConsistencyChecks");


  tcase_add_test(tcase, test_internal_consistency_check_99901);
  tcase_add_test(tcase, test_internal_consistency_check_99902);
  tcase_add_test(tcase, test_internal_consistency_check_99903);
  tcase_add_test(tcase, test_internal_consistency_check_99903_param);
  tcase_add_test(tcase, test_internal_consistency_check_99903_localparam);
  tcase_add_test(tcase, test_internal_consistency_check_99904);
  tcase_add_test(tcase, test_internal_consistency_check_99904_kl);
  tcase_add_test(tcase, test_internal_consistency_check_99904_model);
  tcase_add_test(tcase, test_internal_consistency_check_99904_param);
  tcase_add_test(tcase, test_internal_consistency_check_99904_react);
  tcase_add_test(tcase, test_internal_consistency_check_99904_rule_assign);
  tcase_add_test(tcase, test_internal_consistency_check_99904_rule_rate);
  tcase_add_test(tcase, test_internal_consistency_check_99904_rule_alg);
  tcase_add_test(tcase, test_internal_consistency_check_99904_species);
  tcase_add_test(tcase, test_internal_consistency_check_99904_speciesRef);
  tcase_add_test(tcase, test_internal_consistency_check_99904_unit);
  tcase_add_test(tcase, test_internal_consistency_check_99904_unitdef);
  tcase_add_test(tcase, test_internal_consistency_check_99905);
  tcase_add_test(tcase, test_internal_consistency_check_99905_ct);
  tcase_add_test(tcase, test_internal_consistency_check_99905_delay);
  tcase_add_test(tcase, test_internal_consistency_check_99905_species);
  tcase_add_test(tcase, test_internal_consistency_check_99905_st);
  tcase_add_test(tcase, test_internal_consistency_check_99905_stoichmath);
  tcase_add_test(tcase, test_internal_consistency_check_99905_trigger);
  tcase_add_test(tcase, test_internal_consistency_check_99905_unit);
  tcase_add_test(tcase, test_internal_consistency_check_99905_unitdef);
  tcase_add_test(tcase, test_internal_consistency_check_99906);
  tcase_add_test(tcase, test_internal_consistency_check_99907);
  tcase_add_test(tcase, test_internal_consistency_check_99908);
  tcase_add_test(tcase, test_internal_consistency_check_99909);
  tcase_add_test(tcase, test_internal_consistency_check_99910);
  tcase_add_test(tcase, test_internal_consistency_check_99911_event);
  tcase_add_test(tcase, test_internal_consistency_check_99911_ea);
  tcase_add_test(tcase, test_internal_consistency_check_99911_fd);
  tcase_add_test(tcase, test_internal_consistency_check_99911_kl);
  tcase_add_test(tcase, test_internal_consistency_check_99911_model);
  tcase_add_test(tcase, test_internal_consistency_check_99911_param);
  tcase_add_test(tcase, test_internal_consistency_check_99911_react);
  tcase_add_test(tcase, test_internal_consistency_check_99911_rule_assign);
  tcase_add_test(tcase, test_internal_consistency_check_99911_rule_rate);
  tcase_add_test(tcase, test_internal_consistency_check_99911_rule_alg);
  tcase_add_test(tcase, test_internal_consistency_check_99911_speciesRef);
  tcase_add_test(tcase, test_internal_consistency_check_99912);
  tcase_add_test(tcase, test_internal_consistency_check_99913);
  tcase_add_test(tcase, test_internal_consistency_check_99914);
  tcase_add_test(tcase, test_internal_consistency_check_99915_alg);
  tcase_add_test(tcase, test_internal_consistency_check_99915_assign);
  tcase_add_test(tcase, test_internal_consistency_check_99915_rate);
  tcase_add_test(tcase, test_internal_consistency_check_99916_rule);
  tcase_add_test(tcase, test_internal_consistency_check_99916_reaction);
  tcase_add_test(tcase, test_internal_consistency_check_99917);
  tcase_add_test(tcase, test_internal_consistency_check_99918);
  tcase_add_test(tcase, test_internal_consistency_check_99919);
  tcase_add_test(tcase, test_internal_consistency_check_99920);
  tcase_add_test(tcase, test_internal_consistency_check_99921);
  tcase_add_test(tcase, test_internal_consistency_check_99922);
  tcase_add_test(tcase, test_internal_consistency_check_99923);
  tcase_add_test(tcase, test_internal_consistency_check_99924);
  tcase_add_test(tcase, test_internal_consistency_check_99925);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
