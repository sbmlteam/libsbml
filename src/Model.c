/**
 * Filename    : Model.c
 * Description : SBML Model
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
#include "sbml/Model.h"


/**
 * Creates a new Model and returns a pointer to it.
 */
Model_t *
Model_create (void)
{
  Model_t *m;


  m = (Model_t *) safe_calloc(1, sizeof(Model_t));

  SBase_init((SBase_t *) m, SBML_MODEL);

  m->unitDefinition = (List_t *) List_create();
  m->compartment    = (List_t *) List_create();
  m->species        = (List_t *) List_create();
  m->parameter      = (List_t *) List_create();
  m->rule           = (List_t *) List_create();
  m->reaction       = (List_t *) List_create();

  return m;
}


/**
 * Creates a new Model with the given name and returns a pointer to it.
 * This convenience function is functionally equivalent to:
 *
 *   Model_setName(Model_create(), sname);
 */
Model_t *
Model_createWith (const char *sname)
{
  Model_t *m = Model_create();


  Model_setName(m, sname);

  return m;
}


/**
 * Frees the given Model.
 */
void
Model_free (Model_t *m)
{
  unsigned int size;
  Rule_t       *rule;


  if (m == NULL) return;

  List_freeItems( m->unitDefinition, UnitDefinition_free, UnitDefinition_t );
  List_freeItems( m->compartment   , Compartment_free   , Compartment_t    );
  List_freeItems( m->species       , Species_free       , Species_t        );
  List_freeItems( m->parameter     , Parameter_free     , Parameter_t      );
  List_freeItems( m->reaction      , Reaction_free      , Reaction_t       );

  size = List_size(m->rule);

  while (size--)
  {
    rule = (Rule_t *) List_remove(m->rule, 0);

    /**
     * If I use a switch statement here (which I would prefer) 'gcc -Wall'
     * warns me that not all values of the enumeration are covered in
     * case statements.  How clever!  Grumble, grumble... :(
     */
    if (rule->typecode == SBML_ALGEBRAIC_RULE)
    {
      AlgebraicRule_free((AlgebraicRule_t *) rule);
    }
    else if (rule->typecode == SBML_COMPARTMENT_VOLUME_RULE)
    {
      CompartmentVolumeRule_free((CompartmentVolumeRule_t *) rule);
    }
    else if (rule->typecode == SBML_PARAMETER_RULE)
    {
      ParameterRule_free((ParameterRule_t *) rule);
    }
    else if (rule->typecode == SBML_SPECIES_CONCENTRATION_RULE)
    {
      SpeciesConcentrationRule_free((SpeciesConcentrationRule_t *) rule);
    }
  }

  List_free( m->unitDefinition );
  List_free( m->compartment    );
  List_free( m->species        );
  List_free( m->parameter      );
  List_free( m->reaction       );
  List_free( m->rule           );

  SBase_clear((SBase_t *) m);

  safe_free(m->name);
  safe_free(m);
}


/**
 * Sets the name field of this Model to a copy of sname.
 */
void
Model_setName (Model_t *m, const char *sname)
{
  if (m->name != NULL)
  {
    safe_free(m->name);
  }

  m->name = (sname == NULL) ? NULL : safe_strdup(sname);
}


/**
 * Creates a new UnitDefinition inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addUnitDefinition(m, UnitDefinition_create());
 */
UnitDefinition_t *
Model_createUnitDefinition (Model_t *m)
{
  UnitDefinition_t *ud = UnitDefinition_create();


  Model_addUnitDefinition(m, ud);
  return ud;
}


/**
 * Creates a new Unit inside this Model and returns a pointer to it.  The
 * Unit is added to the last UnitDefinition created.
 *
 * If a UnitDefinitions does not exist for this model, a new Unit is not
 * created and NULL is returned.
 */
Unit_t *
Model_createUnit (Model_t *m)
{
  Unit_t           *u    = NULL;
  UnitDefinition_t *ud   = NULL;
  unsigned int      size = Model_getNumUnitDefinitions(m);


  if (size > 0)
  {
    u  = Unit_create();
    ud = Model_getUnitDefinition(m, size - 1);

    UnitDefinition_addUnit(ud, u);
  }

  return u;
}


/**
 * Creates a new Compartment inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addCompartment(m, Compartment_create());
 */
Compartment_t *
Model_createCompartment (Model_t *m)
{
  Compartment_t *c = Compartment_create();


  Model_addCompartment(m, c);
  return c;
}


/**
 * Creates a new Species inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addSpecies(m, Species_create());
 */
Species_t *
Model_createSpecies (Model_t *m)
{
  Species_t *s = Species_create();


  Model_addSpecies(m, s);
  return s;
}


/**
 * Creates a new Parameter inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addParameter(m, Parameter_create());
 */
Parameter_t *
Model_createParameter (Model_t *m)
{
  Parameter_t *p = Parameter_create();


  Model_addParameter(m, p);
  return p;
}


/**
 * Creates a new AlgebraicRule inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, AlgebraicRule_create());
 */
AlgebraicRule_t *
Model_createAlgebraicRule (Model_t *m)
{
  AlgebraicRule_t *ar = AlgebraicRule_create();


  Model_addRule(m, (Rule_t *) ar);
  return ar;
}


/**
 * Creates a new CompartmentVolumeRule inside this Model and returns a
 * pointer to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, CompartmentVolumeRule_create());
 */
CompartmentVolumeRule_t *
Model_createCompartmentVolumeRule (Model_t *m)
{
  CompartmentVolumeRule_t *cvr = CompartmentVolumeRule_create();


  Model_addRule(m, (Rule_t *) cvr);
  return cvr;
}


/**
 * Creates a new ParameterRule inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, ParameterRule_create());
 */
ParameterRule_t *
Model_createParameterRule (Model_t *m)
{
  ParameterRule_t *pr = ParameterRule_create();


  Model_addRule(m, (Rule_t *) pr);
  return pr;
}


/**
 * Creates a new SpeciesConcentrationRule inside this Model and returns a
 * pointer to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, SpeciesConcentrationRule_create());
 */
SpeciesConcentrationRule_t *
Model_createSpeciesConcentrationRule (Model_t *m)
{
  SpeciesConcentrationRule_t *scr = SpeciesConcentrationRule_create();


  Model_addRule(m, (Rule_t *) scr);
  return scr;
}


/**
 * Creates a new Reaction inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addReaction(m, Reaction_create());
 */
Reaction_t *
Model_createReaction (Model_t *m)
{
  Reaction_t *r = Reaction_create();


  Model_addReaction(m, r);
  return r;
}


/**
 * Creates a new Reactant (i.e. SpeciesReference) inside this Model and
 * returns a pointer to it.  The SpeciesReference is added to the reactants
 * of the last Reaction created.
 *
 * If a Reaction does not exist for this model, a new SpeciesReference is
 * not created and NULL is returned.
 */
SpeciesReference_t *
Model_createReactant (Model_t *m)
{
  SpeciesReference_t *sr   = NULL;
  Reaction_t         *r    = NULL;
  unsigned int        size = Model_getNumReactions(m);


  if (size > 0)
  {
    sr = SpeciesReference_create();
    r  = Model_getReaction(m, size - 1);

    Reaction_addReactant(r, sr);
  }

  return sr;
}


/**
 * Creates a new Product (i.e. SpeciesReference) inside this Model and
 * returns a pointer to it.  The SpeciesReference is added to the products
 * of the last Reaction created.
 *
 * If a Reaction does not exist for this model, a new SpeciesReference is
 * not created and NULL is returned.
 */
SpeciesReference_t *
Model_createProduct (Model_t *m)
{
  SpeciesReference_t *sr   = NULL;
  Reaction_t         *r    = NULL;
  unsigned int        size = Model_getNumReactions(m);


  if (size > 0)
  {
    sr = SpeciesReference_create();
    r  = Model_getReaction(m, size - 1);

    Reaction_addProduct(r, sr);
  }

  return sr;
}


/**
 * Creates a new KineticLaw inside this Model and returns a pointer to it.
 * The KineticLaw is associated with the last Reaction created.
 *
 * If a Reaction does not exist for this model, or a Reaction does exist,
 * but already has a KineticLaw, a new KineticLaw is not created and NULL
 * is returned.
 */
KineticLaw_t *
Model_createKineticLaw (Model_t *m)
{
  KineticLaw_t *kl   = NULL;
  Reaction_t   *r    = NULL;
  unsigned int  size = Model_getNumReactions(m);


  if (size > 0)
  {
    r  = Model_getReaction(m, size - 1);
    kl = r->kineticLaw;

    if (kl == NULL)
    {
      kl            = KineticLaw_create();
      r->kineticLaw = kl;
    }
    else
    {
      kl = NULL;
    }
  }

  return kl;
}


/**
 * Creates a new Parameter (of a KineticLaw) inside this Model and returns
 * a pointer to it.  The Parameter is associated with the KineticLaw of the
 * last Reaction created.
 *
 * If a Reaction does not exist for this model, or a KineticLaw for the
 * Reaction, a new Parameter is not created and NULL is returned.
 */
Parameter_t *
Model_createKineticLawParameter (Model_t *m)
{
  Parameter_t  *p    = NULL;
  KineticLaw_t *kl   = NULL;
  Reaction_t   *r    = NULL;
  unsigned int  size = Model_getNumReactions(m);


  if (size > 0)
  {
    r  = Model_getReaction(m, size - 1);
    kl = r->kineticLaw;

    if (kl != NULL)
    {
      p = Parameter_create();
      KineticLaw_addParameter(kl, p);
    }
  }

  return p;
}


/**
 * Adds the given UnitDefinition to this Model.
 */
void
Model_addUnitDefinition (Model_t *m, UnitDefinition_t *ud)
{
  List_add(m->unitDefinition, ud);
}


/**
 * Adds the given Compartment to this Model.
 */
void
Model_addCompartment (Model_t *m, Compartment_t *c)
{
  List_add(m->compartment, c);
}


/**
 * Adds the given Species to this Model.
 */
void
Model_addSpecies (Model_t *m, Species_t *s)
{
  List_add(m->species, s);
}


/**
 * Adds the given Parameter to this Model.
 */
void
Model_addParameter (Model_t *m, Parameter_t *c)
{
  List_add(m->parameter, c);
}


/**
 * Adds the given Rule to this Model.
 */
void
Model_addRule (Model_t *m, Rule_t *r)
{
  List_add(m->rule, r);
}


/**
 * Adds the given Reaction to this Model.
 */
void
Model_addReaction (Model_t *m, Reaction_t *r)
{
  List_add(m->reaction, r);
}


/**
 * @return the nth UnitDefinition of this Model.
 */
UnitDefinition_t *
Model_getUnitDefinition (Model_t *m, unsigned int n)
{
  return (UnitDefinition_t *) List_get(m->unitDefinition, n);
}


/**
 * @return the nth Compartment of this Model.
 */
Compartment_t *
Model_getCompartment (Model_t *m, unsigned int n)
{
  return (Compartment_t *) List_get(m->compartment, n);
}


/**
 * @return the nth Species of this Model.
 */
Species_t *
Model_getSpecies (Model_t *m, unsigned int n)
{
  return (Species_t *) List_get(m->species, n);
}


/**
 * @return the nth Parameter of this Model.
 */
Parameter_t *
Model_getParameter (Model_t *m, unsigned int n)
{
  return (Parameter_t *) List_get(m->parameter, n);
}


/**
 * @return the nth Rule of this Model.
 */
Rule_t *
Model_getRule (Model_t *m, unsigned int n)
{
  return (Rule_t *) List_get(m->rule, n);
}


/**
 * @return the nth Reaction of this Model.
 */
Reaction_t *
Model_getReaction (Model_t *m, unsigned int n)
{
  return (Reaction_t *) List_get(m->reaction, n);
}


/**
 * @return the number of UnitDefinitions in this Model.
 */
unsigned int
Model_getNumUnitDefinitions (Model_t *m)
{
  return List_size(m->unitDefinition);
}


/**
 * @return the number of Compartments in this Model.
 */
unsigned int
Model_getNumCompartments (Model_t *m)
{
  return List_size(m->compartment);
}


/**
 * @return the number of Species in this Model.
 */
unsigned int
Model_getNumSpecies (Model_t *m)
{
  return List_size(m->species);
}


/**
 * @return the number of Parameters in this Model.  Parameters defined in
 * KineticLaws are not included.
 */
unsigned int
Model_getNumParameters (Model_t *m)
{
  return List_size(m->parameter);
}


/**
 * @return the number of Rules in this Model.
 */
unsigned int
Model_getNumRules (Model_t *m)
{
  return List_size(m->rule);
}


/**
 * @return the number of Reactions in this Model.
 */
unsigned int
Model_getNumReactions (Model_t *m)
{
  return List_size(m->reaction);
}
