/**
 * Filename    : TestSAX2SBMLHandler.cpp
 * Description : SAX2SBMLHandler unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-10-30
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
#include "sbml/SBMLTypes.h"

#include "sbml/SAX2SBMLHandler.hpp"
#include "sbml/SBMLReader.h"
#include "sbml/SBMLUnicodeConstants.hpp"


#ifdef __cplusplus
extern "C" {
#endif


#define XML_HEADER   "<?xml version='1.0' encoding='ascii'?>\n"
#define SBML_HEADER  "<sbml level='1' version='1'> <model name='testModel'>\n"
#define SBML_FOOTER  "</model> </sbml>"

/**
 * Wraps the string s in the appropriate XML or SBML boilerplate.
 */
#define wrapXML(s)   XML_HEADER s
#define wrapSBML(s)  XML_HEADER SBML_HEADER s SBML_FOOTER


SBMLDocument_t *D;


void
SAX2SBMLHandlerTest_setup (void)
{
  D = NULL;
}


void
SAX2SBMLHandlerTest_teardown (void)
{
  SBMLDocument_free(D);
}


START_TEST (test_element_SBML)
{
  const char* s = wrapXML("<sbml level='1' version='1'> </sbml>");
  

  D = readSBMLFromString(s);

  fail_unless(D->level   == 1, NULL);
  fail_unless(D->version == 1, NULL);
}
END_TEST


START_TEST (test_element_Model)
{
  const char* s = wrapXML
  (
    "<sbml level='1' version='1'>"
    "  <model name='testModel'> </model>"
    "</sbml>"
  );

  D = readSBMLFromString(s);
  fail_unless( !strcmp(D->model->name, "testModel"), NULL );
}
END_TEST


START_TEST (test_element_UnitDefinition)
{
  UnitDefinition_t* ud;

  const char* s = wrapSBML("<unitDefinition name='mmls'/>");


  D = readSBMLFromString(s);
  fail_unless( Model_getNumUnitDefinitions(D->model) == 1, NULL );

  ud = Model_getUnitDefinition(D->model, 0);
  fail_unless( !strcmp(ud->name, "mmls"), NULL );
}
END_TEST


START_TEST (test_element_Unit)
{
  Unit_t*           u;
  UnitDefinition_t* ud;


  const char* s = wrapSBML
  (
    "<unitDefinition name='substance'>"
    "  <listOfUnits> <unit kind='mole' scale='-3'/> </listOfUnits>"
    "</unitDefinition>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumUnitDefinitions(D->model) == 1, NULL );

  ud = Model_getUnitDefinition(D->model, 0);

  fail_unless( !strcmp(ud->name, "substance"), NULL );
  fail_unless( UnitDefinition_getNumUnits(ud) == 1, NULL );

  u = UnitDefinition_getUnit(ud, 0);

  fail_unless( u->kind     == UNIT_KIND_MOLE, NULL );
  fail_unless( u->exponent ==  1, NULL );
  fail_unless( u->scale    == -3, NULL );
}
END_TEST


START_TEST (test_element_Unit_defaults)
{
  Unit_t*           u;
  UnitDefinition_t* ud;


  const char* s = wrapSBML
  (
    "<unitDefinition name='bogomips'>"
    "  <listOfUnits> <unit kind='second'/> </listOfUnits>"
    "</unitDefinition>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumUnitDefinitions(D->model) == 1, NULL );

  ud = Model_getUnitDefinition(D->model, 0);

  fail_unless( !strcmp(ud->name, "bogomips"), NULL );
  fail_unless( UnitDefinition_getNumUnits(ud) == 1, NULL );

  u = UnitDefinition_getUnit(ud, 0);

  fail_unless( u->kind     == UNIT_KIND_SECOND, NULL );
  fail_unless( u->exponent == 1, NULL );
  fail_unless( u->scale    == 0, NULL );
}
END_TEST


START_TEST (test_element_Compartment)
{
  Compartment_t* c;

  const char* s = wrapSBML
  (
    "<listOfCompartments>"
    "  <compartment name='mitochondria' volume='.0001' units='milliliters'"
    "               outside='cell'/>"
    "</listOfCompartments>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumCompartments(D->model) == 1, NULL );

  c = Model_getCompartment(D->model, 0);

  fail_unless( !strcmp( c->name   , "mitochondria" ), NULL );
  fail_unless( !strcmp( c->units  , "milliliters"  ), NULL );
  fail_unless( !strcmp( c->outside, "cell"         ), NULL );
  fail_unless( c->volume == .0001, NULL );
}
END_TEST


START_TEST (test_element_Compartment_defaults)
{
  Compartment_t* c;

  const char* s = wrapSBML
  (
     "<listOfCompartments> <compartment name='cell'/> </listOfCompartments>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumCompartments(D->model) == 1, NULL );

  c = Model_getCompartment(D->model, 0);

  fail_unless( !strcmp( c->name, "cell" ), NULL );
  fail_unless( c->volume  == 1.0 , NULL );
  fail_unless( c->units   == NULL, NULL );
  fail_unless( c->outside == NULL, NULL );
}
END_TEST


START_TEST (test_element_Specie)
{
  Species_t* sp;

  const char* s = wrapSBML
  (
    "<specie name='Glucose' compartment='cell' initialAmount='4.1'"
    "        units='volume' boundaryCondition='false' charge='6'/>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumSpecies(D->model) == 1, NULL );

  sp = Model_getSpecies(D->model, 0);

  fail_unless( !strcmp( sp->name       , "Glucose" ), NULL );
  fail_unless( !strcmp( sp->compartment, "cell"    ), NULL );
  fail_unless( !strcmp( sp->units      , "volume"  ), NULL );

  fail_unless( sp->initialAmount     == 4.1, NULL );
  fail_unless( sp->boundaryCondition == 0  , NULL );
  fail_unless( sp->charge            == 6  , NULL );

  fail_unless( sp->isSet.initialAmount     == 1, NULL );
  fail_unless( sp->isSet.boundaryCondition == 1, NULL );
  fail_unless( sp->isSet.charge            == 1, NULL );
}
END_TEST


START_TEST (test_element_Specie_defaults)
{
  Species_t* sp;

  const char* s = wrapSBML
  (
    "<specie name='Glucose' compartment='cell' initialAmount='1.0'/>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumSpecies(D->model) == 1, NULL );

  sp = Model_getSpecies(D->model, 0);

  fail_unless( !strcmp( sp->name       , "Glucose" ), NULL );
  fail_unless( !strcmp( sp->compartment, "cell"    ), NULL );

  fail_unless( sp->initialAmount     == 1.0, NULL );
  fail_unless( sp->boundaryCondition == 0  , NULL );

  fail_unless( sp->isSet.initialAmount     == 1, NULL );
  fail_unless( sp->isSet.boundaryCondition == 0, NULL );
  fail_unless( sp->isSet.charge            == 0, NULL );
}
END_TEST


START_TEST (test_element_Species)
{
  Species_t* sp;

  const char* s = wrapSBML
  (
    "<species name='Glucose' compartment='cell' initialAmount='4.1'"
    "         units='volume' boundaryCondition='false' charge='6'/>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumSpecies(D->model) == 1, NULL );

  sp = Model_getSpecies(D->model, 0);

  fail_unless( !strcmp( sp->name       , "Glucose" ), NULL );
  fail_unless( !strcmp( sp->compartment, "cell"    ), NULL );
  fail_unless( !strcmp( sp->units      , "volume"  ), NULL );

  fail_unless( sp->initialAmount     == 4.1, NULL );
  fail_unless( sp->boundaryCondition == 0  , NULL );
  fail_unless( sp->charge            == 6  , NULL );

  fail_unless( sp->isSet.initialAmount     == 1, NULL );
  fail_unless( sp->isSet.boundaryCondition == 1, NULL );
  fail_unless( sp->isSet.charge            == 1, NULL );
}
END_TEST


START_TEST (test_element_Parameter)
{
  Parameter_t* p;

  const char* s = wrapSBML
  (
    "<parameter name='Km1' value='2.3' units='second'/>"
  );

    
  D = readSBMLFromString(s);

  fail_unless( Model_getNumParameters(D->model) == 1, NULL );

  p = Model_getParameter(D->model, 0);

  fail_unless( !strcmp( p->name , "Km1"    ), NULL );
  fail_unless( !strcmp( p->units, "second" ), NULL );
  fail_unless( p->value == 2.3, NULL );
}
END_TEST


START_TEST (test_element_Reaction)
{
  Reaction_t* r;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'/>"
  );


  D = readSBMLFromString(s);
  
  fail_unless( Model_getNumReactions(D->model) == 1, NULL );
  
  r = Model_getReaction(D->model, 0);

  fail_unless( !strcmp(r->name, "reaction_1"), NULL );
  fail_unless( r->reversible == 0, NULL );
  fail_unless( r->fast       == 0, NULL );
}
END_TEST


START_TEST (test_element_Reaction_defaults)
{
  Reaction_t* r;

  const char* s = wrapSBML("<reaction name='reaction_1'/>");


  D = readSBMLFromString(s);
  
  fail_unless( Model_getNumReactions(D->model) == 1, NULL );
  
  r = Model_getReaction(D->model, 0);

  fail_unless( !strcmp(r->name, "reaction_1"), NULL );
  fail_unless( r->reversible != 0, NULL );
  fail_unless( r->fast       == 0, NULL );
}
END_TEST


START_TEST (test_element_SpecieReference_Reactant)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'>"
    "  <listOfReactants>"
    "    <specieReference specie='X0' stoichiometry='1'/>"
    "  </listOfReactants>"
    "</reaction>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumReactions(D->model) == 1, NULL );
  
  r = Model_getReaction(D->model, 0);

  fail_unless( !strcmp(r->name, "reaction_1"), NULL );
  fail_unless( r->reversible == 0, NULL );
  fail_unless( Reaction_getNumReactants(r) == 1, NULL );

  sr = Reaction_getReactant(r, 0);

  fail_unless( !strcmp(sr->species, "X0"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );
}
END_TEST


START_TEST (test_element_SpecieReference_Product)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'>"
    "  <listOfProducts>"
    "    <specieReference specie='S1' stoichiometry='1'/>"
    "  </listOfProducts>"
    "</reaction>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumReactions(D->model) == 1, NULL );
  
  r = Model_getReaction(D->model, 0);

  fail_unless( !strcmp(r->name, "reaction_1"), NULL );
  fail_unless( r->reversible == 0, NULL );
  fail_unless( Reaction_getNumProducts(r) == 1, NULL );

  sr = Reaction_getProduct(r, 0);

  fail_unless( !strcmp(sr->species, "S1"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );
}
END_TEST


START_TEST (test_element_SpecieReference_defaults)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'>"
    "  <listOfReactants>"
    "    <specieReference specie='X0'/>"
    "  </listOfReactants>"
    "</reaction>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumReactions(D->model) == 1, NULL );
  
  r = Model_getReaction(D->model, 0);

  fail_unless( !strcmp(r->name, "reaction_1"), NULL );
  fail_unless( r->reversible == 0, NULL );
  fail_unless( Reaction_getNumReactants(r) == 1, NULL );

  sr = Reaction_getReactant(r, 0);

  fail_unless( !strcmp(sr->species, "X0"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );
}
END_TEST


START_TEST (test_element_SpeciesReference_defaults)
{
  Reaction_t*         r;
  SpeciesReference_t* sr;

  const char* s = wrapSBML
  (
    "<reaction name='reaction_1' reversible='false'>"
    "  <listOfReactants>"
    "    <speciesReference species='X0'/>"
    "  </listOfReactants>"
    "</reaction>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumReactions(D->model) == 1, NULL );
  
  r = Model_getReaction(D->model, 0);

  fail_unless( !strcmp(r->name, "reaction_1"), NULL );
  fail_unless( r->reversible == 0, NULL );
  fail_unless( Reaction_getNumReactants(r) == 1, NULL );

  sr = Reaction_getReactant(r, 0);

  fail_unless( !strcmp(sr->species, "X0"), NULL );
  fail_unless( sr->stoichiometry == 1, NULL );
  fail_unless( sr->denominator   == 1, NULL );
}
END_TEST


START_TEST (test_element_KineticLaw)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'/>"
    "</reaction>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumReactions(D->model) == 1, NULL );

  r  = Model_getReaction(D->model, 0);
  kl = r->kineticLaw;

  fail_unless( !strcmp(kl->formula, "k1*X0"), NULL );
}
END_TEST


START_TEST (test_element_KineticLaw_Parameter)
{
  Reaction_t*   r;
  KineticLaw_t* kl;
  Parameter_t*  p;

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumReactions(D->model) == 1, NULL );

  r  = Model_getReaction(D->model, 0);
  kl = r->kineticLaw;

  fail_unless( !strcmp(kl->formula, "k1*X0"), NULL );
  fail_unless( KineticLaw_getNumParameters(kl) == 1, NULL );

  p = KineticLaw_getParameter(kl, 0);

  fail_unless( !strcmp(p->name, "k1"), NULL );
  fail_unless( p->value == 0, NULL );
}
END_TEST


START_TEST (test_element_AlgebraicRule)
{
  AlgebraicRule_t *ar;

  const char *s = wrapSBML("<algebraicRule formula='x + 1'/>");


  D = readSBMLFromString(s);

  fail_unless( Model_getNumRules(D->model) == 1, NULL );

  ar = (AlgebraicRule_t *) Model_getRule(D->model, 0);

  fail_unless( !strcmp(ar->formula, "x + 1"), NULL );
}
END_TEST


START_TEST (test_element_CompartmentVolumeRule)
{
  CompartmentVolumeRule_t *cvr;

  const char *s = wrapSBML
  (
    "<compartmentVolumeRule compartment='A' formula='0.10 * t'/>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumRules(D->model) == 1, NULL );

  cvr = (CompartmentVolumeRule_t *) Model_getRule(D->model, 0);

  fail_unless( !strcmp( cvr->compartment, "A"        ), NULL );
  fail_unless( !strcmp( cvr->formula    , "0.10 * t" ), NULL );
  fail_unless( cvr->type == RULE_TYPE_SCALAR, NULL );
}
END_TEST


START_TEST (test_element_ParameterRule)
{
  ParameterRule_t *pr;

  const char *s = wrapSBML
  (
    "<parameterRule name='k' formula='k3/k2'/>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumRules(D->model) == 1, NULL );

  pr = (ParameterRule_t *) Model_getRule(D->model, 0);

  fail_unless( !strcmp( pr->name   , "k"     ), NULL );
  fail_unless( !strcmp( pr->formula, "k3/k2" ), NULL );
  fail_unless( pr->type == RULE_TYPE_SCALAR, NULL );
}
END_TEST


START_TEST (test_element_SpecieConcentrationRule)
{
  SpeciesConcentrationRule_t *scr;

  const char *s = wrapSBML
  (
    "<specieConcentrationRule specie='s2' formula='k * t/(1 + k)'/>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumRules(D->model) == 1, NULL );

  scr = (SpeciesConcentrationRule_t *) Model_getRule(D->model, 0);

  fail_unless( !strcmp( scr->species, "s2"            ), NULL );
  fail_unless( !strcmp( scr->formula, "k * t/(1 + k)" ), NULL );
  fail_unless( scr->type == RULE_TYPE_SCALAR, NULL );
}
END_TEST


START_TEST (test_element_SpecieConcentrationRule_rate)
{
  SpeciesConcentrationRule_t *scr;

  const char *s = wrapSBML
  (
    "<specieConcentrationRule specie='s2' formula='k * t/(1 + k)' type='rate'/>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumRules(D->model) == 1, NULL );

  scr = (SpeciesConcentrationRule_t *) Model_getRule(D->model, 0);

  fail_unless( !strcmp( scr->species, "s2"            ), NULL );
  fail_unless( !strcmp( scr->formula, "k * t/(1 + k)" ), NULL );
  fail_unless( scr->type == RULE_TYPE_RATE, NULL );
}
END_TEST


START_TEST (test_element_SpeciesConcentrationRule)
{
  SpeciesConcentrationRule_t *scr;

  const char *s = wrapSBML
  (
    "<speciesConcentrationRule species='s2' formula='k * t/(1 + k)'/>"
  );


  D = readSBMLFromString(s);

  fail_unless( Model_getNumRules(D->model) == 1, NULL );

  scr = (SpeciesConcentrationRule_t *) Model_getRule(D->model, 0);

  fail_unless( !strcmp( scr->species, "s2"            ), NULL );
  fail_unless( !strcmp( scr->formula, "k * t/(1 + k)" ), NULL );
  fail_unless( scr->type == RULE_TYPE_SCALAR, NULL );
}
END_TEST


START_TEST (test_element_notes)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <notes>This is a test note.</notes>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D  = readSBMLFromString(s);
  r  = Model_getReaction(D->model, 0);
  kl = r->kineticLaw;

  fail_unless( !strcmp(kl->notes, "This is a test note."), NULL );
}
END_TEST


START_TEST (test_element_notes_after)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "    <notes>This note is <b>after</b> everything else.</notes>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D  = readSBMLFromString(s);
  r  = Model_getReaction(D->model, 0);
  kl = r->kineticLaw;

  fail_unless( !strcmp(kl->notes,
                       "This note is <b>after</b> everything else."), NULL );
}
END_TEST


START_TEST (test_element_notes_xmlns)
{
  const char* n =
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\"> Some text... </body>";

  const char* s = wrapSBML
  (
    "<notes>"
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\"> Some text... </body>"
    "</notes>"
  );


  D = readSBMLFromString(s);
  fail_unless( !strcmp(D->model->notes, n), NULL );
}
END_TEST


START_TEST (test_element_notes_nested)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* n = "This is a test <notes>nested</notes> note.";

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <notes>This is a test <notes>nested</notes> note.</notes>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D  = readSBMLFromString(s);
  r  = Model_getReaction(D->model, 0);
  kl = r->kineticLaw;

  fail_unless( !strcmp(kl->notes, n), NULL );
  fail_unless( SBMLDocument_getNumWarnings(D) == 1, NULL );
}
END_TEST


START_TEST (test_element_notes_sbml)
{
  const char* n =
    "Notes are not allowed as part of the SBML element.";

  const char* s = wrapXML
  (
    "<sbml level='1' version='1'>"
    "  <notes>Notes are not allowed as part of the SBML element.</notes>"
    "</sbml>"
  );


  D = readSBMLFromString(s);

  fail_unless( !strcmp( D->notes, n), NULL );
  fail_unless( SBMLDocument_getNumErrors(D) == 1, NULL );
}
END_TEST


START_TEST (test_element_annotation)
{
  const char* a =
    "<annotation xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotation>";

  const char* s = wrapSBML
  (
    "<annotation xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotation>"
  );


  D = readSBMLFromString(s);
  fail_unless( !strcmp(D->model->annotation, a), NULL );
}
END_TEST


START_TEST (test_element_annotations)
{
  const char* a =
    "<annotations xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotations>";

  const char* s = wrapSBML
  (
    "<annotations xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotations>"
  );


  D = readSBMLFromString(s);
  fail_unless( !strcmp(D->model->annotation, a), NULL );
}
END_TEST


START_TEST (test_element_annotation_after)
{
  Reaction_t*   r;
  KineticLaw_t* kl;

  const char* a =
    "<annotation xmlns:vc=\"http://www.sbml.org/2001/ns/vcell\">"
    "      <vc:modifiers vc:name=\"k1\"></vc:modifiers>"
    "    </annotation>";

  const char* s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0'>"
    "    <listOfParameters>"
    "      <parameter name='k1' value='0'/>"
    "    </listOfParameters>"
    "    <annotation xmlns:vc=\"http://www.sbml.org/2001/ns/vcell\">"
    "      <vc:modifiers vc:name=\"k1\"/>"
    "    </annotation>"
    "  </kineticLaw>"
    "</reaction>"
  );


  D  = readSBMLFromString(s);
  r  = Model_getReaction(D->model, 0);
  kl = r->kineticLaw;

  fail_unless( !strcmp(kl->annotation, a), NULL );
}
END_TEST


START_TEST (test_element_annotation_nested)
{
  const char* a =
    "<annotation xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <annotation type=\"bogus\">"
    "    Why would you want to nest annotations?"
    "  </annotation>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotation>";

  const char* s = wrapSBML
  (
    "<annotation xmlns:mysim=\"http://www.mysim.org/ns\">"
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">"
    "  </mysim:nodecolors>"
    "  <annotation type=\"bogus\">"
    "    Why would you want to nest annotations?"
    "  </annotation>"
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>"
    "</annotation>"
  );


  D = readSBMLFromString(s);
  fail_unless( !strcmp(D->model->annotation, a), NULL );
}
END_TEST


START_TEST (test_element_annotation_sbml)
{
  const char* a =
    "<annotation xmlns:jd=\"http://www.sys-bio.org/sbml\">"
    "    <jd:header>"
    "      <VersionHeader SBMLVersion=\"1.0\"></VersionHeader>"
    "    </jd:header>"
    "    <jd:display>"
    "      <SBMLGraphicsHeader BackGroundColor=\"15728639\">"
    "</SBMLGraphicsHeader>"
    "    </jd:display>"
    "  </annotation>";

  const char* s = wrapXML
  (
    "<sbml level=\"1\" version=\"1\">"
    "  <annotation xmlns:jd = \"http://www.sys-bio.org/sbml\">"
    "    <jd:header>"
    "      <VersionHeader SBMLVersion = \"1.0\"/>"
    "    </jd:header>"
    "    <jd:display>"
    "      <SBMLGraphicsHeader BackGroundColor = \"15728639\"/>"
    "    </jd:display>"
    "  </annotation>"
    "</sbml>"
  );


  D = readSBMLFromString(s);

  fail_unless( !strcmp(D->annotation, a), NULL );
  fail_unless( SBMLDocument_getNumErrors(D) == 1, NULL );

}
END_TEST


Suite *
create_suite_SAX2SBMLHandler (void)
{
  Suite *suite = suite_create("SAX2SBMLHandler");
  TCase *tcase = tcase_create("SAX2SBMLHandler");


  tcase_add_checked_fixture( tcase,
                             SAX2SBMLHandlerTest_setup,
                             SAX2SBMLHandlerTest_teardown );

  tcase_add_test( tcase, test_element_SBML                         );
  tcase_add_test( tcase, test_element_Model                        );
  tcase_add_test( tcase, test_element_UnitDefinition               );
  tcase_add_test( tcase, test_element_Unit                         );
  tcase_add_test( tcase, test_element_Unit_defaults                );
  tcase_add_test( tcase, test_element_Compartment                  );
  tcase_add_test( tcase, test_element_Compartment_defaults         );
  tcase_add_test( tcase, test_element_Specie                       );
  tcase_add_test( tcase, test_element_Specie_defaults              );
  tcase_add_test( tcase, test_element_Species                      );
  tcase_add_test( tcase, test_element_Parameter                    );
  tcase_add_test( tcase, test_element_Reaction                     );
  tcase_add_test( tcase, test_element_Reaction_defaults            );
  tcase_add_test( tcase, test_element_SpecieReference_Reactant     );
  tcase_add_test( tcase, test_element_SpecieReference_Product      );
  tcase_add_test( tcase, test_element_SpecieReference_defaults     );
  tcase_add_test( tcase, test_element_SpeciesReference_defaults    );
  tcase_add_test( tcase, test_element_KineticLaw                   );
  tcase_add_test( tcase, test_element_KineticLaw_Parameter         );
  tcase_add_test( tcase, test_element_AlgebraicRule                );
  tcase_add_test( tcase, test_element_CompartmentVolumeRule        );
  tcase_add_test( tcase, test_element_ParameterRule                );
  tcase_add_test( tcase, test_element_SpecieConcentrationRule      );
  tcase_add_test( tcase, test_element_SpecieConcentrationRule_rate );
  tcase_add_test( tcase, test_element_SpeciesConcentrationRule     );
  tcase_add_test( tcase, test_element_notes                        );
  tcase_add_test( tcase, test_element_notes_after                  );
  tcase_add_test( tcase, test_element_notes_xmlns                  );
  tcase_add_test( tcase, test_element_notes_nested                 );
  tcase_add_test( tcase, test_element_notes_sbml                   );
  tcase_add_test( tcase, test_element_annotation                   );
  tcase_add_test( tcase, test_element_annotations                  );
  tcase_add_test( tcase, test_element_annotation_after             );
  tcase_add_test( tcase, test_element_annotation_nested            );
  tcase_add_test( tcase, test_element_annotation_sbml              );

  suite_add_tcase(suite, tcase);

  return suite;
}


#ifdef __cplusplus
}
#endif
