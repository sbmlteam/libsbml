/**
 * @file    printUnits.cpp
 * @brief   Prints some unit information about the model
 * @author  Sarah Keating
 * @author  Michael Hucka
 *
 * <!--------------------------------------------------------------------------
 * This sample program is distributed under a different license than the rest
 * of libSBML.  This program uses the open-source MIT license, as follows:
 *
 * Copyright (c) 2013-2018 by the California Institute of Technology
 * (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
 * and the University of Heidelberg (Germany), with support from the National
 * Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Neither the name of the California Institute of Technology (Caltech), nor
 * of the European Bioinformatics Institute (EMBL-EBI), nor of the University
 * of Heidelberg, nor the names of any contributors, may be used to endorse
 * or promote products derived from this software without specific prior
 * written permission.
 * ------------------------------------------------------------------------ -->
 */


#include <stdio.h>
#include <sbml/SBMLTypes.h>

#include <sbml/UnitDefinition.h>
#include <sbml/units/UnitFormulaFormatter.h>
#include <sbml/units/FormulaUnitsData.h>


int
main (int argc, char *argv[])
{
  unsigned int i,j;
  char* filename;
  SBMLDocument_t* document;
  Model_t* model;

  if (argc != 2)
  {
    printf("\nUsage: printUnits filename\n");
    return 1;
  }

  filename  = argv[1];
  document  = readSBML(filename);

  if (SBMLDocument_getNumErrors(document) > 0)
  {
    fprintf(stderr, "Encountered the following SBML errors:\n");
    SBMLDocument_printErrors(document, stdout);
    return 1;
  }

  model = SBMLDocument_getModel(document);

  if (model == NULL)
  {
    printf("No model present.\n");
    return 1;
  }
  
  for (i = 0; i < Model_getNumSpecies(model); i++)
  {
    Species_t* s = Model_getSpecies(model, i);
    char* u = UnitDefinition_printUnits(Species_getDerivedUnitDefinition(s), 0);
    printf("Species %d: %s\n", i, u);
    free(u);
  }

  for (i = 0; i < Model_getNumCompartments(model); i++)
  {
    Compartment_t *c = Model_getCompartment(model, i);
    char* u = UnitDefinition_printUnits(Compartment_getDerivedUnitDefinition(c), 0);
    printf("Compartment %d: %s\n", i, u);
    free(u);
  }

  for (i = 0; i < Model_getNumParameters(model); i++)
  {
    Parameter_t *p = Model_getParameter(model, i);
    char* u = UnitDefinition_printUnits(Parameter_getDerivedUnitDefinition(p), 0);
    printf("Parameter %d: %s\n", i, u);
    free(u);
  }


  for (i = 0; i < Model_getNumInitialAssignments(model); i++)
  {
    InitialAssignment_t *ia = Model_getInitialAssignment(model, i);
    char* u = UnitDefinition_printUnits(InitialAssignment_getDerivedUnitDefinition(ia), 0);
    printf("InitialAssignment %d: %s", i, u);
    free(u);
    printf("        undeclared units: %s", 
      (InitialAssignment_containsUndeclaredUnits(ia) ? "yes\n" : "no\n"));
  }

  for (i = 0; i < Model_getNumEvents(model); i++)
  {
    Event_t *e = Model_getEvent(model, i);
    printf("Event %d: \n", i);

    if (Event_isSetDelay(e))
    {
        char* u = UnitDefinition_printUnits(Delay_getDerivedUnitDefinition(Event_getDelay(e)), 0);
        printf( "Delay: %s\n", u);
        printf( "        undeclared units: %s",
        (Delay_containsUndeclaredUnits(Event_getDelay(e)) ? "yes\n" : "no\n"));
        free(u);
    }
      
    for (j = 0; j < Event_getNumEventAssignments(e); j++)
    {
      EventAssignment_t *ea = Event_getEventAssignment(e, j);
      char* u = UnitDefinition_printUnits(EventAssignment_getDerivedUnitDefinition(ea), 0);
         printf( "Delay: %s\n", u);
      printf( "        undeclared units: %s", 
        (EventAssignment_containsUndeclaredUnits(ea) ? "yes\n" : "no\n"));
      free(u);
    }
  }

  for (i = 0; i < Model_getNumReactions(model); i++)
  {
    Reaction_t *r = Model_getReaction(model, i);
      
    printf("Reaction %d: \n", i);

    if (Reaction_isSetKineticLaw(r))
    {
      KineticLaw_t *kl = Reaction_getKineticLaw(r);
      char* u = UnitDefinition_printUnits(KineticLaw_getDerivedUnitDefinition(kl), 0);
      printf( "Kinetic Law: %s\n", u);
      printf( "        undeclared units: %s", 
        (KineticLaw_containsUndeclaredUnits(kl) ? "yes\n" : "no\n"));
      free(u);
    }

    for (j = 0; j < Reaction_getNumReactants(r); j++)
    {
      SpeciesReference_t *sr = Reaction_getReactant(r, j);

      if (SpeciesReference_isSetStoichiometryMath(sr))
      {
        StoichiometryMath_t* sm = SpeciesReference_getStoichiometryMath(sr);
        char* u = UnitDefinition_printUnits(StoichiometryMath_getDerivedUnitDefinition(sm), 0);
        printf( "Reactant stoichiometryMath %d: %s\n", j, u);
        printf( "        undeclared units: %s", 
          (StoichiometryMath_containsUndeclaredUnits(sm) ? "yes\n" : "no\n"));
        free(u);
      }
    }

    for (j = 0; j < Reaction_getNumProducts(r); j++)
    {
      SpeciesReference_t *sr = Reaction_getProduct(r, j);

      if (SpeciesReference_isSetStoichiometryMath(sr))
      {
        StoichiometryMath_t* sm = SpeciesReference_getStoichiometryMath(sr);
        char* u = UnitDefinition_printUnits(StoichiometryMath_getDerivedUnitDefinition(sm), 0);
        printf( "Product stoichiometryMath %d: %s\n", j, u);
        printf( "        undeclared units: %s", 
          (StoichiometryMath_containsUndeclaredUnits(sm) ? "yes\n" : "no\n"));
        free(u);
      }
    }
  }

  for (i = 0; i < Model_getNumRules(model); i++)
  {
    Rule_t *r = Model_getRule(model, i);
    char* u = UnitDefinition_printUnits(Rule_getDerivedUnitDefinition(r), 0);
    printf( "Rule %d: %s\n", i, u);
    printf( "        undeclared units: %s",
     (Rule_containsUndeclaredUnits(r) ? "yes\n" : "no\n"));
    free(u);
  }

  SBMLDocument_free(document);
  return 0;
}
