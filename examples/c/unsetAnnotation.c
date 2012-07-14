/**
 * @file    unsetAnnotation.cpp
 * @brief   unset annotation for each element
 * @author  Akiya Jouraku
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <stdio.h>

#include <sbml/SBMLTypes.h>
#include "util.h"

int
main (int argc, char* argv[])
{
  unsigned int i,j,errors;
  const char* filename   = argv[1];
  SBMLDocument_t* document;
  Model_t* m;

  if (argc != 3)
  {
    printf("\nUsage: unsetAnnotation <input-filename> <output-filename>\n");
    return 1;
  }

  
  filename = argv[1];  
  document = readSBML(filename);

  errors =  SBMLDocument_getNumErrors(document);

  if(errors > 0)
  {
    SBMLDocument_printErrors(document, stderr);
    SBMLDocument_free(document);
    return errors;
  }

  m = SBMLDocument_getModel( document );
  SBase_unsetAnnotation((SBase_t*)m);

  for(i=0; i < Model_getNumReactions(m); i++)
  {
    Reaction_t* re = Model_getReaction(m, i);
    SBase_unsetAnnotation((SBase_t*)re);

    for(j=0; j < Reaction_getNumReactants(re); j++)
    {
      SpeciesReference_t* rt = Reaction_getReactant(re,j);
      SBase_unsetAnnotation((SBase_t*)rt);
    }

    for(j=0; j < Reaction_getNumProducts(re); j++)
    {
      SpeciesReference_t* rt = Reaction_getProduct(re,j);
      SBase_unsetAnnotation((SBase_t*)rt);
    }

    for(j=0; j < Reaction_getNumModifiers(re); j++)
    {
      SpeciesReference_t* md = Reaction_getModifier(re,j);
      SBase_unsetAnnotation((SBase_t*)md);
    }

    if(Reaction_isSetKineticLaw(re))
    {
      KineticLaw_t* kl =  Reaction_getKineticLaw(re);
      SBase_unsetAnnotation((SBase_t*)kl);

      for(j=0; j < KineticLaw_getNumParameters(kl); j++)
      {
        Parameter_t* pa = KineticLaw_getParameter(kl, j);
        SBase_unsetAnnotation((SBase_t*)pa);
      }
    }

  }

  for(i=0; i < Model_getNumSpecies(m); i++)
  {
    Species_t* sp = Model_getSpecies(m, i);
    SBase_unsetAnnotation((SBase_t*)sp);
  }

  for(i=0; i < Model_getNumCompartments(m); i++)
  {
    Compartment_t* sp = Model_getCompartment(m,i);
    SBase_unsetAnnotation((SBase_t*)sp);
  }

  for(i=0; i < Model_getNumFunctionDefinitions(m); i++)
  {
    FunctionDefinition_t* sp = Model_getFunctionDefinition(m,i);
    SBase_unsetAnnotation((SBase_t*)sp);
  }

  for(i=0; i < Model_getNumUnitDefinitions(m); i++)
  {
    UnitDefinition_t* sp = Model_getUnitDefinition(m, i);
    SBase_unsetAnnotation((SBase_t*)sp);
  }

  for(i=0; i < Model_getNumParameters(m); i++)
  {
    Parameter_t* sp = Model_getParameter(m, i);
    SBase_unsetAnnotation((SBase_t*)sp);
  }

  for(i=0; i < Model_getNumRules(m); i++)
  {
    Rule_t* sp = Model_getRule(m, i);
    SBase_unsetAnnotation((SBase_t*)sp);
  }

  for(i=0; i < Model_getNumInitialAssignments(m); i++)
  {
    InitialAssignment_t* sp = Model_getInitialAssignment(m, i);
    SBase_unsetAnnotation((SBase_t*)sp);
  }

  for(i=0; i < Model_getNumEvents(m); i++)
  {
    Event_t* sp = Model_getEvent(m, i);
    SBase_unsetAnnotation((SBase_t*)sp);

    for(j=0; j < Event_getNumEventAssignments(sp); j++)
    {
      EventAssignment_t* ea = Event_getEventAssignment(sp, j);
      SBase_unsetAnnotation((SBase_t*)ea);
    }
  }

  for(i=0; i < Model_getNumSpeciesTypes(m); i++)
  {
    SpeciesType_t* sp = Model_getSpeciesType(m, i);
    SBase_unsetAnnotation((SBase_t*)sp);
  }

  for(i=0; i < Model_getNumConstraints(m); i++)
  {
    Constraint_t* sp = Model_getConstraint(m, i);
    SBase_unsetAnnotation((SBase_t*)sp);
  }

  writeSBML(document, argv[2]);

  SBMLDocument_free(document);
  return errors;
}



