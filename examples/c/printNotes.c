/**
 * @file    printNotes.cpp
 * @brief   Prints notes strings for each element
 * @author  Akiya Jouraku
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <stdio.h>

#include <sbml/SBMLTypes.h>
#include "util.h"


void printNotes(SBase_t *sb, const char* id )
{
    if (!SBase_isSetNotes(sb)) return;
  
  printf("----- %s (%s) notes -----\n%s\n\n"
  , SBase_getElementName(sb)
  , id
  , SBase_getNotesString(sb)
  );
}


int
main (int argc, char* argv[])
{
  unsigned int i,j,errors;
  const char* filename;
  SBMLDocument_t* document;
  Model_t* m;

  if (argc != 2)
  {
    printf("\nUsage: printNotes filename\n\n");
    return 1;
  }

  filename  = argv[1];
  document  = readSBML(filename);

  errors = SBMLDocument_getNumErrors( document);

  printf("\n%s\n\n", filename);

  if(errors > 0)
  {
    SBMLDocument_printErrors(document, stderr);
    SBMLDocument_free(document);
    return errors;
  }


  /* Model */

  m = SBMLDocument_getModel(document);
  printNotes((SBase_t*)m, Model_getId(m));

  for(i=0; i < Model_getNumReactions(m); i++)
  {
    Reaction_t* re = Model_getReaction( m, i);
    printNotes((SBase_t*)re, Reaction_getId(re));

    /* SpeciesReference (Reactant) */

    for(j=0; j < Reaction_getNumReactants( re); j++)
    {
      SpeciesReference_t* rt =  Reaction_getReactant(re, j);
      if (SBase_isSetNotes((SBase_t*) rt)) printf("   ");
      printNotes((SBase_t*)rt, SpeciesReference_getSpecies( rt ) );
    }

    /* SpeciesReference (Product) */

    for(j=0; j < Reaction_getNumProducts( re ); j++)
    {
      SpeciesReference_t* rt = Reaction_getProduct( re, j);
      if (SBase_isSetNotes((SBase_t*) rt)) printf("   ");
      printNotes((SBase_t*)rt, SpeciesReference_getSpecies( rt ) );
    }

    /* ModifierSpeciesReference (Modifiers) */

    for(j=0; j < Reaction_getNumModifiers( re ); j++)
    {
      SpeciesReference_t* md = Reaction_getModifier(re, j);
      if (SBase_isSetNotes((SBase_t*) md)) printf("   ");
      printNotes((SBase_t*)md, SpeciesReference_getSpecies( md ) );
    }

    /* KineticLaw */

    if(Reaction_isSetKineticLaw( re ))
    {
      KineticLaw_t* kl = Reaction_getKineticLaw( re );
      if (SBase_isSetNotes((SBase_t*) kl)) printf("   ");
      printNotes((SBase_t*)kl, "");

      /* Parameter */

      for(j=0; j < KineticLaw_getNumParameters( kl ); j++)
      {
        Parameter_t* pa = KineticLaw_getParameter( kl, j);
        if (SBase_isSetNotes((SBase_t*) pa)) printf("   ");
        printNotes((SBase_t*)pa, Parameter_getId(pa));
      }
    }

  }

  /* Species */

  for(i=0; i < Model_getNumSpecies(m); i++)
  {
    Species_t* sp = Model_getSpecies(m, i);
    printNotes((SBase_t*)sp, Species_getId(sp));
  }

  /* Compartments */

  for(i=0; i < Model_getNumCompartments( m ); i++)
  {
    Compartment_t* sp = Model_getCompartment(m, i);
    printNotes((SBase_t*)sp, Compartment_getId(sp));
  }

  /* FunctionDefinition */

  for(i=0; i < Model_getNumFunctionDefinitions(m); i++)
  {
    FunctionDefinition_t* sp = Model_getFunctionDefinition(m, i);
    printNotes((SBase_t*)sp, FunctionDefinition_getId(sp));
  }

  /* UnitDefinition */

  for(i=0; i < Model_getNumUnitDefinitions(m); i++)
  {
    UnitDefinition_t* sp = Model_getUnitDefinition( m, i);
    printNotes((SBase_t*)sp, UnitDefinition_getId(sp));
  }

  /* Parameter */

  for(i=0; i < Model_getNumParameters( m ); i++)
  {
    Parameter_t* sp = Model_getParameter( m, i);
    printNotes((SBase_t*)sp, Parameter_getId(sp));
  }

  /* Rule */

  for(i=0; i < Model_getNumReactions( m ); i++)
  {
    Rule_t* sp = Model_getRule(m, i);
    printNotes((SBase_t*)sp, "");
  }

  /* InitialAssignment */

  for(i=0; i < Model_getNumInitialAssignments(m); i++)
  {
    InitialAssignment_t* sp = Model_getInitialAssignment(m, i);
    printNotes((SBase_t*)sp, "");
  }

  /* Event */

  for(i=0; i < Model_getNumEvents(m); i++)
  {
    Event_t* sp = Model_getEvent(m, i);
    printNotes((SBase_t*)sp, Event_getId(sp));

    /* Trigger */

    if(Event_isSetTrigger( sp ))
    {
      Trigger_t* tg = Event_getTrigger(sp);
      if (SBase_isSetNotes( (SBase_t*) tg)) printf( "   " );
      printNotes((SBase_t*)tg, "");
    }

    /* Delay */

    if(Event_isSetDelay(sp))
    {
      Delay_t* dl = Event_getDelay(sp);
      if (SBase_isSetNotes( (SBase_t*) dl)) printf( "   " );
      printNotes((SBase_t*) dl, "");
    }

    /* EventAssignment */

    for(j=0; j < Event_getNumEventAssignments(sp); j++)
    {
      EventAssignment_t* ea = Event_getEventAssignment(sp, j);
      if (SBase_isSetNotes( (SBase_t*) ea)) printf( "   " );      
      printNotes((SBase_t*)ea, "");
    }
  }

  /* SpeciesType */

  for(i=0; i < Model_getNumSpeciesTypes(m); i++)
  {
    SpeciesType_t* sp = Model_getSpeciesType(m, i);
    printNotes((SBase_t*)sp, SpeciesType_getId(sp));
  }

  /* Constraints */

  for(i=0; i < Model_getNumConstraints(m); i++)
  {
    Constraint_t* sp = Model_getConstraint(m, i);
    printNotes((SBase_t*)sp, "");
  }

  SBMLDocument_free( document );
  return errors;
}



