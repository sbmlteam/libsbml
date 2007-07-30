/**
 * @file    printAnnotation.cpp
 * @brief   Prints annotation strings for each element
 * @author  Akiya Jouraku
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>

#include <sbml/SBMLTypes.h>
#include "util.h"


using namespace std;


void printAnnotation(SBase *sb, const string& id = "")
{
  if (!sb->isSetAnnotation()) return;

  string pid = id;

  if (pid == "" && sb->isSetId())
  {
    pid = sb->getId();    
  }

  cout << "----- " << sb->getElementName() << " (" << pid 
       << ") annotation -----" << endl;
  cout << sb->getAnnotationString() << endl;
  cout << endl;
}


int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cout << endl << "Usage: printAnnotation filename" << endl << endl;
    return 1;
  }

  unsigned int i,j;
  const char* filename   = argv[1];
  SBMLDocument* document;
  SBMLReader reader;

  document = reader.readSBML(filename);

  unsigned int errors = document->getNumErrors();

  cout << endl;
  cout << "filename: " << filename << endl;
  cout << endl;

  if(errors > 0)
  {
    document->printErrors(cerr);
    delete document;

    return errors;
  }


  /* Model */

  Model* m = document->getModel();
  printAnnotation(m);

  for(i=0; i < m->getNumReactions(); i++)
  {
    Reaction* re = m->getReaction(i);
    printAnnotation(re);

    /* SpeciesReference (Reacatant) */

    for(j=0; j < re->getNumReactants(); j++)
    {
      SpeciesReference* rt = re->getReactant(j);
      if (rt->isSetAnnotation()) cout << "   ";
      printAnnotation(rt, (rt->isSetSpecies() ? rt->getSpecies() : "") );
    }

    /* SpeciesReference (Product) */

    for(j=0; j < re->getNumProducts(); j++)
    {
      SpeciesReference* rt = re->getProduct(j);
      if (rt->isSetAnnotation()) cout << "   ";
      printAnnotation(rt, (rt->isSetSpecies() ? rt->getSpecies() : "") );
    }

    /* ModifierSpeciesReference (Modifiers) */

    for(j=0; j < re->getNumModifiers(); j++)
    {
      ModifierSpeciesReference* md = re->getModifier(j);
      if (md->isSetAnnotation()) cout << "   ";
      printAnnotation(md, (md->isSetSpecies() ? md->getSpecies() : "") );
    }

    /* KineticLaw */

    if(re->isSetKineticLaw())
    {
      KineticLaw* kl = re->getKineticLaw();
      if (kl->isSetAnnotation()) cout << "   ";
      printAnnotation(kl);

      /* Parameter */

      for(j=0; j < kl->getNumParameters(); j++)
      {
        Parameter* pa = kl->getParameter(j);
        if (pa->isSetAnnotation()) cout << "      ";
        printAnnotation(pa);
      }
    }

  }

  /* Species */

  for(i=0; i < m->getNumSpecies(); i++)
  {
    Species* sp = m->getSpecies(i);
    printAnnotation(sp);
  }

  /* Compartments */

  for(i=0; i < m->getNumCompartments(); i++)
  {
    Compartment* sp = m->getCompartment(i);
    printAnnotation(sp);
  }

  /* FunctionDefinition */

  for(i=0; i < m->getNumFunctionDefinitions(); i++)
  {
    FunctionDefinition* sp = m->getFunctionDefinition(i);
    printAnnotation(sp);
  }

  /* UnitDefinition */

  for(i=0; i < m->getNumUnitDefinitions(); i++)
  {
    UnitDefinition* sp = m->getUnitDefinition(i);
    printAnnotation(sp);
  }

  /* Parameter */

  for(i=0; i < m->getNumParameters(); i++)
  {
    Parameter* sp = m->getParameter(i);
    printAnnotation(sp);
  }

  /* Rule */

  for(i=0; i < m->getNumRules(); i++)
  {
    Rule* sp = m->getRule(i);
    printAnnotation(sp);
  }

  /* InitialAssignment */

  for(i=0; i < m->getNumInitialAssignments(); i++)
  {
    InitialAssignment* sp = m->getInitialAssignment(i);
    printAnnotation(sp);
  }

  /* Event */

  for(i=0; i < m->getNumEvents(); i++)
  {
    Event* sp = m->getEvent(i);
    printAnnotation(sp);

    /* Trigger */

    if(sp->isSetTrigger())
    {
      const Trigger* tg = sp->getTrigger();
      if (tg->isSetAnnotation()) cout << "   ";
      printAnnotation(const_cast<Trigger*>(tg));
    }

    /* Delay */

    if(sp->isSetDelay())
    {
      const Delay* dl = sp->getDelay();
      if (dl->isSetAnnotation()) cout << "   ";
      printAnnotation(const_cast<Delay*>(dl));
    }

    /* EventAssignment */

    for(j=0; j < sp->getNumEventAssignments(); j++)
    {
      EventAssignment* ea = sp->getEventAssignment(j);
      if (ea->isSetAnnotation()) cout << "   ";
      printAnnotation(ea);
    }
  }

  /* SpeciesType */

  for(i=0; i < m->getNumSpeciesTypes(); i++)
  {
    SpeciesType* sp = m->getSpeciesType(i);
    printAnnotation(sp);
  }

  /* Constraints */

  for(i=0; i < m->getNumConstraints(); i++)
  {
    Constraint* sp = m->getConstraint(i);
    printAnnotation(sp);
  }

  delete document;
  return errors;
}


