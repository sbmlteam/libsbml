/**
 * @file    printUnits.cpp
 * @brief   Prints some unit information about the model
 * @author  Sarah Keating
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>
#include <sbml/SBMLTypes.h>

#include <sbml/UnitDefinition.h>
#include <sbml/units/UnitFormulaFormatter.h>
#include <sbml/units/FormulaUnitsData.h>


using namespace std;


int
main (int argc, char *argv[])
{
  if (argc != 2)
  {
    cout << endl << "Usage: printUnits filename" << endl << endl;
    return 1;
  }

  const char* filename   = argv[1];
  SBMLDocument* document = readSBML(filename);

  if (document->getNumErrors() > 0)
  {
    cerr << "Encountered the following SBML errors:" << endl;
    document->printErrors(cerr);
    return 1;
  }

  Model* model = document->getModel();

  if (model == 0)
  {
    cout << "No model present." << endl;
    return 1;
  }

  unsigned int i,j;
  for (i = 0; i < model->getNumSpecies(); i++)
  {
    Species* s = model->getSpecies(i);
    cout << "Species " << i << ": "
      << printUnits(s->getDerivedUnitDefinition()) << endl;
  }

  for (i = 0; i < model->getNumCompartments(); i++)
  {
    Compartment *c = model->getCompartment(i);
    cout << "Compartment " << i << ": "
      << printUnits(c->getDerivedUnitDefinition()) 
      << endl;
  }

  for (i = 0; i < model->getNumParameters(); i++)
  {
    Parameter *p = model->getParameter(i);
    cout << "Parameter " << i << ": "
      << printUnits(p->getDerivedUnitDefinition()) 
      << endl;
  }


  for (i = 0; i < model->getNumInitialAssignments(); i++)
  {
    InitialAssignment *ia = model->getInitialAssignment(i);
    cout << "InitialAssignment " << i << ": " 
      << printUnits(ia->getDerivedUnitDefinition()) << endl;
    cout << "        undeclared units: ";
    cout << (ia->containsUndeclaredUnits() ? "yes\n" : "no\n");
  }

  for (i = 0; i < model->getNumEvents(); i++)
  {
    Event *e = model->getEvent(i);
    cout << "Event " << i << ": " << endl;

    if (e->isSetDelay())
    {
      cout << "Delay: " 
        << printUnits(e->getDelay()->getDerivedUnitDefinition()) << endl;
      cout << "        undeclared units: ";
      cout << (e->getDelay()->containsUndeclaredUnits() ? "yes\n" : "no\n");
    }
      
    for (j = 0; j < e->getNumEventAssignments(); j++)
    {
      EventAssignment *ea = e->getEventAssignment(j);
      cout << "EventAssignment " << j << ": " 
        << printUnits(ea->getDerivedUnitDefinition()) << endl;
      cout << "        undeclared units: ";
      cout << (ea->containsUndeclaredUnits() ? "yes\n" : "no\n");
    }
  }

  for (i = 0; i < model->getNumReactions(); i++)
  {
    Reaction *r = model->getReaction(i);
      
    cout << "Reaction " << i << ": " << endl;

    if (r->isSetKineticLaw())
    {
      cout << "Kinetic Law: " 
        << printUnits(r->getKineticLaw()->getDerivedUnitDefinition()) << endl;
      cout << "        undeclared units: ";
      cout << (r->getKineticLaw()->containsUndeclaredUnits() ? "yes\n" : "no\n");
    }

    for (j = 0; j < r->getNumReactants(); j++)
    {
      SpeciesReference *sr = r->getReactant(j);

      if (sr->isSetStoichiometryMath())
      {
        cout << "Reactant stoichiometryMath" << j << ": " 
          << printUnits(sr->getDerivedUnitDefinition()) << endl;
        cout << "        undeclared units: ";
        cout << (sr->containsUndeclaredUnits() ? "yes\n" : "no\n");
      }
    }

    for (j = 0; j < r->getNumProducts(); j++)
    {
      SpeciesReference *sr = r->getProduct(j);

      if (sr->isSetStoichiometryMath())
      {
        cout << "Product stoichiometryMath" << j << ": " 
          << printUnits(sr->getDerivedUnitDefinition()) << endl;
        cout << "        undeclared units: ";
        cout << (sr->containsUndeclaredUnits() ? "yes\n" : "no\n");
      }
    }
  }

  for (i = 0; i < model->getNumRules(); i++)
  {
    Rule *r = model->getRule(i);
    cout << "Rule " << i << ": " 
      << printUnits(r->getDerivedUnitDefinition()) << endl;
    cout << "        undeclared units: ";
    cout << (r->containsUndeclaredUnits() ? "yes\n" : "no\n");
  }

  delete document;
  return 0;
}
