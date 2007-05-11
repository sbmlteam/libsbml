/**
 * @file    printModel.cpp
 * @brief   Prints some information about the top-level model
 * @author  Sarah Keating
 * @author  Ben Bornstein
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


using namespace std;


int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cout << endl << "Usage: printSBML <filename>" << endl << endl;
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

  unsigned int level   = document->getLevel  ();
  unsigned int version = document->getVersion();

  cout << endl
       << "File: " << filename
       << " (Level " << level << ", version " << version << ")" << endl;

  Model* model = document->getModel();

  if (model == 0)
  {
    cout << "No model present." << endl;
    return 1;
  }

  cout << "               "
       << (level == 1 ? "name: " : "  id: ")
       << (model->isSetId() ? model->getId() : "(empty)") << endl;

  if (model->isSetSBOTerm())
    cout << "      model sboTerm: " << model->getSBOTerm() << endl;

  cout << "functionDefinitions: " << model->getNumFunctionDefinitions() << endl;
  cout << "    unitDefinitions: " << model->getNumUnitDefinitions    () << endl;
  cout << "   compartmentTypes: " << model->getNumCompartmentTypes   () << endl;
  cout << "        specieTypes: " << model->getNumSpeciesTypes       () << endl;
  cout << "       compartments: " << model->getNumCompartments       () << endl;
  cout << "            species: " << model->getNumSpecies            () << endl;
  cout << "         parameters: " << model->getNumParameters         () << endl;
  cout << " initialAssignments: " << model->getNumInitialAssignments () << endl;
  cout << "              rules: " << model->getNumRules              () << endl;
  cout << "        constraints: " << model->getNumConstraints        () << endl;
  cout << "          reactions: " << model->getNumReactions          () << endl;
  cout << "             events: " << model->getNumEvents             () << endl;
  cout << endl;

  delete document;
  return 0;
}
