/**
 * \file    printModel.cpp
 * \brief   Prints some information about the top-level model
 * \author  Sarah Keating and Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <iostream>
#include <sbml/SBMLTypes.h>


using namespace std;


int
main (int argc, char *argv[])
{
  const char* filename;

  SBMLDocument* d;
  Model*        m;

  unsigned int level, version;


  if (argc != 2)
  {
    cout << endl << "  usage: printSBML <filename>" << endl << endl;
    return 2;
  }


  filename = argv[1];
  d        = readSBML(filename);

  d->printErrors(cout);

  m = d->getModel();

  level   = d->getLevel  ();
  version = d->getVersion();

  cout << endl
       << "File: " << filename
       << " (Level " << level << ", version " << version << ")" << endl;

  cout << "         ";

  if (m == 0)
  {
    cout << "No model present." << endl;
    return 1;
  }

  cout << "         ";
  cout << "  model id: " <<  (m->isSetId() ? m->getId() : "(empty)") << endl;

  cout << "functionDefinitions: " << m->getNumFunctionDefinitions() << endl;
  cout << "    unitDefinitions: " << m->getNumUnitDefinitions    () << endl;
  cout << "   compartmentTypes: " << m->getNumCompartmentTypes   () << endl;
  cout << "        specieTypes: " << m->getNumSpeciesTypes       () << endl;
  cout << "       compartments: " << m->getNumCompartments       () << endl;
  cout << "            species: " << m->getNumSpecies            () << endl;
  cout << "         parameters: " << m->getNumParameters         () << endl;
  cout << " initialAssignments: " << m->getNumInitialAssignments () << endl;
  cout << "              rules: " << m->getNumRules              () << endl;
  cout << "        constraints: " << m->getNumConstraints        () << endl;
  cout << "          reactions: " << m->getNumReactions          () << endl;
  cout << "             events: " << m->getNumEvents             () << endl;
  cout << endl;

  delete d;
  return 0;
}
