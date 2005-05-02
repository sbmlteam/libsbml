/**
 * \file    printModel.cpp
 * \brief   Prints some information about the top-level model
 * \author  Sarah Keating and Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002-2004 California Institute of Technology and
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
 *     Sarah Keating
 *
 *     The SBML Team
 *     STRI
 *     University of Hertfordshire
 *     Hatfield, UK
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"

#include <iostream>


using namespace std;


int
main (int argc, char *argv[])
{
  const char*   filename;
  SBMLDocument* d;

  unsigned int level, version;


  if (argc != 2)
  {
    cout << endl << "  usage: printSBML <filename>" << endl << endl;
    return 1;
  }


  filename = argv[1];
  d        = readSBML(filename);

  d->printWarnings(cout);
  d->printErrors  (cout);
  d->printFatals  (cout);

  m = d->getModel();
  if (!m) return 2;

  level   = d->getLevel  ();
  version = d->getVersion();

  cout << endl
       << "File: " << filename
       << " (Level " << level << ", version " << version << ")" << endl;

  cout << "         ";

  if (level == 1)
  {
	  cout << "model name: " << m->getName() << endl;
  }
  else
  {
    cout <<"  model id: " <<  (m->isSetId() ? m->getId() : "(empty)") << endl;
  }

  cout << "functionDefinitions: " <<   m->getNumFunctionDefinitions() << endl;
  cout << "    unitDefinitions: " <<   m->getNumUnitDefinitions()     << endl;
  cout << "       compartments: " <<   m->getNumCompartments()        << endl;
  cout << "            species: " <<   m->getNumSpecies()             << endl;
  cout << "         parameters: " <<   m->getNumParameters()          << endl;
  cout << "          reactions: " <<   m->getNumReactions()           << endl;
  cout << "              rules: " <<   m->getNumRules()               << endl;
  cout << "             events: " <<   m->getNumEvents()              << endl;
  cout << endl;

  delete d;
  return 0;
}
