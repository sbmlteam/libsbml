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

#include <sbml/Unitdefinition.h>
#include <sbml/units/UnitFormulaFormatter.h>
#include <sbml/units/FormulaUnitsData.h>


using namespace std;


int
main (int argc, char *argv[])
{
  const char* filename;

  SBMLDocument* d;
  Model*        m;
  FormulaUnitsData* fud;
  unsigned int numUnits;


  if (argc != 2)
  {
    cout << endl << "  usage: printUnits <filename>" << endl << endl;
    return 2;
  }


  filename = argv[1];
  d        = readSBML(filename);

  d->printErrors(cout);

  m = d->getModel();


  if (m == 0)
  {
    cout << "No model present." << endl;
    return 1;
  }


  m->createListFormulaUnitsData();

  cout << " formula: " << m->getNumFormulaUnitsData() << endl << endl;

  for (unsigned int n = 0; n < m->getNumFormulaUnitsData(); n++)
  {
    fud = m->getFormulaUnitsData(n);
    numUnits = fud->getUnitDefinition()->getNumUnits();
    cout << "FormulaUnits " << n << ":\n";
    cout << "id: " << fud->getId() << endl;
    cout << "type: " << SBMLTypeCode_toString(fud->getTypecode())<< endl;

    if (fud->getContainsParametersWithUndeclaredUnits())
    {
      cout << "params undeclared: yes\n";
      
      if (fud->getCanIgnoreUndeclaredUnits())
      {
        cout << "can ignore: yes\n";
      }
      else
      {
        cout << "can ignore: no\n";
      }
    }
    else
    {
      cout << "params undeclared: no\n";
    }

    cout << "no units: " << numUnits << endl;
    if (numUnits > 0)
    {
      cout << "units: ";
      for (unsigned int p = 0; p < numUnits; p++)
      {
        cout << "\t" << UnitKind_toString(fud->getUnitDefinition()->getUnit(p)->getKind());
        cout << "\t" << fud->getUnitDefinition()->getUnit(p)->getExponent();
        cout << endl;
      }
    }
    
    cout << endl;
  }

  delete d;
  return 0;
}
