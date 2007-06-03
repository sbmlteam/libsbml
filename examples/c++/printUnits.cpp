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

  model->createListFormulaUnitsData();

  cout << "Total number of formula units: "
       << model->getNumFormulaUnitsData() << endl << endl;

  for (unsigned int n = 0; n < model->getNumFormulaUnitsData(); n++)
  {
    FormulaUnitsData* fud = model->getFormulaUnitsData(n);
    unsigned int numUnits = fud->getUnitDefinition()->getNumUnits();

    cout << "Formula units case #" << (n+1) << " --" << endl;

    cout << "  class of model entity: "
	 << SBMLTypeCode_toString(fud->getTypecode())<< endl;

    cout << "  id of entity in model: " << fud->getId() << endl;

    if (fud->getContainsParametersWithUndeclaredUnits())
    {
      cout << " undeclared parameters?: yes" << endl;
      cout << "  (can they be ignored?: "
	   << (fud->getCanIgnoreUndeclaredUnits() ? "yes)" : "no)") << endl;
    }
    else
    {
      cout << " undeclared parameters?: no" << endl;
    }

    if (numUnits > 0)
    {
      cout << "    units in definition: ";

      for (unsigned int p = 0; p < numUnits; p++)
      {
	UnitKind_t kind = fud->getUnitDefinition()->getUnit(p)->getKind();
	int exp = fud->getUnitDefinition()->getUnit(p)->getExponent();

        cout << UnitKind_toString(kind) << " (exponent = " << exp << ")";

	if (p + 1 < numUnits)
	{
	  cout << ", ";
	}	  

      }
    }
    
    cout << endl << endl;
  }

  delete document;
  return 0;
}
