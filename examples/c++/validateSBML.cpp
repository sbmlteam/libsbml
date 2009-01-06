/**
 * @file    validateSBML.cpp
 * @brief   Validates an SBML file against the appropriate schema
 * @author  Sarah Keating
 * @author  Ben Bornstein
 * @author  Michael Hucka
 *
 * $Id$
 * $HeadURL$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>

#include <sbml/SBMLTypes.h>
#include "util.h"


using namespace std;


int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cout << endl << "Usage: validateSBML filename" << endl << endl;
    return 1;
  }

  const char* filename = argv[1];
  SBMLDocument* document;
  SBMLReader reader;
  unsigned long long start, stop;

  start    = getCurrentMillis();
  document = reader.readSBML(filename);
  stop     = getCurrentMillis();

  cout << endl;
  cout << "             filename: " << filename              << endl;
  cout << "            file size: " << getFileSize(filename) << endl;
  cout << "       read time (ms): " << stop - start          << endl;

  unsigned int errors = document->getNumErrors();
  bool seriousErrors  = false;

  if (errors > 0)
  {
    for (unsigned int i = 0; i < errors; i++)
    {
      if (document->getError(i)->isFatal() || document->getError(i)->isError())
      {
        seriousErrors = true;
        break;
      }
    }

    cerr << endl << "Encountered " << errors << " "
         << (seriousErrors ? "error" : "warning") << (errors == 1 ? "" : "s")
         << " in this file:" << endl;
    document->printErrors(cerr);
  }

  // If serious errors are encountered while reading an SBML document, it
  // does not make sense to go on and do full consistency checking because
  // the model may be nonsense in the first place.

  if (seriousErrors)
  {
    cerr << endl << "Further consistency checking aborted." << endl;
  }
  else
  {
    unsigned int failures = document->checkConsistency();

    if (failures > 0)
    {
      cout << endl << "Encountered " << failures
	   << " consistency failure"
           << (failures == 1 ? "" : "s") 
	   << " and/or warning"
           << (failures == 1 ? "" : "s") 
	   << " in this file:" << endl;
      document->printErrors(cerr);
    }
    else
    {
      cout << "               errors: 0" << endl;
    }
  }

  delete document;
  return errors;
}
