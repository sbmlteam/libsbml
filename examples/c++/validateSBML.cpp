/**
 * @file    validateSBML.cpp
 * @brief   Validates an SBML file against the appropriate schema
 * @author  Sarah Keating
 * @author  Ben Bornstein
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


int
main (int argc, char *argv[])
{
  if (argc != 2)
  {
    cout << endl << "Usage: validateSBML <filename>" << endl << endl;
    return 1;
  }

  const char* filename = argv[1];
  SBMLDocument *document;
  SBMLReader reader;
  unsigned long start, stop;
  unsigned int errors;

  start    = getCurrentMillis();
  document = reader.readSBML(filename);
  stop     = getCurrentMillis();

  errors = document->getNumErrors();

  cout << endl;
  cout << "             filename: " << filename              << endl;
  cout << "            file size: " << getFileSize(filename) << endl;
  cout << "       read time (ms): " << stop - start          << endl;

  if (errors > 0)
  {
    cerr << "Encountered the following SBML errors:" << endl;
    document->printErrors(cerr);
    cerr << endl << "Further consistency checking aborted." << endl;
  }
  else
  {
    errors = document->checkConsistency();
    if (errors > 0)
    {
      cout << " consistency error(s): " << errors << endl;
      cout << endl;
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
