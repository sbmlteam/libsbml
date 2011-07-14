/**
 * @file    readSBML.cpp
 * @brief   Similar to validateSBML, but without the validation
 * @author  Sarah Keating
 * @author  Ben Bornstein
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>

#include <sbml/SBMLTypes.h>
#include <sbml/common/extern.h>
#include "util.h"


using namespace std;
LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cout << endl << "Usage: readSBML filename" << endl << endl;
    return 1;
  }

  const char* filename   = argv[1];
  SBMLDocument* document;
  SBMLReader reader;
  unsigned long long start, stop;

  start    = getCurrentMillis();
  document = reader.readSBML(filename);
  stop     = getCurrentMillis();

  unsigned int errors = document->getNumErrors();

  cout << endl;
  cout << "            filename: " << filename              << endl;
  cout << "           file size: " << getFileSize(filename) << endl;
  cout << "      read time (ms): " << stop - start          << endl;
  cout << " validation error(s): " << errors << endl;
  cout << endl;

  document->printErrors(cerr);

  delete document;
  return errors;
}

END_C_DECLS
