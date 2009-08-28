/**
 * @file    echoSBML.cpp
 * @brief   Echos (and pretty prints) an SBML model.
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>
#include <sbml/SBMLTypes.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

int
main (int argc, char* argv[])
{
  if (argc != 3)
  {
    cout << endl << "Usage: echoSBML input-filename output-filename"
         << endl << endl;
    return 2;
  }

  writeSBML(readSBML(argv[1]), argv[2]);
  return 0;
}
