/**
 * @file    convertSBML.cpp
 * @brief   Converts SBML L1 documents (any version) to L2v1
 * @author  Michael Hucka
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

using namespace std;


#define LATEST_SBML_LEVEL   2
#define LATEST_SBML_VERSION 3

/*
 * 'ignorable' is a list of libSBML error codes that can be ignored for the
 * purposes of this conversion program.  Some conversions are not fatal,
 * but indicate loss of information that does not affect the mathematical
 * structure of a model.
 */
static unsigned int ignorable[] = {
  92001,
  92003,
  92004,
  92005,
  92006,
  93001,
  91003,
  91005,
  91006
};

/*
 * Predicate returning true if the errors encountered are not ignorable.
 */
bool
conversion_errors(SBMLDocument* document, unsigned int errors)
{
  for (unsigned int i = 0; i < errors; i++)
  {
    bool failure = true;

    for (unsigned int n = 0; n < sizeof(ignorable)/sizeof(ignorable[0]); n++)
    {
      if (document->getError(i)->getId() == ignorable[n])
      {
	failure = false;
	break;
      }
    }

    if (failure) return failure;
  }

  return false;
}


/*
 * Main routine.
 */
int
main (int argc, char *argv[])
{
  if (argc != 3)
  {
    cout << "Usage: convertSBML <input-filename> <output-filename>" << endl
	 << "This program will attempt to convert a model either to" << endl
	 << "SBML Level " << LATEST_SBML_LEVEL
	 << " Version " << LATEST_SBML_VERSION
	 << " (if the model is not already) or, if" << endl
	 << "the model is already expressed in Level " << LATEST_SBML_LEVEL
	 << " Version " << LATEST_SBML_VERSION << ", this" << endl
	 << "program will attempt to convert the model to Level 1 Version 2."
	 << endl;
    return 1;
  }

  const char* inputFile   = argv[1];
  const char* outputFile  = argv[2];

  SBMLDocument* document  = readSBML(inputFile);
  unsigned int  errors    = document->getNumErrors();

  if (errors > 0)
  {
    cerr << "Encountered the following SBML errors:" << endl;
    document->printErrors(cerr);
    cerr << "Conversion skipped.  Please correct the problems above first."
	 << endl;
    return errors;
  }

  /**
   * If the given model is not already L2v3, assume that the user wants to
   * convert it to the latest release of SBML (which is L2v3 currently).
   * If the model is already L2v3, assume that the user wants to attempt to
   * convert it down to Level 1 (specifically L1v2).
   */

  unsigned int olevel   = document->getLevel();
  unsigned int oversion = document->getVersion();

  if (olevel < LATEST_SBML_LEVEL || oversion < LATEST_SBML_VERSION)
  {
    cout << "Attempting to convert Level " << olevel << " Version " << oversion
	 << " model to Level " << LATEST_SBML_LEVEL
	 << " Version " << LATEST_SBML_VERSION << "."  << endl;
    document->setLevelAndVersion(LATEST_SBML_LEVEL, LATEST_SBML_VERSION);
  }
  else
  {
    cout << "Attempting to convert Level " << olevel << " Version " << oversion
	 << " model to Level 1 Version 2." << endl;
    document->setLevelAndVersion(1, 2);
  }

  errors = document->getNumErrors();

  if (conversion_errors(document, errors))
  {
    cerr << "Unable to perform conversion due to the following:" << endl;
    document->printErrors(cerr);
    cout << endl;
    cout << "Conversion skipped.  Either libSBML does not (yet)" << endl
	 << "have the ability to convert this model or (automatic)" << endl
	 << "conversion is not possible in this case." << endl;

    return errors;
  }    
  else
  {
    cout << "Conversion completed." << endl;
    writeSBML(document, outputFile);
  }

  return 0;
}

