/**
 * @file    convertSBML.cpp
 * @brief   Converts SBML documents between levels
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


int
main (int argc, char *argv[])
{
  const unsigned int latestLevel   = SBMLDocument::getDefaultLevel();
  const unsigned int latestVersion = SBMLDocument::getDefaultVersion();


  if (argc != 3)
  {
    cout << "Usage: convertSBML input-filename output-filename" << endl
	 << "This program will attempt to convert a model either to" << endl
	 << "SBML Level " << latestLevel << " Version " << latestVersion
	 << " (if the model is not already) or, if" << endl
	 << "the model is already expressed in Level " << latestLevel
	 << " Version " << latestVersion << ", this" << endl
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
  bool success;

  if (olevel < latestLevel || oversion < latestVersion)
  {
    cout << "Attempting to convert Level " << olevel << " Version " << oversion
	 << " model to Level " << latestLevel
	 << " Version " << latestVersion << "."  << endl;
    success = document->setLevelAndVersion(latestLevel, latestVersion);
  }
  else
  {
    cout << "Attempting to convert Level " << olevel << " Version " << oversion
	 << " model to Level 1 Version 2." << endl;
    success = document->setLevelAndVersion(1, 2);
  }

  errors = document->getNumErrors();

  if (!success)
  {
    cerr << "Unable to perform conversion due to the following:" << endl;
    document->printErrors(cerr);
    cout << endl;
    cout << "Conversion skipped.  Either libSBML does not (yet)" << endl
	 << "have the ability to convert this model or (automatic)" << endl
	 << "conversion is not possible in this case." << endl;

    return errors;
  }   
  else if (errors > 0)
  {
    cout << "Information may have been lost in conversion; but a valid model ";
    cout << "was produced by the conversion.\nThe following information ";
    cout << "was provided:\n";
    document->printErrors(cout);
    writeSBML(document, outputFile);
  }
  else
  {
    cout << "Conversion completed." << endl;
    writeSBML(document, outputFile);
  }

  return 0;
}

