/**
* @file    promoteParameters.cpp
* @brief   promotes all local to global paramters
* @author  Frank T. Bergmann
* 
* This file is part of libSBML.  Please visit http://sbml.org for more
* information about SBML, and the latest version of libSBML.
*/


#include <iostream>
#include <sbml/SBMLTypes.h>
#include <sbml/conversion/ConversionProperties.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

  int
  main (int argc, char* argv[])
{
  if (argc != 3)
  {
    cout << endl << "Usage: promoteParameters input-filename output-filename"
      << endl << endl;
    return 2;
  }

  SBMLDocument *d = readSBML(argv[1]);
  if ( d->getNumErrors(LIBSBML_SEV_ERROR) > 0)
  {
    d->printErrors();
  }
  else
  {
    /* create a new conversion properties structure */
    ConversionProperties props;

    /* add an option that we want to promote parameters */
    props.addOption("promoteLocalParameters", true, "Promotes all Local Parameters to Global ones");

    /* perform the conversion */
    if (d->convert(props) != LIBSBML_OPERATION_SUCCESS)
    {
      cout<< "conversion failed ... " << endl;
      return 3;
    }

    writeSBML(d, argv[2]);
  }

  return 0;
}
