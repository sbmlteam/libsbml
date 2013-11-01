/**
* @file    stripPackage.cpp
* @brief   Strips the given package from the given SBML file.
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
  if (argc != 4)
  {
    cout << endl << "Usage: stripPackage input-filename package-to-strip output-filename"
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

    /* add an option that we want to strip a given package */
    props.addOption("stripPackage", true, "Strip SBML Level 3 package constructs from the model");

    /* add an option with the package we want to remove */
    props.addOption("package", argv[2],"Name of the SBML Level 3 package to be stripped");

    /* perform the conversion */
    if (d->convert(props) != LIBSBML_OPERATION_SUCCESS)
    {
      cout<< "conversion failed ... " << endl;
      return 3;
    }

    writeSBML(d, argv[3]);
  }

  return 0;
}
