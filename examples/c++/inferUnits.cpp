/**
* @file    inferUnits.cpp
* @brief   Infers units of parameters (where possible).
* @author  Sarah Keating
* 
* This file is part of libSBML.  Please visit http://sbml.org for more
* information about SBML, and the latest version of libSBML.
*/


#include <iostream>
#include <sbml/SBMLTypes.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/conversion/SBMLConverterRegistry.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

  int
  main (int argc, char* argv[])
{
  if (argc != 3)
  {
    cout << endl << "Usage: inferUnits input-filename output-filename"
      << endl << endl;
    return 2;
  }

  SBMLDocument *d = readSBML(argv[1]);
  d->checkConsistency();
  if ( d->getNumErrors(LIBSBML_SEV_ERROR) > 0)
  {
    d->printErrors();
  }
  else
  {
    /* create a new conversion properties structure */
    ConversionProperties* props = new ConversionProperties();
  
    /* add an option that we want to infer units */
    props->addOption("inferUnits");

    SBMLConverter* converter = 
           SBMLConverterRegistry::getInstance().getConverterFor(*props);
  

    converter->setDocument(d);

    /* perform the conversion */
    if (converter->convert() != LIBSBML_OPERATION_SUCCESS)
    {
      cout<< "conversion failed ... " << endl;
      return 3;
    }

    d->getErrorLog()->clearLog();
    d->validateSBML();
    d->printErrors(cout);

    writeSBML(d, argv[2]);
  }

  return 0;
}
