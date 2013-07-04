/**
 * @file    flattenModel.cpp
 * @brief   SBML hierarchical composition flattening example
 * @author  Sarah Keating
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <iostream>
#include <sstream>

#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/conversion/SBMLConverterRegistry.h>

#ifdef  LIBSBML_HAS_PACKAGE_COMP
#include <sbml/packages/comp/common/CompExtensionTypes.h>
#endif


LIBSBML_CPP_NAMESPACE_USE
using namespace std;

const string usage = "Usage: flattenModel [-p] inputFile outputFile\n"
                     " -p : list unused ports";


int main(int argc,char** argv)
{
  bool leavePorts = false;
  if (argc < 3)
  {
    cout << usage << endl;
    return 1;
  }
  else if (argc == 3)
  {
    if ( string("-p") == string(argv[1]) )
    {
      cout << usage << endl;
      return 1;
    }       
  }

  int  argIndex = 1;
  
  if ( string("-p") == string(argv[1]) )
  {
    leavePorts = true;
    ++argIndex;
  }     

  if (SBMLExtensionRegistry::isPackageEnabled("comp") == false)
  {
    cerr << "The version of libsbml being used does not have the comp"
      << " package code enabled" << endl;
    return 1;
  }

  const char* inputFile   = argv[argIndex];
  const char* outputFile  = argv[argIndex+1];

  SBMLDocument* document = readSBML(inputFile);

  if (document->getNumErrors() > 0)
  {
    cerr << "Encountered the following SBML errors:" << endl;
    document->printErrors(cerr);
    return 1;
  }

  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");
  props->addOption("leavePorts", leavePorts);

  SBMLConverter* converter = 
             SBMLConverterRegistry::getInstance().getConverterFor(*props);
  

  converter->setDocument(document);
  
  int result = converter->convert();

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    cerr << "Conversion failed\n";
    document->printErrors();
  }

  writeSBML(document, outputFile);

}
