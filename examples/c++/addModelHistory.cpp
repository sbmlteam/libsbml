/**
 * \file    addModelHistory.cpp
 * \brief   adds Model History to a model
 * \author  Sarah Keating
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>
#include <sbml/SBMLTypes.h>

#include <sbml/xml/XMLNode.h>
#include <sbml/annotation/ModelHistory.h>
using namespace std;
LIBSBML_CPP_NAMESPACE_USE

void printStatus(std::string message, int status)
{
	std::string statusString; 
	switch(status)
	{
	case LIBSBML_OPERATION_SUCCESS:
		statusString = "succeeded";
		break;
	case LIBSBML_INVALID_OBJECT:
		statusString = "invalid object";
		break;
	case LIBSBML_OPERATION_FAILED:
		statusString = "operation failed";
		break;
	default: 
		statusString = "unknown";
		break;
	}

	cout << message << statusString << endl;

}

int
main (int argc, char *argv[])
{

  SBMLDocument* d;
  unsigned int  errors;

  if (argc != 3)
  {
    cout << endl
         << "  usage: addModelHistory <input-filename> <output-filename>" << endl
         << endl;
    return 2;
  }


  d      = readSBML(argv[1]);
  errors = d->getNumErrors();

  if (errors > 0)
  {
    cout << "Read Error(s):" << endl;
	  d->printErrors(cout);

    cout << "Correct the above and re-run." << endl;
  }
  else
  {
    ModelHistory * h = new ModelHistory();

    ModelCreator *c = new ModelCreator();
    c->setFamilyName("Keating");
    c->setGivenName("Sarah");
    c->setEmail("sbml-team@caltech.edu");
    c->setOrganization("University of Hertfordshire");

    int status = h->addCreator(c);
	printStatus("Status for addCreator: ", status);


    Date * date = new Date("1999-11-13T06:54:32");
    Date * date2 = new Date("2007-11-30T06:54:00-02:00");
   
    status = h->setCreatedDate(date);
	printStatus("Set created date:      ", status);

    status = h->setModifiedDate(date2);
	printStatus("Set modified date:     ", status);

    status = d->getModel()->setModelHistory(h);
	printStatus("Set model history:     ", status);

  
    writeSBML(d, argv[2]);
  }

  delete d;
  return errors;
}

