/**
 * \file    addModelHistory.cpp
 * \brief   adds Model History to a model
 * \author  Sarah Keating
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <stdio.h>

#include <sbml/SBMLTypes.h>

#include <sbml/xml/XMLNode.h>
#include <sbml/annotation/ModelHistory.h>


void printStatus(const char* message, int status)
{
	printf(message);
	switch(status)
	{
	case LIBSBML_OPERATION_SUCCESS:
    printf("succeeded");
		break;
	case LIBSBML_INVALID_OBJECT:
    printf("invalid object");
		break;
	case LIBSBML_OPERATION_FAILED:
    printf("operation failed");
		break;
	default: 
    printf("unknown");
		break;
	}
  printf("\n");
}

int
main (int argc, char *argv[])
{

  SBMLDocument_t* d;
  Model_t* m;
  unsigned int  errors;

  if (argc != 3)
  {
    printf("\n"
      "  usage: addModelHistory <input-filename> <output-filename>\n"
      "\n");
    return 2;
  }


  d      = readSBML(argv[1]);
  errors = SBMLDocument_getNumErrors(d);

  if (errors > 0)
  {
    printf("Read Error(s):\n");
    SBMLDocument_printErrors(d, stdout);	 
    printf("Correct the above and re-run.\n");
  }
  else
  {
    int status;
    Date_t* date, *date2;
    ModelHistory_t* h = ModelHistory_create();
    ModelCreator_t* c = ModelCreator_create();

    ModelCreator_setFamilyName(c, "Keating");
    ModelCreator_setGivenName(c, "Sarah");
    ModelCreator_setEmail(c, "sbml-team@caltech.edu");
    ModelCreator_setOrganisation(c, "University of Hertfordshire");


    status = ModelHistory_addCreator(h, c);
	  printStatus("Status for addCreator: ", status);

    
    date = Date_createFromString("1999-11-13T06:54:32");
    date2 = Date_createFromString("2007-11-30T06:54:00-02:00");
       
    status = ModelHistory_setCreatedDate(h, date);
	  printStatus("Set created date:      ", status);

    status = ModelHistory_setModifiedDate(h, date2);
	  printStatus("Set modified date:     ", status);

    m = SBMLDocument_getModel(d);
    status =  Model_setModelHistory(m, h);
	  printStatus("Set model history:     ", status);
  
    writeSBML(d, argv[2]);
  }

  SBMLDocument_free(d);
  return errors;
}

