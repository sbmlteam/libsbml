/**
 * \file    addModelHistory.cpp
 * \brief   adds Model History to a model
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>
#include <sbml/SBMLTypes.h>

#include <sbml/xml/XMLNode.h>
#include <sbml/annotation/ModelHistory.h>
using namespace std;


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

    h->addCreator(c);

    Date * date = new Date("1999-11-13T06:54:32");
    Date * date2 = new Date("2007-11-31T06:54:00-02:00");
   
    h->setCreatedDate(date);
    h->setModifiedDate(date2);

    d->getModel()->setModelHistory(h);

  
    writeSBML(d, argv[2]);
  }

  delete d;
  return errors;
}
