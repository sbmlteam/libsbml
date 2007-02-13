/**
 * \file    addModelHistory.cpp
 * \brief   adds Model History to a model
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
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
    c->setOrganisation("University of Hertfordshire");

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
