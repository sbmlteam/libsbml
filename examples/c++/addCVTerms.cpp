/**
 * \file    addCVTerms.cpp
 * \brief   adds controlled vocabulary terms to a species in a model
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
#include <sbml/annotation/CVTerm.h>
using namespace std;


int
main (int argc, char *argv[])
{

  SBMLDocument* d;
  unsigned int  errors, n;
  Species *s;

  if (argc != 3)
  {
    cout << endl
         << "  usage: addCVTerms <input-filename> <output-filename>" << endl
         << "  Adds controlled vocabulary term to a species"          << endl
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
  
    n = d->getModel()->getNumSpecies();
    
    if (n <= 0)
    {
      cout << "Model has no species.\n Cannot add CV terms\n";
    }
    else
    {
      s = d->getModel()->getSpecies(0);

      CVTerm *cv = new CVTerm();
      cv->setQualifierType(BIOLOGICAL_QUALIFIER);
      cv->setBiologicalQualifierType(BQB_IS_VERSION_OF);
      cv->addResource("http://www.geneontology.org/#GO:0005892");

      CVTerm *cv2 = new CVTerm();
      cv2->setQualifierType(BIOLOGICAL_QUALIFIER);
      cv2->setBiologicalQualifierType(BQB_IS);
      cv2->addResource("http://www.geneontology.org/#GO:0005895");

      CVTerm *cv1 = new CVTerm();
      cv1->setQualifierType(BIOLOGICAL_QUALIFIER);
      cv1->setBiologicalQualifierType(BQB_IS_VERSION_OF);
      cv1->addResource("http://www.ebi.ac.uk/interpro/#IPR002394");
      
      s->addCVTerm(cv);
      s->addCVTerm(cv2);
      s->addCVTerm(cv1);

      writeSBML(d, argv[2]);
    }
  }

  delete d;
  return errors;
}
