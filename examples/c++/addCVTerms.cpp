/**
 * \file    addCVTerms.cpp
 * \brief   adds controlled vocabulary terms to a species in a model
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
