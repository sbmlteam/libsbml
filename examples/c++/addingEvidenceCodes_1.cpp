/**
 * \file    addingEvidenceCodes_1.cpp
 * \brief   adds controlled vocabulary terms to a reaction in a model
 * \author  Sarah Keating
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <iostream>
#include <sbml/SBMLTypes.h>

#include <sbml/annotation/CVTerm.h>
using namespace std;
LIBSBML_CPP_NAMESPACE_USE

int
main (int argc, char *argv[])
{

  SBMLDocument* d;
  unsigned int  errors, n;
  Reaction *r;

  if (argc != 3)
  {
    cout << endl
         << "  usage: addingEvidenceCodes_1 <input-filename> <output-filename>" << endl
         << "  Adds controlled vocabulary term to a reaction"        << endl
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
  
    n = d->getModel()->getNumReactions();
    
    if (n <= 0)
    {
      cout << "Model has no reactions.\n Cannot add CV terms\n";
    }
    else
    {
      r = d->getModel()->getReaction(0);

      /* check that the reaction has a metaid
       * no CVTerms will be added if there is no metaid to reference
       */
      if (!r->isSetMetaId())
        r->setMetaId("metaid_0000052");

      CVTerm * cv1 = new CVTerm(BIOLOGICAL_QUALIFIER);
      cv1->setBiologicalQualifierType(BQB_IS_DESCRIBED_BY);
      cv1->addResource("urn:miriam:obo.eco:ECO%3A0000183");

      r->addCVTerm(cv1);

      CVTerm * cv2 = new CVTerm(BIOLOGICAL_QUALIFIER);
      cv2->setBiologicalQualifierType(BQB_IS);
      cv2->addResource("urn:miriam:kegg.reaction:R00756");
      cv2->addResource("urn:miriam:reactome:REACT_736");

      r->addCVTerm(cv2);

      writeSBML(d, argv[2]);
    }
  }

  delete d;
  return errors;
}
