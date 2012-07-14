/**
 * \file    addCVTerms.c
 * \brief   adds controlled vocabulary terms to a species in a model
 * \author  Sarah Keating
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <stdio.h>
#include <sbml/SBMLTypes.h>

#include <sbml/xml/XMLNode.h>
#include <sbml/annotation/CVTerm.h>

int
main (int argc, char *argv[])
{

  SBMLDocument_t* d;
  Model_t* m;
  unsigned int  errors, n;
  Species_t *s;

  if (argc != 3)
  {
    printf("\n"
         "  usage: addCVTerms <input-filename> <output-filename>\n"
         "  Adds controlled vocabulary term to a species\n"
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
    m = SBMLDocument_getModel(d);
    n =  Model_getNumSpecies(m);
    
    if (n <= 0)
    {
      printf( "Model has no species.\n Cannot add CV terms\n");
    }
    else
    {
      CVTerm_t *cv, *cv1, *cv2;
      s = Model_getSpecies(m ,0);

      cv = CVTerm_createWithQualifierType(BIOLOGICAL_QUALIFIER);
      CVTerm_setBiologicalQualifierType(cv, BQB_IS_VERSION_OF);
      CVTerm_addResource(cv, "http://www.geneontology.org/#GO:0005892");

      cv2 = CVTerm_createWithQualifierType(BIOLOGICAL_QUALIFIER);
      CVTerm_setBiologicalQualifierType(cv2, BQB_IS);
      CVTerm_addResource(cv2, "http://www.geneontology.org/#GO:0005895");

      cv1 = CVTerm_createWithQualifierType(BIOLOGICAL_QUALIFIER);
      CVTerm_setBiologicalQualifierType(cv1, BQB_IS_VERSION_OF);
      CVTerm_addResource(cv1, "http://www.ebi.ac.uk/interpro/#IPR002394");

      SBase_addCVTerm((SBase_t*)s, cv);
      SBase_addCVTerm((SBase_t*)s, cv2);
      SBase_addCVTerm((SBase_t*)s, cv1);

      writeSBML(d, argv[2]);
    }
  }

  SBMLDocument_free(d);
  return errors;
}

