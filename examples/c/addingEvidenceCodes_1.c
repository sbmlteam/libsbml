/**
 * \file    addingEvidenceCodes_1.c
 * \brief   adds controlled vocabulary terms to a reaction in a model
 * \author  Sarah Keating
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <stdio.h>
#include <sbml/SBMLTypes.h>

#include <sbml/annotation/CVTerm.h>

int
main (int argc, char *argv[])
{

  SBMLDocument_t* d;
  Model_t* m;
  unsigned int  errors, n;
  Reaction_t *r;

  if (argc != 3)
  {
      printf("\n"
         "  usage: addingEvidenceCodes_1 <input-filename> <output-filename>\n"
         "  Adds controlled vocabulary term to a reaction\n"        
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
    n =  Model_getNumReactions(m);
    
    if (n <= 0)
    {
      printf( "Model has no reactions.\n Cannot add CV terms\n");
    }
    else
    {      
      CVTerm_t *cv1, *cv2;
      r = Model_getReaction(m, 0);

      /* check that the reaction has a metaid
       * no CVTerms will be added if there is no metaid to reference
       */
      if (SBase_isSetMetaId((SBase_t*)r))
        SBase_setMetaId((SBase_t*)r, "metaid_0000052");

      cv1 = CVTerm_createWithQualifierType(BIOLOGICAL_QUALIFIER);
      CVTerm_setBiologicalQualifierType(cv1, BQB_IS_DESCRIBED_BY);
      CVTerm_addResource(cv1, "urn:miriam:obo.eco:ECO%3A0000183");

      SBase_addCVTerm((SBase_t*)r, cv1);

      cv2 = CVTerm_createWithQualifierType(BIOLOGICAL_QUALIFIER);
      CVTerm_setBiologicalQualifierType(cv2, BQB_IS);
      CVTerm_addResource(cv2, "urn:miriam:kegg.reaction:R00756");
      CVTerm_addResource(cv2, "urn:miriam:reactome:REACT_736");
      
      SBase_addCVTerm((SBase_t*)r, cv2);

      writeSBML(d, argv[2]);
    }
  }

  SBMLDocument_free(d);
  return errors;
}
