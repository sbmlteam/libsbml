/**
 * \file    TestUnusualRDFAnnotation.cpp
 * \brief   fomula units data unit tests
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/SBMLTypeCodes.h>

#include <sbml/annotation/RDFAnnotation.h>
#include <sbml/annotation/ModelHistory.h>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

CK_CPPSTART


static Model *m;
static SBMLDocument* d;

extern char *TestDataDirectory;

/* 
 * tests the results from rdf annotations
 */



void
UnusualRDFAnnotation_setup (void)
{
  char *filename = safe_strcat(TestDataDirectory, "set_annot_test.xml");

  // The following will return a pointer to a new SBMLDocument.
  d = readSBML(filename);
  m = d->getModel();

  free(filename);
}


void
UnusualRDFAnnotation_teardown (void)
{
  delete d;
}

static bool
equals (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}


START_TEST(test_read)
{
  char *filename = safe_strcat(TestDataDirectory, "set_annot_test.xml");
  SBMLDocument *doc = readSBML(filename);

  // check we get warning about nested annotation not supported
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getError(0)->getErrorId() == 99407);

  delete doc;
  free(filename);
}
END_TEST


START_TEST(test_roundtrip)
{
  //  fails as our parsing puts resources for is in one bag and lists hasProperty as nested

  const char * expected = "<species metaid=\"metaid_0000036\" id=\"SAM\" compartment=\"cytosol\" initialConcentration=\"0.01\">\n"
    "  <annotation>\n"
    "    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vcard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
    "      <rdf:Description rdf:about=\"metaid_0000036\">\n"
    "        <bqbiol:is>\n"
    "          <rdf:Bag>\n"
    "            <rdf:li rdf:resource=\"http://identifiers.org/chebi/CHEBI:59789\"/>\n"
    "            <bqbiol:hasProperty>\n"
    "              <rdf:Bag>\n"
    "                <rdf:li rdf:resource=\"http://amas/match_score/by_name/1.0\"/>\n"
    "              </rdf:Bag>\n"
    "            </bqbiol:hasProperty>\n"
    "            <rdf:li rdf:resource=\"http://identifiers.org/chebi/CHEBI:15414\"/>\n"
    "            <bqbiol:hasProperty>\n"
    "              <rdf:Bag>\n"
    "                <rdf:li rdf:resource=\"http://amas/match_score/by_name/1.0\"/>\n"
    "              </rdf:Bag>\n"
    "            </bqbiol:hasProperty>\n"
    "         </rdf:Bag>\n"
    "        </bqbiol:is>\n"
    "      </rdf:Description>\n"
    "    </rdf:RDF>\n"
    "  </annotation>\n"
    "</species>";

  Species *s = m->getSpecies(0);

  fail_unless(equals(expected, s->toSBML()));

}
END_TEST


START_TEST(test_hasCVTerms)
{
  Species *s = m->getSpecies(0);

  // parsing means we have the following structure
  fail_unless(s->getNumCVTerms() == 1);
  CVTerm *cv = s->getCVTerm(0);
  fail_unless(cv->getBiologicalQualifierType() == BQB_IS);
  fail_unless(cv->getNumResources() == 2);
  fail_unless(cv->getResourceURI(0) == "http://identifiers.org/chebi/CHEBI:59789");
  fail_unless(cv->getResourceURI(1) == "http://identifiers.org/chebi/CHEBI:15414");

  fail_unless(cv->getNumNestedCVTerms() == 2);

  CVTerm *cv1 = cv->getNestedCVTerm(0);
  fail_unless(cv1->getBiologicalQualifierType() == BQB_HAS_PROPERTY);
  fail_unless(cv1->getNumResources() == 1);
  fail_unless(cv1->getResourceURI(0) == "http://amas/match_score/by_name/1.0");

  CVTerm *cv2 = cv->getNestedCVTerm(1);
  fail_unless(cv2->getBiologicalQualifierType() == BQB_HAS_PROPERTY);
  fail_unless(cv2->getNumResources() == 1);
  fail_unless(cv2->getResourceURI(0) == "http://amas/match_score/by_name/1.0");


}
END_TEST


START_TEST(test_read_XMLNode_from_file)
{
  char *filename = safe_strcat(TestDataDirectory, "just_annotation.xml");
  XMLNode *node = XMLNode::readXMLNodeFromFile(filename);
  fail_unless(node != NULL);

  fail_unless(node->getNumChildren() == 1);

  const XMLNode_t* rdf = XMLNode_getChild(node, 0);

  fail_unless(!strcmp(XMLNode_getName(rdf), "RDF"));
  fail_unless(!strcmp(XMLNode_getPrefix(rdf), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(rdf), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(rdf) == 1);

  const XMLNode_t* desc = XMLNode_getChild(rdf, 0);

  fail_unless(!strcmp(XMLNode_getName(desc), "Description"));
  fail_unless(!strcmp(XMLNode_getPrefix(desc), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(desc), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(desc) == 1);

  const XMLNode_t * is1 = XMLNode_getChild(desc, 0);
  fail_unless(!strcmp(XMLNode_getName(is1), "is"));
  fail_unless(!strcmp(XMLNode_getPrefix(is1), "bqbiol"));
  fail_unless(XMLNode_getNumChildren(is1) == 1);

  const XMLNode_t * Bag = XMLNode_getChild(is1, 0);
  fail_unless(!strcmp(XMLNode_getName(Bag), "Bag"));
  fail_unless(!strcmp(XMLNode_getPrefix(Bag), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(Bag), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(Bag) == 4);

  const XMLNode_t * li = XMLNode_getChild(Bag, 0);
  fail_unless(!strcmp(XMLNode_getName(li), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(li->getAttrValue(0) == "http://identifiers.org/chebi/CHEBI:59789");
  fail_unless(XMLNode_getNumChildren(li) == 0);

  const XMLNode_t * hasProp1 = XMLNode_getChild(Bag, 1);
  fail_unless(!strcmp(XMLNode_getName(hasProp1), "hasProperty"));
  fail_unless(!strcmp(XMLNode_getPrefix(hasProp1), "bqbiol"));
  fail_unless(XMLNode_getNumChildren(hasProp1) == 1);

  const XMLNode_t * Bag1 = XMLNode_getChild(hasProp1, 0);
  fail_unless(!strcmp(XMLNode_getName(Bag1), "Bag"));
  fail_unless(!strcmp(XMLNode_getPrefix(Bag1), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(Bag1), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(Bag1) == 1);

  const XMLNode_t * li_Bag1 = XMLNode_getChild(Bag1, 0);
  fail_unless(!strcmp(XMLNode_getName(li_Bag1), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li_Bag1), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li_Bag1), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li_Bag1) == 0);


  const XMLNode_t * li2 = XMLNode_getChild(Bag, 2);
  fail_unless(!strcmp(XMLNode_getName(li2), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li2), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li2), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li2) == 0);

  free(filename);
  delete(node);
}
END_TEST


START_TEST(test_set_annotation)
{
  char* original = writeSBMLToString(d);

  char *filename = safe_strcat(TestDataDirectory, "just_annotation.xml");
  XMLNode *node = XMLNode::readXMLNodeFromFile(filename);
  Species *s = m->getSpecies(0);
  s->unsetAnnotation();

  fail_unless(s->isSetAnnotation() == false);
  fail_unless(s->getNumCVTerms() == 0);

  s->setAnnotation(node);
  fail_unless(s->isSetAnnotation() == true);

  fail_unless(equals(original, writeSBMLToString(d)));

}
END_TEST




Suite *
create_suite_UnusualRDFAnnotation (void)
{
  Suite *suite = suite_create("UnusualRDFAnnotation");
  TCase *tcase = tcase_create("UnusualRDFAnnotation");

  tcase_add_checked_fixture(tcase,
    UnusualRDFAnnotation_setup,
    UnusualRDFAnnotation_teardown);


  //tcase_add_test(tcase, test_roundtrip);
  tcase_add_test(tcase, test_read );
   //tcase_add_test(tcase, test_hasCVTerms);
   //tcase_add_test(tcase, test_read_XMLNode_from_file);
   //tcase_add_test(tcase, test_set_annotation);


  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

