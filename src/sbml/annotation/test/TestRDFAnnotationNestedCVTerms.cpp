/**
 * \file    TestRDFAnnotationNestedCVTermNestedCVTerms.cpp
 * \brief   Tests for reading/writing nested annotation
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
static Model *m31;
static Model *m32;
static SBMLDocument* d;
static SBMLDocument* d31;
static SBMLDocument* d32;

extern char *TestDataDirectory;

/* 
 * tests the results from rdf annotations
 */



void
RDFAnnotationNestedCVTerm_setup (void)
{
  char *filename = safe_strcat(TestDataDirectory, "annotationNested-l2v5.xml");
  char *filename31 = safe_strcat(TestDataDirectory, "annotationNested-l3v1.xml");
  char *filename32 = safe_strcat(TestDataDirectory, "annotationNested-l3v2.xml");

  // The following will return a pointer to a new SBMLDocument.
  d = readSBML(filename);
  m = d->getModel();

  d31 = readSBML(filename31);
  m31 = d31->getModel();
  
  d32 = readSBML(filename32);
  m32 = d32->getModel();

  safe_free(filename);
  safe_free(filename31);
  safe_free(filename32);

}


void
RDFAnnotationNestedCVTerm_teardown (void)
{
  delete d;
  delete d31;
  delete d32;
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

START_TEST (test_RDFAnnotationNestedCVTerm_parseCVTerms)
{
  XMLNode* node = RDFAnnotationParser::parseCVTerms(m->getCompartment(0));

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
  fail_unless(XMLNode_getNumChildren(Bag) == 5);

  const XMLNode_t * li = XMLNode_getChild(Bag, 0);
  fail_unless(!strcmp(XMLNode_getName(li), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li) == 0);

  const XMLNode_t * li1 = XMLNode_getChild(Bag, 1);
  fail_unless(!strcmp(XMLNode_getName(li1), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li1), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li1), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li1) == 0);

  const XMLNode_t * li2 = XMLNode_getChild(Bag, 2);
  fail_unless(!strcmp(XMLNode_getName(li2), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li2), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li2), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li2) == 0);

  const XMLNode_t * li3 = XMLNode_getChild(Bag, 3);
  fail_unless(!strcmp(XMLNode_getName(li3), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li3), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li3), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li3) == 0);

  const XMLNode_t * hasPart1 = XMLNode_getChild(Bag, 4);
  fail_unless(!strcmp(XMLNode_getName(hasPart1), "hasPart"));
  fail_unless(!strcmp(XMLNode_getPrefix(hasPart1), "bqbiol"));
  fail_unless(XMLNode_getNumChildren(hasPart1) == 1);

  const XMLNode_t * Bag1 = XMLNode_getChild(hasPart1, 0);
  fail_unless(!strcmp(XMLNode_getName(Bag1), "Bag"));
  fail_unless(!strcmp(XMLNode_getPrefix(Bag1), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(Bag1), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(Bag1) == 1);

  const XMLNode_t * li4 = XMLNode_getChild(Bag1, 0);
  fail_unless(!strcmp(XMLNode_getName(li4), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li4), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li4), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li4) == 0);

  delete node;
}
END_TEST


START_TEST(test_RDFAnnotationNestedCVTerm_reading)
{
  Compartment *c = m->getCompartment(1);

  fail_unless(c->getNumCVTerms() == 1);

  CVTerm * cv = c->getCVTerm(0);

  fail_unless(cv->getNumResources() == 1);
  fail_unless(cv->getResourceURI(0) == "top");
  fail_unless(cv->getNumNestedCVTerms() == 1);

  const CVTerm * cv1 = cv->getNestedCVTerm(0);

  fail_unless(cv1->getNumResources() == 1);
  fail_unless(cv1->getResourceURI(0) == "nest");
  fail_unless(cv1->getNumNestedCVTerms() == 1);

  const CVTerm * cv2 = cv1->getNestedCVTerm(0);

  fail_unless(cv2->getNumResources() == 1);
  fail_unless(cv2->getResourceURI(0) == "nest_nest");
  fail_unless(cv2->getNumNestedCVTerms() == 0);
}
END_TEST


START_TEST(test_RDFAnnotationNestedCVTerm_writing)
{
 const char * annot =
    "<compartment metaid=\"_4\" id=\"B\">\n"
    "  <annotation>\n"
		"    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"      <rdf:Description rdf:about=\"#_4\">\n"
		"        <bqbiol:is>\n"
		"          <rdf:Bag>\n"
		"            <rdf:li rdf:resource=\"top\"/>\n"
		"            <bqbiol:hasPart>\n"
		"              <rdf:Bag>\n"
		"                <rdf:li rdf:resource=\"nest\"/>\n"
		"              </rdf:Bag>\n"
		"            </bqbiol:hasPart>\n"
		"          </rdf:Bag>\n"
		"        </bqbiol:is>\n"
		"      </rdf:Description>\n"
		"    </rdf:RDF>\n"
    "  </annotation>\n"
    "</compartment>";

  Compartment *c = m->getCompartment(2);

  fail_unless(c->getNumCVTerms() == 0);

  CVTerm * cv = new CVTerm(BIOLOGICAL_QUALIFIER);
  cv->setBiologicalQualifierType(BQB_IS);
  cv->addResource("top");

  CVTerm * cv1 = new CVTerm(BIOLOGICAL_QUALIFIER);
  cv1->setBiologicalQualifierType(BQB_HAS_PART);
  cv1->addResource("nest");
  cv->addNestedCVTerm(cv1);

  delete cv1;

  c->addCVTerm(cv);

  delete cv;

  char * produced = c->toSBML();

  fail_unless(equals(annot, produced));

  safe_free(produced);
}
END_TEST

START_TEST (test_RDFAnnotationNestedCVTerm_dcterms)
{
  XMLNode* node = RDFAnnotationParser::parseModelHistory(m);

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
  fail_unless(XMLNode_getNumChildren(desc) == 3);

  const XMLNode_t * creator = XMLNode_getChild(desc, 0);
  fail_unless(!strcmp(XMLNode_getName(creator), "creator"));
  fail_unless(!strcmp(XMLNode_getPrefix(creator), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(creator), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(creator) == 1);

  const XMLNode_t * Bag = XMLNode_getChild(creator, 0);
  fail_unless(!strcmp(XMLNode_getName(Bag), "Bag"));
  fail_unless(!strcmp(XMLNode_getPrefix(Bag), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(Bag), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(Bag) == 1);

  const XMLNode_t * li = XMLNode_getChild(Bag, 0);
  fail_unless(!strcmp(XMLNode_getName(li), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li) == 3);


  const XMLNode_t *N = XMLNode_getChild(li, 0);
  fail_unless(!strcmp(XMLNode_getName(N), "N"));
  fail_unless(!strcmp(XMLNode_getPrefix(N), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(N), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(N) == 2);

  const XMLNode_t *Family = XMLNode_getChild(N, 0);
  fail_unless(!strcmp(XMLNode_getName(Family), "Family"));
  fail_unless(!strcmp(XMLNode_getPrefix(Family), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(Family), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(Family) == 1);


  const XMLNode_t *Given = XMLNode_getChild(N, 1);
  fail_unless(!strcmp(XMLNode_getName(Given), "Given"));
  fail_unless(!strcmp(XMLNode_getPrefix(Given), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(Given), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(Given) == 1);


  const XMLNode_t *EMAIL = XMLNode_getChild(li, 1);
  fail_unless(!strcmp(XMLNode_getName(EMAIL), "EMAIL"));
  fail_unless(!strcmp(XMLNode_getPrefix(EMAIL), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(EMAIL), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(EMAIL) == 1);

  const XMLNode_t *ORG = XMLNode_getChild(li, 2);
  fail_unless(!strcmp(XMLNode_getName(ORG), "ORG"));
  fail_unless(!strcmp(XMLNode_getPrefix(ORG), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(ORG), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(ORG) == 1);

  const XMLNode_t *Orgname = XMLNode_getChild(ORG, 0);
  fail_unless(!strcmp(XMLNode_getName(Orgname), "Orgname"));
  fail_unless(!strcmp(XMLNode_getPrefix(Orgname), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(Orgname), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(Orgname) == 1);

  const XMLNode_t * created = XMLNode_getChild(desc, 1);
  fail_unless(!strcmp(XMLNode_getName(created), "created"));
  fail_unless(!strcmp(XMLNode_getPrefix(created), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(created), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(created) == 1);

  const XMLNode_t * cr_date = XMLNode_getChild(created, 0);
  fail_unless(!strcmp(XMLNode_getName(cr_date), "W3CDTF"));
  fail_unless(!strcmp(XMLNode_getPrefix(cr_date), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(cr_date), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(cr_date) == 1);

  const XMLNode_t * modified = XMLNode_getChild(desc, 2);
  fail_unless(!strcmp(XMLNode_getName(modified), "modified"));
  fail_unless(!strcmp(XMLNode_getPrefix(modified), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(modified), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(modified) == 1);

  const XMLNode_t * mo_date = XMLNode_getChild(created, 0);
  fail_unless(!strcmp(XMLNode_getName(mo_date), "W3CDTF"));
  fail_unless(!strcmp(XMLNode_getPrefix(mo_date), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(mo_date), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(mo_date) == 1);


  delete node;

}
END_TEST


START_TEST(test_RDFAnnotationNestedCVTerm_writeDC_creator)
{
  const char * expected =
    "<model metaid=\"_000001\" id=\"EPSP_Edelstein\" name=\"Edelstein1996_EPSP_AChEvent\">\n"
    "  <annotation>\n"
		"    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
    "      <rdf:Description rdf:about=\"#_000001\">\n"
    "        <dcterms:creator>\n"
		"          <rdf:Bag>\n"
		"            <rdf:li rdf:parseType=\"Resource\">\n"
    "              <vCard:N rdf:parseType=\"Resource\">\n"
		"                <vCard:Family>Le Novere</vCard:Family>\n"
		"                <vCard:Given>Nicolas</vCard:Given>\n"
		"              </vCard:N>\n"
		"              <vCard:EMAIL>lenov@ebi.ac.uk</vCard:EMAIL>\n"
		"              <vCard:ORG rdf:parseType=\"Resource\">\n"
		"                <vCard:Orgname>EMBL-EBI</vCard:Orgname>\n"
		"              </vCard:ORG>\n"
		"            </rdf:li>\n"
		"          </rdf:Bag>\n"
		"        </dcterms:creator>\n"
		"        <dcterms:created rdf:parseType=\"Resource\">\n"
		"          <dcterms:W3CDTF>2005-02-02T14:56:11</dcterms:W3CDTF>\n"
		"        </dcterms:created>\n"
		"        <dcterms:modified rdf:parseType=\"Resource\">\n"
		"          <dcterms:W3CDTF>2006-05-30T10:46:02</dcterms:W3CDTF>\n"
		"        </dcterms:modified>\n"
		"      </rdf:Description>\n"
		"    </rdf:RDF>\n"
    "  </annotation>\n"
    "</model>";

  Compartment * c = m->removeCompartment(2);
  delete c;

  c = m->removeCompartment(1);
  delete c;

  c = m->removeCompartment(0);
  delete c;

  char * produced = m->toSBML();

  fail_unless(equals(expected, produced));

  safe_free(produced);
}
END_TEST


START_TEST (test_RDFAnnotationNestedCVTerm_parseCVTerms_31)
{
  XMLNode* node = RDFAnnotationParser::parseCVTerms(m31->getCompartment(0));

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
  fail_unless(XMLNode_getNumChildren(Bag) == 5);

  const XMLNode_t * li = XMLNode_getChild(Bag, 0);
  fail_unless(!strcmp(XMLNode_getName(li), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li) == 0);

  const XMLNode_t * li1 = XMLNode_getChild(Bag, 1);
  fail_unless(!strcmp(XMLNode_getName(li1), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li1), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li1), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li1) == 0);

  const XMLNode_t * li2 = XMLNode_getChild(Bag, 2);
  fail_unless(!strcmp(XMLNode_getName(li2), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li2), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li2), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li2) == 0);

  const XMLNode_t * li3 = XMLNode_getChild(Bag, 3);
  fail_unless(!strcmp(XMLNode_getName(li3), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li3), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li3), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li3) == 0);

  const XMLNode_t * hasPart1 = XMLNode_getChild(Bag, 4);
  fail_unless(!strcmp(XMLNode_getName(hasPart1), "hasPart"));
  fail_unless(!strcmp(XMLNode_getPrefix(hasPart1), "bqbiol"));
  fail_unless(XMLNode_getNumChildren(hasPart1) == 1);

  const XMLNode_t * Bag1 = XMLNode_getChild(hasPart1, 0);
  fail_unless(!strcmp(XMLNode_getName(Bag1), "Bag"));
  fail_unless(!strcmp(XMLNode_getPrefix(Bag1), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(Bag1), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(Bag1) == 1);

  const XMLNode_t * li4 = XMLNode_getChild(Bag1, 0);
  fail_unless(!strcmp(XMLNode_getName(li4), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li4), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li4), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li4) == 0);

  delete node;
}
END_TEST


START_TEST(test_RDFAnnotationNestedCVTerm_reading_31)
{
  Compartment *c = m31->getCompartment(1);

  fail_unless(c->getNumCVTerms() == 1);

  CVTerm * cv = c->getCVTerm(0);

  fail_unless(cv->getNumResources() == 1);
  fail_unless(cv->getResourceURI(0) == "top");
  fail_unless(cv->getNumNestedCVTerms() == 1);

  const CVTerm * cv1 = cv->getNestedCVTerm(0);

  fail_unless(cv1->getNumResources() == 1);
  fail_unless(cv1->getResourceURI(0) == "nest");
  fail_unless(cv1->getNumNestedCVTerms() == 1);

  const CVTerm * cv2 = cv1->getNestedCVTerm(0);

  fail_unless(cv2->getNumResources() == 1);
  fail_unless(cv2->getResourceURI(0) == "nest_nest");
  fail_unless(cv2->getNumNestedCVTerms() == 0);
}
END_TEST


START_TEST(test_RDFAnnotationNestedCVTerm_writing_31)
{
 const char * annot =
    "<compartment metaid=\"_4\" id=\"B\" constant=\"true\">\n"
    "  <annotation>\n"
		"    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"      <rdf:Description rdf:about=\"#_4\">\n"
		"        <bqbiol:is>\n"
		"          <rdf:Bag>\n"
		"            <rdf:li rdf:resource=\"top\"/>\n"
		"            <bqbiol:hasPart>\n"
		"              <rdf:Bag>\n"
		"                <rdf:li rdf:resource=\"nest\"/>\n"
		"              </rdf:Bag>\n"
		"            </bqbiol:hasPart>\n"
		"          </rdf:Bag>\n"
		"        </bqbiol:is>\n"
		"      </rdf:Description>\n"
		"    </rdf:RDF>\n"
    "  </annotation>\n"
    "</compartment>";

  Compartment *c = m31->getCompartment(2);

  fail_unless(c->getNumCVTerms() == 0);

  CVTerm * cv = new CVTerm(BIOLOGICAL_QUALIFIER);
  cv->setBiologicalQualifierType(BQB_IS);
  cv->addResource("top");

  CVTerm * cv1 = new CVTerm(BIOLOGICAL_QUALIFIER);
  cv1->setBiologicalQualifierType(BQB_HAS_PART);
  cv1->addResource("nest");
  cv->addNestedCVTerm(cv1);

  delete cv1;

  c->addCVTerm(cv);

  delete cv;

  char * produced = c->toSBML();

  fail_unless(equals(annot, produced));

  safe_free(produced);
}
END_TEST

START_TEST (test_RDFAnnotationNestedCVTerm_dcterms_31)
{
  XMLNode* node = RDFAnnotationParser::parseModelHistory(m31);

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
  fail_unless(XMLNode_getNumChildren(desc) == 3);

  const XMLNode_t * creator = XMLNode_getChild(desc, 0);
  fail_unless(!strcmp(XMLNode_getName(creator), "creator"));
  fail_unless(!strcmp(XMLNode_getPrefix(creator), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(creator), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(creator) == 1);

  const XMLNode_t * Bag = XMLNode_getChild(creator, 0);
  fail_unless(!strcmp(XMLNode_getName(Bag), "Bag"));
  fail_unless(!strcmp(XMLNode_getPrefix(Bag), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(Bag), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(Bag) == 1);

  const XMLNode_t * li = XMLNode_getChild(Bag, 0);
  fail_unless(!strcmp(XMLNode_getName(li), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li) == 3);


  const XMLNode_t *N = XMLNode_getChild(li, 0);
  fail_unless(!strcmp(XMLNode_getName(N), "N"));
  fail_unless(!strcmp(XMLNode_getPrefix(N), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(N), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(N) == 2);

  const XMLNode_t *Family = XMLNode_getChild(N, 0);
  fail_unless(!strcmp(XMLNode_getName(Family), "Family"));
  fail_unless(!strcmp(XMLNode_getPrefix(Family), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(Family), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(Family) == 1);


  const XMLNode_t *Given = XMLNode_getChild(N, 1);
  fail_unless(!strcmp(XMLNode_getName(Given), "Given"));
  fail_unless(!strcmp(XMLNode_getPrefix(Given), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(Given), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(Given) == 1);


  const XMLNode_t *EMAIL = XMLNode_getChild(li, 1);
  fail_unless(!strcmp(XMLNode_getName(EMAIL), "EMAIL"));
  fail_unless(!strcmp(XMLNode_getPrefix(EMAIL), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(EMAIL), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(EMAIL) == 1);

  const XMLNode_t *ORG = XMLNode_getChild(li, 2);
  fail_unless(!strcmp(XMLNode_getName(ORG), "ORG"));
  fail_unless(!strcmp(XMLNode_getPrefix(ORG), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(ORG), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(ORG) == 1);

  const XMLNode_t *Orgname = XMLNode_getChild(ORG, 0);
  fail_unless(!strcmp(XMLNode_getName(Orgname), "Orgname"));
  fail_unless(!strcmp(XMLNode_getPrefix(Orgname), "vCard"));
  fail_unless(!strcmp(XMLNode_getURI(Orgname), "http://www.w3.org/2001/vcard-rdf/3.0#"));
  fail_unless(XMLNode_getNumChildren(Orgname) == 1);

  const XMLNode_t * created = XMLNode_getChild(desc, 1);
  fail_unless(!strcmp(XMLNode_getName(created), "created"));
  fail_unless(!strcmp(XMLNode_getPrefix(created), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(created), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(created) == 1);

  const XMLNode_t * cr_date = XMLNode_getChild(created, 0);
  fail_unless(!strcmp(XMLNode_getName(cr_date), "W3CDTF"));
  fail_unless(!strcmp(XMLNode_getPrefix(cr_date), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(cr_date), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(cr_date) == 1);

  const XMLNode_t * modified = XMLNode_getChild(desc, 2);
  fail_unless(!strcmp(XMLNode_getName(modified), "modified"));
  fail_unless(!strcmp(XMLNode_getPrefix(modified), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(modified), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(modified) == 1);

  const XMLNode_t * mo_date = XMLNode_getChild(created, 0);
  fail_unless(!strcmp(XMLNode_getName(mo_date), "W3CDTF"));
  fail_unless(!strcmp(XMLNode_getPrefix(mo_date), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(mo_date), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(mo_date) == 1);


  delete node;

}
END_TEST


START_TEST(test_RDFAnnotationNestedCVTerm_writeDC_creator_31)
{
  const char * expected =
    "<model metaid=\"_000001\" id=\"EPSP_Edelstein\" name=\"Edelstein1996_EPSP_AChEvent\">\n"
    "  <annotation>\n"
		"    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
    "      <rdf:Description rdf:about=\"#_000001\">\n"
    "        <dcterms:creator>\n"
		"          <rdf:Bag>\n"
		"            <rdf:li rdf:parseType=\"Resource\">\n"
    "              <vCard:N rdf:parseType=\"Resource\">\n"
		"                <vCard:Family>Le Novere</vCard:Family>\n"
		"                <vCard:Given>Nicolas</vCard:Given>\n"
		"              </vCard:N>\n"
		"              <vCard:EMAIL>lenov@ebi.ac.uk</vCard:EMAIL>\n"
		"              <vCard:ORG rdf:parseType=\"Resource\">\n"
		"                <vCard:Orgname>EMBL-EBI</vCard:Orgname>\n"
		"              </vCard:ORG>\n"
		"            </rdf:li>\n"
		"          </rdf:Bag>\n"
		"        </dcterms:creator>\n"
		"        <dcterms:created rdf:parseType=\"Resource\">\n"
		"          <dcterms:W3CDTF>2005-02-02T14:56:11</dcterms:W3CDTF>\n"
		"        </dcterms:created>\n"
		"        <dcterms:modified rdf:parseType=\"Resource\">\n"
		"          <dcterms:W3CDTF>2006-05-30T10:46:02</dcterms:W3CDTF>\n"
		"        </dcterms:modified>\n"
		"      </rdf:Description>\n"
		"    </rdf:RDF>\n"
    "  </annotation>\n"
    "</model>";

  Compartment * c = m31->removeCompartment(2);
  delete c;

  c = m31->removeCompartment(1);
  delete c;

  c = m31->removeCompartment(0);
  delete c;

  char * produced = m31->toSBML();

  fail_unless(equals(expected, produced));

  safe_free(produced);
}
END_TEST


START_TEST (test_RDFAnnotationNestedCVTerm_parseCVTerms_32)
{
  XMLNode* node = RDFAnnotationParser::parseCVTerms(m32->getCompartment(0));

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
  fail_unless(XMLNode_getNumChildren(Bag) == 5);

  const XMLNode_t * li = XMLNode_getChild(Bag, 0);
  fail_unless(!strcmp(XMLNode_getName(li), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li) == 0);

  const XMLNode_t * li1 = XMLNode_getChild(Bag, 1);
  fail_unless(!strcmp(XMLNode_getName(li1), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li1), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li1), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li1) == 0);

  const XMLNode_t * li2 = XMLNode_getChild(Bag, 2);
  fail_unless(!strcmp(XMLNode_getName(li2), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li2), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li2), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li2) == 0);

  const XMLNode_t * li3 = XMLNode_getChild(Bag, 3);
  fail_unless(!strcmp(XMLNode_getName(li3), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li3), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li3), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li3) == 0);

  const XMLNode_t * hasPart1 = XMLNode_getChild(Bag, 4);
  fail_unless(!strcmp(XMLNode_getName(hasPart1), "hasPart"));
  fail_unless(!strcmp(XMLNode_getPrefix(hasPart1), "bqbiol"));
  fail_unless(XMLNode_getNumChildren(hasPart1) == 1);

  const XMLNode_t * Bag1 = XMLNode_getChild(hasPart1, 0);
  fail_unless(!strcmp(XMLNode_getName(Bag1), "Bag"));
  fail_unless(!strcmp(XMLNode_getPrefix(Bag1), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(Bag1), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(Bag1) == 1);

  const XMLNode_t * li4 = XMLNode_getChild(Bag1, 0);
  fail_unless(!strcmp(XMLNode_getName(li4), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li4), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li4), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li4) == 0);

  delete node;
}
END_TEST


START_TEST(test_RDFAnnotationNestedCVTerm_reading_32)
{
  Compartment *c = m32->getCompartment(1);

  fail_unless(c->getNumCVTerms() == 1);

  CVTerm * cv = c->getCVTerm(0);

  fail_unless(cv->getNumResources() == 1);
  fail_unless(cv->getResourceURI(0) == "top");
  fail_unless(cv->getNumNestedCVTerms() == 1);

  const CVTerm * cv1 = cv->getNestedCVTerm(0);

  fail_unless(cv1->getNumResources() == 1);
  fail_unless(cv1->getResourceURI(0) == "nest");
  fail_unless(cv1->getNumNestedCVTerms() == 1);

  const CVTerm * cv2 = cv1->getNestedCVTerm(0);

  fail_unless(cv2->getNumResources() == 1);
  fail_unless(cv2->getResourceURI(0) == "nest_nest");
  fail_unless(cv2->getNumNestedCVTerms() == 0);
}
END_TEST


START_TEST(test_RDFAnnotationNestedCVTerm_writing_32)
{
 const char * annot =
    "<compartment metaid=\"_4\" id=\"B\" constant=\"true\">\n"
    "  <annotation>\n"
		"    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"      <rdf:Description rdf:about=\"#_4\">\n"
		"        <bqbiol:is>\n"
		"          <rdf:Bag>\n"
		"            <rdf:li rdf:resource=\"top\"/>\n"
		"            <bqbiol:hasPart>\n"
		"              <rdf:Bag>\n"
		"                <rdf:li rdf:resource=\"nest\"/>\n"
		"              </rdf:Bag>\n"
		"            </bqbiol:hasPart>\n"
		"          </rdf:Bag>\n"
		"        </bqbiol:is>\n"
		"      </rdf:Description>\n"
		"    </rdf:RDF>\n"
    "  </annotation>\n"
    "</compartment>";

  Compartment *c = m32->getCompartment(2);

  fail_unless(c->getNumCVTerms() == 0);

  CVTerm * cv = new CVTerm(BIOLOGICAL_QUALIFIER);
  cv->setBiologicalQualifierType(BQB_IS);
  cv->addResource("top");

  CVTerm * cv1 = new CVTerm(BIOLOGICAL_QUALIFIER);
  cv1->setBiologicalQualifierType(BQB_HAS_PART);
  cv1->addResource("nest");
  cv->addNestedCVTerm(cv1);

  delete cv1;

  c->addCVTerm(cv);

  delete cv;

  char * produced = c->toSBML();

  fail_unless(equals(annot, produced));

  safe_free(produced);
}
END_TEST

START_TEST (test_RDFAnnotationNestedCVTerm_dcterms_32)
{
  XMLNode* node = RDFAnnotationParser::parseModelHistory(m32);

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
  fail_unless(XMLNode_getNumChildren(desc) == 3);

  const XMLNode_t * creator = XMLNode_getChild(desc, 0);
  fail_unless(!strcmp(XMLNode_getName(creator), "creator"));
  fail_unless(!strcmp(XMLNode_getPrefix(creator), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(creator), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(creator) == 1);

  const XMLNode_t * Bag = XMLNode_getChild(creator, 0);
  fail_unless(!strcmp(XMLNode_getName(Bag), "Bag"));
  fail_unless(!strcmp(XMLNode_getPrefix(Bag), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(Bag), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(Bag) == 1);

  const XMLNode_t * li = XMLNode_getChild(Bag, 0);
  fail_unless(!strcmp(XMLNode_getName(li), "li"));
  fail_unless(!strcmp(XMLNode_getPrefix(li), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(li), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(li) == 3);


  const XMLNode_t *N = XMLNode_getChild(li, 0);
  fail_unless(!strcmp(XMLNode_getName(N), "hasName"));
  fail_unless(!strcmp(XMLNode_getPrefix(N), "vCard4"));
  fail_unless(!strcmp(XMLNode_getURI(N), "http://www.w3.org/2006/vcard/ns#"));
  fail_unless(XMLNode_getNumChildren(N) == 2);

  const XMLNode_t *Family = XMLNode_getChild(N, 0);
  fail_unless(!strcmp(XMLNode_getName(Family), "family-name"));
  fail_unless(!strcmp(XMLNode_getPrefix(Family), "vCard4"));
  fail_unless(!strcmp(XMLNode_getURI(Family), "http://www.w3.org/2006/vcard/ns#"));
  fail_unless(XMLNode_getNumChildren(Family) == 1);


  const XMLNode_t *Given = XMLNode_getChild(N, 1);
  fail_unless(!strcmp(XMLNode_getName(Given), "given-name"));
  fail_unless(!strcmp(XMLNode_getPrefix(Given), "vCard4"));
  fail_unless(!strcmp(XMLNode_getURI(Given), "http://www.w3.org/2006/vcard/ns#"));
  fail_unless(XMLNode_getNumChildren(Given) == 1);


  const XMLNode_t *EMAIL = XMLNode_getChild(li, 1);
  fail_unless(!strcmp(XMLNode_getName(EMAIL), "hasEmail"));
  fail_unless(!strcmp(XMLNode_getPrefix(EMAIL), "vCard4"));
  fail_unless(!strcmp(XMLNode_getURI(EMAIL), "http://www.w3.org/2006/vcard/ns#"));
  fail_unless(XMLNode_getNumChildren(EMAIL) == 1);

  const XMLNode_t *ORG = XMLNode_getChild(li, 2);
  fail_unless(!strcmp(XMLNode_getName(ORG), "organization-name"));
  fail_unless(!strcmp(XMLNode_getPrefix(ORG), "vCard4"));
  fail_unless(!strcmp(XMLNode_getURI(ORG), "http://www.w3.org/2006/vcard/ns#"));
  fail_unless(XMLNode_getNumChildren(ORG) == 1);


  const XMLNode_t * created = XMLNode_getChild(desc, 1);
  fail_unless(!strcmp(XMLNode_getName(created), "created"));
  fail_unless(!strcmp(XMLNode_getPrefix(created), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(created), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(created) == 1);

  const XMLNode_t * cr_date = XMLNode_getChild(created, 0);
  fail_unless(!strcmp(XMLNode_getName(cr_date), "W3CDTF"));
  fail_unless(!strcmp(XMLNode_getPrefix(cr_date), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(cr_date), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(cr_date) == 1);

  const XMLNode_t * modified = XMLNode_getChild(desc, 2);
  fail_unless(!strcmp(XMLNode_getName(modified), "modified"));
  fail_unless(!strcmp(XMLNode_getPrefix(modified), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(modified), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(modified) == 1);

  const XMLNode_t * mo_date = XMLNode_getChild(created, 0);
  fail_unless(!strcmp(XMLNode_getName(mo_date), "W3CDTF"));
  fail_unless(!strcmp(XMLNode_getPrefix(mo_date), "dcterms"));
  fail_unless(!strcmp(XMLNode_getURI(mo_date), "http://purl.org/dc/terms/"));
  fail_unless(XMLNode_getNumChildren(mo_date) == 1);


  delete node;

}
END_TEST


START_TEST(test_RDFAnnotationNestedCVTerm_writeDC_creator_32)
{
  const char * expected =
    "<model metaid=\"_000001\" id=\"EPSP_Edelstein\" name=\"Edelstein1996_EPSP_AChEvent\">\n"
    "  <annotation>\n"
		"    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
    "      <rdf:Description rdf:about=\"#_000001\">\n"
    "        <dcterms:creator>\n"
		"          <rdf:Bag>\n"
		"            <rdf:li rdf:parseType=\"Resource\">\n"
    "              <vCard:N rdf:parseType=\"Resource\">\n"
		"                <vCard:Family>Le Novere</vCard:Family>\n"
		"                <vCard:Given>Nicolas</vCard:Given>\n"
		"              </vCard:N>\n"
		"              <vCard:EMAIL>lenov@ebi.ac.uk</vCard:EMAIL>\n"
		"              <vCard:ORG rdf:parseType=\"Resource\">\n"
		"                <vCard:Orgname>EMBL-EBI</vCard:Orgname>\n"
		"              </vCard:ORG>\n"
		"            </rdf:li>\n"
		"          </rdf:Bag>\n"
		"        </dcterms:creator>\n"
		"        <dcterms:created rdf:parseType=\"Resource\">\n"
		"          <dcterms:W3CDTF>2005-02-02T14:56:11</dcterms:W3CDTF>\n"
		"        </dcterms:created>\n"
		"        <dcterms:modified rdf:parseType=\"Resource\">\n"
		"          <dcterms:W3CDTF>2006-05-30T10:46:02</dcterms:W3CDTF>\n"
		"        </dcterms:modified>\n"
		"      </rdf:Description>\n"
		"    </rdf:RDF>\n"
    "  </annotation>\n"
    "  <listOfCompartments/>\n"
    "</model>";

  Compartment * c = m32->removeCompartment(2);
  delete c;

  c = m32->removeCompartment(1);
  delete c;

  c = m32->removeCompartment(0);
  delete c;

  char * produced = m32->toSBML();

  fail_unless(equals(expected, produced));

  safe_free(produced);
}
END_TEST


Suite *
create_suite_RDFAnnotationNestedCVTerm (void)
{
  Suite *suite = suite_create("RDFAnnotationNestedCVTerm");
  TCase *tcase = tcase_create("RDFAnnotationNestedCVTerm");

  tcase_add_checked_fixture(tcase,
                            RDFAnnotationNestedCVTerm_setup,
                            RDFAnnotationNestedCVTerm_teardown);

  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_parseCVTerms );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_reading );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_writing );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_dcterms );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_writeDC_creator);

  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_parseCVTerms_31 );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_reading_31 );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_writing_31 );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_dcterms_31 );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_writeDC_creator_31);

  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_parseCVTerms_32 );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_reading_32 );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_writing_32 );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_dcterms_32 );
  tcase_add_test(tcase, test_RDFAnnotationNestedCVTerm_writeDC_creator_32);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

