/**
 * \file    TestRDFAnnotation.cpp
 * \brief   fomula units data unit tests
 * \author  Ben Bornstein
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

Model *m4;
Model *m31;
SBMLDocument* d4;
SBMLDocument* d31;

extern char *TestDataDirectory;

/* 
 * tests the results from rdf annotations
 */

void
RDFAnnotationV4_setup (void)
{
  char *filename = safe_strcat(TestDataDirectory, "annotationVcard4.xml");
  char *filename31 = safe_strcat(TestDataDirectory, "annotationVcard4-l3v1.xml");

  // The following will return a pointer to a new SBMLDocument.
  d4 = readSBML(filename);
  m4 = d4->getModel();

  d31 = readSBML(filename);
  m31 = d31->getModel();

  free(filename);
  free(filename31);
}


void
RDFAnnotationV4_teardown (void)
{
  delete d4;
  delete d31;
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



START_TEST (test_RDFAnnotationV4_getModelHistory)
{
  ModelHistory * history = m4->getModelHistory();

  fail_unless(history != NULL);

  ModelCreator * mc = (ModelCreator * )(history->getListCreators()->get(0));

  fail_unless(!strcmp(ModelCreator_getFamilyName(mc), "Hucka"));
  fail_unless(!strcmp(ModelCreator_getGivenName(mc), "Mike"));
  fail_unless(!strcmp(ModelCreator_getEmail(mc), "mhucka@caltech.edu"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(mc), "BNMC"));

  ModelCreator * mc1 = (ModelCreator * )(history->getListCreators()->get(1));

  fail_unless(!strcmp(ModelCreator_getFamilyName(mc1), "Sarah Keating"));
  fail_unless(!strcmp(ModelCreator_getName(mc1), "Sarah Keating"));
  fail_unless(!strcmp(ModelCreator_getGivenName(mc1), "Sarah Keating"));
  fail_unless(!strcmp(ModelCreator_getEmail(mc1), "skeating@caltech.edu"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(mc1), "UH"));

  Date * date = history->getCreatedDate();
  fail_unless(Date_getYear(date) == 2005);
  fail_unless(Date_getMonth(date) == 2);
  fail_unless(Date_getDay(date) == 2);
  fail_unless(Date_getHour(date) == 14);
  fail_unless(Date_getMinute(date) == 56);
  fail_unless(Date_getSecond(date) == 11);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);
  fail_unless(!strcmp(Date_getDateAsString(date), "2005-02-02T14:56:11Z"));

  date = history->getModifiedDate();
  fail_unless(Date_getYear(date) == 2006);
  fail_unless(Date_getMonth(date) == 5);
  fail_unless(Date_getDay(date) == 30);
  fail_unless(Date_getHour(date) == 10);
  fail_unless(Date_getMinute(date) == 46);
  fail_unless(Date_getSecond(date) == 2);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);
  fail_unless(!strcmp(Date_getDateAsString(date), "2006-05-30T10:46:02Z"));

  date = history->getModifiedDate(1);
  fail_unless(Date_getYear(date) == 2007);
  fail_unless(Date_getMonth(date) == 1);
  fail_unless(Date_getDay(date) == 16);
  fail_unless(Date_getHour(date) == 15);
  fail_unless(Date_getMinute(date) == 31);
  fail_unless(Date_getSecond(date) == 52);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);
  fail_unless(!strcmp(Date_getDateAsString(date), "2007-01-16T15:31:52Z"));
}
END_TEST


START_TEST (test_RDFAnnotationV4_modelWithHistoryAndCVTerms)
{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setFamilyName("Keating");
  c->setGivenName("Sarah");

  h->addCreator(c);

  Date *d = new Date(2008, 11, 17, 18, 37, 0, 0, 0, 0);
  h->setCreatedDate(d);
  h->setModifiedDate(d);

  m4->unsetModelHistory();

  m4->setModelHistory(h);

  CVTerm *cv = new CVTerm();
  cv->setQualifierType(BIOLOGICAL_QUALIFIER);
  cv->setBiologicalQualifierType(BQB_IS_VERSION_OF);
  cv->addResource("http://www.geneontology.org/#GO:0005892");

  m4->addCVTerm(cv);
  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m4);

  const char * expected =
    "<annotation>\n"
		"  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"    <rdf:Description rdf:about=\"#_000001\">\n"
		"      <dcterms:creator>\n"
		"        <rdf:Bag>\n"
		"          <rdf:li rdf:parseType=\"Resource\">\n"
		"            <vCard4:hasName rdf:parseType=\"Resource\">\n"
		"              <vCard4:family-name>Keating</vCard4:family-name>\n"
		"              <vCard4:given-name>Sarah</vCard4:given-name>\n"
		"            </vCard4:hasName>\n"
		"          </rdf:li>\n"
		"        </rdf:Bag>\n"
		"      </dcterms:creator>\n"
    "      <dcterms:created rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2008-11-17T18:37:00Z</dcterms:W3CDTF>\n"
    "      </dcterms:created>\n"
    "      <dcterms:modified rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2008-11-17T18:37:00Z</dcterms:W3CDTF>\n"
    "      </dcterms:modified>\n"
		"      <bqbiol:isVersionOf>\n"
		"        <rdf:Bag>\n"
		"          <rdf:li rdf:resource=\"http://www.geneontology.org/#GO:0005892\"/>\n"
		"        </rdf:Bag>\n"
		"      </bqbiol:isVersionOf>\n"
		"    </rdf:Description>\n"
		"  </rdf:RDF>\n"
    "</annotation>";

  if (ann != NULL) 
  {
    fail_unless( equals(expected, ann->toXMLString().c_str()) );
  }
  else
  {
    fail("parseModelHistory failed");
  }

  delete c;
  delete d;
  delete h;
  delete cv;
  delete ann;
}
END_TEST


START_TEST (test_RDFAnnotationV4_modelWithHistoryWithCharacterReference)

{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setFamilyName("Dr&#228;ger");
  c->setGivenName("Andreas");
  c->setEmail("a@bcd");
  c->setOrganization("UT");

  h->addCreator(c);
  Date * d = new Date(2005, 2, 2, 14, 56, 11);
  h->setCreatedDate(d);
  h->addModifiedDate(d);

  m4->unsetModelHistory();

  m4->setModelHistory(h);

  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m4);

  const char * expected =
    "<annotation>\n"
		"  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"    <rdf:Description rdf:about=\"#_000001\">\n"
		"      <dcterms:creator>\n"
		"        <rdf:Bag>\n"
		"          <rdf:li rdf:parseType=\"Resource\">\n"
		"            <vCard4:hasName rdf:parseType=\"Resource\">\n"
		"              <vCard4:family-name>Dr&#228;ger</vCard4:family-name>\n"
		"              <vCard4:given-name>Andreas</vCard4:given-name>\n"
		"            </vCard4:hasName>\n"
		"            <vCard4:hasEmail>a@bcd</vCard4:hasEmail>\n"
		"            <vCard4:organization-name>UT</vCard4:organization-name>\n"
		"          </rdf:li>\n"
		"        </rdf:Bag>\n"
		"      </dcterms:creator>\n"
		"      <dcterms:created rdf:parseType=\"Resource\">\n"
		"        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
		"      </dcterms:created>\n"
		"      <dcterms:modified rdf:parseType=\"Resource\">\n"
		"        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
		"      </dcterms:modified>\n"
		"    </rdf:Description>\n"
		"  </rdf:RDF>\n"
    "</annotation>";

  fail_unless( equals(expected, ann->toXMLString().c_str()) );

  delete c;
  delete d;
  delete h;
  delete ann;
}
END_TEST

START_TEST (test_RDFAnnotationV4_modelWithHistoryWithOneName)

{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setName("John Smith");
  c->setEmail("a@bcd");
  c->setOrganization("UT");

  h->addCreator(c);
  Date * d = new Date(2005, 2, 2, 14, 56, 11);
  h->setCreatedDate(d);
  h->addModifiedDate(d);

  m4->unsetModelHistory();

  m4->setModelHistory(h);

  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m4);

  const char * expected =
    "<annotation>\n"
		"  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"    <rdf:Description rdf:about=\"#_000001\">\n"
		"      <dcterms:creator>\n"
		"        <rdf:Bag>\n"
		"          <rdf:li rdf:parseType=\"Resource\">\n"
    "            <vCard4:fn>\n"
    "              <vCard4:text>John Smith</vCard4:text>\n"
    "            </vCard4:fn>\n"
		"            <vCard4:hasEmail>a@bcd</vCard4:hasEmail>\n"
		"            <vCard4:organization-name>UT</vCard4:organization-name>\n"
		"          </rdf:li>\n"
		"        </rdf:Bag>\n"
		"      </dcterms:creator>\n"
		"      <dcterms:created rdf:parseType=\"Resource\">\n"
		"        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
		"      </dcterms:created>\n"
		"      <dcterms:modified rdf:parseType=\"Resource\">\n"
		"        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
		"      </dcterms:modified>\n"
		"    </rdf:Description>\n"
		"  </rdf:RDF>\n"
    "</annotation>";

  fail_unless( equals(expected, ann->toXMLString().c_str()) );

  delete c;
  delete d;
  delete h;
  delete ann;
}
END_TEST


START_TEST (test_RDFAnnotationV4_getModelHistory_31)
{
  ModelHistory * history = m31->getModelHistory();

  fail_unless(history != NULL);

  ModelCreator * mc = (ModelCreator * )(history->getListCreators()->get(0));

  fail_unless(!strcmp(ModelCreator_getFamilyName(mc), "Hucka"));
  fail_unless(!strcmp(ModelCreator_getGivenName(mc), "Mike"));
  fail_unless(!strcmp(ModelCreator_getEmail(mc), "mhucka@caltech.edu"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(mc), "BNMC"));

  ModelCreator * mc1 = (ModelCreator * )(history->getListCreators()->get(1));

  fail_unless(!strcmp(ModelCreator_getFamilyName(mc1), "Sarah Keating"));
  fail_unless(!strcmp(ModelCreator_getName(mc1), "Sarah Keating"));
  fail_unless(!strcmp(ModelCreator_getGivenName(mc1), "Sarah Keating"));
  fail_unless(!strcmp(ModelCreator_getEmail(mc1), "skeating@caltech.edu"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(mc1), "UH"));

  Date * date = history->getCreatedDate();
  fail_unless(Date_getYear(date) == 2005);
  fail_unless(Date_getMonth(date) == 2);
  fail_unless(Date_getDay(date) == 2);
  fail_unless(Date_getHour(date) == 14);
  fail_unless(Date_getMinute(date) == 56);
  fail_unless(Date_getSecond(date) == 11);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);
  fail_unless(!strcmp(Date_getDateAsString(date), "2005-02-02T14:56:11Z"));

  date = history->getModifiedDate();
  fail_unless(Date_getYear(date) == 2006);
  fail_unless(Date_getMonth(date) == 5);
  fail_unless(Date_getDay(date) == 30);
  fail_unless(Date_getHour(date) == 10);
  fail_unless(Date_getMinute(date) == 46);
  fail_unless(Date_getSecond(date) == 2);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);
  fail_unless(!strcmp(Date_getDateAsString(date), "2006-05-30T10:46:02Z"));

  date = history->getModifiedDate(1);
  fail_unless(Date_getYear(date) == 2007);
  fail_unless(Date_getMonth(date) == 1);
  fail_unless(Date_getDay(date) == 16);
  fail_unless(Date_getHour(date) == 15);
  fail_unless(Date_getMinute(date) == 31);
  fail_unless(Date_getSecond(date) == 52);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);
  fail_unless(!strcmp(Date_getDateAsString(date), "2007-01-16T15:31:52Z"));
}
END_TEST


START_TEST (test_RDFAnnotationV4_modelWithHistoryAndCVTerms_31)
{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setFamilyName("Keating");
  c->setGivenName("Sarah");

  h->addCreator(c);

  Date *d = new Date(2008, 11, 17, 18, 37, 0, 0, 0, 0);
  h->setCreatedDate(d);
  h->setModifiedDate(d);

  m31->unsetModelHistory();

  m31->setModelHistory(h);

  CVTerm *cv = new CVTerm();
  cv->setQualifierType(BIOLOGICAL_QUALIFIER);
  cv->setBiologicalQualifierType(BQB_IS_VERSION_OF);
  cv->addResource("http://www.geneontology.org/#GO:0005892");

  m31->addCVTerm(cv);
  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m31);

  const char * expected =
    "<annotation>\n"
		"  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"    <rdf:Description rdf:about=\"#_000001\">\n"
		"      <dcterms:creator>\n"
		"        <rdf:Bag>\n"
		"          <rdf:li rdf:parseType=\"Resource\">\n"
		"            <vCard4:hasName rdf:parseType=\"Resource\">\n"
		"              <vCard4:family-name>Keating</vCard4:family-name>\n"
		"              <vCard4:given-name>Sarah</vCard4:given-name>\n"
		"            </vCard4:hasName>\n"
		"          </rdf:li>\n"
		"        </rdf:Bag>\n"
		"      </dcterms:creator>\n"
    "      <dcterms:created rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2008-11-17T18:37:00Z</dcterms:W3CDTF>\n"
    "      </dcterms:created>\n"
    "      <dcterms:modified rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2008-11-17T18:37:00Z</dcterms:W3CDTF>\n"
    "      </dcterms:modified>\n"
		"      <bqbiol:isVersionOf>\n"
		"        <rdf:Bag>\n"
		"          <rdf:li rdf:resource=\"http://www.geneontology.org/#GO:0005892\"/>\n"
		"        </rdf:Bag>\n"
		"      </bqbiol:isVersionOf>\n"
		"    </rdf:Description>\n"
		"  </rdf:RDF>\n"
    "</annotation>";

  if (ann != NULL) 
  {
    fail_unless( equals(expected, ann->toXMLString().c_str()) );
  }
  else
  {
    fail("parseModelHistory failed");
  }

  delete c;
  delete d;
  delete h;
  delete cv;
  delete ann;
}
END_TEST


START_TEST (test_RDFAnnotationV4_modelWithHistoryWithCharacterReference_31)

{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setFamilyName("Dr&#228;ger");
  c->setGivenName("Andreas");
  c->setEmail("a@bcd");
  c->setOrganization("UT");

  h->addCreator(c);
  Date * d = new Date(2005, 2, 2, 14, 56, 11);
  h->setCreatedDate(d);
  h->addModifiedDate(d);

  m31->unsetModelHistory();

  m31->setModelHistory(h);

  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m31);

  const char * expected =
    "<annotation>\n"
		"  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"    <rdf:Description rdf:about=\"#_000001\">\n"
		"      <dcterms:creator>\n"
		"        <rdf:Bag>\n"
		"          <rdf:li rdf:parseType=\"Resource\">\n"
		"            <vCard4:hasName rdf:parseType=\"Resource\">\n"
		"              <vCard4:family-name>Dr&#228;ger</vCard4:family-name>\n"
		"              <vCard4:given-name>Andreas</vCard4:given-name>\n"
		"            </vCard4:hasName>\n"
		"            <vCard4:hasEmail>a@bcd</vCard4:hasEmail>\n"
		"            <vCard4:organization-name>UT</vCard4:organization-name>\n"
		"          </rdf:li>\n"
		"        </rdf:Bag>\n"
		"      </dcterms:creator>\n"
		"      <dcterms:created rdf:parseType=\"Resource\">\n"
		"        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
		"      </dcterms:created>\n"
		"      <dcterms:modified rdf:parseType=\"Resource\">\n"
		"        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
		"      </dcterms:modified>\n"
		"    </rdf:Description>\n"
		"  </rdf:RDF>\n"
    "</annotation>";

  fail_unless( equals(expected, ann->toXMLString().c_str()) );

  delete c;
  delete d;
  delete h;
  delete ann;
}
END_TEST

START_TEST (test_RDFAnnotationV4_modelWithHistoryWithOneName_31)

{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setName("John Smith");
  c->setEmail("a@bcd");
  c->setOrganization("UT");

  h->addCreator(c);
  Date * d = new Date(2005, 2, 2, 14, 56, 11);
  h->setCreatedDate(d);
  h->addModifiedDate(d);

  m31->unsetModelHistory();

  m31->setModelHistory(h);

  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m31);

  const char * expected =
    "<annotation>\n"
		"  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"    <rdf:Description rdf:about=\"#_000001\">\n"
		"      <dcterms:creator>\n"
		"        <rdf:Bag>\n"
		"          <rdf:li rdf:parseType=\"Resource\">\n"
    "            <vCard4:fn>\n"
    "              <vCard4:text>John Smith</vCard4:text>\n"
    "            </vCard4:fn>\n"
		"            <vCard4:hasEmail>a@bcd</vCard4:hasEmail>\n"
		"            <vCard4:organization-name>UT</vCard4:organization-name>\n"
		"          </rdf:li>\n"
		"        </rdf:Bag>\n"
		"      </dcterms:creator>\n"
		"      <dcterms:created rdf:parseType=\"Resource\">\n"
		"        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
		"      </dcterms:created>\n"
		"      <dcterms:modified rdf:parseType=\"Resource\">\n"
		"        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
		"      </dcterms:modified>\n"
		"    </rdf:Description>\n"
		"  </rdf:RDF>\n"
    "</annotation>";

  fail_unless( equals(expected, ann->toXMLString().c_str()) );

  delete c;
  delete d;
  delete h;
  delete ann;
}
END_TEST


START_TEST(test_RDFAnnotationV4_modelWithHistoryVcard4_1)

{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setFamilyName("Smith");
  c->setGivenName("John");
  c->setEmail("a@bcd");
  c->setOrganization("UT");
  c->setUseSingleName(true);

  h->addCreator(c);
  Date * d = new Date(2005, 2, 2, 14, 56, 11);
  h->setCreatedDate(d);
  h->addModifiedDate(d);

  m31->unsetModelHistory();

  m31->setModelHistory(h);

  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m31);

  const char * expected =
    "<annotation>\n"
    "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
    "    <rdf:Description rdf:about=\"#_000001\">\n"
    "      <dcterms:creator>\n"
    "        <rdf:Bag>\n"
    "          <rdf:li rdf:parseType=\"Resource\">\n"
    "            <vCard4:fn>\n"
    "              <vCard4:text>John Smith</vCard4:text>\n"
    "            </vCard4:fn>\n"
    "            <vCard4:hasEmail>a@bcd</vCard4:hasEmail>\n"
    "            <vCard4:organization-name>UT</vCard4:organization-name>\n"
    "          </rdf:li>\n"
    "        </rdf:Bag>\n"
    "      </dcterms:creator>\n"
    "      <dcterms:created rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
    "      </dcterms:created>\n"
    "      <dcterms:modified rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
    "      </dcterms:modified>\n"
    "    </rdf:Description>\n"
    "  </rdf:RDF>\n"
    "</annotation>";

  fail_unless(equals(expected, ann->toXMLString().c_str()));

  delete c;
  delete d;
  delete h;
  delete ann;
}
END_TEST


START_TEST(test_RDFAnnotationV4_modelWithHistoryVcard4_2)

{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setFamilyName("Smith");
  c->setGivenName("John");
  c->setEmail("a@bcd");
  c->setOrganization("UT");
  c->setUseSingleName(false);

  h->addCreator(c);
  Date * d = new Date(2005, 2, 2, 14, 56, 11);
  h->setCreatedDate(d);
  h->addModifiedDate(d);

  m31->unsetModelHistory();

  m31->setModelHistory(h);

  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m31);

  const char * expected =
    "<annotation>\n"
    "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
    "    <rdf:Description rdf:about=\"#_000001\">\n"
    "      <dcterms:creator>\n"
    "        <rdf:Bag>\n"
    "          <rdf:li rdf:parseType=\"Resource\">\n"
    "            <vCard4:hasName rdf:parseType=\"Resource\">\n"
    "              <vCard4:family-name>Smith</vCard4:family-name>\n"
    "              <vCard4:given-name>John</vCard4:given-name>\n"
    "            </vCard4:hasName>\n"
    "            <vCard4:hasEmail>a@bcd</vCard4:hasEmail>\n"
    "            <vCard4:organization-name>UT</vCard4:organization-name>\n"
    "          </rdf:li>\n"
    "        </rdf:Bag>\n"
    "      </dcterms:creator>\n"
    "      <dcterms:created rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
    "      </dcterms:created>\n"
    "      <dcterms:modified rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
    "      </dcterms:modified>\n"
    "    </rdf:Description>\n"
    "  </rdf:RDF>\n"
    "</annotation>";

  fail_unless(equals(expected, ann->toXMLString().c_str()));

  delete c;
  delete d;
  delete h;
  delete ann;
}
END_TEST


START_TEST(test_RDFAnnotationV4_modelWithHistoryVcard4_3)

{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setName("John Smith");
  c->setEmail("a@bcd");
  c->setOrganization("UT");
  c->setUseSingleName(false);

  h->addCreator(c);
  Date * d = new Date(2005, 2, 2, 14, 56, 11);
  h->setCreatedDate(d);
  h->addModifiedDate(d);

  m31->unsetModelHistory();

  m31->setModelHistory(h);

  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m31);

  const char * expected =
    "<annotation>\n"
    "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
    "    <rdf:Description rdf:about=\"#_000001\">\n"
    "      <dcterms:creator>\n"
    "        <rdf:Bag>\n"
    "          <rdf:li rdf:parseType=\"Resource\">\n"
    "            <vCard4:hasName rdf:parseType=\"Resource\">\n"
    "              <vCard4:family-name>Smith</vCard4:family-name>\n"
    "              <vCard4:given-name>John</vCard4:given-name>\n"
    "            </vCard4:hasName>\n"
    "            <vCard4:hasEmail>a@bcd</vCard4:hasEmail>\n"
    "            <vCard4:organization-name>UT</vCard4:organization-name>\n"
    "          </rdf:li>\n"
    "        </rdf:Bag>\n"
    "      </dcterms:creator>\n"
    "      <dcterms:created rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
    "      </dcterms:created>\n"
    "      <dcterms:modified rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
    "      </dcterms:modified>\n"
    "    </rdf:Description>\n"
    "  </rdf:RDF>\n"
    "</annotation>";

  fail_unless(equals(expected, ann->toXMLString().c_str()));

  delete c;
  delete d;
  delete h;
  delete ann;
}
END_TEST


START_TEST(test_RDFAnnotationV4_modelWithHistoryVcard4_4)

{
  ModelHistory * h = new ModelHistory();

  ModelCreator *c = new ModelCreator();
  c->setName("John Smith");
  c->setEmail("a@bcd");
  c->setOrganization("UT");
  c->setUseSingleName(true);

  h->addCreator(c);
  Date * d = new Date(2005, 2, 2, 14, 56, 11);
  h->setCreatedDate(d);
  h->addModifiedDate(d);

  m31->unsetModelHistory();

  m31->setModelHistory(h);

  XMLNode *ann = RDFAnnotationParser::parseModelHistory(m31);

  const char * expected =
    "<annotation>\n"
    "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:vCard4=\"http://www.w3.org/2006/vcard/ns#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
    "    <rdf:Description rdf:about=\"#_000001\">\n"
    "      <dcterms:creator>\n"
    "        <rdf:Bag>\n"
    "          <rdf:li rdf:parseType=\"Resource\">\n"
    "            <vCard4:fn>\n"
    "              <vCard4:text>John Smith</vCard4:text>\n"
    "            </vCard4:fn>\n"
    "            <vCard4:hasEmail>a@bcd</vCard4:hasEmail>\n"
    "            <vCard4:organization-name>UT</vCard4:organization-name>\n"
    "          </rdf:li>\n"
    "        </rdf:Bag>\n"
    "      </dcterms:creator>\n"
    "      <dcterms:created rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
    "      </dcterms:created>\n"
    "      <dcterms:modified rdf:parseType=\"Resource\">\n"
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n"
    "      </dcterms:modified>\n"
    "    </rdf:Description>\n"
    "  </rdf:RDF>\n"
    "</annotation>";

  fail_unless(equals(expected, ann->toXMLString().c_str()));

  delete c;
  delete d;
  delete h;
  delete ann;
}
END_TEST


Suite *
create_suite_RDFAnnotationV4 (void)
{
  Suite *suite = suite_create("RDFAnnotationV4");
  TCase *tcase = tcase_create("RDFAnnotationV4");

  tcase_add_checked_fixture(tcase,
                            RDFAnnotationV4_setup,
                            RDFAnnotationV4_teardown);

  tcase_add_test(tcase, test_RDFAnnotationV4_getModelHistory );
  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryAndCVTerms );
  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryWithCharacterReference);
  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryWithOneName);
  
  tcase_add_test(tcase, test_RDFAnnotationV4_getModelHistory_31 );
  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryAndCVTerms_31 );
  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryWithCharacterReference_31);
  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryWithOneName_31);

  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryVcard4_1);
  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryVcard4_2);
  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryVcard4_3);
  tcase_add_test(tcase, test_RDFAnnotationV4_modelWithHistoryVcard4_4);
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

