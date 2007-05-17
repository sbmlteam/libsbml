/**
 * \file    TestRDFAnnotation.cpp
 * \brief   fomula units data unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
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


#include "sbml/common/common.h"
#include "sbml/common/extern.h"

#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"

#include "sbml/SBMLDocument.h"
#include "sbml/Model.h"
#include "sbml/SBMLTypeCodes.h"

#include "RDFAnnotation.h"
#include "ModelHistory.h"

#include <check.h>

static char*         S;

ostringstream*   OSS;
XMLOutputStream* XOS;
static Model *m;
static SBMLDocument* d;

extern char *TestDataDirectory;

/* 
 * tests the results from rdf annotations
 */
CK_CPPSTART
static 

void
RDFAnnotation_setup (void)
{
  d = new SBMLDocument();
  S = 0;
  OSS = new ostringstream;
  XOS = new XMLOutputStream(*OSS);
 
  char *filename = safe_strcat(TestDataDirectory, "annotation.xml");

  d = readSBML(filename);
  m = d->getModel();


}


void
RDFAnnotation_teardown (void)
{
//  delete d;
  free(S);

  delete OSS;
  delete XOS;
}

static bool
equals1 (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}


static bool
equals (const char* expected)
{
  return equals1(expected, OSS->str().c_str());
}

START_TEST (test_RDFAnnotation_getModelHistory)
{
  ModelHistory * history = m->getModelHistory();

  fail_unless(history != NULL);

  ModelCreator * mc = (ModelCreator * )(history->getCreator()->get(0));

  fail_unless(!strcmp(ModelCreator_getFamilyName(mc), "Le Novere"));
  fail_unless(!strcmp(ModelCreator_getGivenName(mc), "Nicolas"));
  fail_unless(!strcmp(ModelCreator_getEmail(mc), "lenov@ebi.ac.uk"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(mc), "EMBL-EBI"));

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

  delete history;
}
END_TEST


START_TEST (test_RDFAnnotation_parseModelHistory)
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
  fail_unless(!strcmp(XMLNode_getPrefix(creator), "dc"));
  fail_unless(!strcmp(XMLNode_getURI(creator), "http://purl.org/dc/elements/1.1/"));
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

START_TEST (test_RDFAnnotation_parseCVTerms)
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

  const XMLNode_t * is = XMLNode_getChild(desc, 0);
  fail_unless(!strcmp(XMLNode_getName(is), "is"));
  fail_unless(!strcmp(XMLNode_getPrefix(is), "bqbiol"));
 // fail_unless(!strcmp(XMLNode_getURI(is), "http://biomodels.net/biology-qualifiers/"));
  fail_unless(XMLNode_getNumChildren(is) == 1);

  const XMLNode_t * Bag = XMLNode_getChild(is, 0);
  fail_unless(!strcmp(XMLNode_getName(Bag), "Bag"));
  fail_unless(!strcmp(XMLNode_getPrefix(Bag), "rdf"));
  fail_unless(!strcmp(XMLNode_getURI(Bag), "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
  fail_unless(XMLNode_getNumChildren(Bag) == 4);

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

  delete node;
}
END_TEST

START_TEST (test_RDFAnnotation_delete)
{
  XMLNode* node = RDFAnnotationParser::parseCVTerms(m->getCompartment(0));

  XMLNode* n1 = RDFAnnotationParser::deleteRDFAnnotation(node);

  const char * expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<annotation/>";

  fail_unless(n1->getNumChildren() == 0);
  fail_unless(n1->getName() == "annotation");

  n1->write(*XOS);

  fail_unless( equals(expected) );

  delete node;
}
END_TEST


START_TEST (test_RDFAnnotation_deleteWithOther)
{
  Compartment* c = m->getCompartment(1);

  XMLNode* node = c->getAnnotation();
  const char * expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<annotation>\n"
		"  <jd2:JDesignerLayout version=\"2.0\" MajorVersion=\"2\" MinorVersion=\"0\" BuildVersion=\"41\">\n"
		"    <jd2:header>\n"
		"      <jd2:VersionHeader JDesignerVersion=\"2.0\"/>\n"
		"      <jd2:ModelHeader Author=\"Mr Untitled\" ModelVersion=\"0.0\" ModelTitle=\"untitled\"/>\n"
		"      <jd2:TimeCourseDetails timeStart=\"0\" timeEnd=\"10\" numberOfPoints=\"1000\"/>\n"
		"    </jd2:header>\n"
		"  </jd2:JDesignerLayout>\n"
    "</annotation>";


  node->write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST

START_TEST (test_RDFAnnotation_recreate)
{
  Compartment* c = m->getCompartment(1);

  const char * expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<compartment id=\"A\">\n"
    "  <annotation>\n"
		"    <jd2:JDesignerLayout version=\"2.0\" MajorVersion=\"2\" MinorVersion=\"0\" BuildVersion=\"41\">\n"
		"      <jd2:header>\n"
		"        <jd2:VersionHeader JDesignerVersion=\"2.0\"/>\n"
		"        <jd2:ModelHeader Author=\"Mr Untitled\" ModelVersion=\"0.0\" ModelTitle=\"untitled\"/>\n"
		"        <jd2:TimeCourseDetails timeStart=\"0\" timeEnd=\"10\" numberOfPoints=\"1000\"/>\n"
		"      </jd2:header>\n"
		"    </jd2:JDesignerLayout>\n"
		"    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"      <rdf:Description rdf:about=\"#\">\n"
		"        <bqbiol:is>\n"
		"          <rdf:Bag>\n"
		"            <rdf:li rdf:resource=\"http://www.geneontology.org/#GO:0007274\"/>\n"
		"          </rdf:Bag>\n"
		"        </bqbiol:is>\n"
		"      </rdf:Description>\n"
		"    </rdf:RDF>\n"
    "  </annotation>\n"
    "</compartment>";


  c->write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST

START_TEST (test_RDFAnnotation_recreateFromEmpty)
{
  Compartment* c = m->getCompartment(3);

  const char * expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<compartment id=\"C\">\n"
    "  <annotation>\n"
		"    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n"
		"      <rdf:Description rdf:about=\"#\">\n"
		"        <bqbiol:is>\n"
		"          <rdf:Bag>\n"
		"            <rdf:li rdf:resource=\"http://www.geneontology.org/#GO:0007274\"/>\n"
		"          </rdf:Bag>\n"
		"        </bqbiol:is>\n"
		"      </rdf:Description>\n"
		"    </rdf:RDF>\n"
    "  </annotation>\n"
    "</compartment>";


  c->write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST


START_TEST (test_RDFAnnotation_deleteWithOutOther)
{
  Compartment* c = m->getCompartment(2);

  XMLNode* node = c->getAnnotation();
  const char * expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<annotation>\n"
		"  <jd2:JDesignerLayout version=\"2.0\" MajorVersion=\"2\" MinorVersion=\"0\" BuildVersion=\"41\">\n"
		"    <jd2:header>\n"
		"      <jd2:VersionHeader JDesignerVersion=\"2.0\"/>\n"
		"      <jd2:ModelHeader Author=\"Mr Untitled\" ModelVersion=\"0.0\" ModelTitle=\"untitled\"/>\n"
		"      <jd2:TimeCourseDetails timeStart=\"0\" timeEnd=\"10\" numberOfPoints=\"1000\"/>\n"
		"    </jd2:header>\n"
		"  </jd2:JDesignerLayout>\n"
    "</annotation>";


  node->write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST


START_TEST (test_RDFAnnotation_recreateWithOutOther)
{
  Compartment* c = m->getCompartment(2);

  const char * expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<compartment id=\"B\">\n"
    "  <annotation>\n"
		"    <jd2:JDesignerLayout version=\"2.0\" MajorVersion=\"2\" MinorVersion=\"0\" BuildVersion=\"41\">\n"
		"      <jd2:header>\n"
		"        <jd2:VersionHeader JDesignerVersion=\"2.0\"/>\n"
		"        <jd2:ModelHeader Author=\"Mr Untitled\" ModelVersion=\"0.0\" ModelTitle=\"untitled\"/>\n"
		"        <jd2:TimeCourseDetails timeStart=\"0\" timeEnd=\"10\" numberOfPoints=\"1000\"/>\n"
		"      </jd2:header>\n"
		"    </jd2:JDesignerLayout>\n"
    "  </annotation>\n"
    "</compartment>";


  c->write(*XOS);

  fail_unless( equals(expected) );

}
END_TEST

Suite *
create_suite_RDFAnnotation (void)
{
  Suite *suite = suite_create("RDFAnnotation");
  TCase *tcase = tcase_create("RDFAnnotation");

  tcase_add_checked_fixture(tcase,
                            RDFAnnotation_setup,
                            RDFAnnotation_teardown);

  tcase_add_test(tcase, test_RDFAnnotation_getModelHistory );
  tcase_add_test(tcase, test_RDFAnnotation_parseModelHistory );
  tcase_add_test(tcase, test_RDFAnnotation_parseCVTerms );
  tcase_add_test(tcase, test_RDFAnnotation_delete );
  tcase_add_test(tcase, test_RDFAnnotation_deleteWithOther );
  tcase_add_test(tcase, test_RDFAnnotation_recreate );
  tcase_add_test(tcase, test_RDFAnnotation_recreateFromEmpty );
  tcase_add_test(tcase, test_RDFAnnotation_deleteWithOutOther );
  tcase_add_test(tcase, test_RDFAnnotation_recreateWithOutOther );
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
