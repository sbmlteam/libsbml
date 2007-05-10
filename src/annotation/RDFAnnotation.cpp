/**
 * @file    RDFAnnotation.cpp
 * @brief   RDFAnnotation I/O
 * @author  Sarah Keating
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

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/SBase.h>
#include <sbml/Model.h>

#include <sbml/SBMLErrorLog.h>

#include <sbml/util/util.h>
#include <sbml/util/List.h>

#include <sbml/annotation/ModelHistory.h>
#include <sbml/annotation/RDFAnnotation.h>

/** @cond doxygen-ignore */

using namespace std;

/** @endcond doxygen-ignore */


/**
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a List of CVTerms from the annotation
 */
void 
RDFAnnotationParser::parseRDFAnnotation(XMLNode * annotation, List * CVTerms)
{

  const string&  name = annotation->getName();
  const XMLNode*  RDFTop = NULL;
  unsigned int n = 0;
  CVTerm * term;

  // need to find the RDF desciption opening annotation
  if (name == "annotation" && annotation->getNumChildren() > 0)
  {
    while (n < annotation->getNumChildren())
    {
      const string &name1 = annotation->getChild(n).getName();
      if (name1 == "RDF")
      {
	if (annotation->getChild(n).getNumChildren() > 0)
	{
	  RDFTop = &(annotation->getChild(n).getChild(0));
	  break;
	}
      }
      n++;
    }
  }

  // find qualifier nodes and create CVTerms
  
  n = 0;
  if (RDFTop)
  {
    while (n < RDFTop->getNumChildren())
    {
      const string &name2 = RDFTop->getChild(n).getPrefix();
      if (name2 == "bqbiol" || name2 == "bqmodel")
      {
        term = new CVTerm(RDFTop->getChild(n));
        CVTerms->add((void *)term);
      }
      n++;
    }
  }
  
}


XMLNode *
RDFAnnotationParser::deleteRDFAnnotation(XMLNode * annotation)
{
  const string&  name = annotation->getName();
  unsigned int n = 0;
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
  XMLNode * newAnnotation = new XMLNode(ann_token);

  // need to find each annotation and remove it if it is an RDF
  if (name == "annotation" && annotation->getNumChildren() > 0)
  {
    while (n < annotation->getNumChildren())
    {
      const string &name1 = annotation->getChild(n).getName();
      if (name1 != "RDF")
      {
        newAnnotation->addChild(annotation->getChild(n));
      }
      n++;
    }
  }

  if (newAnnotation->getNumChildren() == 0)
  {
    newAnnotation =NULL;
  }

  return newAnnotation;
}

/**
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a Model History from the annotation
 */

ModelHistory*
RDFAnnotationParser::parseRDFAnnotation(XMLNode * annotation)
{
  const string&  name = annotation->getName();
  const XMLNode*  RDFTop = NULL;
  ModelHistory * history = NULL;
  ModelCreator* creator = NULL;
  Date * modified = NULL;
  Date * created = NULL;
  unsigned int n = 0;

  // need to find the RDF description opening annotation
  if (!name.empty())
  {
    if (name == "annotation" && annotation->getNumChildren() > 0)
    {
      while (n < annotation->getNumChildren())
      {
	      const string &name1 = annotation->getChild(n).getName();
	      if (!name1.empty())
	      {
	        if (name1 == "RDF")
	        {
	          if (annotation->getChild(n).getNumChildren() > 0)
	          {
	            RDFTop = &(annotation->getChild(n).getChild(0));
	            break;
	          }
	        }
	      }
	      n++;
      }
    }
  }

  // find creation nodes and create history
  
  n = 0;
  if (RDFTop)
  {
	  history = new ModelHistory();
    while (n < RDFTop->getNumChildren())
    {
      const string &prefix = RDFTop->getChild(n).getPrefix();
      if (!prefix.empty())
      {
	      if (prefix == "dc")
	      {
	        creator = new ModelCreator(RDFTop->getChild(n));
	        history->addCreator(creator);
	      }
	      else if (prefix == "dcterms")
	      {
	        const string &name2 = RDFTop->getChild(n).getName();
	        if (!name2.empty())
	        {
	          if (RDFTop->getChild(n).getNumChildren() > 0
		          && RDFTop->getChild(n).getChild(0).getNumChildren() > 0)
	          {
	            if (name2 == "created")
	            {
		            created = new Date(RDFTop->getChild(n).getChild(0).
				              getChild(0).getCharacters());
		            history->setCreatedDate(created);
	            }
	            else if (name2 == "modified")
	            {
		            modified = new Date(RDFTop->getChild(n).getChild(0).
				                getChild(0).getCharacters());
		            history->setModifiedDate(modified);
	            }
	          }
	        }
	      }
      }
      n++;
    }
  }
  

  return history;

}


/**
 * takes a List of CVTerms
 * and creates the RDF annotation
 */

XMLNode * 
RDFAnnotationParser::parseCVTerms(const SBase * object)
{
  if (object->getCVTerms() == NULL || object->getCVTerms()->getSize() == 0)
  {
    return NULL;
  }

  /* create Namespaces - these go on the RDF element */
  XMLNamespaces xmlns = XMLNamespaces();
  xmlns.add("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf");
  xmlns.add("http://purl.org/dc/elements/1.1/", "dc");
  xmlns.add("http://purl.org/dc/terms/", "dcterms");
  xmlns.add("http://www.w3.org/2001/vcard-rdf/3.0#", "vCard");
  xmlns.add("http://biomodels.net/biology-qualifiers/", "bqbiol");
  xmlns.add("http://biomodels.net/model-qualifiers/", "bqmodel");

  /* create the basic triples */
  XMLTriple li_triple = XMLTriple("li", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple bag_triple = XMLTriple("Bag", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple descrip_triple = XMLTriple("Description", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple RDF_triple = XMLTriple("RDF", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple *type_triple;
  
  /* attributes */
  XMLAttributes blank_att = XMLAttributes();
  XMLAttributes desc_att = XMLAttributes();
  desc_att.add("rdf:about", "#" + object->getMetaId());
 
  /* tokens */
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), blank_att);
  XMLToken RDF_token = XMLToken(RDF_triple, blank_att, xmlns);
  XMLToken descrip_token = XMLToken(descrip_triple, desc_att);
  XMLToken bag_token = XMLToken(bag_triple, blank_att);
  XMLToken li_token;
  XMLToken * type_token;

  /* nodes */
  XMLNode * ann = new XMLNode(ann_token);
  XMLNode  li;
  XMLNode  * type;
  XMLNode  * bag;    
  XMLNode descrip = XMLNode(descrip_token);
  XMLNode RDF = XMLNode(RDF_token);

  std::string prefix;
  std::string name;
  std::string uri;

  XMLAttributes *resources;
  XMLAttributes *att;

  /* loop through the cv terms and add */
  /* want to add these in blocks of same qualifier */
  for (unsigned int n = 0; n < object->getCVTerms()->getSize(); n++)
  {

    if (static_cast <CVTerm *> (object->getCVTerms()->get(n))
      ->getQualifierType() == MODEL_QUALIFIER)
    {
      prefix = "bqmodel";
      uri = "http://biomodels.net/model-qualifiers/";

      switch (static_cast <CVTerm *> (object->getCVTerms()->get(n))
                                          ->getModelQualifierType())
      {
      case BQM_IS:
        name = "is";
        break;
      case BQM_IS_DESCRIBED_BY:
        name = "isDescribedBy";
        break;
      case BQM_UNKNOWN:
	/* 2007-02-12 <mhucka@caltech.edu> what should happen here? */
        return NULL;
	break;
      }
    }
    else
    {
      prefix = "bqbiol";
      uri = "http://biomodels.net/biological-qualifiers/";

      switch (static_cast <CVTerm *> (object->getCVTerms()->get(n))
                                          ->getBiologicalQualifierType())
      {
      case BQB_IS:
        name = "is";
        break;
      case BQB_HAS_PART:
        name = "hasPart";
        break;
      case BQB_IS_PART_OF:
        name = "isPartOf";
        break;
      case BQB_IS_VERSION_OF:
        name = "isVersionOf";
        break;
      case BQB_HAS_VERSION:
        name = "hasVersion";
        break;
      case BQB_IS_HOMOLOG_TO:
        name = "isHomologTo";
        break;
      case BQB_IS_DESCRIBED_BY:
        name = "isDescribedBy";
        break;
      case BQB_UNKNOWN:
	/* 2007-02-12 <mhucka@caltech.edu> what should happen here? */
        return NULL;
	break;
      }
    }
    type_triple = new XMLTriple(name, uri, prefix);
    type_token = new XMLToken (*(type_triple), blank_att);
    type = new XMLNode(*(type_token));
    bag = new XMLNode(bag_token);

    resources = static_cast <CVTerm *> (object->getCVTerms()->get(n))
                                                      ->getResources();

    for (int r = 0; r < resources->getLength(); r++)
    {
      att = new XMLAttributes();
      att->add(resources->getName(r), resources->getValue(r)); 
      
      li_token = XMLToken(li_triple, *(att));
      li_token.setEnd();

      li = XMLNode(li_token);

      bag->addChild(li);
    }

    type->addChild(*(bag));
  
    descrip.addChild(*(type));
  }

  RDF.addChild(descrip);
  ann->addChild(RDF);

  return ann;
}


/**
 * takes a Model creator information
 * and creates the RDF annotation
 */
XMLNode * 
RDFAnnotationParser::parseModelHistory(const SBase * model)
{
  if (model->getTypeCode() != SBML_MODEL)
  {
    return NULL;
  }
  
  ModelHistory * history = model->getModelHistory();
  if (history == NULL)
  {
    return NULL;
  }

  /* create Namespaces - these go on the RDF element */
  XMLNamespaces xmlns = XMLNamespaces();
  xmlns.add("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf");
  xmlns.add("http://purl.org/dc/elements/1.1/", "dc");
  xmlns.add("http://purl.org/dc/terms/", "dcterms");
  xmlns.add("http://www.w3.org/2001/vcard-rdf/3.0#", "vCard");
  xmlns.add("http://biomodels.net/biology-qualifiers/", "bqbiol");
  xmlns.add("http://biomodels.net/model-qualifiers/", "bqmodel");

  /* create the basic triples */
  XMLTriple li_triple = XMLTriple("li", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple bag_triple = XMLTriple("Bag", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple descrip_triple = XMLTriple("Description", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple RDF_triple = XMLTriple("RDF", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple creator_triple = XMLTriple("creator",
    "http://purl.org/dc/elements/1.1/",
    "dc");
  XMLTriple N_triple = XMLTriple("N",
    "http://www.w3.org/2001/vcard-rdf/3.0#",
    "vCard");
  XMLTriple Family_triple = XMLTriple("Family",
    "http://www.w3.org/2001/vcard-rdf/3.0#",
    "vCard");
  XMLTriple Given_triple = XMLTriple("Given",
    "http://www.w3.org/2001/vcard-rdf/3.0#",
    "vCard");
  XMLTriple Email_triple = XMLTriple("EMAIL",
    "http://www.w3.org/2001/vcard-rdf/3.0#",
    "vCard");
  XMLTriple Org_triple = XMLTriple("ORG",
    "http://www.w3.org/2001/vcard-rdf/3.0#",
    "vCard");
  XMLTriple Orgname_triple = XMLTriple("Orgname",
    "http://www.w3.org/2001/vcard-rdf/3.0#",
    "vCard");
  XMLTriple created_triple = XMLTriple("created",
    "http://purl.org/dc/terms/",
    "dcterms");
  XMLTriple modified_triple = XMLTriple("modified",
    "http://purl.org/dc/terms/",
    "dcterms");
  XMLTriple W3CDTF_triple = XMLTriple("W3CDTF",
    "http://purl.org/dc/terms/",
    "dcterms");
  XMLTriple empty_triple = XMLTriple( "", "", "");

  
  /* attributes */
  XMLAttributes blank_att = XMLAttributes();
  XMLAttributes desc_att = XMLAttributes();
  desc_att.add("rdf:about", "#" + model->getMetaId());
  XMLAttributes parseType_att = XMLAttributes();
  parseType_att.add("rdf:parseType", "Resource");
 
  /* tokens */
  XMLToken ann_token      = XMLToken(XMLTriple("annotation", "", ""), 
                                                            blank_att);

  XMLToken RDF_token      = XMLToken(RDF_triple,      blank_att, xmlns);
  XMLToken descrip_token  = XMLToken(descrip_triple,  desc_att);
  XMLToken bag_token      = XMLToken(bag_triple,      blank_att);
  XMLToken li_token       = XMLToken(li_triple,       parseType_att);
  XMLToken creator_token  = XMLToken(creator_triple,  parseType_att);
  XMLToken N_token        = XMLToken(N_triple,        parseType_att);
  XMLToken created_token  = XMLToken(created_triple,  parseType_att);
  XMLToken modified_token = XMLToken(modified_triple,  parseType_att);
  XMLToken Family_token   = XMLToken(Family_triple,   blank_att);
  XMLToken Given_token    = XMLToken(Given_triple,    blank_att);
  XMLToken Email_token    = XMLToken(Email_triple,    blank_att);
  XMLToken Org_token      = XMLToken(Org_triple,      blank_att);
  XMLToken Orgname_token  = XMLToken(Orgname_triple,  blank_att);
  XMLToken W3CDTF1_token  = XMLToken(W3CDTF_triple,   blank_att);
  XMLToken W3CDTF2_token  = XMLToken(W3CDTF_triple,   blank_att);
  XMLToken empty_token    = XMLToken("");

  /* nodes */
  XMLNode * ann = new XMLNode(ann_token);
  XMLNode RDF     = XMLNode(RDF_token);
  XMLNode descrip = XMLNode(descrip_token);
  XMLNode * bag;//     = XMLNode(bag_token);
  XMLNode * li;//      = XMLNode(li_token);
  XMLNode * N;//       = XMLNode(N_token);
  XMLNode * Family;//  = XMLNode(Family_token);
  XMLNode * Given;//   = XMLNode(Given_token);
  XMLNode * Email;//   = XMLNode(Email_token);
  XMLNode * Org ;//    = XMLNode(Org_token);
  XMLNode * Orgname;// = XMLNode(Orgname_token);
  XMLNode created = XMLNode(created_token);
  XMLNode modified= XMLNode(modified_token);
  XMLNode W3CDTF1 = XMLNode(W3CDTF1_token);
  XMLNode W3CDTF2 = XMLNode(W3CDTF2_token);
  XMLNode * empty;
  
  /* now add the data from the ModelHistory */
  ModelCreator *c;
  XMLNode * creator;// = new XMLNode(creator_token);
  for (unsigned int n = 0; n < history->getCreator()->getSize(); n++)
  {
    empty = new XMLNode(empty_token);
    c = (ModelCreator * )(history->getCreator()->get(n));
    empty = new XMLNode(empty_token);
    empty->append(c->getFamilyName());
    Family = new XMLNode(Family_token);
    Family->addChild(*(empty));
    empty = new XMLNode(empty_token);
    empty->append(c->getGivenName());
    Given = new XMLNode(Given_token);
    Given->addChild(*(empty));
    N = new XMLNode(N_token);
    N->addChild(*(Family));
    N->addChild(*(Given));
    empty = new XMLNode(empty_token);
    empty->append(c->getEmail());
    Email = new XMLNode(Email_token);
    Email->addChild(*(empty));
    empty = new XMLNode(empty_token);
    empty->append(c->getOrganisation());
    Orgname = new XMLNode(Orgname_token);
    Org = new XMLNode(Org_token);
    Orgname->addChild(*(empty));
    Org->addChild(*(Orgname));
    li = new XMLNode(li_token);
    li->addChild(*N);
    li->addChild(*Email);
    li->addChild(*Org);
    bag = new XMLNode(bag_token);
    bag->addChild(*li);
    creator = new XMLNode(creator_token);
    creator->addChild(*bag);
    descrip.addChild(*creator);
  }

  empty = new XMLNode(empty_token);
  empty->append(history->getCreatedDate()->getDateAsString());
  W3CDTF1.addChild(*(empty));
  empty = new XMLNode(empty_token);
  empty->append(history->getModifiedDate()->getDateAsString());
  W3CDTF2.addChild(*(empty));

  created.addChild(W3CDTF1);
  modified.addChild(W3CDTF2);
  descrip.addChild(created);
  descrip.addChild(modified);

  RDF.addChild(descrip);
  ann->addChild(RDF);



  return ann;
}

