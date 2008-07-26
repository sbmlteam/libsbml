/**
 * @file    RDFAnnotation.cpp
 * @brief   RDFAnnotation I/O
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
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


/*
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a List of CVTerms from the annotation
 */
void 
RDFAnnotationParser::parseRDFAnnotation(const XMLNode * annotation, List * CVTerms)
{

  const string&  name = annotation->getName();
  const XMLNode*  RDFTop = NULL;
  unsigned int n = 0;
  CVTerm * term;
  if (CVTerms == NULL)
    CVTerms = new List();

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
RDFAnnotationParser::deleteRDFAnnotation(const XMLNode * annotation)
{
  const string&  name = annotation->getName();
  unsigned int children = annotation->getNumChildren();
  unsigned int n = 0;
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), annotation->getAttributes(),annotation->getNamespaces());
  XMLNode * newAnnotation = NULL;

  if (name != "annotation")
  {
    return NULL;
  }
  
  if (children > 1)
  {
    newAnnotation = new XMLNode(ann_token);
  
    // need to find each annotation and remove it if it is an RDF
    while (n < children)
    {
      const string &name1 = annotation->getChild(n).getName();
      if (name1 != "RDF")
      {
        newAnnotation->addChild(annotation->getChild(n));
      }
      n++;
    }
  }
  else
  {
    if (children == 1 && annotation->getChild(0).getName() != "RDF")
    {
      newAnnotation = new XMLNode(ann_token);
      newAnnotation->addChild(annotation->getChild(0));
    }
    else
    {
      ann_token.setEnd();
      newAnnotation = new XMLNode(ann_token);
    }
  }

  return newAnnotation;
}

/*
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a Model History from the annotation
 */

ModelHistory*
RDFAnnotationParser::parseRDFAnnotation(const XMLNode * annotation)
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
          // this should be the Bag node containing the list of creators
          const XMLNode *creatorNode = &(RDFTop->getChild(n).getChild(0));
          for (unsigned int c = 0; c < creatorNode->getNumChildren(); c++)
          {
	          creator = new ModelCreator(creatorNode->getChild(c));
	          history->addCreator(creator);
                  delete creator;
          }
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
                            delete created;
	            }
	            else if (name2 == "modified")
	            {
		            modified = new Date(RDFTop->getChild(n).getChild(0).
				                getChild(0).getCharacters());
		            history->setModifiedDate(modified);
                            delete modified;
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
XMLNode * 
RDFAnnotationParser::createAnnotation()
{
  XMLAttributes blank_att = XMLAttributes();
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), blank_att);
  return new XMLNode(ann_token);
}

XMLNode * 
RDFAnnotationParser::createRDFAnnotation()
{
  /* create Namespaces - these go on the RDF element */
  XMLNamespaces xmlns = XMLNamespaces();
  xmlns.add("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf");
  xmlns.add("http://purl.org/dc/elements/1.1/", "dc");
  xmlns.add("http://purl.org/dc/terms/", "dcterms");
  xmlns.add("http://www.w3.org/2001/vcard-rdf/3.0#", "vCard");
  xmlns.add("http://biomodels.net/biology-qualifiers/", "bqbiol");
  xmlns.add("http://biomodels.net/model-qualifiers/", "bqmodel");

  XMLTriple RDF_triple = XMLTriple("RDF", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  
  XMLAttributes blank_att = XMLAttributes();
 
  XMLToken RDF_token = XMLToken(RDF_triple, blank_att, xmlns);

  return new XMLNode(RDF_token);
}

XMLNode * 
RDFAnnotationParser::createRDFDescription(const SBase *object)
{
  XMLTriple descrip_triple = XMLTriple("Description", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");

  XMLAttributes desc_att = XMLAttributes();
  desc_att.add("rdf:about", "#" + object->getMetaId());
 
  XMLToken descrip_token = XMLToken(descrip_triple, desc_att);

  return new XMLNode(descrip_token);
}


/*
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


  XMLNode *CVTerms = createCVTerms(object);

  XMLNode * RDF = createRDFAnnotation();
  RDF->addChild(*CVTerms);

  delete CVTerms;

  XMLNode *ann = createAnnotation();
  ann->addChild(*RDF);

  delete RDF;

  return ann;
}


XMLNode * 
RDFAnnotationParser::createCVTerms(const SBase * object)
{

  /* create the basic triples */
  XMLTriple li_triple = XMLTriple("li", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple bag_triple = XMLTriple("Bag", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  
  /* attributes */
  XMLAttributes blank_att = XMLAttributes();
 
  /* tokens */
  XMLToken bag_token = XMLToken(bag_triple, blank_att);

  std::string prefix;
  std::string name;
  std::string uri;

  XMLAttributes *resources;

  XMLNode *description = createRDFDescription(object);

  /* loop through the cv terms and add */
  /* want to add these in blocks of same qualifier */
  if (object->getCVTerms())
  {
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
          return NULL;
	        break;
        }
      }
      else if (static_cast <CVTerm *> (object->getCVTerms()->get(n))
        ->getQualifierType() == BIOLOGICAL_QUALIFIER)
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
        case BQB_IS_ENCODED_BY:
          name = "isEncodedBy";
          break;
        case BQB_ENCODES:
          name = "encodes";
          break;
        case BQB_UNKNOWN:
          return NULL;
      	  break;
        }
      }
      else
      {
        continue;
      }
      

      resources = static_cast <CVTerm *> (object->getCVTerms()->get(n))
                                                        ->getResources();
      XMLNode   bag(bag_token);

      for (int r = 0; r < resources->getLength(); r++)
      {
        XMLAttributes att;
        att.add(resources->getName(r), resources->getValue(r)); 
        
        XMLToken li_token(li_triple, att);
        li_token.setEnd();
        XMLNode li(li_token);

        bag.addChild(li);
      }

      XMLTriple type_triple(name, uri, prefix);
      XMLToken  type_token(type_triple, blank_att);
      XMLNode   type(type_token);

      type.addChild(bag);
      description->addChild(type);
    }

  }
  return description;
}


/*
 * takes a Model creator information
 * and creates the RDF annotation
 */
XMLNode * 
RDFAnnotationParser::parseModelHistory(const Model *model)
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

  XMLNode *description = createRDFDescription(model);

  /* create the basic triples */
  XMLTriple li_triple = XMLTriple("li", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple bag_triple = XMLTriple("Bag", 
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
  XMLAttributes parseType_att = XMLAttributes();
  parseType_att.add("rdf:parseType", "Resource");
 
  /* tokens */
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
  XMLNode bag     = XMLNode(bag_token);
  XMLNode created = XMLNode(created_token);
  XMLNode modified= XMLNode(modified_token);
  XMLNode W3CDTF1 = XMLNode(W3CDTF1_token);
  XMLNode W3CDTF2 = XMLNode(W3CDTF2_token);
  //
  // The following XMLNode objects are used only
  // in the for loop below (for each ModelCreator object
  // in ModelHistory object) and reset in each step.
  // Thus, they are defined only in the block in which 
  // they are used to avoid a memory leak.
  //  
  //  XMLNode * N
  //  XMLNode * Email
  //  XMLNode * Org
  //  XMLNode Family;
  //  XMLNode Given
  //  XMLNode Orgname
  //  XMLNode li
  //

  /* now add the data from the ModelHistory */

  for (unsigned int n = 0; n < history->getNumCreators(); n++)
  {
    XMLNode * N     = 0;
    XMLNode * Email = 0;
    XMLNode * Org   = 0;

    ModelCreator* c = history->getCreator(n);
    if (c->isSetFamilyName())
    {
      XMLNode empty(empty_token);
      empty.append(c->getFamilyName());

      XMLNode Family(Family_token);
      Family.addChild(empty);

      N = new XMLNode(N_token);
      N->addChild(Family);
    }

    if (c->isSetGivenName())
    {
      XMLNode empty(empty_token);
      empty.append(c->getGivenName());

      XMLNode Given(Given_token);
      Given.addChild(empty);

      if (!N)
      {
        N = new XMLNode(N_token);
      }
      N->addChild(Given);
    }

    if (c->isSetEmail())
    {
      XMLNode empty(empty_token);
      empty.append(c->getEmail());

      Email = new XMLNode(Email_token);
      Email->addChild(empty);
    }

    if (c->isSetOrganisation())
    {
      XMLNode empty(empty_token);
      empty.append(c->getOrganisation());
      XMLNode Orgname(Orgname_token);
      Orgname.addChild(empty);

      Org = new XMLNode(Org_token);
      Org->addChild(Orgname);
    }

    XMLNode li(li_token);
    if (N)
    {
      li.addChild(*N);
      delete N;
    }
    if (Email)
    {
      li.addChild(*Email);
      delete Email;
    }
    if (Org)
    {
      li.addChild(*Org);
      delete Org;
    }
    bag.addChild(li);
  }

  XMLNode creator(creator_token);
  creator.addChild(bag);
  description->addChild(creator);
  
  /* created date */
  if (history->isSetCreatedDate())
  {
    XMLNode empty(empty_token);
    empty.append(history->getCreatedDate()->getDateAsString());
    W3CDTF1.addChild(empty);
    created.addChild(W3CDTF1);
    description->addChild(created);
  }

  /* modified date */
  if (history->isSetModifiedDate())
  {
    XMLNode empty(empty_token);
    empty.append(history->getModifiedDate()->getDateAsString());
    W3CDTF2.addChild(empty);
    modified.addChild(W3CDTF2);
    description->addChild(modified);
  }

  // add CVTerms here

  XMLNode *CVTerms = createCVTerms(model);
  if (CVTerms)
  {
    for (unsigned int i = 0; i < CVTerms->getNumChildren(); i++)
    {
      description->addChild(CVTerms->getChild(i));
    }
    delete CVTerms;
  }

  XMLNode * RDF = createRDFAnnotation();
  RDF->addChild(*description);
  delete description;

  XMLNode *ann = createAnnotation();
  ann->addChild(*RDF);
  delete RDF;

  return ann;
}
