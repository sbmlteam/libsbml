/**
 * @file    RDFAnnotation.cpp
 * @brief   RDFAnnotation I/O
 * @author  Sarah Keating
 * 
 * Copyright (C) 2009-2012 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
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
#include <sbml/annotation/ModelCreator.h>
#include <sbml/annotation/Date.h>
#include <sbml/annotation/RDFAnnotation.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * logs the given erroron the error log of the stream.
 * 
 * @param stream the stream to log the error on
 * @param element the element to log the error for
 * @param code the error code to log
 * @param msg optional message
 */
static void
logError (XMLInputStream* stream, const XMLToken& element, SBMLErrorCode_t code,
          const std::string& msg = "")
{
  if (&element == NULL || &stream == NULL) return;

  SBMLNamespaces* ns = stream->getSBMLNamespaces();
  if (ns != NULL)
  {
    static_cast <SBMLErrorLog*>
      (stream->getErrorLog())->logError(
      code,
      ns->getLevel(), 
      ns->getVersion(),
      msg, 
      element.getLine(), 
      element.getColumn());
  }
  else
  {
    static_cast <SBMLErrorLog*>
      (stream->getErrorLog())->logError(
      code, 
      SBML_DEFAULT_LEVEL, 
      SBML_DEFAULT_VERSION, 
      msg, 
      element.getLine(), 
      element.getColumn());
  }
}

/*
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a List of CVTerms from the annotation
 */
void 
RDFAnnotationParser::parseRDFAnnotation(
     const XMLNode * annotation, 
     List * CVTerms, 
     XMLInputStream* stream /*= NULL*/, 
     const char* metaId /*= NULL*/)
{
  if (annotation == NULL) return;

  const XMLTriple rdfAbout(
                "about", 
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                "rdf");
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
          const XMLNode& current = annotation->getChild(n).getChild(0);
          if (current.getName() == "Description")
          {
            if (current.hasAttr(rdfAbout))
            {
              string about = current.getAttrValue(rdfAbout);
              if (!about.empty())
              {
	              if (metaId == NULL || about.find(metaId) != string::npos)
                {
                  RDFTop = &current;
                  break;
                }
                else
                {
                  if (stream != NULL)
                  {
	                  logError(stream, current, RDFAboutTagNotMetaid);
                  }
                }
              }
              else
              {
                if (stream != NULL)
                {
                  logError(stream, current, RDFEmptyAboutTag);
                }
              }
            }
            else if (current.hasAttr("rdf:about"))
            {
              string about = current.getAttrValue("rdf:about");
              if (!about.empty())
              {
	              if (metaId == NULL || about.find(metaId) != string::npos)
                {
                  RDFTop = &current;
                  break;
                }
                else
                {
                  if (stream != NULL)
                  {
	                  logError(stream, current, RDFAboutTagNotMetaid);
                  }
                }
              }
              else
              {
                if (stream != NULL)
                {
                  logError(stream, current, RDFEmptyAboutTag);
                }
              }
            }
            else
            {
              if (stream != NULL)
              {
                logError(stream, current, RDFMissingAboutTag);
              }
            }
          }
	      }
      }
      n++;
    }
  }

  // find qualifier nodes and create CVTerms
  
  n = 0;
  if (RDFTop != NULL)
  {
    while (n < RDFTop->getNumChildren())
    {
      const string &name2 = RDFTop->getChild(n).getPrefix();
      if (name2 == "bqbiol" || name2 == "bqmodel")
      {
        term = new CVTerm(RDFTop->getChild(n));
        if (term->getResources()->getLength() > 0)
          CVTerms->add((void *)term);
      }
      n++;
    }
  }
  
}


XMLNode *
RDFAnnotationParser::deleteRDFAnnotation(const XMLNode * annotation)
{
  if (annotation == NULL) return NULL; 

  const string&  name = annotation->getName();
  unsigned int children = annotation->getNumChildren();
  unsigned int n = 0;
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), annotation->getAttributes(),annotation->getNamespaces());
  XMLNode * newAnnotation = NULL;
  XMLNode rdfAnnotation;
  bool hasAdditionalRDF = 
      RDFAnnotationParser::hasAdditionalRDFAnnotation(annotation);
  bool hasCVTermRDF = 
      RDFAnnotationParser::hasCVTermRDFAnnotation(annotation);
  bool hasHistoryRDF = 
    RDFAnnotationParser::hasHistoryRDFAnnotation(annotation);

  if (name != "annotation")
  {
    return NULL;
  }


  
  if (children > 1)
  {
    newAnnotation = new XMLNode(ann_token);
  
    // need to find each annotation and add if not RDF
    while (n < children)
    {
      const string &name1 = annotation->getChild(n).getName();
      if (name1 != "RDF")
      {
        newAnnotation->addChild(annotation->getChild(n));
      }
      else
      {
        // is RDF - if there is additional rdf preserve this
        if ((hasCVTermRDF || hasHistoryRDF) && hasAdditionalRDF)
        {
          rdfAnnotation = annotation->getChild(n);
          *(rdfAnnotation).removeChild(0);
          newAnnotation->addChild(rdfAnnotation);
        }
        else if (hasAdditionalRDF)
        {
          rdfAnnotation = annotation->getChild(n);
          newAnnotation->addChild(rdfAnnotation);
        }

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
      if ((hasCVTermRDF || hasHistoryRDF) && hasAdditionalRDF)
      {
        rdfAnnotation = annotation->getChild(0);
        *(rdfAnnotation).removeChild(0);
        newAnnotation = new XMLNode(ann_token);
        newAnnotation->addChild(rdfAnnotation);
      }
      else if (hasAdditionalRDF)
      {
        rdfAnnotation = annotation->getChild(0);
        newAnnotation = new XMLNode(ann_token);
        newAnnotation->addChild(rdfAnnotation);
      }
      else
      {
        ann_token.setEnd();
        newAnnotation = new XMLNode(ann_token);
      }
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
RDFAnnotationParser::parseRDFAnnotation(
     const XMLNode * annotation, 
     XMLInputStream* stream /*= NULL*/, 
     const char* metaId /*= NULL*/)
{
  if (annotation == NULL) return NULL;
  const XMLTriple rdfAbout(
                "about", 
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                "rdf");
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
	          const XMLNode& current = annotation->getChild(n).getChild(0);
            if (current.getName() == "Description")
            {
              if (current.hasAttr(rdfAbout))
              {
                string about = current.getAttrValue(rdfAbout);
                if (!about.empty())
                {
	                if (metaId == NULL || about.find(metaId) != string::npos)
                  {
                    RDFTop = &current;
                    break;
                  }
                  else
                  {
                    if (stream != NULL)
                    {
	                    logError(stream, current, RDFAboutTagNotMetaid);
                    }
                  }
                }
                else
                {
                  if (stream != NULL)
                  {
                    logError(stream, current, RDFEmptyAboutTag);
                  }
                }
              }
              else if (current.hasAttr("rdf:about"))
              {
                string about = current.getAttrValue("rdf:about");
                if (!about.empty())
                {
	                if (metaId == NULL || about.find(metaId) != string::npos)
                  {
                    RDFTop = &current;
                    break;
                  }
                  else
                  {
                    if (stream != NULL)
                    {
	                    logError(stream, current, RDFAboutTagNotMetaid);
                    }
                  }
                }
                else
                {
                  if (stream != NULL)
                  {
                    logError(stream, current, RDFEmptyAboutTag);
                  }
                 }
              }
              else
              {
                if (stream != NULL)
                {
                  logError(stream, current, RDFMissingAboutTag);
                }
              }
            }
	        }
	      }
	      n++;
      }
    }
  }

  // find creation nodes and create history
  
  n = 0;
  if (RDFTop != NULL)
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
		            history->addModifiedDate(modified);
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
  if (object == NULL) return NULL;

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

  if (object == NULL || 
	  object->getCVTerms() == NULL || 
	  object->getCVTerms()->getSize() == 0 ||
    !object->isSetMetaId())
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
  if (object == NULL) return NULL;

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
      CVTerm* current = static_cast <CVTerm *> (object->getCVTerms()->get(n));
      if (current == NULL) continue;

      if (current->getQualifierType() == MODEL_QUALIFIER)
      {
        prefix = "bqmodel";
        uri = "http://biomodels.net/model-qualifiers/";
       
        const char* term = ModelQualifierType_toString(
          current->getModelQualifierType());
        if (term == NULL) return NULL;

        name = term;
        
      }
      else if (current
        ->getQualifierType() == BIOLOGICAL_QUALIFIER)
      {
        prefix = "bqbiol";
        uri = "http://biomodels.net/biological-qualifiers/";

        const char* term = BiolQualifierType_toString(
            current->getBiologicalQualifierType());
        if (term == NULL) return NULL;
        name = term;        
      }
      else
      {
        continue;
      }
      

      resources = current->getResources();
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
RDFAnnotationParser::parseModelHistory(const SBase *object)
{
  if (object == NULL  || 
		(object->getLevel() < 3 && object->getTypeCode() != SBML_MODEL) ||
    !object->isSetMetaId())
  {
    return NULL;
  }
  
  ModelHistory * history = object->getModelHistory();
  if (history == NULL)
  {
    return NULL;
  }

  XMLNode *description = createRDFDescription(object);

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
  // for L2V4 it was realised that it was invalid for the creator 
  // to have a parseType attribute
  XMLToken creator_token;
  if (object->getLevel() > 2 || 
    (object->getLevel() == 2 && object->getVersion() > 3))
  {
    creator_token  = XMLToken(creator_triple,  blank_att);
  }
  else
  {
    creator_token  = XMLToken(creator_triple,  parseType_att);
  }
  XMLToken N_token        = XMLToken(N_triple,        parseType_att);
  XMLToken created_token  = XMLToken(created_triple,  parseType_att);
  XMLToken modified_token = XMLToken(modified_triple,  parseType_att);
  XMLToken Family_token   = XMLToken(Family_triple,   blank_att);
  XMLToken Given_token    = XMLToken(Given_triple,    blank_att);
  XMLToken Email_token    = XMLToken(Email_triple,    blank_att);
  // for L2V4 it was realised that the VCard:ORG 
  // should  have a parseType attribute
  XMLToken Org_token;
  if (object->getLevel() > 2 || 
    (object->getLevel() == 2 && object->getVersion() > 3))
  {
    Org_token  = XMLToken(Org_triple,  parseType_att);
  }
  else
  {
    Org_token  = XMLToken(Org_triple,  blank_att);
  }
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

      if (N == NULL)
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
    if (N != NULL)
    {
      li.addChild(*N);
      delete N;
    }
    if (Email != NULL)
    {
      li.addChild(*Email);
      delete Email;
    }
    if (Org != NULL)
    {
      li.addChild(*Org);
      delete Org;
    }
    if (c->getAdditionalRDF() != NULL)
    {
      li.addChild(*(c->getAdditionalRDF()));
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
    empty.append(history->getModifiedDate(0)->getDateAsString());
    W3CDTF2.addChild(empty);
    modified.addChild(W3CDTF2);
    description->addChild(modified);

    for (unsigned int n = 1; n < history->getNumModifiedDates(); n++)
    {
      XMLNode empty(empty_token);
      W3CDTF2.removeChildren();
      modified.removeChildren();
      empty.append(history->getModifiedDate(n)->getDateAsString());
      W3CDTF2.addChild(empty);
      modified.addChild(W3CDTF2);
      description->addChild(modified);
    }
  }

  // add CVTerms here

  XMLNode *CVTerms = createCVTerms(object);
  if (CVTerms != NULL)
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



/** @cond doxygen-libsbml-internal */

  
bool 
RDFAnnotationParser::hasRDFAnnotation(const XMLNode *annotation)
{
  if (annotation == NULL) return false;

  bool hasRDF = false;
  const string&  name = annotation->getName();
  unsigned int n = 0;

  if (name != "annotation")
  {
    return hasRDF;
  }

  while (n < annotation->getNumChildren())
  {
    const string &name1 = annotation->getChild(n).getName();
    if (name1 == "RDF")
    {
      hasRDF = true;
      break;
    }
    n++;
  }

  return hasRDF;

}


bool 
RDFAnnotationParser::hasAdditionalRDFAnnotation(const XMLNode *annotation)
{
  if (annotation == NULL) return false;

  bool hasAdditionalRDF = false;
  unsigned int n = 0;
  const XMLNode* rdf = NULL;

  if (!RDFAnnotationParser::hasRDFAnnotation(annotation))
  {
    return hasAdditionalRDF;
  }

  // get the RDF annotation
  while ( n < annotation->getNumChildren())
  {
    const string &name1 = annotation->getChild(n).getName();
    if (name1 == "RDF")
    {
      rdf = &(annotation->getChild(n));
      break;
    }
    n++;
  }

  // does it have more than one child
  if (rdf->getNumChildren() > 1)
  {
    hasAdditionalRDF = true;
  }
  else
  {
    // check whether the annotation relates to CVTerms
    List * tempCVTerms = new List();
    parseRDFAnnotation(annotation, tempCVTerms);
    if (tempCVTerms && tempCVTerms->getSize() == 0 && 
      !RDFAnnotationParser::hasHistoryRDFAnnotation(annotation))
    {
      hasAdditionalRDF = true;
    }
    
    if (tempCVTerms != NULL)
    {
      unsigned int size = tempCVTerms->getSize();
      while (size--) delete static_cast<CVTerm*>( tempCVTerms->remove(0) );
    }
    delete tempCVTerms;
  }

  return hasAdditionalRDF;
}


bool 
RDFAnnotationParser::hasCVTermRDFAnnotation(const XMLNode *annotation)
{
  bool hasCVTermRDF = false;

  if (!RDFAnnotationParser::hasRDFAnnotation(annotation))
  {
    return hasCVTermRDF;
  }

  // check whether the annotation relates to CVTerms
  List * tempCVTerms = new List();
  parseRDFAnnotation(annotation, tempCVTerms);
  if (tempCVTerms && tempCVTerms->getSize() > 0)
  {
    hasCVTermRDF = true;
  }

  if (tempCVTerms)
  {
    unsigned int size = tempCVTerms->getSize();
    while (size--) delete static_cast<CVTerm*>( tempCVTerms->remove(0) );
  }
  delete tempCVTerms;


  return hasCVTermRDF;
}


bool 
RDFAnnotationParser::hasHistoryRDFAnnotation(const XMLNode *annotation)
{
  bool hasHistoryRDF = false;

  if (!RDFAnnotationParser::hasRDFAnnotation(annotation))
  {
    return hasHistoryRDF;
  }

  // check whether the annotation relates to Model History
  ModelHistory *temp = parseRDFAnnotation(annotation);
  /* ok I relaxed the test so that an invalid model could be stored
   * but need to check that it resembles an model history in some 
   * way otherwise any RDF could be a model history
   */
  if (temp != NULL) // && temp->getNumCreators() > 0)
  {
    if (temp->getNumCreators() > 0 
      || temp->isSetCreatedDate() == true
      || temp->isSetModifiedDate() == true )
    {
      hasHistoryRDF = true;
    }
  }
  delete temp;


  return hasHistoryRDF;
}


  /** @endcond */

/** @cond doxygen-c-only */

/**
  * Parses an annotation (given as an XMLNode_t tree) into a list of
  * CVTerms.
  *
  * This is used to take an annotation that has been read into an SBML
  * model, identify the RDF elements within it, and create a list of
  * corresponding CVTerms.
  *
  * @param annotation XMLNode_t containing the annotation.
  * 
  * @param CVTerms list of CVTerms to be created.
  *
  * @see RDFAnnotationParser_parseRDFAnnotationWithModelHistory
  * (const XMLNode_t *annotation)
  */
void
RDFAnnotationParser_parseRDFAnnotation(const XMLNode_t * annotation, 
                                       List_t *CVTerms)
{
  if (annotation == NULL) return;
  RDFAnnotationParser::parseRDFAnnotation(annotation, CVTerms);
}

/**
  * Parses an annotation into a ModelHistory_t structure.
  *
  * This is used to take an annotation that has been read into an SBML
  * model, identify the RDF elements representing model history
  * information, and create a list of corresponding CVTerms.
  *
  * @param annotation XMLNode_t containing the annotation.
  *
  * @return a pointer to the ModelHistory_t created.
  *
  * @see RDFAnnotationParser_parseRDFAnnotation
  * (const XMLNode *annotation, List *CVTerms)
  */
ModelHistory_t *
RDFAnnotationParser_parseRDFAnnotationWithModelHistory(const XMLNode_t * annotation)
{
  if (annotation == NULL) return NULL;
  return RDFAnnotationParser::parseRDFAnnotation(annotation);
}

/**
  * Creates a blank annotation and returns the XMLNode_t 
  * corresonding to it.
  *
  * The annotation created by this method is a completely empty SBML
  * <code>&lt;annotation&gt;</code> element.  One use for this is to
  * then call createRDFAnnotation() to construct RDF content for this
  * empty annotation.
  *
  * @return a pointer to an XMLNode_t for the annotation
  *
  * @see RDFAnnotationParser_createRDFAnnotation()
  */
XMLNode_t *
RDFAnnotationParser_createAnnotation()
{
  return RDFAnnotationParser::createAnnotation();
}

/**
  * Creates blank RDF annotation content organized in the form defined in
  * Section 6 of the SBML Level 2 Version 4 specification .
  *
  * The annotation created by this method has namespace declarations for
  * all the relevant XML namespaces used in RDF annotations and also has
  * an empty RDF element.  Note that this is not the containing
  * <code>&lt;annotation&gt;</code> element; the method createAnnotation()
  * is available for that purpose.
  *
  * @return a pointer to an XMLNode_t represting the annotation.
  */
XMLNode_t *
RDFAnnotationParser_createRDFAnnotation()
{
  return RDFAnnotationParser::createRDFAnnotation();
}

/**
  * Deletes any RDF annotation found in the given XMLNode_t tree and returns
  * any remaining annotation content.
  *
  * The name of the given XMLNode_t must be "annotation", or else this
  * method returns NULL.
  *
  * @param annotation the XMLNode_t tree within which the RDF annotation
  * is to be found and deleted
  *
  * @return the XMLNode_t structure with any RDF annotations deleted
  */
XMLNode_t *
RDFAnnotationParser_deleteRDFAnnotation(XMLNode_t *annotation)
{
  if (annotation == NULL) return NULL;
  return RDFAnnotationParser::deleteRDFAnnotation(annotation);
}

/**
  * Takes an SBML object and creates an XMLNode_t corresponding to an
  * RDF "Description" element.
  *
  * This method is a handy way of creating RDF description objects linked
  * by the appropriate "metaid" field, for insertion into RDF annotations
  * in a model.  (Note that this method does not create a complete
  * annotation; it only creates a description element.  For creating empty
  * RDF annotations that can serve as containers for RDF descriptions, see
  * RDFAnnotationParser_createRDFAnnotation().
  *
  * @param object the object to be annotated
  *
  * @return a new XMLNode_t containing the "rdf:about" structure for an
  * RDF "Description" element.
  *
  * @see RDFAnnotationParser_createRDFAnnotation()
  */
XMLNode_t *
RDFAnnotationParser_createRDFDescription(const SBase_t * object)
{
  return RDFAnnotationParser::createRDFDescription(object);
}

/**
  * Takes a list of CVTerms form an SBML object and 
  * creates a the RDF "Description" element.
  *
  * This essentially takes the given SBML object, reads out the CVTerms
  * attached to it, calls createRDFDescriptiom() to create an RDF
  * "Description" element to hold the terms and adds each term with
  * appropriate qualifiers.
  *
  * @param object the SBML object to start from
  *
  * @return the XMLNode_t tree corresponding to the Description element of
  * an RDF annotation.
  */
XMLNode_t *
RDFAnnotationParser_createCVTerms(const SBase_t * object)
{
  return RDFAnnotationParser::createCVTerms(object);
}

/**
  * Takes a list of CVTerms from an SBML object and creates a 
  * complete SBML annotation around it.
  *
  * This essentially takes the given SBML object, calls createCVTerms
  * to read out the CVTerms
  * attached to it, calls createRDFAnnotation() to create an RDF
  * annotation to hold the terms, and finally calls createAnnotation() to
  * wrap the result as an SBML <code>&lt;annotation&gt;</code> element.
  *
  * @param object the SBML object to start from
  *
  * @return the XMLNode_t tree corresponding to the annotation.
  */
XMLNode_t *
RDFAnnotationParser_parseCVTerms(const SBase_t * object)
{
  if (object == NULL) return NULL;
  return RDFAnnotationParser::parseCVTerms(object);
}

/**
  * Takes an SBML Model_t object, reads off the model history information
  * stored in it, and creates a complete SBML annotation to store that
  * history.
  *
  * @param object a Model_t
  *
  * @return the XMLNode_t corresponding to an annotation containing 
  * MIRIAM-compliant model history information in RDF format
  */
XMLNode_t *
RDFAnnotationParser_parseModelHistory(const SBase_t * object)
{
  if (object == NULL) return NULL;
  return RDFAnnotationParser::parseModelHistory(object);
}

/** @endcond */

LIBSBML_CPP_NAMESPACE_END

