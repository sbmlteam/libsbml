/**
 * @file    RDFAnnotationParser.cpp
 * @brief   RDFAnnotation I/O
 * @author  Sarah Keating
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

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/**
 * logs the given erroron the error log of the stream.
 * 
 * @param stream the stream to log the error on.
 * @param element the element to log the error for.
 * @param code the error code to log.
 * @param msg optional message.
 */
static void
logError (XMLInputStream* stream, const XMLToken& element, SBMLErrorCode_t code,
          const std::string& msg = "")
{
  if (stream == NULL) return;

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
     const char* metaId,
     XMLInputStream* stream /*= NULL*/)
{
  if (annotation == NULL) 
    return;

  const XMLTriple rdfAbout(
                "about", 
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                "rdf");
  const XMLNode* RDFDesc = NULL;
  const XMLNode& current = 
                 annotation->getChild("RDF").getChild("Description");

  if (current.hasAttr(rdfAbout) || current.hasAttr("rdf:about"))
  {
    string about;
    if (current.hasAttr(rdfAbout))
    {
      about = current.getAttrValue(rdfAbout);
    }
    else
    {
      about = current.getAttrValue("rdf:about");
    }

    if (!about.empty())
    {
      if (metaId == NULL || about.find(metaId) != string::npos)
      {
        RDFDesc = &current;
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

  // if no error logged create CVTerms
  if (RDFDesc != NULL)
  {
    deriveCVTermsFromAnnotation(annotation, CVTerms);
  }
}

/** @cond doxygenLibsbmlInternal */
/*
 * internal sub function that derives the cvterms from the annotation
 */
void 
RDFAnnotationParser::deriveCVTermsFromAnnotation(
     const XMLNode * annotation, 
     List * CVTerms)
{
  if (annotation == NULL)
    return;

  // the annotation passed in may have a toplevel annotation tag BUT
  // it may not be- so need to check
  // if it isnt then it must be RDF or we do not have an rdf annotation
  bool topLevelIsAnnotation = false;
  if (annotation->getName() == "annotation")
  {
    topLevelIsAnnotation = true;
  }


  CVTerm * term;
  if (CVTerms == NULL)
    CVTerms = new List();

  const XMLNode* RDFDesc = NULL;
  if (topLevelIsAnnotation == true)
  {
    RDFDesc =  &(annotation->getChild("RDF").getChild("Description"));
  }
  else
  {
    if (annotation->getName() == "RDF")
    {
      RDFDesc = &(annotation->getChild("Description"));
    }
  }
  
  // find qualifier nodes and create CVTerms
  
  unsigned int n = 0;
  if (RDFDesc != NULL)
  {
    while (n < RDFDesc->getNumChildren())
    {
      const string &name2 = RDFDesc->getChild(n).getPrefix();
      if (name2 == "bqbiol" || name2 == "bqmodel")
      {
        term = new CVTerm(RDFDesc->getChild(n));
        if (term->getResources()->getLength() > 0)
        {
          CVTerms->add((void *)term->clone());
        }
        delete term;
      }
      n++;
    }
  }
  // rest modified flags
  for (n = 0; n < CVTerms->getSize(); n++)
  {
    static_cast<CVTerm*>(CVTerms->get(n))->resetModifiedFlags();
  }
  
}
/** @endcond */

/*
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a Model History from the annotation
 */

ModelHistory*
RDFAnnotationParser::parseRDFAnnotation(
     const XMLNode * annotation, 
     const char* metaId, 
     XMLInputStream* stream /*= NULL*/)
{
  ModelHistory * history = NULL;

  if (annotation == NULL) 
    return history;

  const XMLTriple rdfAbout(
                "about", 
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                "rdf");
  const XMLNode* RDFDesc = NULL;
  const XMLNode& current = 
                 annotation->getChild("RDF").getChild("Description");

  if (current.hasAttr(rdfAbout) || current.hasAttr("rdf:about"))
  {
    string about;
    if (current.hasAttr(rdfAbout))
    {
      about = current.getAttrValue(rdfAbout);
    }
    else
    {
      about = current.getAttrValue("rdf:about");
    }

    if (!about.empty())
    {
      if (metaId == NULL || about.find(metaId) != string::npos)
      {
        RDFDesc = &current;
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

  // if no error logged create history
  if (RDFDesc != NULL)
  {
    history = deriveHistoryFromAnnotation(annotation);
  }
  
  return history;
}



/** @cond doxygenLibsbmlInternal */
/*
 * internal sub function that derives the hiostory from the annotation
 */

ModelHistory*
RDFAnnotationParser::deriveHistoryFromAnnotation(
     const XMLNode * annotation)
{
  ModelHistory * history = NULL;

  if (annotation == NULL)
    return history;

  // the annotation passed in may have a toplevel annotation tag BUT
  // it may not be- so need to check
  // if it isnt then it must be RDF or we do not have an rdf annotation
  bool topLevelIsAnnotation = false;
  if (annotation->getName() == "annotation")
  {
    topLevelIsAnnotation = true;
  }

  const XMLNode* RDFDesc = NULL;
  if (topLevelIsAnnotation == true)
  {
    RDFDesc =  &(annotation->getChild("RDF").getChild("Description"));
  }
  else
  {
    if (annotation->getName() == "RDF")
    {
      RDFDesc = &(annotation->getChild("Description"));
    }
  }
  

  ModelCreator* creator = NULL;
  Date * modified = NULL;
  Date * created = NULL;
  static const XMLNode outOfRange;

  // find creation nodes and create history
  
  if (RDFDesc != NULL)
  {
    history = new ModelHistory();

    const XMLNode *creatorNode = &(RDFDesc->getChild("creator").getChild("Bag"));
    if (creatorNode->equals(outOfRange) == false)
    {
      for (unsigned int c = 0; c < creatorNode->getNumChildren(); c++)
      {
        creator = new ModelCreator(creatorNode->getChild(c));
        history->addCreator(creator);
        delete creator;
      }
    }
 
    const XMLNode *createdNode = &(RDFDesc->getChild("created").getChild("W3CDTF"));
    if (createdNode->equals(outOfRange) == false)
    {
      if (createdNode->getChild(0).isText() == true)
      {
        created = new Date(createdNode->getChild(0).getCharacters());
        history->setCreatedDate(created);
        delete created;
      }
    }

    /* there are possibly more than one modified elements */
    for (unsigned int n = 0; n < RDFDesc->getNumChildren(); n++)
    {
      if (RDFDesc->getChild(n).getName() == "modified")
      {
        const XMLNode *modifiedNode = &(RDFDesc->getChild(n).getChild("W3CDTF"));
        if (modifiedNode->equals(outOfRange) == false)
        {
          if (modifiedNode->getChild(0).isText() == true)
          {
            modified = new Date(modifiedNode->getChild(0).getCharacters());
            history->addModifiedDate(modified);
            delete modified;
          }
        }
      }
    }
    history->resetModifiedFlags();
  }

  return history;
}
/** @endcond */


XMLNode * 
RDFAnnotationParser::createAnnotation()
{
  XMLAttributes blank_att = XMLAttributes();
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), blank_att);
  return new XMLNode(ann_token);
}

XMLNode * 
RDFAnnotationParser::createRDFAnnotation(unsigned int level, 
                                         unsigned int version)
{
  /* create Namespaces - these go on the RDF element */
  XMLNamespaces xmlns = XMLNamespaces();
  xmlns.add("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf");
  if ((level == 2 && version < 5) )
  {
    xmlns.add("http://purl.org/dc/elements/1.1/", "dc");
  }
  xmlns.add("http://purl.org/dc/terms/", "dcterms");
  if (level < 3 )
    xmlns.add("http://www.w3.org/2001/vcard-rdf/3.0#", "vCard");
  else
  {
    xmlns.add("http://www.w3.org/2001/vcard-rdf/3.0#", "vCard");
    xmlns.add("http://www.w3.org/2006/vcard/ns#", "vCard4");
  }
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
  if (object == NULL) 
    return NULL;

  return createRDFDescription(object->getMetaId());
}


/** @cond doxygenLibsbmlInternal */
XMLNode * 
RDFAnnotationParser::createRDFDescription(const std::string & metaid)
{
  if (metaid.empty() == true) 
    return NULL;

  XMLTriple descrip_triple = XMLTriple("Description", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");

  XMLAttributes desc_att = XMLAttributes();
  desc_att.add("rdf:about", "#" + metaid);
 
  XMLToken descrip_token = XMLToken(descrip_triple, desc_att);

  return new XMLNode(descrip_token);
}
/** @endcond */

/*
 * this is here because we originally exposed the function
 * with this name so we have to leave it
 */

XMLNode * 
RDFAnnotationParser::createCVTerms(const SBase * object)
{
  return createRDFDescriptionWithCVTerms(object);
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


  XMLNode *CVTerms = createRDFDescriptionWithCVTerms(object);

  XMLNode * RDF = createRDFAnnotation(object->getLevel(), object->getVersion());
  RDF->addChild(*CVTerms);

  delete CVTerms;

  XMLNode *ann = createAnnotation();
  ann->addChild(*RDF);

  delete RDF;

  return ann;
}


/** @cond doxygenLibsbmlInternal */
XMLNode * 
RDFAnnotationParser::createRDFDescriptionWithCVTerms(const SBase * object)
{
  if (object == NULL || 
    object->getCVTerms() == NULL || 
    object->getCVTerms()->getSize() == 0 ||
    !object->isSetMetaId())
  {
    return NULL;
  }

  XMLNode *description = createRDFDescription(object);

  /* loop through the cv terms and add */
  /* want to add these in blocks of same qualifier */
  if (object->getCVTerms())
  {
    for (unsigned int n = 0; n < object->getCVTerms()->getSize(); n++)
    {
      CVTerm* current = static_cast <CVTerm *> (object->getCVTerms()->get(n));
      if (current == NULL) continue;

      XMLNode   * type = createQualifierElement(current, object->getLevel(),
                                                object->getVersion());
      if (type != NULL)
      {
        description->addChild(*(type));
        delete type;
      }
    }

  }
  // if all cvterms were bad then the description will contain nothing
  if (description->getNumChildren() == 0)
  {
    delete description;
    return NULL;
  }

  return description;
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
XMLNode * 
RDFAnnotationParser::createBagElement(const CVTerm * term,
                                      unsigned int level, unsigned int version)
{
  if (term->getNumResources() == 0)
  {
    return NULL;
  }

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

  XMLNode *bag = new XMLNode(bag_token);
    
  const XMLAttributes *resources = term->getResources();

  for (int r = 0; r < resources->getLength(); r++)
  {
    XMLAttributes att;
    att.add(resources->getName(r), resources->getValue(r)); 
    
    XMLToken li_token(li_triple, att);
    li_token.setEnd();
    XMLNode li(li_token);

    bag->addChild(li);
  }

  if ((level == 2 && version > 4) || (level == 3))
  {
    // only write out nested annotations in allowed levels
    for (unsigned int n = 0; n < term->getNumNestedCVTerms(); n++)
    {
      XMLNode *nest = createQualifierElement(term->getNestedCVTerm(n),
                                             level, version);
      if (nest != NULL)
      {
        bag->addChild(*(nest));
        delete nest;
      }
    }
  }

  return bag;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
XMLNode * 
RDFAnnotationParser::createQualifierElement(const CVTerm * term, 
                                      unsigned int level, unsigned int version)
{
  std::string prefix;
  std::string name;
  std::string uri;

  /* attributes */
  XMLAttributes blank_att = XMLAttributes();

  if (term->getQualifierType() == MODEL_QUALIFIER)
  {
    prefix = "bqmodel";
    uri = "http://biomodels.net/model-qualifiers/";
   
    const char* term_name = ModelQualifierType_toString(
      term->getModelQualifierType());
    if (term_name == NULL) 
      return NULL;

    name = term_name;   
  }
  else if (term
    ->getQualifierType() == BIOLOGICAL_QUALIFIER)
  {
    prefix = "bqbiol";
    uri = "http://biomodels.net/biological-qualifiers/";

    const char* term_name = BiolQualifierType_toString(
        term->getBiologicalQualifierType());
    if (term_name == NULL)
      return NULL;

    name = term_name;
  }
  else
  {
    return NULL;
  }

  XMLTriple type_triple(name, uri, prefix);
  XMLToken  type_token(type_triple, blank_att);
  XMLNode * type = new XMLNode(type_token);

  XMLNode * bag = createBagElement(term, level, version);
  if (bag != NULL)
  {
    type->addChild(*(bag));
    delete bag;
  }

  return type;
}
/** @endcond */


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

  XMLNode *description = createRDFDescriptionWithHistory(object);

  XMLNode *CVTerms = createRDFDescriptionWithCVTerms(object);

  if (CVTerms != NULL)
  {
    for (unsigned int i = 0; i < CVTerms->getNumChildren(); i++)
    {
      description->addChild(CVTerms->getChild(i));
    }
    delete CVTerms;
  }

  XMLNode * RDF = createRDFAnnotation(object->getLevel(), object->getVersion());
  RDF->addChild(*description);
  delete description;

  XMLNode *ann = createAnnotation();
  ann->addChild(*RDF);
  delete RDF;

  return ann;
}


XMLNode * 
RDFAnnotationParser::parseOnlyModelHistory(const SBase *object)
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

  XMLNode *description = createRDFDescriptionWithHistory(object);

  XMLNode * RDF = createRDFAnnotation(object->getLevel(), object->getVersion());
  RDF->addChild(*description);
  delete description;

  XMLNode *ann = createAnnotation();
  ann->addChild(*RDF);
  delete RDF;

  return ann;
}

/** @cond doxygenLibsbmlInternal */
XMLNode * 
RDFAnnotationParser::createRDFDescriptionWithHistory(const SBase * object)
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
  bool use_vcard3 = true;
  std::string vcard_uri = "http://www.w3.org/2001/vcard-rdf/3.0#";
  std::string vcard_prefix = "vCard";
  std::string name_element = "N";
  std::string family_element = "Family";
  std::string given_element = "Given";
  std::string email_element = "EMAIL";
  std::string org_element = "ORG";
  std::string fn_element = "fn";
  std::string text_element = "text";
  //bool use_fn = false;

  if (object->getLevel() == 3 && object->getVersion() == 2)
  {
    vcard_uri = "http://www.w3.org/2006/vcard/ns#";
    vcard_prefix = "vCard4";
    name_element = "hasName";
    family_element = "family-name";
    given_element = "given-name";
    email_element = "hasEmail";
    org_element = "organization-name";
    use_vcard3 = false;
  }
  /* create the basic triples */
  XMLTriple li_triple = XMLTriple("li", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple bag_triple = XMLTriple("Bag", 
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdf");
  XMLTriple creator_triple;
  if ((object->getLevel() == 2 && object->getVersion() > 4) || 
    (object->getLevel() == 3))
  {
    creator_triple = XMLTriple("creator",
    "http://purl.org/dc/terms/",
    "dcterms");
  }
  else
  {
    creator_triple = XMLTriple("creator",
      "http://purl.org/dc/elements/1.1/",
      "dc");
  }
  XMLTriple N_triple = XMLTriple(name_element,
    vcard_uri, vcard_prefix);
  XMLTriple Family_triple = XMLTriple(family_element,
    vcard_uri, vcard_prefix);
  XMLTriple Given_triple = XMLTriple(given_element,
    vcard_uri, vcard_prefix);
  XMLTriple Email_triple = XMLTriple(email_element,
    vcard_uri, vcard_prefix);
  XMLTriple Org_triple = XMLTriple(org_element,
    vcard_uri, vcard_prefix);
  XMLTriple Fn_triple = XMLTriple("fn",
    vcard_uri, vcard_prefix);
  XMLTriple Text_triple = XMLTriple(text_element,
    vcard_uri, vcard_prefix);
  XMLTriple Orgname_triple = XMLTriple("Orgname",
    vcard_uri, vcard_prefix);
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
  XMLToken Fn_token   = XMLToken(Fn_triple,   blank_att);
  XMLToken Text_token   = XMLToken(Text_triple,   blank_att);
  // for L2V4 it was realised that the VCard:ORG 
  // should  have a parseType attribute
  XMLToken Org_token;
  if ((object->getLevel() == 3 && object->getVersion() == 1)  || 
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
    if (c->usingFNVcard4())
    {
      if (c->usingSingleName())
      {
        // we want to a single name but we have entered it as two
        std::string name = c->getName();
        if (name != c->getGivenName())
        {
          name = c->getGivenName() + " " + c->getFamilyName();
        }
        XMLNode empty(empty_token);
        empty.append(name);

        XMLNode Text(Text_token);
        Text.addChild(empty);

        N = new XMLNode(Fn_token);
        N->addChild(Text);
      }
    }
    else
    {
      std::string name = c->getName();
      std::string fname, gname;
      // we have entered one name but want to display it as 2
      if (name == c->getGivenName())
      {
        std::size_t found = name.find(" ");
        gname = name.substr(0, found);
        fname = name.substr(found + 1);
        c->setFamilyName(fname);
        c->setGivenName(gname);
      }
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
      if (use_vcard3)
      {
        XMLNode empty(empty_token);
        empty.append(c->getOrganisation());
        XMLNode Orgname(Orgname_token);
        Orgname.addChild(empty);

        Org = new XMLNode(Org_token);
        Org->addChild(Orgname);
      }
      else
      {
        XMLNode empty(empty_token);
        empty.append(c->getOrganisation());

        Org = new XMLNode(Org_token);
        Org->addChild(empty);
      }
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
      XMLNode empty1(empty_token);
      W3CDTF2.removeChildren();
      modified.removeChildren();
      empty1.append(history->getModifiedDate(n)->getDateAsString());
      W3CDTF2.addChild(empty1);
      modified.addChild(W3CDTF2);
      description->addChild(modified);
    }
  }

  return description;
}
/** @endcond */


XMLNode *
RDFAnnotationParser::deleteRDFAnnotation(const XMLNode * annotation)
{
  if (annotation == NULL) 
    return NULL; 

  const string&  name = annotation->getName();

  if (name != "annotation")
  {
    return NULL;
  }

  XMLNode * halfAnnotation = deleteRDFHistoryAnnotation(annotation);
  XMLNode * newAnnotation = deleteRDFCVTermAnnotation(halfAnnotation);

  delete halfAnnotation;

  return newAnnotation;
}


XMLNode *
RDFAnnotationParser::deleteRDFHistoryAnnotation(const XMLNode * annotation)
{
  if (annotation == NULL) 
    return NULL; 

  const string&  name = annotation->getName();
  unsigned int children = annotation->getNumChildren();
  unsigned int n = 0;
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), 
    annotation->getAttributes(),annotation->getNamespaces());
  XMLNode * newAnnotation = NULL;
  XMLNode rdfAnnotation;
  bool hasCVTermRDF = 
      RDFAnnotationParser::hasCVTermRDFAnnotation(annotation);
  bool hasHistoryRDF = 
    RDFAnnotationParser::hasHistoryRDFAnnotation(annotation);

  if (name != "annotation")
  {
    return NULL;
  }

  // if there is no history element just return the whole annotation
  if (hasHistoryRDF == false)
  {
     newAnnotation = new XMLNode(ann_token);
     for (unsigned int i = 0; i < children; i++)
     {
       newAnnotation->addChild(annotation->getChild(i));
     }
  }
  else
  {  
    unsigned int rdfPosition = 0;
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
          rdfPosition = n;
        }
        n++;
      }
    }

    // we want to remove ONLY the History bit
    rdfAnnotation = annotation->getChild(rdfPosition);


    // remove and keep the first description element
    // assume that the annotation is proper sbml-miriam
    XMLNode* descr = rdfAnnotation.removeChild(
                      (unsigned int)rdfAnnotation.getIndex("Description"));
    if (hasCVTermRDF == false)
    {
      //we have a history, no cvterms and possibly other rdf
      // so removing the first decription element will have removed history
      // check for additional rdf and add back if it is there
      if (rdfAnnotation.getNumChildren() > 0)
      {
        if (newAnnotation == NULL)
        {
          newAnnotation = new XMLNode(ann_token);
        }
        newAnnotation->addChild(rdfAnnotation);
      }
      else
      {
        if (newAnnotation == NULL)
        {
          ann_token.setEnd();
          newAnnotation = new XMLNode(ann_token);
        }
      }

    }
    else
    {
      // we have history and cvterms with possible other rdf
      // so remove history elements from the description element
      unsigned int nn = descr->getNumChildren();
      for (unsigned int i = nn; i > 0; i--)
      {
        XMLNode child = descr->getChild(i-1);
        if ((child.getName() == "creator") ||
          (child.getName() == "created") ||
          (child.getName() == "modified"))
        {
          delete descr->removeChild(i-1);
        }
      }
      rdfAnnotation.insertChild(0, *descr);
      if (newAnnotation == NULL)
      {
        newAnnotation = new XMLNode(ann_token);
      }
      newAnnotation->insertChild(rdfPosition, rdfAnnotation);
    }
    delete descr;
  }
  return newAnnotation;
}


XMLNode *
RDFAnnotationParser::deleteRDFCVTermAnnotation(const XMLNode * annotation)
{
  if (annotation == NULL) 
    return NULL; 

  const string&  name = annotation->getName();
  unsigned int children = annotation->getNumChildren();
  unsigned int n = 0;
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), 
    annotation->getAttributes(),annotation->getNamespaces());
  XMLNode * newAnnotation = NULL;
  XMLNode rdfAnnotation;
  bool hasCVTermRDF = 
      RDFAnnotationParser::hasCVTermRDFAnnotation(annotation);
  bool hasHistoryRDF = 
    RDFAnnotationParser::hasHistoryRDFAnnotation(annotation);

  if (name != "annotation")
  {
    return NULL;
  }

  // if there is no cv terms just return the whole annotation
  if (hasCVTermRDF == false)
  {
     newAnnotation = new XMLNode(ann_token);
     for (unsigned int i = 0; i < children; i++)
     {
       newAnnotation->addChild(annotation->getChild(i));
     }
  }
  else
  {  
    unsigned int rdfPosition = 0;
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
          rdfPosition = n;
        }
        n++;
      }
    }

    // we want to remove ONLY the CVTerm bit
    rdfAnnotation = annotation->getChild(rdfPosition);


    // remove the first description element keeping a copy
    // assume that the annotation is proper sbml-miriam
    XMLNode* descr = rdfAnnotation.removeChild(
                      (unsigned int)rdfAnnotation.getIndex("Description"));
    if (hasHistoryRDF == false)
    {
      //we have cvterms no history and possibly other rdf
      // so removing the first decription element will have removed cvterms
      // check for additional rdf and add back if it is there
      if (rdfAnnotation.getNumChildren() > 0)
      {
        if (newAnnotation == NULL)
        {
          newAnnotation = new XMLNode(ann_token);
        }
        newAnnotation->addChild(rdfAnnotation);
      }
      else
      {
        if (newAnnotation == NULL)
        {
          ann_token.setEnd();
          newAnnotation = new XMLNode(ann_token);
        }
      }

    }
    else
    {
      // we have history and cvterms with possible other rdf
      // so remove cvterms elements from the description element
      unsigned int nn = descr->getNumChildren();
      for (unsigned int i = nn; i > 0; i--)
      {
        XMLNode child = descr->getChild(i-1);
        if ((child.getName() != "creator") &&
          (child.getName() != "created") &&
          (child.getName() != "modified"))
        {
          delete descr->removeChild(i-1);
        }
      }
      rdfAnnotation.insertChild(0, *descr);
      if (newAnnotation == NULL)
      {
        newAnnotation = new XMLNode(ann_token);
      }
      newAnnotation->insertChild(rdfPosition, rdfAnnotation);
    }
    delete descr;
  }
  return newAnnotation;
}


/** @cond doxygenLibsbmlInternal */
  
bool 
RDFAnnotationParser::hasRDFAnnotation(const XMLNode *annotation)
{
  if (annotation == NULL) 
    return false;

  bool hasRDF = false;
  const string&  name = annotation->getName();
  unsigned int n = 0;

  // since the argument annotation might be anything we have to make the 
  // assumption that it is either an annotation element or an RDF element
  // anything else we have to assume does not have an rdf annotation
  // that we are capable of parsing

  if (name == "RDF")
  {
    return true;
  }
  else if (name != "annotation")
  {
    return false;
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
  if (annotation == NULL) 
    return false;

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
  if (rdf != NULL && rdf->getNumChildren() > 1)
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
    else
    {
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
  deriveCVTermsFromAnnotation(annotation, tempCVTerms);
  if (tempCVTerms && tempCVTerms->getSize() > 0)
  {
    hasCVTermRDF = true;
  }

  if (tempCVTerms)
  {
    unsigned int size = tempCVTerms->getSize();
    for (unsigned int i = size; i > 0; i--)
    {
      delete static_cast<CVTerm*>( tempCVTerms->remove(0) );
    }
    //while (size--) delete static_cast<CVTerm*>( tempCVTerms->remove(0) );
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
  ModelHistory *temp = deriveHistoryFromAnnotation(annotation);
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

#endif /* __cplusplus */
/** @cond doxygenIgnored */
void
RDFAnnotationParser_parseRDFAnnotation(const XMLNode_t * annotation, 
                                       List_t *CVTerms)
{
  if (annotation == NULL) return;
  RDFAnnotationParser::parseRDFAnnotation(annotation, CVTerms);
}

ModelHistory_t *
RDFAnnotationParser_parseRDFAnnotationWithModelHistory(const XMLNode_t * annotation)
{
  if (annotation == NULL) return NULL;
  return RDFAnnotationParser::parseRDFAnnotation(annotation);
}

XMLNode_t *
RDFAnnotationParser_createAnnotation()
{
  return RDFAnnotationParser::createAnnotation();
}

XMLNode_t *
RDFAnnotationParser_createRDFAnnotation()
{
  return RDFAnnotationParser::createRDFAnnotation();
}

XMLNode_t *
RDFAnnotationParser_createRDFAnnotationForLevelAndVersion(unsigned int level,
                                                         unsigned int version)
{
  return RDFAnnotationParser::createRDFAnnotation(level, version);
}

XMLNode_t *
RDFAnnotationParser_deleteRDFAnnotation(XMLNode_t *annotation)
{
  if (annotation == NULL) return NULL;
  return RDFAnnotationParser::deleteRDFAnnotation(annotation);
}

XMLNode_t *
RDFAnnotationParser_createRDFDescription(const SBase_t * object)
{
  return RDFAnnotationParser::createRDFDescription(object);
}

XMLNode_t *
RDFAnnotationParser_createCVTerms(const SBase_t * object)
{
  return RDFAnnotationParser::createCVTerms(object);
}

XMLNode_t *
RDFAnnotationParser_parseCVTerms(const SBase_t * object)
{
  if (object == NULL) return NULL;
  return RDFAnnotationParser::parseCVTerms(object);
}

XMLNode_t *
RDFAnnotationParser_parseModelHistory(const SBase_t * object)
{
  if (object == NULL) return NULL;
  return RDFAnnotationParser::parseModelHistory(object);
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

