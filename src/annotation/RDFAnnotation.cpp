/**
 * \file    RDFAnnotation.cpp
 * \brief   RDFAnnotation I/O
 * \author  Sarah Keating
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


#include <limits>
#include <iomanip>
#include <string>
#include <sstream>

#include <cstdlib>

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/SBase.h>

#include <sbml/SBMLErrorLog.h>

#include <sbml/util/util.h>
#include <sbml/util/List.h>

#include "RDFAnnotation.h"


using namespace std;

/**
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a List of CVTerms from the annotation
 */
LIBSBML_EXTERN
void 
parseRDFAnnotation(XMLNode * annotation, List * CVTerms)
{

  const string&  name = annotation->getName();
  const XMLNode*  RDFTop = NULL;
  const XMLNode* qualifierNode;
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
        RDFTop = &(annotation->getChild(n).getChild(0));
        break;
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


/**
 * takes a List of CVTerms
 * and creates the RDF annotation
 */
LIBSBML_EXTERN
XMLNode * 
parseCVTerms(SBase * object)
{
  /* create Namespaces - these go on the RDF element */
  XMLNamespaces xmlns = XMLNamespaces();
  xmlns.add("http://purl.org/dc/elements/1.1/", "dc");
  xmlns.add("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf");
  xmlns.add("http://purl.org/dc/terms/", "dcterms");
  xmlns.add("http://www.w3.org/2001/vcard-rdf/3.0#", "vcard");
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
  desc_att.add("about", "#" + object->getMetaId());
 
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
  for (unsigned int n = 0; n < object->getCVTerms()->getSize(); n++)
  {

    if (static_cast <CVTerm *> (object->getCVTerms()->get(n))->getQualifierType() == 
                                            MODEL_QUALIFIER)
    {
      prefix = "bqmodel";
      uri = "http://biomodels.net/model-qualifiers/";

      switch (static_cast <CVTerm *> (object->getCVTerms()->get(n))->getModelQualifierType())
      {
      case BQM_IS:
        name = "is";
        break;
      case BQM_IS_DESCRIBED_BY:
        name = "isDescribedBy";
        break;
      }
    }
    else
    {
      prefix = "bqbiol";
      uri = "http://biomodels.net/biological-qualifiers/";

      switch (static_cast <CVTerm *> (object->getCVTerms()->get(n))->getBiologicalQualifierType())
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
      }
    }
    type_triple = new XMLTriple(name, uri, prefix);
    type_token = new XMLToken (*(type_triple), blank_att);
    type = new XMLNode(*(type_token));
    bag = new XMLNode(bag_token);

    resources = static_cast <CVTerm *> (object->getCVTerms()->get(n))->getResources();

    for (unsigned int r = 0; r < resources->getLength(); r++)
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
