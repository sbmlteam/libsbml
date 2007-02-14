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
#include <sbml/layout/Layout.h>


#include <sbml/SBMLErrorLog.h>

#include <sbml/util/util.h>
#include <sbml/util/List.h>

#include <sbml/annotation/ModelHistory.h>

#include "LayoutAnnotation.h"
#include "sbml/layout/Layout.h"
#include "sbml/Model.h"

using namespace std;

/**
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a List of CVTerms from the annotation
 */
LIBSBML_EXTERN
void 
parseLayoutAnnotation(XMLNode * annotation, ListOfLayouts& layouts)
{

  const string&  name = annotation->getName();
  const XMLNode*  LayoutTop = NULL;
  Layout* layout;
  unsigned int n = 0;

  // need to find the RDF desciption opening annotation
  if (name == "annotation" && annotation->getNumChildren() > 0)
  {
    while (n < annotation->getNumChildren())
    {
      const string &name1 = annotation->getChild(n).getName();
      if (name1 == "listOfLayouts") // also check the namespace
      {
        const XMLNamespaces& namespaces=annotation->getChild(n).getNamespaces();
        if(namespaces.getIndex("http://projects.eml.org/bcb/sbml/level2")!=-1)
        {
          LayoutTop = &(annotation->getChild(n).getChild(0));
          break;
        }
      }
      n++;
    }
  }

  // find qualifier nodes and create 

  
  n = 0;
  if (LayoutTop)
  {
    while (n < LayoutTop->getNumChildren())
    {
      const string &name2 = LayoutTop->getChild(n).getName();
      if (name2 == "layout")
      {
        layout = new Layout(LayoutTop->getChild(n));
        layouts.appendAndOwn(layout);
      }
      n++;
    }
  }
}

  
/**
 * Takes an XMLNode and tries to find the layout annotation node and deletes it if it was found.
 */
LIBSBML_EXTERN
XMLNode* deleteLayoutAnnotation(XMLNode* pAnnotation)
{
  XMLNode *newAnnotation = NULL;
  const string&  name = pAnnotation->getName();
  unsigned int n = 0;
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());

  // need to find each annotation and remove it if it is an RDF
  if (name == "annotation" && pAnnotation->getNumChildren() > 0)
  {
    while (n < pAnnotation->getNumChildren())
    {
      const string &name1 = pAnnotation->getChild(n).getName();
      if (name1 != "listOfLayouts" || pAnnotation->getChild(n).getNamespaces().getIndex("http://projects.eml.org/bcb/sbml/level2")==-1)
      {
        if (!newAnnotation)
        {
          newAnnotation = new XMLNode(ann_token);
          newAnnotation->addChild(pAnnotation->getChild(n));
        }
        else
        {
          newAnnotation->addChild(pAnnotation->getChild(n));
        }
      }
      n++;
    }
  }

  return newAnnotation;
}

/**
 * Creates an XMLNode that represents the layouts of the model from the given Model object.
 */
LIBSBML_EXTERN
XMLNode* parseLayouts(const Model* pModel)
{
 
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes()); 
  XMLNode* pNode = new XMLNode(ann_token);
  if(pModel->getListOfLayouts()->size()>0)
  {
    pNode->addChild(pModel->getListOfLayouts()->toXML());
  }
  return pNode;
}
 
  
  



