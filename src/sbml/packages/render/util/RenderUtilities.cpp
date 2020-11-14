/**
 * @file    RenderUtilities.cpp
 * @brief   implementation of RenderUtilities, a class of utility functions for the render package
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
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
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */


#include <sbml/packages/render/util/RenderUtilities.h>

#include <sbml/packages/render/common/RenderExtensionTypes.h>
#include <sbml/packages/render/extension/RenderLayoutPlugin.h>
#include <sbml/packages/render/extension/RenderListOfLayoutsPlugin.h>
#include <sbml/packages/layout/common/LayoutExtensionTypes.h>

#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus

#include <iostream>
#include <string>
using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/**
 * This method adds a correction term to text elements from the old spec so that the text placement
 * is improved.
 */
LIBSBML_EXTERN
void 
fixTextElements(RenderGroup* pGroup,RelAbsVector fontSize)
{
    if(pGroup==NULL) return;
    if(pGroup->isSetFontSize())
    {
        fontSize=pGroup->getFontSize();
    }
    unsigned int i,iMax=pGroup->getListOfElements()->size();
    RenderGroup* pChildGroup;
    Text* pText=NULL;
    SBase* pObject=NULL;
    for(i=0;i<iMax;++i)
    {
        pObject=pGroup->getElement(i);
        pText=dynamic_cast<Text*>(pObject);
        if(pText!=NULL)
        {
            // subtract 80% of the current font size from the
            // y offset of the text
            if(pText->isSetFontSize())
            {
                fontSize=pText->getFontSize();
            }
            // we can only correct the y offset if the font size is an absolute value
            if(fabs(fontSize.getRelativeValue()) < 1e-10)
            {
                RelAbsVector v=pText->getY();
                v.setAbsoluteValue(v.getAbsoluteValue()-0.8*fontSize.getAbsoluteValue());
                pText->setY(v);
            }
        }
        else
        {
            pChildGroup=dynamic_cast<RenderGroup*>(pObject);
            if(pChildGroup!=NULL)
            {
                fixTextElements(pChildGroup,fontSize);
            }
        }
    }
}


/**
 * This method adds a correction term to text elements from the old spec so that the text placement
 * is improved.
 */
LIBSBML_EXTERN
void 
fixTextElements(RenderInformationBase* pRenderInfo)
{
    if(pRenderInfo==NULL) return;
    unsigned int i,iMax=pRenderInfo->getListOfLineEndings()->size();
    for(i=0;i<iMax;++i)
    {
        fixTextElements(pRenderInfo->getLineEnding(i)->getGroup());
    }
    LocalRenderInformation* pLRI=dynamic_cast<LocalRenderInformation*>(pRenderInfo);
    if(pLRI)
    {
        fixTextElements(pLRI);
    }
    else
    {
        fixTextElements(dynamic_cast<GlobalRenderInformation*>(pRenderInfo));
    }
}

/**
 * This method adds a correction term to text elements from the old spec so that the text placement
 * is improved.
 */
LIBSBML_EXTERN
void 
fixTextElements(LocalRenderInformation* pRenderInfo)
{
    if(pRenderInfo==NULL) return;
    unsigned int i,iMax=pRenderInfo->getListOfStyles()->size();
    for(i=0;i<iMax;++i)
    {
        fixTextElements(pRenderInfo->getStyle(i)->getGroup());
    }
}

/**
 * This method adds a correction term to text elements from the old spec so that the text placement
 * is improved.
 */
LIBSBML_EXTERN
void 
fixTextElements(GlobalRenderInformation* pRenderInfo)
{
    if(pRenderInfo==NULL) return;
    unsigned int i,iMax=pRenderInfo->getListOfStyles()->size();
    for(i=0;i<iMax;++i)
    {
        fixTextElements(pRenderInfo->getStyle(i)->getGroup());
    }
}

/**
 * takes an annotation that has been read into the model
 * identifies the listOfLayouts element and creates a List of 
 * Layouts from the annotation
 */
LIBSBML_EXTERN
void 
parseGlobalRenderAnnotation(XMLNode * annotation, ListOfLayouts* pLOL)
{
  if(!pLOL) return;
  const string&  name = annotation->getName();
  const XMLNode*  RenderTop = NULL;
  GlobalRenderInformation* render;
  unsigned int n = 0;
  RenderListOfLayoutsPlugin* plugin = (RenderListOfLayoutsPlugin*)pLOL->getPlugin("render");
       
  // need to find the layout desciption opening annotation
  if (name == "annotation" && annotation->getNumChildren() > 0)
  {
    while (n < annotation->getNumChildren())
    {
      const string &name1 = annotation->getChild(n).getName();
      if (name1 == "listOfGlobalRenderInformation") // also check the namespace
      {
        const XMLNamespaces& namespaces=annotation->getChild(n).getNamespaces();
        if(namespaces.getIndex("http://projects.eml.org/bcb/sbml/render/version1_0")!=-1)
        {
          RenderTop = &(annotation->getChild(n));
          break;
        }
        // keep this in to read the render information prior to version 1
        // (first draft)
        else if(namespaces.getIndex("http://projects.eml.org/bcb/sbml/render/level2")!=-1)
        {
          RenderTop = &(annotation->getChild(n));
          break;
        }
      }
      n++;
    }
  }

  // find qualifier nodes and create 
  n = 0;
  if (RenderTop)
  {
    while (n < RenderTop->getNumChildren())
    {
      const string &name2 = RenderTop->getChild(n).getName();
      if (name2 == "renderInformation")
      {
        render = plugin->createGlobalRenderInformation();
        render->parseXML(RenderTop->getChild(n));
        if(plugin->getListOfGlobalRenderInformation()->getMajorVersion()<1)
        {
            // fix the old text elements with a heuristic
            fixTextElements(render);
        }
      }
      else if(name=="annotation")
      {
        plugin->getListOfGlobalRenderInformation()->setAnnotation(new XMLNode(RenderTop->getChild(n)));
      }
      else if(name=="notes")
      {
        plugin->getListOfGlobalRenderInformation()->setNotes(new XMLNode(RenderTop->getChild(n)));
      }
      n++;
    }
  }
}

  
/**
 * Takes an XMLNode and tries to find the render information annotation node and deletes it if it was found.
 */
LIBSBML_EXTERN
XMLNode* deleteGlobalRenderAnnotation(XMLNode* pAnnotation)
{
  const string&  name = pAnnotation->getName();
  unsigned int n = 0;

  // need to find each annotation and remove it if it is an RDF
  if (name == "annotation" && pAnnotation->getNumChildren() > 0)
  {
    while (n < pAnnotation->getNumChildren())
    {
      const string &name1 = pAnnotation->getChild(n).getName();
      if (name1 == "listOfGlobalRenderInformation" || 
        pAnnotation->getChild(n).getNamespaces().getIndex("http://projects.eml.org/bcb/sbml/render/version1_0")!=-1 ||
        pAnnotation->getChild(n).getNamespaces().getIndex("http://projects.eml.org/bcb/sbml/render/level2")!=-1)
      {
        pAnnotation->removeChild(n);
        continue;
      }
      n++;
    }
  }
  return pAnnotation;
}

/**
 * Creates an XMLNode that represents the layouts of the model from the given Model object.
 */
LIBSBML_EXTERN
XMLNode* parseGlobalRenderInformation(const ListOfLayouts* pList)
{  
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes()); 
  XMLNode* pNode = new XMLNode(ann_token);
  RenderListOfLayoutsPlugin* plugin = (RenderListOfLayoutsPlugin*)pList->getPlugin("render");
  if(plugin->getListOfGlobalRenderInformation()->size()>0)
  {
    pNode->addChild(plugin->getListOfGlobalRenderInformation()->toXML());
  }
  return pNode;
}
 

LIBSBML_EXTERN 
XMLNode* 
deleteLocalRenderAnnotation(XMLNode* pAnnotation)
{
  const string&  name = pAnnotation->getName();
  unsigned int n = 0;

  // need to find each annotation and remove it if it is an RDF
  if (name == "annotation" && pAnnotation->getNumChildren() > 0)
  {
    while (n < pAnnotation->getNumChildren())
    {
      const string &name1 = pAnnotation->getChild(n).getName();
      if (name1 == "listOfRenderInformation" || 
         pAnnotation->getChild(n).getNamespaces().getIndex("http://projects.eml.org/bcb/sbml/render/version1_0")!=-1 || 
         pAnnotation->getChild(n).getNamespaces().getIndex("http://projects.eml.org/bcb/sbml/render/level2")!=-1)
      {
        pAnnotation->removeChild(n);
        continue;
      }
      n++;
    }
  }
  return pAnnotation;
}

LIBSBML_EXTERN 
XMLNode* 
parseLocalRenderInformation(const Layout* pLayout)
{
 
  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes()); 
  XMLNode* pNode = new XMLNode(ann_token);
  RenderLayoutPlugin *plugin = (RenderLayoutPlugin*)pLayout->getPlugin("render");
  if(plugin->getListOfLocalRenderInformation()->size()>0)
  {
    pNode->addChild(plugin->getListOfLocalRenderInformation()->toXML());
  }
  return pNode;
}
  

LIBSBML_EXTERN
void 
parseLocalRenderAnnotation(XMLNode * annotation, Layout* pLayout)
{
  if(!pLayout) return;
  const string&  name = annotation->getName();
  const XMLNode*  RenderTop = NULL;
  LocalRenderInformation* render;
  unsigned int n = 0;

  RenderLayoutPlugin *plugin = (RenderLayoutPlugin*)pLayout->getPlugin("render");

  // need to find the layout desciption opening annotation
  if (name == "annotation" && annotation->getNumChildren() > 0)
  {
    while (n < annotation->getNumChildren())
    {
      const string &name1 = annotation->getChild(n).getName();
      if (name1 == "listOfRenderInformation") // also check the namespace
      {
        const XMLNamespaces& namespaces=annotation->getChild(n).getNamespaces();
        if(namespaces.getIndex("http://projects.eml.org/bcb/sbml/render/version1_0")!=-1)
        {
          RenderTop = &(annotation->getChild(n));
          break;
        }
        // keep in the read render information from the first draft
        else if(namespaces.getIndex("http://projects.eml.org/bcb/sbml/render/level2")!=-1)
        {
          RenderTop = &(annotation->getChild(n));
          break;
        }
      }
      n++;
    }
  }

  // find qualifier nodes and create 

  
  n = 0;
  if (RenderTop)
  {
    while (n < RenderTop->getNumChildren())
    {
      const string &name2 = RenderTop->getChild(n).getName();
      if (name2 == "renderInformation")
      {
        render = plugin->createLocalRenderInformation();
        render->parseXML(RenderTop->getChild(n));
        if(plugin->getListOfLocalRenderInformation()->getMajorVersion()<1)
        {
            fixTextElements(render);
        }
      }
      else if(name=="annotation")
      {
        plugin->getListOfLocalRenderInformation()->setAnnotation(new XMLNode(RenderTop->getChild(n)));
      }
      else if(name=="notes")
      {
        plugin->getListOfLocalRenderInformation()->setNotes(new XMLNode(RenderTop->getChild(n)));
      }
      n++;
    }
  }
}


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
