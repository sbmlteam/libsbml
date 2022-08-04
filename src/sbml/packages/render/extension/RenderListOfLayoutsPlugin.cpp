/**
 * @file    RenderListOfLayoutsPlugin.cpp
 * @brief   Implementation of RenderListOfLayoutsPlugin, the plugin class of
 *          the fbc package for the Model element.
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

#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/extension/RenderListOfLayoutsPlugin.h>
#include <sbml/packages/layout/common/LayoutExtensionTypes.h>
#include <sbml/packages/render/util/RenderUtilities.h>
#include <sbml/packages/render/sbml/GlobalRenderInformation.h>
#include <sbml/packages/render/sbml/ListOfGlobalRenderInformation.h>
#include <sbml/util/ElementFilter.h>


#ifdef __cplusplus

#include <iostream>
using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN

  /*
  * Constructor
  */
  RenderListOfLayoutsPlugin::RenderListOfLayoutsPlugin (const std::string& uri, 
  const std::string &prefix,
  RenderPkgNamespaces *renderns)
  : SBasePlugin(uri,prefix, renderns)
  , mGlobalRenderInformation(renderns)
{
}


/*
* Copy constructor. Creates a copy of this SBase object.
*/
RenderListOfLayoutsPlugin::RenderListOfLayoutsPlugin(const RenderListOfLayoutsPlugin& orig)
  : SBasePlugin(orig)
  , mGlobalRenderInformation(orig.mGlobalRenderInformation)
{
}


/*
* Destroy this object.
*/
RenderListOfLayoutsPlugin::~RenderListOfLayoutsPlugin () {}

/*
* Assignment operator for RenderListOfLayoutsPlugin.
*/
RenderListOfLayoutsPlugin& 
  RenderListOfLayoutsPlugin::operator=(const RenderListOfLayoutsPlugin& orig)
{
  if(&orig!=this)
  {
    SBasePlugin::operator =(orig);
  }    
  return *this;
}


/*
* Creates and returns a deep copy of this RenderListOfLayoutsPlugin object.
* 
* @return a (deep) copy of this RenderListOfLayoutsPlugin object
*/
RenderListOfLayoutsPlugin* 
  RenderListOfLayoutsPlugin::clone () const
{
  return new RenderListOfLayoutsPlugin(*this);  
}


List*
RenderListOfLayoutsPlugin::getAllElements(ElementFilter *filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mGlobalRenderInformation, filter);

  return ret;
}




/** @cond doxygenLibsbmlInternal */

int 
RenderListOfLayoutsPlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model==NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const LayoutModelPlugin* layPlug = dynamic_cast<const LayoutModelPlugin*>
    (model->getPlugin("layout"));

  if (layPlug == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  
  const RenderListOfLayoutsPlugin* lolplug = 
    dynamic_cast<const RenderListOfLayoutsPlugin*>
               (layPlug->getListOfLayouts()->getPlugin("render"));
  
  if (lolplug == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  ListOfLayouts* parent = dynamic_cast<ListOfLayouts*>(getParentSBMLObject());

  if (parent==NULL) 
  {
    return LIBSBML_INVALID_OBJECT;
  }
  
  ret = mGlobalRenderInformation.appendFrom(lolplug->getListOfGlobalRenderInformation());
 
  return ret;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
SBase*
  RenderListOfLayoutsPlugin::createObject(XMLInputStream& stream)
{
  SBase*        object = 0;

  const std::string&   name   = stream.peek().getName();
  const XMLNamespaces& xmlns  = stream.peek().getNamespaces();
  const std::string&   prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix)
  {
    if ( name == "listOfGlobalRenderInformation" ) 
    {
      //cout << "[DEBUG] LayoutModelPlugin::createObject create listOfLayouts" << endl;
      object = &mGlobalRenderInformation;
    
      if (targetPrefix.empty())
      {
        //
        // prefix is empty when writing elements in layout extension.
        //
        mGlobalRenderInformation.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    } 
  }    

  return object;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
  RenderListOfLayoutsPlugin::writeElements (XMLOutputStream& stream) const
{
    if ( getURI() == RenderExtension::getXmlnsL2() ) return;
  if (mGlobalRenderInformation.size() > 0 || mGlobalRenderInformation.isSetDefaultValues())
    mGlobalRenderInformation.write(stream);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void 
RenderListOfLayoutsPlugin::writeAttributes (XMLOutputStream& stream) const
{
  //
  // This function is used only for SBML Level 2.
  //
  if ( getURI() != RenderExtension::getXmlnsL2() ) return;

  SBase *parent = const_cast<SBase*>(getParentSBMLObject());
  if (!parent) return;

  // getting the annotation actually updates all annotations
  // so the call is needed
  /*XMLNode *annotation =*/ parent->getAnnotation();
  //RenderListOfLayoutsPlugin* self = const_cast<RenderListOfLayoutsPlugin*>(this);
  //self->syncAnnotation(parent, parent->getAnnotation());
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* default for components that have no required elements */
bool
  RenderListOfLayoutsPlugin::hasRequiredElements() const
{
  bool allPresent = true;

  return allPresent;
}
/** @endcond */



/*
*
*  (EXTENSION) Additional public functions
*
*/  




/** @cond doxygenLibsbmlInternal */
/*
* Sets the parent SBMLDocument of this SBML object.
*
* @param d the SBMLDocument object to use
*/
void 
  RenderListOfLayoutsPlugin::setSBMLDocument (SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);
  mGlobalRenderInformation.setSBMLDocument(d);  
  if (mGlobalRenderInformation.isSetDefaultValues())
  {
    mGlobalRenderInformation.getDefaultValues()->setSBMLDocument(d);
  }

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
* Sets the parent SBML object of this plugin object to
* this object and child elements (if any).
* (Creates a child-parent relationship by this plugin object)
*/
void
  RenderListOfLayoutsPlugin::connectToParent (SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);
  mGlobalRenderInformation.connectToParent(sbase);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
* Enables/Disables the given package with child elements in this plugin
* object (if any).
*/
void
  RenderListOfLayoutsPlugin::enablePackageInternal(const std::string& pkgURI,
  const std::string& pkgPrefix, bool flag)
{
  mGlobalRenderInformation.enablePackageInternal(pkgURI, pkgPrefix, flag);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return @c true if the subclass read from the stream, false otherwise.
 */
bool
RenderListOfLayoutsPlugin::readOtherXML (SBase* parentObject, XMLInputStream& stream)
{
  // L2 render parsed by the annotation API 
  // @see parseAnnotation / syncAnnotation
  return false; 
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void RenderListOfLayoutsPlugin::parseAnnotation()
{
  ListOfLayouts* lol = (ListOfLayouts*)getParentSBMLObject();

  parseGlobalRenderAnnotation(lol->getAnnotation(), lol);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* 
 * Parse L2 annotation if supported
 *
 */
void 
RenderListOfLayoutsPlugin::parseAnnotation(SBase *parentObject, XMLNode *annotation)
{
  mGlobalRenderInformation.setSBMLDocument(mSBML);  
  parseGlobalRenderAnnotation(annotation,(ListOfLayouts*)parentObject);  
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Synchronizes the annotation of this SBML object.
 */
void
RenderListOfLayoutsPlugin::syncAnnotation (SBase *parentObject, XMLNode *pAnnotation)
{
  if(pAnnotation && pAnnotation->getNumChildren() > 0)
  {
    parentObject->removeTopLevelAnnotationElement("listOfGlobalRenderInformation", "", false);
  }
  
  // only do this for L1 and L2 documents
  if(getLevel() >= 3)
    return;

  if (mGlobalRenderInformation.size() == 0)
    return;

  
  XMLNode *render = parseGlobalRenderInformation((ListOfLayouts*)parentObject);

  if (render == NULL)
    return;

  
  if (pAnnotation == NULL)
  {
    // cannot happen, as syncAnnotation is called with a valid Annotation
    // (possibly empty)
    return;
  }
  else
  {
      if (pAnnotation->isEnd())
      {
          pAnnotation->unsetEnd();
      }
      pAnnotation->addChild(render->getChild(0));
      delete render;
  }
  
  
  
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

bool
RenderListOfLayoutsPlugin::accept (SBMLVisitor& v) const
{
  return true;
}

/** @endcond */



#ifdef DUNNO
/*
 * Sets the annotation (by string) of this SBML object to a copy of annotation.
 */
int
RenderListOfLayoutsPlugin::setAnnotation (const std::string& annotation)
{
  int success = LIBSBML_OPERATION_FAILED;
  if(annotation.empty())
  {
    unsetAnnotation();
    return LIBSBML_OPERATION_SUCCESS;
  }

  XMLNode* annt_xmln;
  if (getSBMLDocument())
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }

  if(annt_xmln)
  {
    success = setAnnotation(annt_xmln);
    delete annt_xmln;
  }
  return success;
}



/*
 * Appends annotation (by string) to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
int
RenderListOfLayoutsPlugin::appendAnnotation (const std::string& annotation)
{
  int success = LIBSBML_OPERATION_FAILED;
  XMLNode* annt_xmln;
  if (getSBMLDocument())
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }

  if(annt_xmln)
  {
    success = appendAnnotation(annt_xmln);
    delete annt_xmln;
  }

  return success;
}


/*
 * Sets the annotation of this SBML object to a copy of annotation.
 */
int
RenderListOfLayoutsPlugin::setAnnotation (const XMLNode* annotation)
{
  int success = SBase::setAnnotation(annotation);
  if(success == LIBSBML_OPERATION_SUCCESS && getLevel() < 3)
  {
    for(unsigned int i=0; i < mGlobalRenderInformation.size(); i++)
    {
      GlobalRenderInformation* gr = static_cast<GlobalRenderInformation*>(mGlobalRenderInformation.remove(0));
      delete gr;
    }
    if(mAnnotation)
    {
      // parse mAnnotation (if any) and set mLayouts 
      mGlobalRenderInformation.setSBMLDocument(mSBML);  
      parseGlobalRenderAnnotation(mAnnotation,this);
    }
  }
  return success;
}


/*
 * Sets the annotation (by string) of this SBML object to a copy of annotation.
 */
int
RenderListOfLayoutsPlugin::setAnnotation (const std::string& annotation)
{
  int success = LIBSBML_OPERATION_FAILED;
  if(annotation.empty())
  {
    unsetAnnotation();
    return LIBSBML_OPERATION_SUCCESS;
  }

  XMLNode* annt_xmln;
  if (getSBMLDocument())
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }

  if(annt_xmln)
  {
    success = setAnnotation(annt_xmln);
    delete annt_xmln;
  }
  return success;
}


/*
 * Appends annotation to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
int
RenderListOfLayoutsPlugin::appendAnnotation (const XMLNode* annotation)
{
  int success = LIBSBML_OPERATION_FAILED;
  if(!annotation) return LIBSBML_OPERATION_SUCCESS;

  XMLNode* new_annotation = NULL;
  const std::string&  name = annotation->getName();

  // check for annotation tags and add if necessary
  if (name != "annotation")
  {
    XMLToken ann_t = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
    new_annotation = new XMLNode(ann_t);
    new_annotation->addChild(*annotation);
  }
  else
  {
    new_annotation = annotation->clone();
  }

  if(getLevel() < 3)
  {
    // parse new_annotation and add mLayouts 
    mGlobalRenderInformation.setSBMLDocument(mSBML);  
    parseGlobalRenderAnnotation(new_annotation,this);
  }

  success = SBase::appendAnnotation(new_annotation);

  delete new_annotation;

  return success;
}


/*
 * Appends annotation (by string) to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
int
RenderListOfLayoutsPlugin::appendAnnotation (const std::string& annotation)
{
  int success = LIBSBML_OPERATION_FAILED;
  XMLNode* annt_xmln;
  if (getSBMLDocument())
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }

  if(annt_xmln)
  {
    success = appendAnnotation(annt_xmln);
    delete annt_xmln;
  }

  return success;
}


#endif
/*
 * Returns a pointer to the list object that contains local render information.
 */
ListOfGlobalRenderInformation* RenderListOfLayoutsPlugin::getListOfGlobalRenderInformation()
{
    return &mGlobalRenderInformation;
}

/*
 * Returns a const pointer to the list object that contains local render information.
 */
const ListOfGlobalRenderInformation* RenderListOfLayoutsPlugin::getListOfGlobalRenderInformation() const
{
    return &mGlobalRenderInformation;
}


/*
 * Returns the number of local render information objects.
 */
unsigned int RenderListOfLayoutsPlugin::getNumGlobalRenderInformationObjects() const
{
    return mGlobalRenderInformation.size();
}

/*
 * Returns a pointer to the local render information object with the given
 * index.
 * If the index is invalid, @c NULL is returned.
 */
GlobalRenderInformation* RenderListOfLayoutsPlugin::getRenderInformation(unsigned int index)
{
    if(index < mGlobalRenderInformation.size())
    {
         return static_cast<GlobalRenderInformation*>(mGlobalRenderInformation.get(index));   
    }
    else
    {
        return NULL;
    }
}

/*
 * Returns a const pointer to the local render information object with the given
 * index.
 * If the index is invalid, @c NULL is returned.
 */
const GlobalRenderInformation* RenderListOfLayoutsPlugin::getRenderInformation(unsigned int index) const
{
  if(index < mGlobalRenderInformation.size())
    {
         return static_cast<const GlobalRenderInformation*>(mGlobalRenderInformation.get(index));   
    }
    else
    {
        return NULL;
    }
}

/*
 * Returns a pointer to the local render information object with the given
 * id.
 * If no object with the given @p id exists, @c NULL is returned.
 */
GlobalRenderInformation* RenderListOfLayoutsPlugin::getRenderInformation(const std::string& id)
{
    GlobalRenderInformation* pResult=NULL;
    unsigned int i=0,iMax=mGlobalRenderInformation.size();
    while(i<iMax)
    {
        if(mGlobalRenderInformation.get(i)->getId()==id)
        {
            pResult=static_cast<GlobalRenderInformation*>(mGlobalRenderInformation.get(i));
            break;
        }
        ++i;
    }
    return pResult;
}

/*
 * Returns a const pointer to the local render information object with the given
 * id.
 * If no object with the given @p id exists, @c NULL is returned.
 */
const GlobalRenderInformation* RenderListOfLayoutsPlugin::getRenderInformation(const std::string& id) const
{
    const GlobalRenderInformation* pResult=NULL;
    unsigned int i=0,iMax=mGlobalRenderInformation.size();
    while(i<iMax)
    {
        if(mGlobalRenderInformation.get(i)->getId()==id)
        {
            pResult=static_cast<const GlobalRenderInformation*>(mGlobalRenderInformation.get(i));
            break;
        }
        ++i;
    }
    return pResult;
}

/*
 * Adds a copy of the given local render information object to the list of
 * local render information objects.
 */
void RenderListOfLayoutsPlugin::addGlobalRenderInformation(const GlobalRenderInformation* pLRI)
{

  mGlobalRenderInformation.appendAndOwn(new GlobalRenderInformation(*pLRI));
}

/*
 * Creates a new local render information object and adds it to the list.
 * The created object does not have a id and it is the responsibility of
 * the calling code to ensure that it gets one.
 * For constraints on the id, please consult the render information document.
 */
GlobalRenderInformation* RenderListOfLayoutsPlugin::createGlobalRenderInformation()
{
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    GlobalRenderInformation* pGRI=new GlobalRenderInformation(renderns);
 
    mGlobalRenderInformation.appendAndOwn(pGRI);
    delete renderns;
    return pGRI;
}

/*
 * Removed the render information with the given index from the list.
 * The removed object is returned. It is the responsibility of the calling
 * code to delete the object.
 * If the index is not valid, @c NULL is returned.
 */
GlobalRenderInformation* RenderListOfLayoutsPlugin::removeGlobalRenderInformation(unsigned int index) 
{
    if(index < mGlobalRenderInformation.size())
    {
        return static_cast<GlobalRenderInformation*>(mGlobalRenderInformation.remove(index));
    }
    else
    {
        return NULL;
    }
}

/*
 * Removed the render information with the given @p id from the list.
 * The removed object is returned. It is the responsibility of the calling
 * code to delete the object.
 * If an object with the given @p id does not exist, @c NULL is returned.
 */
GlobalRenderInformation* RenderListOfLayoutsPlugin::removeGlobalRenderInformation(const std::string& id) 
{
    unsigned int i=0,iMax=mGlobalRenderInformation.size();
    while(i<iMax)
    {
        if(mGlobalRenderInformation.get(i)->isSetId() && mGlobalRenderInformation.get(i)->getId()==id)
        {
            break;
        }
        ++i;
    }
    if(i!=iMax)
    {
        return removeGlobalRenderInformation(i);
    }
    else
    {
        return NULL;
    }
}


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
