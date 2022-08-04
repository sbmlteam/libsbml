/**
 * @file    RenderLayoutPlugin.cpp
 * @brief   Implementation of RenderLayoutPlugin, the plugin class of
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
#include <sbml/packages/render/extension/RenderLayoutPlugin.h>
#include <sbml/packages/render/common/RenderExtensionTypes.h>
#include <sbml/packages/render/util/RenderUtilities.h>


#include <sbml/packages/layout/sbml/Layout.h>

#include <sbml/util/ElementFilter.h>

#include <iostream>
using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN



  /*
  * Constructor
  */
  RenderLayoutPlugin::RenderLayoutPlugin (const std::string& uri, 
  const std::string &prefix,
  RenderPkgNamespaces *renderns)
  : SBasePlugin(uri,prefix, renderns)
  , mLocalRenderInformation(renderns)
{
}


/*
* Copy constructor. Creates a copy of this SBase object.
*/
RenderLayoutPlugin::RenderLayoutPlugin(const RenderLayoutPlugin& orig)
  : SBasePlugin(orig)
  , mLocalRenderInformation(orig.mLocalRenderInformation)
{
}


/*
* Destroy this object.
*/
RenderLayoutPlugin::~RenderLayoutPlugin () {}

/*
* Assignment operator for RenderLayoutPlugin.
*/
RenderLayoutPlugin& 
  RenderLayoutPlugin::operator=(const RenderLayoutPlugin& orig)
{
  if(&orig!=this)
  {
    this->SBasePlugin::operator =(orig);
  }    
  return *this;
}


/*
* Creates and returns a deep copy of this RenderLayoutPlugin object.
* 
* @return a (deep) copy of this RenderLayoutPlugin object
*/
RenderLayoutPlugin* 
  RenderLayoutPlugin::clone () const
{
  return new RenderLayoutPlugin(*this);  
}


List*
RenderLayoutPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mLocalRenderInformation, filter);

  return ret;
}


/** @cond doxygenLibsbmlInternal */
SBase*
  RenderLayoutPlugin::createObject(XMLInputStream& stream)
{
  SBase*        object = 0;

  const std::string&   name   = stream.peek().getName();
  const XMLNamespaces& xmlns  = stream.peek().getNamespaces();
  const std::string&   prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix)
  {
    if ( name == "listOfRenderInformation" ) 
    {
      //cout << "[DEBUG] LayoutModelPlugin::createObject create listOfLayouts" << endl;
      object = &mLocalRenderInformation;
    
      if (targetPrefix.empty())
      {
        //
        // prefix is empty when writing elements in layout extension.
        //
        mLocalRenderInformation.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    } 
  }    

  return object;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void 
RenderLayoutPlugin::writeAttributes (XMLOutputStream& stream) const
{
  //
  // This function is used only for SBML Level 2.
  //
  if ( getURI() != RenderExtension::getXmlnsL2() ) return;

  SBase *parent = const_cast<SBase*>(getParentSBMLObject());
  if (parent == NULL) 
    return;

  // when called this will serialize the annotation
  parent->getAnnotation();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
  RenderLayoutPlugin::writeElements (XMLOutputStream& stream) const
{
  if ( getURI() == RenderExtension::getXmlnsL2() ) return;
  if (mLocalRenderInformation.size() > 0 || mLocalRenderInformation.isSetDefaultValues())
    mLocalRenderInformation.write(stream);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

/* 
 * Parse L2 annotation if supported
 *
 */
void 
RenderLayoutPlugin::parseAnnotation(SBase *parentObject, XMLNode *annotation)
{
  mLocalRenderInformation.setSBMLDocument(mSBML);  
  parseLocalRenderAnnotation(annotation,(Layout*)parentObject);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Synchronizes the annotation of this SBML object.
 */
void
RenderLayoutPlugin::syncAnnotation (SBase *parentObject, XMLNode *pAnnotation)
{
  if(pAnnotation && pAnnotation->getNumChildren() > 0)
  {
      parentObject->removeTopLevelAnnotationElement("listOfRenderInformation", "", false);
  }

  // only do this for L1 and L2 documents
  if(getLevel() >= 3) 
    return;

  
  if (mLocalRenderInformation.size() == 0)
    return;
  
  XMLNode * render = parseLocalRenderInformation((Layout*)parentObject);
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
/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return @c true if the subclass read from the stream, false otherwise.
 */
bool
RenderLayoutPlugin::readOtherXML (SBase* parentObject, XMLInputStream& stream)
{
  // L2 render parsed by the annotation API 
  // @see parseAnnotation / syncAnnotation
  return false; 
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* default for components that have no required elements */
bool
  RenderLayoutPlugin::hasRequiredElements() const
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
  RenderLayoutPlugin::setSBMLDocument (SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);
  mLocalRenderInformation.setSBMLDocument(d);
  if (mLocalRenderInformation.isSetDefaultValues())
  {
    mLocalRenderInformation.getDefaultValues()->setSBMLDocument(d);
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
  RenderLayoutPlugin::connectToParent (SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);
  mLocalRenderInformation.connectToParent(sbase);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
* Enables/Disables the given package with child elements in this plugin
* object (if any).
*/
void
  RenderLayoutPlugin::enablePackageInternal(const std::string& pkgURI,
  const std::string& pkgPrefix, bool flag)
{
  mLocalRenderInformation.enablePackageInternal(pkgURI, pkgPrefix, flag);
}
/** @endcond */


/*
 * Returns a pointer to the list object that contains local render information.
 */
ListOfLocalRenderInformation* RenderLayoutPlugin::getListOfLocalRenderInformation()
{
    return &mLocalRenderInformation;
}

/*
 * Returns a const pointer to the list object that contains local render information.
 */
const ListOfLocalRenderInformation* RenderLayoutPlugin::getListOfLocalRenderInformation() const
{
    return &mLocalRenderInformation;
}


/*
 * Returns the number of local render information objects.
 */
unsigned int RenderLayoutPlugin::getNumLocalRenderInformationObjects() const
{
    return mLocalRenderInformation.size();
}

/*
 * Returns a pointer to the local render information object with the given
 * index.
 * If the index is invalid, @c NULL is returned.
 */
LocalRenderInformation* RenderLayoutPlugin::getRenderInformation(unsigned int index)
{
    if(index < mLocalRenderInformation.size())
    {
         return static_cast<LocalRenderInformation*>(mLocalRenderInformation.get(index));   
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
const LocalRenderInformation* RenderLayoutPlugin::getRenderInformation(unsigned int index) const
{
  if(index < mLocalRenderInformation.size())
    {
         return static_cast<const LocalRenderInformation*>(mLocalRenderInformation.get(index));   
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
LocalRenderInformation* RenderLayoutPlugin::getRenderInformation(const std::string& id)
{
    LocalRenderInformation* pResult=NULL;
    unsigned int i=0,iMax=mLocalRenderInformation.size();
    while(i<iMax)
    {
        if(mLocalRenderInformation.get(i)->getId()==id)
        {
            pResult=static_cast<LocalRenderInformation*>(mLocalRenderInformation.get(i));
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
const LocalRenderInformation* RenderLayoutPlugin::getRenderInformation(const std::string& id) const
{
    const LocalRenderInformation* pResult=NULL;
    unsigned int i=0,iMax=mLocalRenderInformation.size();
    while(i<iMax)
    {
        if(mLocalRenderInformation.get(i)->getId()==id)
        {
            pResult=static_cast<const LocalRenderInformation*>(mLocalRenderInformation.get(i));
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
void RenderLayoutPlugin::addLocalRenderInformation(const LocalRenderInformation* pLRI)
{

  mLocalRenderInformation.appendAndOwn(new LocalRenderInformation(*pLRI));
}

/*
 * Creates a new local render information object and adds it to the list.
 * The created object does not have a id and it is the responsibility of
 * the calling code to ensure that it gets one.
 * For constraints on the id, please consult the render information document.
 */
LocalRenderInformation* RenderLayoutPlugin::createLocalRenderInformation()
{
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    LocalRenderInformation* pGRI=new LocalRenderInformation(renderns);

    mLocalRenderInformation.appendAndOwn(pGRI);
    delete renderns;
    return pGRI;
}

/*
 * Removed the render information with the given index from the list.
 * The removed object is returned. It is the responsibility of the calling
 * code to delete the object.
 * If the index is not valid, @c NULL is returned.
 */
LocalRenderInformation* RenderLayoutPlugin::removeLocalRenderInformation(unsigned int index) 
{
    if(index < mLocalRenderInformation.size())
    {
        return static_cast<LocalRenderInformation*>(mLocalRenderInformation.remove(index));
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
LocalRenderInformation* RenderLayoutPlugin::removeLocalRenderInformation(const std::string& id) 
{
    unsigned int i=0,iMax=mLocalRenderInformation.size();
    while(i<iMax)
    {
        if(mLocalRenderInformation.get(i)->isSetId() && mLocalRenderInformation.get(i)->getId()==id)
        {
            break;
        }
        ++i;
    }
    if(i!=iMax)
    {
        return removeLocalRenderInformation(i);
    }
    else
    {
        return NULL;
    }
}

/** @cond doxygenLibsbmlInternal */

bool
RenderLayoutPlugin::accept (SBMLVisitor& v) const
{
  return true;
}

/** @endcond */




#ifdef ANNOTATION

/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return @c true if the subclass read from the stream, false otherwise.
 */
bool
RenderLayoutPlugin::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const std::string& name = stream.peek().getName();

  // This has to do additional work for reading annotations, so the code

  if (name == "annotation")
  {
    if (mAnnotation)
    {
      if (getLevel() < 3) 
      {
        logError(NotSchemaConformant, getLevel(), getVersion(),
          "Only one <annotation> element is permitted inside a "
          "particular containing element.");
      }
      else
      {
        logError(MultipleAnnotations, getLevel(), getVersion());
      }
    }

    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    // only parse layout from annotations for Level 2 and below
    if(getLevel() < 3)
    {
      mLocalRenderInformation.setSBMLDocument(mSBML);  
      parseLocalRenderAnnotation(mAnnotation,this);
    }
  
    read = true;
  }

  return read;
}
/** @endcond */

/*
 * Sets the annotation of this SBML object to a copy of annotation.
 */
int
RenderLayoutPlugin::setAnnotation (const XMLNode* annotation)
{
  int success = SBase::setAnnotation(annotation);
  if(success == LIBSBML_OPERATION_SUCCESS && getLevel() < 3)
  {
      for(unsigned int i=0; i < mLocalRenderInformation.size(); i++)
      {
          LocalRenderInformation* lr = static_cast<LocalRenderInformation*>(mLocalRenderInformation.remove(0));
          delete lr;
      }

      if(mAnnotation)
      {
          // parse mAnnotation (if any) and set mLayouts 
          mLocalRenderInformation.setSBMLDocument(mSBML);  
          parseLocalRenderAnnotation(mAnnotation,this);
      }
  }
  return success;
}


/*
 * Appends annotation to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
int
RenderLayoutPlugin::appendAnnotation (const XMLNode* annotation)
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
    mLocalRenderInformation.setSBMLDocument(mSBML);  
    parseLocalRenderAnnotation(new_annotation,this);

  }

  success = SBase::appendAnnotation(new_annotation);
  delete new_annotation;

  return success;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Synchronizes the annotation of this SBML object.
 */
void
RenderLayoutPlugin::syncAnnotation ()
{
  SBase::syncAnnotation();
  // only write the render information to an annotation
  // if we are in a L1 or L2 document
  if(getLevel() < 3)
  {
      if(mAnnotation)
      {
          XMLNode* new_annotation = deleteLocalRenderAnnotation(mAnnotation);
          delete mAnnotation;
          mAnnotation = new_annotation;
      }
      if (getListOfLocalRenderInformation()->size()!=0)
      {
          XMLNode * render = parseLocalRenderInformation(this);

          if (render)
          {
              if (!mAnnotation)
              {
                  mAnnotation = render;
              }
              else
              {
                  if (mAnnotation->isEnd())
                  {
                      mAnnotation->unsetEnd();
                  }
                  mAnnotation->addChild(render->getChild(0));
                  delete render;
              }
          }
      }
  }
}
/** @endcond */
#endif

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
