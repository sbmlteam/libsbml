/**
 * @file    FbcModelPlugin.cpp
 * @brief   Implementation of FbcModelPlugin, the plugin class of
 *          the fbc package for the Model element.
 * @author  Frank T. Bergmann
 *
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


#include <sbml/packages/fbc/extension/FbcModelPlugin.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


#include <iostream>
using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new FbcModelPlugin
 */
FbcModelPlugin::FbcModelPlugin(const std::string& uri,  
                               const std::string& prefix, 
                               FbcPkgNamespaces* fbcns) :
  FbcSBasePlugin(uri, prefix, fbcns)
  , mStrict (false)
  , mIsSetStrict (false)
  , mObjectives (fbcns)
  , mGeneProducts (fbcns)
  , mBounds(fbcns)
  , mAssociations(fbcns)
  , mUserDefinedConstraints (fbcns)
{
  // connect child elements to this element.
  connectToChild();
}


/*
 * Copy constructor for FbcModelPlugin.
 */
FbcModelPlugin::FbcModelPlugin(const FbcModelPlugin& orig)
  : FbcSBasePlugin(orig)
  , mStrict (orig.mStrict)
  , mIsSetStrict (orig.mIsSetStrict)
  , mObjectives (orig.mObjectives)
  , mGeneProducts (orig.mGeneProducts)
  , mBounds(orig.mBounds)
  , mAssociations(orig.mAssociations)
  , mUserDefinedConstraints ( orig.mUserDefinedConstraints )
{
  // connect child elements to this element.
  connectToChild();
}


/*
 * Assignment operator for FbcModelPlugin.
 */
FbcModelPlugin& 
FbcModelPlugin::operator=(const FbcModelPlugin& rhs)
{
  if (&rhs != this)
  {
    this->FbcSBasePlugin::operator=(rhs);
    mStrict  = rhs.mStrict;
    mIsSetStrict  = rhs.mIsSetStrict;
    mBounds       = rhs.mBounds;
    mObjectives   = rhs.mObjectives;
    mAssociations = rhs.mAssociations;
    mGeneProducts  = rhs.mGeneProducts;
    mUserDefinedConstraints = rhs.mUserDefinedConstraints;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this FbcModelPlugin object.
 */
FbcModelPlugin* 
FbcModelPlugin::clone () const
{
  return new FbcModelPlugin(*this);
}


/*
 * Destructor for FbcModelPlugin.
 */
FbcModelPlugin::~FbcModelPlugin()
{
}

//---------------------------------------------------------------
//
// overridden virtual functions for read/write/check
//
//---------------------------------------------------------------


LIBSBML_EXTERN
void
parseFbcAnnotation(XMLNode * annotation, ListOfGeneAssociations& associations,
                   FbcPkgNamespaces* fbcns)
{
  
  if (!annotation) return;
  
  const string&  name = annotation->getName();
  const XMLNode*  plOGATop = NULL;
  GeneAssociation* ga;
  unsigned int n = 0;
  
  // need to find the layout description opening annotation
  if (name == "annotation" && annotation->getNumChildren() > 0)
  {
    while (n < annotation->getNumChildren())
    {
      const string &name1 = annotation->getChild(n).getName();
      if (name1 == "listOfGeneAssociations") // also check the namespace
      {
        const XMLNamespaces& namespaces=annotation->getChild(n).getNamespaces();
        if(namespaces.getIndex(FbcExtension::getXmlnsL3V1V1())!=-1)
        {
          plOGATop = &(annotation->getChild(n));
          break;
        }
      }
      n++;
    }
  }
  
  // find qualifier nodes and create
  
  
  n = 0;
  if (plOGATop)
  {
    while (n < plOGATop->getNumChildren())
    {
      const string &name2 = plOGATop->getChild(n).getName();
      
      if (name2 == "annotation")
      {
        const XMLNode &annot = plOGATop->getChild(n);
        associations.setAnnotation(&annot);
      }
      
      if (name2 == "geneAssociation")
      {
        ga = new GeneAssociation(plOGATop->getChild(n), fbcns);
        associations.appendAndOwn(ga);
      }
      
      n++;
    }
  }
}


LIBSBML_EXTERN
XMLNode* deleteFbcAnnotation(XMLNode* pAnnotation)
{
  if (pAnnotation == NULL) 
    return NULL;
  
  const string&  name = pAnnotation->getName();
  unsigned int n = 0;
  
  if (!(name == "annotation" && pAnnotation->getNumChildren() > 0))
    return pAnnotation;

  
  // need to find each annotation and remove it if it is a list of gene associations
  while (n < pAnnotation->getNumChildren())
  {
    const string &name1 = pAnnotation->getChild(n).getName();
    if (name1 == "listOfGeneAssociations" ||
        pAnnotation->getChild(n).getNamespaces().getIndex(FbcExtension::getXmlnsL3V1V1()) !=-1)
    {
      // delete the anotation
      delete pAnnotation->removeChild(n);

      continue;
    }
    n++;
  }
  
  return pAnnotation;
}




  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
FbcModelPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  FbcSBasePlugin::addExpectedAttributes(attributes);

  attributes.add("strict");
}


  /** @endcond */


/** @cond doxygenLibsbmlInternal */
void 
FbcModelPlugin::parseAnnotation(SBase *parentObject, XMLNode *pAnnotation)
{
  if (getPackageVersion() == 3) 
  {
    FbcSBasePlugin::parseAnnotation(parentObject, pAnnotation);
    return;
  }
  if (getPackageVersion() > 1)
    return;

  // read gene associations from annotation in l3v1v1
  mAssociations.setSBMLDocument(mSBML); 
  // don't read if we have an invalid node or already a gene associations object
  if (pAnnotation == NULL || mAssociations.size() > 0)
    return;

  // annotation element has been parsed by the parent element
  // (Model) of this plugin object, thus the annotation element 
  // set to the above pAnnotation variable is parsed in this block.
  
  XMLNode& listOfGeneAssociations = pAnnotation->getChild("listOfGeneAssociations");
  if (listOfGeneAssociations.getNumChildren() == 0)
    return;
 
  // read the xml node, overriding that all errors are flagged as 
  // warnings
  mAssociations.read(listOfGeneAssociations, LIBSBML_OVERRIDE_WARNING);
  // remove listOfLayouts annotation  
  parentObject->removeTopLevelAnnotationElement("listOfGeneAssociations", "", false);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
#ifndef ANNOATION
bool
FbcModelPlugin::readOtherXML (SBase* /*parentObject*/, XMLInputStream& /*stream*/)
{
  return false;
}
#else
bool
FbcModelPlugin::readOtherXML (SBase* parentObject, XMLInputStream& stream)
{
  bool readAnnotationFromStream = false;
  const string& name = stream.peek().getName();
  
  if (!(name.empty()) && name != "annotation")
  {
    return readAnnotationFromStream;
  }
  
  try
  {   
    XMLNode *pAnnotation = parentObject->getAnnotation();
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    
    if (!pAnnotation)
    {
      //
      // (NOTES)
      //
      // annotation element has not been parsed by the parent element
      // (Model) of this plugin object, thus annotation element is
      // parsed via the given XMLInputStream object in this block.
      //
      
      const string& name = stream.peek().getName();
      
    
      if (name == "annotation")
      {
        pAnnotation = new XMLNode(stream);
        
        parseFbcAnnotation(pAnnotation, mAssociations, fbcns);
        
        if (mAssociations.size() > 0)
        {
          //
          // Removes the annotation for layout extension from the annotation
          // of parent element (pAnnotation) and then set the new annotation
          // (newAnnotation) to the parent element.
          //
          XMLNode *newAnnotation = deleteFbcAnnotation(pAnnotation);
          parentObject->setAnnotation(newAnnotation);
          delete newAnnotation;
        }
        else
        {
          //
          // No layout annotation is included in the read annotation
          // (pAnnotation) and thus just set the annotation to the parent
          // element.
          //
          parentObject->setAnnotation(pAnnotation);
        }
        
        delete pAnnotation;
        
        readAnnotationFromStream = true;
      }
      
    }
    else if (mAssociations.size() == 0)
    {
      //
      // (NOTES)
      //
      // annotation element has been parsed by the parent element
      // (Model) of this plugin object, thus the annotation element
      // set to the above pAnnotation variable is parsed in this block.
      //
      parseFbcAnnotation(pAnnotation, mAssociations, fbcns);
      
      if (mAssociations.size() > 0)
      {
        //
        // Removes the annotation for layout extension from the annotation
        // of parent element (pAnnotation) and then set the new annotation
        // (newAnnotation) to the parent element.
        //
        XMLNode *newAnnotation = deleteFbcAnnotation(pAnnotation);
        parentObject->setAnnotation(newAnnotation);
      }
      
      readAnnotationFromStream = true;
    }
    
    delete fbcns;
  }
  catch(...)
  {
    // an exception occured, most likely becase a namespace constructor
    // threw an exception, catching this here, and return false, to indicate
    // that the annotation wasn't read. 
    readAnnotationFromStream = false;
  }
  
  return readAnnotationFromStream;
}
#endif
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
FbcModelPlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  FbcSBasePlugin::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = (int)numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("fbc", FbcUnknown,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", FbcUnknown,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        const std::string details = 
          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(NotSchemaConformant);
        getErrorLog()->logPackageError("fbc", FbcUnknown,
          getPackageVersion(), sbmlLevel, sbmlVersion, details,
          getLine(), getColumn());
      }
    }
  }

  //
  // strict bool   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetStrict = attributes.readInto("strict", mStrict);

  if (mIsSetStrict == false && getPackageVersion() > 1)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("fbc", FbcModelStrictMustBeBoolean,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Fbc attribute 'strict' is missing from <Model> object.";
        getErrorLog()->logPackageError("fbc", FbcModelMustHaveStrict,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
void
FbcModelPlugin::writeAttributes (XMLOutputStream& stream) const
{
  FbcSBasePlugin::writeAttributes(stream);

  if (isSetStrict() == true && getPackageVersion() != 1 && getLevel() == 3)
    stream.writeAttribute("strict", getPrefix(), mStrict);

  Model *parent = static_cast<Model*>(const_cast<SBase*>(getParentSBMLObject()));
  if (parent == NULL) return;


  XMLNode *parentAnnotation = parent->getAnnotation();
  if (parentAnnotation != NULL && parentAnnotation->getNumChildren() > 0)
  {
    deleteFbcAnnotation(parentAnnotation);
  }

  XMLToken ann_token = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
  XMLNode* annt = new XMLNode(ann_token);



  if (mAssociations.size() > 0)
  {
    XMLAttributes loga_attr = XMLAttributes();
    loga_attr.add("xmlns", FbcExtension::getXmlnsL3V1V1());
    XMLToken loga_token = XMLToken(XMLTriple("listOfGeneAssociations", FbcExtension::getXmlnsL3V1V1(), ""), loga_attr);
    XMLNode loga = XMLNode(loga_token);

    for (unsigned int i = 0; i < mAssociations.size(); ++i)
      loga.addChild(mAssociations.get(i)->toXML());

    // then add the ones toXML()
    annt->addChild(loga);
  }


  if (annt && annt->getNumChildren() > 0)
  {
    parent->appendAnnotation(annt);
  }
  delete annt;
}


  /** @endcond */


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------


/*
 * Returns the ListOfFluxBounds in this plugin object.
 *
 * @return ListOfFluxBounds object in this plugin object.
 */
const ListOfFluxBounds* 
  FbcModelPlugin::getListOfFluxBounds () const
{
  return &mBounds;
}


/*
 * Returns the ListOfFluxBounds in this plugin object.
 *
 * @return ListOfFluxBounds object in this plugin object.
 */
ListOfFluxBounds* 
  FbcModelPlugin::getListOfFluxBounds ()
{
  return &mBounds;
}


/*
 * Returns the FluxBound object that belongs to the given index. If the
 * index is invalid, @c NULL is returned.
 *
 * @param n the index number of the FluxBound to get.
 *
 * @return the nth FluxBound in the ListOfFluxBounds.
 * If the index @p n is invalid, @c NULL is returned.
 */
const FluxBound* 
  FbcModelPlugin::getFluxBound (unsigned int n) const
{
  return static_cast<const FluxBound*>(mBounds.get(n));
}


/*
 * Returns the FluxBound object that belongs to the given index. If the
 * index is invalid, @c NULL is returned.
 *
 * @param n the index number of the FluxBound to get.
 *
 * @return the nth FluxBound in the ListOfFluxBounds.
 * If the index @p n is invalid, @c NULL is returned.
 */
FluxBound* 
  FbcModelPlugin::getFluxBound (unsigned int n)
{
  return static_cast<FluxBound*>(mBounds.get(n));
}


/*
 * Returns the FluxBound object based on its identifier.
 *
 * @param sid a string representing the identifier 
 * of the FluxBound to get.
 * 
 * @return FluxBound in the ListOfFluxBounds with the given @p id
 * or NULL if no such FluxBound exists.
 *
 * @see get(unsigned int n)
 * @see size()
 */
FluxBound* 
  FbcModelPlugin::getFluxBound (const std::string& sid)
{
  return static_cast<FluxBound*>(mBounds.get(sid));
}


/*
 * Returns the FluxBound object based on its identifier.
 *
 * @param sid a string representing the identifier 
 * of the FluxBound to get.
 * 
 * @return FluxBound in the ListOfFluxBounds with the given @p id 
 * or NULL if no such FluxBound exists.
 *
 * @see get(unsigned int n)
 * @see size()
 */
const FluxBound* 
  FbcModelPlugin::getFluxBound (const std::string& sid) const
{

  return static_cast<const FluxBound*>(mBounds.get(sid));

}

/*
 * Adds a copy of the given FluxBound object to the list of FluxBounds.
 *
 * @param bound the FluxBound object to be added to the list of FluxBounds.
 *
 * @copydetails doc_returns_success_code
 * @li LIBSBML_OPERATION_SUCCESS
 */ 
int 
  FbcModelPlugin::addFluxBound (const FluxBound* bound)
{
  if (!bound)
  {
    return LIBSBML_OPERATION_FAILED;
  }    
  else if (!bound->hasRequiredElements())
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != bound->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != bound->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != bound->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mBounds.append(bound);
  }
}


/*
 * Creates a new FluxBound object and adds it to the list of FluxBound objects
 * and returns it.
 *
 * @return a newly created FluxBound object
 */
FluxBound* 
  FbcModelPlugin::createFluxBound()
{
  FluxBound* bound = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    bound = new FluxBound(fbcns);
    delete fbcns;
  }
  catch(...)
  {
    /* 
    * NULL will be returned if the mSBMLNS is invalid (basically this
    * should not happen) or some exception is thrown (e.g. std::bad_alloc)
    *
    * (Maybe this should be changed so that caller can detect what kind 
    *  of error happened in this function.)
    */
  }

  if (bound) mBounds.appendAndOwn(bound);

  return bound;
}


/*
 * Removes the nth FluxBound object from this plugin object and
 * returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for
 *  deleting it.
 *
 * @param n the index of the FluxBound object to remove.
 *
 * @return the FluxBound object removed.  As mentioned above, the 
 * caller owns the returned object. @c NULL is returned if the 
 * given index is out of range.
 */
FluxBound* 
  FbcModelPlugin::removeFluxBound (unsigned int n)
{
  return static_cast<FluxBound*>(mBounds.remove(n));
}


/*
 * Removes the FluxBound object with the given @p id attribute from 
 * this plugin object and returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for
 * deleting it.
 *
 * @param sid the id attribute of the FluxBound object to remove.
 *
 * @return the FluxBound object removed.  As mentioned above, the 
 * caller owns the returned object. @c NULL is returned if the 
 * given index is out of range.
 */
FluxBound* 
  FbcModelPlugin::removeFluxBound (const std::string& sid)
{
  return static_cast<FluxBound*>(mBounds.remove(sid));
}


/*
 * Returns the number of FluxBound object in this plugin object.
 *
 * @return the number of FluxBound object in this plugin object.
 */
unsigned int 
  FbcModelPlugin::getNumFluxBounds() const
{
  return mBounds.size();
}




/*
 * Returns the value of the "strict" attribute of this FbcModelPlugin.
 */
bool
FbcModelPlugin::getStrict() const
{
  return mStrict;
}


/*
 * Returns true/false if strict is set.
 */
bool
FbcModelPlugin::isSetStrict() const
{
  return mIsSetStrict;
}


/*
 * Sets strict and returns value indicating success.
 */
int
FbcModelPlugin::setStrict(bool strict)
{
  mStrict = strict;
  mIsSetStrict = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets strict and returns value indicating success.
 */
int
FbcModelPlugin::unsetStrict()
{
  mStrict = false;
  mIsSetStrict = false;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the  "ListOfObjectives" in this FbcModelPlugin object.
 */
const ListOfObjectives*
FbcModelPlugin::getListOfObjectives() const
{
  return &mObjectives;
}


/*
 * Returns the  "ListOfObjectives" in this FbcModelPlugin object.
 */
ListOfObjectives*
FbcModelPlugin::getListOfObjectives()
{
  return &mObjectives;
}


/*
 * Removes the nth Objective from the ListOfObjectives.
 */
Objective*
FbcModelPlugin::removeObjective(unsigned int n)
{
  return mObjectives.remove(n);
}


/*
 * Removes the a Objective with given id from the ListOfObjectives.
 */
Objective*
FbcModelPlugin::removeObjective(const std::string& sid)
{
  return mObjectives.remove(sid);
}


/*
 * Return the nth Objective in the ListOfObjectives within this FbcModelPlugin.
 */
Objective*
FbcModelPlugin::getObjective(unsigned int n)
{
  return mObjectives.get(n);
}


/*
 * Return the nth Objective in the ListOfObjectives within this FbcModelPlugin.
 */
const Objective*
FbcModelPlugin::getObjective(unsigned int n) const
{
  return mObjectives.get(n);
}


/*
 * Return a Objective from the ListOfObjectives by id.
 */
Objective*
FbcModelPlugin::getObjective(const std::string& sid)
{
  return mObjectives.get(sid);
}


/*
 * Return a Objective from the ListOfObjectives by id.
 */
const Objective*
FbcModelPlugin::getObjective(const std::string& sid) const
{
  return mObjectives.get(sid);
}


/*
 * Adds a copy the given Objective to this FbcModelPlugin.
 *
 * @param o the Objective object to add.
 *
 * @copydetails doc_returns_success_code
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
FbcModelPlugin::addObjective(const Objective* o)
{
  if (o == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (o->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != o->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != o->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != o->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mObjectives.append(o);
  }
}


/*
 * Get the number of Objective objects in this FbcModelPlugin.
 *
 * @return the number of Objective objects in this FbcModelPlugin
 */
unsigned int
FbcModelPlugin::getNumObjectives() const
{
  return mObjectives.size();
}


/*
 * Creates a new Objective object, adds it to this FbcModelPlugin's
 * FbcModelPlugin and returns the Objective object created. 
 *
 * @return a new Objective object instance
 *
 * @see addObjective(const Objective* o)
 */
Objective*
FbcModelPlugin::createObjective()
{
  Objective* o = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    o = new Objective(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(o != NULL)
  {
    mObjectives.appendAndOwn(o);
  }

  return o;
}


/*
 * Returns the  "ListOfGeneProducts" in this FbcModelPlugin object.
 */
const ListOfGeneProducts*
FbcModelPlugin::getListOfGeneProducts() const
{
  return &mGeneProducts;
}


/*
 * Returns the  "ListOfGeneProducts" in this FbcModelPlugin object.
 */
ListOfGeneProducts*
FbcModelPlugin::getListOfGeneProducts()
{
  return &mGeneProducts;
}


/*
 * Removes the nth GeneProduct from the ListOfGeneProducts.
 */
GeneProduct*
FbcModelPlugin::removeGeneProduct(unsigned int n)
{
  return mGeneProducts.remove(n);
}


/*
 * Removes the a GeneProduct with given id from the ListOfGeneProducts.
 */
GeneProduct*
FbcModelPlugin::removeGeneProduct(const std::string& sid)
{
  return mGeneProducts.remove(sid);
}


/*
 * Return the nth GeneProduct in the ListOfGeneProducts within this FbcModelPlugin.
 */
GeneProduct*
FbcModelPlugin::getGeneProduct(unsigned int n)
{
  return mGeneProducts.get(n);
}


/*
 * Return the nth GeneProduct in the ListOfGeneProducts within this FbcModelPlugin.
 */
const GeneProduct*
FbcModelPlugin::getGeneProduct(unsigned int n) const
{
  return mGeneProducts.get(n);
}


/*
 * Return a GeneProduct from the ListOfGeneProducts by id.
 */
GeneProduct*
FbcModelPlugin::getGeneProduct(const std::string& sid)
{
  return mGeneProducts.get(sid);
}

GeneProduct* 
FbcModelPlugin::getGeneProductByLabel(const std::string& label)
{
  for (unsigned int i = 0; i < mGeneProducts.size(); ++i)
  {
    GeneProduct* current = mGeneProducts.get(i);
    if (current != NULL && current->getLabel() == label)
      return current;
  }
  return NULL;
}


/*
 * Return a GeneProduct from the ListOfGeneProducts by id.
 */
const GeneProduct*
FbcModelPlugin::getGeneProduct(const std::string& sid) const
{
  return mGeneProducts.get(sid);
}


/*
 * Adds a copy the given GeneProduct to this FbcModelPlugin.
 *
 * @param gp the GeneProduct object to add.
 *
   * @copydetails doc_returns_success_code
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
FbcModelPlugin::addGeneProduct(const GeneProduct* gp)
{
  if (gp == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gp->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != gp->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != gp->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != gp->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mGeneProducts.append(gp);
  }
}


/*
 * Get the number of GeneProduct objects in this FbcModelPlugin.
 *
 * @return the number of GeneProduct objects in this FbcModelPlugin
 */
unsigned int
FbcModelPlugin::getNumGeneProducts() const
{
  return mGeneProducts.size();
}


/*
 * Creates a new GeneProduct object, adds it to this FbcModelPlugin's
 * FbcModelPlugin and returns the GeneProduct object created. 
 *
 * @return a new GeneProduct object instance
 *
 * @see addGeneProduct(const GeneProduct* gp)
 */
GeneProduct*
FbcModelPlugin::createGeneProduct()
{
  GeneProduct* gp = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    gp = new GeneProduct(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(gp != NULL)
  {
    mGeneProducts.appendAndOwn(gp);
  }

  return gp;
}



/* 
 * Returns the current active objective. 
 */
Objective* 
FbcModelPlugin::getActiveObjective()
{
  return getObjective(getActiveObjectiveId());
}

/* 
 * Returns the current active objective. 
 */
const Objective *
FbcModelPlugin::getActiveObjective() const
{
  return getObjective(getActiveObjectiveId());
}

/* 
 * Sets the id of the active objective.
 */
int 
FbcModelPlugin::setActiveObjectiveId(const std::string& objectiveId)
{
  return mObjectives.setActiveObjective(objectiveId);
}

/* 
 * returns the id of the current active objective.
 */  
std::string 
FbcModelPlugin::getActiveObjectiveId() const
{
  return mObjectives.getActiveObjective();
}

/* 
 * Unsets the active objective.
 */  
int 
FbcModelPlugin::unsetActiveObjectiveId()
{
  mObjectives.unsetActiveObjective();
  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Returns the ListOfObjectives in this plugin object.
 *
 * @return ListOfObjectives object in this plugin object.
 */
const ListOfGeneAssociations* 
  FbcModelPlugin::getListOfGeneAssociations () const
{
  return &mAssociations;
}

/*
 * Returns the ListOfGeneAssociations in this plugin object.
 *
 * @return ListOfGeneAssociations object in this plugin object.
 */
ListOfGeneAssociations* 
  FbcModelPlugin::getListOfGeneAssociations ()
{
  return &mAssociations;
}

/*
 * Returns the GeneAssociation object that belongs to the given index. If the
 * index is invalid, @c NULL is returned.
 *
 * @param n the index number of the GeneAssociation to get.
 *
 * @return the nth GeneAssociation in the ListOfGeneAssociations.
 * If the index @p n is invalid, @c NULL is returned.
 */
const GeneAssociation* 
  FbcModelPlugin::getGeneAssociation (unsigned int n) const
{
  return static_cast<const GeneAssociation*>(mAssociations.get(n));
}

/*
 * Returns the GeneAssociation object that belongs to the given index. If the
 * index is invalid, @c NULL is returned.
 *
 * @param n the index number of the GeneAssociation to get.
 *
 * @return the nth GeneAssociation in the ListOfGeneAssociations.
 * If the index @p n is invalid, @c NULL is returned.
 */
GeneAssociation* 
  FbcModelPlugin::getGeneAssociation (unsigned int n)
{
  return static_cast<GeneAssociation*>(mAssociations.get(n));
}

/*
 * Returns the GeneAssociation object based on its identifier.
 *
 * @param sid a string representing the identifier 
 * of the GeneAssociation to get.
 * 
 * @return GeneAssociation in the ListOfGeneAssociations with the given @p id
 * or NULL if no such GeneAssociation exists.
 *
 * @see get(unsigned int n)
 * @see size()
 */
GeneAssociation* 
  FbcModelPlugin::getGeneAssociation (const std::string& sid)
{
  return static_cast<GeneAssociation*>(mAssociations.get(sid));
}

/*
 * Returns the GeneAssociation object based on its identifier.
 *
 * @param sid a string representing the identifier 
 * of the GeneAssociation to get.
 * 
 * @return GeneAssociation in the ListOfGeneAssociations with the given @p id 
 * or NULL if no such GeneAssociation exists.
 *
 * @see get(unsigned int n)
 * @see size()
 */
const GeneAssociation* 
  FbcModelPlugin::getGeneAssociation (const std::string& sid) const
{
  return static_cast<const GeneAssociation*>(mAssociations.get(sid));
}

/*
 * Adds a copy of the given GeneAssociation object to the list of GeneAssociations.
 *
 * @param association the GeneAssociation object to be added to the list of GeneAssociations.
 *
 * @copydetails doc_returns_success_code
 * @li LIBSBML_OPERATION_SUCCESS
 */ 
int 
  FbcModelPlugin::addGeneAssociation (const GeneAssociation* association)
{
  if (!association)
  {
    return LIBSBML_OPERATION_FAILED;
  }    
  else if (!association->hasRequiredElements())
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != association->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != association->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != association->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mAssociations.append(association);
  }
}

/*
 * Creates a new GeneAssociation object and adds it to the list of GeneAssociation objects
 * and returns it.
 *
 * @return a newly created GeneAssociation object
 */
GeneAssociation* 
  FbcModelPlugin::createGeneAssociation()
{
  GeneAssociation* association = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    association = new GeneAssociation(fbcns);
    delete fbcns;
  }
  catch(...)
  {
    /* 
    * NULL will be returned if the mSBMLNS is invalid (basically this
    * should not happen) or some exception is thrown (e.g. std::bad_alloc)
    *
    * (Maybe this should be changed so that caller can detect what kind 
    *  of error happened in this function.)
    */
  }

  if (association) mAssociations.appendAndOwn(association);

  return association;
}

/*
 * Removes the nth GeneAssociation object from this plugin object and
 * returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for
 *  deleting it.
 *
 * @param n the index of the GeneAssociation object to remove.
 *
 * @return the GeneAssociation object removed.  As mentioned above, the 
 * caller owns the returned object. @c NULL is returned if the 
 * given index is out of range.
 */
GeneAssociation* 
  FbcModelPlugin::removeGeneAssociation (unsigned int n)
{
  return static_cast<GeneAssociation*>(mAssociations.remove(n));
}

/*
 * Removes the GeneAssociation object with the given @p id attribute from 
 * this plugin object and returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for
 * deleting it.
 *
 * @param sid the id attribute of the GeneAssociation object to remove.
 *
 * @return the GeneAssociation object removed.  As mentioned above, the 
 * caller owns the returned object. @c NULL is returned if the 
 * given index is out of range.
 */
GeneAssociation* 
  FbcModelPlugin::removeGeneAssociation (const std::string& sid)
{
  return static_cast<GeneAssociation*>(mAssociations.remove(sid));
}

/*
 * Returns the number of GeneAssociation object in this plugin object.
 *
 * @return the number of GeneAssociation object in this plugin object.
 */
int 
  FbcModelPlugin::getNumGeneAssociations() const
{
  return (int)mAssociations.size();
}

/*
 * Returns the ListOfUserDefinedConstraints from this FbcModelPlugin.
 */
const ListOfUserDefinedConstraints*
FbcModelPlugin::getListOfUserDefinedConstraints() const
{
  return &mUserDefinedConstraints;
}


/*
 * Returns the ListOfUserDefinedConstraints from this FbcModelPlugin.
 */
ListOfUserDefinedConstraints*
FbcModelPlugin::getListOfUserDefinedConstraints()
{
  return &mUserDefinedConstraints;
}


/*
 * Get an UserDefinedConstraint from the FbcModelPlugin.
 */
UserDefinedConstraint*
FbcModelPlugin::getUserDefinedConstraint(unsigned int n)
{
  return static_cast< UserDefinedConstraint*>(mUserDefinedConstraints.get(n));
}


/*
 * Get an UserDefinedConstraint from the FbcModelPlugin.
 */
const UserDefinedConstraint*
FbcModelPlugin::getUserDefinedConstraint(unsigned int n) const
{
  return static_cast<const
    UserDefinedConstraint*>(mUserDefinedConstraints.get(n));
}


/*
 * Get an UserDefinedConstraint from the FbcModelPlugin based on its
 * identifier.
 */
UserDefinedConstraint*
FbcModelPlugin::getUserDefinedConstraint(const std::string& sid)
{
  return static_cast<
    UserDefinedConstraint*>(mUserDefinedConstraints.get(sid));
}


/*
 * Get an UserDefinedConstraint from the FbcModelPlugin based on its
 * identifier.
 */
const UserDefinedConstraint*
FbcModelPlugin::getUserDefinedConstraint(const std::string& sid) const
{
  return static_cast<const
    UserDefinedConstraint*>(mUserDefinedConstraints.get(sid));
}


/*
 * Get an UserDefinedConstraint from the FbcModelPlugin based on the LowerBound
 * to which it refers.
 */
const UserDefinedConstraint*
FbcModelPlugin::getUserDefinedConstraintByLowerBound(const std::string& sid)
  const
{
  return mUserDefinedConstraints.getByLowerBound(sid);
}


/*
 * Get an UserDefinedConstraint from the FbcModelPlugin based on the LowerBound
 * to which it refers.
 */
UserDefinedConstraint*
FbcModelPlugin::getUserDefinedConstraintByLowerBound(const std::string& sid)
{
  return mUserDefinedConstraints.getByLowerBound(sid);
}


/*
 * Get an UserDefinedConstraint from the FbcModelPlugin based on the UpperBound
 * to which it refers.
 */
const UserDefinedConstraint*
FbcModelPlugin::getUserDefinedConstraintByUpperBound(const std::string& sid)
  const
{
  return mUserDefinedConstraints.getByUpperBound(sid);
}


/*
 * Get an UserDefinedConstraint from the FbcModelPlugin based on the UpperBound
 * to which it refers.
 */
UserDefinedConstraint*
FbcModelPlugin::getUserDefinedConstraintByUpperBound(const std::string& sid)
{
  return mUserDefinedConstraints.getByUpperBound(sid);
}


/*
 * Adds a copy of the given UserDefinedConstraint to this FbcModelPlugin.
 */
int
FbcModelPlugin::addUserDefinedConstraint(const UserDefinedConstraint* udc)
{
  if (udc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (udc->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != udc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != udc->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != udc->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else if (udc->isSetId() && (mUserDefinedConstraints.get(udc->getId())) !=
    NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mUserDefinedConstraints.append(udc);
  }
}


/*
 * Get the number of UserDefinedConstraint objects in this FbcModelPlugin.
 */
unsigned int
FbcModelPlugin::getNumUserDefinedConstraints() const
{
  return mUserDefinedConstraints.size();
}


/*
 * Creates a new UserDefinedConstraint object, adds it to this FbcModelPlugin
 * object and returns the UserDefinedConstraint object created.
 */
UserDefinedConstraint*
FbcModelPlugin::createUserDefinedConstraint()
{
  UserDefinedConstraint* udc = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    udc = new UserDefinedConstraint(fbcns);
    delete fbcns;
  }
  catch (...)
  {
  }

  if (udc != NULL)
  {
    mUserDefinedConstraints.appendAndOwn(udc);
  }

  return udc;
}


/*
 * Removes the nth UserDefinedConstraint from this FbcModelPlugin and returns a
 * pointer to it.
 */
UserDefinedConstraint*
FbcModelPlugin::removeUserDefinedConstraint(unsigned int n)
{
  return
    static_cast<UserDefinedConstraint*>(mUserDefinedConstraints.remove(n));
}


/*
 * Removes the UserDefinedConstraint from this FbcModelPlugin based on its
 * identifier and returns a pointer to it.
 */
UserDefinedConstraint*
FbcModelPlugin::removeUserDefinedConstraint(const std::string& sid)
{
  return static_cast<UserDefinedConstraint*>(mUserDefinedConstraints.remove(sid));
}



//---------------------------------------------------------------
/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
FbcModelPlugin::writeElements(XMLOutputStream& stream) const
{
  if (getLevel() == 2) return;

  if (getNumObjectives() > 0)
  {
    mObjectives.write(stream);
  }

  if (getNumFluxBounds() > 0)
  {
    mBounds.write(stream);
  }

  if (getNumGeneProducts() > 0)
  {
    mGeneProducts.write(stream);
  }

  if (getNumUserDefinedConstraints() > 0)
  {
    mUserDefinedConstraints.write(stream);
  }
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
* Accept the SBMLVisitor.
*/
bool
FbcModelPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  for (unsigned int i = 0; i < getNumFluxBounds(); i++)
  {
    getFluxBound(i)->accept(v);
  }
  for (unsigned int i = 0; i < getNumObjectives(); i++)
  {
    getListOfObjectives()->accept(v);
    getObjective(i)->accept(v);
  }

  for (unsigned int i = 0; i < getNumGeneProducts(); i++)
  {
    getGeneProduct(i)->accept(v);
  }
  mUserDefinedConstraints.accept(v);

  return true;
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d the SBMLDocument object to use.
 */
void
FbcModelPlugin::setSBMLDocument(SBMLDocument* d)
{
  FbcSBasePlugin::setSBMLDocument(d);

  mBounds.setSBMLDocument(d);  
  mAssociations.setSBMLDocument(d);  
  mObjectives.setSBMLDocument(d);  
  mGeneProducts.setSBMLDocument(d);
  mUserDefinedConstraints.setSBMLDocument(d);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
FbcModelPlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Connect to parent.
 */
void
FbcModelPlugin::connectToParent(SBase* sbase)
{
  FbcSBasePlugin::connectToParent(sbase);

  if (getNumObjectives() > 0)
  {
    mObjectives.connectToParent(sbase);
  }
  mAssociations.connectToParent(sbase);
  mBounds.connectToParent(sbase);
  if (getNumGeneProducts() > 0)
  {
    mGeneProducts.connectToParent(sbase);
  }
  mUserDefinedConstraints.connectToParent(sbase);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Enables the given package.
 */
void
FbcModelPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  mAssociations.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mBounds.enablePackageInternal(pkgURI,pkgPrefix,flag);
  if (getNumObjectives() > 0)
  {
    mObjectives.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
  if (getNumGeneProducts() > 0)
  {
    mGeneProducts.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
  mUserDefinedConstraints.enablePackageInternal(pkgURI, pkgPrefix, flag);
}
/** @endcond */
/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
FbcModelPlugin::updateSBMLNamespace(const std::string& package,
                                    unsigned int level,
                                    unsigned int version)
{
  FbcSBasePlugin::updateSBMLNamespace(package, level, version);

  mObjectives.updateSBMLNamespace(package, level, version);

  mBounds.updateSBMLNamespace(package, level, version);

  mGeneProducts.updateSBMLNamespace(package, level, version);

  mUserDefinedConstraints.updateSBMLNamespace(package, level, version);
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::getAttribute(const std::string& attributeName,
                             bool& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "strict")
  {
    value = getStrict();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::getAttribute(const std::string& attributeName,
                             int& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::getAttribute(const std::string& attributeName,
                             double& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::getAttribute(const std::string& attributeName,
                             unsigned int& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::getAttribute(const std::string& attributeName,
                             std::string& value) const
{
  int return_value = FbcSBasePlugin::getAttribute(attributeName, value);

  if (attributeName == "activeObjective")
  {
    value = getActiveObjectiveId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this FbcModelPlugin's attribute
 * "attributeName" is set.
 */
bool
FbcModelPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = FbcSBasePlugin::isSetAttribute(attributeName);

  if (attributeName == "strict")
  {
    value = isSetStrict();
  }
  else if (attributeName == "activeObjective")
  {
    value = (getActiveObjectiveId().empty() == false);
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  if (attributeName == "strict")
  {
    return_value = setStrict(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::setAttribute(const std::string& attributeName, double value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::setAttribute(const std::string& attributeName,
                             unsigned int value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::setAttribute(const std::string& attributeName,
                             const std::string& value)
{
  int return_value = FbcSBasePlugin::setAttribute(attributeName, value);

  if (attributeName == "activeObjective")
  {
    return_value = setActiveObjectiveId(value);
  }
  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this FbcModelPlugin.
 */
int
FbcModelPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = FbcSBasePlugin::unsetAttribute(attributeName);

  if (attributeName == "strict")
  {
    value = unsetStrict();
  }
  if (attributeName == "activeObjective")
  {
    value = unsetActiveObjectiveId();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this FbcModelPlugin.
 */
SBase*
FbcModelPlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "objective")
  {
    return createObjective();
  }
  else if (elementName == "fluxBound")
  {
    return createFluxBound();
  }
  else if (elementName == "geneProduct")
  {
    return createGeneProduct();
  }
  else if (elementName == "userDefinedConstraint")
  {
    return createUserDefinedConstraint();
  }

  return FbcSBasePlugin::createChildObject(elementName);
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this FbcModelPlugin.
 */
int
FbcModelPlugin::addChildObject(const std::string& elementName,
                               const SBase* element)
{
  if (elementName == "objective" && element->getTypeCode() ==
    SBML_FBC_OBJECTIVE)
  {
    return addObjective((const Objective*)(element));
  }
  else if (elementName == "fluxBound" && element->getTypeCode() ==
    SBML_FBC_FLUXBOUND)
  {
    return addFluxBound((const FluxBound*)(element));
  }
  else if (elementName == "geneProduct" && element->getTypeCode() ==
    SBML_FBC_GENEPRODUCT)
  {
    return addGeneProduct((const GeneProduct*)(element));
  }
  else if (elementName == "userDefinedConstraint" && element->getTypeCode() ==
    SBML_FBC_USERDEFINEDCONSTRAINT)
  {
    return addUserDefinedConstraint((const UserDefinedConstraint*)(element));
  }

  return FbcSBasePlugin::addChildObject(elementName, element);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * FbcModelPlugin.
 */
SBase*
FbcModelPlugin::removeChildObject(const std::string& elementName,
                                  const std::string& id)
{
  if (elementName == "objective")
  {
    return removeObjective(id);
  }
  else if (elementName == "fluxBound")
  {
    return removeFluxBound(id);
  }
  else if (elementName == "geneProduct")
  {
    return removeGeneProduct(id);
  }
  else if (elementName == "userDefinedConstraint")
  {
    return removeUserDefinedConstraint(id);
  }

  return FbcSBasePlugin::removeChildObject(elementName, id);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this FbcModelPlugin.
 */
unsigned int
FbcModelPlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "objective")
  {
    return getNumObjectives();
  }
  else if (elementName == "fluxBound")
  {
    return getNumFluxBounds();
  }
  else if (elementName == "geneProduct")
  {
    return getNumGeneProducts();
  }
  else if (elementName == "userDefinedConstraint")
  {
    return getNumUserDefinedConstraints();
  }

  return FbcSBasePlugin::getNumObjects(elementName);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this FbcModelPlugin.
 */
SBase*
FbcModelPlugin::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "objective")
  {
    return getObjective(index);
  }
  else if (elementName == "fluxBound")
  {
    return getFluxBound(index);
  }
  else if (elementName == "geneProduct")
  {
    return getGeneProduct(index);
  }
  else if (elementName == "userDefinedConstraint")
  {
    return getUserDefinedConstraint(index);
  }

  return FbcSBasePlugin::getObject(elementName, index);
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
FbcModelPlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mObjectives.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mBounds.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mGeneProducts.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mUserDefinedConstraints.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
FbcModelPlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mObjectives.getMetaId() == metaid)
  {
    return &mObjectives;
  }

  if (mBounds.getMetaId() == metaid)
  {
    return &mBounds;
  }

  if (mGeneProducts.getMetaId() == metaid)
  {
    return &mGeneProducts;
  }

  if (mUserDefinedConstraints.getMetaId() == metaid)
  {
    return &mUserDefinedConstraints;
  }

  obj = mObjectives.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mBounds.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mGeneProducts.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mUserDefinedConstraints.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
FbcModelPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mObjectives, filter);
  ADD_FILTERED_LIST(ret, sublist, mBounds, filter);
  ADD_FILTERED_LIST(ret, sublist, mGeneProducts, filter);
  ADD_FILTERED_LIST(ret, sublist, mUserDefinedConstraints, filter);

  return ret;
}

/** @cond doxygenLibsbmlInternal */
int
FbcModelPlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const FbcModelPlugin* modplug =
    static_cast<const FbcModelPlugin*>(model->getPlugin(getPrefix()));

  // absence of a plugin is not an error
  if (modplug == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  Model* parent = static_cast<Model*>(getParentSBMLObject());

  if (parent == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  ret = mObjectives.appendFrom(modplug->getListOfObjectives());

  if (ret != LIBSBML_OPERATION_SUCCESS)
  {
    return ret;
  }

  ret = mBounds.appendFrom(modplug->getListOfFluxBounds());

  if (ret != LIBSBML_OPERATION_SUCCESS)
  {
    return ret;
  }

  ret = mGeneProducts.appendFrom(modplug->getListOfGeneProducts());

  ret = mUserDefinedConstraints.appendFrom(modplug->getListOfUserDefinedConstraints());

  return ret;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
* create object
*/
SBase*
FbcModelPlugin::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const std::string&      name = stream.peek().getName();
  const XMLNamespaces&    xmlns1 = stream.peek().getNamespaces();
  const std::string&      prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns1.hasURI(mURI)) ? xmlns1.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix)
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    if (name == "listOfFluxBounds")
    {
      if (getErrorLog() && mBounds.size() != 0)
      {
        getErrorLog()->logPackageError("fbc", FbcOnlyOneEachListOf,
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
      }

      object = &mBounds;

      if (targetPrefix.empty())
      {
        mBounds.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }
    else if (name == "listOfObjectives")
    {
      if (getErrorLog() && mObjectives.size() != 0)
      {
        getErrorLog()->logPackageError("fbc", FbcOnlyOneEachListOf,
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
      }

      object = &mObjectives;

      if (targetPrefix.empty())
      {
        mObjectives.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }
    else if (name == "listOfGeneAssociations")
    {
      if (getErrorLog() && mAssociations.size() != 0)
      {
        getErrorLog()->logPackageError("fbc", FbcOnlyOneEachListOf,
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
      }

      object = &mAssociations;

      if (targetPrefix.empty())
      {
        mAssociations.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }
    else if (name == "listOfGeneProducts")
    {
      if (getErrorLog() && mGeneProducts.size() != 0)
      {
        getErrorLog()->logPackageError("fbc", FbcOnlyOneEachListOf,
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
      }

      object = &mGeneProducts;

      if (targetPrefix.empty() == true)
      {
        mGeneProducts.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }
    else if (name == "listOfUserDefinedConstraints")
    {
      if (getErrorLog() && mUserDefinedConstraints.size() != 0)
      {
        getErrorLog()->logPackageError("fbc", FbcOnlyOneEachListOf,
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
      }

      object = &mUserDefinedConstraints;

      if (targetPrefix.empty() == true)
      {
        mUserDefinedConstraints.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }


    delete fbcns;
  }

  return object;
}
/** @endcond */



ListOfFluxBounds * 
FbcModelPlugin::getFluxBoundsForReaction(const std::string& reaction) const
{
  ListOfFluxBounds * loFB = new ListOfFluxBounds(getLevel(), getVersion(),
    getPackageVersion());
                                                

  for (unsigned int i = 0; i < getNumFluxBounds(); i++)
  {
    if (getFluxBound(i)->getReaction() == reaction)
    {
      loFB->append(getFluxBound(i));
    }
  }

  if (loFB->size() == 0)
  {
    delete loFB;
    loFB = NULL;
  }

  return loFB;
}

#endif /* __cplusplus */

/** @cond doxygenIgnored */
LIBSBML_EXTERN
int
FbcModelPlugin_addFluxBound(SBasePlugin_t * fbc, FluxBound_t * fb)
{
  return (fbc != NULL) ? static_cast<FbcModelPlugin*>(fbc)->addFluxBound(fb)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
FluxBound_t *
FbcModelPlugin_getFluxBound(SBasePlugin_t * fbc, unsigned int n)
{
  return (fbc != NULL) ? static_cast<FbcModelPlugin*>(fbc)->getFluxBound(n)
    : NULL;
}


LIBSBML_EXTERN
unsigned int
FbcModelPlugin_getNumFluxBounds(SBasePlugin_t * fbc)
{
  return (fbc != NULL) ? static_cast<FbcModelPlugin*>(fbc)->getNumFluxBounds()
    : SBML_INT_MAX;
}


LIBSBML_EXTERN
Objective_t*
FbcModelPlugin_createObjective(SBasePlugin_t * fbc)
{
  return static_cast<FbcModelPlugin*>(fbc)->createObjective();
}


LIBSBML_EXTERN
int
FbcModelPlugin_addObjective(SBasePlugin_t * fbc, Objective_t * obj)
{
  return (fbc != NULL) ? static_cast<FbcModelPlugin*>(fbc)->addObjective(obj)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
Objective_t *
FbcModelPlugin_getObjective(SBasePlugin_t * fbc, unsigned int n)
{
  return (fbc != NULL) ? static_cast<FbcModelPlugin*>(fbc)->getObjective(n)
    : NULL;
}


LIBSBML_EXTERN
unsigned int
FbcModelPlugin_getNumObjectives(SBasePlugin_t * fbc)
{
  return (fbc != NULL) ? static_cast<FbcModelPlugin*>(fbc)->getNumObjectives()
    : SBML_INT_MAX;
}


LIBSBML_EXTERN
char *
FbcModelPlugin_getActiveObjectiveId(SBasePlugin_t * fbc)
{
  if (fbc == NULL)
    return NULL;

  return static_cast<FbcModelPlugin *>(fbc)->getActiveObjectiveId().empty() 
    ? safe_strdup("") 
    : safe_strdup(static_cast<FbcModelPlugin *>(fbc)->getActiveObjectiveId().c_str());
}


LIBSBML_EXTERN
int
FbcModelPlugin_setActiveObjectiveId(SBasePlugin_t * fbc, const char * activeId)
{
  return (fbc != NULL) 
    ? static_cast<FbcModelPlugin *>(fbc)->setActiveObjectiveId(activeId) 
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
GeneProduct_t*
FbcModelPlugin_createGeneProduct(SBasePlugin_t * fbc)
{
  return static_cast<FbcModelPlugin*>(fbc)->createGeneProduct();
}


LIBSBML_EXTERN
int
FbcModelPlugin_addGeneProduct(SBasePlugin_t * fbc, GeneProduct_t * fb)
{
  return (fbc != NULL) ? static_cast<FbcModelPlugin*>(fbc)->addGeneProduct(fb)
    : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
GeneProduct_t *
FbcModelPlugin_getGeneProduct(SBasePlugin_t * fbc, unsigned int n)
{
  return (fbc != NULL) ? static_cast<FbcModelPlugin*>(fbc)->getGeneProduct(n)
    : NULL;
}


LIBSBML_EXTERN
unsigned int
FbcModelPlugin_getNumGeneProducts(SBasePlugin_t * fbc)
{
  return (fbc != NULL) ? static_cast<FbcModelPlugin*>(fbc)->getNumGeneProducts()
    : SBML_INT_MAX;
}

/*
 * Returns a ListOf_t * containing UserDefinedConstraint_t objects from this
 * FbcModelPlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
FbcModelPlugin_getListOfUserDefinedConstraints(FbcModelPlugin_t* fmp)
{
  return (fmp != NULL) ? fmp->getListOfUserDefinedConstraints() : NULL;
}


/*
 * Get an UserDefinedConstraint_t from the FbcModelPlugin_t.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
FbcModelPlugin_getUserDefinedConstraint(FbcModelPlugin_t* fmp, unsigned int n)
{
  return (fmp != NULL) ? fmp->getUserDefinedConstraint(n) : NULL;
}


/*
 * Get an UserDefinedConstraint_t from the FbcModelPlugin_t based on its
 * identifier.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
FbcModelPlugin_getUserDefinedConstraintById(FbcModelPlugin_t* fmp,
                                            const char *sid)
{
  return (fmp != NULL && sid != NULL) ? fmp->getUserDefinedConstraint(sid) :
    NULL;
}


/*
 * Get an UserDefinedConstraint_t from the FbcModelPlugin_t based on the
 * LowerBound to which it refers.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
FbcModelPlugin_getUserDefinedConstraintByLowerBound(FbcModelPlugin_t* fmp,
                                                    const char *sid)
{
  return (fmp != NULL && sid != NULL) ?
    fmp->getUserDefinedConstraintByLowerBound(sid) : NULL;
}


/*
 * Get an UserDefinedConstraint_t from the FbcModelPlugin_t based on the
 * UpperBound to which it refers.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
FbcModelPlugin_getUserDefinedConstraintByUpperBound(FbcModelPlugin_t* fmp,
                                                    const char *sid)
{
  return (fmp != NULL && sid != NULL) ?
    fmp->getUserDefinedConstraintByUpperBound(sid) : NULL;
}


/*
 * Adds a copy of the given UserDefinedConstraint_t to this FbcModelPlugin_t.
 */
LIBSBML_EXTERN
int
FbcModelPlugin_addUserDefinedConstraint(FbcModelPlugin_t* fmp,
                                        const UserDefinedConstraint_t* udc)
{
  return (fmp != NULL) ? fmp->addUserDefinedConstraint(udc) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of UserDefinedConstraint_t objects in this FbcModelPlugin_t.
 */
LIBSBML_EXTERN
unsigned int
FbcModelPlugin_getNumUserDefinedConstraints(FbcModelPlugin_t* fmp)
{
  return (fmp != NULL) ? fmp->getNumUserDefinedConstraints() : SBML_INT_MAX;
}


/*
 * Creates a new UserDefinedConstraint_t object, adds it to this
 * FbcModelPlugin_t object and returns the UserDefinedConstraint_t object
 * created.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
FbcModelPlugin_createUserDefinedConstraint(FbcModelPlugin_t* fmp)
{
  return (fmp != NULL) ? fmp->createUserDefinedConstraint() : NULL;
}


/*
 * Removes the nth UserDefinedConstraint_t from this FbcModelPlugin_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
FbcModelPlugin_removeUserDefinedConstraint(FbcModelPlugin_t* fmp,
                                           unsigned int n)
{
  return (fmp != NULL) ? fmp->removeUserDefinedConstraint(n) : NULL;
}


/*
 * Removes the UserDefinedConstraint_t from this FbcModelPlugin_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
UserDefinedConstraint_t*
FbcModelPlugin_removeUserDefinedConstraintById(FbcModelPlugin_t* fmp,
                                               const char* sid)
{
  return (fmp != NULL && sid != NULL) ? fmp->removeUserDefinedConstraint(sid) :
    NULL;
}


LIBSBML_EXTERN
int
FbcModelPlugin_getStrict(SBasePlugin_t * fmp)
{
  return (int)(((FbcModelPlugin*)(fmp))->getStrict());
}


LIBSBML_EXTERN
int
FbcModelPlugin_setStrict(SBasePlugin_t * fmp, int strict)
{
  return ((FbcModelPlugin*)(fmp))->setStrict((bool)(strict));
}

/** @endcond */


LIBSBML_CPP_NAMESPACE_END




