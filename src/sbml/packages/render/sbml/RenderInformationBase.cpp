/**
 * @file    RenderInformationBase.cpp
 * @brief Implementation of the RenderInformationBase class.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
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
 * Copyright (C) 2011-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright 2010 Ralph Gauges
 *     Group for the modeling of biological processes 
 *     University of Heidelberg
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/render/sbml/RenderInformationBase.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/render/sbml/LinearGradient.h>
#include <sbml/packages/render/sbml/RadialGradient.h>
#include <sbml/packages/render/sbml/GlobalRenderInformation.h>
#include <sbml/packages/render/sbml/LocalRenderInformation.h>
#include <sbml/packages/render/sbml/ListOfLocalRenderInformation.h>


using namespace std;


#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/xml/XMLInputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new RenderInformationBase using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
RenderInformationBase::RenderInformationBase(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
  : SBase(level, version)
  , mProgramName ("")
  , mProgramVersion ("")
  , mReferenceRenderInformation ("")
  , mBackgroundColor ("")
  , mColorDefinitions (level, version, pkgVersion)
  , mGradientBases (level, version, pkgVersion)
  , mLineEndings (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new RenderInformationBase using the given RenderPkgNamespaces
 * object.
 */
RenderInformationBase::RenderInformationBase(RenderPkgNamespaces *renderns)
  : SBase(renderns)
  , mProgramName ("")
  , mProgramVersion ("")
  , mReferenceRenderInformation ("")
  , mBackgroundColor ("")
  , mColorDefinitions (renderns)
  , mGradientBases (renderns)
  , mLineEndings (renderns)
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/*
 * Copy constructor for RenderInformationBase.
 */
RenderInformationBase::RenderInformationBase(const RenderInformationBase& orig)
  : SBase( orig )
  , mProgramName ( orig.mProgramName )
  , mProgramVersion ( orig.mProgramVersion )
  , mReferenceRenderInformation ( orig.mReferenceRenderInformation )
  , mBackgroundColor ( orig.mBackgroundColor )
  , mColorDefinitions ( orig.mColorDefinitions )
  , mGradientBases ( orig.mGradientBases )
  , mLineEndings ( orig.mLineEndings )
{
  connectToChild();
}


/*
 * Assignment operator for RenderInformationBase.
 */
RenderInformationBase&
RenderInformationBase::operator=(const RenderInformationBase& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mProgramName = rhs.mProgramName;
    mProgramVersion = rhs.mProgramVersion;
    mReferenceRenderInformation = rhs.mReferenceRenderInformation;
    mBackgroundColor = rhs.mBackgroundColor;
    mColorDefinitions = rhs.mColorDefinitions;
    mGradientBases = rhs.mGradientBases;
    mLineEndings = rhs.mLineEndings;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this RenderInformationBase object.
 */
RenderInformationBase*
RenderInformationBase::clone() const
{
  return new RenderInformationBase(*this);
}


/*
 * Destructor for RenderInformationBase.
 */
RenderInformationBase::~RenderInformationBase()
{
}


/** @cond doxygenLibsbmlInternal */
/*
 * Parses the xml information in the given node and sets the attributes.
 * This method should never be called by the user. It is only used to read render 
 * information from annotations.
 *
 * @param node the XMLNode object reference that describes the RenderInfromationBase
 * object to be instantiated.
 */
void RenderInformationBase::parseXML(const XMLNode& node)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
     ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="listOfColorDefinitions")
        {
            this->mColorDefinitions=ListOfColorDefinitions(*child);
            this->mColorDefinitions.setSBMLDocument(this->mSBML);
        }
        else if(childName=="listOfGradientDefinitions")
        {
            this->mGradientBases=ListOfGradientDefinitions(*child);
            this->mGradientBases.setSBMLDocument(this->mSBML);
        }
        else if(childName=="listOfLineEndings")
        {
            this->mLineEndings=ListOfLineEndings(*child);
            this->mLineEndings.setSBMLDocument(this->mSBML);
        }
        else if(childName=="annotation")
        {
            this->mAnnotation=new XMLNode(*child);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(*child);
        }
        ++n;
    }
}
/** @endcond */



#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a RenderInformationBase object
 * empty color definition, gradient definition
 * and line endings set.
 * For the object to be valid a valid background color value.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
RenderInformationBase::RenderInformationBase(RenderPkgNamespaces* renderns, const std::string& id)
  : SBase(renderns)
  , mProgramName ("")
  , mProgramVersion ("")
  , mReferenceRenderInformation ("")
  , mBackgroundColor("")
  , mColorDefinitions(renderns) 
  , mGradientBases(renderns) 
  , mLineEndings(renderns) 
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. RenderInformationBase::RenderInformationBase(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    setId(id);
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

/*
 * Returns the value of the "id" attribute of this RenderInformationBase.
 */
const std::string&
RenderInformationBase::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this RenderInformationBase.
 */
const std::string&
RenderInformationBase::getName() const
{
  return mName;
}


/*
 * Returns the value of the "programName" attribute of this
 * RenderInformationBase.
 */
const std::string&
RenderInformationBase::getProgramName() const
{
  return mProgramName;
}


/*
 * Returns the value of the "programVersion" attribute of this
 * RenderInformationBase.
 */
const std::string&
RenderInformationBase::getProgramVersion() const
{
  return mProgramVersion;
}


/*
 * Returns the id of the referenced render information object.
 * RenderInfromation objects can reference other render information objects
 * and information that is not found in the current render information is then
 * expected to be in the referenced render information object.
 *
 * Global render information objects can only reference other global 
 * render information objects, local render information objects can reference other local
 * render information objects from the same list of local render information or other
 * global render infromation.
 *
 * @return the id of the referenced render infromation object.
 */
const std::string& RenderInformationBase::getReferenceRenderInformationId() const
{
    return this->mReferenceRenderInformation;
}

/*
 * Returns the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase.
 */
const std::string&
RenderInformationBase::getReferenceRenderInformation() const
{
  return mReferenceRenderInformation;
}


/*
 * Returns the value of the "backgroundColor" attribute of this
 * RenderInformationBase.
 */
const std::string&
RenderInformationBase::getBackgroundColor() const
{
  return mBackgroundColor;
}


/*
 * Predicate returning @c true if this RenderInformationBase's "id" attribute
 * is set.
 */
bool
RenderInformationBase::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this RenderInformationBase's "name" attribute
 * is set.
 */
bool
RenderInformationBase::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this RenderInformationBase's "programName"
 * attribute is set.
 */
bool
RenderInformationBase::isSetProgramName() const
{
  return (mProgramName.empty() == false);
}


/*
 * Predicate returning @c true if this RenderInformationBase's "programVersion"
 * attribute is set.
 */
bool
RenderInformationBase::isSetProgramVersion() const
{
  return (mProgramVersion.empty() == false);
}


/*
 * Predicate returning @c true if this RenderInformationBase's
 * "referenceRenderInformation" attribute is set.
 */
bool
RenderInformationBase::isSetReferenceRenderInformation() const
{
  return (mReferenceRenderInformation.empty() == false);
}


/*
 * Predicate returning @c true if this RenderInformationBase's
 * "backgroundColor" attribute is set.
 */
bool
RenderInformationBase::isSetBackgroundColor() const
{
  return (mBackgroundColor.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this RenderInformationBase.
 */
int
RenderInformationBase::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this RenderInformationBase.
 */
int
RenderInformationBase::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "programName" attribute of this RenderInformationBase.
 */
int
RenderInformationBase::setProgramName(const std::string& programName)
{
  mProgramName = programName;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "programVersion" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::setProgramVersion(const std::string& programVersion)
{
  mProgramVersion = programVersion;
  return LIBSBML_OPERATION_SUCCESS;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the id of the referenced render information object.
 * The user has to make sure that render infromation referencing 
 * does not create loops.
 *
 * @param id the id of the referenced render infromation
 */
void RenderInformationBase::setReferenceRenderInformationId(const std::string& id)
{
    this->mReferenceRenderInformation=id;
}


/*
 * Sets the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::setReferenceRenderInformation(const std::string&
  referenceRenderInformation)
{
  if (!(SyntaxChecker::isValidInternalSId(referenceRenderInformation)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mReferenceRenderInformation = referenceRenderInformation;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "backgroundColor" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::setBackgroundColor(const std::string& backgroundColor)
{
  mBackgroundColor = backgroundColor;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this RenderInformationBase.
 */
int
RenderInformationBase::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "name" attribute of this RenderInformationBase.
 */
int
RenderInformationBase::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "programName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::unsetProgramName()
{
  mProgramName.erase();

  if (mProgramName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "programVersion" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::unsetProgramVersion()
{
  mProgramVersion.erase();

  if (mProgramVersion.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::unsetReferenceRenderInformation()
{
  mReferenceRenderInformation.erase();

  if (mReferenceRenderInformation.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "backgroundColor" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::unsetBackgroundColor()
{
  mBackgroundColor.erase();

  if (mBackgroundColor.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the ListOfColorDefinitions from this RenderInformationBase.
 */
const ListOfColorDefinitions*
RenderInformationBase::getListOfColorDefinitions() const
{
  return &mColorDefinitions;
}


/*
 * Returns the ListOfColorDefinitions from this RenderInformationBase.
 */
ListOfColorDefinitions*
RenderInformationBase::getListOfColorDefinitions()
{
  return &mColorDefinitions;
}


/*
 * Get a ColorDefinition from the RenderInformationBase.
 */
ColorDefinition*
RenderInformationBase::getColorDefinition(unsigned int n)
{
  return mColorDefinitions.get(n);
}


/*
 * Get a ColorDefinition from the RenderInformationBase.
 */
const ColorDefinition*
RenderInformationBase::getColorDefinition(unsigned int n) const
{
  return mColorDefinitions.get(n);
}


/*
 * Get a ColorDefinition from the RenderInformationBase based on its
 * identifier.
 */
ColorDefinition*
RenderInformationBase::getColorDefinition(const std::string& sid)
{
  return mColorDefinitions.get(sid);
}


/*
 * Get a ColorDefinition from the RenderInformationBase based on its
 * identifier.
 */
const ColorDefinition*
RenderInformationBase::getColorDefinition(const std::string& sid) const
{
  return mColorDefinitions.get(sid);
}


/*
 * Adds a copy of the given ColorDefinition to this RenderInformationBase.
 */
int
RenderInformationBase::addColorDefinition(const ColorDefinition* cd)
{
  if (cd == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (cd->hasRequiredAttributesNoDefaults() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != cd->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != cd->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(cd)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (cd->isSetId() && (mColorDefinitions.get(cd->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mColorDefinitions.append(cd);
  }
}


/*
 * Get the number of ColorDefinition objects in this RenderInformationBase.
 */
unsigned int
RenderInformationBase::getNumColorDefinitions() const
{
  return mColorDefinitions.size();
}


/*
 * Creates a new ColorDefinition object, adds it to this RenderInformationBase
 * object and returns the ColorDefinition object created.
 */
ColorDefinition*
RenderInformationBase::createColorDefinition()
{
  ColorDefinition* cd = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    cd = new ColorDefinition(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (cd != NULL)
  {
    mColorDefinitions.appendAndOwn(cd);
  }

  return cd;
}


/*
 * Removes the nth ColorDefinition from this RenderInformationBase and returns
 * a pointer to it.
 */
ColorDefinition*
RenderInformationBase::removeColorDefinition(unsigned int n)
{
  return mColorDefinitions.remove(n);
}


/*
 * Removes the ColorDefinition from this RenderInformationBase based on its
 * identifier and returns a pointer to it.
 */
ColorDefinition*
RenderInformationBase::removeColorDefinition(const std::string& sid)
{
  return mColorDefinitions.remove(sid);
}


/*
 * Returns the ListOfGradientDefinitions from this RenderInformationBase.
 */
const ListOfGradientDefinitions*
RenderInformationBase::getListOfGradientDefinitions() const
{
  return &mGradientBases;
}


/*
 * Returns the ListOfGradientDefinitions from this RenderInformationBase.
 */
ListOfGradientDefinitions*
RenderInformationBase::getListOfGradientDefinitions()
{
  return &mGradientBases;
}


/*
 * Get a GradientBase from the RenderInformationBase.
 */
GradientBase*
RenderInformationBase::getGradientDefinition(unsigned int n)
{
  return mGradientBases.get(n);
}


/*
 * Get a GradientBase from the RenderInformationBase.
 */
const GradientBase*
RenderInformationBase::getGradientDefinition(unsigned int n) const
{
  return mGradientBases.get(n);
}


/*
 * Get a GradientBase from the RenderInformationBase based on its identifier.
 */
GradientBase*
RenderInformationBase::getGradientDefinition(const std::string& sid)
{
  return mGradientBases.get(sid);
}


/*
 * Get a GradientBase from the RenderInformationBase based on its identifier.
 */
const GradientBase*
RenderInformationBase::getGradientDefinition(const std::string& sid) const
{
  return mGradientBases.get(sid);
}


/*
 * Adds a copy of the given GradientBase to this RenderInformationBase.
 */
int
RenderInformationBase::addGradientDefinition(const GradientBase* gb)
{
  if (gb == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gb->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (gb->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != gb->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != gb->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(gb)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (gb->isSetId() && (mGradientBases.get(gb->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mGradientBases.append(gb);
  }
}


/*
 * Get the number of GradientBase objects in this RenderInformationBase.
 */
unsigned int
RenderInformationBase::getNumGradientDefinitions() const
{
  return mGradientBases.size();
}


/*
 * Creates a new LinearGradient object, adds it to this RenderInformationBase
 * object and returns the LinearGradient object created.
 */
LinearGradient*
RenderInformationBase::createLinearGradientDefinition()
{
  LinearGradient* lg = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    lg = new LinearGradient(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (lg != NULL)
  {
    mGradientBases.appendAndOwn(lg);
  }

  return lg;
}


/*
 * Creates a new RadialGradient object, adds it to this RenderInformationBase
 * object and returns the RadialGradient object created.
 */
RadialGradient*
RenderInformationBase::createRadialGradientDefinition()
{
  RadialGradient* rg = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    rg = new RadialGradient(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (rg != NULL)
  {
    mGradientBases.appendAndOwn(rg);
  }

  return rg;
}


/*
 * Removes the nth GradientBase from this RenderInformationBase and returns a
 * pointer to it.
 */
GradientBase*
RenderInformationBase::removeGradientDefinition(unsigned int n)
{
  return mGradientBases.remove(n);
}


/*
 * Removes the GradientBase from this RenderInformationBase based on its
 * identifier and returns a pointer to it.
 */
GradientBase*
RenderInformationBase::removeGradientDefinition(const std::string& sid)
{
  return mGradientBases.remove(sid);
}


/*
 * Returns the ListOfLineEndings from this RenderInformationBase.
 */
const ListOfLineEndings*
RenderInformationBase::getListOfLineEndings() const
{
  return &mLineEndings;
}


/*
 * Returns the ListOfLineEndings from this RenderInformationBase.
 */
ListOfLineEndings*
RenderInformationBase::getListOfLineEndings()
{
  return &mLineEndings;
}


/*
 * Get a LineEnding from the RenderInformationBase.
 */
LineEnding*
RenderInformationBase::getLineEnding(unsigned int n)
{
  return mLineEndings.get(n);
}


/*
 * Get a LineEnding from the RenderInformationBase.
 */
const LineEnding*
RenderInformationBase::getLineEnding(unsigned int n) const
{
  return mLineEndings.get(n);
}


/*
 * Get a LineEnding from the RenderInformationBase based on its identifier.
 */
LineEnding*
RenderInformationBase::getLineEnding(const std::string& sid)
{
  return mLineEndings.get(sid);
}


/*
 * Get a LineEnding from the RenderInformationBase based on its identifier.
 */
const LineEnding*
RenderInformationBase::getLineEnding(const std::string& sid) const
{
  return mLineEndings.get(sid);
}


/*
 * Adds a copy of the given LineEnding to this RenderInformationBase.
 */
int
RenderInformationBase::addLineEnding(const LineEnding* le)
{
  if (le == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (le->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (le->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != le->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != le->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(le)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (le->isSetId() && (mLineEndings.get(le->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mLineEndings.append(le);
  }
}


/*
 * Get the number of LineEnding objects in this RenderInformationBase.
 */
unsigned int
RenderInformationBase::getNumLineEndings() const
{
  return mLineEndings.size();
}


/*
 * Creates a new LineEnding object, adds it to this RenderInformationBase
 * object and returns the LineEnding object created.
 */
LineEnding*
RenderInformationBase::createLineEnding()
{
  LineEnding* le = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    le = new LineEnding(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (le != NULL)
  {
    mLineEndings.appendAndOwn(le);
  }

  return le;
}


/*
 * Removes the nth LineEnding from this RenderInformationBase and returns a
 * pointer to it.
 */
LineEnding*
RenderInformationBase::removeLineEnding(unsigned int n)
{
  return mLineEndings.remove(n);
}


/*
 * Removes the LineEnding from this RenderInformationBase based on its
 * identifier and returns a pointer to it.
 */
LineEnding*
RenderInformationBase::removeLineEnding(const std::string& sid)
{
  return mLineEndings.remove(sid);
}


/*
 * Predicate returning @c true if this abstract RenderInformationBase is of
 * type GlobalRenderInformation
 */
bool
RenderInformationBase::isGlobalRenderInformation() const
{
  return dynamic_cast<const GlobalRenderInformation*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract RenderInformationBase is of
 * type LocalRenderInformation
 */
bool
RenderInformationBase::isLocalRenderInformation() const
{
  return dynamic_cast<const LocalRenderInformation*>(this) != NULL;
}


/*
 * @copydoc doc_renamesidref_common
 */
void
RenderInformationBase::renameSIdRefs(const std::string& oldid,
                                     const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetReferenceRenderInformation() && mReferenceRenderInformation ==
    oldid)
  {
    setReferenceRenderInformation(newid);
  }
}

// render FIX ME
/*
 * Returns the XML element name of this RenderInformationBase object.
 */
//const std::string&
//RenderInformationBase::getElementName() const
//{
//  static const string name = "renderInformationBase";
//  return name;
//}


/*
 * Returns the libSBML type code for this RenderInformationBase object.
 */
int
RenderInformationBase::getTypeCode() const
{
  if (isLocalRenderInformation())
    return SBML_RENDER_LOCALRENDERINFORMATION;
  else
    return SBML_RENDER_GLOBALRENDERINFORMATION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * RenderInformationBase object have been set.
 */
bool
RenderInformationBase::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
RenderInformationBase::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (getNumColorDefinitions() > 0)
  {
    mColorDefinitions.write(stream);
  }

  if (getNumGradientDefinitions() > 0)
  {
    mGradientBases.write(stream);
  }

  if (getNumLineEndings() > 0)
  {
    mLineEndings.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
RenderInformationBase::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mColorDefinitions.accept(v);

  mGradientBases.accept(v);

  mLineEndings.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
RenderInformationBase::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mColorDefinitions.setSBMLDocument(d);

  mGradientBases.setSBMLDocument(d);

  mLineEndings.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
RenderInformationBase::connectToChild()
{
  SBase::connectToChild();

  mColorDefinitions.connectToParent(this);

  mGradientBases.connectToParent(this);

  mLineEndings.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
RenderInformationBase::enablePackageInternal(const std::string& pkgURI,
                                             const std::string& pkgPrefix,
                                             bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mColorDefinitions.enablePackageInternal(pkgURI, pkgPrefix, flag);

  mGradientBases.enablePackageInternal(pkgURI, pkgPrefix, flag);

  mLineEndings.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::getAttribute(const std::string& attributeName,
                                    bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::getAttribute(const std::string& attributeName,
                                    int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::getAttribute(const std::string& attributeName,
                                    double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::getAttribute(const std::string& attributeName,
                                    unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::getAttribute(const std::string& attributeName,
                                    std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "programName")
  {
    value = getProgramName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "programVersion")
  {
    value = getProgramVersion();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "referenceRenderInformation")
  {
    value = getReferenceRenderInformation();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "backgroundColor")
  {
    value = getBackgroundColor();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this RenderInformationBase's attribute
 * "attributeName" is set.
 */
bool
RenderInformationBase::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "programName")
  {
    value = isSetProgramName();
  }
  else if (attributeName == "programVersion")
  {
    value = isSetProgramVersion();
  }
  else if (attributeName == "referenceRenderInformation")
  {
    value = isSetReferenceRenderInformation();
  }
  else if (attributeName == "backgroundColor")
  {
    value = isSetBackgroundColor();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::setAttribute(const std::string& attributeName,
                                    bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::setAttribute(const std::string& attributeName,
                                    int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::setAttribute(const std::string& attributeName,
                                    double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::setAttribute(const std::string& attributeName,
                                    unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::setAttribute(const std::string& attributeName,
                                    const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }
  else if (attributeName == "programName")
  {
    return_value = setProgramName(value);
  }
  else if (attributeName == "programVersion")
  {
    return_value = setProgramVersion(value);
  }
  else if (attributeName == "referenceRenderInformation")
  {
    return_value = setReferenceRenderInformation(value);
  }
  else if (attributeName == "backgroundColor")
  {
    return_value = setBackgroundColor(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * RenderInformationBase.
 */
int
RenderInformationBase::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "programName")
  {
    value = unsetProgramName();
  }
  else if (attributeName == "programVersion")
  {
    value = unsetProgramVersion();
  }
  else if (attributeName == "referenceRenderInformation")
  {
    value = unsetReferenceRenderInformation();
  }
  else if (attributeName == "backgroundColor")
  {
    value = unsetBackgroundColor();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * RenderInformationBase.
 */
SBase*
RenderInformationBase::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "colorDefinition")
  {
    return createColorDefinition();
  }
  else if (elementName == "linearGradient")
  {
    return createLinearGradientDefinition();
  }
  else if (elementName == "radialGradient")
  {
    return createRadialGradientDefinition();
  }
  else if (elementName == "lineEnding")
  {
    return createLineEnding();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this RenderInformationBase.
 */
int
RenderInformationBase::addChildObject(const std::string& elementName,
                                      const SBase* element)
{
  if (elementName == "colorDefinition" && element->getTypeCode() ==
    SBML_RENDER_COLORDEFINITION)
  {
    return addColorDefinition((const ColorDefinition*)(element));
  }
  else if (elementName == "linearGradient" && element->getTypeCode() ==
    SBML_RENDER_LINEARGRADIENT)
  {
    return addGradientDefinition((const GradientBase*)(element));
  }
  else if (elementName == "radialGradient" && element->getTypeCode() ==
    SBML_RENDER_RADIALGRADIENT)
  {
    return addGradientDefinition((const GradientBase*)(element));
  }
  else if (elementName == "lineEnding" && element->getTypeCode() ==
    SBML_RENDER_LINEENDING)
  {
    return addLineEnding((const LineEnding*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * RenderInformationBase.
 */
SBase*
RenderInformationBase::removeChildObject(const std::string& elementName,
                                         const std::string& id)
{
  if (elementName == "colorDefinition")
  {
    return removeColorDefinition(id);
  }
  else if (elementName == "linearGradient")
  {
    return removeGradientDefinition(id);
  }
  else if (elementName == "radialGradient")
  {
    return removeGradientDefinition(id);
  }
  else if (elementName == "lineEnding")
  {
    return removeLineEnding(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this RenderInformationBase.
 */
unsigned int
RenderInformationBase::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "colorDefinition")
  {
    return getNumColorDefinitions();
  }
  else if (elementName == "gradientBase")
  {
    return getNumGradientDefinitions();
  }
  else if (elementName == "lineEnding")
  {
    return getNumLineEndings();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this RenderInformationBase.
 */
SBase*
RenderInformationBase::getObject(const std::string& elementName,
                                 unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "colorDefinition")
  {
    return getColorDefinition(index);
  }
  else if (elementName == "gradientBase")
  {
    return getGradientDefinition(index);
  }
  else if (elementName == "lineEnding")
  {
    return getLineEnding(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
RenderInformationBase::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mColorDefinitions.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mGradientBases.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mLineEndings.getElementBySId(id);

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
RenderInformationBase::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mColorDefinitions.getMetaId() == metaid)
  {
    return &mColorDefinitions;
  }

  if (mGradientBases.getMetaId() == metaid)
  {
    return &mGradientBases;
  }

  if (mLineEndings.getMetaId() == metaid)
  {
    return &mLineEndings;
  }

  obj = mColorDefinitions.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mGradientBases.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mLineEndings.getElementByMetaId(metaid);

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
RenderInformationBase::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mColorDefinitions, filter);
  ADD_FILTERED_LIST(ret, sublist, mGradientBases, filter);
  ADD_FILTERED_LIST(ret, sublist, mLineEndings, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
RenderInformationBase::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();

  if (name == "listOfColorDefinitions")
  {
    if (mColorDefinitions.size() != 0 && getErrorLog() != NULL)
    {
      getErrorLog()->logPackageError("render",
        RenderRenderInformationBaseAllowedElements, getPackageVersion(),
          getLevel(), getVersion(), "", getLine(), getColumn());
    }

    obj = &mColorDefinitions;
  }
  else if (name == "listOfGradientDefinitions")
  {
    if (mGradientBases.size() != 0 && getErrorLog() != NULL)
    {
      getErrorLog()->logPackageError("render",
        RenderRenderInformationBaseAllowedElements, getPackageVersion(),
          getLevel(), getVersion(), "", getLine(), getColumn());
    }

    obj = &mGradientBases;
  }
  else if (name == "listOfLineEndings")
  {
    if (mLineEndings.size() != 0 && getErrorLog() != NULL)
    {
      getErrorLog()->logPackageError("render",
        RenderRenderInformationBaseAllowedElements, getPackageVersion(),
          getLevel(), getVersion(), "", getLine(), getColumn());
    }

    obj = &mLineEndings;
  }

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
RenderInformationBase::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("programName");

  attributes.add("programVersion");

  attributes.add("referenceRenderInformation");

  attributes.add("backgroundColor");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
RenderInformationBase::readAttributes(const XMLAttributes& attributes,
                                      const ExpectedAttributes&
                                        expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  SBase::readAttributes(attributes, expectedAttributes);
  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs - 1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render",
          RenderRenderInformationBaseAllowedAttributes, pkgVersion, level, version,
          details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderRenderInformationBaseAllowedCoreAttributes, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // id SId (use = "required" )
  // 

  assigned = attributes.readInto("id", mId);
  if (log)
  {
    if (assigned == true)
    {
      if (mId.empty() == true)
      {
        logEmptyString(mId, level, version, "<RenderInformationBase>");
      }
      else if (SyntaxChecker::isValidSBMLSId(mId) == false)
      {
        log->logPackageError("render", RenderIdSyntaxRule, pkgVersion, level,
          version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
      }
    }
    else
    {
      std::string message = "Render attribute 'id' is missing from the "
        "<RenderInformationBase> element.";
      log->logPackageError("render",
        RenderRenderInformationBaseAllowedAttributes, pkgVersion, level, version,
        message, getLine(), getColumn());
    }
  }
  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (log && mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<RenderInformationBase>");
    }
  }

  // 
  // programName string (use = "optional" )
  // 

  assigned = attributes.readInto("programName", mProgramName);

  if (log && assigned == true)
  {
    if (mProgramName.empty() == true)
    {
      logEmptyString(mProgramName, level, version, "<RenderInformationBase>");
    }
  }

  // 
  // programVersion string (use = "optional" )
  // 

  assigned = attributes.readInto("programVersion", mProgramVersion);

  if (assigned == true)
  {
    if (log && mProgramVersion.empty() == true)
    {
      logEmptyString(mProgramVersion, level, version,
        "<RenderInformationBase>");
    }
  }

  // 
  // referenceRenderInformation SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("referenceRenderInformation",
    mReferenceRenderInformation);

  if (log && assigned == true)
  {
    if (mReferenceRenderInformation.empty() == true)
    {
      logEmptyString(mReferenceRenderInformation, level, version,
        "<RenderInformationBase>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mReferenceRenderInformation) ==
      false)
    {
      std::string msg = "The referenceRenderInformation attribute on the <" +
        getElementName() + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mReferenceRenderInformation + "', which does not conform "
        "to the syntax.";
      log->logPackageError("render",
        RenderRenderInformationBaseReferenceRenderInformationMustBeRenderInformationBase,
          pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }

  // 
  // backgroundColor string (use = "optional" )
  // 

  assigned = attributes.readInto("backgroundColor", mBackgroundColor);

  if (assigned == true)
  {
    if (log && mBackgroundColor.empty() == true)
    {
      logEmptyString(mBackgroundColor, level, version,
        "<RenderInformationBase>");
    }
  }
  else
  {
    mBackgroundColor = "#FFFFFFFF";
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
RenderInformationBase::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetProgramName() == true)
  {
    stream.writeAttribute("programName", getPrefix(), mProgramName);
  }

  if (isSetProgramVersion() == true)
  {
    stream.writeAttribute("programVersion", getPrefix(), mProgramVersion);
  }

  if (isSetReferenceRenderInformation() == true)
  {
    stream.writeAttribute("referenceRenderInformation", getPrefix(),
      mReferenceRenderInformation);
  }

  if (isSetBackgroundColor() == true)
  {
    stream.writeAttribute("backgroundColor", getPrefix(), mBackgroundColor);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new GlobalRenderInformation (RenderInformationBase_t) using the
 * given SBML Level, Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
RenderInformationBase_t *
RenderInformationBase_createGlobalRenderInformation(unsigned int level,
                                                    unsigned int version,
                                                    unsigned int pkgVersion)
{
  return new GlobalRenderInformation(level, version, pkgVersion);
}


/*
 * Creates a new LocalRenderInformation (RenderInformationBase_t) using the
 * given SBML Level, Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
RenderInformationBase_t *
RenderInformationBase_createLocalRenderInformation(unsigned int level,
                                                   unsigned int version,
                                                   unsigned int pkgVersion)
{
  return new LocalRenderInformation(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this RenderInformationBase_t object.
 */
LIBSBML_EXTERN
RenderInformationBase_t*
RenderInformationBase_clone(const RenderInformationBase_t* rib)
{
  if (rib != NULL)
  {
    return static_cast<RenderInformationBase_t*>(rib->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this RenderInformationBase_t object.
 */
LIBSBML_EXTERN
void
RenderInformationBase_free(RenderInformationBase_t* rib)
{
  if (rib != NULL)
  {
    delete rib;
  }
}


/*
 * Returns the value of the "id" attribute of this RenderInformationBase_t.
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getId(const RenderInformationBase_t * rib)
{
  if (rib == NULL)
  {
    return NULL;
  }

  return rib->getId().empty() ? NULL : safe_strdup(rib->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this RenderInformationBase_t.
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getName(const RenderInformationBase_t * rib)
{
  if (rib == NULL)
  {
    return NULL;
  }

  return rib->getName().empty() ? NULL : safe_strdup(rib->getName().c_str());
}


/*
 * Returns the value of the "programName" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getProgramName(const RenderInformationBase_t * rib)
{
  if (rib == NULL)
  {
    return NULL;
  }

  return rib->getProgramName().empty() ? NULL :
    safe_strdup(rib->getProgramName().c_str());
}


/*
 * Returns the value of the "programVersion" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getProgramVersion(const RenderInformationBase_t * rib)
{
  if (rib == NULL)
  {
    return NULL;
  }

  return rib->getProgramVersion().empty() ? NULL :
    safe_strdup(rib->getProgramVersion().c_str());
}


/*
 * Returns the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getReferenceRenderInformation(const
  RenderInformationBase_t * rib)
{
  if (rib == NULL)
  {
    return NULL;
  }

  return rib->getReferenceRenderInformation().empty() ? NULL :
    safe_strdup(rib->getReferenceRenderInformation().c_str());
}


/*
 * Returns the value of the "backgroundColor" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
char *
RenderInformationBase_getBackgroundColor(const RenderInformationBase_t * rib)
{
  if (rib == NULL)
  {
    return NULL;
  }

  return rib->getBackgroundColor().empty() ? NULL :
    safe_strdup(rib->getBackgroundColor().c_str());
}


/*
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetId(const RenderInformationBase_t * rib)
{
  return (rib != NULL) ? static_cast<int>(rib->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's "name"
 * attribute is set.
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetName(const RenderInformationBase_t * rib)
{
  return (rib != NULL) ? static_cast<int>(rib->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's
 * "programName" attribute is set.
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetProgramName(const RenderInformationBase_t * rib)
{
  return (rib != NULL) ? static_cast<int>(rib->isSetProgramName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's
 * "programVersion" attribute is set.
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetProgramVersion(const RenderInformationBase_t * rib)
{
  return (rib != NULL) ? static_cast<int>(rib->isSetProgramVersion()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's
 * "referenceRenderInformation" attribute is set.
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetReferenceRenderInformation(const
  RenderInformationBase_t * rib)
{
  return (rib != NULL) ?
    static_cast<int>(rib->isSetReferenceRenderInformation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderInformationBase_t's
 * "backgroundColor" attribute is set.
 */
LIBSBML_EXTERN
int
RenderInformationBase_isSetBackgroundColor(const RenderInformationBase_t * rib)
{
  return (rib != NULL) ? static_cast<int>(rib->isSetBackgroundColor()) : 0;
}


/*
 * Sets the value of the "id" attribute of this RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_setId(RenderInformationBase_t * rib, const char * id)
{
  return (rib != NULL) ? rib->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_setName(RenderInformationBase_t * rib,
                              const char * name)
{
  return (rib != NULL) ? rib->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "programName" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_setProgramName(RenderInformationBase_t * rib,
                                     const char * programName)
{
  return (rib != NULL) ? rib->setProgramName(programName) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "programVersion" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_setProgramVersion(RenderInformationBase_t * rib,
                                        const char * programVersion)
{
  return (rib != NULL) ? rib->setProgramVersion(programVersion) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_setReferenceRenderInformation(
                                                    RenderInformationBase_t *
                                                      rib,
                                                    const char *
                                                      referenceRenderInformation)
{
  return (rib != NULL) ?
    rib->setReferenceRenderInformation(referenceRenderInformation) :
      LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "backgroundColor" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_setBackgroundColor(RenderInformationBase_t * rib,
                                         const char * backgroundColor)
{
  return (rib != NULL) ? rib->setBackgroundColor(backgroundColor) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetId(RenderInformationBase_t * rib)
{
  return (rib != NULL) ? rib->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetName(RenderInformationBase_t * rib)
{
  return (rib != NULL) ? rib->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "programName" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetProgramName(RenderInformationBase_t * rib)
{
  return (rib != NULL) ? rib->unsetProgramName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "programVersion" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetProgramVersion(RenderInformationBase_t * rib)
{
  return (rib != NULL) ? rib->unsetProgramVersion() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "referenceRenderInformation" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetReferenceRenderInformation(RenderInformationBase_t *
  rib)
{
  return (rib != NULL) ? rib->unsetReferenceRenderInformation() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "backgroundColor" attribute of this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_unsetBackgroundColor(RenderInformationBase_t * rib)
{
  return (rib != NULL) ? rib->unsetBackgroundColor() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing ColorDefinition_t objects from this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
ListOf_t*
RenderInformationBase_getListOfColorDefinitions(RenderInformationBase_t* rib)
{
  return (rib != NULL) ? rib->getListOfColorDefinitions() : NULL;
}


/*
 * Get a ColorDefinition_t from the RenderInformationBase_t.
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_getColorDefinition(RenderInformationBase_t* rib,
                                         unsigned int n)
{
  return (rib != NULL) ? rib->getColorDefinition(n) : NULL;
}


/*
 * Get a ColorDefinition_t from the RenderInformationBase_t based on its
 * identifier.
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_getColorDefinitionById(RenderInformationBase_t* rib,
                                             const char *sid)
{
  return (rib != NULL && sid != NULL) ? rib->getColorDefinition(sid) : NULL;
}


/*
 * Adds a copy of the given ColorDefinition_t to this RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_addColorDefinition(RenderInformationBase_t* rib,
                                         const ColorDefinition_t* cd)
{
  return (rib != NULL) ? rib->addColorDefinition(cd) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of ColorDefinition_t objects in this RenderInformationBase_t.
 */
LIBSBML_EXTERN
unsigned int
RenderInformationBase_getNumColorDefinitions(RenderInformationBase_t* rib)
{
  return (rib != NULL) ? rib->getNumColorDefinitions() : SBML_INT_MAX;
}


/*
 * Creates a new ColorDefinition_t object, adds it to this
 * RenderInformationBase_t object and returns the ColorDefinition_t object
 * created.
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_createColorDefinition(RenderInformationBase_t* rib)
{
  return (rib != NULL) ? rib->createColorDefinition() : NULL;
}


/*
 * Removes the nth ColorDefinition_t from this RenderInformationBase_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_removeColorDefinition(RenderInformationBase_t* rib,
                                            unsigned int n)
{
  return (rib != NULL) ? rib->removeColorDefinition(n) : NULL;
}


/*
 * Removes the ColorDefinition_t from this RenderInformationBase_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
ColorDefinition_t*
RenderInformationBase_removeColorDefinitionById(RenderInformationBase_t* rib,
                                                const char* sid)
{
  return (rib != NULL && sid != NULL) ? rib->removeColorDefinition(sid) : NULL;
}


/*
 * Returns a ListOf_t * containing GradientBase_t objects from this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
ListOf_t*
RenderInformationBase_getListOfGradientDefinitions(RenderInformationBase_t*
  rib)
{
  return (rib != NULL) ? rib->getListOfGradientDefinitions() : NULL;
}


/*
 * Get a GradientBase_t from the RenderInformationBase_t.
 */
LIBSBML_EXTERN
GradientBase_t*
RenderInformationBase_getGradientDefinition(RenderInformationBase_t* rib,
                                            unsigned int n)
{
  return (rib != NULL) ? rib->getGradientDefinition(n) : NULL;
}


/*
 * Get a GradientBase_t from the RenderInformationBase_t based on its
 * identifier.
 */
LIBSBML_EXTERN
GradientBase_t*
RenderInformationBase_getGradientBaseById(RenderInformationBase_t* rib,
                                          const char *sid)
{
  return (rib != NULL && sid != NULL) ? rib->getGradientDefinition(sid) : NULL;
}


/*
 * Adds a copy of the given GradientBase_t to this RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_addGradientDefinition(RenderInformationBase_t* rib,
                                            const GradientBase_t* gb)
{
  return (rib != NULL) ? rib->addGradientDefinition(gb) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of GradientBase_t objects in this RenderInformationBase_t.
 */
LIBSBML_EXTERN
unsigned int
RenderInformationBase_getNumGradientDefinitions(RenderInformationBase_t* rib)
{
  return (rib != NULL) ? rib->getNumGradientDefinitions() : SBML_INT_MAX;
}


/*
 * Creates a new LinearGradient_t object, adds it to this
 * RenderInformationBase_t object and returns the LinearGradient_t object
 * created.
 */
LIBSBML_EXTERN
LinearGradient_t*
RenderInformationBase_createLinearGradientDefinition(RenderInformationBase_t* rib)
{
  return (rib != NULL) ? rib->createLinearGradientDefinition() : NULL;
}


/*
 * Creates a new RadialGradient_t object, adds it to this
 * RenderInformationBase_t object and returns the RadialGradient_t object
 * created.
 */
LIBSBML_EXTERN
RadialGradient_t*
RenderInformationBase_createRadialGradientDefinition(RenderInformationBase_t* rib)
{
  return (rib != NULL) ? rib->createRadialGradientDefinition() : NULL;
}


/*
 * Removes the nth GradientBase_t from this RenderInformationBase_t and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
GradientBase_t*
RenderInformationBase_removeGradientDefinition(RenderInformationBase_t* rib,
                                               unsigned int n)
{
  return (rib != NULL) ? rib->removeGradientDefinition(n) : NULL;
}


/*
 * Removes the GradientBase_t from this RenderInformationBase_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
GradientBase_t*
RenderInformationBase_removeGradientDefinitionById(RenderInformationBase_t* rib,
                                             const char* sid)
{
  return (rib != NULL && sid != NULL) ? rib->removeGradientDefinition (sid) : NULL;
}


/*
 * Returns a ListOf_t * containing LineEnding_t objects from this
 * RenderInformationBase_t.
 */
LIBSBML_EXTERN
ListOf_t*
RenderInformationBase_getListOfLineEndings(RenderInformationBase_t* rib)
{
  return (rib != NULL) ? rib->getListOfLineEndings() : NULL;
}


/*
 * Get a LineEnding_t from the RenderInformationBase_t.
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_getLineEnding(RenderInformationBase_t* rib,
                                    unsigned int n)
{
  return (rib != NULL) ? rib->getLineEnding(n) : NULL;
}


/*
 * Get a LineEnding_t from the RenderInformationBase_t based on its identifier.
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_getLineEndingById(RenderInformationBase_t* rib,
                                        const char *sid)
{
  return (rib != NULL && sid != NULL) ? rib->getLineEnding(sid) : NULL;
}


/*
 * Adds a copy of the given LineEnding_t to this RenderInformationBase_t.
 */
LIBSBML_EXTERN
int
RenderInformationBase_addLineEnding(RenderInformationBase_t* rib,
                                    const LineEnding_t* le)
{
  return (rib != NULL) ? rib->addLineEnding(le) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of LineEnding_t objects in this RenderInformationBase_t.
 */
LIBSBML_EXTERN
unsigned int
RenderInformationBase_getNumLineEndings(RenderInformationBase_t* rib)
{
  return (rib != NULL) ? rib->getNumLineEndings() : SBML_INT_MAX;
}


/*
 * Creates a new LineEnding_t object, adds it to this RenderInformationBase_t
 * object and returns the LineEnding_t object created.
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_createLineEnding(RenderInformationBase_t* rib)
{
  return (rib != NULL) ? rib->createLineEnding() : NULL;
}


/*
 * Removes the nth LineEnding_t from this RenderInformationBase_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_removeLineEnding(RenderInformationBase_t* rib,
                                       unsigned int n)
{
  return (rib != NULL) ? rib->removeLineEnding(n) : NULL;
}


/*
 * Removes the LineEnding_t from this RenderInformationBase_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
LineEnding_t*
RenderInformationBase_removeLineEndingById(RenderInformationBase_t* rib,
                                           const char* sid)
{
  return (rib != NULL && sid != NULL) ? rib->removeLineEnding(sid) : NULL;
}


/*
 * Predicate returning @c 1 if this RenderInformationBase_t is of type
 * GlobalRenderInformation_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isGlobalRenderInformation(const RenderInformationBase_t *
  rib)
{
  return (rib != NULL) ? static_cast<int>(rib->isGlobalRenderInformation()) :
    0;
}


/*
 * Predicate returning @c 1 if this RenderInformationBase_t is of type
 * LocalRenderInformation_t
 */
LIBSBML_EXTERN
int
RenderInformationBase_isLocalRenderInformation(const RenderInformationBase_t *
  rib)
{
  return (rib != NULL) ? static_cast<int>(rib->isLocalRenderInformation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RenderInformationBase_t object have been set.
 */
LIBSBML_EXTERN
int
RenderInformationBase_hasRequiredAttributes(const RenderInformationBase_t *
  rib)
{
  return (rib != NULL) ? static_cast<int>(rib->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


