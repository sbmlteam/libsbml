/**
 * @file    GradientBase.cpp
 * @brief   abstract base class for gradient objects
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
#include <sbml/packages/render/sbml/GradientBase.h>
#include <sbml/packages/render/sbml/ListOfGradientDefinitions.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/render/sbml/LinearGradient.h>
#include <sbml/packages/render/sbml/RadialGradient.h>

#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>


#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new GradientBase using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
GradientBase::GradientBase(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
  : SBase(level, version)
  , mSpreadMethod (GRADIENT_SPREADMETHOD_PAD)
  , mGradientStops (level, version, pkgVersion)
  , mElementName("gradientBase")
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new GradientBase using the given RenderPkgNamespaces object.
 */
GradientBase::GradientBase(RenderPkgNamespaces *renderns)
  : SBase(renderns)
  , mSpreadMethod (GRADIENT_SPREADMETHOD_PAD)
  , mGradientStops (renderns)
  , mElementName("gradientBase")
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GradientBase object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * GradientBase object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the GradientBase
 * object to be instantiated.
 */
GradientBase::GradientBase(const XMLNode& node, unsigned int l2version) 
  : SBase(2, l2version)
  , mGradientStops(node, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    mURI = RenderExtension::getXmlnsL3V1V1();        

    ExpectedAttributes ea;
    addExpectedAttributes(ea);

    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="stop")
        {
            this->mGradientStops.appendAndOwn(new GradientStop(*child));
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

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */



#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a GradientBase with no gradient stops.
 * The spreadMethod attribute is set to GradientBase::PAD and the id is
 * set to the given value.
 * This object is not valid until it gets at least two gradient stops.
 *
 * @param id The id for the gradient definition object
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
GradientBase::GradientBase(RenderPkgNamespaces* renderns, const std::string& id)
  : SBase(renderns)
//  , mId(id)
, mSpreadMethod(GRADIENT_SPREADMETHOD_PAD)
, mGradientStops(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. GradientBase::GradientBase(const std::string& id) is deprecated." << std::endl;
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
 * Copy constructor for GradientBase.
 */
GradientBase::GradientBase(const GradientBase& orig)
  : SBase( orig )
  , mSpreadMethod ( orig.mSpreadMethod )
  , mGradientStops ( orig.mGradientStops )
  , mElementName ( orig.mElementName )
{
  connectToChild();
}


/*
 * Assignment operator for GradientBase.
 */
GradientBase&
GradientBase::operator=(const GradientBase& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mSpreadMethod = rhs.mSpreadMethod;
    mGradientStops = rhs.mGradientStops;
    mElementName = rhs.mElementName;
    connectToChild();
  }

  return *this;
}


/*
 * Destructor for GradientBase.
 */
GradientBase::~GradientBase()
{
}


/*
 * Returns the value of the "id" attribute of this GradientBase.
 */
const std::string&
GradientBase::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this GradientBase.
 */
const std::string&
GradientBase::getName() const
{
  return mName;
}


/*
 * Returns the value of the "spreadMethod" attribute of this GradientBase.
 */
int GradientBase::getSpreadMethod() const
{
    return this->mSpreadMethod;
}


/*
 * Returns the value of the "spreadMethod" attribute of this GradientBase.
 */
std::string
GradientBase::getSpreadMethodAsString() const
{
  std::string code_str =
    GradientSpreadMethod_toString((GradientSpreadMethod_t)(mSpreadMethod));
  return code_str;
}


/*
 * Returns the value of the "spreadMethod" attribute of this GradientBase.
 */
//const std::string&
//GradientBase::getSpreadMethodString() const
//{
//  static const std::string code_str =
//    SpreadMethod_toString((GradientBase::SPREADMETHOD)(mSpreadMethod));
//  return code_str;
//}
std::string
GradientBase::getSpreadMethodString() const
{
  std::string code_str = 
    SpreadMethod_toString((GradientBase::SPREADMETHOD)(mSpreadMethod));
  return code_str;
}


/*
 * Predicate returning @c true if this GradientBase's "id" attribute is set.
 */
bool
GradientBase::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this GradientBase's "name" attribute is set.
 */
bool
GradientBase::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this GradientBase's "spreadMethod" attribute
 * is set.
 */
bool
GradientBase::isSetSpreadMethod() const
{
  return (mSpreadMethod != GRADIENT_SPREAD_METHOD_INVALID);
}


/*
 * Sets the value of the "id" attribute of this GradientBase.
 */
int
GradientBase::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this GradientBase.
 */
int
GradientBase::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "spreadMethod" attribute of this GradientBase.
 */
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the spread method.
 * Valid values are GradientBase::PAD, GradientBase::REFLECT
 * and GradientBase::REPEAT.
 *
 * @param method The new spread method for the gradient.
 */
void GradientBase::setSpreadMethod(GradientBase::SPREADMETHOD method)
{
    this->mSpreadMethod=method;
}


/*
 * Sets the value of the "spreadMethod" attribute of this GradientBase.
 */
int
GradientBase::setSpreadMethod(const GradientSpreadMethod_t spreadMethod)
{
  if (GradientSpreadMethod_isValid(spreadMethod) == 0)
  {
    mSpreadMethod = GRADIENT_SPREAD_METHOD_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSpreadMethod = spreadMethod;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "spreadMethod" attribute of this GradientBase.
 */
int
GradientBase::setSpreadMethod(const std::string& spreadMethod)
{
  mSpreadMethod =
    GradientSpreadMethod_fromString(spreadMethod.c_str());

  if (mSpreadMethod == GRADIENT_SPREAD_METHOD_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
  //if (GradientSpreadMethod_isValidString(spreadMethod.c_str()) == 0)
  //{
  //  mSpreadMethod = GRADIENT_SPREADMETHOD_PAD;
  //  return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  //}
  //else
  //{
  //  mSpreadMethod = GradientSpreadMethod_fromString(spreadMethod.c_str());
  //  return LIBSBML_OPERATION_SUCCESS;
//  }
}


/*
 * Unsets the value of the "id" attribute of this GradientBase.
 */
int
GradientBase::unsetId()
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
 * Unsets the value of the "name" attribute of this GradientBase.
 */
int
GradientBase::unsetName()
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
 * Unsets the value of the "spreadMethod" attribute of this GradientBase.
 */
int
GradientBase::unsetSpreadMethod()
{
  mSpreadMethod = GRADIENT_SPREAD_METHOD_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the ListOfGradientStops from this GradientBase.
 */
const ListOfGradientStops*
GradientBase::getListOfGradientStops() const
{
  return &mGradientStops;
}


/*
 * Returns the ListOfGradientStops from this GradientBase.
 */
ListOfGradientStops*
GradientBase::getListOfGradientStops()
{
  return &mGradientStops;
}


/*
 * Get a GradientStop from the GradientBase.
 */
GradientStop*
GradientBase::getGradientStop(unsigned int n)
{
  return mGradientStops.get(n);
}


GradientStop*
GradientBase::getGradientStop(const std::string& sid)
{
  return mGradientStops.get(sid);
}


/*
 * Get a GradientStop from the GradientBase.
 */
const GradientStop*
GradientBase::getGradientStop(unsigned int n) const
{
  return mGradientStops.get(n);
}


/*
 * Adds a copy of the given GradientStop to this GradientBase.
 */
int
GradientBase::addGradientStop(const GradientStop* gs)
{
  if (gs == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gs->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (gs->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != gs->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != gs->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(gs)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mGradientStops.append(gs);
  }
}


/*
 * Get the number of GradientStop objects in this GradientBase.
 */
unsigned int
GradientBase::getNumGradientStops() const
{
  return mGradientStops.size();
}


/*
 * Creates a new GradientStop object, adds it to this GradientBase object and
 * returns the GradientStop object created.
 */
GradientStop*
GradientBase::createGradientStop()
{
  GradientStop* gs = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    gs = new GradientStop(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (gs != NULL)
  {
    mGradientStops.appendAndOwn(gs);
  }

  return gs;
}


/*
 * Removes the nth GradientStop from this GradientBase and returns a pointer to
 * it.
 */
GradientStop*
GradientBase::removeGradientStop(unsigned int n)
{
  return mGradientStops.remove(n);
}


GradientStop*
GradientBase::removeGradientStop(const std::string& sid)
{
  return mGradientStops.remove(sid);
}


/*
 * Predicate returning @c true if this abstract GradientBase is of type
 * LinearGradient
 */
bool
GradientBase::isLinearGradient() const
{
  return dynamic_cast<const LinearGradient*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GradientBase is of type
 * RadialGradient
 */
bool
GradientBase::isRadialGradient() const
{
  return dynamic_cast<const RadialGradient*>(this) != NULL;
}


/*
 * Returns the XML element name of this GradientBase object.
 */
const std::string&
GradientBase::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this GradientBase object.
 */
void
GradientBase::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this GradientBase object.
 */
int
GradientBase::getTypeCode() const
{
  return SBML_RENDER_GRADIENTDEFINITION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * GradientBase object have been set.
 */
bool
GradientBase::hasRequiredAttributes() const
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
GradientBase::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  for (unsigned int i = 0; i < getNumGradientStops(); i++)
  {
    getGradientStop(i)->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
GradientBase::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mGradientStops.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
GradientBase::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mGradientStops.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
GradientBase::connectToChild()
{
  SBase::connectToChild();

  mGradientStops.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
GradientBase::enablePackageInternal(const std::string& pkgURI,
                                    const std::string& pkgPrefix,
                                    bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mGradientStops.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfGradientDefinitions object.
 *
 * @return the XMLNode with the XML representation for the 
 * ListOfGradientDefinitions object.
 */
XMLNode ListOfGradientDefinitions::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::getAttribute(const std::string& attributeName,
                           bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::getAttribute(const std::string& attributeName,
                           double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::getAttribute(const std::string& attributeName,
                           unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "spreadMethod")
  {
    value = getSpreadMethodAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this GradientBase's attribute "attributeName"
 * is set.
 */
bool
GradientBase::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "spreadMethod")
  {
    value = isSetSpreadMethod();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::setAttribute(const std::string& attributeName,
                           unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "spreadMethod")
  {
    return_value = setSpreadMethod(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this GradientBase.
 */
int
GradientBase::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "spreadMethod")
  {
    value = unsetSpreadMethod();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this GradientBase.
 */
SBase*
GradientBase::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "gradientStop")
  {
    return createGradientStop();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this GradientBase.
 */
int
GradientBase::addChildObject(const std::string& elementName,
                             const SBase* element)
{
  if (elementName == "gradientStop" && element->getTypeCode() ==
    SBML_RENDER_GRADIENT_STOP)
  {
    return addGradientStop((const GradientStop*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * GradientBase.
 */
SBase*
GradientBase::removeChildObject(const std::string& elementName,
                                const std::string& id)
{
  if (elementName == "gradientStop")
  {
    for (unsigned int i = 0; i < getNumGradientStops(); i++)
    {
      if (getGradientStop(i)->getId() == id)
      {
        return removeGradientStop(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this GradientBase.
 */
unsigned int
GradientBase::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "gradientStop")
  {
    return getNumGradientStops();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this GradientBase.
 */
SBase*
GradientBase::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "gradientStop")
  {
    return getGradientStop(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
GradientBase::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mGradientStops.getElementBySId(id);

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
GradientBase::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mGradientStops.getMetaId() == metaid)
  {
    return &mGradientStops;
  }

  obj = mGradientStops.getElementByMetaId(metaid);

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
GradientBase::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mGradientStops, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
GradientBase::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  //const std::string& name = stream.peek().getName();

  obj = mGradientStops.createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
GradientBase::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("spreadMethod");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
GradientBase::readAttributes(const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfGradientDefinitions*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderGradientBaseAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderUnknown, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderGradientBaseAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderGradientBaseAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // id SId (use = "required" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (log && mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<GradientBase>");
    }
    else if (log && SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("render", RenderIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Render attribute 'id' is missing from the "
      "<GradientBase> element.";
    if (log)
    {
      log->logPackageError("render", RenderGradientBaseAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
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
      logEmptyString(mName, level, version, "<GradientBase>");
    }
  }

  // 
  // spreadMethod enum (use = "optional" )
  // 

  std::string spreadMethod;
  assigned = attributes.readInto("spreadMethod", spreadMethod);

  if (assigned == true)
  {
    if (log && spreadMethod.empty() == true)
    {
      logEmptyString(spreadMethod, level, version, "<GradientBase>");
    }
    else
    {
      mSpreadMethod = GradientSpreadMethod_fromString(spreadMethod.c_str());

      if (log && GradientSpreadMethod_isValid((GradientSpreadMethod_t)(mSpreadMethod)) == 0)
      {
        std::string msg = "The spreadMethod on the <GradientBase> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + spreadMethod + "', which is not a valid option.";

        log->logPackageError("render",
          RenderGradientBaseSpreadMethodMustBeGradientSpreadMethodEnum,
            pkgVersion, level, version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
      this->mSpreadMethod = GRADIENT_SPREADMETHOD_PAD;
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
GradientBase::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetSpreadMethod() == true && mSpreadMethod != GRADIENT_SPREADMETHOD_PAD)
  {
    stream.writeAttribute("spreadMethod", getPrefix(),
      GradientSpreadMethod_toString((GradientSpreadMethod_t)(mSpreadMethod)));
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Converts the given string into a spread method.
 * If the string does not represnt a valid spread method, PAD is
 * returned.
 */
int GradientBase::getSpreadMethodForString(const std::string& s)
{
    GradientBase::SPREADMETHOD m=PAD;
    if(s=="reflect")
    {
        m=REFLECT;
    }
    else if(s=="repeat")
    {
        m=REPEAT;
    }
    return m;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * This method is used when writing out gradietns to XML.
 * I writes out the attributes and children that re common to linear and radial gradient.
 */
void GradientBase::addGradientAttributesAndChildren(const GradientBase& gradient,XMLAttributes& att,XMLNode& node)
{
    addSBaseAttributes(gradient,att);
    att.add("id",gradient.mId);
    switch(gradient.mSpreadMethod)
    {
        default:
        case PAD:
            break;
        case REFLECT:
            att.add("spreadMethod","reflect");
            break;
        case REPEAT:
            att.add("spreadMethod","repeat");
            break;
    }
    // add the notes and annotations
    if(gradient.mNotes)
    {
        node.addChild(*gradient.mNotes);
    }
    if(gradient.mAnnotation)
    {
        node.addChild(*gradient.mAnnotation);
    }
    unsigned int i,iMax=gradient.mGradientStops.size();
    const GradientStop* stop=NULL;
    for(i=0;i<iMax;++i)
    {
        stop=dynamic_cast<const GradientStop*>(gradient.mGradientStops.get(i));  
        node.addChild(stop->toXML());
    }
}
/** @endcond */


#endif /* __cplusplus */


/*
 * Creates a new LinearGradient (GradientBase_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
GradientBase_t *
GradientBase_createLinearGradient(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion)
{
  return new LinearGradient(level, version, pkgVersion);
}


/*
 * Creates a new RadialGradient (GradientBase_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
GradientBase_t *
GradientBase_createRadialGradient(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion)
{
  return new RadialGradient(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this GradientBase_t object.
 */
LIBSBML_EXTERN
GradientBase_t*
GradientBase_clone(const GradientBase_t* gb)
{
  if (gb != NULL)
  {
    return static_cast<GradientBase_t*>(gb->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this GradientBase_t object.
 */
LIBSBML_EXTERN
void
GradientBase_free(GradientBase_t* gb)
{
  if (gb != NULL)
  {
    delete gb;
  }
}


/*
 * Returns the value of the "id" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
char *
GradientBase_getId(const GradientBase_t * gb)
{
  if (gb == NULL)
  {
    return NULL;
  }

  return gb->getId().empty() ? NULL : safe_strdup(gb->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
char *
GradientBase_getName(const GradientBase_t * gb)
{
  if (gb == NULL)
  {
    return NULL;
  }

  return gb->getName().empty() ? NULL : safe_strdup(gb->getName().c_str());
}


/*
 * Returns the value of the "spreadMethod" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
GradientSpreadMethod_t
GradientBase_getSpreadMethod(const GradientBase_t * gb)
{
  if (gb == NULL)
  {
    return GRADIENT_SPREAD_METHOD_INVALID;
  }

  return (GradientSpreadMethod_t)(gb->getSpreadMethod());
}


/*
 * Returns the value of the "spreadMethod" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
char *
GradientBase_getSpreadMethodAsString(const GradientBase_t * gb)
{
  return (char*)(GradientSpreadMethod_toString((GradientSpreadMethod_t)(gb->getSpreadMethod())));
}


/*
 * Predicate returning @c 1 (true) if this GradientBase_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
GradientBase_isSetId(const GradientBase_t * gb)
{
  return (gb != NULL) ? static_cast<int>(gb->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this GradientBase_t's "name" attribute is
 * set.
 */
LIBSBML_EXTERN
int
GradientBase_isSetName(const GradientBase_t * gb)
{
  return (gb != NULL) ? static_cast<int>(gb->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this GradientBase_t's "spreadMethod"
 * attribute is set.
 */
LIBSBML_EXTERN
int
GradientBase_isSetSpreadMethod(const GradientBase_t * gb)
{
  return (gb != NULL) ? static_cast<int>(gb->isSetSpreadMethod()) : 0;
}


/*
 * Sets the value of the "id" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
int
GradientBase_setId(GradientBase_t * gb, const char * id)
{
  return (gb != NULL) ? gb->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
int
GradientBase_setName(GradientBase_t * gb, const char * name)
{
  return (gb != NULL) ? gb->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "spreadMethod" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
int
GradientBase_setSpreadMethod(GradientBase_t * gb,
                             GradientSpreadMethod_t spreadMethod)
{
  return (gb != NULL) ? gb->setSpreadMethod(spreadMethod) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "spreadMethod" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
int
GradientBase_setSpreadMethodAsString(GradientBase_t * gb,
                                     const char * spreadMethod)
{
  return (gb != NULL) ? gb->setSpreadMethod(spreadMethod):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
int
GradientBase_unsetId(GradientBase_t * gb)
{
  return (gb != NULL) ? gb->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
int
GradientBase_unsetName(GradientBase_t * gb)
{
  return (gb != NULL) ? gb->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "spreadMethod" attribute of this GradientBase_t.
 */
LIBSBML_EXTERN
int
GradientBase_unsetSpreadMethod(GradientBase_t * gb)
{
  return (gb != NULL) ? gb->unsetSpreadMethod() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing GradientStop_t objects from this
 * GradientBase_t.
 */
LIBSBML_EXTERN
ListOf_t*
GradientBase_getListOfGradientStops(GradientBase_t* gb)
{
  return (gb != NULL) ? gb->getListOfGradientStops() : NULL;
}


/*
 * Get a GradientStop_t from the GradientBase_t.
 */
LIBSBML_EXTERN
GradientStop_t*
GradientBase_getGradientStop(GradientBase_t* gb, unsigned int n)
{
  return (gb != NULL) ? gb->getGradientStop(n) : NULL;
}


/*
 * Adds a copy of the given GradientStop_t to this GradientBase_t.
 */
LIBSBML_EXTERN
int
GradientBase_addGradientStop(GradientBase_t* gb, const GradientStop_t* gs)
{
  return (gb != NULL) ? gb->addGradientStop(gs) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of GradientStop_t objects in this GradientBase_t.
 */
LIBSBML_EXTERN
unsigned int
GradientBase_getNumGradientStops(GradientBase_t* gb)
{
  return (gb != NULL) ? gb->getNumGradientStops() : SBML_INT_MAX;
}


/*
 * Creates a new GradientStop_t object, adds it to this GradientBase_t object
 * and returns the GradientStop_t object created.
 */
LIBSBML_EXTERN
GradientStop_t*
GradientBase_createGradientStop(GradientBase_t* gb)
{
  return (gb != NULL) ? gb->createGradientStop() : NULL;
}


/*
 * Removes the nth GradientStop_t from this GradientBase_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
GradientStop_t*
GradientBase_removeGradientStop(GradientBase_t* gb, unsigned int n)
{
  return (gb != NULL) ? gb->removeGradientStop(n) : NULL;
}


/*
 * Predicate returning @c 1 if this GradientBase_t is of type LinearGradient_t
 */
LIBSBML_EXTERN
int
GradientBase_isLinearGradient(const GradientBase_t * gb)
{
  return (gb != NULL) ? static_cast<int>(gb->isLinearGradient()) : 0;
}


/*
 * Predicate returning @c 1 if this GradientBase_t is of type RadialGradient_t
 */
LIBSBML_EXTERN
int
GradientBase_isRadialGradient(const GradientBase_t * gb)
{
  return (gb != NULL) ? static_cast<int>(gb->isRadialGradient()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GradientBase_t object have been set.
 */
LIBSBML_EXTERN
int
GradientBase_hasRequiredAttributes(const GradientBase_t * gb)
{
  return (gb != NULL) ? static_cast<int>(gb->hasRequiredAttributes()) : 0;
}



const char* SPREADMETHOD_STRINGS[] =
{
  "pad",
  "reflect",
  "repeat", 
  "invalid"
};


LIBSBML_EXTERN
GradientBase::SPREADMETHOD
SpreadMethod_fromString(const char* name)
{
  if (name != NULL)
  {
    const GradientBase::SPREADMETHOD  lo = GradientBase::PAD;
    const GradientBase::SPREADMETHOD  hi = GradientBase::REPEAT;

    return (GradientBase::SPREADMETHOD)util_bsearchStringsI(SPREADMETHOD_STRINGS, name, lo, hi);
  }

  return GradientBase::INVALID;

}

LIBSBML_EXTERN
const char*
SpreadMethod_toString(GradientBase::SPREADMETHOD method)
{
  if ((method < GradientBase::PAD) || (method > GradientBase::REPEAT))
  {
    method = GradientBase::INVALID;
  }

  return SPREADMETHOD_STRINGS[method];

}

LIBSBML_CPP_NAMESPACE_END 


