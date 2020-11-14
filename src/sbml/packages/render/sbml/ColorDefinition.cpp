/**
 * @file    ColorDefinition.cpp
 * @brief Implementation of the ColorDefinition class.
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
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <functional>
#include <string.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include "sbml/packages/layout/util/LayoutUtilities.h"
#include "sbml/xml/XMLInputStream.h"
#include <sbml/packages/render/extension/RenderExtension.h>

#include <assert.h>

#include <sbml/packages/render/sbml/ColorDefinition.h>
#include <sbml/packages/render/sbml/ListOfColorDefinitions.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>


using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ColorDefinition using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
ColorDefinition::ColorDefinition(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
  : SBase(level, version)
    ,mRed(0)
    ,mGreen(0)
    ,mBlue(0)
    ,mAlpha(255)
  , mValue ("")
{
  mValue = createValueString();
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ColorDefinition using the given RenderPkgNamespaces object.
 */
ColorDefinition::ColorDefinition(RenderPkgNamespaces *renderns)
   : SBase(renderns)
    ,mRed(0)
    ,mGreen(0)
    ,mBlue(0)
    ,mAlpha(255)
  , mValue ("")
{
  mValue = createValueString();
  setElementNamespace(renderns->getURI());
  loadPlugins(renderns);
 }


/*
 * Creates a new ColorDefinition object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * ColorDefinition object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the ColorDefinition
 * object to be instantiated.
 */
ColorDefinition::ColorDefinition(const XMLNode& node, unsigned int l2version)
  : SBase(2, l2version)
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
        if(childName=="annotation")
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
 * Constructor which sets the ColorDefinition to completely opaque
 * black and sets the id to the given string.
 *
 * @param id the id of the color definition. The user has to make sure 
 * that the id is unique within the given set of color definitions. 
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
ColorDefinition::ColorDefinition(RenderPkgNamespaces* renderns, const std::string& id)
   :SBase(renderns)
//    ,mId(id)
  , mRed(0)
  , mGreen(0)
  , mBlue(0)
  , mAlpha(255)
  , mValue("")
{
  mValue = createValueString();
  setId(id);

  // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);

#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. ColorDefinition::ColorDefinition(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which sets the ColorDefinition to the given RGBA values
 * and sets the id.
 *
 * @param id the id of the color definition. The user has to make sure 
 * that the id is unique within the given set of color definitions. 
 * @param r Red component value. Has to be in the range of 0 to 255.
 * @param g Green component value. Has to be in the range of 0 to 255.
 * @param b Blue component value. Has to be in the range of 0 to 255.
 * @param a Alpha component value. Has to be in the range of 0 to 255. 
 * The alpha component can be omitted. In that case it has a default value of 255.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
ColorDefinition::ColorDefinition(RenderPkgNamespaces* renderns, const std::string& id,unsigned char r,unsigned char g,unsigned char b,unsigned char a)
   :SBase(renderns)
//    ,mId(id)
    ,mRed(r)
    ,mGreen(g)
    ,mBlue(b)
    ,mAlpha(a)
  , mValue("")
{
  mValue = createValueString();
  setId(id);

  // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);

#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. ColorDefinition::ColorDefinition(const std::string& id,unsigned char r,unsigned char g,unsigned char b,unsigned char a) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
}
/** @endcond */
#endif // OMIT_DEPRECATED


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which sets the ColorDefinition to the given RGBA values.
 *
 * @param r Red component value. Has to be in the range of 0 to 255.
 * @param g Green component value. Has to be in the range of 0 to 255.
 * @param b Blue component value. Has to be in the range of 0 to 255.
 * @param a Alpha component value. Has to be in the range of 0 to 255. 
 * The alpha component can be omitted. In that case it has a default value of 255.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
ColorDefinition::ColorDefinition(RenderPkgNamespaces* renderns, unsigned char r,unsigned char g,unsigned char b,unsigned char a)
   :SBase(renderns)
//    ,mId()
    ,mRed(r)
    ,mGreen(g)
    ,mBlue(b)
    ,mAlpha(a)
  , mValue("")
{
  mValue = createValueString();

  // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);

#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. ColorDefinition::ColorDefinition(unsigned char r,unsigned char g,unsigned char b,unsigned char a) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
}
/** @endcond */
#endif // OMIT_DEPRECATED

/*
 * Copy constructor for ColorDefinition.
 */
ColorDefinition::ColorDefinition(const ColorDefinition& orig)
  : SBase( orig )
  , mRed(orig.mRed)
  , mGreen(orig.mGreen)
  , mBlue(orig.mBlue)
  , mAlpha(orig.mAlpha)
  , mValue(orig.mValue)
{
}


/*
 * Assignment operator for ColorDefinition.
 */
ColorDefinition&
ColorDefinition::operator=(const ColorDefinition& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mValue = rhs.mValue;
    mRed = rhs.mRed;
    mGreen = rhs.mGreen;
    mBlue = rhs.mBlue;
    mAlpha = rhs.mAlpha;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ColorDefinition object.
 */
ColorDefinition*
ColorDefinition::clone() const
{
  return new ColorDefinition(*this);
}


/*
 * Destructor for ColorDefinition.
 */
ColorDefinition::~ColorDefinition()
{
}


/*
 * Returns the value of the "id" attribute of this ColorDefinition.
 */
const std::string&
ColorDefinition::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this ColorDefinition.
 */
const std::string&
ColorDefinition::getName() const
{
  return mName;
}


/*
 * Returns the value of the "value" attribute of this ColorDefinition.
 */
const std::string&
ColorDefinition::getValue() const
{
  return mValue;
}


/*
 * Returns the red color component.
 *
 * @return the red color component for the ColorDefinition.
 */
unsigned char ColorDefinition::getRed() const
{
    return mRed;
}


/*
 * Returns the green color component.
 *
 * @return the green color component for the ColorDefinition.
 */
unsigned char ColorDefinition::getGreen() const
{
    return mGreen;
}


/*
 * Returns the blue color component.
 *
 * @return the blue color component for the ColorDefinition.
 */
unsigned char ColorDefinition::getBlue() const
{
    return mBlue;
}


/*
 * Returns the alpha color component.
 *
 * @return the alpha color component for the ColorDefinition.
 */
unsigned char ColorDefinition::getAlpha() const
{
    return mAlpha;
}

/*
 * Predicate returning @c true if this ColorDefinition's "id" attribute is set.
 */
bool
ColorDefinition::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this ColorDefinition's "name" attribute is
 * set.
 */
bool
ColorDefinition::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this ColorDefinition's "value" attribute is
 * set.
 */
bool
ColorDefinition::isSetValue() const
{
  return (mValue.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this ColorDefinition.
 */
int
ColorDefinition::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this ColorDefinition.
 */
int
ColorDefinition::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "value" attribute of this ColorDefinition.
 */
int
ColorDefinition::setValue(const std::string& value)
{
  mValue = value;
  setColorValue(value);
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the red color component.
 *
 * @param c the new red component value for the color definition.
 */
void ColorDefinition::setRed(unsigned char c)
{
    this->mRed=c;
    mValue = createValueString();
}


/*
 * Sets the green color component.
 *
 * @param c the new green component value for the color definition.
 */
void ColorDefinition::setGreen(unsigned char c)
{
    this->mGreen=c;
    mValue = createValueString();
}


/*
 * Sets the blue color component.
 *
 * @param c the new blue component value for the color definition.
 */
void ColorDefinition::setBlue(unsigned char c)
{
    this->mBlue=c;
    mValue = createValueString();
}


/*
 * Sets alpha red color component.
 *
 * @param c the new alpha component value for the color definition.
 */
void ColorDefinition::setAlpha(unsigned char c)
{
    this->mAlpha=c;
    mValue = createValueString();
}


/*
 * Sets the red green, blue and alpha color component.
 * The alpha value is optional and defaults to 255 if not given.
 * @param r Red component value. Has to be in the range of 0 to 255.
 * @param g Green component value. Has to be in the range of 0 to 255.
 * @param b Blue component value. Has to be in the range of 0 to 255.
 * @param a Alpha component value. Has to be in the range of 0 to 255. 
 * The alpha component can be omitted. In that case it has a default value of 255.
 */
void ColorDefinition::setRGBA(unsigned char r,unsigned char g,unsigned char b,unsigned char a)
{
    this->mRed=r;
    this->mGreen=g;
    this->mBlue=b;
    this->mAlpha=a;
    mValue = createValueString();
}


/*
 * Sets the color value from a given value string.
 * If the string is not a valid value string, the color value is set to
 * black and false is returned.
 *
 * @param valueString A const reference to a string that represents a valid color value,
 * e.g. "#FFFFFFFF" for fully opaque white.
 *
 * @return @c true or false depending on whether setting the color value from the string
 * was successfull.
 */
bool ColorDefinition::setColorValue(const std::string& valueString)
{
    bool result=true;	
    size_t first_letter=valueString.find_first_not_of(std::string(" \t\r\n"));
    if(first_letter == std::string::npos)
    {
        result=false;
    }
    else
    {
      std::string trimmed=valueString.substr(first_letter,valueString.find_last_not_of(" \t\r\n")-first_letter+1);
      if(trimmed[0]=='#' && (trimmed.size() == 7 || trimmed.size() == 9) && trimmed.find_first_not_of("0123456789ABCDEFabcdef",1) == std::string::npos)
      {
        this->mRed=(unsigned char)strtol(trimmed.substr(1,2).c_str(),NULL,16);
        this->mGreen=(unsigned char)strtol(trimmed.substr(3,2).c_str(),NULL,16);
        this->mBlue=(unsigned char)strtol(trimmed.substr(5,2).c_str(),NULL,16);
        if(trimmed.size() == 9)
        {
            // set the alpha value
            this->mAlpha=(unsigned char)strtol(trimmed.substr(7,2).c_str(),NULL,16);
        }
        else
        {
            // if no alpha is given, the color is completely opaque
            this->mAlpha=255;
        }
      }
      else
      {
   result=false;
      }
    }
    if(result==false)
     {
         this->mRed=0;
         this->mGreen=0;
         this->mBlue=0;
         this->mAlpha=255;
     }
     return result;
}


/*
 * Creates a string the represents the current color value.
 *
 * @return The string representation of the color value.
 */
std::string ColorDefinition::createValueString() const
{
   std::ostringstream os;
   os << "#" << std::hex << std::setw(2) << std::setfill('0') <<  (unsigned int)mRed << std::hex << std::setw(2) << std::setfill('0') <<  (unsigned int)mGreen << std::hex << std::setw(2) << std::setfill('0') <<  (unsigned int)mBlue;
   if(mAlpha!=255)
   {
     os  << std::hex << std::setw(2) << std::setfill('0') <<  (unsigned int)mAlpha;
   }
   return os.str();
}


/*
 * Unsets the value of the "id" attribute of this ColorDefinition.
 */
int
ColorDefinition::unsetId()
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
 * Unsets the value of the "name" attribute of this ColorDefinition.
 */
int
ColorDefinition::unsetName()
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
 * Unsets the value of the "value" attribute of this ColorDefinition.
 */
int
ColorDefinition::unsetValue()
{
  mValue.erase();
  setColorValue(mValue);

  if (mValue.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this ColorDefinition object.
 */
const std::string&
ColorDefinition::getElementName() const
{
  static const string name = "colorDefinition";
  return name;
}


/*
 * Returns the libSBML type code for this ColorDefinition object.
 */
int
ColorDefinition::getTypeCode() const
{
  return SBML_RENDER_COLORDEFINITION;
}


/*
* Predicate returning @c true if all the required attributes but not defaults for this
* ColorDefinition object have been set.
*/
bool
ColorDefinition::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  if (isSetValue() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/** @cond doxygenLibsbmlInternal */

/*
* Predicate returning @c true if all the required attributes but not defaults for this
* ColorDefinition object have been set.
*/
bool
ColorDefinition::hasRequiredAttributesNoDefaults() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  return allPresent;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
ColorDefinition::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
ColorDefinition::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
ColorDefinition::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
ColorDefinition::enablePackageInternal(const std::string& pkgURI,
                                       const std::string& pkgPrefix,
                                       bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::getAttribute(const std::string& attributeName,
                              bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::getAttribute(const std::string& attributeName,
                              int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::getAttribute(const std::string& attributeName,
                              double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::getAttribute(const std::string& attributeName,
                              unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "value")
  {
    value = getValue();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this ColorDefinition's attribute
 * "attributeName" is set.
 */
bool
ColorDefinition::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "value")
  {
    value = isSetValue();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::setAttribute(const std::string& attributeName,
                              unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "value")
  {
    return_value = setValue(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this ColorDefinition.
 */
int
ColorDefinition::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "value")
  {
    value = unsetValue();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfColorDefinitions object.
 *
 * @return the XMLNode with the XML representation for the 
 * ColorDefinition object.
 */
XMLNode ColorDefinition::toXML() const
{
  return getXmlNodeForSBase(this);  
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
ColorDefinition::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("value");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ColorDefinition::readAttributes(const XMLAttributes& attributes,
                                const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfColorDefinitions*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderColorDefinitionAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderRenderInformationBaseLOColorDefinitionsAllowedCoreAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
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
        log->logPackageError("render", RenderColorDefinitionAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render",
          RenderColorDefinitionAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
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
      logEmptyString(mId, level, version, "<ColorDefinition>");
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
      "<ColorDefinition> element.";
    log->logPackageError("render", RenderColorDefinitionAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (log && mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<ColorDefinition>");
    }
  }

  // 
  // value string (use = "required" )
  // 

  assigned = attributes.readInto("value", mValue);

  if (assigned == true)
  {
    if (log && mValue.empty() == true)
    {
      logEmptyString(mValue, level, version, "<ColorDefinition>");
    }
    else
    {
      this->setColorValue(mValue);
    }
  }
  else
  {
    if (log)
    {
      std::string message = "Render attribute 'value' is missing from the "
        "<ColorDefinition> element.";
      log->logPackageError("render", RenderColorDefinitionAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }
}


/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ColorDefinition::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetValue() == true)
  {
    stream.writeAttribute("value", getPrefix(), createValueString());
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new ColorDefinition_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
ColorDefinition_t *
ColorDefinition_create(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
{
  return new ColorDefinition(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this ColorDefinition_t object.
 */
LIBSBML_EXTERN
ColorDefinition_t*
ColorDefinition_clone(const ColorDefinition_t* cd)
{
  if (cd != NULL)
  {
    return static_cast<ColorDefinition_t*>(cd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this ColorDefinition_t object.
 */
LIBSBML_EXTERN
void
ColorDefinition_free(ColorDefinition_t* cd)
{
  if (cd != NULL)
  {
    delete cd;
  }
}


/*
 * Returns the value of the "id" attribute of this ColorDefinition_t.
 */
LIBSBML_EXTERN
char *
ColorDefinition_getId(const ColorDefinition_t * cd)
{
  if (cd == NULL)
  {
    return NULL;
  }

  return cd->getId().empty() ? NULL : safe_strdup(cd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this ColorDefinition_t.
 */
LIBSBML_EXTERN
char *
ColorDefinition_getName(const ColorDefinition_t * cd)
{
  if (cd == NULL)
  {
    return NULL;
  }

  return cd->getName().empty() ? NULL : safe_strdup(cd->getName().c_str());
}


/*
 * Returns the value of the "value" attribute of this ColorDefinition_t.
 */
LIBSBML_EXTERN
char *
ColorDefinition_getValue(const ColorDefinition_t * cd)
{
  if (cd == NULL)
  {
    return NULL;
  }

  return cd->getValue().empty() ? NULL : safe_strdup(cd->getValue().c_str());
}


/*
 * Predicate returning @c 1 (true) if this ColorDefinition_t's "id" attribute
 * is set.
 */
LIBSBML_EXTERN
int
ColorDefinition_isSetId(const ColorDefinition_t * cd)
{
  return (cd != NULL) ? static_cast<int>(cd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ColorDefinition_t's "name" attribute
 * is set.
 */
LIBSBML_EXTERN
int
ColorDefinition_isSetName(const ColorDefinition_t * cd)
{
  return (cd != NULL) ? static_cast<int>(cd->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ColorDefinition_t's "value"
 * attribute is set.
 */
LIBSBML_EXTERN
int
ColorDefinition_isSetValue(const ColorDefinition_t * cd)
{
  return (cd != NULL) ? static_cast<int>(cd->isSetValue()) : 0;
}


/*
 * Sets the value of the "id" attribute of this ColorDefinition_t.
 */
LIBSBML_EXTERN
int
ColorDefinition_setId(ColorDefinition_t * cd, const char * id)
{
  return (cd != NULL) ? cd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this ColorDefinition_t.
 */
LIBSBML_EXTERN
int
ColorDefinition_setName(ColorDefinition_t * cd, const char * name)
{
  return (cd != NULL) ? cd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "value" attribute of this ColorDefinition_t.
 */
LIBSBML_EXTERN
int
ColorDefinition_setValue(ColorDefinition_t * cd, const char * value)
{
  return (cd != NULL) ? cd->setValue(value) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this ColorDefinition_t.
 */
LIBSBML_EXTERN
int
ColorDefinition_unsetId(ColorDefinition_t * cd)
{
  return (cd != NULL) ? cd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this ColorDefinition_t.
 */
LIBSBML_EXTERN
int
ColorDefinition_unsetName(ColorDefinition_t * cd)
{
  return (cd != NULL) ? cd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "value" attribute of this ColorDefinition_t.
 */
LIBSBML_EXTERN
int
ColorDefinition_unsetValue(ColorDefinition_t * cd)
{
  return (cd != NULL) ? cd->unsetValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * ColorDefinition_t object have been set.
 */
LIBSBML_EXTERN
int
ColorDefinition_hasRequiredAttributes(const ColorDefinition_t * cd)
{
  return (cd != NULL) ? static_cast<int>(cd->hasRequiredAttributes()) : 0;
}


LIBSBML_CPP_NAMESPACE_END


