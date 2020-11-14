/**
 * @file    GradientStop.cpp
 * @brief   class representing a stop in a gradient definition
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
#include <sbml/packages/render/sbml/GradientStop.h>
#include <sbml/packages/render/sbml/ListOfGradientStops.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>


using namespace std;

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/packages/layout/util/LayoutAnnotation.h> 
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new GradientStop using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
GradientStop::GradientStop(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion)
  : SBase(level, version)
  , mOffset (RelAbsVector(0.0, 0.0))
  , mStopColor ("")
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new GradientStop using the given RenderPkgNamespaces object.
 */
GradientStop::GradientStop(RenderPkgNamespaces *renderns)
  : SBase(renderns)
  , mOffset(RelAbsVector(0.0, 0.0))
  , mStopColor ("")
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/**
* Creates a new GradientStop object from the given XMLNode object.
* The XMLNode object has to contain a valid XML representation of a
* GradientStop object as defined in the render extension specification.
* This method is normally called when render information is read from a file and
* should normally not have to be called explicitly.
*
* @param node the XMLNode object reference that describes the GradientStop
* object to be instantiated.
*/
GradientStop::GradientStop(const XMLNode& node, unsigned int l2version) : SBase(2, l2version)
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
/*
 * Copy constructor for GradientStop.
 */
GradientStop::GradientStop(const GradientStop& orig)
  : SBase( orig )
  , mOffset ( orig.mOffset )
  , mStopColor ( orig.mStopColor )
{
  connectToChild();
}


/*
 * Assignment operator for GradientStop.
 */
GradientStop&
GradientStop::operator=(const GradientStop& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mStopColor = rhs.mStopColor;
    mOffset = rhs.mOffset;

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this GradientStop object.
 */
GradientStop*
GradientStop::clone() const
{
  return new GradientStop(*this);
}


/*
 * Destructor for GradientStop.
 */
GradientStop::~GradientStop()
{
}


/*
 * Returns the value of the "stop-color" attribute of this GradientStop.
 */
const std::string&
GradientStop::getStopColor() const
{
  return mStopColor;
}


/*
 * Predicate returning @c true if this GradientStop's "stop-color" attribute is
 * set.
 */
bool
GradientStop::isSetStopColor() const
{
  return (mStopColor.empty() == false);
}


/*
 * Sets the value of the "stop-color" attribute of this GradientStop.
 */
int
GradientStop::setStopColor(const std::string& stopColor)
{
  mStopColor = stopColor;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "stop-color" attribute of this GradientStop.
 */
int
GradientStop::unsetStopColor()
{
  mStopColor.erase();

  if (mStopColor.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the value of the "offset" element of this GradientStop.
 */
const RelAbsVector&
GradientStop::getOffset() const
{
  return mOffset;
}


/*
 * Returns the value of the "offset" element of this GradientStop.
 */
RelAbsVector&
GradientStop::getOffset()
{
  return mOffset;
}


/*
 * Predicate returning @c true if this GradientStop's "offset" element is set.
 */
bool
GradientStop::isSetOffset() const
{
  return true;
}


/*
 * Sets the value of the "offset" element of this GradientStop.
 */
int
GradientStop::setOffset(const RelAbsVector& offset)
{
  mOffset = offset;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the offset for the gradient stop.
 *
 * @param abs the absolute value of the offset.
 *
 * @param rel the relative value of the offset.
 */
void GradientStop::setOffset(double abs,double rel)
{
    this->mOffset=RelAbsVector(abs,rel);
}


/*
 * Sets the offset to the value specified by the given string.
 * The string has to represent a combination of an absolute 
 * and relative value.
 * Valid value string would e.g. be "45.0", "30%" or
 * "10+5%". If the value is a combination of both relative and 
 * absolute value, the absolute value has to come before the relative
 * value. Number can be given as integer values or floating point values
 * and the two components can be combined by '+' or '-'. Depending on
 * whethr the relative value should be added or subtracted from the 
 * absolute value.
 * If the given string is not valid, the offset will have an absolute 
 * and a relative value of NaN.
 *
 * @param a string representing a valid offset value.
 */
void GradientStop::setOffset(const std::string& co)
{
    this->mOffset=RelAbsVector(co);
}


/*
 * Unsets the value of the "offset" element of this GradientStop.
 */
int
GradientStop::unsetOffset()
{
  mOffset.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this GradientStop object.
 */
const std::string&
GradientStop::getElementName() const
{
  static const string name = "stop";
  return name;
}


/*
 * Returns the libSBML type code for this GradientStop object.
 */
int
GradientStop::getTypeCode() const
{
  return SBML_RENDER_GRADIENT_STOP;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * GradientStop object have been set.
 */
bool
GradientStop::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetStopColor() == false)
  {
    allPresent = false;
  }
  //render - FIX_ME

    //result = result && 
    //    (this->mOffset.getRelativeValue() == this->mOffset.getRelativeValue()) &&
    //    (this->mOffset.getAbsoluteValue() == this->mOffset.getAbsoluteValue());
    //result = result && (this->mStopColor.find_first_not_of(" \t\n\r") != std::string::npos);
  return allPresent;
}


/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
GradientStop::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  v.leave(*this);
  return true;
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::getAttribute(const std::string& attributeName,
                           bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::getAttribute(const std::string& attributeName,
                           double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::getAttribute(const std::string& attributeName,
                           unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::getAttribute(const std::string& attributeName,
                           std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "stop-color")
  {
    value = getStopColor();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this GradientStop's attribute "attributeName"
 * is set.
 */
bool
GradientStop::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "stop-color")
  {
    value = isSetStopColor();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::setAttribute(const std::string& attributeName,
                           unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::setAttribute(const std::string& attributeName,
                           const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "stop-color")
  {
    return_value = setStopColor(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this GradientStop.
 */
int
GradientStop::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "stop-color")
  {
    value = unsetStopColor();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this GradientStop object.
 *
 * @return the XMLNode with the XML representation for the 
 * GradientStop object.
 */
XMLNode GradientStop::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
GradientStop::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("stop-color");
  attributes.add("offset");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
GradientStop::readAttributes(const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfGradientStops*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderGradientStopAllowedAttributes,
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
        log->logPackageError("render", RenderGradientStopAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderGradientStopAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // stop-color string (use = "required" )
  // 

  assigned = attributes.readInto("stop-color", mStopColor);

  if (assigned == true)
  {
    if (log && mStopColor.empty() == true)
    {
      logEmptyString(mStopColor, level, version, "<GradientStop>");
    }
  }
  else
  {
    std::string message = "Render attribute 'stop-color' is missing from the "
      "<GradientStop> element.";
    if (log)
    {
      log->logPackageError("render", RenderGradientStopAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // offset string (use = "required" )
  // 
  std::string s;
  RelAbsVector v = RelAbsVector();

  //
  // offset RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("offset", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned && log)
  {
    std::string message = "Render attribute 'offset' is missing from the "
      "<GradientStop> element.";
    log->logPackageError("render", RenderGradientStopAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'offset' on the "
        "<GradientStop> does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderGradientStopOffsetMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setOffset(v);
    }
    v.erase();
  }

}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
GradientStop::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetOffset())
  {
    std::ostringstream os;
    os << this->mOffset;
    stream.writeAttribute("offset", getPrefix(), os.str());
  }

  if (isSetStopColor() == true)
  {
    stream.writeAttribute("stop-color", getPrefix(), mStopColor);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new GradientStop_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
GradientStop_t *
GradientStop_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion)
{
  return new GradientStop(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this GradientStop_t object.
 */
LIBSBML_EXTERN
GradientStop_t*
GradientStop_clone(const GradientStop_t* gs)
{
  if (gs != NULL)
  {
    return static_cast<GradientStop_t*>(gs->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this GradientStop_t object.
 */
LIBSBML_EXTERN
void
GradientStop_free(GradientStop_t* gs)
{
  if (gs != NULL)
  {
    delete gs;
  }
}


/*
 * Returns the value of the "stop-color" attribute of this GradientStop_t.
 */
LIBSBML_EXTERN
char *
GradientStop_getStopColor(const GradientStop_t * gs)
{
  if (gs == NULL)
  {
    return NULL;
  }

  return gs->getStopColor().empty() ? NULL :
    safe_strdup(gs->getStopColor().c_str());
}


/*
 * Predicate returning @c 1 (true) if this GradientStop_t's "stop-color"
 * attribute is set.
 */
LIBSBML_EXTERN
int
GradientStop_isSetStopColor(const GradientStop_t * gs)
{
  return (gs != NULL) ? static_cast<int>(gs->isSetStopColor()) : 0;
}


/*
 * Sets the value of the "stop-color" attribute of this GradientStop_t.
 */
LIBSBML_EXTERN
int
GradientStop_setStopColor(GradientStop_t * gs, const char * stopColor)
{
  return (gs != NULL) ? gs->setStopColor(stopColor) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "stop-color" attribute of this GradientStop_t.
 */
LIBSBML_EXTERN
int
GradientStop_unsetStopColor(GradientStop_t * gs)
{
  return (gs != NULL) ? gs->unsetStopColor() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "offset" element of this GradientStop_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
GradientStop_getOffset(const GradientStop_t * gs)
{
  if (gs == NULL)
  {
    return NULL;
  }

  return &(gs->getOffset());
}


/*
 * Predicate returning @c 1 (true) if this GradientStop_t's "offset" element is
 * set.
 */
LIBSBML_EXTERN
int
GradientStop_isSetOffset(const GradientStop_t * gs)
{
  return (gs != NULL) ? static_cast<int>(gs->isSetOffset()) : 0;
}


/*
 * Sets the value of the "offset" element of this GradientStop_t.
 */
LIBSBML_EXTERN
int
GradientStop_setOffset(GradientStop_t * gs, const RelAbsVector_t& offset)
{
  return (gs != NULL) ? gs->setOffset(offset) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "offset" element of this GradientStop_t.
 */
LIBSBML_EXTERN
int
GradientStop_unsetOffset(GradientStop_t * gs)
{
  return (gs != NULL) ? gs->unsetOffset() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GradientStop_t object have been set.
 */
LIBSBML_EXTERN
int
GradientStop_hasRequiredAttributes(const GradientStop_t * gs)
{
  return (gs != NULL) ? static_cast<int>(gs->hasRequiredAttributes()) : 0;
}


LIBSBML_CPP_NAMESPACE_END


