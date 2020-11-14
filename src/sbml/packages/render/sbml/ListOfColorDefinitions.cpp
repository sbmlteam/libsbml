/**
 * @file ListOfColorDefinitions.cpp
 * @brief Implementation of the ListOfColorDefinitions class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2013-2017 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/render/sbml/ListOfColorDefinitions.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new ListOfColorDefinitions object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * ListOfColorDefinitions object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the ListOfColorDefinitions
 * object to be instantiated.
 */
ListOfColorDefinitions::ListOfColorDefinitions(const XMLNode& node)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    ExpectedAttributes ea;
    mURI = RenderExtension::getXmlnsL3V1V1();
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="colorDefinition")
        {
            ColorDefinition* cd=new ColorDefinition(*child);
      this->appendAndOwn(cd);
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


/*
 * Creates a new ListOfColorDefinitions using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
ListOfColorDefinitions::ListOfColorDefinitions(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfColorDefinitions using the given RenderPkgNamespaces
 * object.
 */
ListOfColorDefinitions::ListOfColorDefinitions(RenderPkgNamespaces *renderns)
  : ListOf(renderns)
{
  setElementNamespace(renderns->getURI());
}


/*
 * Copy constructor for ListOfColorDefinitions.
 */
ListOfColorDefinitions::ListOfColorDefinitions(const ListOfColorDefinitions&
  orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfColorDefinitions.
 */
ListOfColorDefinitions&
ListOfColorDefinitions::operator=(const ListOfColorDefinitions& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfColorDefinitions object.
 */
ListOfColorDefinitions*
ListOfColorDefinitions::clone() const
{
  return new ListOfColorDefinitions(*this);
}


/*
 * Destructor for ListOfColorDefinitions.
 */
ListOfColorDefinitions::~ListOfColorDefinitions()
{
}


/*
 * Get a ColorDefinition from the ListOfColorDefinitions.
 */
ColorDefinition*
ListOfColorDefinitions::get(unsigned int n)
{
  return static_cast<ColorDefinition*>(ListOf::get(n));
}


/*
 * Get a ColorDefinition from the ListOfColorDefinitions.
 */
const ColorDefinition*
ListOfColorDefinitions::get(unsigned int n) const
{
  return static_cast<const ColorDefinition*>(ListOf::get(n));
}


/*
 * Get a ColorDefinition from the ListOfColorDefinitions based on its
 * identifier.
 */
ColorDefinition*
ListOfColorDefinitions::get(const std::string& sid)
{
  return const_cast<ColorDefinition*>(static_cast<const
    ListOfColorDefinitions&>(*this).get(sid));
}


/*
 * Get a ColorDefinition from the ListOfColorDefinitions based on its
 * identifier.
 */
const ColorDefinition*
ListOfColorDefinitions::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<ColorDefinition>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const ColorDefinition*>
    (*result);
}


/*
 * Removes the nth ColorDefinition from this ListOfColorDefinitions and returns
 * a pointer to it.
 */
ColorDefinition*
ListOfColorDefinitions::remove(unsigned int n)
{
  return static_cast<ColorDefinition*>(ListOf::remove(n));
}


/*
 * Removes the ColorDefinition from this ListOfColorDefinitions based on its
 * identifier and returns a pointer to it.
 */
ColorDefinition*
ListOfColorDefinitions::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<ColorDefinition>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <ColorDefinition*> (item);
}


/*
 * Adds a copy of the given ColorDefinition to this ListOfColorDefinitions.
 */
int
ListOfColorDefinitions::addColorDefinition(const ColorDefinition* cd)
{
  if (cd == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (cd->hasRequiredAttributes() == false)
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
  else
  {
    return append(cd);
  }
}


/*
 * Get the number of ColorDefinition objects in this ListOfColorDefinitions.
 */
unsigned int
ListOfColorDefinitions::getNumColorDefinitions() const
{
  return size();
}


/*
 * Creates a new ColorDefinition object, adds it to this ListOfColorDefinitions
 * object and returns the ColorDefinition object created.
 */
ColorDefinition*
ListOfColorDefinitions::createColorDefinition()
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
    appendAndOwn(cd);
  }

  return cd;
}


/*
 * Returns the XML element name of this ListOfColorDefinitions object.
 */
const std::string&
ListOfColorDefinitions::getElementName() const
{
  static const string name = "listOfColorDefinitions";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfColorDefinitions object.
 */
int
ListOfColorDefinitions::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfColorDefinitions object.
 */
int
ListOfColorDefinitions::getItemTypeCode() const
{
  return SBML_RENDER_COLORDEFINITION;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfColorDefinitions object.
 *
 * @return the XMLNode with the XML representation for the 
 * ListOfColorDefinitions object.
 */
XMLNode ListOfColorDefinitions::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new ColorDefinition in this ListOfColorDefinitions
 */
SBase*
ListOfColorDefinitions::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  if (name == "colorDefinition")
  {
    object = new ColorDefinition(renderns);
    appendAndOwn(object);
  }

  delete renderns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Render package
 */
void
ListOfColorDefinitions::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(RenderExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(RenderExtension::getXmlnsL3V1V1(), prefix);
    }
  }

  stream << xmlns;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get a ColorDefinition_t from the ListOf_t.
 */
LIBSBML_EXTERN
ColorDefinition_t*
ListOfColorDefinitions_getColorDefinition(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfColorDefinitions*>(lo)->get(n);
}


/*
 * Get a ColorDefinition_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
ColorDefinition_t*
ListOfColorDefinitions_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfColorDefinitions*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth ColorDefinition_t from this ListOf_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
ColorDefinition_t*
ListOfColorDefinitions_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfColorDefinitions*>(lo)->remove(n);
}


/*
 * Removes the ColorDefinition_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
ColorDefinition_t*
ListOfColorDefinitions_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfColorDefinitions*>(lo)->remove(sid)
    : NULL;
}




LIBSBML_CPP_NAMESPACE_END


