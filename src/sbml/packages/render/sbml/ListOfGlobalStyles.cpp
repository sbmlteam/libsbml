/**
 * @file ListOfGlobalStyles.cpp
 * @brief Implementation of the ListOfGlobalStyles class.
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
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
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
#include <sbml/packages/render/sbml/ListOfGlobalStyles.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfGlobalStyles using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
ListOfGlobalStyles::ListOfGlobalStyles(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfGlobalStyles using the given RenderPkgNamespaces object.
 */
ListOfGlobalStyles::ListOfGlobalStyles(RenderPkgNamespaces *renderns)
  : ListOf(renderns)
{
  setElementNamespace(renderns->getURI());
}


ListOfGlobalStyles::ListOfGlobalStyles(const XMLNode& node)
{
  const XMLAttributes& attributes = node.getAttributes();
  const XMLNode* child;
  ExpectedAttributes ea;
  mURI = RenderExtension::getXmlnsL3V1V1();
  addExpectedAttributes(ea);
  this->readAttributes(attributes, ea);
  unsigned int n = 0, nMax = node.getNumChildren();
  while (n<nMax)
  {
    child = &node.getChild(n);
    const std::string& childName = child->getName();
    if (childName == "style")
    {
      GlobalStyle* style = new GlobalStyle(*child);
      this->appendAndOwn(style);
    }
    else if (childName == "annotation")
    {
      this->mAnnotation = new XMLNode(*child);
    }
    else if (childName == "notes")
    {
      this->mNotes = new XMLNode(*child);
    }
    ++n;
  }
}


/*
 * Copy constructor for ListOfGlobalStyles.
 */
ListOfGlobalStyles::ListOfGlobalStyles(const ListOfGlobalStyles& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfGlobalStyles.
 */
ListOfGlobalStyles&
ListOfGlobalStyles::operator=(const ListOfGlobalStyles& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfGlobalStyles object.
 */
ListOfGlobalStyles*
ListOfGlobalStyles::clone() const
{
  return new ListOfGlobalStyles(*this);
}


/*
 * Destructor for ListOfGlobalStyles.
 */
ListOfGlobalStyles::~ListOfGlobalStyles()
{
}


/*
 * Get a GlobalStyle from the ListOfGlobalStyles.
 */
GlobalStyle*
ListOfGlobalStyles::get(unsigned int n)
{
  return static_cast<GlobalStyle*>(ListOf::get(n));
}


/*
 * Get a GlobalStyle from the ListOfGlobalStyles.
 */
const GlobalStyle*
ListOfGlobalStyles::get(unsigned int n) const
{
  return static_cast<const GlobalStyle*>(ListOf::get(n));
}


/*
 * Get a GlobalStyle from the ListOfGlobalStyles based on its identifier.
 */
GlobalStyle*
ListOfGlobalStyles::get(const std::string& sid)
{
  return const_cast<GlobalStyle*>(static_cast<const
    ListOfGlobalStyles&>(*this).get(sid));
}


/*
 * Get a GlobalStyle from the ListOfGlobalStyles based on its identifier.
 */
const GlobalStyle*
ListOfGlobalStyles::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<GlobalStyle>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const GlobalStyle*>
    (*result);
}


/*
 * Removes the nth GlobalStyle from this ListOfGlobalStyles and returns a
 * pointer to it.
 */
GlobalStyle*
ListOfGlobalStyles::remove(unsigned int n)
{
  return static_cast<GlobalStyle*>(ListOf::remove(n));
}


/*
 * Removes the GlobalStyle from this ListOfGlobalStyles based on its identifier
 * and returns a pointer to it.
 */
GlobalStyle*
ListOfGlobalStyles::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<GlobalStyle>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <GlobalStyle*> (item);
}


/*
 * Adds a copy of the given GlobalStyle to this ListOfGlobalStyles.
 */
int
ListOfGlobalStyles::addGlobalStyle(const GlobalStyle* gs)
{
  if (gs == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gs->hasRequiredAttributes() == false)
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
    return append(gs);
  }
}


/*
 * Get the number of GlobalStyle objects in this ListOfGlobalStyles.
 */
unsigned int
ListOfGlobalStyles::getNumGlobalStyles() const
{
  return size();
}


/*
 * Creates a new GlobalStyle object, adds it to this ListOfGlobalStyles object
 * and returns the GlobalStyle object created.
 */
GlobalStyle*
ListOfGlobalStyles::createGlobalStyle()
{
  GlobalStyle* gs = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    gs = new GlobalStyle(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (gs != NULL)
  {
    appendAndOwn(gs);
  }

  return gs;
}


/*
 * Returns the XML element name of this ListOfGlobalStyles object.
 */
const std::string&
ListOfGlobalStyles::getElementName() const
{
  static const string name = "listOfStyles";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfGlobalStyles object.
 */
int
ListOfGlobalStyles::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfGlobalStyles object.
 */
int
ListOfGlobalStyles::getItemTypeCode() const
{
  return SBML_RENDER_GLOBALSTYLE;
}


XMLNode ListOfGlobalStyles::toXML() const
{
  return getXmlNodeForSBase(this);
}


/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new GlobalStyle in this ListOfGlobalStyles
 */
SBase*
ListOfGlobalStyles::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  if (name == "style")
  {
    object = new GlobalStyle(renderns);
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
ListOfGlobalStyles::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a GlobalStyle_t from the ListOf_t.
 */
LIBSBML_EXTERN
GlobalStyle_t*
ListOfGlobalStyles_getGlobalStyle(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGlobalStyles*>(lo)->get(n);
}


/*
 * Get a GlobalStyle_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
GlobalStyle_t*
ListOfGlobalStyles_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfGlobalStyles*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth GlobalStyle_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
GlobalStyle_t*
ListOfGlobalStyles_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGlobalStyles*>(lo)->remove(n);
}


/*
 * Removes the GlobalStyle_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
GlobalStyle_t*
ListOfGlobalStyles_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfGlobalStyles*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


