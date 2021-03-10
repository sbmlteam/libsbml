/**
 * @file ListOfLocalStyles.cpp
 * @brief Implementation of the ListOfLocalStyles class.
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
#include <sbml/packages/render/sbml/ListOfLocalStyles.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfLocalStyles using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
ListOfLocalStyles::ListOfLocalStyles(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfLocalStyles using the given RenderPkgNamespaces object.
 */
ListOfLocalStyles::ListOfLocalStyles(RenderPkgNamespaces *renderns)
  : ListOf(renderns)
{
  setElementNamespace(renderns->getURI());
}


ListOfLocalStyles::ListOfLocalStyles(const XMLNode& node)
{
  const XMLAttributes& attributes = node.getAttributes();
  const XMLNode* child;
  ExpectedAttributes ea;
  addExpectedAttributes(ea);
  mURI = RenderExtension::getXmlnsL3V1V1();
  this->readAttributes(attributes, ea);
  unsigned int n = 0, nMax = node.getNumChildren();
  while (n<nMax)
  {
    child = &node.getChild(n);
    const std::string& childName = child->getName();
    if (childName == "style")
    {
      LocalStyle* style = new LocalStyle(*child);
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
 * Copy constructor for ListOfLocalStyles.
 */
ListOfLocalStyles::ListOfLocalStyles(const ListOfLocalStyles& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfLocalStyles.
 */
ListOfLocalStyles&
ListOfLocalStyles::operator=(const ListOfLocalStyles& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfLocalStyles object.
 */
ListOfLocalStyles*
ListOfLocalStyles::clone() const
{
  return new ListOfLocalStyles(*this);
}


/*
 * Destructor for ListOfLocalStyles.
 */
ListOfLocalStyles::~ListOfLocalStyles()
{
}


/*
 * Get a LocalStyle from the ListOfLocalStyles.
 */
LocalStyle*
ListOfLocalStyles::get(unsigned int n)
{
  return static_cast<LocalStyle*>(ListOf::get(n));
}


/*
 * Get a LocalStyle from the ListOfLocalStyles.
 */
const LocalStyle*
ListOfLocalStyles::get(unsigned int n) const
{
  return static_cast<const LocalStyle*>(ListOf::get(n));
}

/** @cond doxygenLibsbmlInternal */
/*
* Used by ListOf::get() to lookup an SBase based by its id.
*/
struct IdEqLocalStyle
{
  const std::string& id;

  IdEqLocalStyle(const std::string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
    return static_cast <LocalStyle *> (sb)->getId() == id;
  }
};
/** @endcond */


/*
 * Get a LocalStyle from the ListOfLocalStyles based on its identifier.
 */
LocalStyle*
ListOfLocalStyles::get(const std::string& sid)
{
  return const_cast<LocalStyle*>(static_cast<const
    ListOfLocalStyles&>(*this).get(sid));
}


/*
 * Get a LocalStyle from the ListOfLocalStyles based on its identifier.
 */
const LocalStyle*
ListOfLocalStyles::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqLocalStyle(sid));
  return (result == mItems.end()) ? 0 : static_cast <const LocalStyle*>
    (*result);
}


/*
 * Removes the nth LocalStyle from this ListOfLocalStyles and returns a pointer
 * to it.
 */
LocalStyle*
ListOfLocalStyles::remove(unsigned int n)
{
  return static_cast<LocalStyle*>(ListOf::remove(n));
}


/*
 * Removes the LocalStyle from this ListOfLocalStyles based on its identifier
 * and returns a pointer to it.
 */
LocalStyle*
ListOfLocalStyles::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEqLocalStyle(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <LocalStyle*> (item);
}


/*
 * Adds a copy of the given LocalStyle to this ListOfLocalStyles.
 */
int
ListOfLocalStyles::addLocalStyle(const LocalStyle* ls)
{
  if (ls == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ls->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ls->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ls->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(ls)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(ls);
  }
}


/*
 * Get the number of LocalStyle objects in this ListOfLocalStyles.
 */
unsigned int
ListOfLocalStyles::getNumLocalStyles() const
{
  return size();
}


/*
 * Creates a new LocalStyle object, adds it to this ListOfLocalStyles object
 * and returns the LocalStyle object created.
 */
LocalStyle*
ListOfLocalStyles::createLocalStyle()
{
  LocalStyle* ls = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    ls = new LocalStyle(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (ls != NULL)
  {
    appendAndOwn(ls);
  }

  return ls;
}


/*
 * Returns the XML element name of this ListOfLocalStyles object.
 */
const std::string&
ListOfLocalStyles::getElementName() const
{
  static const string name = "listOfStyles";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfLocalStyles object.
 */
int
ListOfLocalStyles::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfLocalStyles object.
 */
int
ListOfLocalStyles::getItemTypeCode() const
{
  return SBML_RENDER_LOCALSTYLE;
}


XMLNode ListOfLocalStyles::toXML() const
{
  return getXmlNodeForSBase(this);
}


/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new LocalStyle in this ListOfLocalStyles
 */
SBase*
ListOfLocalStyles::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  if (name == "style")
  {
    object = new LocalStyle(renderns);
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
ListOfLocalStyles::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a LocalStyle_t from the ListOf_t.
 */
LIBSBML_EXTERN
LocalStyle_t*
ListOfLocalStyles_getLocalStyle(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfLocalStyles*>(lo)->get(n);
}


/*
 * Get a LocalStyle_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
LocalStyle_t*
ListOfLocalStyles_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfLocalStyles*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth LocalStyle_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
LocalStyle_t*
ListOfLocalStyles_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfLocalStyles*>(lo)->remove(n);
}


/*
 * Removes the LocalStyle_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
LocalStyle_t*
ListOfLocalStyles_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfLocalStyles*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


