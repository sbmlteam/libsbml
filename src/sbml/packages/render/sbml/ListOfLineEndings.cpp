/**
 * @file ListOfLineEndings.cpp
 * @brief Implementation of the ListOfLineEndings class.
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
#include <sbml/packages/render/sbml/ListOfLineEndings.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfLineEndings using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
ListOfLineEndings::ListOfLineEndings(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfLineEndings using the given RenderPkgNamespaces object.
 */
ListOfLineEndings::ListOfLineEndings(RenderPkgNamespaces *renderns)
  : ListOf(renderns)
{
  setElementNamespace(renderns->getURI());
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new ListOfLineEndings object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * ListOfLineEndings object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the ListOfLineEndings
 * object to be instantiated.
 */
    ListOfLineEndings::ListOfLineEndings(const XMLNode& node, unsigned int l2version)       
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
        if(childName=="lineEnding")
        {
            LineEnding* le=new LineEnding(*child, l2version);
            this->appendAndOwn(le);
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


/*
 * Copy constructor for ListOfLineEndings.
 */
ListOfLineEndings::ListOfLineEndings(const ListOfLineEndings& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfLineEndings.
 */
ListOfLineEndings&
ListOfLineEndings::operator=(const ListOfLineEndings& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfLineEndings object.
 */
ListOfLineEndings*
ListOfLineEndings::clone() const
{
  return new ListOfLineEndings(*this);
}


/*
 * Destructor for ListOfLineEndings.
 */
ListOfLineEndings::~ListOfLineEndings()
{
}


/*
 * Get a LineEnding from the ListOfLineEndings.
 */
LineEnding*
ListOfLineEndings::get(unsigned int n)
{
  return static_cast<LineEnding*>(ListOf::get(n));
}


/*
 * Get a LineEnding from the ListOfLineEndings.
 */
const LineEnding*
ListOfLineEndings::get(unsigned int n) const
{
  return static_cast<const LineEnding*>(ListOf::get(n));
}


/*
 * Get a LineEnding from the ListOfLineEndings based on its identifier.
 */
LineEnding*
ListOfLineEndings::get(const std::string& sid)
{
  return const_cast<LineEnding*>(static_cast<const
    ListOfLineEndings&>(*this).get(sid));
}


/*
 * Get a LineEnding from the ListOfLineEndings based on its identifier.
 */
const LineEnding*
ListOfLineEndings::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<LineEnding>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const LineEnding*>
    (*result);
}


/*
 * Removes the nth LineEnding from this ListOfLineEndings and returns a pointer
 * to it.
 */
LineEnding*
ListOfLineEndings::remove(unsigned int n)
{
  return static_cast<LineEnding*>(ListOf::remove(n));
}


/*
 * Removes the LineEnding from this ListOfLineEndings based on its identifier
 * and returns a pointer to it.
 */
LineEnding*
ListOfLineEndings::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<LineEnding>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <LineEnding*> (item);
}


/*
 * Adds a copy of the given LineEnding to this ListOfLineEndings.
 */
int
ListOfLineEndings::addLineEnding(const LineEnding* le)
{
  if (le == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (le->hasRequiredAttributes() == false)
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
  else
  {
    return append(le);
  }
}


/*
 * Get the number of LineEnding objects in this ListOfLineEndings.
 */
unsigned int
ListOfLineEndings::getNumLineEndings() const
{
  return size();
}


/*
 * Creates a new LineEnding object, adds it to this ListOfLineEndings object
 * and returns the LineEnding object created.
 */
LineEnding*
ListOfLineEndings::createLineEnding()
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
    appendAndOwn(le);
  }

  return le;
}


/*
 * Returns the XML element name of this ListOfLineEndings object.
 */
const std::string&
ListOfLineEndings::getElementName() const
{
  static const string name = "listOfLineEndings";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfLineEndings object.
 */
int
ListOfLineEndings::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfLineEndings object.
 */
int
ListOfLineEndings::getItemTypeCode() const
{
  return SBML_RENDER_LINEENDING;
}



/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfLineEndings object.
 *
 * @return the XMLNode with the XML representation for the 
 * ListOfLineEndings object.
 */
XMLNode ListOfLineEndings::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new LineEnding in this ListOfLineEndings
 */
SBase*
ListOfLineEndings::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  if (name == "lineEnding")
  {
    object = new LineEnding(renderns);
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
ListOfLineEndings::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a LineEnding_t from the ListOf_t.
 */
LIBSBML_EXTERN
LineEnding_t*
ListOfLineEndings_getLineEnding(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfLineEndings*>(lo)->get(n);
}


/*
 * Get a LineEnding_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
LineEnding_t*
ListOfLineEndings_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfLineEndings*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth LineEnding_t from this ListOf_t and returns a pointer to it.
 */
LIBSBML_EXTERN
LineEnding_t*
ListOfLineEndings_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfLineEndings*>(lo)->remove(n);
}


/*
 * Removes the LineEnding_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
LineEnding_t*
ListOfLineEndings_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfLineEndings*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


