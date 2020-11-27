/**
 * @file ListOfGradientStops.cpp
 * @brief Implementation of the ListOfGradientStops class.
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
#include <sbml/packages/render/sbml/ListOfGradientStops.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>


using namespace std;

#include <sbml/packages/layout/util/LayoutAnnotation.h> 
#include <sbml/packages/layout/util/LayoutUtilities.h>


LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfGradientStops using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
ListOfGradientStops::ListOfGradientStops(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfGradientStops using the given RenderPkgNamespaces
 * object.
 */
ListOfGradientStops::ListOfGradientStops(RenderPkgNamespaces *renderns)
  : ListOf(renderns)
{
  setElementNamespace(renderns->getURI());
}


/** @cond doxygenLibsbmlInternal */
/**
* Creates a new ListOfGradientStops object from the given XMLNode object.
* The XMLNode object has to contain a valid XML representation of a
* ListOfGradientStops object as defined in the render extension specification.
* This method is normally called when render information is read from a file and
* should normally not have to be called explicitly.
*
* @param node the XMLNode object reference that describes the ListOfGradientStops
* object to be instantiated.
*/
ListOfGradientStops::ListOfGradientStops(const XMLNode& node, unsigned int l2version)
  : ListOf(2, l2version)
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
        if(childName=="gradientStop")
        {
            GradientStop* stop=new GradientStop(*child);
            this->appendAndOwn(stop);
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
 * Copy constructor for ListOfGradientStops.
 */
ListOfGradientStops::ListOfGradientStops(const ListOfGradientStops& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfGradientStops.
 */
ListOfGradientStops&
ListOfGradientStops::operator=(const ListOfGradientStops& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfGradientStops object.
 */
ListOfGradientStops*
ListOfGradientStops::clone() const
{
  return new ListOfGradientStops(*this);
}


/*
 * Destructor for ListOfGradientStops.
 */
ListOfGradientStops::~ListOfGradientStops()
{
}


/*
 * Get a GradientStop from the ListOfGradientStops.
 */
GradientStop*
ListOfGradientStops::get(unsigned int n)
{
  return static_cast<GradientStop*>(ListOf::get(n));
}


/*
 * Get a GradientStop from the ListOfGradientStops.
 */
const GradientStop*
ListOfGradientStops::get(unsigned int n) const
{
  return static_cast<const GradientStop*>(ListOf::get(n));
}


/*
 * Get a GradientStop from the ListOfGradientStops based on its identifier.
 */
GradientStop*
ListOfGradientStops::get(const std::string& sid)
{
  return const_cast<GradientStop*>(static_cast<const
    ListOfGradientStops&>(*this).get(sid));
}


/*
 * Get a GradientStop from the ListOfGradientStops based on its identifier.
 */
const GradientStop*
ListOfGradientStops::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<GradientStop>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const GradientStop*>
    (*result);
}


/*
 * Removes the nth GradientStop from this ListOfGradientStops and returns a
 * pointer to it.
 */
GradientStop*
ListOfGradientStops::remove(unsigned int n)
{
  return static_cast<GradientStop*>(ListOf::remove(n));
}


/*
 * Removes the GradientStop from this ListOfGradientStops based on its
 * identifier and returns a pointer to it.
 */
GradientStop*
ListOfGradientStops::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<GradientStop>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <GradientStop*> (item);
}


/*
 * Adds a copy of the given GradientStop to this ListOfGradientStops.
 */
int
ListOfGradientStops::addGradientStop(const GradientStop* gs)
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
 * Get the number of GradientStop objects in this ListOfGradientStops.
 */
unsigned int
ListOfGradientStops::getNumGradientStops() const
{
  return size();
}


/*
 * Creates a new GradientStop object, adds it to this ListOfGradientStops
 * object and returns the GradientStop object created.
 */
GradientStop*
ListOfGradientStops::createGradientStop()
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
    appendAndOwn(gs);
  }

  return gs;
}


/*
 * Returns the XML element name of this ListOfGradientStops object.
 */
const std::string&
ListOfGradientStops::getElementName() const
{
  static const string name = "listOfGradientStops";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfGradientStops object.
 */
int
ListOfGradientStops::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfGradientStops object.
 */
int
ListOfGradientStops::getItemTypeCode() const
{
  return SBML_RENDER_GRADIENT_STOP;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new GradientStop in this ListOfGradientStops
 */
SBase*
ListOfGradientStops::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

    if (name == "stop")
  {
    object = new GradientStop(renderns);
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
ListOfGradientStops::writeXMLNS(XMLOutputStream& stream) const
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

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfGradientStops object.
 *
 * @return the XMLNode with the XML representation for the 
 * ListOfGradientStops object.
 */
XMLNode ListOfGradientStops::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */




#endif /* __cplusplus */


/*
 * Get a GradientStop_t from the ListOf_t.
 */
LIBSBML_EXTERN
GradientStop_t*
ListOfGradientStops_getGradientStop(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGradientStops*>(lo)->get(n);
}


/*
 * Get a GradientStop_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
GradientStop_t*
ListOfGradientStops_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfGradientStops*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth GradientStop_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
GradientStop_t*
ListOfGradientStops_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGradientStops*>(lo)->remove(n);
}


/*
 * Removes the GradientStop_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
GradientStop_t*
ListOfGradientStops_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfGradientStops*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


