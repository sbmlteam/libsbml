/**
 * @file ListOfGradientDefinitions.cpp
 * @brief Implementation of the ListOfGradientDefinitions class.
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
#include <sbml/packages/render/sbml/ListOfGradientDefinitions.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#include <sbml/packages/render/sbml/LinearGradient.h>
#include <sbml/packages/render/sbml/RadialGradient.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfGradientDefinitions using the given SBML Level, Version
 * and &ldquo;render&rdquo; package version.
 */
ListOfGradientDefinitions::ListOfGradientDefinitions(unsigned int level,
                                                     unsigned int version,
                                                     unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfGradientDefinitions using the given RenderPkgNamespaces
 * object.
 */
ListOfGradientDefinitions::ListOfGradientDefinitions(RenderPkgNamespaces
  *renderns)
  : ListOf(renderns)
{
  setElementNamespace(renderns->getURI());
}
/*
 * Creates a new ListOfGradientDefinitions object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * ListOfGradientDefinitions object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the ListOfGradientDefinitions
 * object to be instantiated.
 */
ListOfGradientDefinitions::ListOfGradientDefinitions(const XMLNode& node, unsigned int l2version)
  : ListOf(2, l2version)
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
        if(childName=="linearGradient")
        {
            LinearGradient* gradient=new LinearGradient(*child, l2version);
            this->appendAndOwn(gradient);
        }
        else if(childName=="radialGradient")
        {
            RadialGradient* gradient=new RadialGradient(*child, l2version);
            this->appendAndOwn(gradient);
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


/*
 * Copy constructor for ListOfGradientDefinitions.
 */
ListOfGradientDefinitions::ListOfGradientDefinitions(const
  ListOfGradientDefinitions& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfGradientDefinitions.
 */
ListOfGradientDefinitions&
ListOfGradientDefinitions::operator=(const ListOfGradientDefinitions& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfGradientDefinitions object.
 */
ListOfGradientDefinitions*
ListOfGradientDefinitions::clone() const
{
  return new ListOfGradientDefinitions(*this);
}


/*
 * Destructor for ListOfGradientDefinitions.
 */
ListOfGradientDefinitions::~ListOfGradientDefinitions()
{
}


/*
 * Get a GradientBase from the ListOfGradientDefinitions.
 */
GradientBase*
ListOfGradientDefinitions::get(unsigned int n)
{
  return static_cast<GradientBase*>(ListOf::get(n));
}


/*
 * Get a GradientBase from the ListOfGradientDefinitions.
 */
const GradientBase*
ListOfGradientDefinitions::get(unsigned int n) const
{
  return static_cast<const GradientBase*>(ListOf::get(n));
}


/*
 * Get a GradientBase from the ListOfGradientDefinitions based on its
 * identifier.
 */
GradientBase*
ListOfGradientDefinitions::get(const std::string& sid)
{
  return const_cast<GradientBase*>(static_cast<const
    ListOfGradientDefinitions&>(*this).get(sid));
}


/*
 * Get a GradientBase from the ListOfGradientDefinitions based on its
 * identifier.
 */
const GradientBase*
ListOfGradientDefinitions::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<GradientBase>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const GradientBase*>
    (*result);
}


/*
 * Removes the nth GradientBase from this ListOfGradientDefinitions and returns
 * a pointer to it.
 */
GradientBase*
ListOfGradientDefinitions::remove(unsigned int n)
{
  return static_cast<GradientBase*>(ListOf::remove(n));
}


/*
 * Removes the GradientBase from this ListOfGradientDefinitions based on its
 * identifier and returns a pointer to it.
 */
GradientBase*
ListOfGradientDefinitions::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<GradientBase>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <GradientBase*> (item);
}


/*
 * Adds a copy of the given GradientBase to this ListOfGradientDefinitions.
 */
int
ListOfGradientDefinitions::addGradientBase(const GradientBase* gb)
{
  if (gb == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (gb->hasRequiredAttributes() == false)
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
  else
  {
    return append(gb);
  }
}


/*
 * Get the number of GradientBase objects in this ListOfGradientDefinitions.
 */
unsigned int
ListOfGradientDefinitions::getNumGradientBases() const
{
  return size();
}


/*
 * Creates a new LinearGradient object, adds it to this
 * ListOfGradientDefinitions object and returns the LinearGradient object
 * created.
 */
LinearGradient*
ListOfGradientDefinitions::createLinearGradient()
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
    appendAndOwn(lg);
  }

  return lg;
}


/*
 * Creates a new RadialGradient object, adds it to this
 * ListOfGradientDefinitions object and returns the RadialGradient object
 * created.
 */
RadialGradient*
ListOfGradientDefinitions::createRadialGradient()
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
    appendAndOwn(rg);
  }

  return rg;
}


/*
 * Returns the XML element name of this ListOfGradientDefinitions object.
 */
const std::string&
ListOfGradientDefinitions::getElementName() const
{
  static const string name = "listOfGradientDefinitions";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfGradientDefinitions object.
 */
int
ListOfGradientDefinitions::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfGradientDefinitions object.
 */
int
ListOfGradientDefinitions::getItemTypeCode() const
{
  return SBML_RENDER_GRADIENTDEFINITION;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new GradientBase in this ListOfGradientDefinitions
 */
SBase*
ListOfGradientDefinitions::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  if (name == "linearGradient")
  {
    object = new LinearGradient(renderns);
    appendAndOwn(object);
  }

  if (name == "radialGradient")
  {
    object = new RadialGradient(renderns);
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
ListOfGradientDefinitions::writeXMLNS(XMLOutputStream& stream) const
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
 * checks concrete types
 */
bool
ListOfGradientDefinitions::isValidTypeForList(SBase* item)
{
  unsigned int tc = item->getTypeCode();

  return ((tc == SBML_RENDER_LINEARGRADIENT) || (tc ==
    SBML_RENDER_RADIALGRADIENT));
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get a GradientBase_t from the ListOf_t.
 */
LIBSBML_EXTERN
GradientBase_t*
ListOfGradientDefinitions_getGradientBase(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGradientDefinitions*>(lo)->get(n);
}


/*
 * Get a GradientBase_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
GradientBase_t*
ListOfGradientDefinitions_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfGradientDefinitions*>(lo)->get(sid)
    : NULL;
}


/*
 * Removes the nth GradientBase_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
GradientBase_t*
ListOfGradientDefinitions_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfGradientDefinitions*>(lo)->remove(n);
}


/*
 * Removes the GradientBase_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
GradientBase_t*
ListOfGradientDefinitions_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfGradientDefinitions*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


