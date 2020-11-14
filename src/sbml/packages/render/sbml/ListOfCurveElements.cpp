/**
 * @file    ListOfCurveElements.cpp
 * @brief   storage class for curve elements
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
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/packages/render/sbml/ListOfCurveElements.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#include <sbml/packages/render/sbml/RenderCubicBezier.h>
#include <sbml/packages/render/sbml/RenderPoint.h>

#include <sbml/xml/XMLInputStream.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <assert.h>
using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfCurveElements using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
ListOfCurveElements::ListOfCurveElements(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfCurveElements using the given RenderPkgNamespaces
 * object.
 */
ListOfCurveElements::ListOfCurveElements(RenderPkgNamespaces *renderns)
  : ListOf(renderns)
{
  setElementNamespace(renderns->getURI());
}

/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new ListOfCurveElements object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * ListOfCurveElements object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the ListOfCurveElements
 * object to be instantiated.
 */
ListOfCurveElements::ListOfCurveElements(const XMLNode& node, unsigned int l2version)
  : ListOf(2, l2version)
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
        if(childName=="element")
        {
            RenderPoint* ls=NULL;
            const XMLAttributes& innerAttributes=child->getAttributes();
            int typeIndex=innerAttributes.getIndex("type");
            if(typeIndex==-1 || innerAttributes.getURI(typeIndex)!="http://www.w3.org/2001/XMLSchema-instance")
            {
                // throw
                ++n;
                continue;
            }
            if(innerAttributes.getValue(typeIndex)=="RenderCubicBezier")
            {
                ls=new RenderCubicBezier(*child);
            }
            else
            {
                ls=new RenderPoint(*child);
            }
            this->appendAndOwn(ls);
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
 * Copy constructor for ListOfCurveElements.
 */
ListOfCurveElements::ListOfCurveElements(const ListOfCurveElements& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfCurveElements.
 */
ListOfCurveElements&
ListOfCurveElements::operator=(const ListOfCurveElements& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfCurveElements object.
 */
ListOfCurveElements*
ListOfCurveElements::clone() const
{
  return new ListOfCurveElements(*this);
}


/*
 * Destructor for ListOfCurveElements.
 */
ListOfCurveElements::~ListOfCurveElements()
{
}


/*
 * Get a RenderPoint from the ListOfCurveElements.
 */
RenderPoint*
ListOfCurveElements::get(unsigned int n)
{
  return static_cast<RenderPoint*>(ListOf::get(n));
}


/*
 * Get a RenderPoint from the ListOfCurveElements.
 */
const RenderPoint*
ListOfCurveElements::get(unsigned int n) const
{
  return static_cast<const RenderPoint*>(ListOf::get(n));
}


/*
 * Get a RenderPoint from the ListOfCurveElements based on its identifier.
 */
RenderPoint*
ListOfCurveElements::get(const std::string& sid)
{
  return const_cast<RenderPoint*>(static_cast<const
    ListOfCurveElements&>(*this).get(sid));
}


/*
 * Get a RenderPoint from the ListOfCurveElements based on its identifier.
 */
const RenderPoint*
ListOfCurveElements::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<RenderPoint>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const RenderPoint*>
    (*result);
}


/*
 * Removes the nth RenderPoint from this ListOfCurveElements and returns a
 * pointer to it.
 */
RenderPoint*
ListOfCurveElements::remove(unsigned int n)
{
  return static_cast<RenderPoint*>(ListOf::remove(n));
}


/*
 * Removes the RenderPoint from this ListOfCurveElements based on its
 * identifier and returns a pointer to it.
 */
RenderPoint*
ListOfCurveElements::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<RenderPoint>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <RenderPoint*> (item);
}


/*
 * Adds a copy of the given RenderPoint to this ListOfCurveElements.
 */
int
ListOfCurveElements::addRenderPoint(const RenderPoint* rp)
{
  if (rp == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (rp->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != rp->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != rp->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(rp)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(rp);
  }
}


/*
 * Get the number of RenderPoint objects in this ListOfCurveElements.
 */
unsigned int
ListOfCurveElements::getNumRenderPoints() const
{
  return size();
}


/*
 * Creates a new RenderCubicBezier object, adds it to this ListOfCurveElements
 * object and returns the RenderCubicBezier object created.
 */
RenderCubicBezier*
ListOfCurveElements::createCubicBezier()
{
  RenderCubicBezier* rcb = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    rcb = new RenderCubicBezier(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (rcb != NULL)
  {
    appendAndOwn(rcb);
  }

  return rcb;
}




/*
 * Returns the XML element name of this ListOfCurveElements object.
 */
const std::string&
ListOfCurveElements::getElementName() const
{
  static const string name = "listOfElements";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfCurveElements object.
 */
int
ListOfCurveElements::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfCurveElements object.
 */
int
ListOfCurveElements::getItemTypeCode() const
{
    return SBML_RENDER_LINESEGMENT;
}


/** @cond doxygenLibsbmlInternal */
bool ListOfCurveElements::isValidTypeForList(SBase * item)
{
  if (item == NULL) return false;
  int typeCode = item->getTypeCode();
  if (typeCode == SBML_RENDER_LINESEGMENT || 
      typeCode == SBML_RENDER_POINT ||
      typeCode == SBML_RENDER_CUBICBEZIER)
      return true;
  return false;
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfCurveElements object.
 *
 * @return the XMLNode with the XML representation for the 
 * ListOfCurveElements object.
 */
XMLNode ListOfCurveElements::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderPoint in this ListOfCurveElements
 */
SBase*
ListOfCurveElements::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

    if (name == "element")
    {
        std::string type = "RenderPoint";
        // find the correct type attribute
        int index=stream.peek().getAttributes().getIndex("type","http://www.w3.org/2001/XMLSchema-instance");
        if(index != -1)
        {
          type=stream.peek().getAttributes().getValue(index);
        }

        if(type=="RenderPoint")
        {
            object = new RenderPoint(renderns);
            if(object != NULL)
            {
              static_cast<RenderPoint*>(object)->setElementName("element");
            }
        }
        else if(type=="RenderCubicBezier")
        {
            object = new RenderCubicBezier(renderns);
            if(object != NULL)
            {
              static_cast<RenderCubicBezier*>(object)->setElementName("element");
            }
        }
        if(object) this->mItems.push_back(object);
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
ListOfCurveElements::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a RenderPoint_t from the ListOf_t.
 */
LIBSBML_EXTERN
RenderPoint_t*
ListOfCurveElements_getRenderPoint(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfCurveElements*>(lo)->get(n);
}


/*
 * Get a RenderPoint_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
RenderPoint_t*
ListOfCurveElements_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfCurveElements*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth RenderPoint_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
RenderPoint_t*
ListOfCurveElements_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfCurveElements*>(lo)->remove(n);
}


/*
 * Removes the RenderPoint_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
RenderPoint_t*
ListOfCurveElements_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfCurveElements*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


