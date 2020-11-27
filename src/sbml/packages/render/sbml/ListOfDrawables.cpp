/**
 * @file ListOfDrawables.cpp
 * @brief Implementation of the ListOfDrawables class.
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
#include <sbml/packages/render/sbml/ListOfDrawables.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#include <sbml/packages/render/sbml/Image.h>
#include <sbml/packages/render/sbml/Ellipse.h>
#include <sbml/packages/render/sbml/Rectangle.h>
#include <sbml/packages/render/sbml/Polygon.h>
#include <sbml/packages/render/sbml/RenderGroup.h>
#include <sbml/packages/render/sbml/LineEnding.h>
#include <sbml/packages/render/sbml/ListOfLineEndings.h>
#include <sbml/packages/render/sbml/Text.h>
#include <sbml/packages/render/sbml/RenderCurve.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfDrawables using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
ListOfDrawables::ListOfDrawables(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfDrawables using the given RenderPkgNamespaces object.
 */
ListOfDrawables::ListOfDrawables(RenderPkgNamespaces *renderns)
  : ListOf(renderns)
{
  setElementNamespace(renderns->getURI());
}


/*
 * Copy constructor for ListOfDrawables.
 */
ListOfDrawables::ListOfDrawables(const ListOfDrawables& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfDrawables.
 */
ListOfDrawables&
ListOfDrawables::operator=(const ListOfDrawables& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfDrawables object.
 */
ListOfDrawables*
ListOfDrawables::clone() const
{
  return new ListOfDrawables(*this);
}


/*
 * Destructor for ListOfDrawables.
 */
ListOfDrawables::~ListOfDrawables()
{
}


/*
 * Get a Transformation2D from the ListOfDrawables.
 */
Transformation2D*
ListOfDrawables::get(unsigned int n)
{
  return static_cast<Transformation2D*>(ListOf::get(n));
}


/*
 * Get a Transformation2D from the ListOfDrawables.
 */
const Transformation2D*
ListOfDrawables::get(unsigned int n) const
{
  return static_cast<const Transformation2D*>(ListOf::get(n));
}


/*
 * Get a Transformation2D from the ListOfDrawables based on its identifier.
 */
Transformation2D*
ListOfDrawables::get(const std::string& sid)
{
  return const_cast<Transformation2D*>(static_cast<const
    ListOfDrawables&>(*this).get(sid));
}


/*
 * Get a Transformation2D from the ListOfDrawables based on its identifier.
 */
const Transformation2D*
ListOfDrawables::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<Transformation2D>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Transformation2D*>
    (*result);
}


/*
 * Removes the nth Transformation2D from this ListOfDrawables and returns a
 * pointer to it.
 */
Transformation2D*
ListOfDrawables::remove(unsigned int n)
{
  return static_cast<Transformation2D*>(ListOf::remove(n));
}


/*
 * Removes the Transformation2D from this ListOfDrawables based on its
 * identifier and returns a pointer to it.
 */
Transformation2D*
ListOfDrawables::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<Transformation2D>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Transformation2D*> (item);
}


/*
 * Adds a copy of the given Transformation2D to this ListOfDrawables.
 */
int
ListOfDrawables::addTransformation2D(const Transformation2D* td)
{
  if (td == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (td->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != td->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != td->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(td)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(td);
  }
}


/*
 * Get the number of Transformation2D objects in this ListOfDrawables.
 */
unsigned int
ListOfDrawables::getNumTransformation2Ds() const
{
  return size();
}


/*
 * Creates a new Image object, adds it to this ListOfDrawables object and
 * returns the Image object created.
 */
Image*
ListOfDrawables::createImage()
{
  Image* i = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    i = new Image(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (i != NULL)
  {
    appendAndOwn(i);
  }

  return i;
}


/*
 * Creates a new Ellipse object, adds it to this ListOfDrawables object and
 * returns the Ellipse object created.
 */
Ellipse*
ListOfDrawables::createEllipse()
{
  Ellipse* e = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    e = new Ellipse(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (e != NULL)
  {
    appendAndOwn(e);
  }

  return e;
}


/*
 * Creates a new Rectangle object, adds it to this ListOfDrawables object and
 * returns the Rectangle object created.
 */
Rectangle*
ListOfDrawables::createRectangle()
{
  Rectangle* r = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    r = new Rectangle(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (r != NULL)
  {
    appendAndOwn(r);
  }

  return r;
}


/*
 * Creates a new Polygon object, adds it to this ListOfDrawables object and
 * returns the Polygon object created.
 */
Polygon*
ListOfDrawables::createPolygon()
{
  Polygon* p = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    p = new Polygon(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (p != NULL)
  {
    appendAndOwn(p);
  }

  return p;
}


/*
 * Creates a new RenderGroup object, adds it to this ListOfDrawables object and
 * returns the RenderGroup object created.
 */
RenderGroup*
ListOfDrawables::createGroup()
{
  RenderGroup* rg = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    rg = new RenderGroup(renderns);
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
 * Creates a new LineEnding object, adds it to this ListOfDrawables object and
 * returns the LineEnding object created.
 */
LineEnding*
ListOfDrawables::createLineEnding()
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
 * Creates a new Text object, adds it to this ListOfDrawables object and
 * returns the Text object created.
 */
Text*
ListOfDrawables::createText()
{
  Text* t = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    t = new Text(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (t != NULL)
  {
    appendAndOwn(t);
  }

  return t;
}


/*
 * Creates a new RenderCurve object, adds it to this ListOfDrawables object and
 * returns the RenderCurve object created.
 */
RenderCurve*
ListOfDrawables::createCurve()
{
  RenderCurve* rc = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    rc = new RenderCurve(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (rc != NULL)
  {
    appendAndOwn(rc);
  }

  return rc;
}


/*
 * Returns the XML element name of this ListOfDrawables object.
 */
const std::string&
ListOfDrawables::getElementName() const
{
  static const string name = "listOfDrawables";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfDrawables object.
 */
int
ListOfDrawables::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfDrawables object.
 */
int
ListOfDrawables::getItemTypeCode() const
{
  return SBML_RENDER_TRANSFORMATION2D;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Transformation2D in this ListOfDrawables
 */
SBase*
ListOfDrawables::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  RENDER_CREATE_NS(renderns, getSBMLNamespaces());

  //if (name == "transformation2D")
  //{
  //  object = new Transformation2D(renderns);
  //  appendAndOwn(object);
  //}

    if (name == "g")
    {
       object = new RenderGroup(renderns);
    }

    if(name=="curve")
    {
       // only newstyle cures will be supported for L3 
       object = new RenderCurve(renderns);
    }

  if (name == "image")
  {
    object = new Image(renderns);
    appendAndOwn(object);
  }

  if (name == "ellipse")
  {
    object = new Ellipse(renderns);
    appendAndOwn(object);
  }

  if (name == "rectangle")
  {
    object = new Rectangle(renderns);
    appendAndOwn(object);
  }

  if (name == "polygon")
  {
    object = new Polygon(renderns);
    appendAndOwn(object);
  }

  if (name == "renderGroup")
  {
    object = new RenderGroup(renderns);
    appendAndOwn(object);
  }

  if (name == "lineEnding")
  {
    object = new LineEnding(renderns);
    appendAndOwn(object);
  }

  if (name == "text")
  {
    object = new Text(renderns);
    appendAndOwn(object);
  }

  if (name == "renderCurve")
  {
    object = new RenderCurve(renderns);
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
ListOfDrawables::writeXMLNS(XMLOutputStream& stream) const
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
ListOfDrawables::isValidTypeForList(SBase* item)
{
  unsigned int tc = item->getTypeCode();

  return ((tc == SBML_RENDER_IMAGE) || (tc == SBML_RENDER_ELLIPSE) || (tc ==
    SBML_RENDER_RECTANGLE) || (tc == SBML_RENDER_POLYGON) || (tc ==
      SBML_RENDER_GROUP) || (tc == SBML_RENDER_LINEENDING) || (tc ==
        SBML_RENDER_TEXT) || (tc == SBML_RENDER_CURVE));
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get a Transformation2D_t from the ListOf_t.
 */
LIBSBML_EXTERN
Transformation2D_t*
ListOfDrawables_getTransformation2D(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDrawables*>(lo)->get(n);
}


/*
 * Get a Transformation2D_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
Transformation2D_t*
ListOfDrawables_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDrawables*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth Transformation2D_t from this ListOf_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
Transformation2D_t*
ListOfDrawables_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDrawables*>(lo)->remove(n);
}


/*
 * Removes the Transformation2D_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
Transformation2D_t*
ListOfDrawables_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDrawables*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


