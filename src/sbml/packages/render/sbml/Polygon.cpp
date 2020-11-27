/**
 * @file    Polygon.cpp
 * @brief Implementation of the Polygon class.
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

#include <sbml/packages/render/sbml/Polygon.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <sbml/util/ElementFilter.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/render/sbml/RenderPoint.h>
#include <sbml/packages/render/sbml/RenderCubicBezier.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN


#include <sbml/xml/XMLInputStream.h>


#ifdef __cplusplus


/*
 * Creates a new Polygon using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
Polygon::Polygon(unsigned int level,
                 unsigned int version,
                 unsigned int pkgVersion)
  : GraphicalPrimitive2D(level, version, pkgVersion)
  , mRenderPoints (level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new Polygon using the given RenderPkgNamespaces object.
 */
Polygon::Polygon(RenderPkgNamespaces *renderns)
  : GraphicalPrimitive2D(renderns)
  , mRenderPoints (renderns)
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}



/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Polygon object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Polygon object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the Polygon
 * object to be instantiated.
 */
Polygon::Polygon(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive2D(node, l2version)
  , mRenderPoints(node, l2version)
{
   ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(node.getAttributes(), ea);
    const XMLNode* child;
    unsigned int n=0,nMax = node.getNumChildren();
    RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="listOfElements")
        {
            this->mRenderPoints=ListOfCurveElements(*child);
        }
        // keep to read old version
        else if(childName=="listOfCurveSegments")
        {
            // we have to read that in differently
            unsigned int i=0,iMax = child->getNumChildren();
            const XMLNode* child2;
            while(i<iMax)
            {
                child2=&child->getChild(i);
                const std::string& childName2=child2->getName();
                if(childName2=="curveSegment")
                {
                    const XMLAttributes& innerAttributes=child2->getAttributes();
                    int typeIndex=innerAttributes.getIndex("type");
                    if(typeIndex==-1 || innerAttributes.getURI(typeIndex)!="http://www.w3.org/2001/XMLSchema-instance")
                    {
                        // throw
                        ++i;
                        continue;
                    }
                    // read the elements of a cubic bezier
                    // if it is the first element, we need the start point,
                    // else we only need the two basepoints and the
                    // endpoint
                    unsigned int j,jMax=child2->getNumChildren();
                    bool startSet=false;
                    bool endSet=false;
                    RenderPoint start(renderns);
                    RenderPoint* end = new RenderPoint(renderns);
                    for(j=0;j<jMax;++j)
                    {
                        const XMLNode* child3=&child2->getChild(j);
                        std::string childName3=child3->getName();
                        if(childName3=="start")
                        {
                            start=RenderPoint(*child3);
                            startSet=true;

                        }
                        // add the basepoints and the endpoint
                        else if(childName3=="end")
                        {
                          delete end; 
                            end=new RenderPoint(*child3);
                            endSet=true;

                            if (jMax > 2)
                            {
                              RelAbsVector x = end->x();
                              RelAbsVector y = end->y();
                              RelAbsVector z = end->z();

                              delete end;
                              RenderCubicBezier* cend = new RenderCubicBezier(renderns);
                              cend->setX(x);
                              cend->setY(y);
                              cend->setZ(z);

                              RenderPoint* basePoint1 = new RenderPoint(child2->getChild("basePoint1"));
                              cend->setBasePoint1(basePoint1->x(), basePoint1->y(), basePoint1->z());
                              delete basePoint1;
                              
                              RenderPoint* basePoint2 = new RenderPoint(child2->getChild("basePoint2"));
                              cend->setBasePoint2(basePoint2->x(), basePoint2->y(), basePoint2->z());
                              delete basePoint2;

                              end = cend;



                            }

                        }
                    }
                    if(!startSet || !endSet)
                    {
                        // skip this point
                        // TODO this is an error
                        continue;
                    }
                    if(this->mRenderPoints.size()==0)
                    {
                        // add the start point
                        this->mRenderPoints.appendAndOwn(new RenderPoint(start));
                    }
                    // add the end point
                    this->mRenderPoints.appendAndOwn(end);                    

                }
                else if(childName2=="annotation")
                {
                    this->mRenderPoints.setAnnotation(new XMLNode(*child));
                }
                else if(childName2=="notes")
                {
                    this->mRenderPoints.setNotes(new XMLNode(*child));
                }
                ++i;
            }
        }
        ++n;
    }

    
  delete renderns;
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */




#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Instanciates a polygon with the given @p id and no elements.
 * All attributes inherited from GraphicalPrimitive are set as described
 * in the corresponding constructor of that class (@see GraphicalPrimitive2D)
 *
 * @param id id string for the polygon
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Polygon::Polygon(RenderPkgNamespaces* renderns, const std::string& id)
  : GraphicalPrimitive2D(renderns, id)
  , mRenderPoints(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Polygon::Polygon(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED


/*
 * Copy constructor for Polygon.
 */
Polygon::Polygon(const Polygon& orig)
  : GraphicalPrimitive2D( orig )
  , mRenderPoints ( orig.mRenderPoints )
{
  connectToChild();
}


/*
 * Assignment operator for Polygon.
 */
Polygon&
Polygon::operator=(const Polygon& rhs)
{
  if (&rhs != this)
  {
    GraphicalPrimitive2D::operator=(rhs);
    mRenderPoints = rhs.mRenderPoints;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Polygon object.
 */
Polygon*
Polygon::clone() const
{
  return new Polygon(*this);
}


/*
 * Destructor for Polygon.
 */
Polygon::~Polygon()
{
}


/*
 * Returns the ListOfCurveElements from this Polygon.
 */
const ListOfCurveElements*
Polygon::getListOfElements() const
{
  return &mRenderPoints;
}


/*
 * Returns the ListOfCurveElements from this Polygon.
 */
ListOfCurveElements*
Polygon::getListOfElements()
{
  return &mRenderPoints;
}


/*
 * Get a RenderPoint from the Polygon.
 */
RenderPoint*
Polygon::getElement(unsigned int n)
{
  return mRenderPoints.get(n);
}


/*
 * Get a RenderPoint from the Polygon.
 */
const RenderPoint*
Polygon::getElement(unsigned int n) const
{
  return mRenderPoints.get(n);
}


/*
 * Adds a copy of the given RenderPoint to this Polygon.
 */
int
Polygon::addElement(const RenderPoint* rp)
{
  if (rp == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (rp->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (rp->hasRequiredElements() == false)
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
    return mRenderPoints.append(rp);
  }
}


/*
 * Get the number of RenderPoint objects in this Polygon.
 */
unsigned int
Polygon::getNumElements() const
{
  return mRenderPoints.size();
}


/*
 * Creates a new RenderPoint object, adds it to this Polygon object and returns
 * the RenderPoint object created.
 */
RenderPoint*
Polygon::createPoint()
{
  RenderPoint* rp = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    rp = new RenderPoint(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (rp != NULL)
  {
    mRenderPoints.appendAndOwn(rp);
  }

  return rp;
}


/*
 * Creates a new RenderCubicBezier object, adds it to this Polygon object and
 * returns the RenderCubicBezier object created.
 */
RenderCubicBezier*
Polygon::createCubicBezier()
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
    mRenderPoints.appendAndOwn(rcb);
  }

  return rcb;
}


/*
 * Removes the nth RenderPoint from this Polygon and returns a pointer to it.
 */
RenderPoint*
Polygon::removeElement(unsigned int n)
{
  return mRenderPoints.remove(n);
}


RenderPoint*
Polygon::removeElement(const std::string& sid)
{
  return mRenderPoints.remove(sid);
}


/*
 * Returns the XML element name of this Polygon object.
 */
const std::string&
Polygon::getElementName() const
{
  static const string name = "polygon";
  return name;
}


/*
 * Returns the libSBML type code for this Polygon object.
 */
int
Polygon::getTypeCode() const
{
  return SBML_RENDER_POLYGON;
}

/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
Polygon::writeElements(XMLOutputStream& stream) const
{
  GraphicalPrimitive2D::writeElements(stream);

  if (getNumElements() > 0)
  {
    mRenderPoints.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Polygon::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mRenderPoints.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Polygon::setSBMLDocument(SBMLDocument* d)
{
  GraphicalPrimitive2D::setSBMLDocument(d);

  mRenderPoints.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
Polygon::connectToChild()
{
  GraphicalPrimitive2D::connectToChild();

  mRenderPoints.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Polygon::enablePackageInternal(const std::string& pkgURI,
                               const std::string& pkgPrefix,
                               bool flag)
{
  GraphicalPrimitive2D::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mRenderPoints.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this Polygon.
 */
SBase*
Polygon::createChildObject(const std::string& elementName)
{
  GraphicalPrimitive2D* obj = NULL;

  // TO DO

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this Polygon.
 */
int
Polygon::addChildObject(const std::string& elementName, const SBase* element)
{
  // TO DO

  return -1;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * Polygon.
 */
SBase*
Polygon::removeChildObject(const std::string& elementName,
                           const std::string& id)
{
  // TO DO

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this Polygon.
 */
unsigned int
Polygon::getNumObjects(const std::string& elementName)
{
  // TO DO

  return 0;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this Polygon.
 */
SBase*
Polygon::getObject(const std::string& elementName, unsigned int index)
{
  // TO DO

  return NULL;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
Polygon::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mRenderPoints.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
Polygon::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mRenderPoints.getMetaId() == metaid)
  {
    return &mRenderPoints;
  }

  obj = mRenderPoints.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
Polygon::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mRenderPoints, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this Polygon object.
 *
 * @return the XMLNode with the XML representation for the 
 * Polygon object.
 */
XMLNode Polygon::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
Polygon::createObject(XMLInputStream& stream)
{
  SBase* obj = GraphicalPrimitive2D::createObject(stream);

  const std::string& name = stream.peek().getName();

  if (name == "listOfElements")
  {
    if (mRenderPoints.size() != 0 && getErrorLog() != NULL)
    {
      getErrorLog()->logPackageError("render", RenderPolygonAllowedElements,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
    }

    obj = &mRenderPoints;
  }

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
Polygon::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive2D::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Polygon::readAttributes(const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  SBMLErrorLog* log = getErrorLog();

  GraphicalPrimitive2D::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderUnknown, pkgVersion, level,
          version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderPolygonAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
Polygon::writeAttributes(XMLOutputStream& stream) const
{
  GraphicalPrimitive2D::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new Polygon_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Polygon_t *
Polygon_create(unsigned int level,
               unsigned int version,
               unsigned int pkgVersion)
{
  return new Polygon(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Polygon_t object.
 */
LIBSBML_EXTERN
Polygon_t*
Polygon_clone(const Polygon_t* p)
{
  if (p != NULL)
  {
    return static_cast<Polygon_t*>(p->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Polygon_t object.
 */
LIBSBML_EXTERN
void
Polygon_free(Polygon_t* p)
{
  if (p != NULL)
  {
    delete p;
  }
}


/*
 * Returns a ListOf_t * containing RenderPoint_t objects from this Polygon_t.
 */
LIBSBML_EXTERN
ListOf_t*
Polygon_getListOfElements(Polygon_t* p)
{
  return (p != NULL) ? p->getListOfElements() : NULL;
}


/*
 * Get a RenderPoint_t from the Polygon_t.
 */
LIBSBML_EXTERN
RenderPoint_t*
Polygon_getElement(Polygon_t* p, unsigned int n)
{
  return (p != NULL) ? p->getElement(n) : NULL;
}


/*
 * Adds a copy of the given RenderPoint_t to this Polygon_t.
 */
LIBSBML_EXTERN
int
Polygon_addElement(Polygon_t* p, const RenderPoint_t* rp)
{
  return (p != NULL) ? p->addElement(rp) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of RenderPoint_t objects in this Polygon_t.
 */
LIBSBML_EXTERN
unsigned int
Polygon_getNumElements(Polygon_t* p)
{
  return (p != NULL) ? p->getNumElements() : SBML_INT_MAX;
}


/*
 * Creates a new RenderPoint_t object, adds it to this Polygon_t object and
 * returns the RenderPoint_t object created.
 */
LIBSBML_EXTERN
RenderPoint_t*
Polygon_createPoint(Polygon_t* p)
{
  return (p != NULL) ? p->createPoint() : NULL;
}


/*
 * Creates a new RenderCubicBezier_t object, adds it to this Polygon_t object
 * and returns the RenderCubicBezier_t object created.
 */
LIBSBML_EXTERN
RenderCubicBezier_t*
Polygon_createCubicBezier(Polygon_t* p)
{
  return (p != NULL) ? p->createCubicBezier() : NULL;
}


/*
 * Removes the nth RenderPoint_t from this Polygon_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
RenderPoint_t*
Polygon_removeElement(Polygon_t* p, unsigned int n)
{
  return (p != NULL) ? p->removeElement(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Polygon_t object have been set.
 */
LIBSBML_EXTERN
int
Polygon_hasRequiredAttributes(const Polygon_t * p)
{
  return (p != NULL) ? static_cast<int>(p->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * Polygon_t object have been set.
 */
LIBSBML_EXTERN
int
Polygon_hasRequiredElements(const Polygon_t * p)
{
  return (p != NULL) ? static_cast<int>(p->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


