/**
 * @file    GraphicalPrimitive1D.cpp
 * @brief   abstract base class for graphical 1D objects
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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

#include "GraphicalPrimitive1D.h"

#include <limits>
#include <sstream>
#include <string.h> 
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GraphicalPrimitive1D object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
GraphicalPrimitive1D::GraphicalPrimitive1D (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    Transformation2D(level,version,pkgVersion)
////    ,mId("")
    ,mStroke("")
    ,mStrokeWidth(std::numeric_limits<double>::quiet_NaN())
{
  
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GraphicalPrimitive1D object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
GraphicalPrimitive1D::GraphicalPrimitive1D (RenderPkgNamespaces* renderns):
    Transformation2D(renderns)
////    ,mId("")
    ,mStroke("")
    ,mStrokeWidth(std::numeric_limits<double>::quiet_NaN())
{
      // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */

/*
 * Copy constructor.
 */
GraphicalPrimitive1D::GraphicalPrimitive1D(const GraphicalPrimitive1D& other)
  : Transformation2D(other)
//  , mId(other.mId)
  , mStroke(other.mStroke)
  , mStrokeWidth(other.mStrokeWidth)
{
  setId(other.mId);
}


/*
 * Destroy this object.
 */
GraphicalPrimitive1D::~GraphicalPrimitive1D ()
{
}



/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GraphicalPrimitive1D object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * GraphicalPrimitive1D object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the GraphicalPrimitive1D
 * object to be instantiated.
 */
GraphicalPrimitive1D::GraphicalPrimitive1D(const XMLNode& node, unsigned int l2version)
  :Transformation2D(node, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;    
    
    ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes,ea);
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
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
GraphicalPrimitive1D::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Transformation2D::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("stroke");
  attributes.add("stroke-width");
  attributes.add("stroke-dasharray");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void GraphicalPrimitive1D::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
  Transformation2D::readAttributes(attributes, expectedAttributes);
    attributes.readInto("id", mId, getErrorLog(), false,getLine(), getColumn());
    attributes.readInto("stroke", this->mStroke, getErrorLog(), false, getLine(), getColumn());
    std::string s;
    attributes.readInto("stroke-width", s, getErrorLog(), false, getLine(), getColumn());
    if(s!="")
    {
        this->mStrokeWidth=strtod(s.c_str(),NULL);
    }
    else
    {
        this->mStrokeWidth=std::numeric_limits<double>::quiet_NaN();
    }
    // parse the stroke-dasharray
    if(attributes.readInto("stroke-dasharray", s, getErrorLog(), false, getLine(), getColumn()) && !s.empty())
    {
        this->setDashArray(s);
    }
}
/** @endcond */



#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a GraphicalPrimitive1D.
 * The transformation properties are not set, neither is the stroke or the stroke width.
 * The id is set to the given string.
 *
 * @param id The id for the GraphicalPrimitive1D object
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
GraphicalPrimitive1D::GraphicalPrimitive1D(RenderPkgNamespaces* renderns, const std::string& id):
    Transformation2D(renderns)
//    ,mId(id)
    ,mStroke("")
    ,mStrokeWidth(std::numeric_limits<double>::quiet_NaN())
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. GraphicalPrimitive1D::GraphicalPrimitive1D(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
  setId(id);

        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the stroke color to the given color definition id or color value string.
 * (@see ColorDefinition)
 *
 * @param stroke id of a ColorDefinition object or a valid color value string.
 */
void GraphicalPrimitive1D::setStroke(const std::string& id)
{
    this->mStroke=id;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the stroke width.
 *
 * @param width New width for strokes. Should be a positive value.
 */
void GraphicalPrimitive1D::setStrokeWidth(double width)
{
    this->mStrokeWidth=(width>0 || width!=width)?width:0.0;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the dasharray to the values in the given array.
 *
 * @param array Array of alternating stroke and gap length values.
 */
void GraphicalPrimitive1D::setDashArray(const std::vector<unsigned int>& array)
{
    this->mStrokeDashArray=array;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the stroke color.
 *
 * @return the id of the color definition or a color value string.
 */
const std::string& GraphicalPrimitive1D::getStroke() const
{
    return this->mStroke;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the stroke width.
 *
 * @return the stroke width
 */
double GraphicalPrimitive1D::getStrokeWidth() const
{
    return this->mStrokeWidth;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const reference to the stroke dasharray.
 *
 * @return const reference to stroke dash array
 */
const std::vector<unsigned int>& GraphicalPrimitive1D::getDashArray() const
{
    return this->mStrokeDashArray;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a reference to the stroke dasharray.
 *
 * @return reference to stroke dash array
 */
std::vector<unsigned int>& GraphicalPrimitive1D::getDashArray()
{
    return this->mStrokeDashArray;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true is the stroke width has been set or false otherwise.
 * The stroke width is considered set if it is not NaN.
 *
 * @return true is the stroke width is set.
 */
bool GraphicalPrimitive1D::isSetStrokeWidth() const
{
    return (mStrokeWidth==mStrokeWidth);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true is the stroke has been set or false otherwise.
 * The stroke color is considered set if the string is not empty.
 *
 * @return true if the stroke color is set.
 */
bool GraphicalPrimitive1D::isSetStroke() const
{
    return (!this->mStroke.empty()) && (this->mStroke != "none");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true is the dash array has been set or false otherwise.
 * The array is considered set if it is not empty and if the first entry is
 * not NaN.
 *
 * @true if the dasharray is set.
 */
bool GraphicalPrimitive1D::isSetDashArray() const
{
    bool result=true;
    if(mStrokeDashArray.empty() || mStrokeDashArray[0]!=mStrokeDashArray[0])
    {
        result=false;
    }
    return result;
}


/* 
 * Returns the number of defined dashes.
 */
unsigned int 
GraphicalPrimitive1D::getNumDashes() const
{
  return (unsigned int)mStrokeDashArray.size();
}

/*
 * Returns the dash at the given index. 
 */
unsigned int 
GraphicalPrimitive1D::getDashByIndex(unsigned int index) const
{
  if (index >= getNumDashes())
    return -1;
  return mStrokeDashArray[index];
}

/*
 * Adds a dash at the end of the current list
 */
void 
GraphicalPrimitive1D::addDash(unsigned int dash)
{
  mStrokeDashArray.push_back(dash);
}

/*
 * Clears all defined dashes.
 */
void 
GraphicalPrimitive1D::clearDashes()
{
  mStrokeDashArray.clear();
}

/* 
 * Sets the dash at the given position.
 */
void 
GraphicalPrimitive1D::setDashByIndex(unsigned int index, unsigned int dash)
{
  if (index >= getNumDashes()) return;
  mStrokeDashArray[index]=dash;
}

/*
 * Removes the dash at the given index
 */
void 
GraphicalPrimitive1D::removeDash(unsigned int index)
{
  if (index >= getNumDashes()) return;
  mStrokeDashArray.erase(mStrokeDashArray.begin() + index);
}

/* 
 * Inserts the dash at the given position.
 */
void 
GraphicalPrimitive1D::insertDash(unsigned int index, unsigned int dash)
{
  if (index >= getNumDashes()) return;
  mStrokeDashArray.insert(mStrokeDashArray.begin() + index, dash);
}


/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.  For example:
 *
 *   SBase::writeAttributes(stream);
 *   stream.writeAttribute( "id"  , mId   );
 *   stream.writeAttribute( "name", mName );
 *   ...
 */
void GraphicalPrimitive1D::writeAttributes (XMLOutputStream& stream) const
{
    Transformation2D::writeAttributes(stream);
    if(this->isSetId())
    {
        stream.writeAttribute("id", getPrefix(), this->getId());
    }
    // stroke, stroke_width
    if(this->isSetStroke())
    {
        stream.writeAttribute("stroke", getPrefix(), this->getStroke());
    }
    if(this->isSetStrokeWidth())
    {
        std::ostringstream os;
        os << this->getStrokeWidth();
        stream.writeAttribute("stroke-width", getPrefix(), os.str());
    }
    if(this->isSetDashArray())
    {
        std::ostringstream os;
        std::vector<unsigned int>::const_iterator it=this->getDashArray().begin();
        std::vector<unsigned int>::const_iterator endit=this->getDashArray().end();
        os << *it;
        ++it;
        while(it!=endit)
        {
            os << " , " << *it;
            ++it;
        }
        stream.writeAttribute("stroke-dasharray", getPrefix(), os.str());
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Adds all set attributes specific to the given GraphicalPrimitive1D objects to the given
 * XMLAttributes object.
 */
void GraphicalPrimitive1D::addGraphicalPrimitive1DAttributes(const GraphicalPrimitive1D& primitive,XMLAttributes& attributes) 
{
    if(primitive.isSetId())
    {
        attributes.add("id",primitive.getId());
    }
    // stroke, stroke_width
    if(primitive.isSetStroke())
    {
        attributes.add("stroke",primitive.getStroke());
    }
    if(primitive.isSetStrokeWidth())
    {
        std::ostringstream os;
        os << primitive.getStrokeWidth();
        attributes.add("stroke-width",os.str());
    }
    if(primitive.isSetDashArray())
    {
        std::ostringstream os;
        std::vector<unsigned int>::const_iterator it=primitive.getDashArray().begin();
        std::vector<unsigned int>::const_iterator endit=primitive.getDashArray().end();
        os << *it;
        ++it;
        while(it!=endit)
        {
            os << " , " << *it;
            ++it;
        }
        attributes.add("stroke-dasharray",os.str());
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 *  This method parses a dasharray string into the given vector.
 *  The vector is first cleared.
 *  If the dasharray is invalid, false is returned.
 */
bool GraphicalPrimitive1D::parseDashArray(const std::string& s,std::vector<unsigned int>& array)
{
    array.clear();
    bool result=true;
    if(!s.empty())
    {
        std::istringstream is(s);
        size_t size=s.size()+1;
        char* tmp=new char[size];
        char* tmp2=tmp;
        char* tmp3;
        char** endptr;
        long value;
        is.getline(tmp2,size,',');
        // continue until the string is empty or the stream is
        // no longer usable
        while(tmp2[0] != 0 && is.good())
        {
            endptr=&tmp2;
            tmp3=tmp2;
            value=strtol(tmp2,endptr,10);
            if(value < 0 || *endptr == tmp3 || ( **endptr != '\0' && **endptr != ' ' && **endptr != '\t'))
            {
                result=false;
                array.clear();
            }
            else
            {
                array.push_back((unsigned int)value);
            } 
            // read next element 
            is.getline(tmp2,size,',');
        }
        // check if the string was parsed to the end
        if(is.eof())
        {
            // we have to parse the current tmp2 
            // and check if that was ok
            endptr=&tmp2;
            tmp3=tmp2;
            value=strtol(tmp2,endptr,10);
            if(value < 0 || *endptr == tmp3 || ( **endptr != '\0' && **endptr != ' ' && **endptr != '\t'))
            {
                result=false;
                array.clear();
            }
            else
            {
                array.push_back((unsigned int)value);
            }  
        }
        else
        {
            // since we did not parse to the end, 
            // there most have been an error
            result=false;
            array.clear(); 
        } 
        delete[] tmp;
    }
    return result;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the dasharray from the given string.
 * If the string is not a valid dasharray string, false
 * is returned and the dasharray remains in the state is was
 * before the call.
 *
 * The individual numerical values in the string have to be separated by kommas.
 *
 * @param arrayString a string with number representing a dash array.
 *
 * @return true is setting the dasharray from the string succeed or false otherwise.
 */
bool GraphicalPrimitive1D::setDashArray(const std::string& arrayString)
{
    std::vector<unsigned int> array;
    bool result=false;
    if(this->parseDashArray(arrayString,array)==true)
    {
        this->mStrokeDashArray=array;
        result=true;
    }
    return result;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the value of the "id" attribute of this GraphicalPrimitive.
 *
 * @return the id of the GraphicalPrimitive
 */
const std::string& GraphicalPrimitive1D::getId () const
{
    return mId;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Predicate returning @c true or @c false depending on whether this
 * GraphicalPrimitive's "id" attribute has been set.
 *
 * @return returns true or false depending on whether the id on the 
 * GraphicalPrimitive has been set.
 */
bool GraphicalPrimitive1D::isSetId () const
{
    return (mId.empty() == false);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the value of the "id" attribute of this GraphicalPrimitive.
 *
 * @param id the new id for the GraphicalPrimitive 
 *
 * @return status if the operation succeeded
 */
int GraphicalPrimitive1D::setId (const std::string& id)
{
    if (!(SyntaxChecker::isValidSBMLSId(id)))
    {
        return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
        mId = id;
        return LIBSBML_OPERATION_SUCCESS;
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Unsets the value of the "id" attribute of this GraphicalPrimitive.
 */
int GraphicalPrimitive1D::unsetId ()
{
    mId.erase();
    if (mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END 


