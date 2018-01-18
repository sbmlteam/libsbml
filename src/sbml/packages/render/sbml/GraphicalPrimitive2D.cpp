/**
 * @file    GraphicalPrimitive2D.cpp
 * @brief   abstract base class for graphical 2D objects
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

#include "GraphicalPrimitive2D.h"
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GraphicalPrimitive2D object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
GraphicalPrimitive2D::GraphicalPrimitive2D (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    GraphicalPrimitive1D(level,version,pkgVersion)
    ,mFillRule(GraphicalPrimitive2D::UNSET),mFill("")
{
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GraphicalPrimitive2D object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
GraphicalPrimitive2D::GraphicalPrimitive2D (RenderPkgNamespaces* renderns):
    GraphicalPrimitive1D(renderns)
    ,mFillRule(GraphicalPrimitive2D::UNSET),mFill("")
{
      // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GraphicalPrimitive2D object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * GraphicalPrimitive2D object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the GraphicalPrimitive2D
 * object to be instantiated.
 */
GraphicalPrimitive2D::GraphicalPrimitive2D(const XMLNode& node, unsigned int l2version)
  :GraphicalPrimitive1D(node, l2version)
{
   ExpectedAttributes ea;
   addExpectedAttributes(ea);
   this->readAttributes(node.getAttributes(), ea);

   
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */


/*
 * Destroy this object.
 */
GraphicalPrimitive2D::~GraphicalPrimitive2D ()
{
}



/** @cond doxygenLibsbmlInternal */
void
GraphicalPrimitive2D::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive1D::addExpectedAttributes(attributes);

  attributes.add("fill");
  attributes.add("fill-rule");
}
/** @endcond */

/*
 * Copy constructor.
 */
GraphicalPrimitive2D::GraphicalPrimitive2D(const GraphicalPrimitive2D& other) 
  : GraphicalPrimitive1D(other)
  , mFillRule(other.mFillRule)
  , mFill(other.mFill)
{

}

/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void GraphicalPrimitive2D::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
  GraphicalPrimitive1D::readAttributes(attributes, expectedAttributes);
    attributes.readInto("fill", this->mFill, getErrorLog(), false, getLine(), getColumn());
    std::string s;
    attributes.readInto("fill-rule", s, getErrorLog(), false, getLine(), getColumn());
    if(s=="evenodd")
    {
        this->mFillRule=GraphicalPrimitive2D::EVENODD;
    }
    else if(s=="inherit")
    {
        this->mFillRule=GraphicalPrimitive2D::INHERIT;
    }
    else if(s=="nonzero")
    {
        this->mFillRule=GraphicalPrimitive2D::NONZERO;
    }
    else
    {
        this->mFillRule=GraphicalPrimitive2D::UNSET;
    }
}
/** @endcond */




#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a GraphicalPrimitive2D.
 * The attributes inherited from GraphicalPrimitive1D are set as described
 * in the corresponding constructor for GraphicalPrimitive1D (@see GraphicalPrimitive1D).
 *
 * The fill and the fill rule are unset.
 * 
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
GraphicalPrimitive2D::GraphicalPrimitive2D(RenderPkgNamespaces* renderns, const std::string& id)
    :GraphicalPrimitive1D(renderns, id),mFillRule(GraphicalPrimitive2D::UNSET),mFill("")
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. GraphicalPrimitive2D::GraphicalPrimitive2D(const std::string& id) is deprecated." << std::endl;
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

/** @cond doxygenLibsbmlInternal */
/*
 * Set fill color to the id of a color definition, the id of a gradient
 * definition or a color value string.
 *
 * @param color the id of a color deifnition or a gradient or a color value string.
 */
void GraphicalPrimitive2D::setFillColor(const std::string& color)
{
    this->mFill=color;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the fill rule. Possible values are GraphicalPrimitive2D::NONZERO,
 * GraphicalPrimitive2D::EVENODD, GraphicalPrimitive::INHERIT or
 * GraphicalPrimitive2D::UNSET.
 *
 * If the fill rule for an object is unset, it default to INHERIT,
 * which means that it inherits the attribute from it's parent group.
 * The topmost group in an object hierarchy has a default value for this 
 * attribute which is GraphicalPrimitive2D::NONZERO.
 *
 * For more details please consult the render extension specification.
 *
 * @param rule the fill rule to be set.
 */
void GraphicalPrimitive2D::setFillRule(GraphicalPrimitive2D::FILL_RULE rule)
{
    this->mFillRule=rule;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the fill color.
 *
 * @return this id of the fill color or the fill gradient or the fill color value string.
 */
const std::string& GraphicalPrimitive2D::getFillColor() const
{
    return this->mFill;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the fill rule.
 *
 * @return this fill rule enum
 */
GraphicalPrimitive2D::FILL_RULE GraphicalPrimitive2D::getFillRule() const
{
    return this->mFillRule;
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
void GraphicalPrimitive2D::writeAttributes (XMLOutputStream& stream) const
{
    GraphicalPrimitive1D::writeAttributes(stream);
    if(this->isSetFillColor())
    {
        stream.writeAttribute("fill", getPrefix(), this->mFill);
    }
    switch(this->mFillRule)
    {
        case GraphicalPrimitive2D::EVENODD:
            stream.writeAttribute("fill-rule", getPrefix(), "evenodd");
            break;
        case GraphicalPrimitive2D::INHERIT:
            stream.writeAttribute("fill-rule", getPrefix(), "inherit");
            break;
        case GraphicalPrimitive2D::NONZERO:
            // if the fill rule has been set explicitely,
            // we have to write it because otherwise
            // it is assumed to be inherited.
            stream.writeAttribute("fill-rule", getPrefix(), "nonzero");
            break;
        case GraphicalPrimitive2D::UNSET:
        default:
            break;
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Adds all set attributes specific to the given GraphicalPrimitive2D objects to the given
 * XMLAttributes object.
 */
void GraphicalPrimitive2D::addGraphicalPrimitive2DAttributes(const GraphicalPrimitive2D& object,XMLAttributes& att)
{
    if(object.isSetFillColor())
    {
        att.add("fill",object.mFill);
    }
    switch(object.mFillRule)
    {
        case GraphicalPrimitive2D::EVENODD:
            att.add("fill-rule","evenodd");
            break;
        case GraphicalPrimitive2D::INHERIT:
            att.add("fill-rule","inherit");
            break;
        case GraphicalPrimitive2D::NONZERO:
            // if the fill rule has been set explicitely,
            // we have to write it because otherwise
            // it is assumed to be inherited.
            att.add("fill-rule","nonzero");
            break;
        default:
        case GraphicalPrimitive2D::UNSET:
            break;
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the fill attribute is set or false otherwise.
 * The fill attribute is considered set if the string is not empty.
 *
 * @return true is the fill color is set.
 */
bool GraphicalPrimitive2D::isSetFillColor() const
{
    return (!this->mFill.empty()) && (this->mFill != "none");
}
/** @endcond */


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the fill attribute is set or false otherwise.
 * The fill attribute is considered set if the string is not empty.
 *
 * This function is deprecated, please use isSetFillColor instead.
 *
 * @return true is the fill color is set.
 */
bool GraphicalPrimitive2D::isSetFill() const
{
    return this->isSetFillColor();
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. GraphicalPrimitive2D::isSetFill() is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
}
/** @endcond */
#endif // OMIT_DEPRECATED

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the fill rule attribute is set or false otherwise.
 * The fill rule is considered as set if it is not GraphicalPrimitive2D::UNSET.
 * 
 * @return true is the fill color is set.
 */
bool GraphicalPrimitive2D::isSetFillRule() const
{
    return this->mFillRule!=GraphicalPrimitive2D::UNSET;
}
/** @endcond */


const char* FILL_RULE_STRINGS[] =
{
  "unset",
  "nonzero",
  "evenodd", 
  "inherit", 
  "invalid"
};



LIBSBML_EXTERN
GraphicalPrimitive2D::FILL_RULE
FillRule_fromString(const char* name)
{
  if (name != NULL)
  {
    const GraphicalPrimitive2D::FILL_RULE  lo = GraphicalPrimitive2D::UNSET;
    const GraphicalPrimitive2D::FILL_RULE  hi = GraphicalPrimitive2D::INHERIT;

    return (GraphicalPrimitive2D::FILL_RULE)util_bsearchStringsI(FILL_RULE_STRINGS, name, lo, hi);
  }

  return GraphicalPrimitive2D::UNSET;

}

LIBSBML_EXTERN
const char*
FillRule_toString(GraphicalPrimitive2D::FILL_RULE rule)
{
  if ((rule < GraphicalPrimitive2D::UNSET) || (rule > GraphicalPrimitive2D::INHERIT))
  {
    rule = GraphicalPrimitive2D::INVALID;
  }

  return FILL_RULE_STRINGS[rule];
}


LIBSBML_CPP_NAMESPACE_END 
