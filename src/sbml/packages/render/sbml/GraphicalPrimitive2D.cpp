/**
 * @file    GraphicalPrimitive2D.cpp
 * @brief Implementation of the GraphicalPrimitive2D class.
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
#include <sbml/packages/render/sbml/GraphicalPrimitive2D.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#include <sbml/packages/render/sbml/Ellipse.h>
#include <sbml/packages/render/sbml/Rectangle.h>
#include <sbml/packages/render/sbml/Polygon.h>
#include <sbml/packages/render/sbml/RenderGroup.h>
#include <sbml/packages/render/sbml/ListOfDrawables.h>
#include <sbml/packages/render/sbml/LineEnding.h>
#include <sbml/packages/render/sbml/ListOfLineEndings.h>

#include <sbml/packages/render/extension/RenderExtension.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED



using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN

#ifdef __cplusplus





/*
 * Creates a new GraphicalPrimitive2D using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
GraphicalPrimitive2D::GraphicalPrimitive2D(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
  : GraphicalPrimitive1D(level, version, pkgVersion)
  , mFill ("")
  , mFillRule (FILL_RULE_UNSET)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GraphicalPrimitive2D using the given RenderPkgNamespaces
 * object.
 */
GraphicalPrimitive2D::GraphicalPrimitive2D(RenderPkgNamespaces *renderns)
  : GraphicalPrimitive1D(renderns)
  , mFill ("")
  , mFillRule (FILL_RULE_UNSET)
{
  setElementNamespace(renderns->getURI());
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GraphicalPrimitive2D object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * GraphicalPrimitive2D object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
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
    :GraphicalPrimitive1D(renderns, id)
  , mFill("")
  ,mFillRule(GraphicalPrimitive2D::UNSET)
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

/*
 * Copy constructor for GraphicalPrimitive2D.
 */
GraphicalPrimitive2D::GraphicalPrimitive2D(const GraphicalPrimitive2D& orig)
  : GraphicalPrimitive1D( orig )
  , mFill ( orig.mFill )
  , mFillRule ( orig.mFillRule )
{
}


/*
 * Assignment operator for GraphicalPrimitive2D.
 */
GraphicalPrimitive2D&
GraphicalPrimitive2D::operator=(const GraphicalPrimitive2D& rhs)
{
  if (&rhs != this)
  {
    GraphicalPrimitive1D::operator=(rhs);
    mFill = rhs.mFill;
    mFillRule = rhs.mFillRule;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this GraphicalPrimitive2D object.
 */
GraphicalPrimitive2D*
GraphicalPrimitive2D::clone() const
{
  return (GraphicalPrimitive2D*)(Transformation::clone());
}


/*
 * Destructor for GraphicalPrimitive2D.
 */
GraphicalPrimitive2D::~GraphicalPrimitive2D()
{
}


/*
 * Returns the value of the "fill" attribute of this GraphicalPrimitive2D.
 */
const std::string&
GraphicalPrimitive2D::getFill() const
{
  return mFill;
}


/*
 * Returns the fill color.
 *
 * @return this id of the fill color or the fill gradient or the fill color value string.
 */
const std::string& GraphicalPrimitive2D::getFillColor() const
{
    return this->mFill;
}


/*
 * Returns the value of the "fill-rule" attribute of this GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::getFillRule() const
{
  return mFillRule;
}


/*
 * Returns the value of the "fill-rule" attribute of this GraphicalPrimitive2D.
 */
std::string
GraphicalPrimitive2D::getFillRuleAsString() const
{
  std::string code_str = FillRule_toString((FillRule_t)(mFillRule));
  return code_str;
}


/*
 * Predicate returning @c true if this GraphicalPrimitive2D's "fill" attribute
 * is set.
 */
bool
GraphicalPrimitive2D::isSetFill() const
{
    return (!this->mFill.empty()) && (this->mFill != "none");
}


/*
 * Returns true if the fill attribute is set or false otherwise.
 * The fill attribute is considered set if the string is not empty.
 *
 * @return @c true if the fill color is set.
 */
bool GraphicalPrimitive2D::isSetFillColor() const
{
    return (!this->mFill.empty()) && (this->mFill != "none");
}


/*
 * Predicate returning @c true if this GraphicalPrimitive2D's "fill-rule"
 * attribute is set.
 */
bool
GraphicalPrimitive2D::isSetFillRule() const
{
  return (mFillRule != FILL_RULE_INVALID && mFillRule != FILL_RULE_UNSET);
}


/*
 * Sets the value of the "fill" attribute of this GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::setFill(const std::string& fill)
{
  mFill = fill;
  return LIBSBML_OPERATION_SUCCESS;
}


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


/*
 * Sets the value of the "fill-rule" attribute of this GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::setFillRule(const FillRule_t fillRule)
{
  if (FillRule_isValid(fillRule) == 0)
  {
    mFillRule = FILL_RULE_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mFillRule = fillRule;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


void GraphicalPrimitive2D::setFillRule(FILL_RULE rule)
{
  this->mFillRule = rule;
}


/*
 * Sets the value of the "fill-rule" attribute of this GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::setFillRule(const std::string& fillRule)
{
  mFillRule = FillRule_fromString(fillRule.c_str());

  if (mFillRule == FILL_RULE_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "fill" attribute of this GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::unsetFill()
{
  mFill.erase();

  if (mFill.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "fill-rule" attribute of this GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::unsetFillRule()
{
  mFillRule = FILL_RULE_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
 * type Ellipse
 */
bool
GraphicalPrimitive2D::isEllipse() const
{
  return dynamic_cast<const Ellipse*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
 * type Rectangle
 */
bool
GraphicalPrimitive2D::isRectangle() const
{
  return dynamic_cast<const Rectangle*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
 * type Polygon
 */
bool
GraphicalPrimitive2D::isPolygon() const
{
  return dynamic_cast<const Polygon*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
 * type RenderGroup
 */
bool
GraphicalPrimitive2D::isRenderGroup() const
{
  return dynamic_cast<const RenderGroup*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract GraphicalPrimitive2D is of
 * type LineEnding
 */
bool
GraphicalPrimitive2D::isLineEnding() const
{
  return dynamic_cast<const LineEnding*>(this) != NULL;
}


/*
 * Returns the XML element name of this GraphicalPrimitive2D object.
 */
const std::string&
GraphicalPrimitive2D::getElementName() const
{
  static const string name = "graphicalPrimitive2D";
  return name;
}


/*
 * Returns the libSBML type code for this GraphicalPrimitive2D object.
 */
int
GraphicalPrimitive2D::getTypeCode() const
{
  return SBML_RENDER_GRAPHICALPRIMITIVE2D;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * GraphicalPrimitive2D object have been set.
 */
bool
GraphicalPrimitive2D::hasRequiredAttributes() const
{
  bool allPresent = GraphicalPrimitive1D::hasRequiredAttributes();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
GraphicalPrimitive2D::writeElements(XMLOutputStream& stream) const
{
  GraphicalPrimitive1D::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
GraphicalPrimitive2D::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
GraphicalPrimitive2D::setSBMLDocument(SBMLDocument* d)
{
  GraphicalPrimitive1D::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
GraphicalPrimitive2D::enablePackageInternal(const std::string& pkgURI,
                                            const std::string& pkgPrefix,
                                            bool flag)
{
  GraphicalPrimitive1D::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::getAttribute(const std::string& attributeName,
                                   bool& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::getAttribute(const std::string& attributeName,
                                   int& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::getAttribute(const std::string& attributeName,
                                   double& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::getAttribute(const std::string& attributeName,
                                   unsigned int& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::getAttribute(const std::string& attributeName,
                                   std::string& value) const
{
  int return_value = GraphicalPrimitive1D::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "fill")
  {
    value = getFill();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "fill-rule")
  {
    value = getFillRuleAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this GraphicalPrimitive2D's attribute
 * "attributeName" is set.
 */
bool
GraphicalPrimitive2D::isSetAttribute(const std::string& attributeName) const
{
  bool value = GraphicalPrimitive1D::isSetAttribute(attributeName);

  if (attributeName == "fill")
  {
    value = isSetFill();
  }
  else if (attributeName == "fill-rule")
  {
    value = isSetFillRule();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::setAttribute(const std::string& attributeName,
                                   bool value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::setAttribute(const std::string& attributeName,
                                   int value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::setAttribute(const std::string& attributeName,
                                   double value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::setAttribute(const std::string& attributeName,
                                   unsigned int value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::setAttribute(const std::string& attributeName,
                                   const std::string& value)
{
  int return_value = GraphicalPrimitive1D::setAttribute(attributeName, value);

  if (attributeName == "fill")
  {
    return_value = setFill(value);
  }
  else if (attributeName == "fill-rule")
  {
    return_value = setFillRule(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * GraphicalPrimitive2D.
 */
int
GraphicalPrimitive2D::unsetAttribute(const std::string& attributeName)
{
  int value = GraphicalPrimitive1D::unsetAttribute(attributeName);

  if (attributeName == "fill")
  {
    value = unsetFill();
  }
  else if (attributeName == "fill-rule")
  {
    value = unsetFillRule();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
GraphicalPrimitive2D::createObject(XMLInputStream& stream)
{
  SBase* obj = GraphicalPrimitive1D::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
GraphicalPrimitive2D::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive1D::addExpectedAttributes(attributes);

  attributes.add("fill");

  attributes.add("fill-rule");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
GraphicalPrimitive2D::readAttributes(const XMLAttributes& attributes,
                                     const ExpectedAttributes&
                                       expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  GraphicalPrimitive1D::readAttributes(attributes, expectedAttributes);

  // 
  // fill string (use = "optional" )
  // 

  assigned = attributes.readInto("fill", mFill);

  if (assigned == true)
  {
    if (mFill.empty() == true && log)
    {
      logEmptyString(mFill, level, version, "<GraphicalPrimitive2D>");
    }
  }

  // 
  // fill-rule enum (use = "optional" )
  // 

  std::string fillRule;
  assigned = attributes.readInto("fill-rule", fillRule);

  if (assigned == true)
  {
    if (fillRule.empty() == true && log)
    {
      logEmptyString(fillRule, level, version, "<GraphicalPrimitive2D>");
    }
    else
    {
      mFillRule = FillRule_fromString(fillRule.c_str());

      if (FillRule_isValid((FillRule_t)(mFillRule)) == 0)
      {
        std::string msg = "The fill-rule on the <GraphicalPrimitive2D> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + fillRule + "', which is not a valid option.";
        if (log)
        {
          log->logPackageError("render",
            RenderGraphicalPrimitive2DFillRuleMustBeFillRuleEnum, pkgVersion,
            level, version, msg, getLine(), getColumn());
        }
      }
    }
  }
  else
  {
    mFillRule = FILL_RULE_UNSET;
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
GraphicalPrimitive2D::writeAttributes(XMLOutputStream& stream) const
{
  GraphicalPrimitive1D::writeAttributes(stream);

  if (isSetFill() == true)
  {
    stream.writeAttribute("fill", getPrefix(), mFill);
  }

  if (isSetFillRule() == true)
  {
    stream.writeAttribute("fill-rule", getPrefix(),
      FillRule_toString((FillRule_t)(mFillRule)));
  }

  SBase::writeExtensionAttributes(stream);
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
            // if the fill rule has been set explicitly,
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

#endif /* __cplusplus */


/*
* Creates a new Ellipse (GraphicalPrimitive2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createEllipse(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Ellipse(level, version, pkgVersion);
}


/*
* Creates a new Rectangle (GraphicalPrimitive2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createRectangle(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Rectangle(level, version, pkgVersion);
}


/*
* Creates a new Polygon (GraphicalPrimitive2D_t) using the given SBML Level, Version
* and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createPolygon(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new Polygon(level, version, pkgVersion);
}


/*
* Creates a new RenderGroup (GraphicalPrimitive2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createRenderGroup(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new RenderGroup(level, version, pkgVersion);
}


/*
* Creates a new LineEnding (GraphicalPrimitive2D_t) using the given SBML Level,
* Version and &ldquo;render&rdquo; package version.
*/
LIBSBML_EXTERN
GraphicalPrimitive2D_t *
GraphicalPrimitive2D_createLineEnding(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new LineEnding(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this GraphicalPrimitive2D_t object.
 */
LIBSBML_EXTERN
GraphicalPrimitive2D_t*
GraphicalPrimitive2D_clone(const GraphicalPrimitive2D_t* gpd)
{
  if (gpd != NULL)
  {
    return static_cast<GraphicalPrimitive2D_t*>(gpd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this GraphicalPrimitive2D_t object.
 */
LIBSBML_EXTERN
void
GraphicalPrimitive2D_free(GraphicalPrimitive2D_t* gpd)
{
  if (gpd != NULL)
  {
    delete gpd;
  }
}


/*
 * Returns the value of the "fill" attribute of this GraphicalPrimitive2D_t.
 */
LIBSBML_EXTERN
char *
GraphicalPrimitive2D_getFill(const GraphicalPrimitive2D_t * gpd)
{
  if (gpd == NULL)
  {
    return NULL;
  }

  return gpd->getFill().empty() ? NULL : safe_strdup(gpd->getFill().c_str());
}


/*
 * Returns the value of the "fill-rule" attribute of this
 * GraphicalPrimitive2D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_getFillRule(const GraphicalPrimitive2D_t * gpd)
{
  if (gpd == NULL)
  {
    return FILL_RULE_INVALID;
  }

  return gpd->getFillRule();
}


/*
 * Returns the value of the "fill-rule" attribute of this
 * GraphicalPrimitive2D_t.
 */
LIBSBML_EXTERN
char *
GraphicalPrimitive2D_getFillRuleAsString(const GraphicalPrimitive2D_t * gpd)
{
  return (char*)(FillRule_toString((FillRule_t)(gpd->getFillRule())));
}


/*
 * Predicate returning @c 1 (true) if this GraphicalPrimitive2D_t's "fill"
 * attribute is set.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isSetFill(const GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isSetFill()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this GraphicalPrimitive2D_t's "fill-rule"
 * attribute is set.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isSetFillRule(const GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isSetFillRule()) : 0;
}


/*
 * Sets the value of the "fill" attribute of this GraphicalPrimitive2D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_setFill(GraphicalPrimitive2D_t * gpd, const char * fill)
{
  return (gpd != NULL) ? gpd->setFill(fill) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "fill-rule" attribute of this GraphicalPrimitive2D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_setFillRule(GraphicalPrimitive2D_t * gpd,
                                 FillRule_t fillRule)
{
  return (gpd != NULL) ? gpd->setFillRule(fillRule) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "fill-rule" attribute of this GraphicalPrimitive2D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_setFillRuleAsString(GraphicalPrimitive2D_t * gpd,
                                         const char * fillRule)
{
  return (gpd != NULL) ? gpd->setFillRule(fillRule): LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "fill" attribute of this GraphicalPrimitive2D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_unsetFill(GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? gpd->unsetFill() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "fill-rule" attribute of this
 * GraphicalPrimitive2D_t.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_unsetFillRule(GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? gpd->unsetFillRule() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type Ellipse_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isEllipse(const GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isEllipse()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type
 * Rectangle_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isRectangle(const GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isRectangle()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type Polygon_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isPolygon(const GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isPolygon()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type
 * RenderGroup_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isRenderGroup(const GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isRenderGroup()) : 0;
}


/*
 * Predicate returning @c 1 if this GraphicalPrimitive2D_t is of type
 * LineEnding_t
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_isLineEnding(const GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->isLineEnding()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GraphicalPrimitive2D_t object have been set.
 */
LIBSBML_EXTERN
int
GraphicalPrimitive2D_hasRequiredAttributes(const GraphicalPrimitive2D_t * gpd)
{
  return (gpd != NULL) ? static_cast<int>(gpd->hasRequiredAttributes()) : 0;
}


LIBSBML_CPP_NAMESPACE_END 
