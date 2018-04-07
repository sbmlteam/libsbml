/**
 * @file    GlobalStyle.cpp
 * @brief   class representing a global style object
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

#include "GlobalStyle.h"
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <assert.h>
#include <algorithm>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string GlobalStyle::ELEMENT_NAME="style";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GlobalStyle object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
GlobalStyle::GlobalStyle (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    Style(level,version,pkgVersion)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GlobalStyle object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
GlobalStyle::GlobalStyle (RenderPkgNamespaces* renderns):
    Style(renderns)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
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
 * Creates a new GlobalStyle object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * GlobalStyle object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the GlobalStyle
 * object to be instantiated.
 */
GlobalStyle::GlobalStyle(const XMLNode& node, unsigned int l2version)
  :Style(node, l2version)
{
}
/** @endcond */


/*
 * Destroy this object.
 */
GlobalStyle::~GlobalStyle ()
{
}


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a GlobalStyle with the given @p id
 * and all lists empty.
 *
 * @param id the new id for the GlobalStyle.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
GlobalStyle::GlobalStyle(RenderPkgNamespaces* renderns, const std::string& id):Style(renderns,id)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. GlobalStyle::GlobalStyle(const std::string& id) is deprecated." << std::endl;
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
 * Creates and returns a deep copy of this GlobalStyle object.
 *
 * @return a (deep) copy of this GlobalStyle.
 */
GlobalStyle* GlobalStyle::clone() const
{
    return new GlobalStyle(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * GlobalStyle, is always @c "renderInformation".
 * 
 * @return the name of this element, i.e., @c "renderInformation".
 */
const std::string& GlobalStyle::getElementName() const
{
  static std::string name = GlobalStyle::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the libSBML type code for this %SBML object.
 * 
 * @if clike LibSBML attaches an identifying code to every
 * kind of SBML object.  These are known as <em>SBML type codes</em>.
 * The set of possible type codes is defined in the enumeration
 * #SBMLTypeCode_t.  The names of the type codes all begin with the
 * characters @c SBML_. @endif@if java LibSBML attaches an
 * identifying code to every kind of SBML object.  These are known as
 * <em>SBML type codes</em>.  In other languages, the set of type codes
 * is stored in an enumeration; in the Java language interface for
 * libSBML, the type codes are defined as static integer constants in
 * interface class {@link libsbmlConstants}.  The names of the type codes
 * all begin with the characters @c SBML_. @endif
 *
 * @return the SBML type code for this object, or @c SBML_UNKNOWN (default).
 *
 * This method is purely abstract and has to be implemented by derived
 * classes.
 *
 * @see getElementName()
 */
int GlobalStyle::getTypeCode() const
{
    return SBML_RENDER_GLOBALSTYLE;
}
/** @endcond */



LIBSBML_CPP_NAMESPACE_END 
