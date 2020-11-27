/**
 * @file    GlobalStyle.cpp
 * @brief Implementation of the GlobalStyle class.
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

#include <sbml/packages/render/sbml/GlobalStyle.h>
#include <sbml/packages/render/sbml/ListOfGlobalStyles.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <assert.h>
#include <algorithm>

using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new GlobalStyle using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
GlobalStyle::GlobalStyle(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : Style(level, version, pkgVersion)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new GlobalStyle using the given RenderPkgNamespaces object.
 */
GlobalStyle::GlobalStyle(RenderPkgNamespaces *renderns)
  : Style(renderns)
{
  setElementNamespace(renderns->getURI());
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GlobalStyle object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * GlobalStyle object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the GlobalStyle
 * object to be instantiated.
 */
GlobalStyle::GlobalStyle(const XMLNode& node, unsigned int l2version)
  :Style(node, l2version)
{
}
/** @endcond */
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




/*
 * Copy constructor for GlobalStyle.
 */
GlobalStyle::GlobalStyle(const GlobalStyle& orig)
  : Style( orig )
{
}


/*
 * Assignment operator for GlobalStyle.
 */
GlobalStyle&
GlobalStyle::operator=(const GlobalStyle& rhs)
{
  if (&rhs != this)
  {
    Style::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this GlobalStyle object.
 */
GlobalStyle*
GlobalStyle::clone() const
{
  return new GlobalStyle(*this);
}


/*
 * Destructor for GlobalStyle.
 */
GlobalStyle::~GlobalStyle()
{
}


/*
 * Returns the XML element name of this GlobalStyle object.
 */
const std::string&
GlobalStyle::getElementName() const
{
  static const string name = "style";
  return name;
}


/*
 * Returns the libSBML type code for this GlobalStyle object.
 */
int
GlobalStyle::getTypeCode() const
{
  return SBML_RENDER_GLOBALSTYLE;
}


/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
GlobalStyle::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Style::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
GlobalStyle::readAttributes(const XMLAttributes& attributes,
                            const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfGlobalStyles*>(getParentSBMLObject())->size() < 2)
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
        log->logPackageError("render",
          RenderGlobalRenderInformationLOGlobalStylesAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  Style::readAttributes(attributes, expectedAttributes);

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
        log->logPackageError("render", RenderGlobalStyleAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }
}

/** @endcond */
#endif /* __cplusplus */


/*
 * Creates a new GlobalStyle_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
GlobalStyle_t *
GlobalStyle_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
{
  return new GlobalStyle(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this GlobalStyle_t object.
 */
LIBSBML_EXTERN
GlobalStyle_t*
GlobalStyle_clone(const GlobalStyle_t* gs)
{
  if (gs != NULL)
  {
    return static_cast<GlobalStyle_t*>(gs->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this GlobalStyle_t object.
 */
LIBSBML_EXTERN
void
GlobalStyle_free(GlobalStyle_t* gs)
{
  if (gs != NULL)
  {
    delete gs;
  }
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * GlobalStyle_t object have been set.
 */
LIBSBML_EXTERN
int
GlobalStyle_hasRequiredAttributes(const GlobalStyle_t * gs)
{
  return (gs != NULL) ? static_cast<int>(gs->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


