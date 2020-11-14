/**
 * @file:   BindingSiteSpeciesType.cpp
 * @brief:  Implementation of the BindingSiteSpeciesType class
 * @author: SBMLTeam
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
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/packages/multi/sbml/BindingSiteSpeciesType.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new BindingSiteSpeciesType with the given level, version, and package version.
 */
BindingSiteSpeciesType::BindingSiteSpeciesType (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : MultiSpeciesType(level, version)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new BindingSiteSpeciesType with the given MultiPkgNamespaces object.
 */
BindingSiteSpeciesType::BindingSiteSpeciesType (MultiPkgNamespaces* multins)
  : MultiSpeciesType(multins)
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for BindingSiteSpeciesType.
 */
BindingSiteSpeciesType::BindingSiteSpeciesType (const BindingSiteSpeciesType& orig)
  : MultiSpeciesType(orig)
{
}


/*
 * Assignment for BindingSiteSpeciesType.
 */
BindingSiteSpeciesType&
BindingSiteSpeciesType::operator=(const BindingSiteSpeciesType& rhs)
{
  if (&rhs != this)
  {
    MultiSpeciesType::operator=(rhs);
  }
  return *this;
}


/*
 * Clone for BindingSiteSpeciesType.
 */
BindingSiteSpeciesType*
BindingSiteSpeciesType::clone () const
{
  return new BindingSiteSpeciesType(*this);
}


/*
 * Destructor for BindingSiteSpeciesType.
 */
BindingSiteSpeciesType::~BindingSiteSpeciesType ()
{
}


/*
 * Returns the XML element name of this object
 */
const std::string&
BindingSiteSpeciesType::getElementName () const
{
  static const string name = "bindingSiteSpeciesType";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
BindingSiteSpeciesType::getTypeCode () const
{
  return SBML_MULTI_BINDING_SITE_SPECIES_TYPE;
}


/*
 * check if all the required attributes are set
 */
bool
BindingSiteSpeciesType::hasRequiredAttributes () const
{
  bool allPresent = MultiSpeciesType::hasRequiredAttributes();

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
BindingSiteSpeciesType::writeElements (XMLOutputStream& stream) const
{
  MultiSpeciesType::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
BindingSiteSpeciesType::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
BindingSiteSpeciesType::setSBMLDocument (SBMLDocument* d)
{
  MultiSpeciesType::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
BindingSiteSpeciesType::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  MultiSpeciesType::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
BindingSiteSpeciesType::createObject(XMLInputStream& stream)
{
  SBase* object = MultiSpeciesType::createObject(stream);

  connectToChild();


  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
BindingSiteSpeciesType::addExpectedAttributes(ExpectedAttributes& attributes)
{
  MultiSpeciesType::addExpectedAttributes(attributes);

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
BindingSiteSpeciesType::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  MultiSpeciesType::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
BindingSiteSpeciesType::writeAttributes (XMLOutputStream& stream) const
{
  MultiSpeciesType::writeAttributes(stream);

}


  /** @endcond */


LIBSBML_EXTERN
BindingSiteSpeciesType_t *
BindingSiteSpeciesType_create(unsigned int level, unsigned int version,
                              unsigned int pkgVersion)
{
  return new BindingSiteSpeciesType(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
BindingSiteSpeciesType_free(BindingSiteSpeciesType_t * bsst)
{
  if (bsst != NULL)
    delete bsst;
}


LIBSBML_EXTERN
BindingSiteSpeciesType_t *
BindingSiteSpeciesType_clone(BindingSiteSpeciesType_t * bsst)
{
  if (bsst != NULL)
  {
    return static_cast<BindingSiteSpeciesType_t*>(bsst->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
int
BindingSiteSpeciesType_hasRequiredAttributes(const BindingSiteSpeciesType_t * bsst)
{
  return (bsst != NULL) ? static_cast<int>(bsst->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


