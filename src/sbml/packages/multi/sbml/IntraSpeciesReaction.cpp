/**
 * @file:   IntraSpeciesReaction.cpp
 * @brief:  Implementation of the IntraSpeciesReaction class
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


#include <sbml/packages/multi/sbml/IntraSpeciesReaction.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new IntraSpeciesReaction with the given level, version, and package version.
 */
IntraSpeciesReaction::IntraSpeciesReaction (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : Reaction(level, version)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new IntraSpeciesReaction with the given MultiPkgNamespaces object.
 */
IntraSpeciesReaction::IntraSpeciesReaction (MultiPkgNamespaces* multins)
  : Reaction(multins)
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for IntraSpeciesReaction.
 */
IntraSpeciesReaction::IntraSpeciesReaction (const IntraSpeciesReaction& orig)
  : Reaction(orig)
{
}


/*
 * Assignment for IntraSpeciesReaction.
 */
IntraSpeciesReaction&
IntraSpeciesReaction::operator=(const IntraSpeciesReaction& rhs)
{
  if (&rhs != this)
  {
    Reaction::operator=(rhs);
  }
  return *this;
}


/*
 * Clone for IntraSpeciesReaction.
 */
IntraSpeciesReaction*
IntraSpeciesReaction::clone () const
{
  return new IntraSpeciesReaction(*this);
}


/*
 * Destructor for IntraSpeciesReaction.
 */
IntraSpeciesReaction::~IntraSpeciesReaction ()
{
}


/*
 * Returns the XML element name of this object
 */
const std::string&
IntraSpeciesReaction::getElementName () const
{
  static const string name = "intraSpeciesReaction";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
IntraSpeciesReaction::getTypeCode () const
{
  return SBML_MULTI_INTRA_SPECIES_REACTION;
}


/*
 * check if all the required attributes are set
 */
bool
IntraSpeciesReaction::hasRequiredAttributes () const
{
  bool allPresent = Reaction::hasRequiredAttributes();

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
IntraSpeciesReaction::writeElements (XMLOutputStream& stream) const
{
  Reaction::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
IntraSpeciesReaction::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
IntraSpeciesReaction::setSBMLDocument (SBMLDocument* d)
{
  Reaction::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
IntraSpeciesReaction::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  Reaction::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
IntraSpeciesReaction::createObject(XMLInputStream& stream)
{
  SBase* object = Reaction::createObject(stream);

  connectToChild();


  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
IntraSpeciesReaction::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Reaction::addExpectedAttributes(attributes);

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
IntraSpeciesReaction::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  Reaction::readAttributes(attributes, expectedAttributes);

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
        getErrorLog()->logPackageError("multi", MultiIntSpeRec_AllowedAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiIntSpeRec_AllowedAtts,
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
IntraSpeciesReaction::writeAttributes (XMLOutputStream& stream) const
{
  Reaction::writeAttributes(stream);

}


  /** @endcond */


LIBSBML_EXTERN
IntraSpeciesReaction_t *
IntraSpeciesReaction_create(unsigned int level, unsigned int version,
                            unsigned int pkgVersion)
{
  return new IntraSpeciesReaction(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
IntraSpeciesReaction_free(IntraSpeciesReaction_t * isr)
{
  if (isr != NULL)
    delete isr;
}


LIBSBML_EXTERN
IntraSpeciesReaction_t *
IntraSpeciesReaction_clone(IntraSpeciesReaction_t * isr)
{
  if (isr != NULL)
  {
    return static_cast<IntraSpeciesReaction_t*>(isr->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
int
IntraSpeciesReaction_hasRequiredAttributes(const IntraSpeciesReaction_t * isr)
{
  return (isr != NULL) ? static_cast<int>(isr->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


