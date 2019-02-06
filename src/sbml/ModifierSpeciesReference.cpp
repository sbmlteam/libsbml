/**
 * @file    ModifierSpeciesReference.cpp
 * @brief   Implementation of ModifierSpeciesReference. 
 * @author  Ben Bornstein
 *
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * ---------------------------------------------------------------------- -->*/


#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/ModifierSpeciesReference.h>
#include <sbml/extension/SBasePlugin.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus


ModifierSpeciesReference::ModifierSpeciesReference (unsigned int level, 
                          unsigned int version) :
  SimpleSpeciesReference(level, version)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


ModifierSpeciesReference::ModifierSpeciesReference (SBMLNamespaces *sbmlns) :
  SimpleSpeciesReference(sbmlns)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  loadPlugins(sbmlns);
}


/*
 * Destroys this ModifierSpeciesReference.
 */
ModifierSpeciesReference::~ModifierSpeciesReference ()
{
}


/** @cond doxygenLibsbmlInternal */
bool
ModifierSpeciesReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}
/** @endcond */


/*
 * @return a (deep) copy of this ModifierSpeciesReference.
 */
ModifierSpeciesReference*
ModifierSpeciesReference::clone () const
{
  return new ModifierSpeciesReference(*this);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * (default).
 *
 * @see getElementName()
 */
int
ModifierSpeciesReference::getTypeCode () const
{
  return SBML_MODIFIER_SPECIES_REFERENCE;
}


/*
 * @return the name of this element ie "modifierSpeciesReference".
 
 */
const string&
ModifierSpeciesReference::getElementName () const
{
  static const string name = "modifierSpeciesReference";
  return name;
}


bool 
ModifierSpeciesReference::hasRequiredAttributes() const
{
  bool allPresent = SimpleSpeciesReference::hasRequiredAttributes();

  return allPresent;
}
/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::getAttribute(const std::string& attributeName,
                                       bool& value) const
{
  int return_value = SimpleSpeciesReference::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::getAttribute(const std::string& attributeName,
                                       int& value) const
{
  int return_value = SimpleSpeciesReference::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::getAttribute(const std::string& attributeName,
                                       double& value) const
{
  int return_value = SimpleSpeciesReference::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::getAttribute(const std::string& attributeName,
                                       unsigned int& value) const
{
  int return_value = SimpleSpeciesReference::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::getAttribute(const std::string& attributeName,
                                       std::string& value) const
{
  int return_value = SimpleSpeciesReference::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
//int
//ModifierSpeciesReference::getAttribute(const std::string& attributeName,
//                                       const char* value) const
//{
//  int return_value = SimpleSpeciesReference::getAttribute(attributeName,
//    value);
//
//  return return_value;
//}
//
/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this ModifierSpeciesReference's attribute
 * "attributeName" is set.
 */
bool
ModifierSpeciesReference::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = SimpleSpeciesReference::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::setAttribute(const std::string& attributeName,
                                       bool value)
{
  int return_value = SimpleSpeciesReference::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::setAttribute(const std::string& attributeName,
                                       int value)
{
  int return_value = SimpleSpeciesReference::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::setAttribute(const std::string& attributeName,
                                       double value)
{
  int return_value = SimpleSpeciesReference::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::setAttribute(const std::string& attributeName,
                                       unsigned int value)
{
  int return_value = SimpleSpeciesReference::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::setAttribute(const std::string& attributeName,
                                       const std::string& value)
{
  int return_value = SimpleSpeciesReference::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
//int
//ModifierSpeciesReference::setAttribute(const std::string& attributeName,
//                                       const char* value)
//{
//  int return_value = SimpleSpeciesReference::setAttribute(attributeName,
//    value);
//
//  return return_value;
//}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * ModifierSpeciesReference.
 */
int
ModifierSpeciesReference::unsetAttribute(const std::string& attributeName)
{
  int value = SimpleSpeciesReference::unsetAttribute(attributeName);

  return value;
}

/** @endcond */

#endif /* cplusplus */

/** @cond doxygenLibsbmlInternal */


/** @cond doxygenIgnored */
/** @endcond */


LIBSBML_EXTERN
ModifierSpeciesReference_t *
ModifierSpeciesReference_create(unsigned int level, unsigned int version)
{
  try
  {
    ModifierSpeciesReference* obj = new ModifierSpeciesReference(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


LIBSBML_EXTERN
ModifierSpeciesReference_t *
ModifierSpeciesReference_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    ModifierSpeciesReference* obj = new ModifierSpeciesReference(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


LIBSBML_EXTERN
void
ModifierSpeciesReference_free(ModifierSpeciesReference_t * msr)
{
  if (msr != NULL)
    delete msr;
}


LIBSBML_EXTERN
ModifierSpeciesReference_t *
ModifierSpeciesReference_clone(ModifierSpeciesReference_t * msr)
{
  if (msr != NULL)
  {
    return static_cast<ModifierSpeciesReference_t*>(msr->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
ModifierSpeciesReference_getId(const ModifierSpeciesReference_t * msr)
{
	return (msr != NULL && msr->isSetId()) ? msr->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
ModifierSpeciesReference_getName(const ModifierSpeciesReference_t * msr)
{
	return (msr != NULL && msr->isSetName()) ? msr->getName().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
ModifierSpeciesReference_getSpecies(const ModifierSpeciesReference_t * msr)
{
	return (msr != NULL && msr->isSetSpecies()) ? msr->getSpecies().c_str() : NULL;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_isSetId(const ModifierSpeciesReference_t * msr)
{
  return (msr != NULL) ? static_cast<int>(msr->isSetId()) : 0;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_isSetName(const ModifierSpeciesReference_t * msr)
{
  return (msr != NULL) ? static_cast<int>(msr->isSetName()) : 0;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_isSetSpecies(const ModifierSpeciesReference_t * msr)
{
  return (msr != NULL) ? static_cast<int>(msr->isSetSpecies()) : 0;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_setId(ModifierSpeciesReference_t * msr, const char * id)
{
  if (msr != NULL)
    return (id == NULL) ? msr->setId("") : msr->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_setName(ModifierSpeciesReference_t * msr, const char * name)
{
  if (msr != NULL)
    return (name == NULL) ? msr->setName("") : msr->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_setSpecies(ModifierSpeciesReference_t * msr, const char * species)
{
  if (msr != NULL)
    return (species == NULL) ? msr->setSpecies("") : msr->setSpecies(species);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_unsetId(ModifierSpeciesReference_t * msr)
{
  return (msr != NULL) ? msr->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_unsetName(ModifierSpeciesReference_t * msr)
{
  return (msr != NULL) ? msr->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_unsetSpecies(ModifierSpeciesReference_t * msr)
{
  return (msr != NULL) ? msr->unsetSpecies() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ModifierSpeciesReference_hasRequiredAttributes(const ModifierSpeciesReference_t * msr)
{
  return (msr != NULL) ? static_cast<int>(msr->hasRequiredAttributes()) : 0;
}



LIBSBML_CPP_NAMESPACE_END

