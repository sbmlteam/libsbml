/**
 * @file ListOfUncertainties.cpp
 * @brief Implementation of the ListOfUncertainties class.
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
#include <sbml/packages/distrib/sbml/ListOfUncertainties.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfUncertainties using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
ListOfUncertainties::ListOfUncertainties(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfUncertainties using the given DistribPkgNamespaces
 * object.
 */
ListOfUncertainties::ListOfUncertainties(DistribPkgNamespaces *distribns)
  : ListOf(distribns)
{
  setElementNamespace(distribns->getURI());
}


/*
 * Copy constructor for ListOfUncertainties.
 */
ListOfUncertainties::ListOfUncertainties(const ListOfUncertainties& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfUncertainties.
 */
ListOfUncertainties&
ListOfUncertainties::operator=(const ListOfUncertainties& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfUncertainties object.
 */
ListOfUncertainties*
ListOfUncertainties::clone() const
{
  return new ListOfUncertainties(*this);
}


/*
 * Destructor for ListOfUncertainties.
 */
ListOfUncertainties::~ListOfUncertainties()
{
}


/*
 * Get an Uncertainty from the ListOfUncertainties.
 */
Uncertainty*
ListOfUncertainties::get(unsigned int n)
{
  return static_cast<Uncertainty*>(ListOf::get(n));
}


/*
 * Get an Uncertainty from the ListOfUncertainties.
 */
const Uncertainty*
ListOfUncertainties::get(unsigned int n) const
{
  return static_cast<const Uncertainty*>(ListOf::get(n));
}


/*
 * Get an Uncertainty from the ListOfUncertainties based on its identifier.
 */
Uncertainty*
ListOfUncertainties::get(const std::string& sid)
{
  return const_cast<Uncertainty*>(static_cast<const
    ListOfUncertainties&>(*this).get(sid));
}


/*
 * Get an Uncertainty from the ListOfUncertainties based on its identifier.
 */
const Uncertainty*
ListOfUncertainties::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<Uncertainty>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const Uncertainty*>
    (*result);
}


/*
 * Removes the nth Uncertainty from this ListOfUncertainties and returns a
 * pointer to it.
 */
Uncertainty*
ListOfUncertainties::remove(unsigned int n)
{
  return static_cast<Uncertainty*>(ListOf::remove(n));
}


/*
 * Removes the Uncertainty from this ListOfUncertainties based on its
 * identifier and returns a pointer to it.
 */
Uncertainty*
ListOfUncertainties::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<Uncertainty>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Uncertainty*> (item);
}


/*
 * Adds a copy of the given Uncertainty to this ListOfUncertainties.
 */
int
ListOfUncertainties::addUncertainty(const Uncertainty* u)
{
  if (u == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (u->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != u->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != u->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(u)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(u);
  }
}


/*
 * Get the number of Uncertainty objects in this ListOfUncertainties.
 */
unsigned int
ListOfUncertainties::getNumUncertainties() const
{
  return size();
}


/*
 * Creates a new Uncertainty object, adds it to this ListOfUncertainties object
 * and returns the Uncertainty object created.
 */
Uncertainty*
ListOfUncertainties::createUncertainty()
{
  Uncertainty* u = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    u = new Uncertainty(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (u != NULL)
  {
    appendAndOwn(u);
  }

  return u;
}


/*
 * Returns the XML element name of this ListOfUncertainties object.
 */
const std::string&
ListOfUncertainties::getElementName() const
{
  static const string name = "listOfUncertainties";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfUncertainties object.
 */
int
ListOfUncertainties::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfUncertainties object.
 */
int
ListOfUncertainties::getItemTypeCode() const
{
  return SBML_DISTRIB_UNCERTAINTY;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Uncertainty in this ListOfUncertainties
 */
SBase*
ListOfUncertainties::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "uncertainty")
  {
    object = new Uncertainty(distribns);
    appendAndOwn(object);
  }

  delete distribns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Distrib package
 */
void
ListOfUncertainties::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(DistribExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(DistribExtension::getXmlnsL3V1V1(), prefix);
    }
  }

  stream << xmlns;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get an Uncertainty_t from the ListOf_t.
 */
LIBSBML_EXTERN
Uncertainty_t*
ListOfUncertainties_getUncertainty(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfUncertainties*>(lo)->get(n);
}


/*
 * Get an Uncertainty_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
Uncertainty_t*
ListOfUncertainties_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfUncertainties*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth Uncertainty_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
Uncertainty_t*
ListOfUncertainties_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfUncertainties*>(lo)->remove(n);
}


/*
 * Removes the Uncertainty_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
Uncertainty_t*
ListOfUncertainties_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfUncertainties*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


