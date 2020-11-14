/**
 * @file ListOfSampledFields.cpp
 * @brief Implementation of the ListOfSampledFields class.
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
#include <sbml/packages/spatial/sbml/ListOfSampledFields.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfSampledFields using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfSampledFields::ListOfSampledFields(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfSampledFields using the given SpatialPkgNamespaces
 * object.
 */
ListOfSampledFields::ListOfSampledFields(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfSampledFields.
 */
ListOfSampledFields::ListOfSampledFields(const ListOfSampledFields& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfSampledFields.
 */
ListOfSampledFields&
ListOfSampledFields::operator=(const ListOfSampledFields& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfSampledFields object.
 */
ListOfSampledFields*
ListOfSampledFields::clone() const
{
  return new ListOfSampledFields(*this);
}


/*
 * Destructor for ListOfSampledFields.
 */
ListOfSampledFields::~ListOfSampledFields()
{
}


/*
 * Get a SampledField from the ListOfSampledFields.
 */
SampledField*
ListOfSampledFields::get(unsigned int n)
{
  return static_cast<SampledField*>(ListOf::get(n));
}


/*
 * Get a SampledField from the ListOfSampledFields.
 */
const SampledField*
ListOfSampledFields::get(unsigned int n) const
{
  return static_cast<const SampledField*>(ListOf::get(n));
}


/*
 * Get a SampledField from the ListOfSampledFields based on its identifier.
 */
SampledField*
ListOfSampledFields::get(const std::string& sid)
{
  return const_cast<SampledField*>(static_cast<const
    ListOfSampledFields&>(*this).get(sid));
}


/*
 * Get a SampledField from the ListOfSampledFields based on its identifier.
 */
const SampledField*
ListOfSampledFields::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<SampledField>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const SampledField*>
    (*result);
}


/*
 * Removes the nth SampledField from this ListOfSampledFields and returns a
 * pointer to it.
 */
SampledField*
ListOfSampledFields::remove(unsigned int n)
{
  return static_cast<SampledField*>(ListOf::remove(n));
}


/*
 * Removes the SampledField from this ListOfSampledFields based on its
 * identifier and returns a pointer to it.
 */
SampledField*
ListOfSampledFields::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<SampledField>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SampledField*> (item);
}


/*
 * Adds a copy of the given SampledField to this ListOfSampledFields.
 */
int
ListOfSampledFields::addSampledField(const SampledField* sf)
{
  if (sf == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sf->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sf->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sf->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(sf)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(sf);
  }
}


/*
 * Get the number of SampledField objects in this ListOfSampledFields.
 */
unsigned int
ListOfSampledFields::getNumSampledFields() const
{
  return size();
}


/*
 * Creates a new SampledField object, adds it to this ListOfSampledFields
 * object and returns the SampledField object created.
 */
SampledField*
ListOfSampledFields::createSampledField()
{
  SampledField* sf = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sf = new SampledField(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (sf != NULL)
  {
    appendAndOwn(sf);
  }

  return sf;
}


/*
 * Returns the XML element name of this ListOfSampledFields object.
 */
const std::string&
ListOfSampledFields::getElementName() const
{
  static const string name = "listOfSampledFields";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfSampledFields object.
 */
int
ListOfSampledFields::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfSampledFields object.
 */
int
ListOfSampledFields::getItemTypeCode() const
{
  return SBML_SPATIAL_SAMPLEDFIELD;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SampledField in this ListOfSampledFields
 */
SBase*
ListOfSampledFields::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "sampledField")
  {
    object = new SampledField(spatialns);
    appendAndOwn(object);
  }

  delete spatialns;
  return object;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the namespace for the Spatial package
 */
void
ListOfSampledFields::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    const XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(SpatialExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(SpatialExtension::getXmlnsL3V1V1(), prefix);
    }
  }

  stream << xmlns;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get a SampledField_t from the ListOf_t.
 */
LIBSBML_EXTERN
SampledField_t*
ListOfSampledFields_getSampledField(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfSampledFields*>(lo)->get(n);
}


/*
 * Get a SampledField_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
SampledField_t*
ListOfSampledFields_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfSampledFields*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth SampledField_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
SampledField_t*
ListOfSampledFields_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfSampledFields*>(lo)->remove(n);
}


/*
 * Removes the SampledField_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
SampledField_t*
ListOfSampledFields_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfSampledFields*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


