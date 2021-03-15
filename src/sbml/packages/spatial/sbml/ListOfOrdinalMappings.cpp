/**
 * @file ListOfOrdinalMappings.cpp
 * @brief Implementation of the ListOfOrdinalMappings class.
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
#include <sbml/packages/spatial/sbml/ListOfOrdinalMappings.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfOrdinalMappings using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ListOfOrdinalMappings::ListOfOrdinalMappings(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfOrdinalMappings using the given SpatialPkgNamespaces
 * object.
 */
ListOfOrdinalMappings::ListOfOrdinalMappings(SpatialPkgNamespaces *spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Copy constructor for ListOfOrdinalMappings.
 */
ListOfOrdinalMappings::ListOfOrdinalMappings(const ListOfOrdinalMappings& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfOrdinalMappings.
 */
ListOfOrdinalMappings&
ListOfOrdinalMappings::operator=(const ListOfOrdinalMappings& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfOrdinalMappings object.
 */
ListOfOrdinalMappings*
ListOfOrdinalMappings::clone() const
{
  return new ListOfOrdinalMappings(*this);
}


/*
 * Destructor for ListOfOrdinalMappings.
 */
ListOfOrdinalMappings::~ListOfOrdinalMappings()
{
}


/*
 * Get an OrdinalMapping from the ListOfOrdinalMappings.
 */
OrdinalMapping*
ListOfOrdinalMappings::get(unsigned int n)
{
  return static_cast<OrdinalMapping*>(ListOf::get(n));
}


/*
 * Get an OrdinalMapping from the ListOfOrdinalMappings.
 */
const OrdinalMapping*
ListOfOrdinalMappings::get(unsigned int n) const
{
  return static_cast<const OrdinalMapping*>(ListOf::get(n));
}


/*
 * Get an OrdinalMapping from the ListOfOrdinalMappings based on its
 * identifier.
 */
OrdinalMapping*
ListOfOrdinalMappings::get(const std::string& sid)
{
  return const_cast<OrdinalMapping*>(static_cast<const
    ListOfOrdinalMappings&>(*this).get(sid));
}


/*
 * Get an OrdinalMapping from the ListOfOrdinalMappings based on its
 * identifier.
 */
const OrdinalMapping*
ListOfOrdinalMappings::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<OrdinalMapping>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const OrdinalMapping*>
    (*result);
}


/*
 * Removes the nth OrdinalMapping from this ListOfOrdinalMappings and returns a
 * pointer to it.
 */
OrdinalMapping*
ListOfOrdinalMappings::remove(unsigned int n)
{
  return static_cast<OrdinalMapping*>(ListOf::remove(n));
}


/*
 * Removes the OrdinalMapping from this ListOfOrdinalMappings based on its
 * identifier and returns a pointer to it.
 */
OrdinalMapping*
ListOfOrdinalMappings::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<OrdinalMapping>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <OrdinalMapping*> (item);
}


/*
 * Adds a copy of the given OrdinalMapping to this ListOfOrdinalMappings.
 */
int
ListOfOrdinalMappings::addOrdinalMapping(const OrdinalMapping* om)
{
  if (om == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (om->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != om->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != om->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(om)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(om);
  }
}


/*
 * Get the number of OrdinalMapping objects in this ListOfOrdinalMappings.
 */
unsigned int
ListOfOrdinalMappings::getNumOrdinalMappings() const
{
  return size();
}


/*
 * Creates a new OrdinalMapping object, adds it to this ListOfOrdinalMappings
 * object and returns the OrdinalMapping object created.
 */
OrdinalMapping*
ListOfOrdinalMappings::createOrdinalMapping()
{
  OrdinalMapping* om = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    om = new OrdinalMapping(spatialns);
    delete spatialns;
  }
  catch (...)
  {
  }

  if (om != NULL)
  {
    appendAndOwn(om);
  }

  return om;
}


/*
 * Used by ListOfOrdinalMappings::get() to lookup an OrdinalMapping based on
 * its GeometryDefinition.
 */
struct IdEqGD
{
  const string& id;
   
  IdEqGD (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<OrdinalMapping*>(sb)->getGeometryDefinition() == id);
  }
};


/*
 * Get an OrdinalMapping from the ListOfOrdinalMappings based on the
 * GeometryDefinition to which it refers.
 */
const OrdinalMapping*
ListOfOrdinalMappings::getByGeometryDefinition(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqGD(sid));
  return (result == mItems.end()) ? 0 : static_cast <const OrdinalMapping*>
    (*result);
}


/*
 * Get an OrdinalMapping from the ListOfOrdinalMappings based on the
 * GeometryDefinition to which it refers.
 */
OrdinalMapping*
ListOfOrdinalMappings::getByGeometryDefinition(const std::string& sid)
{
  return const_cast<OrdinalMapping*>(static_cast<const
    ListOfOrdinalMappings&>(*this).getByGeometryDefinition(sid));
}


/*
 * Returns the XML element name of this ListOfOrdinalMappings object.
 */
const std::string&
ListOfOrdinalMappings::getElementName() const
{
  static const string name = "listOfOrdinalMappings";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfOrdinalMappings object.
 */
int
ListOfOrdinalMappings::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfOrdinalMappings object.
 */
int
ListOfOrdinalMappings::getItemTypeCode() const
{
  return SBML_SPATIAL_ORDINALMAPPING;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new OrdinalMapping in this ListOfOrdinalMappings
 */
SBase*
ListOfOrdinalMappings::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "ordinalMapping")
  {
    object = new OrdinalMapping(spatialns);
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
ListOfOrdinalMappings::writeXMLNS(XMLOutputStream& stream) const
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
 * Get an OrdinalMapping_t from the ListOf_t.
 */
LIBSBML_EXTERN
OrdinalMapping_t*
ListOfOrdinalMappings_getOrdinalMapping(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfOrdinalMappings*>(lo)->get(n);
}


/*
 * Get an OrdinalMapping_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
OrdinalMapping_t*
ListOfOrdinalMappings_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfOrdinalMappings*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth OrdinalMapping_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
OrdinalMapping_t*
ListOfOrdinalMappings_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfOrdinalMappings*>(lo)->remove(n);
}


/*
 * Removes the OrdinalMapping_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
OrdinalMapping_t*
ListOfOrdinalMappings_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfOrdinalMappings*>(lo)->remove(sid)
    : NULL;
}




LIBSBML_CPP_NAMESPACE_END


