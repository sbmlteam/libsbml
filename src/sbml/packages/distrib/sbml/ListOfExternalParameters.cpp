/**
 * @file ListOfExternalParameters.cpp
 * @brief Implementation of the ListOfExternalParameters class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/distrib/sbml/ListOfExternalParameters.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfExternalParameters using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
ListOfExternalParameters::ListOfExternalParameters(unsigned int level,
                                                   unsigned int version,
                                                   unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfExternalParameters using the given DistribPkgNamespaces
 * object.
 */
ListOfExternalParameters::ListOfExternalParameters(DistribPkgNamespaces
  *distribns)
  : ListOf(distribns)
{
  setElementNamespace(distribns->getURI());
}


/*
 * Copy constructor for ListOfExternalParameters.
 */
ListOfExternalParameters::ListOfExternalParameters(const
  ListOfExternalParameters& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfExternalParameters.
 */
ListOfExternalParameters&
ListOfExternalParameters::operator=(const ListOfExternalParameters& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfExternalParameters object.
 */
ListOfExternalParameters*
ListOfExternalParameters::clone() const
{
  return new ListOfExternalParameters(*this);
}


/*
 * Destructor for ListOfExternalParameters.
 */
ListOfExternalParameters::~ListOfExternalParameters()
{
}


/*
 * Get a DistribExternalParameter from the ListOfExternalParameters.
 */
DistribExternalParameter*
ListOfExternalParameters::get(unsigned int n)
{
  return static_cast<DistribExternalParameter*>(ListOf::get(n));
}


/*
 * Get a DistribExternalParameter from the ListOfExternalParameters.
 */
const DistribExternalParameter*
ListOfExternalParameters::get(unsigned int n) const
{
  return static_cast<const DistribExternalParameter*>(ListOf::get(n));
}


/*
 * Get a DistribExternalParameter from the ListOfExternalParameters based on
 * its identifier.
 */
DistribExternalParameter*
ListOfExternalParameters::get(const std::string& sid)
{
  return const_cast<DistribExternalParameter*>(static_cast<const
    ListOfExternalParameters&>(*this).get(sid));
}


/*
 * Get a DistribExternalParameter from the ListOfExternalParameters based on
 * its identifier.
 */
const DistribExternalParameter*
ListOfExternalParameters::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(),
    IdEq<DistribExternalParameter>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    DistribExternalParameter*> (*result);
}


/*
 * Removes the nth DistribExternalParameter from this ListOfExternalParameters
 * and returns a pointer to it.
 */
DistribExternalParameter*
ListOfExternalParameters::remove(unsigned int n)
{
  return static_cast<DistribExternalParameter*>(ListOf::remove(n));
}


/*
 * Removes the DistribExternalParameter from this ListOfExternalParameters
 * based on its identifier and returns a pointer to it.
 */
DistribExternalParameter*
ListOfExternalParameters::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(),
    IdEq<DistribExternalParameter>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <DistribExternalParameter*> (item);
}


/*
 * Adds a copy of the given DistribExternalParameter to this
 * ListOfExternalParameters.
 */
int
ListOfExternalParameters::addDistribExternalParameter(const
  DistribExternalParameter* dep)
{
  if (dep == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (dep->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != dep->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != dep->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(dep)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(dep);
  }
}


/*
 * Get the number of DistribExternalParameter objects in this
 * ListOfExternalParameters.
 */
unsigned int
ListOfExternalParameters::getNumDistribExternalParameters() const
{
  return size();
}


/*
 * Creates a new DistribExternalParameter object, adds it to this
 * ListOfExternalParameters object and returns the DistribExternalParameter
 * object created.
 */
DistribExternalParameter*
ListOfExternalParameters::createDistribExternalParameter()
{
  DistribExternalParameter* dep = NULL;

  try
  {
    DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
      getPackageVersion());
    dep = new DistribExternalParameter(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (dep != NULL)
  {
    appendAndOwn(dep);
  }

  return dep;
}


/*
 * Returns the XML element name of this ListOfExternalParameters object.
 */
const std::string&
ListOfExternalParameters::getElementName() const
{
  static const string name = "listOfExternalParameters";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfExternalParameters object.
 */
int
ListOfExternalParameters::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfExternalParameters object.
 */
int
ListOfExternalParameters::getItemTypeCode() const
{
  return SBML_DISTRIB_EXTERNALPARAMETER;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new DistribExternalParameter in this ListOfExternalParameters
 */
SBase*
ListOfExternalParameters::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "externalParameter")
  {
    object = new DistribExternalParameter(distribns);
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
ListOfExternalParameters::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a DistribExternalParameter_t from the ListOf_t.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfExternalParameters_getDistribExternalParameter(ListOf_t* lo,
                                                     unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfExternalParameters*>(lo)->get(n);
}


/*
 * Get a DistribExternalParameter_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfExternalParameters_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfExternalParameters*>(lo)->get(sid)
    : NULL;
}


/*
 * Removes the nth DistribExternalParameter_t from this ListOf_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfExternalParameters_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfExternalParameters*>(lo)->remove(n);
}


/*
 * Removes the DistribExternalParameter_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
ListOfExternalParameters_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfExternalParameters*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


