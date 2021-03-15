/**
 * @file ListOfUncertParameters.cpp
 * @brief Implementation of the ListOfUncertParameters class.
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
#include <sbml/packages/distrib/sbml/ListOfUncertParameters.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfUncertParameters using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
ListOfUncertParameters::ListOfUncertParameters(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfUncertParameters using the given DistribPkgNamespaces
 * object.
 */
ListOfUncertParameters::ListOfUncertParameters(DistribPkgNamespaces *distribns)
  : ListOf(distribns)
{
  setElementNamespace(distribns->getURI());
}


/*
 * Copy constructor for ListOfUncertParameters.
 */
ListOfUncertParameters::ListOfUncertParameters(const ListOfUncertParameters&
  orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfUncertParameters.
 */
ListOfUncertParameters&
ListOfUncertParameters::operator=(const ListOfUncertParameters& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfUncertParameters object.
 */
ListOfUncertParameters*
ListOfUncertParameters::clone() const
{
  return new ListOfUncertParameters(*this);
}


/*
 * Destructor for ListOfUncertParameters.
 */
ListOfUncertParameters::~ListOfUncertParameters()
{
}


/*
 * Get an UncertParameter from the ListOfUncertParameters.
 */
UncertParameter*
ListOfUncertParameters::get(unsigned int n)
{
  return static_cast<UncertParameter*>(ListOf::get(n));
}


/*
 * Get an UncertParameter from the ListOfUncertParameters.
 */
const UncertParameter*
ListOfUncertParameters::get(unsigned int n) const
{
  return static_cast<const UncertParameter*>(ListOf::get(n));
}


/*
 * Get an UncertParameter from the ListOfUncertParameters based on its
 * identifier.
 */
UncertParameter*
ListOfUncertParameters::get(const std::string& sid)
{
  return const_cast<UncertParameter*>(static_cast<const
    ListOfUncertParameters&>(*this).get(sid));
}


/*
 * Get an UncertParameter from the ListOfUncertParameters based on its
 * identifier.
 */
const UncertParameter*
ListOfUncertParameters::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<UncertParameter>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const UncertParameter*>
    (*result);
}


/*
 * Removes the nth UncertParameter from this ListOfUncertParameters and returns
 * a pointer to it.
 */
UncertParameter*
ListOfUncertParameters::remove(unsigned int n)
{
  return static_cast<UncertParameter*>(ListOf::remove(n));
}


/*
 * Removes the UncertParameter from this ListOfUncertParameters based on its
 * identifier and returns a pointer to it.
 */
UncertParameter*
ListOfUncertParameters::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<UncertParameter>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <UncertParameter*> (item);
}


/*
 * Adds a copy of the given UncertParameter to this ListOfUncertParameters.
 */
int
ListOfUncertParameters::addUncertParameter(const UncertParameter* up)
{
  if (up == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (up->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != up->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != up->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(up)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(up);
  }
}


/*
* Adds a copy of the given UncertParameter to this ListOfUncertParameters.
*/
int
ListOfUncertParameters::addUncertSpan(const UncertSpan* up)
{
  if (up == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (up->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != up->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != up->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(up)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(up);
  }
}


/*
 * Get the number of UncertParameter objects in this ListOfUncertParameters.
 */
unsigned int
ListOfUncertParameters::getNumUncertParameters() const
{
  return size();
}


/*
 * Creates a new UncertParameter object, adds it to this ListOfUncertParameters
 * object and returns the UncertParameter object created.
 */
UncertParameter*
ListOfUncertParameters::createUncertParameter()
{
  UncertParameter* up = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    up = new UncertParameter(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (up != NULL)
  {
    appendAndOwn(up);
  }

  return up;
}


/*
* Creates a new UncertSpan object, adds it to this ListOfUncertSpans
* object and returns the UncertSpan object created.
*/
UncertSpan*
ListOfUncertParameters::createUncertSpan()
{
  UncertSpan* up = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    up = new UncertSpan(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (up != NULL)
  {
    appendAndOwn(up);
  }

  return up;
}


/*
 * Used by ListOfUncertParameters::get() to lookup an UncertParameter based on
 * its Var.
 */
struct IdEqV
{
  const string& id;
   
  IdEqV (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<UncertParameter*>(sb)->getVar() == id);
  }
};


struct UTypeEqV
{
  const UncertType_t utype;

  UTypeEqV (const UncertType_t utype) : utype(utype) { }
  bool operator() (SBase* sb)
  {
    return (static_cast<UncertParameter*>(sb)->getType() == utype);
  }
};


/*
 * Get an UncertParameter from the ListOfUncertParameters based on the element
 * to which it refers.
 */
const UncertParameter*
ListOfUncertParameters::getByVar(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqV(sid));
  return (result == mItems.end()) ? 0 : static_cast <const UncertParameter*>
    (*result);
}


const UncertParameter*
ListOfUncertParameters::getByType(UncertType_t utype) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), UTypeEqV(utype));
  return (result == mItems.end()) ? 0 : static_cast <const UncertParameter*>
    (*result);
}


/*
 * Get an UncertParameter from the ListOfUncertParameters based on the element
 * to which it refers.
 */
UncertParameter*
ListOfUncertParameters::getByVar(const std::string& sid)
{
  return const_cast<UncertParameter*>(static_cast<const
    ListOfUncertParameters&>(*this).getByVar(sid));
}


UncertParameter*
ListOfUncertParameters::getByType(UncertType_t utype)
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), UTypeEqV(utype));
  return (result == mItems.end()) ? 0 : static_cast <UncertParameter*>
    (*result);
}


/*
 * Returns the XML element name of this ListOfUncertParameters object.
 */
const std::string&
ListOfUncertParameters::getElementName() const
{
  static const string name = "listOfUncertParameters";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfUncertParameters object.
 */
int
ListOfUncertParameters::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfUncertParameters object.
 */
int
ListOfUncertParameters::getItemTypeCode() const
{
  return SBML_DISTRIB_UNCERTPARAMETER;
}

/** @cond doxygenLibsbmlInternal */
bool
ListOfUncertParameters::isValidTypeForList(SBase * item)
{
  int tc = item->getTypeCode();
  return ((tc == SBML_DISTRIB_UNCERTPARAMETER)
    || (tc == SBML_DISTRIB_UNCERTSTATISTICSPAN));
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new UncertParameter in this ListOfUncertParameters
 */
SBase*
ListOfUncertParameters::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "uncertParameter")
  {
    object = new UncertParameter(distribns);
    appendAndOwn(object);
  }
  else if (name == "uncertSpan")
  {
    object = new UncertSpan(distribns);
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
ListOfUncertParameters::writeXMLNS(XMLOutputStream& stream) const
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
 * Get an UncertParameter_t from the ListOf_t.
 */
LIBSBML_EXTERN
UncertParameter_t*
ListOfUncertParameters_getUncertParameter(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfUncertParameters*>(lo)->get(n);
}


/*
 * Get an UncertParameter_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
UncertParameter_t*
ListOfUncertParameters_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfUncertParameters*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth UncertParameter_t from this ListOf_t and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
UncertParameter_t*
ListOfUncertParameters_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfUncertParameters*>(lo)->remove(n);
}


/*
 * Removes the UncertParameter_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
UncertParameter_t*
ListOfUncertParameters_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfUncertParameters*>(lo)->remove(sid)
    : NULL;
}




LIBSBML_CPP_NAMESPACE_END


