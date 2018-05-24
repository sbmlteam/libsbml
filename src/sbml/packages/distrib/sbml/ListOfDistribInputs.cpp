/**
 * @file ListOfDistribInputs.cpp
 * @brief Implementation of the ListOfDistribInputs class.
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
#include <sbml/packages/distrib/sbml/ListOfDistribInputs.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfDistribInputs using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
ListOfDistribInputs::ListOfDistribInputs(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion)
  : DistribListOfBase(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ListOfDistribInputs using the given DistribPkgNamespaces
 * object.
 */
ListOfDistribInputs::ListOfDistribInputs(DistribPkgNamespaces *distribns)
  : DistribListOfBase(distribns)
{
  setElementNamespace(distribns->getURI());
}


/*
 * Copy constructor for ListOfDistribInputs.
 */
ListOfDistribInputs::ListOfDistribInputs(const ListOfDistribInputs& orig)
  : DistribListOfBase( orig )
{
}


/*
 * Assignment operator for ListOfDistribInputs.
 */
ListOfDistribInputs&
ListOfDistribInputs::operator=(const ListOfDistribInputs& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ListOfDistribInputs object.
 */
ListOfDistribInputs*
ListOfDistribInputs::clone() const
{
  return new ListOfDistribInputs(*this);
}


/*
 * Destructor for ListOfDistribInputs.
 */
ListOfDistribInputs::~ListOfDistribInputs()
{
}


/*
 * Get a DistribInput from the ListOfDistribInputs.
 */
DistribInput*
ListOfDistribInputs::get(unsigned int n)
{
  return static_cast<DistribInput*>(ListOf::get(n));
}


/*
 * Get a DistribInput from the ListOfDistribInputs by id.
 */
DistribInput*
ListOfDistribInputs::getByIndex(unsigned int n)
{
  return const_cast<DistribInput*>(
    static_cast<const ListOfDistribInputs&>(*this).getByIndex(n));
}


#ifndef SWIG
template<class CNAME>
struct IndexEq : public std::unary_function<SBase*, bool>
{
  unsigned int n;

  IndexEq (unsigned int n) : n(n) { }
  bool operator() (SBase* sb) 
       { return static_cast <CNAME*> (sb)->getIndex() == n; }
};
#endif /* SWIG */

/*
 * Get a DistribInput from the ListOfDistribInputs by id.
 */
const DistribInput*
ListOfDistribInputs::getByIndex(unsigned int n) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IndexEq<DistribInput>(n) );
  return (result == mItems.end()) ? 0 : static_cast <DistribInput*> (*result);
}


/*
 * Get a DistribInput from the ListOfDistribInputs.
 */
const DistribInput*
ListOfDistribInputs::get(unsigned int n) const
{
  return static_cast<const DistribInput*>(ListOf::get(n));
}


/*
 * Get a DistribInput from the ListOfDistribInputs based on its identifier.
 */
DistribInput*
ListOfDistribInputs::get(const std::string& sid)
{
  return const_cast<DistribInput*>(static_cast<const
    ListOfDistribInputs&>(*this).get(sid));
}


/*
 * Get a DistribInput from the ListOfDistribInputs based on its identifier.
 */
const DistribInput*
ListOfDistribInputs::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEq<DistribInput>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const DistribInput*>
    (*result);
}


/*
 * Removes the nth DistribInput from this ListOfDistribInputs and returns a
 * pointer to it.
 */
DistribInput*
ListOfDistribInputs::remove(unsigned int n)
{
  return static_cast<DistribInput*>(ListOf::remove(n));
}


/*
 * Removes the DistribInput from this ListOfDistribInputs based on its
 * identifier and returns a pointer to it.
 */
DistribInput*
ListOfDistribInputs::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(), IdEq<DistribInput>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <DistribInput*> (item);
}


/*
 * Adds a copy of the given DistribInput to this ListOfDistribInputs.
 */
int
ListOfDistribInputs::addDistribInput(const DistribInput* di)
{
  if (di == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (di->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != di->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != di->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(di)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(di);
  }
}


/*
 * Get the number of DistribInput objects in this ListOfDistribInputs.
 */
unsigned int
ListOfDistribInputs::getNumDistribInputs() const
{
  return size();
}


/*
 * Creates a new DistribInput object, adds it to this ListOfDistribInputs
 * object and returns the DistribInput object created.
 */
DistribInput*
ListOfDistribInputs::createDistribInput()
{
  DistribInput* di = NULL;

  try
  {
    DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
      getPackageVersion());
    di = new DistribInput(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (di != NULL)
  {
    appendAndOwn(di);
  }

  return di;
}


/*
 * Returns the XML element name of this ListOfDistribInputs object.
 */
const std::string&
ListOfDistribInputs::getElementName() const
{
  static const string name = "listOfInputs";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfDistribInputs object.
 */
int
ListOfDistribInputs::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfDistribInputs object.
 */
int
ListOfDistribInputs::getItemTypeCode() const
{
  return SBML_DISTRIB_DISTRIBINPUT;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new DistribInput in this ListOfDistribInputs
 */
SBase*
ListOfDistribInputs::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "input")
  {
    object = new DistribInput(distribns);
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
ListOfDistribInputs::writeXMLNS(XMLOutputStream& stream) const
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
 * Get a DistribInput_t from the ListOf_t.
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_getDistribInput(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDistribInputs*>(lo)->get(n);
}


/*
 * Get a DistribInput_t from the ListOf_t based on its identifier.
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDistribInputs*>(lo)->get(sid) :
    NULL;
}


/*
 * Removes the nth DistribInput_t from this ListOf_t and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfDistribInputs*>(lo)->remove(n);
}


/*
 * Removes the DistribInput_t from this ListOf_t based on its identifier and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
DistribInput_t*
ListOfDistribInputs_removeById(ListOf_t* lo, const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast <ListOfDistribInputs*>(lo)->remove(sid) :
    NULL;
}




LIBSBML_CPP_NAMESPACE_END


