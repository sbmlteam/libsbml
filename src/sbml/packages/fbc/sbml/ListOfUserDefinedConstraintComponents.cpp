/**
 * @file ListOfUserDefinedConstraintComponents.cpp
 * @brief Implementation of the ListOfUserDefinedConstraintComponents class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/fbc/sbml/ListOfUserDefinedConstraintComponents.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ListOfUserDefinedConstraintComponents using the given SBML
 * Level, Version and &ldquo;fbc&rdquo; package version.
 */
ListOfUserDefinedConstraintComponents::ListOfUserDefinedConstraintComponents(
                                                                             unsigned
                                                                               int
                                                                                 level,
                                                                             unsigned
                                                                               int
                                                                                 version,
                                                                             unsigned
                                                                               int
                                                                                 pkgVersion)
  : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ListOfUserDefinedConstraintComponents using the given
 * FbcPkgNamespaces object.
 */
ListOfUserDefinedConstraintComponents::ListOfUserDefinedConstraintComponents(FbcPkgNamespaces
  *fbcns)
  : ListOf(fbcns)
{
  setElementNamespace(fbcns->getURI());
}


/*
 * Copy constructor for ListOfUserDefinedConstraintComponents.
 */
ListOfUserDefinedConstraintComponents::ListOfUserDefinedConstraintComponents(const
  ListOfUserDefinedConstraintComponents& orig)
  : ListOf( orig )
{
}


/*
 * Assignment operator for ListOfUserDefinedConstraintComponents.
 */
ListOfUserDefinedConstraintComponents&
ListOfUserDefinedConstraintComponents::operator=(const
  ListOfUserDefinedConstraintComponents& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this
 * ListOfUserDefinedConstraintComponents object.
 */
ListOfUserDefinedConstraintComponents*
ListOfUserDefinedConstraintComponents::clone() const
{
  return new ListOfUserDefinedConstraintComponents(*this);
}


/*
 * Destructor for ListOfUserDefinedConstraintComponents.
 */
ListOfUserDefinedConstraintComponents::~ListOfUserDefinedConstraintComponents()
{
}


/*
 * Get an UserDefinedConstraintComponent from the
 * ListOfUserDefinedConstraintComponents.
 */
UserDefinedConstraintComponent*
ListOfUserDefinedConstraintComponents::get(unsigned int n)
{
  return static_cast<UserDefinedConstraintComponent*>(ListOf::get(n));
}


/*
 * Get an UserDefinedConstraintComponent from the
 * ListOfUserDefinedConstraintComponents.
 */
const UserDefinedConstraintComponent*
ListOfUserDefinedConstraintComponents::get(unsigned int n) const
{
  return static_cast<const UserDefinedConstraintComponent*>(ListOf::get(n));
}


/*
 * Get an UserDefinedConstraintComponent from the
 * ListOfUserDefinedConstraintComponents based on its identifier.
 */
UserDefinedConstraintComponent*
ListOfUserDefinedConstraintComponents::get(const std::string& sid)
{
  return const_cast<UserDefinedConstraintComponent*>(static_cast<const
    ListOfUserDefinedConstraintComponents&>(*this).get(sid));
}


/*
 * Get an UserDefinedConstraintComponent from the
 * ListOfUserDefinedConstraintComponents based on its identifier.
 */
const UserDefinedConstraintComponent*
ListOfUserDefinedConstraintComponents::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(),
    IdEq<UserDefinedConstraintComponent>(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    UserDefinedConstraintComponent*> (*result);
}


/*
 * Removes the nth UserDefinedConstraintComponent from this
 * ListOfUserDefinedConstraintComponents and returns a pointer to it.
 */
UserDefinedConstraintComponent*
ListOfUserDefinedConstraintComponents::remove(unsigned int n)
{
  return static_cast<UserDefinedConstraintComponent*>(ListOf::remove(n));
}


/*
 * Removes the UserDefinedConstraintComponent from this
 * ListOfUserDefinedConstraintComponents based on its identifier and returns a
 * pointer to it.
 */
UserDefinedConstraintComponent*
ListOfUserDefinedConstraintComponents::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if(mItems.begin(), mItems.end(),
    IdEq<UserDefinedConstraintComponent>(sid));

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <UserDefinedConstraintComponent*> (item);
}


/*
 * Adds a copy of the given UserDefinedConstraintComponent to this
 * ListOfUserDefinedConstraintComponents.
 */
int
ListOfUserDefinedConstraintComponents::addUserDefinedConstraintComponent(const
  UserDefinedConstraintComponent* udcc)
{
  if (udcc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (udcc->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != udcc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(udcc)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(udcc);
  }
}


/*
 * Get the number of UserDefinedConstraintComponent objects in this
 * ListOfUserDefinedConstraintComponents.
 */
unsigned int
ListOfUserDefinedConstraintComponents::getNumUserDefinedConstraintComponents()
  const
{
  return size();
}


/*
 * Creates a new UserDefinedConstraintComponent object, adds it to this
 * ListOfUserDefinedConstraintComponents object and returns the
 * UserDefinedConstraintComponent object created.
 */
UserDefinedConstraintComponent*
ListOfUserDefinedConstraintComponents::createUserDefinedConstraintComponent()
{
  UserDefinedConstraintComponent* udcc = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(),
      getPackageVersion());
    udcc = new UserDefinedConstraintComponent(fbcns);
    delete fbcns;
  }
  catch (...)
  {
  }

  if (udcc != NULL)
  {
    appendAndOwn(udcc);
  }

  return udcc;
}


/*
 * Used by ListOfUserDefinedConstraintComponents::get() to lookup an
 * UserDefinedConstraintComponent based on its Variable.
 */
struct IdEqV
{
  const string& id;
   
  IdEqV (const string& id) : id(id) { }
  bool operator() (SBase* sb)
  {
  return (static_cast<UserDefinedConstraintComponent*>(sb)->getVariable() ==
    id);
  }
};


/*
 * Get an UserDefinedConstraintComponent from the
 * ListOfUserDefinedConstraintComponents based on the Variable to which it
 * refers.
 */
const UserDefinedConstraintComponent*
ListOfUserDefinedConstraintComponents::getByVariable(const std::string& sid)
  const
{
  vector<SBase*>::const_iterator result;
  result = find_if(mItems.begin(), mItems.end(), IdEqV(sid));
  return (result == mItems.end()) ? 0 : static_cast <const
    UserDefinedConstraintComponent*> (*result);
}


/*
 * Get an UserDefinedConstraintComponent from the
 * ListOfUserDefinedConstraintComponents based on the Variable to which it
 * refers.
 */
UserDefinedConstraintComponent*
ListOfUserDefinedConstraintComponents::getByVariable(const std::string& sid)
{
  return const_cast<UserDefinedConstraintComponent*>(static_cast<const
    ListOfUserDefinedConstraintComponents&>(*this).getByVariable(sid));
}


/*
 * Returns the XML element name of this ListOfUserDefinedConstraintComponents
 * object.
 */
const std::string&
ListOfUserDefinedConstraintComponents::getElementName() const
{
  static const string name = "listOfUserDefinedConstraintComponents";
  return name;
}


/*
 * Returns the libSBML type code for this ListOfUserDefinedConstraintComponents
 * object.
 */
int
ListOfUserDefinedConstraintComponents::getTypeCode() const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the SBML objects contained in this
 * ListOfUserDefinedConstraintComponents object.
 */
int
ListOfUserDefinedConstraintComponents::getItemTypeCode() const
{
  return SBML_FBC_USERDEFINEDCONSTRAINTCOMPONENT;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new UserDefinedConstraintComponent in this
 * ListOfUserDefinedConstraintComponents
 */
SBase*
ListOfUserDefinedConstraintComponents::createObject(XMLInputStream& stream)
{
  const std::string& name = stream.peek().getName();
  SBase* object = NULL;
  FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());

  if (name == "userDefinedConstraintComponent")
  {
    object = new UserDefinedConstraintComponent(fbcns);
    appendAndOwn(object);
  }

  delete fbcns;
  return object;
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Get an UserDefinedConstraintComponent_t from the ListOf_t.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
ListOfUserDefinedConstraintComponents_getUserDefinedConstraintComponent(
                                                                        ListOf_t*
                                                                          lo,
                                                                        unsigned
                                                                          int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfUserDefinedConstraintComponents*>(lo)->get(n);
}


/*
 * Get an UserDefinedConstraintComponent_t from the ListOf_t based on its
 * identifier.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
ListOfUserDefinedConstraintComponents_getById(ListOf_t* lo, const char *sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfUserDefinedConstraintComponents*>(lo)->get(sid) : NULL;
}


/*
 * Removes the nth UserDefinedConstraintComponent_t from this ListOf_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
ListOfUserDefinedConstraintComponents_remove(ListOf_t* lo, unsigned int n)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return static_cast <ListOfUserDefinedConstraintComponents*>(lo)->remove(n);
}


/*
 * Removes the UserDefinedConstraintComponent_t from this ListOf_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
UserDefinedConstraintComponent_t*
ListOfUserDefinedConstraintComponents_removeById(ListOf_t* lo,
                                                 const char* sid)
{
  if (lo == NULL)
  {
    return NULL;
  }

  return (sid != NULL) ? static_cast
    <ListOfUserDefinedConstraintComponents*>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


