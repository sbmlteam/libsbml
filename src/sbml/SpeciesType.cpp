/**
 * @file    SpeciesType.cpp
 * @brief   Implementation of SpeciesType and ListOfSpeciesTypes.
 * @author  Ben Bornstein
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

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/SpeciesType.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

SpeciesType::SpeciesType (unsigned int level, unsigned int version) :
   SBase ( level, version )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


SpeciesType::SpeciesType (SBMLNamespaces * sbmlns) :
   SBase ( sbmlns )
{
  if (!hasValidLevelVersionNamespaceCombination())
  {
    throw SBMLConstructorException(getElementName(), sbmlns);
  }

  loadPlugins(sbmlns);
}


/*
 * Destroys this SpeciesType.
 */
SpeciesType::~SpeciesType ()
{
}


/*
 * Copy constructor. Creates a copy of this SpeciesType.
 */
SpeciesType::SpeciesType(const SpeciesType& orig)
  : SBase             ( orig )
{
}


/*
 * Assignment operator
 */
SpeciesType& SpeciesType::operator=(const SpeciesType& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
  }

  return *this;
}


/** @cond doxygenLibsbmlInternal */
bool
SpeciesType::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}
/** @endcond */


/*
 * @return a (deep) copy of this SpeciesType.
 */
SpeciesType*
SpeciesType::clone () const
{
  return new SpeciesType(*this);
}


/*
 * @return the id of this SBML object.
 */
const string&
SpeciesType::getId () const
{
  return mId;
}


/*
 * @return the name of this SBML object.
 */
const string&
SpeciesType::getName () const
{
  return (getLevel() == 1) ? mId : mName;
}


/*
 * @return @c true if the id of this SBML object is set, false
 * otherwise.
 */
bool
SpeciesType::isSetId () const
{
  return (mId.empty() == false);
}


/*
 * @return @c true if the name of this SBML object is set, false
 * otherwise.
 */
bool
SpeciesType::isSetName () const
{
  return (getLevel() == 1) ? (mId.empty() == false) : 
                            (mName.empty() == false);
}


/*
 * Sets the id of this SBML object to a copy of @p sid.
 */
int
SpeciesType::setId (const std::string& sid)
{
  /* since the setId function has been used as an
   * alias for setName we cant require it to only
   * be used on a L2 model
   */
/*  if (getLevel() == 1)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
*/
  if (!(SyntaxChecker::isValidInternalSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mId = sid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the name of this SBML object to a copy of name.
 */
int
SpeciesType::setName (const std::string& name)
{
  /* if this is setting an L2 name the type is string
   * whereas if it is setting an L1 name its type is SId
   */
  if (getLevel() == 1)
  {
    if (!(SyntaxChecker::isValidInternalSId(name)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mId = name;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the name of this SBML object.
 */
int
SpeciesType::unsetName ()
{
  if (getLevel() == 1) 
  {
    mId.erase();
  }
  else 
  {
    mName.erase();
  }

  if (getLevel() == 1 && mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (mName.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
SpeciesType::getTypeCode () const
{
  return SBML_SPECIES_TYPE;
}


/*
 * @return the name of this element ie "speciesType".
 */
const string&
SpeciesType::getElementName () const
{
  static const string name = "speciesType";
  return name;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parent's
 * implementation of this method as well.
 */
void
SpeciesType::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}
/** @endcond */


bool 
SpeciesType::hasRequiredAttributes() const
{
  bool allPresent = true;

  /* required attributes for speciesType: id */

  if (!isSetId())
    allPresent = false;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::getAttribute(const std::string& attributeName,
                          std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpeciesType.
 */
//int
//SpeciesType::getAttribute(const std::string& attributeName,
//                          const char* value) const
//{
//  int return_value = SBase::getAttribute(attributeName, value);
//
//  return return_value;
//}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SpeciesType's attribute "attributeName"
 * is set.
 */
bool
SpeciesType::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::setAttribute(const std::string& attributeName,
                          const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpeciesType.
 */
//int
//SpeciesType::setAttribute(const std::string& attributeName, const char* value)
//{
//  int return_value = SBase::setAttribute(attributeName, value);
//
//  return return_value;
//}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this SpeciesType.
 */
int
SpeciesType::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  return value;
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */
/**
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
SpeciesType::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("name");
  attributes.add("id");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
SpeciesType::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  SBase::readAttributes(attributes, expectedAttributes);

  switch (level)
  {
  case 1:
    logError(NotSchemaConformant, level, version,
	      "SpeciesType is not a valid component for this level/version.");
    break;
  case 2:
    if (version == 1)
    {
      logError(NotSchemaConformant, level, version,
	        "SpeciesType is not a valid component for this level/version.");
    }
    else
    {
      readL2Attributes(attributes);
    }
    break;
  case 3:
  default:
    logError(NotSchemaConformant, level, version,
	      "SpeciesType is not a valid component for this level/version.");
    break;
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
SpeciesType::readL2Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // id: SId  { use="required" }  (L2v2 ->)
  //
  bool assigned = attributes.readInto("id", mId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<speciesType>");
  }
  if (!SyntaxChecker::isValidInternalSId(mId)) 
    logError(InvalidIdSyntax, level, version, "The id '" + mId + "' does not conform to the syntax.");

  //
  // name: string  { use="optional" }  (L2v2 ->)
  //
  attributes.readInto("name", mName, getErrorLog(), false, getLine(), getColumn());
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parent's implementation
 * of this method as well.
 */
void
SpeciesType::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  /* invalid level/version */
  if (level < 2 || (level == 2 && version == 1))
  {
    return;
  }

  //
  // id: SId  { use="required" }  (L2v2 ->)
  //
  stream.writeAttribute("id", mId);

  //
  // name: string  { use="optional" }  (L2v2 ->)
  //
  stream.writeAttribute("name", mName);
  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3 ->)
  // is written in SBase::writeAttributes()
  //

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}
/** @endcond */


/*
 * Creates a new ListOfSpeciesTypes items.
 */
ListOfSpeciesTypes::ListOfSpeciesTypes (unsigned int level, unsigned int version)
  : ListOf(level,version)
{
}


/*
 * Creates a new ListOfSpeciesTypes items.
 */
ListOfSpeciesTypes::ListOfSpeciesTypes (SBMLNamespaces* sbmlns)
  : ListOf(sbmlns)
{
  loadPlugins(sbmlns);
}


/*
 * @return a (deep) copy of this ListOfSpeciesTypes.
 */
ListOfSpeciesTypes*
ListOfSpeciesTypes::clone () const
{
  return new ListOfSpeciesTypes(*this);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfSpeciesTypes::getItemTypeCode () const
{
  return SBML_SPECIES_TYPE;
}


/*
 * @return the name of this element ie "listOfSpeciesTypes".
 */
const string&
ListOfSpeciesTypes::getElementName () const
{
  static const string name = "listOfSpeciesTypes";
  return name;
}


/* return nth item in list */
SpeciesType *
ListOfSpeciesTypes::get(unsigned int n)
{
  return static_cast<SpeciesType*>(ListOf::get(n));
}


/* return nth item in list */
const SpeciesType *
ListOfSpeciesTypes::get(unsigned int n) const
{
  return static_cast<const SpeciesType*>(ListOf::get(n));
}


/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqST : public unary_function<SBase*, bool>
{
  const string& mId;

  IdEqST (const string& id) : mId(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <SpeciesType *> (sb)->getId() == mId; }
};


/* return item by id */
SpeciesType*
ListOfSpeciesTypes::get (const std::string& sid)
{
  return const_cast<SpeciesType*>( 
    static_cast<const ListOfSpeciesTypes&>(*this).get(sid) );
}


/* return item by id */
const SpeciesType*
ListOfSpeciesTypes::get (const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqST(sid) );
  return (result == mItems.end()) ? NULL : 
                             static_cast <SpeciesType*> (*result);
}


/* Removes the nth item from this list */
SpeciesType*
ListOfSpeciesTypes::remove (unsigned int n)
{
   return static_cast<SpeciesType*>(ListOf::remove(n));
}


/* Removes item in this list by id */
SpeciesType*
ListOfSpeciesTypes::remove (const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqST(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SpeciesType*> (item);
}


/** @cond doxygenLibsbmlInternal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfSpeciesTypes::getElementPosition () const
{
  return 4;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or @c NULL if the token was not recognized.
 */
SBase*
ListOfSpeciesTypes::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = NULL;


  if (name == "speciesType")
  {
    try
    {
      object = new SpeciesType(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      // species type does not exist in L3, hence we fall back
      object = new SpeciesType(2,
        4);
    }
    catch ( ... )
    {
      // species type does not exist in L3, hence we fall back 
      object = new SpeciesType(2,4);
    }
    
    if (object != NULL) mItems.push_back(object);
  }

  return object;
}
/** @endcond */



#endif /* __cplusplus */
/** @cond doxygenIgnored */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_create (unsigned int level, unsigned int version)
{
  try
  {
    SpeciesType* obj = new SpeciesType(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    SpeciesType* obj = new SpeciesType(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


LIBSBML_EXTERN
void
SpeciesType_free (SpeciesType_t *st)
{
  if (st != NULL)
  delete st;
}


LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_clone (const SpeciesType_t *st)
{
  return (st != NULL) ? static_cast<SpeciesType*>( st->clone() ) : NULL;
}


LIBSBML_EXTERN
const XMLNamespaces_t *
SpeciesType_getNamespaces(SpeciesType_t *st)
{
  return (st != NULL) ? st->getNamespaces() : NULL;
}


LIBSBML_EXTERN
const char *
SpeciesType_getId (const SpeciesType_t *st)
{
  return (st != NULL && st->isSetId()) ? st->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
SpeciesType_getName (const SpeciesType_t *st)
{
  return (st != NULL && st->isSetName()) ? st->getName().c_str() : NULL;
}


LIBSBML_EXTERN
int
SpeciesType_isSetId (const SpeciesType_t *st)
{
  return (st != NULL) ? static_cast<int>( st->isSetId() ) : 0;
}


LIBSBML_EXTERN
int
SpeciesType_isSetName (const SpeciesType_t *st)
{
  return (st != NULL) ? static_cast<int>( st->isSetName() ) : 0;
}


LIBSBML_EXTERN
int
SpeciesType_setId (SpeciesType_t *st, const char *sid)
{
  if (st != NULL)
    return (sid == NULL) ? st->setId("") : st->setId(sid);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesType_setName (SpeciesType_t *st, const char *name)
{
  if (st != NULL)
    return (name == NULL) ? st->unsetName() : st->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpeciesType_unsetName (SpeciesType_t *st)
{
  return (st != NULL) ? st->unsetName() : LIBSBML_INVALID_OBJECT;
}



LIBSBML_EXTERN
SpeciesType_t *
ListOfSpeciesTypes_getById (ListOf_t *lo, const char *sid)
{
  if (lo != NULL)
    return (sid != NULL) ? 
      static_cast <ListOfSpeciesTypes *> (lo)->get(sid) : NULL;
  else
    return NULL;
}


LIBSBML_EXTERN
SpeciesType_t *
ListOfSpeciesTypes_removeById (ListOf_t *lo, const char *sid)
{
  if (lo != NULL)
    return (sid != NULL) ? 
      static_cast <ListOfSpeciesTypes *> (lo)->remove(sid) : NULL;
  else
    return NULL;
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END
