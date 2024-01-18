/**
 * @file   Objective.cpp
 * @brief  Implementation of the Objective class
 * @author SBMLTeam
 *
 *<!---------------------------------------------------------------------------
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
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <iostream>
#include <limits>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/fbc/sbml/Objective.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/*
 * Creates a new Objective with the given level, version, and package version.
 */
Objective::Objective (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
//  , mId ("")
//  , mName ("")
  , mType (OBJECTIVE_TYPE_UNKNOWN)
  , mFluxObjectives (level, version, pkgVersion)
  , mTypeString()
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new Objective with the given FbcPkgNamespaces object.
 */
Objective::Objective (FbcPkgNamespaces* fbcns)
  : SBase(fbcns)
//  , mId ("")
//  , mName ("")
  , mType (OBJECTIVE_TYPE_UNKNOWN)
  , mFluxObjectives (fbcns)
  , mTypeString()
{
  // set the element namespace of this object
  setElementNamespace(fbcns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(fbcns);
}


/*
 * Copy constructor for Objective.
 */
Objective::Objective (const Objective& orig)
  : SBase(orig)
  //, mId (orig.mId)
  //, mName(orig.mName)
  , mType(orig.mType)
  , mFluxObjectives(orig.mFluxObjectives)
{
  

  // connect to child objects
  connectToChild();
}


/*
 * Assignment for Objective.
 */
Objective&
Objective::operator=(const Objective& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mType  = rhs.mType;
    mFluxObjectives  = rhs.mFluxObjectives;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for Objective.
 */
Objective*
Objective::clone () const
{
  return new Objective(*this);
}


/*
 * Destructor for Objective.
 */
Objective::~Objective ()
{
}


/*
 * Returns the value of the "id" attribute of this Objective.
 */
const std::string&
Objective::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this Objective.
 */
const std::string&
Objective::getName() const
{
  return mName;
}


/*
 * Returns the value of the "type" attribute of this Objective.
 */
const std::string&
Objective::getType()
{
  if (ObjectiveType_toString(mType) != NULL)
  {
    mTypeString.assign(ObjectiveType_toString(mType));
  }
  else
  {
    mTypeString.assign("");
  }
  return mTypeString;
}

ObjectiveType_t
Objective::getObjectiveType() const
{
  return mType;
}



/*
 * Returns true/false if id is set.
 */
bool
Objective::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
Objective::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if type is set.
 */
bool
Objective::isSetType() const
{
  return mType != OBJECTIVE_TYPE_UNKNOWN;
}


/*
 * Sets id and returns value indicating success.
 */
int
Objective::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
Objective::setName(const std::string& name)
{
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets type and returns value indicating success.
 */
int
Objective::setType(ObjectiveType_t type)
{
  if (ObjectiveType_isValidObjectiveType(type) == 0)
  {
    mType = OBJECTIVE_TYPE_UNKNOWN;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mType = type;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets type and returns value indicating success.
 */
int
Objective::setType(const std::string& type)
{
  return setType(ObjectiveType_fromString(type.c_str()));
}


/*
 * Unsets id and returns value indicating success.
 */
int
Objective::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets name and returns value indicating success.
 */
int
Objective::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets type and returns value indicating success.
 */
int
Objective::unsetType()
{
  mType = OBJECTIVE_TYPE_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the  "ListOfFluxObjectives" in this Objective object.
 */
const ListOfFluxObjectives*
Objective::getListOfFluxObjectives() const
{
  return &mFluxObjectives;
}


/*
 * Returns the  "ListOfFluxObjectives" in this Objective object.
 */
ListOfFluxObjectives*
Objective::getListOfFluxObjectives()
{
  return &mFluxObjectives;
}


/*
 * Removes the nth FluxObjective from the ListOfFluxObjectives.
 */
FluxObjective*
Objective::removeFluxObjective(unsigned int n)
{
  return mFluxObjectives.remove(n);
}


/*
 * Removes the a FluxObjective with given id from the ListOfFluxObjectives.
 */
FluxObjective*
Objective::removeFluxObjective(const std::string& sid)
{
  return mFluxObjectives.remove(sid);
}


/*
 * Return the nth FluxObjective in the ListOfFluxObjectives within this Objective.
 */
FluxObjective*
Objective::getFluxObjective(unsigned int n)
{
  return mFluxObjectives.get(n);
}


/*
 * Return the nth FluxObjective in the ListOfFluxObjectives within this Objective.
 */
const FluxObjective*
Objective::getFluxObjective(unsigned int n) const
{
  return mFluxObjectives.get(n);
}


/*
 * Return a FluxObjective from the ListOfFluxObjectives by id.
 */
FluxObjective*
Objective::getFluxObjective(const std::string& sid)
{
  return mFluxObjectives.get(sid);
}


/*
 * Return a FluxObjective from the ListOfFluxObjectives by id.
 */
const FluxObjective*
Objective::getFluxObjective(const std::string& sid) const
{
  return mFluxObjectives.get(sid);
}


/*
 * Adds a copy the given FluxObjective to this Objective.
 *
 * @param fo the FluxObjective object to add.
 *
 * @copydetails doc_returns_success_code
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
Objective::addFluxObjective(const FluxObjective* fo)
{
  if (fo == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (fo->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != fo->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(fo)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mFluxObjectives.append(fo);
  }
}


/*
 * Get the number of FluxObjective objects in this Objective.
 *
 * @return the number of FluxObjective objects in this Objective
 */
unsigned int
Objective::getNumFluxObjectives() const
{
  return mFluxObjectives.size();
}


/*
 * Creates a new FluxObjective object, adds it to this Objective
 * and returns the FluxObjective object created. 
 *
 * @return a new FluxObjective object instance
 *
 * @see addFluxObjective(const FluxObjective* fo)
 */
FluxObjective*
Objective::createFluxObjective()
{
  FluxObjective* fo = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    fo = new FluxObjective(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(fo != NULL)
  {
    mFluxObjectives.appendAndOwn(fo);
  }

  return fo;
}


List*
Objective::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mFluxObjectives, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
Objective::getElementName () const
{
  static const string name = "objective";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
Objective::getTypeCode () const
{
  return SBML_FBC_OBJECTIVE;
}


/*
 * check if all the required attributes are set
 */
bool
Objective::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetType() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
Objective::hasRequiredElements () const
{
  return getNumFluxObjectives() > 0;
}


  /** @cond doxygenLibsbmlInternal */

/** @cond doxygenLibsbmlInternal */
bool
Objective::getIsSetListOfFluxObjectives() const
{
  return mFluxObjectives.size() != 0;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * write contained elements
 */
void
Objective::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  if (getNumFluxObjectives() > 0)
  {
    mFluxObjectives.write(stream);
  }

  SBase::writeExtensionElements(stream);
}
/** @endcond */


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
Objective::accept (SBMLVisitor& v) const
{
  v.visit(*this);
  for (unsigned int n = 0; n < getNumFluxObjectives(); n++)
  {
    getFluxObjective(n)->accept(v);
  }
  v.leave(*this);

  return true;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
Objective::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  mFluxObjectives.setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
Objective::connectToChild()
{
  SBase::connectToChild();

  mFluxObjectives.connectToParent(this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
Objective::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mFluxObjectives.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Objective.
 */
int
Objective::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Objective.
 */
int
Objective::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Objective.
 */
int
Objective::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Objective.
 */
int
Objective::getAttribute(const std::string& attributeName,
                        unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Objective.
 */
int
Objective::getAttribute(const std::string& attributeName,
                        std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "type")
  {
    value = const_cast<Objective*>(this)->getType();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Objective's attribute "attributeName" is
 * set.
 */
bool
Objective::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "type")
  {
    value = isSetType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Objective.
 */
int
Objective::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Objective.
 */
int
Objective::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Objective.
 */
int
Objective::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Objective.
 */
int
Objective::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Objective.
 */
int
Objective::setAttribute(const std::string& attributeName,
                        const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }
  else if (attributeName == "type")
  {
    return_value = setType(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Objective.
 */
int
Objective::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "type")
  {
    value = unsetType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this Objective.
 */
SBase*
Objective::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "fluxObjective")
  {
    return createFluxObjective();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this Objective.
 */
unsigned int
Objective::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "fluxObjective")
  {
    return getNumFluxObjectives();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this Objective.
 */
SBase*
Objective::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "fluxObjective")
  {
    return getFluxObjective(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
Objective::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mFluxObjectives.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
Objective::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mFluxObjectives.getMetaId() == metaid)
  {
    return &mFluxObjectives;
  }

  obj = mFluxObjectives.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
Objective::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const string& name = stream.peek().getName();

  if (name == "listOfFluxes" || name == "listOfFluxObjectives")
  {
    if (mFluxObjectives.size() != 0)
    {
      getErrorLog()->logPackageError("fbc", FbcObjectiveOneListOfObjectives,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
    }

    object = &mFluxObjectives;
  }
  connectToChild();


  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
Objective::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("type");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
Objective::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();
 
  // look to see whether an unknown attribute error was logged
  // during the read of the listOfFluxBounds - which will have
  // happened immediately prior to this read
  if (getErrorLog() != NULL && 
    static_cast<ListOfObjectives*>(getParentSBMLObject())->size() < 2)
  {
    unsigned int numErrs = getErrorLog()->getNumErrors();
    for (int n = (int)numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = 
          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("fbc", FbcLOObjectivesAllowedAttributes,
          getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      } 
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = 
          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", FbcLOObjectivesAllowedAttributes,
          getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

  SBase::readAttributes(attributes,expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    unsigned int numErrs = getErrorLog()->getNumErrors();
    for (int n = (int)numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = 
          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("fbc", FbcObjectiveRequiredAttributes,
          getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      } 
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = 
          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", FbcObjectiveAllowedL3Attributes,
          getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }


  //
  // Reads an attribute "id" (optional)
  //
  bool assigned = attributes.readInto("id", mId);

  if (assigned)
  {
    // "id" attribute is set to this fbc element

    if (mId.empty())
    {
      //
      // Logs an error if the "id" attribute is empty.
      //
      logEmptyString(mId, sbmlLevel, sbmlVersion, "<fbc>");
    }
    else if (!SyntaxChecker::isValidSBMLSId(mId)) 
    {
      //
      // Logs an error if the "id" attribute doesn't
      // conform to the SBML type SId.
      //
      getErrorLog()->logPackageError("fbc", FbcSBMLSIdSyntax, 
        getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Fbc attribute 'id' is missing.";
    getErrorLog()->logPackageError("fbc", FbcObjectiveRequiredAttributes, 
      getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  attributes.readInto("name", mName);
  
  //
  // type string   ( use = "required" )
  //
  std::string type;
  assigned = attributes.readInto("type", type);

  if (assigned == true)
  {
    // check string is not empty

    if (type.empty() == true)
    {
      logEmptyString(type, sbmlLevel, sbmlVersion, "<Objective>");
    }
    else 
    {
       mType = ObjectiveType_fromString( type.c_str() );
       if (ObjectiveType_isValidObjectiveType((ObjectiveType_t)(int)mType) == 0)
       {
          getErrorLog()->logPackageError("fbc", FbcObjectiveTypeMustBeEnum, 
            getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
       }
    }
  }
  else
  {
    std::string message = "Fbc attribute 'type' is missing.";
    getErrorLog()->logPackageError("fbc", FbcObjectiveRequiredAttributes, 
      getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
Objective::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetType() == true)
    stream.writeAttribute("type", getPrefix(),
    ObjectiveType_toString(mType));

  SBase::writeExtensionAttributes(stream);
}


  /** @endcond */


/*
 * Constructor 
 */
ListOfObjectives::ListOfObjectives(unsigned int level, 
                   unsigned int version, 
                   unsigned int pkgVersion)
 : ListOf(level, version)
 , mActiveObjective()
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion)); 
}

ListOfObjectives::ListOfObjectives(const ListOfObjectives& other)
  : ListOf(other)
  , mActiveObjective(other.mActiveObjective)
{

}

/*
 * Assignment for Objective.
 */
ListOfObjectives&
ListOfObjectives::operator=(const ListOfObjectives& rhs)
{
  if (&rhs != this)
  {
    ListOf::operator=(rhs);
    mActiveObjective = rhs.mActiveObjective;

  }
  return *this;
}

/*
 * Constructor 
 */
ListOfObjectives::ListOfObjectives(FbcPkgNamespaces* fbcns)
  : ListOf(fbcns)
  , mActiveObjective()
{
  setElementNamespace(fbcns->getURI());
}


/*
 * Returns a deep copy of this ListOfObjectives 
 */
ListOfObjectives* 
ListOfObjectives::clone () const
 {
  return new ListOfObjectives(*this);
}


/*
 * Get a Objective from the ListOfObjectives by index.
 */
Objective*
ListOfObjectives::get(unsigned int n)
{
  return static_cast<Objective*>(ListOf::get(n));
}


/*
 * Get a Objective from the ListOfObjectives by index.
 */
const Objective*
ListOfObjectives::get(unsigned int n) const
{
  return static_cast<const Objective*>(ListOf::get(n));
}


/*
 * Get a Objective from the ListOfObjectives by id.
 */
Objective*
ListOfObjectives::get(const std::string& sid)
{
  return const_cast<Objective*>(
    static_cast<const ListOfObjectives&>(*this).get(sid));
}


/*
 * Get a Objective from the ListOfObjectives by id.
 */
const Objective*
ListOfObjectives::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Objective>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <Objective*> (*result);
}


/*
 * Adds a copy the given Objective to this ListOfObjectives.
 *
 * @param o the Objective object to add.
 *
 * @copydetails doc_returns_success_code
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfObjectives::addObjective(const Objective* o)
{
  if (o == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (o->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != o->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(o)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(o);
  }
}


/*
 * Get the number of Objective objects in this ListOfObjectives.
 *
 * @return the number of Objective objects in this ListOfObjectives
 */
unsigned int 
ListOfObjectives::getNumObjectives() const
{
  return size();
}

/*
 * Creates a new Objective object, adds it to this ListOfObjectives
 * and returns the Objective object created. 
 *
 * @return a new Objective object instance
 *
 * @see addObjective(const Objective* o)
 */
Objective* 
ListOfObjectives::createObjective()
{
  Objective* o = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    o = new Objective(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(o != NULL)
  {
    appendAndOwn(o);
  }

  return o;
}

/*
 * Removes the nth Objective from this ListOfObjectives
 */
Objective*
ListOfObjectives::remove(unsigned int n)
{
  return static_cast<Objective*>(ListOf::remove(n));
}


/*
 * Removes the Objective from this ListOfObjectives with the given identifier
 */
Objective*
ListOfObjectives::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Objective>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Objective*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfObjectives::getElementName () const
{
  static const string name = "listOfObjectives";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfObjectives::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfObjectives::getItemTypeCode () const
{
  return SBML_FBC_OBJECTIVE;
}


  /** @cond doxygenLibsbmlInternal */


int ListOfObjectives::appendFrom(const ListOf* list)
{
  int ret = ListOf::appendFrom(list);
  if (ret != LIBSBML_OPERATION_SUCCESS) return ret;

  const ListOfObjectives* objectives = static_cast<const ListOfObjectives*>(list);
  if (objectives == NULL) return LIBSBML_INVALID_OBJECT;

  if (!isSetActiveObjective()) {
    setActiveObjective(objectives->getActiveObjective());
  }
  return ret;
}

void
ListOfObjectives::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (mActiveObjective == oldid) mActiveObjective = newid;
  ListOf::renameSIdRefs(oldid, newid);
}


/*
 * Creates a new Objective in this ListOfObjectives
 */
SBase*
ListOfObjectives::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "objective")
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    object = new Objective(fbcns);
    appendAndOwn(object);
    delete fbcns;
  }

  return object;
}


  /** @endcond */


bool
ListOfObjectives::isSetActiveObjective() const
{
  return !mActiveObjective.empty();
}

int
ListOfObjectives::setActiveObjective(const std::string &activeObjective)
{
  if (!SyntaxChecker::isValidSBMLSId(activeObjective))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  mActiveObjective = activeObjective;
  return LIBSBML_OPERATION_SUCCESS;
}

const std::string &
ListOfObjectives::getActiveObjective() const
{
  return mActiveObjective;
}

int
ListOfObjectives::unsetActiveObjective()
{
  mActiveObjective.erase();
  if (mActiveObjective.empty())
    return LIBSBML_OPERATION_SUCCESS;
  else
    return LIBSBML_OPERATION_FAILED;
}

/** @cond doxygenLibsbmlInternal */
void
ListOfObjectives::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);
  //
  // required attribute is not defined for SBML Level 2 or lesser.
  //
  if (getLevel() > 2)
  {
    attributes.add("activeObjective");
  }
}
/** @endcond */
/** @cond doxygenLibsbmlInternal */
void
ListOfObjectives::readAttributes(const XMLAttributes& attributes,
const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes, expectedAttributes);

  if (getLevel() > 2)
  {
    bool assigned = attributes.readInto("activeObjective", mActiveObjective,
      getErrorLog(), false, getLine(), getColumn());
    if (assigned && mActiveObjective.empty())
    {
      logEmptyString(mActiveObjective, getLevel(), getVersion(),
        "<listOfObjectives>");
    }
    if (!SyntaxChecker::isValidSBMLSId(mActiveObjective))
    {
      getErrorLog()->logPackageError("fbc", FbcActiveObjectiveSyntax,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
    }

  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
ListOfObjectives::writeAttributes(XMLOutputStream& stream) const
{
  //
  // required attribute is not defined for SBML Level 2 .
  //
  if (getLevel() < 3)
    return;

  //cout << "[DEBUG] SBMLDocumentPlugin::writeAttributes() " << endl;
  if (isSetActiveObjective())
  {
    stream.writeAttribute("activeObjective", getPrefix(), mActiveObjective);
  }
}
/** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Fbc package.
 */
void
ListOfObjectives::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(FbcExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(FbcExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond */


/** @cond doxygenLibsbmlInternal */
bool
ListOfObjectives::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}
/** @endcond */

#endif // __cplusplus


LIBSBML_EXTERN
Objective_t *
Objective_create(unsigned int level, unsigned int version,
                 unsigned int pkgVersion)
{
  return new Objective(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
Objective_free(Objective_t * o)
{
  if (o != NULL)
    delete o;
}


LIBSBML_EXTERN
Objective_t *
Objective_clone(Objective_t * o)
{
  if (o != NULL)
  {
    return static_cast<Objective_t*>(o->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
Objective_getId(const Objective_t * o)
{
  return (o != NULL && o->isSetId()) ? o->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
Objective_getName(const Objective_t * o)
{
  return (o != NULL && o->isSetName()) ? o->getName().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
Objective_getType(Objective_t * obj)
{
  if (obj == NULL)
    return NULL;

  return obj->getType().empty() ? "" : obj->getType().c_str();
}


LIBSBML_EXTERN
int
Objective_isSetId(const Objective_t * o)
{
  return (o != NULL) ? static_cast<int>(o->isSetId()) : 0;
}


LIBSBML_EXTERN
int
Objective_isSetName(const Objective_t * o)
{
  return (o != NULL) ? static_cast<int>(o->isSetName()) : 0;
}


LIBSBML_EXTERN
int
Objective_isSetType(const Objective_t * o)
{
  return (o != NULL) ? static_cast<int>(o->isSetType()) : 0;
}


LIBSBML_EXTERN
int
Objective_setId(Objective_t * o, const char * id)
{
  if (o != NULL)
    return (id == NULL) ? o->setId("") : o->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Objective_setName(Objective_t * o, const char * name)
{
  if (o != NULL)
    return (name == NULL) ? o->setName("") : o->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Objective_setType(Objective_t * o, const char* type)
{
  if (o != NULL)
    return o->setType(type);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Objective_unsetId(Objective_t * o)
{
  return (o != NULL) ? o->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Objective_unsetName(Objective_t * o)
{
  return (o != NULL) ? o->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Objective_unsetType(Objective_t * o)
{
  return (o != NULL) ? o->unsetType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Objective_addFluxObjective(Objective_t * o, FluxObjective_t * fo)
{
  return  (o != NULL) ? o->addFluxObjective(fo) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
FluxObjective_t *
Objective_createFluxObjective(Objective_t * o)
{
  return  (o != NULL) ? o->createFluxObjective() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
Objective_getListOfFluxObjectives(Objective_t * o)
{
  return  (o != NULL) ? (ListOf_t *)o->getListOfFluxObjectives() : NULL;
}

LIBSBML_EXTERN
FluxObjective_t *
Objective_getFluxObjective(Objective_t * o, unsigned int n)
{
  return  (o != NULL) ? o->getFluxObjective(n) : NULL;
}

LIBSBML_EXTERN
FluxObjective_t *
Objective_getFluxObjectiveById(Objective_t * o, const char * sid)
{
  return  (o != NULL) ? o->getFluxObjective(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
Objective_getNumFluxObjectives(Objective_t * o)
{
  return  (o != NULL) ? o->getNumFluxObjectives() : SBML_INT_MAX;
}

LIBSBML_EXTERN
FluxObjective_t *
Objective_removeFluxObjective(Objective_t * o, unsigned int n)
{
  return  (o != NULL) ? o->removeFluxObjective(n) : NULL;
}

LIBSBML_EXTERN
FluxObjective_t *
Objective_removeFluxObjectiveById(Objective_t * o, const char * sid)
{
  return  (o != NULL) ? o->removeFluxObjective(sid) : NULL;
}

LIBSBML_EXTERN
int
Objective_hasRequiredAttributes(const Objective_t * o)
{
  return (o != NULL) ? static_cast<int>(o->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
Objective_hasRequiredElements(const Objective_t * o)
{
  return (o != NULL) ? static_cast<int>(o->hasRequiredElements()) : 0;
}


LIBSBML_EXTERN
Objective_t *
ListOfObjectives_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfObjectives *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
Objective_t *
ListOfObjectives_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfObjectives *>(lo)->remove(sid) : NULL;
}


static
const char* OBJECTIVE_TYPE_STRINGS[] =
{
    "maximize"
  , "minimize"
  , "unknown"
};


LIBSBML_EXTERN
const char* 
ObjectiveType_toString(ObjectiveType_t type)
{
  int max = OBJECTIVE_TYPE_UNKNOWN;

  if (type < OBJECTIVE_TYPE_MAXIMIZE || type >= max)
  {
      return NULL;
  }

  return OBJECTIVE_TYPE_STRINGS[type];
}


LIBSBML_EXTERN
ObjectiveType_t 
ObjectiveType_fromString(const char* s)
{
  if (s == NULL) 
  {
    return OBJECTIVE_TYPE_UNKNOWN;
  }

  int max = OBJECTIVE_TYPE_UNKNOWN;
  for (int i = 0; i < max; i++)
  {
    if (strcmp(OBJECTIVE_TYPE_STRINGS[i], s) == 0)
      return (ObjectiveType_t)i;
  }
  return OBJECTIVE_TYPE_UNKNOWN;
}


LIBSBML_EXTERN
int 
ObjectiveType_isValidObjectiveType(ObjectiveType_t effect)
{
  int max = OBJECTIVE_TYPE_UNKNOWN;

  if (effect < OBJECTIVE_TYPE_MAXIMIZE || effect >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}




LIBSBML_EXTERN
int 
ObjectiveType_isValidObjectiveTypeString(const char* s)
{
  return ObjectiveType_isValidObjectiveType
                                         (ObjectiveType_fromString(s));
}

LIBSBML_CPP_NAMESPACE_END


