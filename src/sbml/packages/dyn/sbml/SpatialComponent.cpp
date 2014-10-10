/**
 * @file:   SpatialComponent.cpp
 * @brief:  Implementation of the SpatialComponent class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
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
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/packages/dyn/sbml/SpatialComponent.h>
#include <sbml/packages/dyn/validator/DynSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpatialComponent with the given level, version, and package version.
 */
SpatialComponent::SpatialComponent (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mSpatialIndex (SPATIALKIND_UNKNOWN)
  , mVariable ("")
  , mId ("")
  , mName ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new DynPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new SpatialComponent with the given DynPkgNamespaces object.
 */
SpatialComponent::SpatialComponent (DynPkgNamespaces* dynns)
  : SBase(dynns)
  , mSpatialIndex (SPATIALKIND_UNKNOWN)
  , mVariable ("")
  , mId ("")
  , mName ("")
{
  // set the element namespace of this object
  setElementNamespace(dynns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(dynns);
}


/*
 * Copy constructor for SpatialComponent.
 */
SpatialComponent::SpatialComponent (const SpatialComponent& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mSpatialIndex  = orig.mSpatialIndex;
    mVariable  = orig.mVariable;
    mId  = orig.mId;
    mName  = orig.mName;
  }
}


/*
 * Assignment for SpatialComponent.
 */
SpatialComponent&
SpatialComponent::operator=(const SpatialComponent& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mSpatialIndex  = rhs.mSpatialIndex;
    mVariable  = rhs.mVariable;
    mId  = rhs.mId;
    mName  = rhs.mName;
  }
  return *this;
}


/*
 * Clone for SpatialComponent.
 */
SpatialComponent*
SpatialComponent::clone () const
{
  return new SpatialComponent(*this);
}


/*
 * Destructor for SpatialComponent.
 */
SpatialComponent::~SpatialComponent ()
{
}


/*
 * Returns the value of the "spatialIndex" attribute of this SpatialComponent.
 */
SpatialKind_t
SpatialComponent::getSpatialIndex() const
{
  return mSpatialIndex;
}


/*
 * Returns the value of the "variable" attribute of this SpatialComponent.
 */
const std::string&
SpatialComponent::getVariable() const
{
  return mVariable;
}


/*
 * Returns the value of the "id" attribute of this SpatialComponent.
 */
const std::string&
SpatialComponent::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this SpatialComponent.
 */
const std::string&
SpatialComponent::getName() const
{
  return mName;
}


/*
 * Returns true/false if spatialIndex is set.
 */
bool
SpatialComponent::isSetSpatialIndex() const
{
  return mSpatialIndex != SPATIALKIND_UNKNOWN;
}


/*
 * Returns true/false if variable is set.
 */
bool
SpatialComponent::isSetVariable() const
{
  return (mVariable.empty() == false);
}


/*
 * Returns true/false if id is set.
 */
bool
SpatialComponent::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
SpatialComponent::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets spatialIndex and returns value indicating success.
 */
int
SpatialComponent::setSpatialIndex(SpatialKind_t spatialIndex)
{
  mSpatialIndex = spatialIndex;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets spatialIndex and returns value indicating success.
 */
int
SpatialComponent::setSpatialIndex(const std::string& spatialIndex)
{
  SpatialKind_t parsed = SpatialKind_parse(spatialIndex.c_str());
  if (parsed == SPATIALKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mSpatialIndex = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets variable and returns value indicating success.
 */
int
SpatialComponent::setVariable(const std::string& variable)
{
  if (&(variable) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(variable)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mVariable = variable;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets id and returns value indicating success.
 */
int
SpatialComponent::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
SpatialComponent::setName(const std::string& name)
{
  if (&(name) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets spatialIndex and returns value indicating success.
 */
int
SpatialComponent::unsetSpatialIndex()
{
  mSpatialIndex = SPATIALKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets variable and returns value indicating success.
 */
int
SpatialComponent::unsetVariable()
{
  mVariable.erase();

  if (mVariable.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
SpatialComponent::unsetId()
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
SpatialComponent::unsetName()
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
 * rename attributes that are SIdRefs or instances in math
 */
void
SpatialComponent::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetVariable() == true && mVariable == oldid)
  {
    setVariable(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
SpatialComponent::getElementName () const
{
  static const string name = "spatialComponent";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SpatialComponent::getTypeCode () const
{
  return SBML_DYN_SPATIALCOMPONENT;
}


/*
 * check if all the required attributes are set
 */
bool
SpatialComponent::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetSpatialIndex() == false)
    allPresent = false;

  if (isSetVariable() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SpatialComponent::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
SpatialComponent::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SpatialComponent::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SpatialComponent::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
SpatialComponent::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialIndex");
  attributes.add("variable");
  attributes.add("id");
  attributes.add("name");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpatialComponent::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfSpatialComponents - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfSpatialComponents*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("dyn", DynUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("dyn", DynUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("dyn", DynUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("dyn", DynUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // spatialIndex enum  ( use = "required" )
  //
  mSpatialIndex = SPATIALKIND_UNKNOWN;
  {
    std::string stringValue;
    assigned = attributes.readInto("spatialIndex", stringValue);

    if (assigned == true)
    {
      // parse enum

      mSpatialIndex = SpatialKind_parse(stringValue.c_str());
    }
  }
  if(mSpatialIndex == SPATIALKIND_UNKNOWN)
  {
    std::string message = "Dyn attribute 'spatialIndex' is missing.";
    getErrorLog()->logPackageError("dyn", DynUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // variable SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("variable", mVariable);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mVariable.empty() == true)
    {
      logEmptyString(mVariable, getLevel(), getVersion(), "<SpatialComponent>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mVariable) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute variable='" + mVariable + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Dyn attribute 'variable' is missing.";
    getErrorLog()->logPackageError("dyn", DynUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // id SId  ( use = "optional" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<SpatialComponent>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }

  //
  // name string   ( use = "optional" )
  //
  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    // check string is not empty

    if (mName.empty() == true)
    {
      logEmptyString(mName, getLevel(), getVersion(), "<SpatialComponent>");
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
SpatialComponent::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetSpatialIndex() == true)
    stream.writeAttribute("spatialIndex", getPrefix(), SpatialKind_toString(mSpatialIndex));

  if (isSetVariable() == true)
    stream.writeAttribute("variable", getPrefix(), mVariable);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfSpatialComponents::ListOfSpatialComponents(unsigned int level, 
                          unsigned int version, 
                          unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DynPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfSpatialComponents::ListOfSpatialComponents(DynPkgNamespaces* dynns)
  : ListOf(dynns)
{
  setElementNamespace(dynns->getURI());
}


/*
 * Returns a deep copy of this ListOfSpatialComponents 
 */
ListOfSpatialComponents* 
ListOfSpatialComponents::clone () const
 {
  return new ListOfSpatialComponents(*this);
}


/*
 * Get a SpatialComponent from the ListOfSpatialComponents by index.
*/
SpatialComponent*
ListOfSpatialComponents::get(unsigned int n)
{
  return static_cast<SpatialComponent*>(ListOf::get(n));
}


/*
 * Get a SpatialComponent from the ListOfSpatialComponents by index.
 */
const SpatialComponent*
ListOfSpatialComponents::get(unsigned int n) const
{
  return static_cast<const SpatialComponent*>(ListOf::get(n));
}


/*
 * Get a SpatialComponent from the ListOfSpatialComponents by id.
 */
SpatialComponent*
ListOfSpatialComponents::get(const std::string& sid)
{
	return const_cast<SpatialComponent*>(
    static_cast<const ListOfSpatialComponents&>(*this).get(sid));
}


/*
 * Get a SpatialComponent from the ListOfSpatialComponents by id.
 */
const SpatialComponent*
ListOfSpatialComponents::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpatialComponent>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpatialComponent*> (*result);
}


/**
 * Adds a copy the given "SpatialComponent" to this ListOfSpatialComponents.
 *
 * @param sc; the SpatialComponent object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfSpatialComponents::addSpatialComponent(const SpatialComponent* sc)
{
  if (sc == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sc->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sc->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sc->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(sc)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(sc);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of SpatialComponent objects in this ListOfSpatialComponents.
 *
 * @return the number of SpatialComponent objects in this ListOfSpatialComponents
 */
unsigned int 
ListOfSpatialComponents::getNumSpatialComponents() const
{
	return size();
}

/**
 * Creates a new SpatialComponent object, adds it to this ListOfSpatialComponents
 * SpatialComponent and returns the SpatialComponent object created. 
 *
 * @return a new SpatialComponent object instance
 *
 * @see addSpatialComponent(const SpatialComponent* sc)
 */
SpatialComponent* 
ListOfSpatialComponents::createSpatialComponent()
{
  SpatialComponent* sc = NULL;

  try
  {
    DYN_CREATE_NS(dynns, getSBMLNamespaces());
    sc = new SpatialComponent(dynns);
    delete dynns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(sc != NULL)
  {
    appendAndOwn(sc);
  }

  return sc;
}

/*
 * Removes the nth SpatialComponent from this ListOfSpatialComponents
 */
SpatialComponent*
ListOfSpatialComponents::remove(unsigned int n)
{
  return static_cast<SpatialComponent*>(ListOf::remove(n));
}


/*
 * Removes the SpatialComponent from this ListOfSpatialComponents with the given identifier
 */
SpatialComponent*
ListOfSpatialComponents::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpatialComponent>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <SpatialComponent*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfSpatialComponents::getElementName () const
{
  static const string name = "listOfSpatialComponents";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfSpatialComponents::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfSpatialComponents::getItemTypeCode () const
{
  return SBML_DYN_SPATIALCOMPONENT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SpatialComponent in this ListOfSpatialComponents
 */
SBase*
ListOfSpatialComponents::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "spatialComponent")
  {
    DYN_CREATE_NS(dynns, getSBMLNamespaces());
    object = new SpatialComponent(dynns);
    appendAndOwn(object);
    delete dynns;
  }

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Dyn package.
 */
void
ListOfSpatialComponents::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(DynExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(DynExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
SpatialComponent_t *
SpatialComponent_create(unsigned int level, unsigned int version,
                        unsigned int pkgVersion)
{
  return new SpatialComponent(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SpatialComponent_free(SpatialComponent_t * sc)
{
  if (sc != NULL)
    delete sc;
}


LIBSBML_EXTERN
SpatialComponent_t *
SpatialComponent_clone(SpatialComponent_t * sc)
{
  if (sc != NULL)
  {
    return static_cast<SpatialComponent_t*>(sc->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
SpatialKind_t
SpatialComponent_getSpatialIndex(const SpatialComponent_t * sc)
{
	return (sc != NULL) ? sc->getSpatialIndex() : SPATIALKIND_UNKNOWN;
}


LIBSBML_EXTERN
const char *
SpatialComponent_getVariable(const SpatialComponent_t * sc)
{
	return (sc != NULL && sc->isSetVariable()) ? sc->getVariable().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
SpatialComponent_getId(const SpatialComponent_t * sc)
{
	return (sc != NULL && sc->isSetId()) ? sc->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
SpatialComponent_getName(const SpatialComponent_t * sc)
{
	return (sc != NULL && sc->isSetName()) ? sc->getName().c_str() : NULL;
}


LIBSBML_EXTERN
int
SpatialComponent_isSetSpatialIndex(const SpatialComponent_t * sc)
{
  return (sc != NULL) ? static_cast<int>(sc->isSetSpatialIndex()) : 0;
}


LIBSBML_EXTERN
int
SpatialComponent_isSetVariable(const SpatialComponent_t * sc)
{
  return (sc != NULL) ? static_cast<int>(sc->isSetVariable()) : 0;
}


LIBSBML_EXTERN
int
SpatialComponent_isSetId(const SpatialComponent_t * sc)
{
  return (sc != NULL) ? static_cast<int>(sc->isSetId()) : 0;
}


LIBSBML_EXTERN
int
SpatialComponent_isSetName(const SpatialComponent_t * sc)
{
  return (sc != NULL) ? static_cast<int>(sc->isSetName()) : 0;
}


LIBSBML_EXTERN
int
SpatialComponent_setSpatialIndex(SpatialComponent_t * sc, SpatialKind_t spatialIndex)
{
  if (sc != NULL)
    return sc->setSpatialIndex(spatialIndex);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialComponent_setVariable(SpatialComponent_t * sc, const char * variable)
{
  if (sc != NULL)
    return (variable == NULL) ? sc->setVariable("") : sc->setVariable(variable);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialComponent_setId(SpatialComponent_t * sc, const char * id)
{
  if (sc != NULL)
    return (id == NULL) ? sc->setId("") : sc->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialComponent_setName(SpatialComponent_t * sc, const char * name)
{
  if (sc != NULL)
    return (name == NULL) ? sc->setName("") : sc->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialComponent_unsetSpatialIndex(SpatialComponent_t * sc)
{
  return (sc != NULL) ? sc->unsetSpatialIndex() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialComponent_unsetVariable(SpatialComponent_t * sc)
{
  return (sc != NULL) ? sc->unsetVariable() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialComponent_unsetId(SpatialComponent_t * sc)
{
  return (sc != NULL) ? sc->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialComponent_unsetName(SpatialComponent_t * sc)
{
  return (sc != NULL) ? sc->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialComponent_hasRequiredAttributes(const SpatialComponent_t * sc)
{
  return (sc != NULL) ? static_cast<int>(sc->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
SpatialComponent_t *
ListOfSpatialComponents_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpatialComponents *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
SpatialComponent_t *
ListOfSpatialComponents_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpatialComponents *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


