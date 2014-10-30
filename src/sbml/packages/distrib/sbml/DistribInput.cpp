/**
 * @file:   DistribInput.cpp
 * @brief:  Implementation of the DistribInput class
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


#include <sbml/packages/distrib/sbml/DistribInput.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DistribInput with the given level, version, and package version.
 */
DistribInput::DistribInput (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
   ,mId ("")
   ,mName ("")
   ,mIndex (SBML_INT_MAX)
   ,mIsSetIndex (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new DistribInput with the given DistribPkgNamespaces object.
 */
DistribInput::DistribInput (DistribPkgNamespaces* distribns)
  : SBase(distribns)
   ,mId ("")
   ,mName ("")
   ,mIndex (SBML_INT_MAX)
   ,mIsSetIndex (false)
{
  // set the element namespace of this object
  setElementNamespace(distribns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribInput.
 */
DistribInput::DistribInput (const DistribInput& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mName  = orig.mName;
    mIndex  = orig.mIndex;
    mIsSetIndex  = orig.mIsSetIndex;

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for DistribInput.
 */
DistribInput&
DistribInput::operator=(const DistribInput& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mIndex  = rhs.mIndex;
    mIsSetIndex  = rhs.mIsSetIndex;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for DistribInput.
 */
DistribInput*
DistribInput::clone () const
{
  return new DistribInput(*this);
}


/*
 * Destructor for DistribInput.
 */
DistribInput::~DistribInput ()
{
}


/*
 * Returns the value of the "id" attribute of this DistribInput.
 */
const std::string&
DistribInput::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this DistribInput.
 */
const std::string&
DistribInput::getName() const
{
  return mName;
}


/*
 * Returns the value of the "index" attribute of this DistribInput.
 */
unsigned int
DistribInput::getIndex() const
{
  return mIndex;
}


/*
 * Returns true/false if id is set.
 */
bool
DistribInput::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
DistribInput::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if index is set.
 */
bool
DistribInput::isSetIndex() const
{
  return mIsSetIndex;
}


/*
 * Sets id and returns value indicating success.
 */
int
DistribInput::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
DistribInput::setName(const std::string& name)
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
 * Sets index and returns value indicating success.
 */
int
DistribInput::setIndex(unsigned int index)
{
  mIndex = index;
  mIsSetIndex = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
DistribInput::unsetId()
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
DistribInput::unsetName()
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
 * Unsets index and returns value indicating success.
 */
int
DistribInput::unsetIndex()
{
  mIndex = SBML_INT_MAX;
  mIsSetIndex = false;

  if (isSetIndex() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


List*
DistribInput::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
DistribInput::getElementName () const
{
  static const string name = "distribInput";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
DistribInput::getTypeCode () const
{
  return SBML_DISTRIB_INPUT;
}


/*
 * check if all the required attributes are set
 */
bool
DistribInput::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetIndex() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
DistribInput::hasRequiredElements () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
DistribInput::writeElements (XMLOutputStream& stream) const
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
DistribInput::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */

  v.leave(*this);

  return true;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
DistribInput::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
DistribInput::connectToChild()
{
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
DistribInput::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
DistribInput::createObject(XMLInputStream& stream)
{
  //const string& name = stream.peek().getName();
  SBase* object = NULL;

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  delete distribns;

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
DistribInput::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("index");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
DistribInput::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfDistribInputs - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfDistribInputs*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details);
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
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
    }
  }

  bool assigned = false;

  //
  // id string   ( use = "required" )
  //
  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    // check string is not empty

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<DistribInput>");
    }
  }
  else
  {
    std::string message = "Distrib attribute 'id' is missing.";
    getErrorLog()->logPackageError("distrib", DistribUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message);
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
      logEmptyString(mName, getLevel(), getVersion(), "<DistribInput>");
    }
  }

  //
  // index unsigned int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetIndex = attributes.readInto("index", mIndex);

  if (mIsSetIndex == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
      else
      {
        std::string message = "Distrib attribute 'index' is missing.";
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
DistribInput::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetIndex() == true)
    stream.writeAttribute("index", getPrefix(), mIndex);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfDistribInputs::ListOfDistribInputs(unsigned int level, 
                      unsigned int version, 
                      unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfDistribInputs::ListOfDistribInputs(DistribPkgNamespaces* distribns)
  : ListOf(distribns)
{
  setElementNamespace(distribns->getURI());
}


/*
 * Returns a deep copy of this ListOfDistribInputs 
 */
ListOfDistribInputs* 
ListOfDistribInputs::clone () const
 {
  return new ListOfDistribInputs(*this);
}


/*
 * Get a DistribInput from the ListOfDistribInputs by index.
*/
DistribInput*
ListOfDistribInputs::get(unsigned int n)
{
  return static_cast<DistribInput*>(ListOf::get(n));
}


/*
 * Get a DistribInput from the ListOfDistribInputs by index.
 */
const DistribInput*
ListOfDistribInputs::get(unsigned int n) const
{
  return static_cast<const DistribInput*>(ListOf::get(n));
}


/*
 * Get a DistribInput from the ListOfDistribInputs by id.
 */
DistribInput*
ListOfDistribInputs::get(const std::string& sid)
{
  return const_cast<DistribInput*>(
    static_cast<const ListOfDistribInputs&>(*this).get(sid));
}


/*
 * Get a DistribInput from the ListOfDistribInputs by id.
 */
const DistribInput*
ListOfDistribInputs::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<DistribInput>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <DistribInput*> (*result);
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
 * Removes the nth DistribInput from this ListOfDistribInputs
 */
DistribInput*
ListOfDistribInputs::remove(unsigned int n)
{
  return static_cast<DistribInput*>(ListOf::remove(n));
}


/*
 * Removes the DistribInput from this ListOfDistribInputs with the given identifier
 */
DistribInput*
ListOfDistribInputs::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<DistribInput>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <DistribInput*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfDistribInputs::getElementName () const
{
  static const string name = "listOfDistribInputs";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfDistribInputs::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfDistribInputs::getItemTypeCode () const
{
  return SBML_DISTRIB_INPUT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new DistribInput in this ListOfDistribInputs
 */
SBase*
ListOfDistribInputs::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "distribInput")
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    object = new DistribInput(distribns);
    appendAndOwn(object);
    delete distribns;
  }

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Distrib package.
 */
void
ListOfDistribInputs::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(DistribExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(DistribExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond doxygenLibsbmlInternal */


/*
 * 
 */
LIBSBML_EXTERN
DistribInput_t *
DistribInput_create(unsigned int level, unsigned int version,
                    unsigned int pkgVersion)
{
  return new DistribInput(level, version, pkgVersion);
}


/*
 * 
 */
LIBSBML_EXTERN
void
DistribInput_free(DistribInput_t * di)
{
  if (di != NULL)
    delete di;
}


/*
 *
 */
LIBSBML_EXTERN
DistribInput_t *
DistribInput_clone(DistribInput_t * di)
{
  if (di != NULL)
  {
    return static_cast<DistribInput_t*>(di->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 *
 */
LIBSBML_EXTERN
char *
DistribInput_getId(DistribInput_t * di)
{
  if (di == NULL)
    return NULL;

  return di->getId().empty() ? NULL : safe_strdup(di->getId().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
char *
DistribInput_getName(DistribInput_t * di)
{
  if (di == NULL)
    return NULL;

  return di->getName().empty() ? NULL : safe_strdup(di->getName().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
unsigned int
DistribInput_getIndex(DistribInput_t * di)
{
  return (di != NULL) ? di->getIndex() : SBML_INT_MAX;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_isSetId(DistribInput_t * di)
{
  return (di != NULL) ? static_cast<int>(di->isSetId()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_isSetName(DistribInput_t * di)
{
  return (di != NULL) ? static_cast<int>(di->isSetName()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_isSetIndex(DistribInput_t * di)
{
  return (di != NULL) ? static_cast<int>(di->isSetIndex()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_setId(DistribInput_t * di, const char * id)
{
  return (di != NULL) ? di->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_setName(DistribInput_t * di, const char * name)
{
  return (di != NULL) ? di->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_setIndex(DistribInput_t * di, unsigned int index)
{
  return (di != NULL) ? di->setIndex(index) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_unsetId(DistribInput_t * di)
{
  return (di != NULL) ? di->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_unsetName(DistribInput_t * di)
{
  return (di != NULL) ? di->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_unsetIndex(DistribInput_t * di)
{
  return (di != NULL) ? di->unsetIndex() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
DistribInput_hasRequiredAttributes(DistribInput_t * di)
{
  return (di != NULL) ? static_cast<int>(di->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
DistribInput_t *
ListOfDistribInputs_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDistribInputs *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
DistribInput_t *
ListOfDistribInputs_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDistribInputs *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


