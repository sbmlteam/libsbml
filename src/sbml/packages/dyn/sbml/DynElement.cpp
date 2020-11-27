/**
 * @file:   DynElement.cpp
 * @brief:  Implementation of the DynElement class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
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


#include <sbml/packages/dyn/sbml/DynElement.h>
#include <sbml/packages/dyn/validator/DynSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DynElement with the given level, version, and package version.
 */
DynElement::DynElement (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
////  , mIdRef ("")
////  , mId ("")
////  , mName ("")
  , mMetaIdRef ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new DynPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new DynElement with the given DynPkgNamespaces object.
 */
DynElement::DynElement (DynPkgNamespaces* dynns)
  : SBase(dynns)
////  , mIdRef ("")
////  , mId ("")
////  , mName ("")
  , mMetaIdRef ("")
{
  // set the element namespace of this object
  setElementNamespace(dynns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(dynns);
}


/*
 * Copy constructor for DynElement.
 */
DynElement::DynElement (const DynElement& orig)
  : SBase(orig)
//  , mIdRef  ( orig.mIdRef)
//  , mId  ( orig.mId)
//  , mName  ( orig.mName)
  , mMetaIdRef  ( orig.mMetaIdRef)
{
}


/*
 * Assignment for DynElement.
 */
DynElement&
DynElement::operator=(const DynElement& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mIdRef  = rhs.mIdRef;
    mId  = rhs.mId;
    mName  = rhs.mName;
    mMetaIdRef  = rhs.mMetaIdRef;
  }
  return *this;
}


/*
 * Clone for DynElement.
 */
DynElement*
DynElement::clone () const
{
  return new DynElement(*this);
}


/*
 * Destructor for DynElement.
 */
DynElement::~DynElement ()
{
}


/*
 * Returns the value of the "idRef" attribute of this DynElement.
 */
const std::string&
DynElement::getIdRef() const
{
  return mIdRef;
}


/*
 * Returns the value of the "id" attribute of this DynElement.
 */
const std::string&
DynElement::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this DynElement.
 */
const std::string&
DynElement::getName() const
{
  return mName;
}


/*
 * Returns the value of the "metaIdRef" attribute of this DynElement.
 */
const std::string&
DynElement::getMetaIdRef() const
{
  return mMetaIdRef;
}


/*
 * Returns true/false if idRef is set.
 */
bool
DynElement::isSetIdRef() const
{
  return (mIdRef.empty() == false);
}


/*
 * Returns true/false if id is set.
 */
bool
DynElement::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
DynElement::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if metaIdRef is set.
 */
bool
DynElement::isSetMetaIdRef() const
{
  return (mMetaIdRef.empty() == false);
}


/*
 * Sets idRef and returns value indicating success.
 */
int
DynElement::setIdRef(const std::string& idRef)
{
  if (!(SyntaxChecker::isValidInternalSId(idRef)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mIdRef = idRef;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets id and returns value indicating success.
 */
int
DynElement::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
DynElement::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets metaIdRef and returns value indicating success.
 */
int
DynElement::setMetaIdRef(const std::string& metaIdRef)
{
  if (!SyntaxChecker::isValidXMLID(metaIdRef))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  mMetaIdRef = metaIdRef;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets idRef and returns value indicating success.
 */
int
DynElement::unsetIdRef()
{
  mIdRef.erase();

  if (mIdRef.empty() == true)
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
DynElement::unsetId()
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
DynElement::unsetName()
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
 * Unsets metaIdRef and returns value indicating success.
 */
int
DynElement::unsetMetaIdRef()
{
  mMetaIdRef.erase();

  if (mMetaIdRef.empty() == true)
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
DynElement::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetIdRef() == true && mIdRef == oldid)
  {
    setIdRef(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
DynElement::getElementName () const
{
  static const string name = "dynElement";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
DynElement::getTypeCode () const
{
  return SBML_DYN_ELEMENT;
}


/*
 * check if all the required attributes are set
 */
bool
DynElement::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetIdRef() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
DynElement::writeElements (XMLOutputStream& stream) const
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
DynElement::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
DynElement::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
DynElement::enablePackageInternal(const std::string& pkgURI,
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
DynElement::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("idRef");
  attributes.add("id");
  attributes.add("name");
  attributes.add("metaIdRef");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
DynElement::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfDynElements - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfDynElements*>(getParentSBMLObject())->size() < 2)
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
  // idRef SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("idRef", mIdRef);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mIdRef.empty() == true)
    {
      logEmptyString(mIdRef, getLevel(), getVersion(), "<DynElement>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mIdRef) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute idRef='" + mIdRef + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Dyn attribute 'idRef' is missing.";
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
      logEmptyString(mId, sbmlLevel, sbmlVersion, "<DynElement>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, sbmlLevel, sbmlVersion,
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
      logEmptyString(mName, getLevel(), getVersion(), "<DynElement>");
    }
  }

  //
  // metaIdRef string   ( use = "optional" )
  //
  assigned = attributes.readInto("metaIdRef", mMetaIdRef);

  if (assigned == true)
  {
    // check string is not empty

    if (mMetaIdRef.empty() == true)
    {
      logEmptyString(mMetaIdRef, getLevel(), getVersion(), "<DynElement>");
    }
    else if (SyntaxChecker::isValidXMLID(mMetaIdRef) == false)
    {
      std::string msg = "The metaIdRef attribute on the <" + this->getElementName() + "> ";
      if (this->isSetId()) {
        msg += "with id '" + this->getId() + "' ";
      }
      msg += "is '" + mMetaIdRef + "', which does not conform to the syntax.";
      getErrorLog()->logPackageError("dyn", DynUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, msg, getLine(), getColumn());
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
DynElement::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetIdRef() == true)
    stream.writeAttribute("idRef", getPrefix(), mIdRef);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  if (isSetMetaIdRef() == true)
    stream.writeAttribute("metaIdRef", getPrefix(), mMetaIdRef);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfDynElements::ListOfDynElements(unsigned int level, 
                    unsigned int version, 
                    unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new DynPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfDynElements::ListOfDynElements(DynPkgNamespaces* dynns)
  : ListOf(dynns)
{
  setElementNamespace(dynns->getURI());
}


/*
 * Returns a deep copy of this ListOfDynElements 
 */
ListOfDynElements* 
ListOfDynElements::clone () const
 {
  return new ListOfDynElements(*this);
}


/*
 * Get a DynElement from the ListOfDynElements by index.
*/
DynElement*
ListOfDynElements::get(unsigned int n)
{
  return static_cast<DynElement*>(ListOf::get(n));
}


/*
 * Get a DynElement from the ListOfDynElements by index.
 */
const DynElement*
ListOfDynElements::get(unsigned int n) const
{
  return static_cast<const DynElement*>(ListOf::get(n));
}


/*
 * Get a DynElement from the ListOfDynElements by id.
 */
DynElement*
ListOfDynElements::get(const std::string& sid)
{
	return const_cast<DynElement*>(
    static_cast<const ListOfDynElements&>(*this).get(sid));
}


/*
 * Get a DynElement from the ListOfDynElements by id.
 */
const DynElement*
ListOfDynElements::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<DynElement>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <DynElement*> (*result);
}


/**
 * Adds a copy the given "DynElement" to this ListOfDynElements.
 *
 * @param de; the DynElement object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfDynElements::addDynElement(const DynElement* de)
{
  if (de == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (de->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != de->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != de->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(de)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(de);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of DynElement objects in this ListOfDynElements.
 *
 * @return the number of DynElement objects in this ListOfDynElements
 */
unsigned int 
ListOfDynElements::getNumDynElements() const
{
	return size();
}

/**
 * Creates a new DynElement object, adds it to this ListOfDynElements
 * DynElement and returns the DynElement object created. 
 *
 * @return a new DynElement object instance
 *
 * @see addDynElement(const DynElement* de)
 */
DynElement* 
ListOfDynElements::createDynElement()
{
  DynElement* de = NULL;

  try
  {
    DYN_CREATE_NS(dynns, getSBMLNamespaces());
    de = new DynElement(dynns);
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

  if(de != NULL)
  {
    appendAndOwn(de);
  }

  return de;
}

/*
 * Removes the nth DynElement from this ListOfDynElements
 */
DynElement*
ListOfDynElements::remove(unsigned int n)
{
  return static_cast<DynElement*>(ListOf::remove(n));
}


/*
 * Removes the DynElement from this ListOfDynElements with the given identifier
 */
DynElement*
ListOfDynElements::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<DynElement>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <DynElement*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfDynElements::getElementName () const
{
  static const string name = "listOfDynElements";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfDynElements::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfDynElements::getItemTypeCode () const
{
  return SBML_DYN_ELEMENT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new DynElement in this ListOfDynElements
 */
SBase*
ListOfDynElements::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "dynElement")
  {
    DYN_CREATE_NS(dynns, getSBMLNamespaces());
    object = new DynElement(dynns);
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
ListOfDynElements::writeXMLNS(XMLOutputStream& stream) const
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
DynElement_t *
DynElement_create(unsigned int level, unsigned int version,
                  unsigned int pkgVersion)
{
  return new DynElement(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
DynElement_free(DynElement_t * de)
{
  if (de != NULL)
    delete de;
}


LIBSBML_EXTERN
DynElement_t *
DynElement_clone(DynElement_t * de)
{
  if (de != NULL)
  {
    return static_cast<DynElement_t*>(de->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
DynElement_getIdRef(const DynElement_t * de)
{
	return (de != NULL && de->isSetIdRef()) ? de->getIdRef().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
DynElement_getId(const DynElement_t * de)
{
	return (de != NULL && de->isSetId()) ? de->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
DynElement_getName(const DynElement_t * de)
{
	return (de != NULL && de->isSetName()) ? de->getName().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
DynElement_getMetaIdRef(const DynElement_t * de)
{
	return (de != NULL && de->isSetMetaIdRef()) ? de->getMetaIdRef().c_str() : NULL;
}


LIBSBML_EXTERN
int
DynElement_isSetIdRef(const DynElement_t * de)
{
  return (de != NULL) ? static_cast<int>(de->isSetIdRef()) : 0;
}


LIBSBML_EXTERN
int
DynElement_isSetId(const DynElement_t * de)
{
  return (de != NULL) ? static_cast<int>(de->isSetId()) : 0;
}


LIBSBML_EXTERN
int
DynElement_isSetName(const DynElement_t * de)
{
  return (de != NULL) ? static_cast<int>(de->isSetName()) : 0;
}


LIBSBML_EXTERN
int
DynElement_isSetMetaIdRef(const DynElement_t * de)
{
  return (de != NULL) ? static_cast<int>(de->isSetMetaIdRef()) : 0;
}


LIBSBML_EXTERN
int
DynElement_setIdRef(DynElement_t * de, const char * idRef)
{
  if (de != NULL)
    return (idRef == NULL) ? de->setIdRef("") : de->setIdRef(idRef);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DynElement_setId(DynElement_t * de, const char * id)
{
  if (de != NULL)
    return (id == NULL) ? de->setId("") : de->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DynElement_setName(DynElement_t * de, const char * name)
{
  if (de != NULL)
    return (name == NULL) ? de->setName("") : de->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DynElement_setMetaIdRef(DynElement_t * de, const char * metaIdRef)
{
  if (de != NULL)
    return (metaIdRef == NULL) ? de->setMetaIdRef("") : de->setMetaIdRef(metaIdRef);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DynElement_unsetIdRef(DynElement_t * de)
{
  return (de != NULL) ? de->unsetIdRef() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DynElement_unsetId(DynElement_t * de)
{
  return (de != NULL) ? de->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DynElement_unsetName(DynElement_t * de)
{
  return (de != NULL) ? de->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DynElement_unsetMetaIdRef(DynElement_t * de)
{
  return (de != NULL) ? de->unsetMetaIdRef() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
DynElement_hasRequiredAttributes(const DynElement_t * de)
{
  return (de != NULL) ? static_cast<int>(de->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
DynElement_t *
ListOfDynElements_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDynElements *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
DynElement_t *
ListOfDynElements_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDynElements *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


