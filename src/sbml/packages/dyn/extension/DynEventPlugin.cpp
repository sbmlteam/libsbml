/**
 * @file:   DynEventPlugin.cpp
 * @brief:  Implementation of the DynEventPlugin class
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


#include <sbml/packages/dyn/extension/DynEventPlugin.h>
#include <sbml/packages/dyn/validator/DynSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new DynEventPlugin
 */
DynEventPlugin::DynEventPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               DynPkgNamespaces* dynns) :
    SBasePlugin(uri, prefix, dynns)
  , mCboTerm ("")
  , mApplyToAll (false)
  , mIsSetApplyToAll (false)
  , mElements (dynns)
{
}


/*
 * Copy constructor for DynEventPlugin.
 */
DynEventPlugin::DynEventPlugin(const DynEventPlugin& orig) :
    SBasePlugin(orig)
{
    mCboTerm  = orig.mCboTerm;
    mApplyToAll  = orig.mApplyToAll;
    mIsSetApplyToAll  = orig.mIsSetApplyToAll;
    mElements  = orig.mElements;
}


/*
 * Assignment operator for DynEventPlugin.
 */
DynEventPlugin& 
DynEventPlugin::operator=(const DynEventPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mCboTerm  = rhs.mCboTerm;
    mApplyToAll  = rhs.mApplyToAll;
    mIsSetApplyToAll  = rhs.mIsSetApplyToAll;
    mElements  = rhs.mElements;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DynEventPlugin object.
 */
DynEventPlugin* 
DynEventPlugin::clone () const
{
  return new DynEventPlugin(*this);
}


/*
 * Destructor for DynEventPlugin.
 */
DynEventPlugin::~DynEventPlugin()
{
}


//---------------------------------------------------------------
//
// overridden virtual functions for read/write/check
//
//---------------------------------------------------------------

/*
 * create object
 */
SBase*
DynEventPlugin::createObject (XMLInputStream& stream)
{
  SBase* object = NULL; 

  const std::string&      name   = stream.peek().getName(); 
  const XMLNamespaces&    xmlns  = stream.peek().getNamespaces(); 
  const std::string&      prefix = stream.peek().getPrefix(); 

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix) 
  { 
    DYN_CREATE_NS(dynns, getSBMLNamespaces());
    if (name == "listOfElements" ) 
    { 
      object = &mElements;

      if (targetPrefix.empty() == true) 
      { 
        mElements.getSBMLDocument()->enableDefaultNS(mURI, true); 
      } 
    } 

    delete dynns;
  } 

  return object; 
}


/*
 * write elements
 */
void
DynEventPlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumElements() > 0) 
  { 
    mElements.write(stream);
  } 
}


/*
 * Checks if this plugin object has all the required elements.
 */
bool
DynEventPlugin::hasRequiredElements () const
{
  bool allPresent = true; 

  // TO DO 

  return allPresent; 
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
DynEventPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBasePlugin::addExpectedAttributes(attributes);

  attributes.add("cboTerm");
  attributes.add("applyToAll");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
DynEventPlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  SBasePlugin::readAttributes(attributes, expectedAttributes);

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
  // cboTerm string   ( use = "optional" )
  //
  assigned = attributes.readInto("cboTerm", mCboTerm);

  if (assigned == true)
  {
    // check string is not empty

    if (mCboTerm.empty() == true)
    {
      logEmptyString(mCboTerm, getLevel(), getVersion(), getPackageVersion(), "<DynEventPlugin>");
    }
  }

  //
  // applyToAll bool   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetApplyToAll = attributes.readInto("applyToAll", mApplyToAll);

  if (mIsSetApplyToAll == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("dyn", DynUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Dyn attribute 'applyToAll' is missing.";
        getErrorLog()->logPackageError("dyn", DynUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
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
DynEventPlugin::writeAttributes (XMLOutputStream& stream) const
{
  SBasePlugin::writeAttributes(stream);

  if (isSetCboTerm() == true)
    stream.writeAttribute("cboTerm", getPrefix(), mCboTerm);

  if (isSetApplyToAll() == true)
    stream.writeAttribute("applyToAll", getPrefix(), mApplyToAll);

}


  /** @endcond doxygenLibsbmlInternal */


//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------

List*
DynEventPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mElements, filter);

  return ret;
}


/*
 * Returns the value of the "cboTerm" attribute of this DynEventPlugin.
 */
const std::string&
DynEventPlugin::getCboTerm() const
{
  return mCboTerm;
}


/*
 * Returns the value of the "applyToAll" attribute of this DynEventPlugin.
 */
bool
DynEventPlugin::getApplyToAll() const
{
  return mApplyToAll;
}


/*
 * Returns true/false if cboTerm is set.
 */
bool
DynEventPlugin::isSetCboTerm() const
{
  return (mCboTerm.empty() == false);
}


/*
 * Returns true/false if applyToAll is set.
 */
bool
DynEventPlugin::isSetApplyToAll() const
{
  return mIsSetApplyToAll;
}


/*
 * Sets cboTerm and returns value indicating success.
 */
int
DynEventPlugin::setCboTerm(const std::string& cboTerm)
{
  mCboTerm = cboTerm;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets applyToAll and returns value indicating success.
 */
int
DynEventPlugin::setApplyToAll(bool applyToAll)
{
  mApplyToAll = applyToAll;
  mIsSetApplyToAll = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets cboTerm and returns value indicating success.
 */
int
DynEventPlugin::unsetCboTerm()
{
  mCboTerm.erase();

  if (mCboTerm.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets applyToAll and returns value indicating success.
 */
int
DynEventPlugin::unsetApplyToAll()
{
  mApplyToAll = false;
  mIsSetApplyToAll = false;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the  "ListOfDynElements" in this DynEventPlugin object.
 */
const ListOfDynElements*
DynEventPlugin::getListOfElements() const
{
  return &mElements;
}


/*
 * Returns the  "ListOfDynElements" in this DynEventPlugin object.
 */
ListOfDynElements*
DynEventPlugin::getListOfElements()
{
  return &mElements;
}


/*
 * Removes the nth Element from the ListOfDynElements.
 */
DynElement*
DynEventPlugin::removeElement(unsigned int n)
{
	return mElements.remove(n);
}


/*
 * Removes the a Element with given id from the ListOfDynElements.
 */
DynElement*
DynEventPlugin::removeElement(const std::string& sid)
{
	return mElements.remove(sid);
}


/*
 * Return the nth Element in the ListOfDynElements within this DynEventPlugin.
 */
DynElement*
DynEventPlugin::getElement(unsigned int n)
{
	return mElements.get(n);
}


/*
 * Return the nth Element in the ListOfDynElements within this DynEventPlugin.
 */
const DynElement*
DynEventPlugin::getElement(unsigned int n) const
{
	return mElements.get(n);
}


/*
 * Return a Element from the ListOfDynElements by id.
 */
DynElement*
DynEventPlugin::getElement(const std::string& sid)
{
	return mElements.get(sid);
}


/*
 * Return a Element from the ListOfDynElements by id.
 */
const DynElement*
DynEventPlugin::getElement(const std::string& sid) const
{
	return mElements.get(sid);
}


/*
 * Adds a copy the given "DynElement" to this DynEventPlugin.
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
DynEventPlugin::addElement(const DynElement* de)
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
  else
  {
    mElements.append(de);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of DynElement objects in this DynEventPlugin.
 *
 * @return the number of DynElement objects in this DynEventPlugin
 */
unsigned int
DynEventPlugin::getNumElements() const
{
  return mElements.size();
}


/*
 * Creates a new DynElement object, adds it to this DynEventPlugins
 * DynEventPlugin and returns the DynElement object created. 
 *
 * @return a new DynElement object instance
 *
 * @see addDynElement(const DynElement* de)
 */
DynElement*
DynEventPlugin::createElement()
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
    mElements.appendAndOwn(de);
  }

  return de;
}


//---------------------------------------------------------------


/*
 * Set the SBMLDocument.
 */
void
DynEventPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  {
    mElements.setSBMLDocument(d);
  }
}


/*
 * Connect to parent.
 */
void
DynEventPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  if (getNumElements() > 0)
  {
    mElements.connectToParent(sbase);
  }
}


/*
 * Enables the given package.
 */
void
DynEventPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  if (getNumElements() > 0)
  {
    mElements.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}


/*
 * Accept the SBMLVisitor.
 */
bool
DynEventPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  v.visit(*model);
  v.leave(*model);

  for(unsigned int i = 0; i < getNumElements(); i++)
  {
    getElement(i)->accept(v);
  }

  return true;
}




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


