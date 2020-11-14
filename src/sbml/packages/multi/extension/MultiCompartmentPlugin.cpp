/**
 * @file:   MultiCompartmentPlugin.cpp
 * @brief:  Implementation of the MultiCompartmentPlugin class
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


#include <sbml/packages/multi/extension/MultiCompartmentPlugin.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new MultiCompartmentPlugin
 */
MultiCompartmentPlugin::MultiCompartmentPlugin(const std::string& uri,  
                                 const std::string& prefix, 
                               MultiPkgNamespaces* multins) :
    SBasePlugin(uri, prefix, multins)
  , mListOfCompartmentReferences (multins)
   ,mCompartmentType ("")
   ,mIsType (false)
   ,mIsSetIsType (false)
{
}


/*
 * Copy constructor for MultiCompartmentPlugin.
 */
MultiCompartmentPlugin::MultiCompartmentPlugin(const MultiCompartmentPlugin& orig) :
    SBasePlugin(orig)
  , mListOfCompartmentReferences ( orig.mListOfCompartmentReferences)
  , mCompartmentType  ( orig.mCompartmentType)
  , mIsType  ( orig.mIsType)
  , mIsSetIsType  ( orig.mIsSetIsType)
{
}


/*
 * Assignment operator for MultiCompartmentPlugin.
 */
MultiCompartmentPlugin& 
MultiCompartmentPlugin::operator=(const MultiCompartmentPlugin& rhs)
{
  if (&rhs != this)
  {
    this->SBasePlugin::operator=(rhs);
    mListOfCompartmentReferences = rhs.mListOfCompartmentReferences;
    mCompartmentType  = rhs.mCompartmentType;
    mIsType  = rhs.mIsType;
    mIsSetIsType  = rhs.mIsSetIsType;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this MultiCompartmentPlugin object.
 */
MultiCompartmentPlugin* 
MultiCompartmentPlugin::clone () const
{
  return new MultiCompartmentPlugin(*this);
}


/*
 * Destructor for MultiCompartmentPlugin.
 */
MultiCompartmentPlugin::~MultiCompartmentPlugin()
{
}


//---------------------------------------------------------------
//
// overridden virtual functions for read/write/check
//
//---------------------------------------------------------------

/** @cond doxygenLibsbmlInternal */
/*
 * create object
 */
SBase*
MultiCompartmentPlugin::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  std::string prefix(stream.peek().getPrefix());

  const std::string& targetPrefix =
      (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix)
    {
      MULTI_CREATE_NS(multins, getSBMLNamespaces());
      if (!targetPrefix.empty())
        {
          prefix += ":";
        }

      if (name == "listOfCompartmentReferences")
        {
          if (mListOfCompartmentReferences.size() > 0)
            {
              getErrorLog()->logPackageError("multi", MultiLofCpaRefs_OnlyOne,
                  getPackageVersion(), getLevel(), getVersion(),
                  "Extended <compartment> may only have one <" + prefix
                      + "listOfCompartmentReferences>", getLine(),
                  getColumn());
            }
          else
            {
              object = &mListOfCompartmentReferences;

              if (targetPrefix.empty() == true)
                {
                  mListOfCompartmentReferences.getSBMLDocument()->enableDefaultNS(
                      mURI, true);
                }
            }
        }
      delete multins;
    }

  return object;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * write elements
 */
void
MultiCompartmentPlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumCompartmentReferences() > 0) 
  { 
    mListOfCompartmentReferences.write(stream);
  } 
}
/** @endcond */


/*
 * Checks if this plugin object has all the required elements.
 */
bool
MultiCompartmentPlugin::hasRequiredElements () const
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
MultiCompartmentPlugin::addExpectedAttributes(ExpectedAttributes& attributes)
{
  attributes.add("compartmentType");
  attributes.add("isType");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
MultiCompartmentPlugin::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

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
        getErrorLog()->logPackageError("multi", MultiUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                       getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // compartmentType SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("compartmentType", mCompartmentType);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mCompartmentType.empty() == true)
    {
      logEmptyString(mCompartmentType, getLevel(), getVersion(), 
        getPackageVersion(), "<MultiCompartmentPlugin>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mCompartmentType) == false && getErrorLog() != NULL)
    {
      std::string details = "The syntax of the attribute compartmentType='" + mCompartmentType + "' does not conform.";
      getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                 getPackageVersion(), sbmlLevel, sbmlVersion, details,
                 getLine(), getColumn());
    }
  }

  //
  // isType bool   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetIsType = attributes.readInto("isType", mIsType);

  if (mIsSetIsType == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("multi", MultiExCpa_IsTypeAtt_Invalid,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "",
                     getLine(), getColumn());
      }
      else
      {
        std::string message = "Multi attribute 'isType' is missing.";
        getErrorLog()->logPackageError("multi", MultiExCpa_IsTypeAtt_Required,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message,
                       getLine(), getColumn());
      }
    }
  }

  // check if there is any attribute not expected
  //
  // check that all attributes of this plugin object are expected
  //
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::string name = attributes.getName(i);
    std::string uri  = attributes.getURI(i);

    if (uri != mURI) continue;

    if (!expectedAttributes.hasAttribute(name))
    {
        std::string message = " The attribute '" + name + "' is not an expected attribute in the multi package.";
        getErrorLog()->logPackageError("multi", MultiExCpa_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message,
                       getLine(), getColumn());
    }
  }


}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
MultiCompartmentPlugin::writeAttributes (XMLOutputStream& stream) const
{
  if (isSetCompartmentType() == true)
    stream.writeAttribute("compartmentType", getPrefix(), mCompartmentType);

  if (isSetIsType() == true)
    stream.writeAttribute("isType", getPrefix(), mIsType);
}


  /** @endcond */



//---------------------------------------------------------------
//
// Functions for interacting with the members of the plugin
//
//---------------------------------------------------------------


/*
 * Returns the value of the "compartmentType" attribute of this MultiCompartmentPlugin.
 */
const std::string&
MultiCompartmentPlugin::getCompartmentType() const
{
  return mCompartmentType;
}


/*
 * Returns the value of the "isType" attribute of this MultiCompartmentPlugin.
 */
bool
MultiCompartmentPlugin::getIsType() const
{
  return mIsType;
}


/*
 * Returns true/false if compartmentType is set.
 */
bool
MultiCompartmentPlugin::isSetCompartmentType() const
{
  return (mCompartmentType.empty() == false);
}


/*
 * Returns true/false if isType is set.
 */
bool
MultiCompartmentPlugin::isSetIsType() const
{
  return mIsSetIsType;
}


/*
 * Sets compartmentType and returns value indicating success.
 */
int
MultiCompartmentPlugin::setCompartmentType(const std::string& compartmentType)
{
  if (!(SyntaxChecker::isValidInternalSId(compartmentType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCompartmentType = compartmentType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets isType and returns value indicating success.
 */
int
MultiCompartmentPlugin::setIsType(bool isType)
{
  mIsType = isType;
  mIsSetIsType = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets compartmentType and returns value indicating success.
 */
int
MultiCompartmentPlugin::unsetCompartmentType()
{
  mCompartmentType.erase();

  if (mCompartmentType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets isType and returns value indicating success.
 */
int
MultiCompartmentPlugin::unsetIsType()
{
  mIsType = false;
  mIsSetIsType = false;
  return LIBSBML_OPERATION_SUCCESS;
}



List*
MultiCompartmentPlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mListOfCompartmentReferences, filter);

  return ret;
}


/*
 * Returns the ListOfCompartmentReferences in this plugin object.
 */
const ListOfCompartmentReferences* 
MultiCompartmentPlugin::getListOfCompartmentReferences () const
{
  return &this->mListOfCompartmentReferences;
}


/*
 * Returns the ListOfCompartmentReferences in this plugin object.
 */
ListOfCompartmentReferences* 
MultiCompartmentPlugin::getListOfCompartmentReferences ()
{
  return &this->mListOfCompartmentReferences;
}


/*
 * Returns the CompartmentReference object that belongs to the given index.
 */
const CompartmentReference*
MultiCompartmentPlugin::getCompartmentReference(unsigned int n) const
{
  return static_cast<const CompartmentReference*>(mListOfCompartmentReferences.get(n));
}


/*
 * Returns the CompartmentReference object that belongs to the given index.
 */
CompartmentReference*
MultiCompartmentPlugin::getCompartmentReference(unsigned int n)
{
  return static_cast<CompartmentReference*>(mListOfCompartmentReferences.get(n));
}


/*
 * Returns the CompartmentReference object based on its identifier.
 */
const CompartmentReference*
MultiCompartmentPlugin::getCompartmentReference(const std::string& sid) const
{
  return static_cast<const CompartmentReference*>(mListOfCompartmentReferences.get(sid));
}


/*
 * Returns the CompartmentReference object based on its identifier.
 */
CompartmentReference*
MultiCompartmentPlugin::getCompartmentReference(const std::string& sid)
{
  return static_cast<CompartmentReference*>(mListOfCompartmentReferences.get(sid));
}


/*
 * Adds a copy of the given CompartmentReference to the ListOfCompartmentReferences in this plugin object.
 */
int
MultiCompartmentPlugin::addCompartmentReference (const CompartmentReference* compartmentReference)
{
  if (compartmentReference == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (compartmentReference->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != compartmentReference->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != compartmentReference->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != compartmentReference->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    mListOfCompartmentReferences.append(compartmentReference);
  }

  return LIBSBML_OPERATION_SUCCESS;

}


/*
 * Creates a new CompartmentReference object and adds it to the ListOfCompartmentReferences in this plugin object.
 */
CompartmentReference* 
MultiCompartmentPlugin::createCompartmentReference ()
{
   CompartmentReference* cr = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    cr = new CompartmentReference(multins);
    delete multins;
  }
  catch(...)
  {
  }

  if (cr != NULL)
  {
    mListOfCompartmentReferences.appendAndOwn(cr);
  }

  return cr;
}


/*
 * Removes the nth CompartmentReference object from this plugin object
 */
CompartmentReference* 
MultiCompartmentPlugin::removeCompartmentReference(unsigned int n)
{
  return static_cast<CompartmentReference*>(mListOfCompartmentReferences.remove(n));
}


/*
 * Removes the CompartmentReference object with the given id from this plugin object
 */
CompartmentReference* 
MultiCompartmentPlugin::removeCompartmentReference(const std::string& sid)
{
  return static_cast<CompartmentReference*>(mListOfCompartmentReferences.remove(sid));
}


/*
 * Returns the number of CompartmentReference objects in this plugin object.
 */
unsigned int 
MultiCompartmentPlugin::getNumCompartmentReferences () const
{
  return mListOfCompartmentReferences.size();
}


//---------------------------------------------------------------


/** @cond doxygenLibsbmlInternal */
/*
 * Set the SBMLDocument.
 */
void
MultiCompartmentPlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mListOfCompartmentReferences.setSBMLDocument(d);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Connect to parent.
 */
void
MultiCompartmentPlugin::connectToParent(SBase* sbase)
{
  SBasePlugin::connectToParent(sbase);

  mListOfCompartmentReferences.connectToParent(sbase);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Enables the given package.
 */
void
MultiCompartmentPlugin::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix, bool flag)
{
  mListOfCompartmentReferences.enablePackageInternal(pkgURI, pkgPrefix, flag);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Accept the SBMLVisitor.
 */
bool
MultiCompartmentPlugin::accept(SBMLVisitor& v) const
{
  const Compartment * compartment = static_cast<const Compartment * >(this->getParentSBMLObject());
  v.visit(*compartment);

  for(unsigned int i = 0; i < getNumCompartmentReferences(); i++)
  {
    getCompartmentReference(i)->accept(v);
  }

  return true;
}
/** @endcond */




#endif /* __cplusplus */


/*
 * Returns a ListOf_t * containing CompartmentReference_t objects from this
 * MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
ListOf_t*
MultiCompartmentPlugin_getListOfCompartmentReferences(MultiCompartmentPlugin_t*
  mcp)
{
  return (mcp != NULL) ? mcp->getListOfCompartmentReferences() : NULL;
}


/*
 * Get a CompartmentReference_t from the MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
CompartmentReference_t*
MultiCompartmentPlugin_getCompartmentReference(MultiCompartmentPlugin_t* mcp,
                                               unsigned int n)
{
  return (mcp != NULL) ? mcp->getCompartmentReference(n) : NULL;
}


/*
 * Get a CompartmentReference_t from the MultiCompartmentPlugin_t based on its
 * identifier.
 */
LIBSBML_EXTERN
CompartmentReference_t*
MultiCompartmentPlugin_getCompartmentReferenceById(
                                                   MultiCompartmentPlugin_t*
                                                     mcp,
                                                   const char *sid)
{
  return (mcp != NULL && sid != NULL) ? mcp->getCompartmentReference(sid) :
    NULL;
}


/*
 * Adds a copy of the given CompartmentReference_t to this
 * MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
int
MultiCompartmentPlugin_addCompartmentReference(MultiCompartmentPlugin_t* mcp,
                                               const CompartmentReference_t*
                                                 cr)
{
  return (mcp != NULL) ? mcp->addCompartmentReference(cr) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of CompartmentReference_t objects in this
 * MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
unsigned int
MultiCompartmentPlugin_getNumCompartmentReferences(MultiCompartmentPlugin_t*
  mcp)
{
  return (mcp != NULL) ? mcp->getNumCompartmentReferences() : SBML_INT_MAX;
}


/*
 * Creates a new CompartmentReference_t object, adds it to this
 * MultiCompartmentPlugin_t object and returns the CompartmentReference_t
 * object created.
 */
LIBSBML_EXTERN
CompartmentReference_t*
MultiCompartmentPlugin_createCompartmentReference(MultiCompartmentPlugin_t*
  mcp)
{
  return (mcp != NULL) ? mcp->createCompartmentReference() : NULL;
}


/*
 * Removes the nth CompartmentReference_t from this MultiCompartmentPlugin_t
 * and returns a pointer to it.
 */
LIBSBML_EXTERN
CompartmentReference_t*
MultiCompartmentPlugin_removeCompartmentReference(
                                                  MultiCompartmentPlugin_t*
                                                    mcp,
                                                  unsigned int n)
{
  return (mcp != NULL) ? mcp->removeCompartmentReference(n) : NULL;
}


/*
 * Removes the CompartmentReference_t from this MultiCompartmentPlugin_t based
 * on its identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
CompartmentReference_t*
MultiCompartmentPlugin_removeCompartmentReferenceById(
                                                      MultiCompartmentPlugin_t*
                                                        mcp,
                                                      const char* sid)
{
  return (mcp != NULL && sid != NULL) ? mcp->removeCompartmentReference(sid) :
    NULL;
}


/*
 * Returns the value of the "compartmentType" attribute of this
 * MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
char *
MultiCompartmentPlugin_getCompartmentType(const MultiCompartmentPlugin_t * mcp)
{
  if (mcp == NULL)
  {
    return NULL;
  }

  return mcp->getCompartmentType().empty() ? NULL :
    safe_strdup(mcp->getCompartmentType().c_str());
}


/*
 * Returns the value of the "isType" attribute of this
 * MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
int
MultiCompartmentPlugin_getIsType(const MultiCompartmentPlugin_t * mcp)
{
  return (mcp != NULL) ? static_cast<int>(mcp->getIsType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this MultiCompartmentPlugin_t's
 * "compartmentType" attribute is set.
 */
LIBSBML_EXTERN
int
MultiCompartmentPlugin_isSetCompartmentType(const MultiCompartmentPlugin_t *
  mcp)
{
  return (mcp != NULL) ? static_cast<int>(mcp->isSetCompartmentType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this MultiCompartmentPlugin_t's "isType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
MultiCompartmentPlugin_isSetIsType(const MultiCompartmentPlugin_t * mcp)
{
  return (mcp != NULL) ? static_cast<int>(mcp->isSetIsType()) : 0;
}


/*
 * Sets the value of the "compartmentType" attribute of this
 * MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
int
MultiCompartmentPlugin_setCompartmentType(MultiCompartmentPlugin_t * mcp,
                                          const char * compartmentType)
{
  return (mcp != NULL) ? mcp->setCompartmentType(compartmentType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "isType" attribute of this MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
int
MultiCompartmentPlugin_setIsType(MultiCompartmentPlugin_t * mcp, int isType)
{
  return (mcp != NULL) ? mcp->setIsType(isType) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "compartmentType" attribute of this
 * MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
int
MultiCompartmentPlugin_unsetCompartmentType(MultiCompartmentPlugin_t * mcp)
{
  return (mcp != NULL) ? mcp->unsetCompartmentType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "isType" attribute of this MultiCompartmentPlugin_t.
 */
LIBSBML_EXTERN
int
MultiCompartmentPlugin_unsetIsType(MultiCompartmentPlugin_t * mcp)
{
  return (mcp != NULL) ? mcp->unsetIsType() : LIBSBML_INVALID_OBJECT;
}




LIBSBML_CPP_NAMESPACE_END

