/**
 * @file:   SubListOfSpeciesFeatures.cpp
 * @brief:  Implementation of the SubListOfSpeciesFeatures class
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


#include <sbml/packages/multi/sbml/SubListOfSpeciesFeatures.h>
#include <sbml/packages/multi/sbml/SpeciesFeature.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include <sbml/util/ElementFilter.h>
#include <sbml/SBase.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
SubListOfSpeciesFeatures::SubListOfSpeciesFeatures(unsigned int level,
                        unsigned int version,
                        unsigned int pkgVersion)
 : ListOf(level, version)
////  ,mId("")
  ,mRelation (MULTI_RELATION_UNKNOWN)
  ,mComponent("")
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));

  connectToChild();
}


/*
 * Constructor
 */
SubListOfSpeciesFeatures::SubListOfSpeciesFeatures(MultiPkgNamespaces* multins)
  : ListOf(multins)
////  ,mId("")
  ,mRelation (MULTI_RELATION_UNKNOWN )
  ,mComponent("")
{
  setElementNamespace(multins->getURI());
  connectToChild();
}

/*
 * Copy constructor for SubListOfSpeciesFeatures.
 */
SubListOfSpeciesFeatures::SubListOfSpeciesFeatures (const SubListOfSpeciesFeatures & orig)
  : ListOf(orig)
//  ,mId(orig.mId)
  ,mRelation(orig.mRelation)
  ,mComponent(orig.mComponent)
{
  setElementNamespace(orig.getURI());
  connectToChild();
}



/*
 * Destructor
 */
SubListOfSpeciesFeatures::~SubListOfSpeciesFeatures()
{

}


/*
 * Returns a deep copy of this SubListOfSpeciesFeatures
 */
SubListOfSpeciesFeatures*
SubListOfSpeciesFeatures::clone () const
 {
  return new SubListOfSpeciesFeatures(*this);
}

/*
 * Returns the value of the "id" attribute of this SubListOfSpeciesFeatures.
 */
const std::string&
SubListOfSpeciesFeatures::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this SubListOfSpeciesFeatures.
 */
const std::string&
SubListOfSpeciesFeatures::getName() const
{
  return mName;
}


/*
 * Returns true/false if id is set.
 */
bool
SubListOfSpeciesFeatures::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
SubListOfSpeciesFeatures::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
SubListOfSpeciesFeatures::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
SubListOfSpeciesFeatures::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
SubListOfSpeciesFeatures::unsetId()
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
SubListOfSpeciesFeatures::unsetName()
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
 * Returns the value of the "component" attribute of this SubListOfSpeciesFeatures.
 */
const std::string&
SubListOfSpeciesFeatures::getComponent() const
{
  return mComponent;
}

/*
 * Returns true/false if component is set.
 */
bool
SubListOfSpeciesFeatures::isSetComponent() const
{
  return (mComponent.empty() == false);
}

/*
 * Sets component and returns value indicating success.
 */
int
SubListOfSpeciesFeatures::setComponent(const std::string& component)
{
  if (!(SyntaxChecker::isValidInternalSId(component)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mComponent = component;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Unsets component and returns value indicating success.
 */
int
SubListOfSpeciesFeatures::unsetComponent()
{
  mComponent.erase();

  if (mComponent.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Creates a new SpeciesFeature object and adds it to this SubListOfSpeciesFeatures object.
 */
SpeciesFeature*
SubListOfSpeciesFeatures::createSpeciesFeature ()
{
   SpeciesFeature* sf = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    sf = new SpeciesFeature(multins);
    delete multins;
  }
  catch(...)
  {
  }

  if (sf != NULL)
  {
    this->appendAndOwn(sf);
  }

  return sf;
}

/*
 * Get a SpeciesFeature from the SubListOfSpeciesFeatures by index.
 */
SpeciesFeature*
SubListOfSpeciesFeatures::get(unsigned int n)
{
  return static_cast<SpeciesFeature*>(ListOf::get(n));
}


/*
 * Get a SpeciesFeature from the SubListOfSpeciesFeatures by index.
 */
const SpeciesFeature*
SubListOfSpeciesFeatures::get(unsigned int n) const
{
  return static_cast<const SpeciesFeature*>(ListOf::get(n));
}


/*
 * Get a SpeciesFeature from the SubListOfSpeciesFeatures by id.
 */
SpeciesFeature*
SubListOfSpeciesFeatures::get(const std::string& sid)
{
  return const_cast<SpeciesFeature*>(
    static_cast<const SubListOfSpeciesFeatures&>(*this).get(sid));
}


/*
 * Get a SpeciesFeature from the SubListOfSpeciesFeatures by id.
 */
const SpeciesFeature*
SubListOfSpeciesFeatures::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesFeature>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpeciesFeature*> (*result);
}


/*
 * Removes the nth SpeciesFeature from this SubListOfSpeciesFeatures
 */
SpeciesFeature*
SubListOfSpeciesFeatures::remove(unsigned int n)
{
  return static_cast<SpeciesFeature*>(ListOf::remove(n));
}


/*
 * Removes the SpeciesFeature from this SubListOfSpeciesFeatures with the given identifier
 */
SpeciesFeature*
SubListOfSpeciesFeatures::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpeciesFeature>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SpeciesFeature*> (item);
}


 /*
 * Returns the value of the "relation" attribute of this SubListOfSpeciesFeatures.
 */
Relation_t
SubListOfSpeciesFeatures::getRelation() const
{
  return mRelation;
}


/*
 * Predicate returning @c true or @c false depending on whether this
 * SubListOfSpeciesFeatures's "relation" attribute has been set.
 */
bool
SubListOfSpeciesFeatures::isSetRelation() const
{
  return (mRelation != MULTI_RELATION_UNKNOWN);
}


/*
 * Sets the value of the "relation" attribute of this SubListOfSpeciesFeatures.
 */
int
SubListOfSpeciesFeatures::setRelation(Relation_t relation)
{
  if (SubListOfSpeciesFeatures_isValidRelation(relation) == 0)
  {
    mRelation = MULTI_RELATION_UNKNOWN;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mRelation = relation;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "relation" attribute of this SubListOfSpeciesFeatures.
 */
int
SubListOfSpeciesFeatures::setRelation(const std::string& relation)
{
  //if (SubListOfSpeciesFeatures_isValidRelationString(relation.c_str()) == 0)
  //{
  //  mRelation = MULTI_RELATION_UNKNOWN;
  //  return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  //}
  //else
  //{
  //  mRelation = Relation_fromString(relation.c_str());
  //  return LIBSBML_OPERATION_SUCCESS;
  //}
  //bgoli22
  mRelation = Relation_fromString(relation.c_str());
  if (mRelation == MULTI_RELATION_UNKNOWN)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "relation" attribute of this SubListOfSpeciesFeatures.
 */
int
SubListOfSpeciesFeatures::unsetRelation()
{
  mRelation = MULTI_RELATION_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
SubListOfSpeciesFeatures::getElementName () const
{
  static const string name = "subListOfSpeciesFeatures";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SubListOfSpeciesFeatures::getTypeCode () const
{
  return SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
SubListOfSpeciesFeatures::getItemTypeCode () const
{
  return SBML_MULTI_SPECIES_FEATURE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SpeciesFeature in this SubListOfSpeciesFeatures
 */
SBase*
SubListOfSpeciesFeatures::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "speciesFeature")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new SpeciesFeature(multins);
    appendAndOwn(object);
    delete multins;
  }

  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Multi package.
 */
void
SubListOfSpeciesFeatures::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(MultiExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(MultiExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond */

/** @cond doxygenLibsbmlInternal */
void
SubListOfSpeciesFeatures::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("relation");
  attributes.add("component");

}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
SubListOfSpeciesFeatures::readAttributes (const XMLAttributes& attributes,
                                  const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  SBase::readAttributes(attributes, expectedAttributes);

  if (getErrorLog() != NULL)
  {
      unsigned int numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiSubLofSpeFtrs_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
      else
      if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiSubLofSpeFtrs_AllowedCoreAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details,
                       getLine(), getColumn());
      }
    }
  }


  bool assigned = false;

  //
  // id SId  ( use = "optional" )
  //
  assigned = attributes.readInto("id", mId, getErrorLog(), false);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<SubListOfSpeciesFeatures>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute id='" + mId + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
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
       logEmptyString(mName, getLevel(), getVersion(), "<SubListOfSpeciesFeatures>");
     }
   }


  //
  // relation string   ( required )
  //
  std::string relation;
  assigned = attributes.readInto("relation", relation,
                                   getErrorLog(), true);

  if (assigned == true)
  {
    // check string is not empty

    if (relation.empty() == true)
    {
      logEmptyString(relation, getLevel(), getVersion(),
                                    "<SubListOfSpeciesFeatures>");
    }
    else
    {
       mRelation = Relation_fromString( relation.c_str() );
       if (SubListOfSpeciesFeatures_isValidRelation(mRelation) == 0)
       {
          getErrorLog()->logPackageError("multi", MultiSubLofSpeFtrs_RelationAtt_Ref,
                       getPackageVersion(), getLevel(), getVersion(), "",
                       getLine(), getColumn());
       }
    }
  }
  else {
	    std::string message = "Multi attribute 'relation' is missing.";
	    getErrorLog()->logPackageError("multi", MultiSubLofSpeFtrs_AllowedMultiAtts,
	                   getPackageVersion(), sbmlLevel, sbmlVersion, message,
	                   getLine(), getColumn());

  }

  assigned = attributes.readInto("component", mComponent);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mComponent.empty() == true)
    {
      logEmptyString(mComponent, getLevel(), getVersion(), "<SubListOfSpeciesFeatures>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mComponent) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute component='" + mComponent + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }

}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
SubListOfSpeciesFeatures::writeAttributes (XMLOutputStream& stream) const
{
  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetRelation() == true)
    stream.writeAttribute("relation", getPrefix(), Relation_toString(mRelation));

  if (isSetComponent() == true)
    stream.writeAttribute("component", getPrefix(), mComponent);

  SBase::writeExtensionAttributes(stream);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * write elements
 */
void
SubListOfSpeciesFeatures::writeElements (XMLOutputStream& stream) const
{
  ListOf::writeElements(stream);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor.
 */
bool
SubListOfSpeciesFeatures::accept (SBMLVisitor& v) const
{
	v.visit(*this);

	// SpeciesFeature
	for(unsigned int i = 0; i < getNumSpeciesFeatures(); i++)
	{
	  get(i)->accept(v);
	}

	v.leave(*this);

	return true;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
SubListOfSpeciesFeatures::connectToChild()
{

}
/** @endcond */


unsigned int
SubListOfSpeciesFeatures::getNumSpeciesFeatures() const
{
  return static_cast<unsigned int>( mItems.size() );
}


static
const char* RELATION_STRINGS[] =
{
    "and"
  , "or"
  , "not"
  , "unknown"
};


LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isValidRelation(Relation_t relation)
{
  int max = MULTI_RELATION_UNKNOWN;

  if (relation < MULTI_RELATION_AND || relation >= max)
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
SubListOfSpeciesFeatures_isValidRelationString(const char* s)
{
  return SubListOfSpeciesFeatures_isValidRelation(Relation_fromString(s));
}




LIBSBML_EXTERN
const char*
Relation_toString(Relation_t relation)
{
  int max = MULTI_RELATION_UNKNOWN;

  if (relation < MULTI_RELATION_AND || relation >= max)
  {
    return NULL;
  }

  return RELATION_STRINGS[relation];
}


LIBSBML_EXTERN
Relation_t
Relation_fromString(const char* s)
{
  if (s == NULL)
  {
    return MULTI_RELATION_UNKNOWN;
  }

  int max = MULTI_RELATION_UNKNOWN;
  for (int i = 0; i < max; i++)
  {
    if (strcmp(RELATION_STRINGS[i], s) == 0)
    {
      return (Relation_t)i;
    }
  }
  return MULTI_RELATION_UNKNOWN;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #Relation_t is valid.
 */
LIBSBML_EXTERN
int
Relation_isValid(Relation_t r)
{
  int min = MULTI_RELATION_AND;
  int max = MULTI_RELATION_UNKNOWN;

  if (r < min || r >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #Relation_t.
 */
LIBSBML_EXTERN
int
Relation_isValidString(const char* code)
{
  return Relation_isValid(Relation_fromString(code));
}


/*
 * Creates a new SubListOfSpeciesFeatures_t using the given SBML Level, Version
 * and &ldquo;multi&rdquo; package version.
 */
LIBSBML_EXTERN
SubListOfSpeciesFeatures_t *
SubListOfSpeciesFeatures_create(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new SubListOfSpeciesFeatures(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this SubListOfSpeciesFeatures_t object.
 */
LIBSBML_EXTERN
SubListOfSpeciesFeatures_t*
SubListOfSpeciesFeatures_clone(const SubListOfSpeciesFeatures_t* slosf)
{
  if (slosf != NULL)
  {
    return static_cast<SubListOfSpeciesFeatures_t*>(slosf->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this SubListOfSpeciesFeatures_t object.
 */
LIBSBML_EXTERN
void
SubListOfSpeciesFeatures_free(SubListOfSpeciesFeatures_t* slosf)
{
  if (slosf != NULL)
  {
    delete slosf;
  }
}


/*
 * Returns the value of the "id" attribute of this SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
char *
SubListOfSpeciesFeatures_getId(const SubListOfSpeciesFeatures_t * slosf)
{
  if (slosf == NULL)
  {
    return NULL;
  }

  return slosf->getId().empty() ? NULL : safe_strdup(slosf->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
char *
SubListOfSpeciesFeatures_getName(const SubListOfSpeciesFeatures_t * slosf)
{
  if (slosf == NULL)
  {
    return NULL;
  }

  return slosf->getName().empty() ? NULL :
    safe_strdup(slosf->getName().c_str());
}


/*
 * Returns the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
Relation_t
SubListOfSpeciesFeatures_getRelation(const SubListOfSpeciesFeatures_t * slosf)
{
  if (slosf == NULL)
  {
    return MULTI_RELATION_UNKNOWN;
  }

  return slosf->getRelation();
}


/*
 * Returns the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
const char *
SubListOfSpeciesFeatures_getRelationAsString(const SubListOfSpeciesFeatures_t *
  slosf)
{
  return Relation_toString(slosf->getRelation());
}


/*
 * Returns the value of the "component" attribute of this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
char *
SubListOfSpeciesFeatures_getComponent(const SubListOfSpeciesFeatures_t * slosf)
{
  if (slosf == NULL)
  {
    return NULL;
  }

  return slosf->getComponent().empty() ? NULL :
    safe_strdup(slosf->getComponent().c_str());
}


/*
 * Predicate returning @c 1 (true) if this SubListOfSpeciesFeatures_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isSetId(const SubListOfSpeciesFeatures_t * slosf)
{
  return (slosf != NULL) ? static_cast<int>(slosf->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SubListOfSpeciesFeatures_t's "name"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isSetName(const SubListOfSpeciesFeatures_t * slosf)
{
  return (slosf != NULL) ? static_cast<int>(slosf->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SubListOfSpeciesFeatures_t's
 * "relation" attribute is set.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isSetRelation(const SubListOfSpeciesFeatures_t *
  slosf)
{
  return (slosf != NULL) ? static_cast<int>(slosf->isSetRelation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SubListOfSpeciesFeatures_t's
 * "component" attribute is set.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isSetComponent(const SubListOfSpeciesFeatures_t *
  slosf)
{
  return (slosf != NULL) ? static_cast<int>(slosf->isSetComponent()) : 0;
}


/*
 * Sets the value of the "id" attribute of this SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setId(SubListOfSpeciesFeatures_t * slosf,
  const char * id)
{
  return (slosf != NULL) ? slosf->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setName(SubListOfSpeciesFeatures_t * slosf,
  const char * name)
{
  return (slosf != NULL) ? slosf->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setRelation(SubListOfSpeciesFeatures_t * slosf,
  Relation_t relation)
{
  return (slosf != NULL) ? slosf->setRelation(relation) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setRelationAsString(
  SubListOfSpeciesFeatures_t *
  slosf,
  const char * relation)
{
  return (slosf != NULL) ? slosf->setRelation(relation) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "component" attribute of this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setComponent(SubListOfSpeciesFeatures_t * slosf,
  const char * component)
{
  return (slosf != NULL) ? slosf->setComponent(component) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_unsetId(SubListOfSpeciesFeatures_t * slosf)
{
  return (slosf != NULL) ? slosf->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_unsetName(SubListOfSpeciesFeatures_t * slosf)
{
  return (slosf != NULL) ? slosf->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_unsetRelation(SubListOfSpeciesFeatures_t * slosf)
{
  return (slosf != NULL) ? slosf->unsetRelation() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "component" attribute of this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_unsetComponent(SubListOfSpeciesFeatures_t * slosf)
{
  return (slosf != NULL) ? slosf->unsetComponent() : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of SpeciesFeature_t objects in this
 * SubListOfSpeciesFeatures_t.
 */
LIBSBML_EXTERN
unsigned int
SubListOfSpeciesFeatures_getNumSpeciesFeatures(SubListOfSpeciesFeatures_t*
  slosf)
{
  return (slosf != NULL) ? slosf->getNumSpeciesFeatures() : SBML_INT_MAX;
}


/*
 * Creates a new SpeciesFeature_t object, adds it to this
 * SubListOfSpeciesFeatures_t object and returns the SpeciesFeature_t object
 * created.
 */
LIBSBML_EXTERN
SpeciesFeature_t*
SubListOfSpeciesFeatures_createSpeciesFeature(SubListOfSpeciesFeatures_t*
  slosf)
{
  return (slosf != NULL) ? slosf->createSpeciesFeature() : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SubListOfSpeciesFeatures_t object have been set.
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_hasRequiredAttributes(const SubListOfSpeciesFeatures_t
  * slosf)
{
  return (slosf != NULL) ? static_cast<int>(slosf->hasRequiredAttributes()) :
    0;
}

LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */

