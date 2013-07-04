/**
 * @file    ReplacedBy.cpp
 * @brief   Implementation of ReplacedBy, the SBaseRef derived class of the comp package.
 * @author  Lucian Smith
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2011 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <iostream>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/comp/sbml/ReplacedBy.h>
#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/extension/CompSBasePlugin.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

ReplacedBy::ReplacedBy (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : Replacing(level,version, pkgVersion)
{
}


ReplacedBy::ReplacedBy(CompPkgNamespaces* compns)
  : Replacing(compns)
{
  loadPlugins(compns);
}


ReplacedBy::ReplacedBy(const ReplacedBy& source) 
  : Replacing(source)
{
}

ReplacedBy& ReplacedBy::operator=(const ReplacedBy& source)
{
  if(&source!=this)
  {
    Replacing::operator=(source);
  }
  return *this;
}

ReplacedBy*
ReplacedBy::clone() const
{
  return new ReplacedBy(*this);
}

ReplacedBy::~ReplacedBy ()
{
}


const std::string&
ReplacedBy::getElementName () const
{
  static const std::string name = "replacedBy";
  return name;
}


int
ReplacedBy::getTypeCode () const
{
  return SBML_COMP_REPLACEDBY;
}


int 
ReplacedBy::removeFromParentAndDelete()
{
  SBase* parent = getParentSBMLObject();
  if (parent==NULL) return LIBSBML_OPERATION_FAILED;
  CompSBasePlugin* comp_parent = static_cast<CompSBasePlugin*>(parent->getPlugin(getPrefix()));
  if (comp_parent==NULL) return LIBSBML_OPERATION_FAILED;
  return comp_parent->unsetReplacedBy();
}


int ReplacedBy::performReplacement()
{
  //Find the various objects and plugin objects we need for this to work.
  SBase* parent = getParentSBMLObject();
  if (parent==NULL) return LIBSBML_INVALID_OBJECT;
  SBase* ref = getReferencedElement();
  if (ref==NULL) return LIBSBML_INVALID_OBJECT;
  CompSBasePlugin* refplug = static_cast<CompSBasePlugin*>(ref->getPlugin(getPrefix()));
  if (refplug==NULL) return LIBSBML_INVALID_OBJECT;

  //Update the IDs.
  int ret = updateIDs(ref, parent);
  
  //ReplacedBy elements do get the name of the top-level element, assuming it has one:
  if (parent->isSetId()) {
    ref->setId(parent->getId());
  }
  if (parent->isSetMetaId()) {
    ref->setMetaId(parent->getMetaId());
  }
  if (ret != LIBSBML_OPERATION_SUCCESS) return ret;

  /*
  //Now recurse down the 'replace*' tree, renaming IDs and deleting things as we go.
  for (unsigned int re=0; re<refplug->getNumReplacedElements(); re++) {
    refplug->getReplacedElement(re)->replaceWithAndMaybeDelete(parent, true, NULL);
  }
  if (refplug->isSetReplacedBy()) {
    //If the subelement is replaced by something further down, we perform that replacement, now that it has its new ID.
    int ret = refplug->getReplacedBy()->performReplacement();
    if (ret != LIBSBML_OPERATION_SUCCESS) return ret;
  }
  */
  //And finally, delete the parent object.
  return CompBase::removeFromParentAndPorts(parent);
}


/** @cond doxygen-libsbml-internal */

bool
ReplacedBy::accept (SBMLVisitor& v) const
{
  v.visit(*this);

  if (isSetSBaseRef() == true)
  {
    getSBaseRef()->accept(v);
  }

  return true;
}

/** @endcond */

/**
 * 
 */
LIBSBML_EXTERN
ReplacedBy_t *
ReplacedBy_create(unsigned int level, unsigned int version,
                  unsigned int pkgVersion)
{
	return new ReplacedBy(level, version, pkgVersion);
}


/**
 * 
 */
LIBSBML_EXTERN
void
ReplacedBy_free(ReplacedBy_t * rb)
{
	if (rb != NULL)
		delete rb;
}


/**
 * 
 */
LIBSBML_EXTERN
ReplacedBy_t *
ReplacedBy_clone(ReplacedBy_t * rb)
{
	if (rb != NULL)
	{
		return static_cast<ReplacedBy_t*>(rb->clone());
	}
	else
	{
		return NULL;
	}
}


/**
 * 
 */
LIBSBML_EXTERN
char *
ReplacedBy_getSubmodelRef(ReplacedBy_t * rb)
{
	if (rb == NULL)
		return NULL;

	return rb->getSubmodelRef().empty() ? NULL : safe_strdup(rb->getSubmodelRef().c_str());
}


/**
 * 
 */
LIBSBML_EXTERN
int
ReplacedBy_isSetSubmodelRef(ReplacedBy_t * rb)
{
	return (rb != NULL) ? static_cast<int>(rb->isSetSubmodelRef()) : 0;
}


/**
 * 
 */
LIBSBML_EXTERN
int
ReplacedBy_setSubmodelRef(ReplacedBy_t * rb, const char * submodelRef)
{
	return (rb != NULL) ? rb->setSubmodelRef(submodelRef) : LIBSBML_INVALID_OBJECT;
}


/**
 * 
 */
LIBSBML_EXTERN
int
ReplacedBy_unsetSubmodelRef(ReplacedBy_t * rb)
{
	return (rb != NULL) ? rb->unsetSubmodelRef() : LIBSBML_INVALID_OBJECT;
}


/**
 * 
 */
LIBSBML_EXTERN
int
ReplacedBy_hasRequiredAttributes(ReplacedBy_t * rb)
{
	return (rb != NULL) ? static_cast<int>(rb->hasRequiredAttributes()) : 0;
}



LIBSBML_CPP_NAMESPACE_END

