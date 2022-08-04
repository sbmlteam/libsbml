/**
 * @file    SBMLResolverRegistry.cpp
 * @brief   Implementation of SBMLResolverRegistry, a registry of available resolvers.
 * @author  Frank Bergmann
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
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#ifdef __cplusplus

#include <algorithm>
#include <vector>
#include <string>
#include <sstream>
#include <cstdlib>

#include <sbml/SBMLDocument.h>
#include <sbml/packages/comp/util/SBMLResolverRegistry.h>
#include <sbml/packages/comp/util/SBMLResolver.h>
#include <sbml/packages/comp/util/SBMLFileResolver.h>
#include <sbml/util/util.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/** @cond doxygenLibsbmlInternal */
SBMLResolverRegistry* SBMLResolverRegistry::mInstance = NULL;
/** @endcond */

void 
SBMLResolverRegistry::deleteResolerRegistryInstance()
{
  delete SBMLResolverRegistry::mInstance;
  SBMLResolverRegistry::mInstance = NULL;
}

SBMLResolverRegistry&
SBMLResolverRegistry::getInstance()
{
  if (SBMLResolverRegistry::mInstance == NULL) 
  {
    mInstance = new SBMLResolverRegistry();
    std::atexit(&SBMLResolverRegistry::deleteResolerRegistryInstance);
  }
  return *mInstance;
}

int
SBMLResolverRegistry::addResolver (const SBMLResolver* resolver)
{
  if (resolver == NULL) return LIBSBML_INVALID_OBJECT;

  mResolvers.push_back(resolver->clone());

  return LIBSBML_OPERATION_SUCCESS;
}

int
SBMLResolverRegistry::addOwnedSBMLDocument(const SBMLDocument* doc)
{
  if (doc == NULL) return LIBSBML_INVALID_OBJECT;

 // mOwnedDocuments.insert(doc);

  return LIBSBML_OPERATION_SUCCESS;
}

int
SBMLResolverRegistry::removeResolver(int index)
{
  if (index < 0 || index >= getNumResolvers())
     return LIBSBML_INVALID_OBJECT;
  
  SBMLResolver *current = const_cast<SBMLResolver *>(mResolvers.at((size_t)index));
  if (current != NULL)
    delete current;
  
  mResolvers.erase(mResolvers.begin() + index);
  
  return LIBSBML_OPERATION_SUCCESS;
}


int
SBMLResolverRegistry::getNumResolvers() const
{
  return (int)mResolvers.size();
}

SBMLResolver*
SBMLResolverRegistry::getResolverByIndex(int index) const
{
  if (index < 0 || index >= getNumResolvers())
    return NULL;
  return mResolvers.at((size_t)index)->clone();
}


/** @cond doxygenLibsbmlInternal */
SBMLResolverRegistry::SBMLResolverRegistry()
{
  // for now ensure that we always have a file resolver in there
  // 
  SBMLFileResolver resolver;
  addResolver(&resolver);

}

SBMLResolverRegistry::~SBMLResolverRegistry()
{
  size_t numResolvers = mResolvers.size();
  for (size_t i = 0; i < numResolvers; ++i)
  {
    SBMLResolver *current = const_cast<SBMLResolver *>(mResolvers.back());
    mResolvers.pop_back();
    if (current != NULL)
    {
      delete current;
      current = NULL;
    }
  }
  mResolvers.clear();

  while(mOwnedDocuments.size())
  {
    const SBMLDocument* doc = *(mOwnedDocuments.begin());
    delete doc;
    mOwnedDocuments.erase(doc);
  }
}

SBMLDocument*
SBMLResolverRegistry::resolve(const std::string& uri, const std::string baseUri/*=""*/) const
{
  SBMLDocument* result = NULL;
  std::vector<const SBMLResolver*>::const_iterator it = mResolvers.begin();
  while(it != mResolvers.end())
  {
    result = (*it)->resolve(uri, baseUri);
    if (result != NULL)
      return result;
    ++it;
  }
  return result;
}

SBMLUri* 
SBMLResolverRegistry::resolveUri(const std::string& uri, const std::string baseUri/*=""*/) const
{
  SBMLUri* result = NULL;
  std::vector<const SBMLResolver*>::const_iterator it = mResolvers.begin();
  while(it != mResolvers.end())
  {
    result = (*it)->resolveUri(uri, baseUri);
    if (result != NULL)
      return result;
    ++it;
  }
  return result;
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */



