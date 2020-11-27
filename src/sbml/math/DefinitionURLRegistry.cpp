/**
 * @file    DefinitionURLRegistry.cpp
 * @brief   Implementation of DefinitionURLRegistry, a registry of available DefinitionURLs.
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

#ifdef __cplusplus


#include <sbml/common/operationReturnValues.h>

#include <sbml/math/DefinitionURLRegistry.h>
#include <sbml/util/util.h>


using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN


DefinitionURLRegistry& 
DefinitionURLRegistry::getInstance()
{
  static DefinitionURLRegistry singletonObj;
  return singletonObj;
}


void 
DefinitionURLRegistry::addSBMLDefinitions()
{
  // old behaviour when a stream may not know sbmlns
  addDefinitionURL("http://www.sbml.org/sbml/symbols/time", AST_NAME_TIME);
  addDefinitionURL("http://www.sbml.org/sbml/symbols/delay", AST_FUNCTION_DELAY);
  addDefinitionURL("http://www.sbml.org/sbml/symbols/avogadro", AST_NAME_AVOGADRO);
#ifdef USE_L3V2EXTENDEDMATH
  addDefinitionURL("http://www.sbml.org/sbml/symbols/rateOf", AST_FUNCTION_RATE_OF);
#endif
  setCoreDefinitionsAdded();
}

int 
DefinitionURLRegistry::addDefinitionURL (const std::string& url, int type)
{
  unsigned int n = getNumDefinitionURLs();
  getInstance().mDefinitionURLs.insert(std::pair<std::string, int>(url, type));
  if (getNumDefinitionURLs() == n + 1)
    return LIBSBML_OPERATION_SUCCESS;
  else
    return LIBSBML_OPERATION_FAILED;
}

int
DefinitionURLRegistry::getType(const std::string& url)
{
  int type = AST_UNKNOWN;
  UrlIt it = getInstance().mDefinitionURLs.find(url);
  if (it != getInstance().mDefinitionURLs.end())
  {
    type = it->second;
  }

  return type;
}

std::string DefinitionURLRegistry::getDefinitionUrlByIndex(int index)
{
  UrlMap::const_iterator beg = getInstance().mDefinitionURLs.begin();
  for (size_t i = 0; i < getInstance().mDefinitionURLs.size(); ++i)
  {
    if (i == index)
      return beg->first;
    ++beg;
  }
  return std::string();
}

int
DefinitionURLRegistry::getNumDefinitionURLs()
{
  return (int)(getInstance().mDefinitionURLs.size());
}

/** @cond doxygenLibsbmlInternal */
DefinitionURLRegistry::DefinitionURLRegistry()
{
  mCoreInit = false;
}

void 
DefinitionURLRegistry::setCoreDefinitionsAdded()
{
  getInstance().mCoreInit = true;
}

bool
DefinitionURLRegistry::getCoreDefinitionsAdded()
{
  return getInstance().mCoreInit;
}

void
DefinitionURLRegistry::clearDefinitions()
{
  getInstance().mDefinitionURLs.clear();
  getInstance().mCoreInit = false;
}


/** @endcond */


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */



