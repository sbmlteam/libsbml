/**
 * @file    DefinitionURLRegistry.h
 * @brief   Definition of DefinitionURLRegistry, a registry of available DefinitionURLs.
 * @author  Frank Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 *
 * @class DefinitionURLRegistry
 * @sbmlbrief{core} Registry of all libSBML SBML DefinitionURLs.
 *
 * @htmlinclude libsbml-facility-only-warning.html
 *
 * LibSBML provides facilities for transforming and converting SBML
 * documents in various ways.  These transformations can involve
 * essentially anything that can be written algorithmically; examples
 * include converting the units of measurement in a model, or converting
 * from one Level+Version combination of SBML to another.  DefinitionURLs are
 * implemented as objects derived from the class DefinitionURL.
 *
 * The DefinitionURL registry, implemented as a singleton object of class
 * DefinitionURLRegistry, maintains a list of known DefinitionURLs and provides
 * methods for discovering them.  Callers can use the method
 * DefinitionURLRegistry::getNumDefinitionURLs() to find out how many
 * DefinitionURLs are registered, then use
 * DefinitionURLRegistry::getDefinitionURLByIndex(@if java int@endif) to
 * iterate over each one; alternatively, callers can use
 * DefinitionURLRegistry::getDefinitionURLFor(@if java const ConversionProperties@endif)
 * to search for a DefinitionURL having specific properties.
 */

#ifndef DefinitionURLRegistry_h
#define DefinitionURLRegistry_h


#include <sbml/common/extern.h>
#include <map>
#include <sbml/math/ASTNodeType.h>
#include <sbml/SBMLNamespaces.h>


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

#ifndef SWIG
typedef std::map <const std::string, int > UrlMap;
typedef std::map <const std::string, int >::iterator UrlIt;
#endif

class LIBSBML_EXTERN DefinitionURLRegistry
{
public:
  /**
   * Returns the singleton instance for the DefinitionURL registry.
   *
   * Prior to using the registry, callers have to obtain a copy of the
   * registry.  This static method provides the means for doing that.
   *
   * @return the singleton for the DefinitionURL registry.
   */
  static DefinitionURLRegistry& getInstance();


  /**
   * Adds the given DefinitionURL to the registry of SBML DefinitionURLs.
   *
   * @param DefinitionURL the DefinitionURL to add to the registry.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   */
  static int addDefinitionURL (const std::string& url, int type);


  /**
   * Returns the number of DefinitionURLs known by the registry.
   *
   * @return the number of registered DefinitionURLs.
   *
   * @see getDefinitionURLByIndex(@if java int@endif)
   */
  static int getNumDefinitionURLs();


  static void addSBMLDefinitions();

  /**
   * Destructor
   */
  virtual ~DefinitionURLRegistry();

  static bool getCoreDefinitionsAdded();

  static int getType(const std::string& url);

  static std::string getDefinitionUrlByIndex(int index);

  static void clearDefinitions();


protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * protected constructor, use the getInstance() method to access the registry.
   */
  DefinitionURLRegistry();

  static void setCoreDefinitionsAdded();

  /** @endcond */


protected:
  /** @cond doxygenLibsbmlInternal */
  UrlMap mDefinitionURLs;

  bool mCoreInit;
  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif /* !DefinitionURLRegistry_h */

