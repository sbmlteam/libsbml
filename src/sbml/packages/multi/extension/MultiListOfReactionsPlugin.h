/**
 * @file:   MultiListOfReactionsPlugin.h
 * @brief:  Implementation of the MultiListOfReactionsPlugin class
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
 *
 * @class MultiListOfReactionsPlugin
 * @sbmlbrief{multi} Extension of ListOfReactions for the "multi" package.
 *
 * The MultiListOfReactionsPlugin class extends the ListOfReactions class to
 * allow a ListOfReactions to contain IntraSpeciesReaction objects as well as
 * Reaction objects.
 */

#ifndef MultiListOfReactionsPlugin_H__
#define MultiListOfReactionsPlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/multi/extension/MultiExtension.h>
#include <sbml/packages/multi/sbml/IntraSpeciesReaction.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiListOfReactionsPlugin : public SBasePlugin
{
public:

  /**
   * Creates a new MultiListOfReactionsPlugin
   *
   * @copydetails doc_what_are_xmlnamespaces
   *
   * @copydetails doc_what_are_sbmlnamespaces
   *
   * @param uri the URI of the SBML Level&nbsp;3 package implemented by
   * this libSBML package extension.
   *
   * @param prefix the XML namespace prefix being used for the package.
   *
   * @param multins the namespaces object for the package.
   */
  MultiListOfReactionsPlugin(const std::string& uri, const std::string& prefix,
                             MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiListOfReactionsPlugin.
   *
   * @param orig the MultiListOfReactionsPlugin instance to copy.
   */
  MultiListOfReactionsPlugin(const MultiListOfReactionsPlugin& orig);


  /**
   * Assignment operator for MultiListOfReactionsPlugin.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  MultiListOfReactionsPlugin& operator=(const MultiListOfReactionsPlugin& rhs);


  /**
   * Creates and returns a deep copy of this MultiListOfReactionsPlugin object.
   *
   * @return a (deep) copy of this MultiListOfReactionsPlugin object.
   */
  virtual MultiListOfReactionsPlugin* clone () const;


  /**
   * Destructor for MultiListOfReactionsPlugin.
   */
  virtual ~MultiListOfReactionsPlugin();

  //---------------------------------------------------------------


  //---------------------------------------------------------------
  //
  // Functions for interacting with the members of the plugin
  //
  //---------------------------------------------------------------

  /** @cond doxygenLibsbmlInternal */
  SBase* createObject (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  virtual bool isValidTypeForList(SBase* item) const;
  /** @endcond */

};


LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* MultiListOfReactionsPlugin_H__ */


