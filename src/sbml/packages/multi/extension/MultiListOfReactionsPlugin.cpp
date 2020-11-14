/**
 * @file:   MultiListOfReactionsPlugin.cpp
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
 */


#include <sbml/packages/multi/extension/MultiListOfReactionsPlugin.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new MultiListOfReactionsPlugin
 */
MultiListOfReactionsPlugin::MultiListOfReactionsPlugin(const std::string& uri,
                                 const std::string& prefix, 
                               MultiPkgNamespaces* multins) :
    SBasePlugin(uri, prefix, multins)
{
}


/*
 * Copy constructor for MultiListOfReactionsPlugin.
 */
MultiListOfReactionsPlugin::MultiListOfReactionsPlugin(const MultiListOfReactionsPlugin& orig) :
    SBasePlugin(orig)
{
}

/*
 * Destructor for MultiListOfReactionsPlugin.
 */
MultiListOfReactionsPlugin::~MultiListOfReactionsPlugin()
{
}

/*
 * Assignment operator for MultiListOfReactionsPlugin.
 */
MultiListOfReactionsPlugin&
MultiListOfReactionsPlugin::operator=(const MultiListOfReactionsPlugin& rhs)
{
  return *this;
}


/*
 * Creates and returns a deep copy of this MultiListOfReactionsPlugin object.
 */
MultiListOfReactionsPlugin*
MultiListOfReactionsPlugin::clone () const
{
  return new MultiListOfReactionsPlugin(*this);
}

//---------------------------------------------------------------

/** @cond doxygenLibsbmlInternal */
/*
 * Accept the SBMLVisitor.
 */
bool
MultiListOfReactionsPlugin::accept(SBMLVisitor& v) const
{
  // here we dont need to do this as the constraints to the model
  // have already been applied via the multimodelplugin
  //const Model * model = static_cast<const Model * >(this->getParentSBMLObject());

  //v.visit(*model);
  //v.leave(*model);

  return true;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool
MultiListOfReactionsPlugin::isValidTypeForList(SBase* item) const
{
  return item->getTypeCode() == SBML_REACTION ||
      item->getTypeCode() == SBML_MULTI_INTRA_SPECIES_REACTION;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
SBase*
MultiListOfReactionsPlugin::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = NULL;


  if (name == "intraSpeciesReaction")
  {
    try
    {
       MULTI_CREATE_NS(multins, getSBMLNamespaces());
       object = new IntraSpeciesReaction(multins);
       delete multins;
    }
    catch (SBMLConstructorException*)
    {
      object = new IntraSpeciesReaction(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      object = new IntraSpeciesReaction(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }

    if (object != NULL)
    {
      ListOf * lo = dynamic_cast<ListOf*>(getParentSBMLObject());
      if (lo != NULL)
        lo->appendAndOwn(object);
    }
  }

  return object;
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


