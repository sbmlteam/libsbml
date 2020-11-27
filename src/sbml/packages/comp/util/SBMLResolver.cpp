/**
 * @file    SBMLResolver.cpp
 * @brief   Implementation of SBMLResolver, the base class of package extensions.
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

#include <sbml/packages/comp/util/SBMLResolver.h>
#include <sbml/packages/comp/util/SBMLUri.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLConstructorException.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

SBMLResolver::SBMLResolver ()
{
}


/*
 * Copy constructor.
 */
SBMLResolver::SBMLResolver(const SBMLResolver& )
{
  //  
}


/*
 * Destroy this object.
 */
SBMLResolver::~SBMLResolver ()
{

}


/*
 * Assignment operator for SBMLResolver.
 */
SBMLResolver&
SBMLResolver::operator=(const SBMLResolver& rhs)
{
  if(&rhs!=this)
  {
    //
  }

  return *this;
}


SBMLResolver*
SBMLResolver::clone () const
{
  return new SBMLResolver(*this);
}


SBMLDocument*
SBMLResolver::resolve(const std::string &, const std::string& /*=""*/) const
{
  return NULL;
}

SBMLUri* 
SBMLResolver::resolveUri(const std::string &, const std::string& /*=""*/) const
{
  return NULL;
}

/** @cond doxygenIgnored */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


