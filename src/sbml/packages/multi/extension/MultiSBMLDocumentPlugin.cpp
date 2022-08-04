/**
 * @file:   MultiSBMLDocumentPlugin.cpp
 * @brief:  Implementation of the MultiSBMLDocumentPlugin class
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
#include <sbml/packages/multi/extension/MultiSBMLDocumentPlugin.h>
#include <sbml/packages/multi/validator/MultiConsistencyValidator.h>
#include <sbml/packages/multi/validator/MultiIdentifierConsistencyValidator.h>
#include <sbml/packages/multi/validator/MultiMathMLConsistencyValidator.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>


#ifdef __cplusplus

/** @cond doxygenLibsbmlInternal */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
MultiSBMLDocumentPlugin::MultiSBMLDocumentPlugin (const string& uri, 
                              const string &prefix, MultiPkgNamespaces *multins)
  : SBMLDocumentPlugin(uri,prefix, multins)
{
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
MultiSBMLDocumentPlugin::MultiSBMLDocumentPlugin(const MultiSBMLDocumentPlugin& orig)
  : SBMLDocumentPlugin(orig)
{
}
/** @endcond */


MultiSBMLDocumentPlugin& 
MultiSBMLDocumentPlugin::operator=(const MultiSBMLDocumentPlugin& orig)
{
  if(&orig!=this)
  {
    SBMLDocumentPlugin::operator =(orig);
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this MultiSBMLDocumentPlugin object.
 */
MultiSBMLDocumentPlugin* 
MultiSBMLDocumentPlugin::clone () const
{
  return new MultiSBMLDocumentPlugin(*this);  
}

/*
 * Destroy this object.
 */
MultiSBMLDocumentPlugin::~MultiSBMLDocumentPlugin () 
{
}


/** @cond doxygenLibsbmlInternal */

void 
MultiSBMLDocumentPlugin::readAttributes (const XMLAttributes& attributes,
                            const ExpectedAttributes& /*expectedAttributes*/)
{
  // for now don't read the required flag for L2 models 
  if (getSBMLDocument() != NULL && getSBMLDocument()->getLevel() < 3) return;
  
  // for "multi:required" attribute
  unsigned int numErrs = getErrorLog()->getNumErrors();
  XMLTriple tripleRequired("required", mURI, getPrefix());
  bool assigned = attributes.readInto(tripleRequired, mRequired);
  if (assigned == false)
  {
    if (getErrorLog()->getNumErrors() == numErrs + 1 && 
        getErrorLog()->contains(XMLAttributeTypeMismatch))
    {
			getErrorLog()->remove(XMLAttributeTypeMismatch);
      getErrorLog()->logPackageError("multi", MultiSBML_RequiredAttMustBeBoolean,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
    }
    else
    {
      getErrorLog()->logPackageError("multi", MultiSBML_RequiredAttMissing,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
    }
  }
  else
  {
      mIsSetRequired = true;
      if (mRequired == false)
      {
        getErrorLog()->logPackageError("multi", MultiSBML_RequiredAttMustBeTrue,
          getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
      }
  }
}

/** @endcond*/


/** @cond doxygenLibsbmlInternal */
bool
MultiSBMLDocumentPlugin::isCompFlatteningImplemented() const
{
  return false;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
unsigned int 
MultiSBMLDocumentPlugin::checkConsistency()
{
  unsigned int nerrors = 0;
  unsigned int total_errors = 0;

  SBMLDocument* doc = static_cast<SBMLDocument *>(this->getParentSBMLObject());
  SBMLErrorLog *log = doc->getErrorLog();

  unsigned char applicableValidators = doc->getApplicableValidators();

  /* determine which validators to run */
  bool id    = ((applicableValidators & 0x01) == 0x01);
  bool sbml  = ((applicableValidators & 0x02) == 0x02);
  bool math = ((applicableValidators & 0x08) == 0x08);
  /* LIST OTHERS HERE */

  MultiIdentifierConsistencyValidator id_validator;
  MultiConsistencyValidator validator;
  MultiMathMLConsistencyValidator math_validator;
  /* LIST OTHERS HERE */

  if (id)
  {
    id_validator.init();
    nerrors = id_validator.validate(*doc);
    total_errors += nerrors;
    if (nerrors > 0) 
    {
      log->add(id_validator.getFailures() );
      /* only want to bail if errors not warnings */
      if (log->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0)
      {
        return total_errors;
      }
    }
  }

  if (math)
  {
    math_validator.init();
    nerrors = math_validator.validate(*doc);
    total_errors += nerrors;
    if (nerrors > 0)
    {
      log->add(math_validator.getFailures());
      /* only want to bail if errors not warnings */
      if (log->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0)
      {
        return total_errors;
      }
    }
  }

  if (sbml)
  {
    validator.init();
    nerrors = validator.validate(*doc);
    total_errors += nerrors;
    if (nerrors > 0) 
    {
      log->add(validator.getFailures() );
	  // DO NOT NEED THIS IN LAST CALL
      // /* only want to bail if errors not warnings */
      // if (log->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0)
      // {
      //   return total_errors;
      // }
    }
  }

  /* ADD OTHERS HERE */

  return total_errors;  
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
