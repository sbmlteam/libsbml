/**
 * @file    VConstraint.cpp
 * @brief   Base class for all SBML Validator Constraints
 * @author  Ben Bornstein
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/validator/VConstraint.h>
#include <sbml/extension/SBasePlugin.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

VConstraint::VConstraint (unsigned int id, Validator& v) :
    mId       ( id   )
  , mSeverity ( 2    )
  , mValidator( v    )
  , mLogMsg   ( true )
{
}


VConstraint::~VConstraint ()
{
}


/*
 * @return the id of this Constraint.
 */
unsigned int
VConstraint::getId () const
{
  return mId;
}


/*
 * @return the severity for violating this Constraint.
 */
unsigned int
VConstraint::getSeverity () const
{
  return mSeverity;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Logs a constraint failure to the validator for the given SBML object.
 */
void
VConstraint::logFailure (const SBase& object)
{
  logFailure(object, msg);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Logs a constraint failure to the validator for the given SBML object.
 * The parameter message is used instead of the constraint's member
 * variable msg.
 */
void
VConstraint::logFailure (const SBase& object, const std::string& message)
{
  std::string pkg = object.getPackageName();
  unsigned int pkgVersion = object.getPackageVersion();
  if (mId > 99999 && pkg == "core")
  {
    // we are dealing with a core object that is logging errors 
    // relating to a package
    // need to work out which pkg

    unsigned int offset = (unsigned int)(floor((double)mId/100000.0)) * 100000;

    if (offset == 9900000)
    {
      // we are dealing with the strict units validator
      mId = mId - offset;
    }
    else if (offset == 1400000 && object.getLevel() == 3 && object.getVersion() == 2)
    {
      // we are using the l3v2extended math package but in l3v2 which means we want to report core
      mId = mId - offset;
    }
    else
    {
      // it is possible that the object does not have a direct plugin
      // it may the child of an object that does
      // so lets cut straight to the parent document
      const SBMLDocument * doc = object.getSBMLDocument();
      if (doc != NULL)
      {
        for (unsigned int i = 0; i < doc->getNumPlugins(); i++)
        {
          const SBMLExtension * ext = doc->getPlugin(i)->getSBMLExtension();

          if (ext->getErrorIdOffset() == offset)
          {
            pkg = doc->getPlugin(i)->getPackageName();
            pkgVersion = doc->getPlugin(i)->getPackageVersion();
            break;
          }
        }
      }
    }
  }

  // if we are usinga  consistency validator we want the level and version
  // of the target sbml - which we have conveniently saved in the validator
  // but for now only with 98000 numbers
  unsigned int level = object.getLevel();
  unsigned int version = object.getVersion();
  if ((98000 < mId) && (mId < 98999))
  {
    if (mValidator.getConsistencyLevel() != 0)
    {
      level = mValidator.getConsistencyLevel();
      version = mValidator.getConsistencyVersion();
    }
  }

  SBMLError error = SBMLError( mId, level, version,
			       message, object.getLine(), object.getColumn(),
             LIBSBML_SEV_ERROR, LIBSBML_CAT_SBML, pkg, pkgVersion);

  if (error.getSeverity() != LIBSBML_SEV_NOT_APPLICABLE)
    mValidator.logFailure(error);

/*    ( SBMLError( mId, object.getLevel(), object.getVersion(),
                 message, object.getLine(), object.getColumn(),
                 LIBSBML_SEV_ERROR, LIBSBML_CAT_SBML ));*/

}
/** @endcond */

#endif /* __cplusplus */


/** @cond doxygenIgnored */
/** @endcond */
LIBSBML_CPP_NAMESPACE_END

