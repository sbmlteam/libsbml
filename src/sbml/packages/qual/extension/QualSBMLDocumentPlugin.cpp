/**
 * @file    QualSBMLDocumentPlugin.cpp
 * @brief   Implementation of QualSBMLDocumentPlugin, the plugin class of
 *          qual package for the SBase element.
 * @author  Sarah Keating
 *
 *<!---------------------------------------------------------------------------
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
 *------------------------------------------------------------------------- -->
 */

//#include <ostream>
//
//#include <sbml/common/libsbml-version.h>
//#include <sbml/packages/qual/common/qualfwd.h>
#include <sbml/packages/qual/extension/QualSBMLDocumentPlugin.h>
#include <sbml/packages/qual/validator/QualConsistencyValidator.h>
#include <sbml/packages/qual/validator/QualIdentifierConsistencyValidator.h>
#include <sbml/packages/qual/validator/QualMathConsistencyValidator.h>
//#include <sbml/packages/qual/validator/QualValidator.h>
#include <sbml/packages/qual/validator/QualSBMLError.h>

//#include <iostream>

#ifdef __cplusplus

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

QualSBMLDocumentPlugin::QualSBMLDocumentPlugin (const string& uri, 
                              const string &prefix, QualPkgNamespaces *qualns)
  : SBMLDocumentPlugin(uri,prefix, qualns)
{
}


QualSBMLDocumentPlugin::QualSBMLDocumentPlugin(const QualSBMLDocumentPlugin& orig)
  : SBMLDocumentPlugin(orig)
{
}


QualSBMLDocumentPlugin& 
QualSBMLDocumentPlugin::operator=(const QualSBMLDocumentPlugin& orig)
{
  if(&orig!=this)
  {
    SBMLDocumentPlugin::operator =(orig);
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this QualSBMLDocumentPlugin object.
 */
QualSBMLDocumentPlugin* 
QualSBMLDocumentPlugin::clone () const
{
  return new QualSBMLDocumentPlugin(*this);  
}

/*
 * Destroy this object.
 */
QualSBMLDocumentPlugin::~QualSBMLDocumentPlugin () 
{
}


/** @cond doxygenLibsbmlInternal */
void 
QualSBMLDocumentPlugin::readAttributes (const XMLAttributes& attributes,
                            const ExpectedAttributes&)
{
  // for now don't read the required flag for L2 models 
  if (getSBMLDocument() != NULL && getSBMLDocument()->getLevel() < 3) return;
  
  unsigned int numErrs = getErrorLog()->getNumErrors();
  XMLTriple tripleRequired("required", mURI, getPrefix());
  bool assigned = attributes.readInto(tripleRequired, mRequired);
  if (assigned == false)
  {
    if (getErrorLog()->getNumErrors() == numErrs + 1 && 
        getErrorLog()->contains(XMLAttributeTypeMismatch))
    {
      getErrorLog()->remove(XMLAttributeTypeMismatch);
      getErrorLog()->logPackageError("qual", QualAttributeRequiredMustBeBoolean,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
    }
    else
    {
      getErrorLog()->logPackageError("qual", QualAttributeRequiredMissing,
        getPackageVersion(), getLevel(), getVersion(), "", getLine(), getColumn());
    }
  }
  else
  {
    mIsSetRequired = true;
    /* LOG ERROR RELATING TO EXPECTED VALUE */
    //if (mRequired == true)
    //{
    //  getErrorLog()->logPackageError("qual", ERROR,
    //    getPackageVersion(), getLevel(), getVersion());
    //}
  }
}
/** @endcond*/


/** @cond doxygenLibsbmlInternal */
bool
QualSBMLDocumentPlugin::isCompFlatteningImplemented() const
{
  return true;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
unsigned int 
QualSBMLDocumentPlugin::checkConsistency()
{
  unsigned int nerrors = 0;
  unsigned int total_errors = 0;

  SBMLDocument* doc = static_cast<SBMLDocument *>(this->getParentSBMLObject());
  SBMLErrorLog *log = doc->getErrorLog();

  unsigned char applicableValidators = doc->getApplicableValidators();

  /* determine which validators to run */
  bool id    = ((applicableValidators & 0x01) == 0x01);
  bool sbml  = ((applicableValidators & 0x02) == 0x02);
  bool math  = ((applicableValidators & 0x08) == 0x08);

  QualIdentifierConsistencyValidator id_validator;
  QualConsistencyValidator validator;
  QualMathConsistencyValidator math_validator;

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

  if (sbml)
  {
    validator.init();
    nerrors = validator.validate(*doc);
    total_errors += nerrors;
    if (nerrors > 0) 
    {
      log->add(validator.getFailures() );
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
      log->add( math_validator.getFailures() );
    }
  }
  return total_errors;  
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
bool 
QualSBMLDocumentPlugin::accept(SBMLVisitor& v) const
{
  const SBMLDocument *doc = static_cast<const SBMLDocument *>(this->getParentSBMLObject());
  v.visit(*doc);

  v.leave(*doc);

  return true;
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                     bool& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                     int& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                     double& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                     unsigned int& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                     std::string& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this QualSBMLDocumentPlugin's attribute
 * "attributeName" is set.
 */
bool
QualSBMLDocumentPlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBMLDocumentPlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                     bool value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                     int value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                     double value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                     unsigned int value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                     const std::string& value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * QualSBMLDocumentPlugin.
 */
int
QualSBMLDocumentPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBMLDocumentPlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */
LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
