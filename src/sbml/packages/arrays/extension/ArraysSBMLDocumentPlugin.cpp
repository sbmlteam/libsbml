/**
 * @file ArraysSBMLDocumentPlugin.cpp
 * @brief Implementation of the ArraysSBMLDocumentPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/arrays/extension/ArraysSBMLDocumentPlugin.h>
#include <sbml/packages/arrays/validator/ArraysSBMLError.h>
#include <sbml/packages/arrays/validator/ArraysConsistencyValidator.h>
#include <sbml/packages/arrays/validator/ArraysIdentifierConsistencyValidator.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ArraysSBMLDocumentPlugin using the given uri, prefix and
 * package namespace.
 */
ArraysSBMLDocumentPlugin::ArraysSBMLDocumentPlugin(const std::string& uri,
                                                   const std::string& prefix,
                                                   ArraysPkgNamespaces*
                                                     arraysns)
  : SBMLDocumentPlugin(uri, prefix, arraysns)
{
}


/*
 * Copy constructor for ArraysSBMLDocumentPlugin.
 */
ArraysSBMLDocumentPlugin::ArraysSBMLDocumentPlugin(const
  ArraysSBMLDocumentPlugin& orig)
  : SBMLDocumentPlugin( orig )
{
}


/*
 * Assignment operator for ArraysSBMLDocumentPlugin.
 */
ArraysSBMLDocumentPlugin&
ArraysSBMLDocumentPlugin::operator=(const ArraysSBMLDocumentPlugin& rhs)
{
  if (&rhs != this)
  {
    SBMLDocumentPlugin::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ArraysSBMLDocumentPlugin object.
 */
ArraysSBMLDocumentPlugin*
ArraysSBMLDocumentPlugin::clone() const
{
  return new ArraysSBMLDocumentPlugin(*this);
}


/*
 * Destructor for ArraysSBMLDocumentPlugin.
 */
ArraysSBMLDocumentPlugin::~ArraysSBMLDocumentPlugin()
{
}



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
ArraysSBMLDocumentPlugin::accept(SBMLVisitor& v) const
{
  const SBMLDocument* sbmld = static_cast<const
    SBMLDocument*>(this->getParentSBMLObject());
  v.visit(*sbmld);
  v.leave(*sbmld);

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate indicating whether 'comp' flattening has been implemented for the
 * Arrays package.
 */
bool
ArraysSBMLDocumentPlugin::isCompFlatteningImplemented() const
{
  return false;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Calls check consistency for any relevant Arrays validators.
 */
unsigned int
ArraysSBMLDocumentPlugin::checkConsistency()
{
  unsigned int nerrors = 0;
  unsigned int total_errors = 0;

  SBMLDocument* doc = static_cast<SBMLDocument*>(this->getParentSBMLObject());
  SBMLErrorLog* log = doc->getErrorLog();

  unsigned char applicableValidators = doc->getApplicableValidators();
  // disable for now
  bool id = false;//((applicableValidators & 0x01) ==0x01);
  bool core = false;//((applicableValidators & 0x02) ==0x02);

  ArraysIdentifierConsistencyValidator id_validator;
  ArraysConsistencyValidator core_validator;

  if (id)
  {
    id_validator.init();
    nerrors = id_validator.validate(*doc);
    total_errors += nerrors;
    if (nerrors > 0)
    {
      log->add(id_validator.getFailures());
      if (log->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0)
      {
        return total_errors;
      }
    }
  }

  if (core)
  {
    core_validator.init();
    nerrors = core_validator.validate(*doc);
    total_errors += nerrors;
    if (nerrors > 0)
    {
      log->add(core_validator.getFailures());
      if (log->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0)
      {
        return total_errors;
      }
    }
  }

  return total_errors;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the Arrays attributes in the top-level element.
 */
void
ArraysSBMLDocumentPlugin::readAttributes(const XMLAttributes& attributes,
                                         const ExpectedAttributes&
                                           expectedAttributes)
{
  if (getSBMLDocument() != NULL && getSBMLDocument()->getLevel() < 3)
  {
    return;
  }

  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs = log->getNumErrors();
  XMLTriple tripleReqd("required", mURI, getPrefix());
  bool assigned = attributes.readInto(tripleReqd, mRequired);

  if (assigned == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      log->logPackageError("arrays", ArraysAttributeRequiredMustBeBoolean,
        getPackageVersion(), getLevel(), getVersion());
    }
    else
    {
      log->logPackageError("arrays", ArraysAttributeRequiredMissing,
        getPackageVersion(), getLevel(), getVersion());
    }
  }
  else
  {
    mIsSetRequired = true;
    if (mRequired != true)
    {
      log->logPackageError("arrays", ArraysAttributeRequiredMustHaveValue,
        getPackageVersion(), getLevel(), getVersion());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                       bool& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                       int& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                       double& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                       unsigned int& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                       std::string& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this ArraysSBMLDocumentPlugin's attribute
 * "attributeName" is set.
 */
bool
ArraysSBMLDocumentPlugin::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = SBMLDocumentPlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                       bool value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                       int value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                       double value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                       unsigned int value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                       const std::string& value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * ArraysSBMLDocumentPlugin.
 */
int
ArraysSBMLDocumentPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBMLDocumentPlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_END


