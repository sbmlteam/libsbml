/**
 * @file SpatialSBMLDocumentPlugin.cpp
 * @brief Implementation of the SpatialSBMLDocumentPlugin class.
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
#include <sbml/packages/spatial/extension/SpatialSBMLDocumentPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/packages/spatial/validator/SpatialConsistencyValidator.h>
#include <sbml/packages/spatial/validator/SpatialIdentifierConsistencyValidator.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new SpatialSBMLDocumentPlugin using the given URI, prefix and
 * package namespace.
 */
SpatialSBMLDocumentPlugin::SpatialSBMLDocumentPlugin(const std::string& uri,
                                                     const std::string& prefix,
                                                     SpatialPkgNamespaces*
                                                       spatialns)
  : SBMLDocumentPlugin(uri, prefix, spatialns)
{
}


/*
 * Copy constructor for SpatialSBMLDocumentPlugin.
 */
SpatialSBMLDocumentPlugin::SpatialSBMLDocumentPlugin(const
  SpatialSBMLDocumentPlugin& orig)
  : SBMLDocumentPlugin( orig )
{
}


/*
 * Assignment operator for SpatialSBMLDocumentPlugin.
 */
SpatialSBMLDocumentPlugin&
SpatialSBMLDocumentPlugin::operator=(const SpatialSBMLDocumentPlugin& rhs)
{
  if (&rhs != this)
  {
    SBMLDocumentPlugin::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialSBMLDocumentPlugin object.
 */
SpatialSBMLDocumentPlugin*
SpatialSBMLDocumentPlugin::clone() const
{
  return new SpatialSBMLDocumentPlugin(*this);
}


/*
 * Destructor for SpatialSBMLDocumentPlugin.
 */
SpatialSBMLDocumentPlugin::~SpatialSBMLDocumentPlugin()
{
}



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
SpatialSBMLDocumentPlugin::accept(SBMLVisitor& v) const
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
 * Spatial package.
 */
bool
SpatialSBMLDocumentPlugin::isCompFlatteningImplemented() const
{
  return false;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Calls check consistency for any relevant Spatial validators.
 */
unsigned int
SpatialSBMLDocumentPlugin::checkConsistency()
{
  unsigned int nerrors = 0;
  unsigned int total_errors = 0;

  SBMLDocument* doc = static_cast<SBMLDocument*>(this->getParentSBMLObject());
  SBMLErrorLog* log = doc->getErrorLog();

  unsigned char applicableValidators = doc->getApplicableValidators();
  bool id = ((applicableValidators & 0x01) ==0x01);
  bool core = ((applicableValidators & 0x02) ==0x02);

  SpatialIdentifierConsistencyValidator id_validator;
  SpatialConsistencyValidator core_validator;

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
 * Reads the Spatial attributes in the top-level element.
 */
void
SpatialSBMLDocumentPlugin::readAttributes(const XMLAttributes& attributes,
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
      log->logPackageError("spatial", SpatialAttributeRequiredMustBeBoolean,
        getPackageVersion(), getLevel(), getVersion());
    }
    else
    {
      log->logPackageError("spatial", SpatialAttributeRequiredMissing,
        getPackageVersion(), getLevel(), getVersion());
    }
  }
  else
  {
    mIsSetRequired = true;
    if (mRequired != true)
    {
      log->logPackageError("spatial", SpatialAttributeRequiredMustHaveValue,
        getPackageVersion(), getLevel(), getVersion());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                        bool& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                        int& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                        double& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                        unsigned int& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::getAttribute(const std::string& attributeName,
                                        std::string& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SpatialSBMLDocumentPlugin's attribute
 * "attributeName" is set.
 */
bool
SpatialSBMLDocumentPlugin::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = SBMLDocumentPlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                        bool value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                        int value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                        double value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                        unsigned int value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::setAttribute(const std::string& attributeName,
                                        const std::string& value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * SpatialSBMLDocumentPlugin.
 */
int
SpatialSBMLDocumentPlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBMLDocumentPlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_END


