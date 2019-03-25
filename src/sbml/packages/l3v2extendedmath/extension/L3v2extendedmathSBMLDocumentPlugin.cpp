/**
 * @file L3v2extendedmathSBMLDocumentPlugin.cpp
 * @brief Implementation of the L3v2extendedmathSBMLDocumentPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/l3v2extendedmath/extension/L3v2extendedmathSBMLDocumentPlugin.h>
#include <sbml/packages/l3v2extendedmath/validator/L3v2extendedmathSBMLError.h>
#include <sbml/packages/l3v2extendedmath/validator/L3v2extendedmathMathMLConsistencyValidator.h>
#include <sbml/packages/l3v2extendedmath/validator/L3v2extendedmathUnitConsistencyValidator.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new L3v2extendedmathSBMLDocumentPlugin using the given URI, prefix
 * and package namespace.
 */
L3v2extendedmathSBMLDocumentPlugin::L3v2extendedmathSBMLDocumentPlugin(
                                                                       const
                                                                         std::string&
                                                                           uri,
                                                                       const
                                                                         std::string&
                                                                           prefix,
                                                                       L3v2extendedmathPkgNamespaces*
                                                                         l3v2extendedmathns)
  : SBMLDocumentPlugin(uri, prefix, l3v2extendedmathns)
{
}


/*
 * Copy constructor for L3v2extendedmathSBMLDocumentPlugin.
 */
L3v2extendedmathSBMLDocumentPlugin::L3v2extendedmathSBMLDocumentPlugin(const
  L3v2extendedmathSBMLDocumentPlugin& orig)
  : SBMLDocumentPlugin( orig )
{
}


/*
 * Assignment operator for L3v2extendedmathSBMLDocumentPlugin.
 */
L3v2extendedmathSBMLDocumentPlugin&
L3v2extendedmathSBMLDocumentPlugin::operator=(const
  L3v2extendedmathSBMLDocumentPlugin& rhs)
{
  if (&rhs != this)
  {
    SBMLDocumentPlugin::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this L3v2extendedmathSBMLDocumentPlugin
 * object.
 */
L3v2extendedmathSBMLDocumentPlugin*
L3v2extendedmathSBMLDocumentPlugin::clone() const
{
  return new L3v2extendedmathSBMLDocumentPlugin(*this);
}


/*
 * Destructor for L3v2extendedmathSBMLDocumentPlugin.
 */
L3v2extendedmathSBMLDocumentPlugin::~L3v2extendedmathSBMLDocumentPlugin()
{
}



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
L3v2extendedmathSBMLDocumentPlugin::accept(SBMLVisitor& v) const
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
 * L3v2extendedmath package.
 */
bool
L3v2extendedmathSBMLDocumentPlugin::isCompFlatteningImplemented() const
{
  return false;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Calls check consistency for any relevant L3v2extendedmath validators.
 */
unsigned int
L3v2extendedmathSBMLDocumentPlugin::checkConsistency()
{
  unsigned int nerrors = 0;
  unsigned int total_errors = 0;

  SBMLDocument* doc = static_cast<SBMLDocument*>(this->getParentSBMLObject());
  SBMLErrorLog* log = doc->getErrorLog();

  unsigned char applicableValidators = doc->getApplicableValidators();
  bool math = ((applicableValidators & 0x08) == 0x08);
  bool units = ((applicableValidators & 0x10) == 0x10);

  L3v2extendedmathMathMLConsistencyValidator math_validator;
  L3v2extendedmathUnitConsistencyValidator unit_validator;

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

  if (units)
  {
    unit_validator.init();
    nerrors = unit_validator.validate(*doc);
    total_errors += nerrors;
    if (nerrors > 0)
    {
      log->add(unit_validator.getFailures());
    }
  }

  return total_errors;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the L3v2extendedmath attributes in the top-level element.
 */
void
L3v2extendedmathSBMLDocumentPlugin::readAttributes(
                                                   const XMLAttributes&
                                                     attributes,
                                                   const ExpectedAttributes&
                                                     expectedAttributes)
{
  if (getSBMLDocument() != NULL && getSBMLDocument()->getLevel() < 3)
  {
    return;
  }
  else if (getSBMLDocument()->getVersion() > 1)
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
      log->logPackageError("l3v2extendedmath",
        L3v2extendedmathAttributeRequiredMustBeBoolean, getPackageVersion(),
          getLevel(), getVersion(), "", getLine(), getColumn());
    }
    else
    {
      log->logPackageError("l3v2extendedmath",
        L3v2extendedmathAttributeRequiredMissing, getPackageVersion(),
          getLevel(), getVersion(), "", getLine(), getColumn());
    }
  }
  else
  {
    mIsSetRequired = true;
    if (mRequired != true)
    {
      log->logPackageError("l3v2extendedmath",
        L3v2extendedmathAttributeRequiredMustHaveValue, getPackageVersion(),
          getLevel(), getVersion(), "", getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::getAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 bool& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::getAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 int& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::getAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 double& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::getAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 unsigned int& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::getAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 std::string& value) const
{
  int return_value = SBMLDocumentPlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this L3v2extendedmathSBMLDocumentPlugin's
 * attribute "attributeName" is set.
 */
bool
L3v2extendedmathSBMLDocumentPlugin::isSetAttribute(const std::string&
  attributeName) const
{
  bool value = SBMLDocumentPlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::setAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 bool value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::setAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 int value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::setAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 double value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::setAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 unsigned int value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::setAttribute(
                                                 const std::string&
                                                   attributeName,
                                                 const std::string& value)
{
  int return_value = SBMLDocumentPlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * L3v2extendedmathSBMLDocumentPlugin.
 */
int
L3v2extendedmathSBMLDocumentPlugin::unsetAttribute(const std::string&
  attributeName)
{
  int value = SBMLDocumentPlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_END


