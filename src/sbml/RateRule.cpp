/**
 * @file    RateRule.cpp
 * @brief   Implementations of RateRule.
 * @author  Ben Bornstein
 *
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
 * ---------------------------------------------------------------------- -->*/

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLNamespaces.h>

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#include <sbml/SBO.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/RateRule.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

RateRule::RateRule (unsigned int level, unsigned int version) :
  Rule(SBML_RATE_RULE, level, version)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}

RateRule::RateRule (SBMLNamespaces *sbmlns) :
  Rule(SBML_RATE_RULE, sbmlns)
{
  if (!hasValidLevelVersionNamespaceCombination())
  {
    throw SBMLConstructorException(getElementName(), sbmlns);
  }

  loadPlugins(sbmlns);
}


/*
 * Destroys this RateRule.
 */
RateRule::~RateRule ()
{
}

/*
 * @return a (deep) copy of this Rule.
 */
RateRule*
RateRule::clone () const
{
  return new RateRule(*this);
}


bool 
RateRule::hasRequiredAttributes() const
{
  bool allPresent = Rule::hasRequiredAttributes();

  /* required attributes for rateRule: variable (comp/species/name in L1) */

  if (!isSetVariable())
    allPresent = false;

  return allPresent;
}




/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = Rule::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = Rule::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = Rule::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::getAttribute(const std::string& attributeName,
                       unsigned int& value) const
{
  int return_value = Rule::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::getAttribute(const std::string& attributeName,
                       std::string& value) const
{
  int return_value = LIBSBML_OPERATION_FAILED;
  if (getLevel() > 1)
  {
    return_value = Rule::getAttribute(attributeName, value);
  }

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "variable")
  {
    value = getVariable();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    int l1type = getL1TypeCode();
    if ((attributeName == "name" && l1type == SBML_PARAMETER_RULE) 
      || (attributeName == "compartment" && l1type == SBML_COMPARTMENT_VOLUME_RULE) 
      || (attributeName == "species" && l1type == SBML_SPECIES_CONCENTRATION_RULE))
    {
      value = getVariable();
      return_value = LIBSBML_OPERATION_SUCCESS;
    }
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this RateRule.
 */
//int
//RateRule::getAttribute(const std::string& attributeName,
//                       const char* value) const
//{
//  int return_value = LIBSBML_OPERATION_FAILED;
//  if (getLevel() > 1)
//  {
//    return_value = Rule::getAttribute(attributeName, value);
//  }
//
//  if (return_value == LIBSBML_OPERATION_SUCCESS)
//  {
//    return return_value;
//  }
//
//  if (attributeName == "variable")
//  {
//    value = getVariable().c_str();
//    return_value = LIBSBML_OPERATION_SUCCESS;
//  }
//  else
//  {
//    int l1type = getL1TypeCode();
//    if ((attributeName == "name" && l1type == SBML_PARAMETER_RULE) 
//      || (attributeName == "compartment" && l1type == SBML_COMPARTMENT_VOLUME_RULE) 
//      || (attributeName == "species" && l1type == SBML_SPECIES_CONCENTRATION_RULE))
//    {
//      value = getVariable().c_str();
//      return_value = LIBSBML_OPERATION_SUCCESS;
//    }
//  }
//
//  return return_value;
//}
//
/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this RateRule's attribute "attributeName" is
 * set.
 */
bool
RateRule::isSetAttribute(const std::string& attributeName) const
{
  bool value = false;
  if (getLevel() > 1)
  {
    value = Rule::isSetAttribute(attributeName);
  }

  if (attributeName == "variable")
  {
    value = isSetVariable();
  }
  else
  {
    int l1type = getL1TypeCode();
    if ((attributeName == "name" && l1type == SBML_PARAMETER_RULE) 
      || (attributeName == "compartment" && l1type == SBML_COMPARTMENT_VOLUME_RULE) 
      || (attributeName == "species" && l1type == SBML_SPECIES_CONCENTRATION_RULE))
    {
      value = isSetVariable();
    }
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = Rule::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::setAttribute(const std::string& attributeName, int value)
{
  int return_value = Rule::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::setAttribute(const std::string& attributeName, double value)
{
  int return_value = Rule::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = Rule::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::setAttribute(const std::string& attributeName,
                       const std::string& value)
{
  int return_value = LIBSBML_OPERATION_FAILED;
  if (getLevel() > 1)
  {
    return_value = Rule::setAttribute(attributeName, value);
  }

  if (attributeName == "variable")
  {
    return_value = setVariable(value);
  }
  else
  {
    int l1type = getL1TypeCode();
    if ((attributeName == "name" && l1type == SBML_PARAMETER_RULE) 
      || (attributeName == "compartment" && l1type == SBML_COMPARTMENT_VOLUME_RULE) 
      || (attributeName == "species" && l1type == SBML_SPECIES_CONCENTRATION_RULE))
    {
      return_value = setVariable(value);
    }
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RateRule.
 */
//int
//RateRule::setAttribute(const std::string& attributeName, const char* value)
//{
//  int return_value = LIBSBML_OPERATION_FAILED;
//  if (getLevel() > 1)
//  {
//    return_value = Rule::setAttribute(attributeName, value);
//  }
//
//  if (attributeName == "variable")
//  {
//    return_value = setVariable(value);
//  }
//  else
//  {
//    int l1type = getL1TypeCode();
//    if ((attributeName == "name" && l1type == SBML_PARAMETER_RULE) 
//      || (attributeName == "compartment" && l1type == SBML_COMPARTMENT_VOLUME_RULE) 
//      || (attributeName == "species" && l1type == SBML_SPECIES_CONCENTRATION_RULE))
//    {
//        return_value = setVariable(value);
//    }
//  }
//
//  return return_value;
//}
//
/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this RateRule.
 */
int
RateRule::unsetAttribute(const std::string& attributeName)
{
  int return_value = LIBSBML_OPERATION_FAILED;
  if (getLevel() > 1)
  {
    return_value = Rule::unsetAttribute(attributeName);
  }

  if (attributeName == "variable")
  {
    return_value = unsetVariable();
  }
  else
  {
    int l1type = getL1TypeCode();
    if ((attributeName == "name" && l1type == SBML_PARAMETER_RULE) 
      || (attributeName == "compartment" && l1type == SBML_COMPARTMENT_VOLUME_RULE) 
      || (attributeName == "species" && l1type == SBML_SPECIES_CONCENTRATION_RULE))
    {
      return_value = unsetVariable();
    }
  }

  return return_value;
}

/** @endcond */







/** @cond doxygenLibsbmlInternal */
bool
RateRule::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}
/** @endcond */


void
RateRule::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  Rule::renameSIdRefs(oldid, newid);
  if (isSetVariable()) {
    if (getVariable()==oldid) {
      setVariable(newid);
    }
  }
}

#endif /* __cplusplus */


/** @cond doxygenIgnored */
/** @endcond */


LIBSBML_EXTERN
RateRule_t *
RateRule_create(unsigned int level, unsigned int version)
{
  try
  {
    RateRule* obj = new RateRule(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


LIBSBML_EXTERN
RateRule_t *
RateRule_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    RateRule* obj = new RateRule(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


LIBSBML_EXTERN
void
RateRule_free(RateRule_t * rr)
{
  if (rr != NULL)
    delete rr;
}


LIBSBML_EXTERN
RateRule_t *
RateRule_clone(RateRule_t * rr)
{
  if (rr != NULL)
  {
    return static_cast<RateRule_t*>(rr->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
RateRule_getVariable(const RateRule_t * rr)
{
	return (rr != NULL && rr->isSetVariable()) ? rr->getVariable().c_str() : NULL;
}


LIBSBML_EXTERN
const ASTNode_t*
RateRule_getMath(const RateRule_t * rr)
{
	if (rr == NULL)
		return NULL;

	return (ASTNode_t*)(rr->getMath());
}


LIBSBML_EXTERN
const char *
RateRule_getFormula (const RateRule_t *r)
{
  return (r != NULL && r->isSetFormula()) ? r->getFormula().c_str() : NULL;
}


LIBSBML_EXTERN
int
RateRule_isSetVariable(const RateRule_t * rr)
{
  return (rr != NULL) ? static_cast<int>(rr->isSetVariable()) : 0;
}


LIBSBML_EXTERN
int
RateRule_isSetMath(const RateRule_t * rr)
{
  return (rr != NULL) ? static_cast<int>(rr->isSetMath()) : 0;
}


LIBSBML_EXTERN
int
RateRule_isSetFormula (const RateRule_t *r)
{
  return (r != NULL) ? static_cast<int>( r->isSetFormula() ) : 0;
}


LIBSBML_EXTERN
int
RateRule_setVariable(RateRule_t * rr, const char * variable)
{
  if (rr != NULL)
    return (variable == NULL) ? rr->setVariable("") : rr->setVariable(variable);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
RateRule_setMath(RateRule_t * rr, const ASTNode_t* math)
{
	return (rr != NULL) ? rr->setMath(math) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
RateRule_setFormula (RateRule_t *r, const char *formula)
{
  if (r != NULL)
    return (formula == NULL) ? r->setMath(NULL) : r->setFormula(formula);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
RateRule_unsetVariable(RateRule_t * ar)
{
  return (ar != NULL) ? static_cast<int>(ar->unsetVariable()) 
                      : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
RateRule_hasRequiredAttributes(const RateRule_t * rr)
{
  return (rr != NULL) ? static_cast<int>(rr->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
RateRule_hasRequiredElements(const RateRule_t * rr)
{
	return (rr != NULL) ? static_cast<int>(rr->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END
