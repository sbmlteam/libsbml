/**
 * @file    SBMLConverter.cpp
 * @brief   Implementation of SBMLConverter, the base class of package extensions.
 * @author  Sarah Keating
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
 
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/SBMLConstructorException.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>

#include <sbml/math/ASTNode.h>
#include <sbml/util/IdList.h>
#include <sbml/conversion/ExpressionAnalyser.h>
#include <sbml/Model.h>
#include <sbml/AssignmentRule.h>
#include <sbml/Rule.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN

SBMLConverter::SBMLConverter () :
    mDocument (NULL)
  , mOriginalModel(NULL)
  , mProps(NULL)
  , mName("")
{
}

SBMLConverter::SBMLConverter (const std::string& name)
  : mDocument (NULL)
  , mOriginalModel(NULL)
  , mProps(NULL)
  , mName(name)
{
}

/*
 * Copy constructor.
 */
SBMLConverter::SBMLConverter(const SBMLConverter& orig) :
    mDocument (orig.mDocument)
  , mOriginalModel(orig.mOriginalModel)
  , mProps(NULL)
  , mName(orig.mName)
{
  if (orig.mProps != NULL) 
  {
    mProps = new ConversionProperties(*orig.mProps);
  }  
}


/*
 * Destroy this object.
 */
SBMLConverter::~SBMLConverter ()
{
  if (mProps != NULL)
  {
    delete mProps;
    mProps = NULL;
  }
  if (mOriginalModel != NULL)
  {
      delete mOriginalModel;
      mOriginalModel = NULL;
  }
}


/*
 * Assignment operator for SBMLConverter.
 */
SBMLConverter& 
SBMLConverter::operator=(const SBMLConverter& rhs)
{  
  if(&rhs!=this)
  {
    mDocument = rhs.mDocument;
    mOriginalModel = rhs.mOriginalModel;
    mName = rhs.mName;
    
    if (mProps != NULL)
    {
      delete mProps;
      mProps = NULL;
    }
    
    if (rhs.mProps != NULL)
    {
      mProps = new ConversionProperties(*rhs.mProps);
    }
    else
    {
      mProps = NULL;
    }
  }

  return *this;
}


SBMLConverter*
SBMLConverter::clone () const
{
  return new SBMLConverter(*this);
}


SBMLDocument* 
SBMLConverter::getDocument()
{
  return mDocument;
}


const SBMLDocument* 
SBMLConverter::getDocument() const
{
  return mDocument;
}

int 
SBMLConverter::setDocument(const SBMLDocument* doc)
{
  if (mDocument == doc)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  mDocument = const_cast<SBMLDocument *> (doc);
  return LIBSBML_OPERATION_SUCCESS;
}



int 
SBMLConverter::setDocument(SBMLDocument* doc)
{
  if (mDocument == doc)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  mDocument = doc;
  return LIBSBML_OPERATION_SUCCESS;
}


const std::string&
SBMLConverter::getName() const
{
  return mName;
}

bool SBMLConverter::mathContainsId(const ASTNode* ast, const std::string& id) const
{
    bool present = false;
    List* variables = ast->getListOfNodes(ASTNode_isName);
    IdList vars;
    for (unsigned int i = 0; i < variables->getSize(); i++)
    {
        ASTNode* node = static_cast<ASTNode*>(variables->get(i));
        string   name = node->getName() ? node->getName() : "";
        vars.append(name);
    }
    if (vars.contains(id))
    {
        present = true;
    }
    delete variables;

    return present;
}

int
SBMLConverter::convert()
{
  return LIBSBML_OPERATION_FAILED;
}
  
bool 
SBMLConverter::matchesProperties(const ConversionProperties &) const
{
  return false;
}

SBMLNamespaces* 
SBMLConverter::getTargetNamespaces() 
{
  if (mProps == NULL) return NULL;
  return mProps->getTargetNamespaces();
}

int 
SBMLConverter::setProperties(const ConversionProperties *props)
{
  if (props == NULL) return LIBSBML_OPERATION_FAILED; 
  if (mProps != NULL)
  {
    delete mProps;
    mProps = NULL;
  }
  mProps = props->clone();
  return LIBSBML_OPERATION_SUCCESS;
}
  
ConversionProperties* 
SBMLConverter::getProperties() const
{
  return mProps;
}

ConversionProperties 
SBMLConverter::getDefaultProperties() const
{
  static ConversionProperties prop;
  return prop;
}

ASTNode*
SBMLConverter::replaceMathWithAssignedVariables(ASTNode* original)
{
    // there may be bits of the math that are actually assigned with an assignment rule
    // and therefore should be replaced
    // e.g. math equals 2 * k1 * A + k2 * B
    // in a model with an assignment rule k3 = 2 * k1 * A
    // so the math could become k3 + k2 * B
    unsigned int numAssignmentRules = 0;
    IdList assignmentRulesVariables = getListAssignmentRuleVariables(numAssignmentRules);
    if (numAssignmentRules == 0)
    {
        return original;
    }

    ExpressionAnalyser analyser;
    ASTNode* newMath = original->deepCopy();
    for (unsigned int i = 0; i < numAssignmentRules; i++)
    {
        AssignmentRule* ar = mOriginalModel->getAssignmentRule(assignmentRulesVariables.at(i));
        if (ar != NULL && ar->isSetMath() == true)
        {
            ASTNode* arMath = ar->getMath()->deepCopy();
            ASTNode* variable = new ASTNode(AST_NAME);
            variable->setName(ar->getVariable().c_str());
            //cout << "assignment rule " << i << ": " << SBML_formulaToL3String(arMath) << " variable " 
            //    << SBML_formulaToL3String(variable) << " original " << SBML_formulaToL3String(newMath) << endl;

            analyser.replaceExpressionInNodeWithNode(newMath, arMath, variable);
            //cout << "afterwards assignment rule " << i << ": " << SBML_formulaToL3String(arMath) << " variable "
            //    << SBML_formulaToL3String(variable) << " original " << SBML_formulaToL3String(newMath) << endl;
        }
    }
    return newMath;
}


IdList
SBMLConverter::getListAssignmentRuleVariables(unsigned int& numAssignmentRules)
{
    IdList assignmentRuleVariables;
    unsigned int numRules = mOriginalModel->getNumRules();
    for (unsigned int i = 0; i < numRules; i++)
    {
        Rule* r = mOriginalModel->getRule(i);
        if (r != NULL && r->isAssignment())
        {
            numAssignmentRules++;
            assignmentRuleVariables.append(r->getVariable());
        }
    }
    return assignmentRuleVariables;
}

ASTNode* SBMLConverter::replaceAssignedVariablesWithMath(ASTNode* original)
{
    // there may be bits of the math that are use a variable assigned with an assignment rule
    // and therefore should be replaced with the math
    // e.g. math equals k3 + k2 * B
    // in a model with an assignment rule k3 = 2 * k1 * A
    // so the math could become 2 * k1 * A + k2 * B
    unsigned int numAssignmentRules = 0;
    IdList assignmentRulesVariables = getListAssignmentRuleVariables(numAssignmentRules);
    if (numAssignmentRules == 0)
    {
        return original;
    }

    ExpressionAnalyser analyser;
    ASTNode* newMath = original->deepCopy();
    for (unsigned int i = 0; i < numAssignmentRules; i++)
    {
        AssignmentRule* ar = mOriginalModel->getAssignmentRule(assignmentRulesVariables.at(i));
        if (ar != NULL && ar->isSetMath() == true)
        {
            ASTNode* arMath = ar->getMath()->deepCopy();
            if (arMath->isCSymbolFunction() || arMath->getType() == AST_NAME_TIME)
            {
                // we cannot replace a CSymbolFunction as it may have different values in different contexts
                delete arMath;
                continue;
            }
            ASTNode* variable = new ASTNode(AST_NAME);
            variable->setName(ar->getVariable().c_str());
            analyser.replaceExpressionInNodeWithNode(newMath, variable, arMath);
        }
    }
    return newMath;
}
/** @cond doxygenIgnored */
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


