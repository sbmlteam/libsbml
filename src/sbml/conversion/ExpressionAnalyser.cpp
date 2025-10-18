/**
 * @file    ExpressionAnalyser.cpp
 * @brief   Implementation of ExpressionAnalyser
 * @author  Sarah Keating
 * @author  Alessandro Felder
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more

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


#include <sbml/conversion/ExpressionAnalyser.h>


#include <algorithm>
#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <sbml/SBMLTypes.h>
#include <sbml/math/ASTNodeType.h>
#include <sbml/conversion/SBMLRateRuleConverter.h>

#ifdef __cplusplus

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

struct compareExpressions
{
    // function to order expressions based on their type
    bool operator() (SubstitutionValues_t* values1, SubstitutionValues_t* values2)
    {
        if (values1 == NULL || values2 == NULL)
            return false;
        if (values1->type == TYPE_UNKNOWN || values2->type == TYPE_UNKNOWN)
            return false;
        if (values1->type < values2->type)
            return true;
        return false;
    }
};

pairODEs ExpressionAnalyser::deepCopyODEs(pairODEs odes)
{
    pairODEs newOdes;
    for (odeIt it = odes.begin(); it != odes.end(); ++it)
    {
        newOdes.push_back(std::make_pair(it->first, it->second->deepCopy()));
    }

    return newOdes;
}



ExpressionAnalyser::ExpressionAnalyser()
    : mModel (NULL), 
      mODEs (),
    mNewVarName("newVar"),
    mNewVarCount(1),
    mHiddenSpecies (NULL),
    mHiddenNodes (NULL)
{
}



ExpressionAnalyser::ExpressionAnalyser(Model * m, pairODEs odes)
    : mModel(m),
    mNewVarName("newVar"),
    mNewVarCount(1),
    mHiddenSpecies(NULL),
    mHiddenNodes(NULL)
{
  SBMLTransforms::mapComponentValues(mModel);
  mModel->populateAllElementIdList();
  mODEs = deepCopyODEs(odes);
}

ExpressionAnalyser::ExpressionAnalyser(const ExpressionAnalyser& orig) :
  mModel( orig.mModel),
    mODEs(orig.mODEs),
    mNewVarName(orig.mNewVarName),
    mNewVarCount(orig.mNewVarCount),
    mHiddenSpecies(orig.mHiddenSpecies),
    mHiddenNodes(orig.mHiddenNodes)
{
    SBMLTransforms::mapComponentValues(mModel);
    mModel->populateAllElementIdList();
}

/*
* Assignment operator for SBMLLevelVersionConverter.
*/
ExpressionAnalyser&
ExpressionAnalyser::operator=(const ExpressionAnalyser& rhs)
{
  if (&rhs != this)
  {
    mModel = rhs.mModel;
    mODEs = rhs.mODEs;
    mNewVarName = rhs.mNewVarName;
    mNewVarCount = rhs.mNewVarCount;
    mHiddenSpecies = rhs.mHiddenSpecies;
    mHiddenNodes = rhs.mHiddenNodes;
  }
  SBMLTransforms::mapComponentValues(mModel);
  mModel->populateAllElementIdList();


  return *this;
}

ExpressionAnalyser* 
ExpressionAnalyser::clone() const
{
  return new ExpressionAnalyser(*this);
}

/*
 * Destroy this object.
 */
ExpressionAnalyser::~ExpressionAnalyser ()
{
  // note the odes are owned by the converter
  SBMLTransforms::clearComponentValues(mModel);
  mHiddenSpecies = NULL;
  mHiddenNodes = NULL;
  if (mExpressions.size() > 0)
  {
      for (unsigned int i = 0; i < mExpressions.size(); i++)
      {
          SubstitutionValues_t* value = mExpressions.at(i);
          delete value->dxdt_expression;
          delete value->dydt_expression;
          delete value->v_expression;
          delete value->w_expression;
          delete value;
      }
      mExpressions.clear();
  }
}

void ExpressionAnalyser::identifyHiddenSpeciesWithinExpressions()
{
    if (mExpressions.empty())
        return;
    if (mHiddenNodes == NULL)
        mHiddenNodes = new List();

    // need to actually address the expressions in the correct order
   // replace k-x-y first with newParam type=TYPE_K_MINUS_X_MINUS_Y
   // and then k+v-x-y with newParam+v  type=TYPE_K_PLUS_V_MINUS_X_MINUS_Y
   // and then k-x+w-y with newParam+w  type=TYPE_K_MINUS_X_PLUS_W_MINUS_Y
    // and then k-x with newParam2      type=TYPE_K_MINUS_X
    // and then k+v-x with newParam2+v  type=TYPE_K_PLUS_V_MINUS_X  


    // need to go through the expressions and make sure that they are substituting the correct thing
    // this needs to be done before any parameters are substituted I think
    int i = -1;
    for (unsigned int j = 0; j < mExpressions.size(); j++)
    {
        SubstitutionValues_t* exp = mExpressions[j];
        // if this is the first expression then we need to create a new parameter 
        // and cannot rely on any other expression
        if (j == 0)
        {
            if (exp->type == TYPE_K_MINUS_X_MINUS_Y || 
                exp->type == TYPE_K_MINUS_X)
            {
                // we have a value for  k-x-y or k-x
                addSingleNewParameter(exp);
            }
            else if (exp->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y ||
                exp->type == TYPE_K_PLUS_V_MINUS_X)
            {
                // we have a value for  k+v-x-y and no value for k-x-y
                // we have a value for  k+v-x and no value for k-x
                addNewParameterPlusVOrW(exp);
            }
            else if (exp->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y)
            {
                // we have a value for  k-x+w-y and no value for k-x-y
                addNewParameterPlusVOrW(exp, "w");
            }
        }
        else // j > 0 so not the first expression
        {
            if (exp->type == TYPE_K_MINUS_X_MINUS_Y ||
                exp->type == TYPE_K_MINUS_X)
            {
                // we have a different value for  k-x-y 
                // we know this because the expressions are in order and k-x-y is always first
                // and the code that decides whether to add the expression well have checked that it is not identical
                // so we if we have an expression of this type but is not first then it must be
                // k-x-y but with a different value
                // similarly for k-x
                addSingleNewParameter(exp);
            }
            else if (exp->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y ||
                exp->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y ||
                exp->type == TYPE_K_PLUS_V_MINUS_X)
            {
                // the rules will be the same for k+v-x-y and k-x+w-y   but must have k-x-y before
                // and k+v-x which must have k-x before
                // so need to determine the type of the expression
                ExpressionType_t type = exp->type;
                ExpressionType_t parentType = (type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y || type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y)
                    ? TYPE_K_MINUS_X_MINUS_Y : TYPE_K_MINUS_X;
                std::string vOrW = (type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y || type == TYPE_K_PLUS_V_MINUS_X) ? "v" : "w";

                if (getSubstitutionValuesByType(parentType) != NULL)
                {
                    // here we are dealing with the fact that we have k+v-x-y and k-x-y
                    // need to check whether they have the same values for k,x,y
                    // but need to know which expression as we might have two of the same k-x-y k-x-a
                    i = getMatchingParentExpression(exp, j);
                    if (i != -1)
                    {
                        // the values are the same so we can use the same new parameter
                        addPreviousParameterPlusVOrW(exp, mExpressions[i], vOrW);
                    }
                    else
                    {
                        addNewParameterPlusVOrW(exp, vOrW);
                    }
                }
                else if (mExpressions[j - 1]->type == type)
                {
                    // here we are dealing with the fact that we have k+v-x-y and k+v-x-a
                    // so we have two different expressions of the same type
                    // need to create a new parameter 
                    addNewParameterPlusVOrW(exp, vOrW);
                }
                else if (type == TYPE_K_PLUS_V_MINUS_X)
                {
                    // we have k+v-x but no value for k-x so we must create a new parameter
                    addNewParameterPlusVOrW(exp, "v");
                }
            }
        }
    }
}

SubstitutionValues_t* 
ExpressionAnalyser::getSubstitutionValuesByType(ExpressionType_t type)
{
    unsigned int index = 0;
    SubstitutionValues_t* exp = NULL;

    while (exp == NULL && index < mExpressions.size())
    {
        exp = mExpressions[index];
        if (exp->type == type)
        {
            index++;
            return exp;
        }
        else
        {
            exp = NULL;
            index++;
        }
    }
    return NULL;
}

int 
ExpressionAnalyser::getMatchingParentExpression(SubstitutionValues_t* value, unsigned int index)
{
    int matchingIndex = -1;
    for (unsigned int i = 0; i < index; i++)
    {
        if (matchesK(mExpressions[i], value) &&
            matchesXValue(mExpressions[i], value) &&
            matchesYValue(mExpressions[i], value))
        {
            matchingIndex = i;
            break;
        }
    }
    return matchingIndex;
}

void ExpressionAnalyser::addSingleNewParameter(SubstitutionValues_t* exp)
{
    std::string zName = getUniqueNewParameterName();
    exp->z_value = zName;
    mNewVarCount++;
    ASTNode* z = new ASTNode(AST_NAME);
    z->setName(zName.c_str());
    exp->z_expression = z->deepCopy();
    mHiddenNodes->add(z);
    delete z;

}

void ExpressionAnalyser::addNewParameterPlusVOrW(SubstitutionValues_t* exp, std::string vOrW)
{
    ASTNode* var = (vOrW == "v") ? exp->v_expression->deepCopy() : exp->w_expression->deepCopy();    
    std::string zName = getUniqueNewParameterName();
    exp->z_value = zName;
    mNewVarCount++;
    ASTNode* z = new ASTNode(AST_NAME);
    z->setName(zName.c_str());
    ASTNode* replacement = new ASTNode(AST_PLUS);
    replacement->addChild(z);
    replacement->addChild(var);
    exp->z_expression = replacement->deepCopy();
    mHiddenNodes->add(z);
    delete replacement;
}

void ExpressionAnalyser::addPreviousParameterPlusVOrW(SubstitutionValues_t* exp, SubstitutionValues_t* previous, std::string vOrW)
{
    ASTNode* var = (vOrW == "v") ? exp->v_expression->deepCopy() : exp->w_expression->deepCopy();
    exp->z_value = previous->z_value;
    ASTNode* z = new ASTNode(AST_NAME);
    z->setName(previous->z_value.c_str());
    ASTNode* replacement = new ASTNode(AST_PLUS);
    replacement->addChild(z);
    replacement->addChild(var);
    exp->z_expression = replacement->deepCopy();
    delete replacement;

}



/*
* Check whether two SubstitutionValues_t match the values that we expect if we need to add them
* based on the type of the SubstitutionValues_t
 */
bool ExpressionAnalyser::expressionExists(SubstitutionValues_t* current, 
    SubstitutionValues_t* mightAdd)
{
    bool alreadyExists = false;
    // all expressions will have K and X
    alreadyExists = matchesType(current, mightAdd) &&
        matchesK(current, mightAdd) &&
        matchesXValue(current, mightAdd) &&
        matchesDxdtExpression(current, mightAdd);

    if (alreadyExists)
    {
        switch (current->type)
        {
        case TYPE_K_MINUS_X_MINUS_Y:
            alreadyExists = alreadyExists && matchesYValue(current, mightAdd) &&
                matchesDydtExpression(current, mightAdd);
            break;
        case TYPE_K_PLUS_V_MINUS_X_MINUS_Y:
            alreadyExists = alreadyExists && matchesVExpression(current, mightAdd) &&
                matchesYValue(current, mightAdd);
            break;
        case TYPE_K_MINUS_X_PLUS_W_MINUS_Y:
            alreadyExists = alreadyExists && matchesWExpression(current, mightAdd) &&
                matchesYValue(current, mightAdd);
            break;
        default:
            break;
        }
        return alreadyExists;
    }

    return alreadyExists;
}
bool ExpressionAnalyser::parentExpressionExists(SubstitutionValues_t* current, SubstitutionValues_t* mightAdd)
{
    bool parentExists = false;

    // if the expressions are on the same level then one cannot be the child of another
    if (current->levelInExpression == mightAdd->levelInExpression)
    {
        return false;
    }

    // here we want to find out if the expression is actually a child of another 
    // and therefore need not be logged
    // all expressions will have K and X but the type may not be the same
    parentExists = matchesK(current, mightAdd) &&
        matchesXValue(current, mightAdd) &&
        matchesDxdtExpression(current, mightAdd);

    if (parentExists)
    {
        switch (current->type)
        {
        case TYPE_K_MINUS_X_MINUS_Y:
            parentExists = mightAdd->type == TYPE_K_MINUS_X;
            break;
        case TYPE_K_PLUS_V_MINUS_X_MINUS_Y:
            switch (mightAdd->type)
            {
            case TYPE_K_PLUS_V_MINUS_X:
                parentExists = matchesVExpression(current, mightAdd);
                break;
            default:
                parentExists = false;
                break;
            }
            break;
        case TYPE_K_MINUS_X_PLUS_W_MINUS_Y:
            switch (mightAdd->type)
            {
            case TYPE_K_MINUS_X:
                parentExists = true;
                break;
            default:
                parentExists = false;
                break;
            }
            break;
        default:
            break;
        }
        return parentExists;
    }

    return parentExists;
}


bool ExpressionAnalyser::matchesK(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return matchesKValue(values1, values2) || matchesKRealValue(values1, values2);
}

bool ExpressionAnalyser::matchesKValue(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return values1->k_value == values2->k_value;
}

bool ExpressionAnalyser::matchesKRealValue(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return ((util_isNaN(values1->k_real_value) && util_isNaN(values2->k_real_value)) ||
        util_isEqual(values1->k_real_value, values2->k_real_value));
}

bool ExpressionAnalyser::matchesXValue(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return values1->x_value == values2->x_value;
}

bool ExpressionAnalyser::matchesYValue(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return values1->y_value == values2->y_value;
}

bool ExpressionAnalyser::matchesVExpression(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return (values1->v_expression != NULL && values2->v_expression != NULL &&
        values1->v_expression->exactlyEqual(*(values2->v_expression)) == true);
}

bool ExpressionAnalyser::matchesWExpression(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return (values1->w_expression != NULL && values2->w_expression != NULL &&
        values1->w_expression->exactlyEqual(*(values2->w_expression)) == true);
}

bool ExpressionAnalyser::matchesDxdtExpression(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return (values1->dxdt_expression != NULL && values2->dxdt_expression != NULL &&
        values1->dxdt_expression->exactlyEqual(*(values2->dxdt_expression)) == true);
}

bool ExpressionAnalyser::matchesDydtExpression(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return (values1->dydt_expression != NULL && values2->dydt_expression != NULL &&
        values1->dydt_expression->exactlyEqual(*(values2->dydt_expression)) == true);
}

bool ExpressionAnalyser::matchesType(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return values1->type == values2->type;
}


List* ExpressionAnalyser::getHiddenSpecies()
{
    return mHiddenSpecies;
}

unsigned int ExpressionAnalyser::getNumHiddenSpecies()
{
    if (mHiddenSpecies == NULL)
    {
        return 0;
    }
    return mHiddenSpecies->getSize();
}

SubstitutionValues_t* 
ExpressionAnalyser::createBlankSubstitutionValues()
{
    SubstitutionValues_t* values = new SubstitutionValues_t;
    values->type = TYPE_UNKNOWN;
    values->k_real_value = util_NaN();
    values->dxdt_expression = NULL;
    values->dydt_expression = NULL;
    values->v_expression = NULL;
    values->w_expression = NULL;
    values->z_expression = NULL;
    values->k_value = "";
    values->x_value = "";
    values->y_value = "";
    values->z_value = "";
    values->odeIndex = 0;
    values->current = NULL;
    values->levelInExpression = 0;
    return values;
}

unsigned int
ExpressionAnalyser::getNumExpressions()
{
    return mExpressions.size();
}

SubstitutionValues_t* ExpressionAnalyser::getExpression(unsigned int index)
{
    if (index < mExpressions.size())
    {
        return mExpressions.at(index);
    }
    return nullptr;
}

/*
* Check whether the expression has a parent expression which may already have been analysed 
* in which case we do not need to re analyse the child expression
* e.g. if we have k-x-y do not need to analyse k-x
*/
bool
ExpressionAnalyser::shouldAddExpression(SubstitutionValues_t* value, ASTNodePair currentNode)
{
  bool found = false;
  bool foundParent = false;
  size_t size = mExpressions.size();

  while (size > 0 && !found)
  {
      found = expressionExists(mExpressions.at(size - 1), value);
      size--;
  }
  if (!found)
  {
      size = mExpressions.size();
      while (size > 0 && !foundParent)
      {
          foundParent = parentExpressionExists(mExpressions.at(size - 1), value);
          size--;
      }

  }
  return !found && !foundParent;
}


bool
ExpressionAnalyser::analyseNode(ASTNode* node, SubstitutionValues_t *value)
{
//    cout << "current node: " << SBML_formulaToL3String(node) << endl;
    ASTNodeType_t type = node->getType();

    // type must be plus or minus
    if (type != AST_PLUS && type != AST_MINUS)
    {
        return false;
    }
    unsigned int numChildren = node->getNumChildren();

    // we must have two children
    if (numChildren != 2)
    {
        return false;
    }


    ASTNode* rightChild = node->getRightChild();
    ASTNode* leftChild = node->getLeftChild();

    // determine which type of expression we have
    // individual functions will populate the value of the SubstitutionValues_t
    if (isTypeKminusX(numChildren, rightChild, leftChild, type, value) ||
        isTypeKminusXminusY(numChildren, rightChild, leftChild, type, value) ||
        isTypeKplusVminusX(numChildren, rightChild, leftChild, type, value) ||
        isTypeKplusVminusXminusY(numChildren, rightChild, leftChild, type, value) ||
        isTypeKminusXplusWminusY(numChildren, rightChild, leftChild, type, value))
    {
        value->current = node;
        return true;
    }
    return false;
}


/*
* Return the ODE for the given variable 
* or an ASTNode representing zero if there is no time derivative
*/
ASTNode*
ExpressionAnalyser::getODEFor(std::string name)
{
  for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  {
    std::pair<std::string, ASTNode*> ode = mODEs.at(odeIndex);
    if (name == ode.first)
    {
      return ode.second;
    }
  }
  ASTNode* zero = new ASTNode(AST_REAL);
  zero->setValue(0.0);
  return zero->deepCopy();
}

ASTNode* 
ExpressionAnalyser::getODE(unsigned int odeIndex)
{
    if (odeIndex < mODEs.size())
    {
        return mODEs.at(odeIndex).second;
    }
    return nullptr;
}

void
ExpressionAnalyser::analyse()
{
  for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  {
    std::pair<std::string, ASTNode*> ode = mODEs.at(odeIndex);
    ASTNode* odeRHS = ode.second;
    odeRHS->refactor();
    ASTNodeLevels operators = odeRHS->getListOfNodesWithLevel(true);
    ASTNodeLevelsIterator it = operators.begin();

    while (it != operators.end())
    {
        ASTNodePair currentNode = (ASTNodePair)*it;
      SubstitutionValues_t* value = createBlankSubstitutionValues();

      if (analyseNode(currentNode.second, value))
      {
        value->odeIndex = odeIndex;
        value->levelInExpression = currentNode.first;
        if (shouldAddExpression(value, currentNode))
        {
            mExpressions.push_back(value);
        }
      }
      it++;
    }
  }
}

void ExpressionAnalyser::orderExpressions()
{
    std::sort(mExpressions.begin(), mExpressions.end(), compareExpressions());
}

void
ExpressionAnalyser::detectHiddenSpecies(bool testing)
{
  mExpressions.clear();
  
  // find cases of k-x/k-x-y/k+v-x/k+v-x-y/k-x+w-y
  analyse();
  //for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  //{
  //    cout << mODEs[odeIndex].first << ": " << SBML_formulaToL3String(mODEs[odeIndex].second) << endl;
  //}
  orderExpressions();
  identifyHiddenSpeciesWithinExpressions();
  if (!testing)
  {
      substituteNodes();
  }
}

/*
* Replace a child node within a node with the given replacement mode
*
* param node ASTNode * parent node containing node to be replaced
* param replaced ASTNode * node to be replaced if found in parent node
* param replacement
*/
bool
ExpressionAnalyser::replaceExpressionInNodeWithNode(ASTNode* node, ASTNode* replaced, ASTNode* replacement)
{
    bool replacementMade = false;
    if (node == NULL || replaced == NULL || replacement == NULL)
    {
        return replacementMade;
    }

    /*cout << "node " << SBML_formulaToL3String(node) << endl;
    cout << "replaced " << SBML_formulaToL3String(replaced) << endl;
    cout << "replacement " << SBML_formulaToL3String(replacement) << endl;*/
    // we might be replcing the whole node
    if (node->exactlyEqual(*replaced))
    {
        // delete the node and replace it with the replacement
        // this is a bit of a hack but it works
        // we need to make sure that we are not deleting the replacement as it is now owned by the parent node
        // so we need to deep copy it first
        *node = *replacement;
        replacementMade = true;
    }
    else
    {
        std::pair<ASTNode*, int>currentParentAndIndex = make_pair((ASTNode*)NULL, (int)(NAN));
        ASTNode* currentParent;
        int index;
        do
        {
            currentParentAndIndex = getParentNode(replaced, node);
            currentParent = currentParentAndIndex.first;
            index = currentParentAndIndex.second;
            if (currentParent != NULL)
            {
                currentParent->replaceChild(index, replacement->deepCopy(), false);
                replacementMade = true;
            // intentionally, don't delete replacement as it's now owned by currentParent!
            }
        } while (currentParent != NULL);
    }
    return replacementMade;
}

std::string
ExpressionAnalyser::getUniqueNewParameterName()
{ 
  return mNewVarName + std::to_string(mNewVarCount);
}

void ExpressionAnalyser::substituteNodes()
{
  for (unsigned int i = 0; i < mExpressions.size(); i++)
  {
    SubstitutionValues_t* exp = mExpressions.at(i);
    replaceExpressionInNodeWithNode(getODE(exp->odeIndex), exp->current, exp->z_expression); //getODEFor(exp->current);
    addParametersAndRateRules(exp);
  }
}


void
ExpressionAnalyser::addParametersAndRateRules(SubstitutionValues_t* exp)
{
    if (exp->z_value.empty()) return;
    if (mHiddenSpecies == NULL) mHiddenSpecies = new List();
  //for (unsigned int i = 0; i < mExpressions.size(); i++)
  //{
  //  SubstitutionValues_t *exp = mExpressions.at(i);
    if (mModel->getParameter(exp->z_value) == NULL)
    {
      // create expression for z
        ASTNode* kx = new ASTNode(AST_MINUS);
        ASTNode* x = new ASTNode(AST_NAME);
        ASTNode* k = NULL;

        x->setName(exp->x_value.c_str());

        if (exp->k_value == "number")
        {
            k = new ASTNode(AST_REAL);
            k->setValue(exp->k_real_value);
        }
        else
        {
            k = new ASTNode(AST_NAME);
            k->setName(exp->k_value.c_str());
        }
      
      kx->addChild(k);
      kx->addChild(x);

      ASTNode* zNode = new ASTNode(AST_MINUS);


      // add raterule defining dz/dt
      ASTNode* dxdt = exp->dxdt_expression->deepCopy();
      RateRule* raterule = mModel->createRateRule();
      raterule->setVariable(exp->z_value);
      ASTNode* math = new ASTNode(AST_TIMES);
      ASTNode* minus1 = new ASTNode(AST_REAL);
      minus1->setValue(-1.0);

      ASTNode* dydt = NULL;
      ASTNode* plus = NULL;
      ASTNode* y = NULL;
      switch (exp->type)
      {
      case TYPE_K_MINUS_X:
      case TYPE_K_PLUS_V_MINUS_X:
        // dz/dt = -dx/dt
        math->addChild(minus1);
        math->addChild(dxdt);

        // z = k - x
        (*zNode) = *kx;

        break;
      case TYPE_K_MINUS_X_MINUS_Y:
      case TYPE_K_PLUS_V_MINUS_X_MINUS_Y:
      case TYPE_K_MINUS_X_PLUS_W_MINUS_Y:
        // dz/dt = - (dx/dt + dy/dt)
        dydt = exp->dydt_expression->deepCopy();
        plus = new ASTNode(AST_PLUS);
        plus->addChild(dxdt);
        plus->addChild(dydt);
        math->addChild(minus1);
        math->addChild(plus);

        // z = k-x-y
        y = new ASTNode(AST_NAME);
        y->setName(exp->y_value.c_str());
        zNode->addChild(kx);
        zNode->addChild(y);

        break;
      default:
        break;
      }
      raterule->setMath(math);

      // introduce z
      Parameter* zParam = mModel->createParameter();
      zParam->setId(exp->z_value);
      zParam->setConstant(false);
      zParam->setValue(SBMLTransforms::evaluateASTNode(zNode, mModel));
      mHiddenSpecies->add(zParam);

      delete zNode;
      delete math; //its children dxdt and minus1 deleted as part of this.
    //}
  }
}



unsigned int ExpressionAnalyser::getNumHiddenNodes()
{   
    if (mHiddenNodes == NULL)
        return 0;
    else
        return mHiddenNodes->getSize();
}

Parameter* ExpressionAnalyser::getHiddenSpecies(unsigned int index)
{  
    if (mHiddenSpecies != NULL && index < mHiddenSpecies->getSize())
    {
        return static_cast<Parameter*>(mHiddenSpecies->get(index));
    }
    return NULL;
}


bool ExpressionAnalyser::isTypeKminusXminusY(unsigned int numChildren, ASTNode* rightChild, 
    ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value)
{
    // for this type of expression, k-x-y, the left child is the expression k-x, the right child is the y variable
    // the type is MINUS and the number of children is 2
    if (numChildren != 2 || type != AST_MINUS)
    {
        return false;
    }


    if (isTypeKminusX(leftChild->getNumChildren(), leftChild->getRightChild(), 
        leftChild->getLeftChild(), leftChild->getType(), value) && 
        isVariableSpeciesOrParameter(rightChild))
    {
        value->type = TYPE_K_MINUS_X_MINUS_Y;
        value->y_value = rightChild->getName();
        value->dydt_expression = (getODEFor(rightChild->getName()))->deepCopy();
        return true;
    }
    return false;
}

bool ExpressionAnalyser::isTypeKminusX(unsigned int numChildren, ASTNode* rightChild, 
    ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value)
{   
    // for this type of expression, k-x, the left child is the parameter k, the right child is the x variable
    // the type is MINUS and the number of children is 2
    bool isNumber = false;
    if (numChildren != 2 || type != AST_MINUS)
    {
        return false;
    }


    if (isNumericalConstantOrConstantParameter(leftChild, isNumber)
      && isVariableSpeciesOrParameter(rightChild))
    {   
      if (isNumber)
      {
          value->k_value = "number";
          value->k_real_value = leftChild->getValue();
      }
      else
      {
          value->k_value = leftChild->getName();
      }
      value->x_value = rightChild->getName();
      value->dxdt_expression = (getODEFor(rightChild->getName()))->deepCopy();
      value->type = TYPE_K_MINUS_X;
      return true;
    }
    return false;
}

bool ExpressionAnalyser::isTypeKplusVminusX(unsigned int numChildren, ASTNode* rightChild, 
    ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value)
{
    // for this type of expression, k+v-x, the left child is the expression k+v, the right child is the x variable
    // the type is MINUS and the number of children is 2
    if (numChildren != 2 || type != AST_MINUS)
    {
        return false;
    }


    if (isTypeKplusV(leftChild->getNumChildren(), leftChild->getRightChild(),
        leftChild->getLeftChild(), leftChild->getType(), value) &&
        isVariableSpeciesOrParameter(rightChild))
    {
        value->type = TYPE_K_PLUS_V_MINUS_X;
        value->x_value = rightChild->getName();
        value->dxdt_expression = (getODEFor(rightChild->getName()))->deepCopy();
        return true;
    }
    return false;
}

bool ExpressionAnalyser::isTypeKplusV(unsigned int numChildren, ASTNode* rightChild, 
    ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value)
{
    // for this type of expression, k+v, the left child is the constant k, the right child is the v variable
    // the type is PLUS and the number of children is 2
    if (numChildren != 2 || type != AST_PLUS)
    {
        return false;
    }

    bool isNumber = false;
    if (isNumericalConstantOrConstantParameter(leftChild, isNumber))
    {   
      if (isNumber)
      {
          value->k_value = "number";
          value->k_real_value = leftChild->getValue();
      }
      else
      {
          value->k_value = leftChild->getName();
      }
      value->v_expression = rightChild->deepCopy();
      return true;
    }
    return false;
}

bool ExpressionAnalyser::isTypeKplusVminusXminusY(unsigned int numChildren, ASTNode* rightChild, ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value)
{
    // for this type of expression, k+v-x-y, the left child is the expression k+v-x, the right child is the y variable
    // the type is MINUS and the number of children is 2
    if (numChildren != 2 || type != AST_MINUS)
    {
        return false;
    }


    if (isTypeKplusVminusX(leftChild->getNumChildren(), leftChild->getRightChild(),
        leftChild->getLeftChild(), leftChild->getType(), value) &&
        isVariableSpeciesOrParameter(rightChild))
    {
        value->type = TYPE_K_PLUS_V_MINUS_X_MINUS_Y;
        value->y_value = rightChild->getName();
        value->dydt_expression = (getODEFor(rightChild->getName()))->deepCopy();
        return true;
    }
    return false;
}

bool ExpressionAnalyser::isTypeKminusXplusWminusY(unsigned int numChildren, ASTNode* rightChild, 
    ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value)
{
    // for this type of expression, k-x+w-y, the left child is the expression w+k-x, the right child is the y variable
    // the type is MINUS and the number of children is 2
    if (numChildren != 2 || type != AST_MINUS)
    {
        return false;
    }


    if (isTypeWplusKminusX(leftChild->getNumChildren(), leftChild->getRightChild(),
        leftChild->getLeftChild(), leftChild->getType(), value) &&
        isVariableSpeciesOrParameter(rightChild))
    {
        value->y_value = rightChild->getName();
        value->dydt_expression = (getODEFor(rightChild->getName()))->deepCopy();
        return true;
    }
    return false;
}

bool ExpressionAnalyser::isTypeWplusKminusX(unsigned int numChildren, ASTNode* rightChild, 
    ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value)
{
    // for this type of expression, w+k-x, the left child is the expression w, the right child is the k-x expression
    // the type is PLUS and the number of children is 2
    if (numChildren != 2 || type != AST_PLUS)
    {
        return false;
    }


    if (isTypeKminusX(rightChild->getNumChildren(), rightChild->getRightChild(),
        rightChild->getLeftChild(), rightChild->getType(), value))
    {
        value->type = TYPE_K_MINUS_X_PLUS_W_MINUS_Y;
        value->w_expression = leftChild->deepCopy();
        return true;
    }
    return false;
}


/*
 * Check whether for node is a name node representing species or a non constant parameter
*/
bool ExpressionAnalyser::isVariableSpeciesOrParameter(ASTNode* node)
{
    if (!node->isName()) // some nodes, like * operators, don't seem to have a name in the first place
        return false;
    Species* species = mModel->getSpecies(node->getName());
    Parameter* parameter = mModel->getParameter(node->getName()); // some species in rate rules may be defined as variable parameters
    bool isVariableSpeciesOrParameter = (species != NULL && !species->getConstant());
    bool isVariableParameter = (parameter!=NULL && !parameter->getConstant());
    return isVariableSpeciesOrParameter || isVariableParameter;
}

/*
* Check whether for node is a name node representing a constant parameter or a numerical node
*/
bool ExpressionAnalyser::isNumericalConstantOrConstantParameter(ASTNode* node, bool& isNumber)
{
    bool isConstantParameter = false;
    isNumber = false;

    if (node->isName()) // some nodes, like * operators, don't seem to have a name in the first place
    {
        Parameter* parameter = mModel->getParameter(node->getName());
        isConstantParameter = (parameter != NULL) && (parameter->getConstant());
    }
    bool isNumericalConstant = node->isNumber() || node->isConstant();

    if (isConstantParameter)
        return true;
    else if (isNumericalConstant)
    {
        isNumber = true;
        return true;
    }
    else
        return false;
}


std::pair<ASTNode*, int> ExpressionAnalyser::getParentNode(const ASTNode* child, const ASTNode* root)
{
  //cout << "root " << SBML_formulaToL3String(root) << endl;
  //cout << "child " << SBML_formulaToL3String(child) << endl;

  for (unsigned int i = 0; i < root->getNumChildren(); i++)
    {
        if (root->getChild(i)->exactlyEqual(*(child)))
        {
            return std::pair<ASTNode*, int>(const_cast<ASTNode*>(root), i);
        }
    }
    for (unsigned int i = 0; i < root->getNumChildren(); i++)
    {
      std::pair<ASTNode*, int> parent = getParentNode(child, root->getChild(i));
      if (parent.first != NULL)
      {
        return parent;
      }
    }
    return std::pair<ASTNode*, int>(NULL, (int)(NAN));
}

/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

