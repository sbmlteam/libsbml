
/**
 * @file    ExpressionAnalyser.cpp
 * @brief   Implementation of ExpressionAnalyser
 * @author  Sarah Keating
 * @author  Alessandro Felder
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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

#ifdef __cplusplus

#include <algorithm>
#include <string>
#include <vector>
#include <map>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN




ExpressionAnalyser::ExpressionAnalyser() :
   mODEs (NULL)
  ,mModel (NULL)
{
}

ExpressionAnalyser::ExpressionAnalyser(const ExpressionAnalyser& orig) :
   mODEs(NULL)
  , mModel(NULL)
{
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
  }

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
  for (std::vector<std::pair<std::string, ASTNode*> >::iterator it = mODEs.begin(); it != mODEs.end(); ++it)
  {
    if (it->second != NULL)
    {
      delete it->second;
      it->second = NULL;
    }
  }
  mODEs.clear();
  SBMLTransforms::clearComponentValues();
}

/*
* Set ode pairs
*/
int
ExpressionAnalyser::setODEPairs(std::vector< std::pair< std::string, ASTNode*> > odes)
{
  mODEs = odes;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Set ode model
*/
int
ExpressionAnalyser::setModel(Model* model)
{
  mModel = model;
  SBMLTransforms::mapComponentValues(model);
  return LIBSBML_OPERATION_SUCCESS;
}

/*
* Check whether the expression has a parent expression which may already have been analysed 
* in which case we do not need to re analyse the child expression
* e.g. if we have k-x-y do not need to analyse k-x
*/
bool
ExpressionAnalyser::hasParentExpressionAlready(SubstitutionValues_t* value)
{
  bool match = false;
  for (unsigned int i = mExpressions.size(); i > 0; i--)
  {
    SubstitutionValues_t* exp = mExpressions.at(i - 1);
    if (value->type == TYPE_K_MINUS_X || value->type == TYPE_K_MINUS_X_MINUS_Y)
    {
      if (value->type == exp->type && value->dxdt_expression == exp->dxdt_expression  &&
        value->k_value == exp->k_value && value->x_value == exp->x_value)
      {
        match = true;
      }
    }
  }
  return match;
}


bool
ExpressionAnalyser::analyseNode(ASTNode* node, SubstitutionValues_t *value)
{
  unsigned int numChildren = node->getNumChildren();
  ASTNodeType_t type = node->getType();
  ASTNode* rightChild = node->getRightChild();
  ASTNode* leftChild = node->getLeftChild();

  //  k-x or k-x-y node binary; minus; right child (x,y) is variable
  if (numChildren !=2 || type != AST_MINUS || !isVariableSpeciesOrParameter(rightChild))
    return false;

  // 

  // if left child is  numerical constant or a parameter, it IS k-x
  if (isNumericalConstantOrConstantParameter(leftChild))
  {
    value->k_value = leftChild->getName();
    value->x_value = rightChild->getName();
    value->dxdt_expression = getODEFor(rightChild->getName());
    value->type = TYPE_K_MINUS_X;
    value->current = node;
    return true;
  }
  else
  {
    // if left child is k-x then we have k-x-y
    if (analyseNode(leftChild, value))
    {
      value->y_value = rightChild->getName();
      value->dydt_expression = getODEFor(rightChild->getName());
      value->type = TYPE_K_MINUS_X_MINUS_Y;
      value->current = node;
      return true;

    }
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
void
ExpressionAnalyser::analyse()
{
  for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  {
    std::pair<std::string, ASTNode*> ode = mODEs.at(odeIndex);
    ASTNode* odeRHS = ode.second;
    odeRHS->reduceToBinary();
    List* operators = odeRHS->getListOfNodes((ASTNodePredicate)ASTNode_isOperator);
    ListIterator it = operators->begin();

    while (it != operators->end())
    {
      ASTNode* currentNode = (ASTNode*)*it;
      SubstitutionValues_t* value = new SubstitutionValues_t;
      value->type = TYPE_UNKNOWN;
      if (analyseNode(currentNode, value))
      {
        value->odeIndex = odeIndex;
        if (!hasParentExpressionAlready(value))
        {
          mExpressions.push_back(value);
        }
      }
      it++;
    }
  }
}

void
ExpressionAnalyser::detectHiddenSpecies(List * hiddenSpecies)
{
 analyse();
  //for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  //{
  //  std::pair<std::string, ASTNode*> ode = mODEs.at(odeIndex);
  //  ASTNode* odeRHS = ode.second;
  //  odeRHS->reduceToBinary();
  //  //      Step 1: iterative, in-place replacement of any -x+y terms with y-x terms
  //  reorderMinusXPlusYIteratively(odeRHS);

  //  // Step 2 TODO
  //  List* operators = odeRHS->getListOfNodes((ASTNodePredicate)ASTNode_isOperator);
  //  ListIterator it = operators->begin();
  //  while (it != operators->end())
  //  {
  //    ASTNode* currentNode = (ASTNode*)*it;
  //    if (isKMinusXMinusY(currentNode))
  //    {
  //      currentNode->printMath();
  //      // (a) introduce z=k-x-y with dz/dt = -dx/dt-dy/dt (add to list of additional ODEs to add at the end)
  //      // TODO
  //      // (b) replace in ALL ODEs (not just current) k-x-y with z (interior loop over mODEs again?)
  //      // (c) replace in ALL ODEs (not just current) k+v-x-y with v+z
  //      // (d) replace in ALL ODEs (not just current) k-x+w-y with w+z
  //    }
  //    it++;
  //  }
    addHiddenVariablesForKMinusX(hiddenSpecies);
  //  // Step 3
  //  //it = operators->begin();
  //  //while (it != operators->end())
  //  //{
  //  //    ASTNode* currentNode = (ASTNode*)*it;
  //  //    if (isKMinusX(currentNode, mModel))
  //  //    {
  //  //        // remember x name for later, before we replace the current node
  //  //        std::string xName = std::string(currentNode->getRightChild()->getName());

  //  //        // (a)
  //  //        // introduce z=k-x
  //  //        Parameter* zParam = mModel->createParameter();
  //  //        const std::string zName = "z" + std::to_string(mModel->getNumParameters());
  //  //        zParam->setId(zName);
  //  //        zParam->setConstant(false);
  //  //        hiddenSpecies.add(zParam);

  //  //        // replace k - x with z in current ODE
  //  //        ASTNode* z = new ASTNode(ASTNodeType_t::AST_NAME);
  //  //        z->setName(zName.c_str());
  //  //        std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
  //  //        ASTNode* currentParent = currentParentAndIndex.first;
  //  //        int index = currentParentAndIndex.second;
  //  //        if (currentParent != NULL)
  //  //        {
  //  //          currentParent->replaceChild(index, z, true);
  //  //          // intentionally, don't delete z as it's now owned by currentParent!
  //  //        }

  //  //        // add raterule defining dz/dt = -dxdt
  //  //        ASTNode* dxdt = odeRHS->deepCopy();
  //  //        RateRule* raterule = mModel->createRateRule();
  //  //        raterule->setVariable(zName);
  //  //        ASTNode* math = new ASTNode(ASTNodeType_t::AST_TIMES);
  //  //        ASTNode* minus1 = new ASTNode(ASTNodeType_t::AST_REAL);
  //  //        minus1->setValue(-1.0);
  //  //        math->addChild(minus1);
  //  //        math->addChild(dxdt);
  //  //        raterule->setMath(math);
  //  //        delete math; //its children dxdt and minus1 deleted as part of this.

  //  //        // TODO
  //  //        // (b) replace in ALL ODEs (not just current) k-x with z
  //  //        // (c) replace in ALL ODEs (not just current) k+v-x with v+z
  //  //    }
  //  //    it++;
  //  //}

  //}
  ////return hiddenSpecies;
}

/*
* Replace a child node within a node with the given replacement mode
*
* param node ASTNode * parent node containing node to be replaced
* param replaced ASTNode * node to be replaced if found in parent node
* param replacement
*/
void
ExpressionAnalyser::replaceExpressionInNodeWithNode(ASTNode* node, ASTNode* replaced, ASTNode* replacement)
{
  // we might be replcing the whole node
  if (node == replaced)
  {
    replaced = node->deepCopy();
    (*node) = *replacement;
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
        // intentionally, don't delete replacement as it's now owned by currentParent!
      }
    } while (currentParent != NULL);
  }
}

void
ExpressionAnalyser::replaceExpressionInNodeWithVar(ASTNode* node, ASTNode* replaced, std::string var)
{
  ASTNode* z = new ASTNode(ASTNodeType_t::AST_NAME);
  z->setName(var.c_str());
  replaceExpressionInNodeWithNode(node, replaced, z);
}

std::string
ExpressionAnalyser::getUniqueNewParameterName()
{
  return "z" + std::to_string(mModel->getNumParameters());
}


void
ExpressionAnalyser::addParameterAndRateRule(List* hiddenSpecies, SubstitutionValues_t *exp)
{
  // introduce z
  Parameter* zParam = mModel->createParameter();
  zParam->setId(exp->z_value);
  zParam->setConstant(false);
  zParam->setValue(SBMLTransforms::evaluateASTNode(exp->current, mModel));
  hiddenSpecies->add(zParam);

  if (exp->type == TYPE_K_MINUS_X)
  {
    replaceExpressionInNodeWithVar(exp->dxdt_expression, exp->current, exp->z_value);
  }
  else if (exp->type == TYPE_K_MINUS_X_MINUS_Y)
  {
    replaceExpressionInNodeWithVar(exp->dxdt_expression, exp->current, exp->z_value);
    replaceExpressionInNodeWithVar(exp->dydt_expression, exp->current, exp->z_value);

  }

  // add raterule defining dz/dt
  ASTNode* dxdt = exp->dxdt_expression->deepCopy(); 
  RateRule* raterule = mModel->createRateRule();
  raterule->setVariable(exp->z_value);
  ASTNode* math = new ASTNode(ASTNodeType_t::AST_TIMES);
  ASTNode* minus1 = new ASTNode(ASTNodeType_t::AST_REAL);
  minus1->setValue(-1.0);
  switch (exp->type)
  {
  case TYPE_K_MINUS_X:
    // dz/dt = -dx/dt
    math->addChild(minus1);
    math->addChild(dxdt);
    break;
  case TYPE_K_MINUS_X_MINUS_Y:
    // dz/dt = - (dx/dt + dy/dt)
    ASTNode* dydt = exp->dydt_expression->deepCopy();
    ASTNode* plus = new ASTNode(AST_PLUS);
    plus->addChild(dxdt);
    plus->addChild(dydt);
    math->addChild(minus1);
    math->addChild(plus);

    break;
  }
  raterule->setMath(math);
  delete math; //its children dxdt and minus1 deleted as part of this.

}


bool
ExpressionAnalyser::addHiddenVariablesForKMinusX(List* hiddenSpecies)
{
  for (unsigned int i = 0; i < mExpressions.size(); i++)
  {
    SubstitutionValues_t *exp = mExpressions.at(i);
    ASTNode* currentNode = exp->current;
    for (unsigned int j = 0; j < mODEs.size(); j++)
    {
      std::pair<std::string, ASTNode*> ode = mODEs.at(j);
      ASTNode* odeRHS = ode.second;
      std::string zName = parameterAlreadyCreated(exp);
      if (!zName.empty())
      {
        replaceExpressionInNodeWithVar(odeRHS, currentNode, zName);
      }
      else
      {
        std::string zName = getUniqueNewParameterName();
        exp->z_value = zName;

        // replace exp with z in current ODE
        replaceExpressionInNodeWithVar(odeRHS, currentNode, zName);
        addParameterAndRateRule(hiddenSpecies, exp);

      }
    }
  } // end for
  return true;
}


std::string
ExpressionAnalyser::parameterAlreadyCreated(SubstitutionValues_t* exp)
{
  std::string renamed = "";
  bool match = false;
  unsigned int i = 0;
  while (!match && i < mExpressions.size())
  {
    if (mExpressions.at(i)->type == exp->type && mExpressions.at(i)->k_value == exp->k_value &&
      mExpressions.at(i)->x_value == exp->x_value && mExpressions.at(i)->z_value.empty() != true)
    {
      renamed = mExpressions.at(i)->z_value;
      match = true;
    }
    i++;
  }
  return renamed;
}

// helper function to determine whether a node is the "+" in a -x+y expression, where x,y are variable species.
bool ExpressionAnalyser::isMinusXPlusY(ASTNode* node)
{
    //if node is not binary, it's not -x+y
    if (node->getNumChildren() != 2)
        return false;

    // if node is not a plus, it's not -x+y
    if (node->getType() != ASTNodeType_t::AST_PLUS)
        return false;

    // if left child is not a minus, it's not -x+y
    ASTNode* leftChild = node->getLeftChild();
    if (leftChild->getType() != ASTNodeType_t::AST_MINUS)
        return false;

    // if right child is not a variable species (y), it's not -x+y
    ASTNode* rightChild = node->getRightChild();
    if (!isVariableSpeciesOrParameter(rightChild))
        return false;

    // if we get to this point, the only thing left to check is 
    // whether the ->left->right grandchild (the x in -x+y) is a variable species.
    ASTNode* grandChild = leftChild->getRightChild();
    return isVariableSpeciesOrParameter(grandChild);
}

// helper function to determine whether a node is the second "-" in a k-x-y expression, where x,y are variable species and k is a numerical constant or a parameter
bool ExpressionAnalyser::isKMinusXMinusY(ASTNode* node)
{
    // if node is not binary, it's not k-x-y
    if (node->getNumChildren() != 2)
        return false;

    // if node is not a minus, it's not the second "-" in k-x-y
    if (node->getType() != ASTNodeType_t::AST_MINUS)
        return false;

    // if right child is not a variable, it's not k-x-y
    ASTNode* rightChild = node->getRightChild();
    if(!isVariableSpeciesOrParameter(rightChild))
        return false;

    // if left child is not a minus, it's not k-x-y
    ASTNode* leftChild = node->getLeftChild();
    if (leftChild->getType() != ASTNodeType_t::AST_MINUS)
        return false;

    // if left child is not binary, it's not k-x-y
    if (leftChild->getNumChildren() != 2)
        return false;

    // if we've gotten this far, what's left to check are the children of the left child (k and x)
    ASTNode* k = leftChild->getLeftChild();
    ASTNode* x = leftChild->getRightChild();
    return isNumericalConstantOrConstantParameter(k) && isVariableSpeciesOrParameter(x);
}

// check whether a node is the minus sign in a k-x expression
pairString ExpressionAnalyser::isKMinusX(ASTNode* node)
{
    // if node is not binary, it's not k-x
    if (node->getNumChildren() != 2)
      return make_pair("", "");

    // if node is not a minus, it's not "-" in k-x
    if (node->getType() != ASTNodeType_t::AST_MINUS)
      return make_pair("", "");

    // if right child is not a variable species, it's not k-x
    ASTNode* rightChild = node->getRightChild();
    if (!isVariableSpeciesOrParameter(rightChild))
      return make_pair("", "");

    // if left child is not a numerical constant or a parameter, it's not k-x - else it is!
    ASTNode* leftChild = node->getLeftChild();
    if (isNumericalConstantOrConstantParameter(leftChild))
      return make_pair(leftChild->getName(), rightChild->getName());
    else
      return make_pair("", "");
}

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

bool ExpressionAnalyser::isNumericalConstantOrConstantParameter(ASTNode* node)
{
    if (!node->isName()) // some nodes, like * operators, don't seem to have a name in the first place
        return false;
    Parameter* parameter = mModel->getParameter(node->getName());
    bool isConstantParameter = (parameter != NULL) && (parameter->getConstant());
    bool isNumericalConstant = node->isNumber() && node->isConstant();
    return isNumericalConstant || isConstantParameter;
}

void ExpressionAnalyser::reorderMinusXPlusYIteratively(ASTNode* odeRHS)
{
    int numMinusXPlusY = INT16_MAX;
    while (numMinusXPlusY != 0)
    {
        // since check is centred on a node that is + or -, we can iterate over operator nodes in tree
        numMinusXPlusY = 0;
        List* operators = odeRHS->getListOfNodes((ASTNodePredicate)ASTNode_isOperator);
        ListIterator it = operators->begin();
        while (it != operators->end())
        {
            ASTNode* currentNode = (ASTNode*)*it;
            if (isMinusXPlusY(currentNode))
            {
              // we have a binary node that is (k-x) + y
                //Swap -x+y node for y-x node
              ASTNode* yMinusX = new ASTNode(ASTNodeType_t::AST_MINUS);
              std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
              ASTNode* currentParent = currentParentAndIndex.first;
              if (currentParent == NULL)
              {
                // ode is whole expression
                ASTNode* y = currentNode->getRightChild();
                ASTNode* x = currentNode->getLeftChild()->getRightChild();
                ASTNode* k = currentNode->getLeftChild()->getLeftChild();
                
              }
              else
              {
                int index = currentParentAndIndex.second;
                currentParent->replaceChild(index, yMinusX, true);
                numMinusXPlusY++;
              }
            }
            it++;
        }
        delete operators;
    }
}

std::pair<ASTNode*, int> ExpressionAnalyser::getParentNode(const ASTNode* child, const ASTNode* root)
{
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


