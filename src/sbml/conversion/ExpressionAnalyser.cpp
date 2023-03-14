
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


#include <algorithm>
#include <string>
#include <vector>
#include <map>
#include <sbml/SBMLTypes.h>
#include <sbml/math/ASTNodeType.h>
#include <sbml/conversion/SBMLRateRuleConverter.h>

#ifdef __cplusplus

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN


ExpressionAnalyser::ExpressionAnalyser()
{
}


ExpressionAnalyser::ExpressionAnalyser(Model * m, pairODEs odes)
{
  mModel = m;
  mODEs = odes;
  SBMLTransforms::mapComponentValues(mModel);
  mModel->populateAllElementIdList();
  mNewVarName = "newVar";
  mNewVarCount = 1;
}

ExpressionAnalyser::ExpressionAnalyser(const ExpressionAnalyser& orig) :
  mModel( orig.mModel)
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
  SBMLTransforms::clearComponentValues(mModel);
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
  SBMLTransforms::clearComponentValues(mModel);
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
ExpressionAnalyser::hasExpressionAlreadyRecorded(SubstitutionValues_t* value)
{
  for (unsigned int i = mExpressions.size(); i > 0; i--)
  {
    SubstitutionValues_t* exp = mExpressions.at(i - 1);

    // if we already have parent eg K+v-x-y dont record children eg K+v-x
    std::pair<ASTNode*, int> parent = getParentNode(value->current, exp->current);
    if (parent.first != NULL)
    {
      return true;
    }
    switch (value->type)
    {
    case TYPE_K_MINUS_X_MINUS_Y:
      if (value->k_value == exp->k_value  &&
        value->x_value == exp->x_value &&
        value->y_value == exp->y_value &&
        value->dxdt_expression == exp->dxdt_expression &&
        value->dydt_expression == exp->dydt_expression &&
        value->type == exp->type)
      {
        return true;
      }
      break;
    case TYPE_K_PLUS_V_MINUS_X_MINUS_Y:
      if (value->k_value == exp->k_value  &&
        value->x_value == exp->x_value &&
        value->y_value == exp->y_value &&
        value->dxdt_expression == exp->dxdt_expression &&
        value->dydt_expression == exp->dydt_expression &&
        value->v_expression == exp->v_expression &&
        value->type == exp->type)
      {
        return true;
      }
      break;
    case TYPE_K_MINUS_X_PLUS_W_MINUS_Y:
      if (value->k_value == exp->k_value  &&
        value->x_value == exp->x_value &&
        value->y_value == exp->y_value &&
        value->dxdt_expression == exp->dxdt_expression &&
        value->dydt_expression == exp->dydt_expression &&
        value->w_expression == exp->w_expression &&
        value->type == exp->type)
      {
        return true;
      }
      break;
    case TYPE_K_MINUS_X:
      if (value->k_value == exp->k_value  &&
        value->x_value == exp->x_value &&
        value->dxdt_expression == exp->dxdt_expression &&
        value->type == exp->type)
      {
        return true;
      }
      break;
    case TYPE_K_PLUS_V_MINUS_X:
      if (value->k_value == exp->k_value  &&
        value->x_value == exp->x_value &&
        value->dxdt_expression == exp->dxdt_expression &&
        value->v_expression == exp->v_expression &&
        value->type == exp->type)
      {
        return true;
      }
      break;
    case TYPE_MINUS_X_PLUS_Y:
      if (value->x_value == exp->x_value &&
        value->y_value == exp->y_value &&
        value->dxdt_expression == exp->dxdt_expression &&
        value->dydt_expression == exp->dydt_expression &&
        value->type == exp->type)
      {
        return true;
      }
      break;
    default:
      break;
    }
  }
  return false;
}


bool
ExpressionAnalyser::analyseNode(ASTNode* node, SubstitutionValues_t *value)
{
  unsigned int numChildren = node->getNumChildren();
  ASTNodeType_t type = node->getType();
  ASTNode* rightChild = node->getRightChild();
  ASTNode* leftChild = node->getLeftChild();
  //node->printMath();
  switch (type)
  {
  case AST_PLUS:
    //  -x+y node binary; plus; left child type minus; rightchild var/const
    //           +
    //        -     y
    //        x
    if (numChildren != 2 || rightChild->getType() != AST_NAME
      || leftChild->getType() != AST_MINUS
      || leftChild->getNumChildren() != 1)
      return false;

    // if we get to this point, the only thing left to check is 
    // whether the ->left->right grandchild (the x in -x+y) is a variable species.
    if (isVariableSpeciesOrParameter(leftChild->getChild(0)))
    {
      value->x_value = leftChild->getChild(0)->getName();
      value->y_value = rightChild->getName();
      value->dydt_expression = getODEFor(rightChild->getName());
      value->dxdt_expression = getODEFor(leftChild->getChild(0)->getName());
      value->type = TYPE_MINUS_X_PLUS_Y;
      value->current = node;
      return true;
    }
    break;

  case AST_MINUS:
    //          -                -               
    //        k   x           -     y    
    //                      k   x      
    //  k-x or k-x-y node binary; right child (x,y) is variable
    //          -                    -                          -
    //      +      x            -         y              +            y
    //   k     v            +      x                 -       w
    //                  k      v                 k       x
    //  k+v-x; right child x var; left child plus node with left child k constant
    //  k+v-x-y; rightchild var y; left child minus == k+v-x
    //  k-x+w-y; rightchild var y; left child plus with left child == k-x 
    if (numChildren != 2 || !isVariableSpeciesOrParameter(rightChild))
      return false;
    // if left child is  numerical constant or a parameter and right child variable, it IS k-x
    if (isNumericalConstantOrConstantParameter(leftChild) 
      && isVariableSpeciesOrParameter(rightChild))
    {
      value->k_value = leftChild->getName();
      value->x_value = rightChild->getName();
      value->dxdt_expression = getODEFor(rightChild->getName());
      value->type = TYPE_K_MINUS_X;
      value->current = node;
      return true;
    }
    // left child + with it's left child const we have k+v-x
    // left child + with it's left child k-x we have k-x+w-y
    else if (leftChild->getType() == AST_PLUS)
    {
      if (isNumericalConstantOrConstantParameter(leftChild->getChild(0)))
      {
        value->k_value = leftChild->getChild(0)->getName();
        value->x_value = rightChild->getName();
        value->dxdt_expression = getODEFor(rightChild->getName());
        value->v_expression = leftChild->getChild(1);
        value->type = TYPE_K_PLUS_V_MINUS_X;
        value->current = node;
        return true;
      }
      else if (analyseNode(leftChild->getChild(0), value) && value->type == TYPE_K_MINUS_X)
      {
        value->y_value = rightChild->getName();
        value->dydt_expression = getODEFor(rightChild->getName());
        value->w_expression = leftChild->getChild(1);
        value->type = TYPE_K_MINUS_X_PLUS_W_MINUS_Y;
        value->current = node;
        return true;
      }
    }
    else if (leftChild->getType() == AST_MINUS
      && isVariableSpeciesOrParameter(leftChild->getRightChild()))
    {
      // if left child is k+v-x we have k+v-x-y
      // or if left child is k-x we have k-x-y
      if (analyseNode(leftChild, value))
      {
        if (value->type == TYPE_K_PLUS_V_MINUS_X)
        {
          value->type = TYPE_K_PLUS_V_MINUS_X_MINUS_Y;
          value->y_value = rightChild->getName();
          value->dydt_expression = getODEFor(value->y_value);
          value->current = node;
          return true;
        }
        else if (value->type == TYPE_K_MINUS_X)
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
    break;
  default:
    return false;
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
ExpressionAnalyser::analyse(bool minusXPlusYOnly)
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
      if (minusXPlusYOnly && currentNode->getType() != AST_PLUS)
      {
        it++;
        continue;
      }
      SubstitutionValues_t* value = new SubstitutionValues_t;
      value->type = TYPE_UNKNOWN;
      value->dxdt_expression = NULL;
      value->dydt_expression = NULL;
      value->v_expression = NULL;
      value->w_expression = NULL;
      if (analyseNode(currentNode, value))
      {
        value->odeIndex = odeIndex;
        if (!hasExpressionAlreadyRecorded(value))
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
  // find -x+y and replace with y-x 
  analyse(true);
  reorderMinusXPlusYIteratively();
  mExpressions.clear();
  
  // find cases of k-x/k-x-y/k+v-x/k+v-x-y/k-x+w-y
  analyse();

  for (unsigned int i = 0; i < mExpressions.size(); i++)
  {
    SubstitutionValues_t *exp = mExpressions.at(i);
    ASTNode* currentNode = exp->current;
    for (unsigned int j = 0; j < mODEs.size(); j++)
    {
      std::pair<std::string, ASTNode*> ode = mODEs.at(j);
      ASTNode* odeRHS = ode.second;
      int index = parameterAlreadyCreated(exp);
      if (index >= 0)
      {
        exp->z_value = mExpressions.at(index)->z_value;
        replaceExpressionWithNewParameter(odeRHS, exp);
      }
      else
      {
        std::string zName = getUniqueNewParameterName();
        exp->z_value = zName;
        replaceExpressionWithNewParameter(odeRHS, exp);
      }
//     cout << "ode in main: " << SBML_formulaToL3String(odeRHS) << endl;
    }
  } 
  addParametersAndRateRules(hiddenSpecies);
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
  if (node == NULL)
  {
    return;
  }
  //cout << "node: " << SBML_formulaToL3String(node) << endl;
  //cout << "with: " << SBML_formulaToL3String(replaced) << endl;
  //cout << "by: " << SBML_formulaToL3String(replacement) << endl;
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
  ASTNode* z = new ASTNode(AST_NAME);
  z->setName(var.c_str());
  replaceExpressionInNodeWithNode(node, replaced, z);
}

std::string
ExpressionAnalyser::getUniqueNewParameterName()
{ 
  char number[4];
  sprintf(number, "%u", mNewVarCount);

  std::string name = mNewVarName + string(number);
  IdList ids = mModel->getAllElementIdList();
  while (ids.contains(name))
  {
    mNewVarCount++;
    sprintf(number, "%u", mNewVarCount);
    name = mNewVarName + string(number);
  }
  return name;
}


void
ExpressionAnalyser::addParametersAndRateRules(List* hiddenSpecies)
{
  for (unsigned int i = 0; i < mExpressions.size(); i++)
  {
    SubstitutionValues_t *exp = mExpressions.at(i);
    if (mModel->getParameter(exp->z_value) == NULL)
    {
      // create expression for z
      ASTNode* kx = new ASTNode(AST_MINUS);
      ASTNode* k = new ASTNode(AST_NAME);
      k->setName(exp->k_value.c_str());
      ASTNode* x = new ASTNode(AST_NAME);
      x->setName(exp->x_value.c_str());
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
      hiddenSpecies->add(zParam);

      delete zNode;
      delete math; //its children dxdt and minus1 deleted as part of this.
    }
  }
}


void
ExpressionAnalyser::replaceExpressionWithNewParameter(ASTNode* ode, SubstitutionValues_t* exp)
{
  if (exp->type == TYPE_K_MINUS_X || exp->type == TYPE_K_MINUS_X_MINUS_Y)
  {
    replaceExpressionInNodeWithVar(ode, exp->current, exp->z_value);
    //cout << "ode in new param var: " << SBML_formulaToL3String(ode) << endl;
    for (unsigned int i = 0; i < mExpressions.size(); i++)
    {
      SubstitutionValues_t *thisexp = mExpressions.at(i);

      if (thisexp->dxdt_expression != NULL)
      {
        replaceExpressionInNodeWithVar(thisexp->dxdt_expression, exp->current, exp->z_value);
      }
      if (thisexp->dydt_expression != NULL)
      {
        replaceExpressionInNodeWithVar(thisexp->dydt_expression, exp->current, exp->z_value);
      }
    }

  }
  if (exp->type == TYPE_K_PLUS_V_MINUS_X || exp->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y)
  {
    ASTNode* replacement = new ASTNode(AST_PLUS);
    ASTNode* z = new ASTNode(AST_NAME);
    z->setName(exp->z_value.c_str());
    ASTNode *v = exp->v_expression->deepCopy();
    replacement->addChild(z);
    replacement->addChild(v);
    replaceExpressionInNodeWithNode(ode, exp->current, replacement);
    //cout << "ode in new param node: " << SBML_formulaToL3String(ode) << endl;
    for (unsigned int i = 0; i < mExpressions.size(); i++)
    {
      SubstitutionValues_t *thisexp = mExpressions.at(i);

      if (thisexp->dxdt_expression != NULL)
      {
        //cout << "dxdt_b4: " << SBML_formulaToL3String(thisexp->dxdt_expression) << endl;
        replaceExpressionInNodeWithNode(thisexp->dxdt_expression, exp->current, replacement);
        //cout << "dxdt: " << SBML_formulaToL3String(thisexp->dxdt_expression) << endl;
      }
      if (thisexp->dydt_expression != NULL)
      {
        //cout << "dydt_b4: " << SBML_formulaToL3String(thisexp->dydt_expression) << endl;
        replaceExpressionInNodeWithNode(thisexp->dydt_expression, exp->current, replacement);
        //cout << "dydt: " << SBML_formulaToL3String(thisexp->dydt_expression) << endl;
      }
    }
  }
  if (exp->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y)
  {
    ASTNode* replacement = new ASTNode(AST_PLUS);
    ASTNode* z = new ASTNode(AST_NAME);
    z->setName(exp->z_value.c_str());
    ASTNode *v = exp->w_expression->deepCopy();
    replacement->addChild(z);
    replacement->addChild(v);
    //cout << "ode in new param node: " << SBML_formulaToL3String(ode) << endl;
    //cout << "current in new param node: " << SBML_formulaToL3String(exp->current) << endl;
    //cout << "replace in new param node: " << SBML_formulaToL3String(replacement) << endl;
    replaceExpressionInNodeWithNode(ode, exp->current, replacement);
    //cout << "ode in new param node: " << SBML_formulaToL3String(ode) << endl;
    for (unsigned int i = 0; i < mExpressions.size(); i++)
    {
      SubstitutionValues_t *thisexp = mExpressions.at(i);

      if (thisexp->dxdt_expression != NULL)
      {
        //cout << "dxdt_b4: " << SBML_formulaToL3String(thisexp->dxdt_expression) << endl;
        replaceExpressionInNodeWithNode(thisexp->dxdt_expression, exp->current, replacement);
        //cout << "dxdt: " << SBML_formulaToL3String(thisexp->dxdt_expression) << endl;
      }
      if (thisexp->dydt_expression != NULL)
      {
        //cout << "dydt_b4: " << SBML_formulaToL3String(thisexp->dydt_expression) << endl;
        replaceExpressionInNodeWithNode(thisexp->dydt_expression, exp->current, replacement);
        //cout << "dydt: " << SBML_formulaToL3String(thisexp->dydt_expression) << endl;
      }
    }
  }
}


bool
ExpressionAnalyser::addHiddenVariablesForKMinusX(List* hiddenSpecies)
{
  return true;
}

/*
* check that the variable names in these two expressions match
*/
bool
ExpressionAnalyser::matchesVariables(SubstitutionValues_t* exp, SubstitutionValues_t* exp1)
{
  if (exp->k_value == exp1->k_value &&
    exp->x_value == exp1->x_value &&
    exp->y_value == exp1->y_value)
  {
    return true;
  }
  return false;
}


/*
* Have we already created a parameter for this expression
* if so, return name
*/
int
ExpressionAnalyser::parameterAlreadyCreated(SubstitutionValues_t* exp)
{
  int match = -1;
  unsigned int i = 0;
  while (!match && i < mExpressions.size())
  {
    SubstitutionValues_t* thisexp = mExpressions.at(i);
    if (thisexp->z_value.empty() || !matchesVariables(thisexp, exp))
    {
      i++;
      continue;
    }
    if (thisexp->type == exp->type)
    {
      match = i;
    }
    else if (thisexp->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y && exp->type == TYPE_K_MINUS_X_MINUS_Y)
    {
      match = i;
    }
    i++;
  }
  return match;
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
bool ExpressionAnalyser::isNumericalConstantOrConstantParameter(ASTNode* node)
{
    if (!node->isName()) // some nodes, like * operators, don't seem to have a name in the first place
        return false;
    Parameter* parameter = mModel->getParameter(node->getName());
    bool isConstantParameter = (parameter != NULL) && (parameter->getConstant());
    bool isNumericalConstant = node->isNumber() && node->isConstant();
    return isNumericalConstant || isConstantParameter;
}

/*
* Reorder any instance of - x + y with y - x in the set of ODEs.
* Fages Algorithm 3.1 Step 1
*/
void ExpressionAnalyser::reorderMinusXPlusYIteratively()
{
  for (unsigned int i = 0; i < mExpressions.size(); i++)
  {
    SubstitutionValues_t* exp = mExpressions.at(i);
    if (exp->type != TYPE_MINUS_X_PLUS_Y)
      continue;
    ASTNode* ode = (mODEs.at(exp->odeIndex)).second;
    ASTNode* replacement = new ASTNode(AST_MINUS);
    ASTNode* y = new ASTNode(AST_NAME);
    y->setName((exp->y_value).c_str());
    ASTNode* x = new ASTNode(AST_NAME);
    x->setName((exp->x_value).c_str());
    replacement->addChild(y);
    replacement->addChild(x);
    replaceExpressionInNodeWithNode(ode, exp->current, replacement);
  }
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


