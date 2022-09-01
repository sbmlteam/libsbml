
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
  return LIBSBML_OPERATION_SUCCESS;
}


void
ExpressionAnalyser::detectHiddenSpecies(List * hiddenSpecies)
{
  for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  {
    std::pair<std::string, ASTNode*> ode = mODEs.at(odeIndex);
    ASTNode* odeRHS = ode.second;
    odeRHS->reduceToBinary();
    //      Step 1: iterative, in-place replacement of any -x+y terms with y-x terms
    reorderMinusXPlusYIteratively(odeRHS);

    // Step 2 TODO
    List* operators = odeRHS->getListOfNodes((ASTNodePredicate)ASTNode_isOperator);
    ListIterator it = operators->begin();
    while (it != operators->end())
    {
      ASTNode* currentNode = (ASTNode*)*it;
      if (isKMinusXMinusY(currentNode))
      {
        currentNode->printMath();
        // (a) introduce z=k-x-y with dz/dt = -dx/dt-dy/dt (add to list of additional ODEs to add at the end)
        // TODO
        // (b) replace in ALL ODEs (not just current) k-x-y with z (interior loop over mODEs again?)
        // (c) replace in ALL ODEs (not just current) k+v-x-y with v+z
        // (d) replace in ALL ODEs (not just current) k-x+w-y with w+z
      }
      it++;
    }
    addHiddenVariablesForKMinusX(odeRHS, hiddenSpecies, operators);
    // Step 3
    //it = operators->begin();
    //while (it != operators->end())
    //{
    //    ASTNode* currentNode = (ASTNode*)*it;
    //    if (isKMinusX(currentNode, mModel))
    //    {
    //        // remember x name for later, before we replace the current node
    //        std::string xName = std::string(currentNode->getRightChild()->getName());

    //        // (a)
    //        // introduce z=k-x
    //        Parameter* zParam = mModel->createParameter();
    //        const std::string zName = "z" + std::to_string(mModel->getNumParameters());
    //        zParam->setId(zName);
    //        zParam->setConstant(false);
    //        hiddenSpecies.add(zParam);

    //        // replace k - x with z in current ODE
    //        ASTNode* z = new ASTNode(ASTNodeType_t::AST_NAME);
    //        z->setName(zName.c_str());
    //        std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
    //        ASTNode* currentParent = currentParentAndIndex.first;
    //        int index = currentParentAndIndex.second;
    //        if (currentParent != NULL)
    //        {
    //          currentParent->replaceChild(index, z, true);
    //          // intentionally, don't delete z as it's now owned by currentParent!
    //        }

    //        // add raterule defining dz/dt = -dxdt
    //        ASTNode* dxdt = odeRHS->deepCopy();
    //        RateRule* raterule = mModel->createRateRule();
    //        raterule->setVariable(zName);
    //        ASTNode* math = new ASTNode(ASTNodeType_t::AST_TIMES);
    //        ASTNode* minus1 = new ASTNode(ASTNodeType_t::AST_REAL);
    //        minus1->setValue(-1.0);
    //        math->addChild(minus1);
    //        math->addChild(dxdt);
    //        raterule->setMath(math);
    //        delete math; //its children dxdt and minus1 deleted as part of this.

    //        // TODO
    //        // (b) replace in ALL ODEs (not just current) k-x with z
    //        // (c) replace in ALL ODEs (not just current) k+v-x with v+z
    //    }
    //    it++;
    //}
  }

  //return hiddenSpecies;
}

bool
ExpressionAnalyser::addHiddenVariablesForKMinusX(ASTNode* odeRHS, List* hiddenSpecies, 
                                                    List* operators)
{
  ListIterator it = operators->begin();
  while (it != operators->end())
  {
    ASTNode* currentNode = (ASTNode*)*it;
    pairString KX = isKMinusX(currentNode);
    if (KX.first.empty() == false)
    {
      std::string zName = parameterKminusXAlreadyCreated(KX);
      if (!zName.empty())
      {

        // replace k - x with z in current ODE
        ASTNode* z = new ASTNode(ASTNodeType_t::AST_NAME);
        z->setName(zName.c_str());
        std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
        ASTNode* currentParent = currentParentAndIndex.first;
        int index = currentParentAndIndex.second;
        if (currentParent != NULL)
        {
          currentParent->replaceChild(index, z, true);
          // intentionally, don't delete z as it's now owned by currentParent!
        }

      }
      else
      {
        const std::string zName = "z" + std::to_string(mModel->getNumParameters());
        kMinusX.push_back(make_pair(KX, zName));

        // replace k - x with z in current ODE
        ASTNode* z = new ASTNode(ASTNodeType_t::AST_NAME);
        z->setName(zName.c_str());
        std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
        ASTNode* currentParent = currentParentAndIndex.first;
        int index = currentParentAndIndex.second;
        if (currentParent != NULL)
        {
          currentParent->replaceChild(index, z, true);
          // intentionally, don't delete z as it's now owned by currentParent!
        }
        // introduce z=k-x
        Parameter* zParam = mModel->createParameter();
        zParam->setId(zName);
        zParam->setConstant(false);
        hiddenSpecies->add(zParam);

        // add raterule defining dz/dt = -dxdt
        ASTNode* dxdt = odeRHS->deepCopy();
        RateRule* raterule = mModel->createRateRule();
        raterule->setVariable(zName);
        ASTNode* math = new ASTNode(ASTNodeType_t::AST_TIMES);
        ASTNode* minus1 = new ASTNode(ASTNodeType_t::AST_REAL);
        minus1->setValue(-1.0);
        math->addChild(minus1);
        math->addChild(dxdt);
        raterule->setMath(math);
        delete math; //its children dxdt and minus1 deleted as part of this.
      }
      //  variableAdded = true;
      //}

                   // (c) replace in ALL ODEs (not just current) k+v-x with v+z
    }
    it++;
  }
  return true;
}
std::string
ExpressionAnalyser::parameterKminusXAlreadyCreated(pairString kx)
{
  std::string renamed = "";
  bool match = false;
  unsigned int i = 0;
  while (!match && i < kMinusX.size())
  {
    if (kMinusX.at(i).first == kx)
    {
      renamed = kMinusX.at(i).second;
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

    // if right child is not a variable species, it's not -x+y
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
                //Swap -x+y node for y-x node
                ASTNode* yMinusX = new ASTNode(ASTNodeType_t::AST_MINUS);
                std::pair<ASTNode*, int> currentParentAndIndex = getParentNode(currentNode, odeRHS);
                ASTNode* currentParent = currentParentAndIndex.first;
                int index = currentParentAndIndex.second;
                currentParent->replaceChild(index, yMinusX, true);
                numMinusXPlusY++;
            }
            it++;
        }
        delete operators;
    }
}

std::pair<ASTNode*, int> ExpressionAnalyser::getParentNode(ASTNode* child, ASTNode* root)
{
    for (unsigned int i = 0; i < root->getNumChildren(); i++)
    {
        if (root->getChild(i) == child)
        {
            return std::pair<ASTNode*, int>(root, i);
        }
    }
    for (unsigned int i = 0; i < root->getNumChildren(); i++)
    {
        return getParentNode(child, root->getChild(i));
    }
    return std::pair<ASTNode*, int>(NULL, (int)(NAN));
}

/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


