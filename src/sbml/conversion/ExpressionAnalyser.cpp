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


ExpressionAnalyser::ExpressionAnalyser()
    : mModel (NULL), 
      mODEs (),
    mNewVarName("newVar"),
    mNewVarCount(1),
    mHiddenSpecies (NULL)
{
}



ExpressionAnalyser::ExpressionAnalyser(Model * m, pairODEs odes)
    : mModel(m),
    mODEs(odes),
    mNewVarName("newVar"),
    mNewVarCount(1),
    mHiddenSpecies(NULL) 
{
  SBMLTransforms::mapComponentValues(mModel);
  mModel->populateAllElementIdList();
}

ExpressionAnalyser::ExpressionAnalyser(const ExpressionAnalyser& orig) :
  mModel( orig.mModel),
    mODEs(orig.mODEs),
    mNewVarName(orig.mNewVarName),
    mNewVarCount(orig.mNewVarCount),
    mHiddenSpecies(orig.mHiddenSpecies)
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
  mHiddenSpecies = NULL;
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

void ExpressionAnalyser::substituteParametersForExpressions(List* hiddenSpecies)
{
    if (hiddenSpecies == NULL || mExpressions.empty())
        return;

    // need to actually address the expressions in the correct order
   // replace k-x-y first with newParam type=TYPE_K_MINUS_X_MINUS_Y
   // and then k+v-x-y with newParam+v  type=TYPE_K_PLUS_V_MINUS_X_MINUS_Y
   // and then k-x+w-y with newParam+w  type=TYPE_K_MINUS_X_PLUS_W_MINUS_Y
    // and then k-x with newParam2      type=TYPE_K_MINUS_X
    // and then k+v-x with newParam2+v  type=TYPE_K_PLUS_V_MINUS_X  


    // need to go through the expressions and make sure that they are substituting the correct thing
    // this needs to be done before any parameters are substituted I think

    for (unsigned int j = 0; j < mExpressions.size(); j++)
    {
        SubstitutionValues_t* exp = mExpressions[j];
        if (exp->type == TYPE_K_MINUS_X_MINUS_Y && exp->z_value.empty())
        {
            std::string zName = getUniqueNewParameterName();
            exp->z_value = zName;
            mNewVarCount++;
        }
        else if (j > 0 &&
            exp->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y &&
            exp->z_expression == NULL)
        {
            // we have a value for  k-x-y
            ASTNode* replacement = new ASTNode(AST_PLUS);
            ASTNode* z = new ASTNode(AST_NAME);
            z->setName(mExpressions[j - 1]->z_value.c_str());
            ASTNode* v = exp->v_expression->deepCopy();
            replacement->addChild(z);
            replacement->addChild(v);
            exp->z_expression = replacement->deepCopy();

        }
    }
}

void ExpressionAnalyser::substituteParameters(List* hiddenSpecies, SubstitutionValues_t* exp)
{
    for (unsigned int j = 0; j < mODEs.size(); j++)
    {
        std::pair<std::string, ASTNode*> ode = mODEs.at(j);
        ASTNode* odeRHS = ode.second;
        bool index = isParameterAlreadyCreated(exp->z_value);
        if (index == true)
        {
            replaceExpressionWithNewParameter(odeRHS, exp);
        }
        else
        {
            std::string zName = getUniqueNewParameterName();
            exp->z_value = zName;
            mNewVarCount++;
//            replaceExpressionWithNewParameter(odeRHS, exp);
//            addParametersAndRateRules(hiddenSpecies, exp);
        }
        //cout << "ode in main: " << SBML_formulaToL3String(odeRHS) << endl;
    }
}

SubstitutionValues_t* ExpressionAnalyser::getSubstitutionValuesByType(ExpressionType_t type,
    size_t index)
{
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

/*
* Check whether two SubstitutionValues_t are identical.
 */
bool
ExpressionAnalyser::areIdenticalSubstitutionValues(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    //printSubstitutionValues(values1);
    //printSubstitutionValues(values2);

    if (values1->k_value == values2->k_value &&
        values1->x_value == values2->x_value &&
        values1->y_value == values2->y_value &&
        values1->type == values2->type)
        //&& if this is a new expression then the z_value will not have been set
        //values1->z_value == values2->z_value)
        //&& The same expression may come from different odes
        //values1->odeIndex == values2->odeIndex)
    {
        if ((util_isNaN(values1->k_real_value) && util_isNaN(values2->k_real_value)) ||
            util_isEqual(values1->k_real_value, values2->k_real_value))
        {
            if ((values1->dxdt_expression != NULL && values2->dxdt_expression != NULL &&
                values1->dxdt_expression->exactlyEqual(*(values2->dxdt_expression)) == false)
                ||
                (values1->dydt_expression != NULL && values2->dydt_expression != NULL &&
                    values1->dydt_expression->exactlyEqual(*(values2->dydt_expression)) == false)
                ||
                (values1->v_expression != NULL && values2->v_expression != NULL &&
                    values1->v_expression->exactlyEqual(*(values2->v_expression)) == false)
                ||
                (values1->w_expression != NULL && values2->w_expression != NULL &&
                    values1->w_expression->exactlyEqual(*(values2->w_expression)) == false)
                ||
                (values1->current != NULL && values2->current != NULL &&
                    values1->current->exactlyEqual(*(values2->current)) == false)
                )
            {
                //cout << "false " << endl;
                return false;
            }
            else
            {
                //cout << "true " << endl;
                return true;
            }
        }
        else
        {
            //cout << "true " << endl;
            return true;
        }
    }
    else
    {
        //cout << "false " << endl;
        return false;
    }
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
    alreadyExists = matchesK(current, mightAdd) &&
        matchesXValue(current, mightAdd) &&
        matchesDxdtExpression(current, mightAdd);

    //switch (current->type)
    //{
    //case TYPE_K_MINUS_X_MINUS_Y:
    //    switch (mightAdd->type)
    //    {
    //    case TYPE_K_MINUS_X_MINUS_Y:
    //        alreadyExists = matchesKValue(current, mightAdd) && 
    //            matchesXValue(current, mightAdd) && 
    //            matchesYValue(current, mightAdd) && 
    //            matchesDxdtExpression(current, mightAdd) && 
    //            matchesDydtExpression(current, mightAdd) ;
    //        break;
    //    case TYPE_K_PLUS_V_MINUS_X_MINUS_Y:
    //        alreadyExists = matchesKValue(current, mightAdd) && 
    //            matchesVExpression(current, mightAdd) && 
    //            matchesXValue(current, mightAdd) && 
    //            matchesYValue(current, mightAdd);
    //        break;
    //    case TYPE_K_MINUS_X_PLUS_W_MINUS_Y:
    //        alreadyExists = matchesKValue(current, mightAdd) && 
    //            matchesWExpression(current, mightAdd) && 
    //            matchesXValue(current, mightAdd) && 
    //            matchesYValue(current, mightAdd);
    //        break;
    //    case TYPE_K_MINUS_X:
    //        alreadyExists = matchesKValue(current, mightAdd) && 
    //            matchesXValue(current, mightAdd) &&
    //            matchesDxdtExpression(current, mightAdd);
    //         break;
    //    case TYPE_K_PLUS_V_MINUS_X:
    //        alreadyExists = matchesKValue(current, mightAdd) && 
    //            matchesVExpression(current, mightAdd) && 
    //            matchesXValue(current, mightAdd);
    //        break;
    //    default:
    //        break;
    //    }
    //    break;
    //case TYPE_K_PLUS_V_MINUS_X_MINUS_Y:
    //    switch (mightAdd->type)
    //    { // if we already have k+v-x-y then we need to record that we have k-x-y as well
    //        // so we only record a match value if one of the values doesn't match
    //    case TYPE_K_MINUS_X_MINUS_Y:
    //        alreadyExists = !matchesKValue(current, mightAdd) ||
    //            !matchesXValue(current, mightAdd) ||
    //            !matchesYValue(current, mightAdd) ||
    //            !matchesDxdtExpression(current, mightAdd) ||
    //            !matchesDydtExpression(current, mightAdd);
    //        break;
    //    case TYPE_K_PLUS_V_MINUS_X_MINUS_Y:
    //        alreadyExists = matchesKValue(current, mightAdd) && 
    //            matchesVExpression(current, mightAdd) && 
    //            matchesXValue(current, mightAdd) && 
    //            matchesYValue(current, mightAdd);
    //        break;
    //    case TYPE_K_MINUS_X_PLUS_W_MINUS_Y:
    //        alreadyExists = matchesKValue(current, mightAdd) && 
    //            matchesWExpression(current, mightAdd) && 
    //            matchesXValue(current, mightAdd) &&
    //            matchesYValue(current, mightAdd);
    //        break;
    //    case TYPE_K_MINUS_X:
    //        alreadyExists = matchesKValue(current, mightAdd) &&
    //            matchesXValue(current, mightAdd) &&
    //            matchesDxdtExpression(current, mightAdd);
    //        break;
    //    case TYPE_K_PLUS_V_MINUS_X:
    //        alreadyExists = matchesKValue(current, mightAdd) &&
    //            matchesXValue(current, mightAdd) &&
    //            matchesDxdtExpression(current, mightAdd);
    //        break;
    //    default:
    //        break;
    //    }
    //    break;
    //case TYPE_K_MINUS_X_PLUS_W_MINUS_Y:
    //    printSubstitutionValues(current);
    //    printSubstitutionValues(mightAdd);
    //    alreadyExists = matchesKValue(current, mightAdd) && 
    //        matchesWExpression(current, mightAdd) && 
    //        matchesXValue(current, mightAdd) && 
    //        matchesDxdtExpression(current, mightAdd) &&
    //        matchesYValue(current, mightAdd) &&
    //        matchesDydtExpression(current, mightAdd);
    //    break;
    //case TYPE_K_MINUS_X:
    //    alreadyExists = matchesKValue(current, mightAdd) && 
    //        matchesXValue(current, mightAdd);
    //    break;
    //case TYPE_K_PLUS_V_MINUS_X:
    //    alreadyExists = matchesKValue(current, mightAdd) && 
    //        matchesVExpression(current, mightAdd) && 
    //        matchesXValue(current, mightAdd);
    //    break;
    //default:
    //    break;
    //}

    return alreadyExists;
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

bool ExpressionAnalyser::matchesCurrentNode(SubstitutionValues_t* values1, SubstitutionValues_t* values2)
{
    return (values1->current != NULL && values2->current != NULL &&
        values1->current->exactlyEqual(*(values2->current)) == true);
}


void
ExpressionAnalyser::printSubstitutionValues(const SubstitutionValues_t* values)
{
    cout << "k_value: " << values->k_value << endl;
    cout << "k_real_value: " << values->k_real_value << endl;
        cout << "x_value: " << values->x_value << endl;
        cout << "y_value: " << values->y_value << endl;
        cout << "z_value: " << values->z_value << endl;
        if (values->dxdt_expression != NULL) 
            cout << "dxdt_expression: " << SBML_formulaToL3String(values->dxdt_expression) << " " << values->dxdt_expression << endl;
        else
            cout << "dxdt_expression: NULL" << endl;
        if (values->dydt_expression != NULL) 
            cout << "dydt_expression: " << SBML_formulaToL3String(values->dydt_expression) << " " << values->dydt_expression << endl;
        else
            cout << "dydt_expression: NULL" << endl;
        if (values->v_expression != NULL) 
            cout << "v_expression: " << SBML_formulaToL3String(values->v_expression) << " " << values->v_expression << endl;
        else
            cout << "v_expression: NULL" << endl;
        if (values->w_expression != NULL) 
            cout << "w_expression: " << SBML_formulaToL3String(values->w_expression) << " " << values->w_expression << endl;
        else
            cout << "w_expression: NULL" << endl;
        if (values->z_expression != NULL)
            cout << "z_expression: " << SBML_formulaToL3String(values->z_expression) << " " << values->z_expression << endl;
        else
            cout << "z_expression: NULL" << endl;
        cout << "type: " << values->type << endl;
        if (values->current != NULL) 
            cout << "current: " << SBML_formulaToL3String(values->current) << " " << values->current << endl;
        else
            cout << "current: NULL" << endl;
        cout << "odeIndex: " << values->odeIndex << endl;
}

SubstitutionValues_t* ExpressionAnalyser::createBlankSubstitutionValues()
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
ExpressionAnalyser::hasExpressionAlreadyBeenRecorded(SubstitutionValues_t* value)
{
  bool found = false;
  size_t size = mExpressions.size();

  while (size > 0 && !found)
  {
      found = expressionExists(mExpressions.at(size - 1), value);
      size--;
  }
  return found  ;
}


bool
ExpressionAnalyser::analyseNode(ASTNode* node, SubstitutionValues_t *value)
{
    unsigned int numChildren = node->getNumChildren();
    ASTNodeType_t type = node->getType();
    ASTNode* rightChild = node->getRightChild();
    ASTNode* leftChild = node->getLeftChild();
      cout << "RIGHT CHILD: " << SBML_formulaToL3String(rightChild) << endl;
      cout << "left CHILD: " << SBML_formulaToL3String(leftChild) << endl;

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



  //switch (type)
  //{
  ////case AST_PLUS:
  //  //  -x+y node binary; plus; left child type minus; rightchild var/const
  //  //           +
  //  //        -     y
  //  //        x
  //  // we might have encountered w+(k-x)as part of k-x+w-y but it might not be part of it
  //    if (rightChild->getNumChildren() == 2 && analyseNode(rightChild, value) && value->type == TYPE_K_MINUS_X)
  //    {
  //        value->w_expression = leftChild;
  //        value->type = TYPE_K_MINUS_X_PLUS_W_MINUS_Y;
  //        value->current = node;
  //        return true;
  //    }
  //  if (numChildren != 2 || rightChild->getType() != AST_NAME
  //    || leftChild->getType() != AST_MINUS
  //    || leftChild->getNumChildren() != 1)
  //    return false;

  //  // if we get to this point, the only thing left to check is 
  //  // whether the ->left->right grandchild (the x in -x+y) is a variable species.
  //  if (isVariableSpeciesOrParameter(leftChild->getChild(0)))
  //  {
  //    value->x_value = leftChild->getChild(0)->getName();
  //    value->y_value = rightChild->getName();
  //    value->dydt_expression = getODEFor(rightChild->getName());
  //    value->dxdt_expression = getODEFor(leftChild->getChild(0)->getName());
  //    value->type = TYPE_MINUS_X_PLUS_Y;
  //    value->current = node;
  //    return true;
  //  }
  //  break;

  //case AST_MINUS:
  //  //          -                -               
  //  //        k   x           -     y    
  //  //                      k   x      
  //  //  k-x or k-x-y node binary; right child (x,y) is variable
  //  //          -                    -                          -
  //  //      +      x            -         y              +            y
  //  //   k     v            +      x                 -       w
  //  //                  k      v                 k       x
  //  //    
  //  // //  k+v-x; right child x var; left child plus node with left child k constant
  //  //  k+v-x-y; rightchild var y; left child minus == k+v-x
  //  //  k-x+w-y; rightchild var y; left child plus with left child == k-x 
  //  if (numChildren != 2 || !isVariableSpeciesOrParameter(rightChild))
  //    return false;
  //  // if left child is  numerical constant or a parameter and right child variable, it IS k-x
  //  if (isNumericalConstantOrConstantParameter(leftChild, isNumber)
  //    && isVariableSpeciesOrParameter(rightChild))
  //  {
  //   
  //    if (isNumber)
  //    {
  //        value->k_value = "number";
  //        value->k_real_value = leftChild->getValue();
  //    }
  //    else
  //    {
  //        value->k_value = leftChild->getName();
  //    }
  //    value->x_value = rightChild->getName();
  //    value->dxdt_expression = getODEFor(rightChild->getName());
  //    value->type = TYPE_K_MINUS_X;
  //    value->current = node;
  //    return true;
  //  }
  //  // left child + with it's left child const we have k+v-x
  //  // left child + with it's left child k-x+w-y we have already finished
  //  else if (leftChild->getType() == AST_PLUS)
  //  {

  //      // TO DO fix this for k Or w being a number
  //      if (value->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y)
  //      {
  //          return true;
  //      }
  //      else if (analyseNode(leftChild, value))
  //      {
  //          if (value->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y)
  //          {
  //              value->y_value = rightChild->getName();
  //              value->dydt_expression = getODEFor(rightChild->getName());
  //              value->current = node;
  //              return true;
  //          }
  //          else if (value->type == TYPE_K_MINUS_X)
  //          {
  //              value->y_value = rightChild->getName();
  //              value->dydt_expression = getODEFor(rightChild->getName());
  //              value->w_expression = leftChild->getChild(1);
  //              value->type = TYPE_K_MINUS_X_PLUS_W_MINUS_Y;
  //              value->current = node;
  //              return true;

  //          }
  //      }
  //      else if (isNumericalConstantOrConstantParameter(leftChild->getChild(0), isNumber))
  //      {
  //          value->k_value = leftChild->getChild(0)->getName();
  //          value->x_value = rightChild->getName();
  //          value->dxdt_expression = getODEFor(rightChild->getName());
  //          value->v_expression = leftChild->getChild(1);
  //          value->type = TYPE_K_PLUS_V_MINUS_X;
  //          value->current = node;
  //          return true;
  //      }
  //  }
  //  else if (leftChild->getType() == AST_MINUS
  //    && isVariableSpeciesOrParameter(leftChild->getRightChild()))
  //  {
  //    // if left child is k+v-x we have k+v-x-ymean you'll have to come againthat you can pay3
  //    // or if left child is k-x we have k-x-y
  //    if (analyseNode(leftChild, value))
  //    {
  //      if (value->type == TYPE_K_PLUS_V_MINUS_X)
  //      {
  //        value->type = TYPE_K_PLUS_V_MINUS_X_MINUS_Y;
  //        value->y_value = rightChild->getName();
  //        value->dydt_expression = getODEFor(value->y_value);
  //        value->current = node;
  //        return true;
  //      }
  //      else if (value->type == TYPE_K_MINUS_X)
  //      {
  //        value->y_value = rightChild->getName();
  //        value->dydt_expression = getODEFor(rightChild->getName());
  //        value->type = TYPE_K_MINUS_X_MINUS_Y;
  //        value->current = node;
  //        return true;
  //      }
  //    }
  //    return false;
  //  }
  //  break;
  //default:
  //  return false;
  //}
  //return false;


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
ExpressionAnalyser::detect_minusXPlusYOnly()
{
    for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
    {
        std::pair<std::string, ASTNode*> ode = mODEs.at(odeIndex);
        ASTNode* odeRHS = ode.second;
        //odeRHS->decompose();
        odeRHS->reduceToBinary();
        odeRHS->decompose();
        List* operators = odeRHS->getListOfNodes((ASTNodePredicate)ASTNode_isOperator);
        ListIterator it = operators->begin();

        while (it != operators->end())
        {
            ASTNode* currentNode = (ASTNode*)*it;
                cout << "current node in not -x+y analyze: " << SBML_formulaToL3String(currentNode) << endl;
            if (currentNode->getType() != AST_PLUS)
            {
                it++;
                continue;
            }
            SubstitutionValues_t* value = createBlankSubstitutionValues();
            unsigned int numChildren = currentNode->getNumChildren();
            ASTNode* rightChild = currentNode->getRightChild();
            ASTNode* leftChild = currentNode->getLeftChild();
            //  -x+y node binary; plus; left child type minus; rightchild var/const
            //           +
            //        -     y
            //        x
            if (numChildren != 2 || rightChild->getType() != AST_NAME
                || leftChild->getType() != AST_MINUS
                || leftChild->getNumChildren() != 1)
            {
                it++;
                continue;
            }

            // if we get to this point, the only thing left to check is 
            // whether the ->left->right grandchild (the x in -x+y) is a variable species.
            if (isVariableSpeciesOrParameter(leftChild->getChild(0)))
            {
                value->x_value = leftChild->getChild(0)->getName();
                value->y_value = rightChild->getName();
                value->dydt_expression = getODEFor(rightChild->getName());
                value->dxdt_expression = getODEFor(leftChild->getChild(0)->getName());
                value->type = TYPE_MINUS_X_PLUS_Y;
                value->current = currentNode;
                value->odeIndex = odeIndex;
                if (!hasExpressionAlreadyBeenRecorded(value))
                {
                    printSubstitutionValues(value);
                    mExpressions.push_back(value);
                }

            }

            it++;
        }
    }

}
void
ExpressionAnalyser::analyse(bool minusXPlusYOnly)
{
  for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  {
    std::pair<std::string, ASTNode*> ode = mODEs.at(odeIndex);
    ASTNode* odeRHS = ode.second;
    odeRHS->decompose();
    odeRHS->reduceToBinary();
    List* operators = odeRHS->getListOfNodes((ASTNodePredicate)ASTNode_isOperator);
    ListIterator it = operators->begin();

    while (it != operators->end())
    {
      ASTNode* currentNode = (ASTNode*)*it;
      if (minusXPlusYOnly)
        cout << "current node in -x+y analyze: " << SBML_formulaToL3String(currentNode)  << endl;
      else
        cout << "current node in not -x+y analyze: " << SBML_formulaToL3String(currentNode) << endl;
      if (minusXPlusYOnly && currentNode->getType() != AST_PLUS)
      {
        it++;
        continue;
      }
      SubstitutionValues_t* value = createBlankSubstitutionValues();

      if (analyseNode(currentNode, value))
      {
        value->odeIndex = odeIndex;
        if (!hasExpressionAlreadyBeenRecorded(value))
        {
            //printSubstitutionValues(value);
            mExpressions.push_back(value);
        }
      }
      it++;
    }
  }
}

void ExpressionAnalyser::orderExpressions()
{
  for (unsigned int i = 0; i < mExpressions.size(); i++)
  {
    for (unsigned int j = i + 1; j < mExpressions.size(); j++)
    {
      if (mExpressions[i]->type > mExpressions[j]->type)
      {
        SubstitutionValues_t* temp = mExpressions[i];
        mExpressions[i] = mExpressions[j];
        mExpressions[j] = temp;
      }
    }
  }
}

void
ExpressionAnalyser::detectHiddenSpecies(List * hiddenSpecies)
{
  // find -x+y and replace with y-x 
  // actually don't have to do this if we decompose the AST
  // 
  //detect_minusXPlusYOnly();
  //for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  //{
  //    cout << mODEs[odeIndex].first << ": " << m(mODEs[odeIndex].second) << endl;
  //}

  //reorderMinusXPlusYIteratively();
  mExpressions.clear();
  
  // find cases of k-x/k-x-y/k+v-x/k+v-x-y/k-x+w-y
  analyse();
  //for (unsigned int odeIndex = 0; odeIndex < mODEs.size(); odeIndex++)
  //{
  //    cout << mODEs[odeIndex].first << ": " << SBML_formulaToL3String(mODEs[odeIndex].second) << endl;
  //}
  substituteParametersForExpressions(hiddenSpecies);
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
  return mNewVarName + std::to_string(mNewVarCount);
}


void
ExpressionAnalyser::addParametersAndRateRules(List* hiddenSpecies, SubstitutionValues_t* exp)
{
    if (exp->z_value.empty()) return;
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
      hiddenSpecies->add(zParam);

      delete zNode;
      delete math; //its children dxdt and minus1 deleted as part of this.
    //}
  }
}


void
ExpressionAnalyser::replaceExpressionWithNewParameter(ASTNode* ode, SubstitutionValues_t* exp)
{
  if (exp->type == TYPE_K_MINUS_X || exp->type == TYPE_K_MINUS_X_MINUS_Y)
  {
      printSubstitutionValues(exp);
    replaceExpressionInNodeWithVar(ode, exp->current, exp->z_value);
    //cout << "ode in new param var: " << SBML_formulaToL3String(ode) << endl;
    for (unsigned int i = 0; i < mExpressions.size(); i++)
    {
      SubstitutionValues_t *thisexp = mExpressions.at(i);
      printSubstitutionValues(thisexp);
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


/*
* Have we already created a parameter for this expression
* if so, return name
*/
bool
ExpressionAnalyser::isParameterAlreadyCreated(std::string& name)
{
    if (name.empty())
    {
        return false;
    }
    else
    {
        mModel->clearAllElementIdList();
        mModel->populateAllElementIdList();
        IdList& ids = mModel->getAllElementIdList();
        return ids.contains(name);
    }
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
        value->dydt_expression = getODEFor(rightChild->getName());
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
      value->dxdt_expression = getODEFor(rightChild->getName());
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
        value->dxdt_expression = getODEFor(rightChild->getName());
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
      value->v_expression = rightChild;
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
        value->dydt_expression = getODEFor(rightChild->getName());
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
        value->dydt_expression = getODEFor(rightChild->getName());
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
        value->w_expression = leftChild;
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


