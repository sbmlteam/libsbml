/**
 * @file ArraysASTPlugin.cpp
 * @brief Implementation of the ArraysASTPlugin class.
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
#include <sbml/packages/arrays/extension/ArraysASTPlugin.h>
#include <sbml/math/ASTNode.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/math/L3ParserSettings.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/math/MathML.h>
#include <sbml/math/L3FormulaFormatter.h>

/** @cond doxygenIgnored */

using namespace std;

/** @endcond */
LIBSBML_CPP_NAMESPACE_BEGIN

#ifdef __cplusplus

static string ARRAYS_XMLNS_L3V1V1;
static string ARRAYS_XMLNS_L3V2V1;

static const ASTNodeType_t AST_ARRAYS_TYPES[] =
{
  AST_LINEAR_ALGEBRA_VECTOR
  , AST_LINEAR_ALGEBRA_SELECTOR
};

static const char *AST_ARRAYS_STRINGS[] =
{
  "vector"
  , "selector"
};

void
ArraysASTPlugin::populateNodeTypes()
{
  vector<unsigned int> zero, onetothree;
  zero.push_back(1);
  onetothree.push_back(1);
  onetothree.push_back(2);
  onetothree.push_back(3);

  ASTNodeValues_t node;
  node.type = AST_LINEAR_ALGEBRA_VECTOR;
  node.name = "vector";
  node.csymbolURL = "";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_ANY;
  node.numAllowedChildren = zero;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_LINEAR_ALGEBRA_SELECTOR;
  node.name = "selector";
  node.csymbolURL = "";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = onetothree;

  mPkgASTNodeValues.push_back(node);


}

ArraysASTPlugin::ArraysASTPlugin() :
  ASTBasePlugin()
{
  mExtendedMathType = EM_ARRAYS;
  populateNodeTypes();
}

ArraysASTPlugin::~ArraysASTPlugin()
{
}


int ArraysASTPlugin::allowedInFunctionDefinition(ASTNodeType_t type) const
{
  if (defines(type))
  {
    return 1;
  }
  return -1;
}

/*
* Constructor
*/
ArraysASTPlugin::ArraysASTPlugin(const std::string& uri)
  : ASTBasePlugin(uri)
{
  mExtendedMathType = EM_ARRAYS;
  populateNodeTypes();
}

ArraysASTPlugin::ArraysASTPlugin(const ArraysASTPlugin& orig)
  : ASTBasePlugin(orig)
{

}


ArraysASTPlugin&
ArraysASTPlugin::operator=(const ArraysASTPlugin& rhs)
{
  if (&rhs != this)
  {
    this->ASTBasePlugin::operator = (rhs);
  }

  return *this;
}


ArraysASTPlugin*
ArraysASTPlugin::clone() const
{
  return new ArraysASTPlugin(*this);
}

//const char * ArraysASTPlugin::getConstCharFor(ASTNodeType_t type) const
//{
//  for (size_t t = 0; t < mPkgASTNodeTypes.size(); t++) {
//    if (mPkgASTNodeTypes[t] == type) {
//      return AST_ARRAYS_STRINGS[t];
//    }
//  }
//  return NULL;
//}

bool ArraysASTPlugin::hasCorrectNamespace(SBMLNamespaces * namespaces) const
{
  if (namespaces == NULL)
  {
    return false;
  }
  ArraysExtension ext;
  const string arraysns1 = ext.getXmlnsL3V1V1();

  XMLNamespaces* xmlns = namespaces->getNamespaces();
  if (xmlns->hasURI(arraysns1)) //|| xmlns->hasURI(ARRAYS_XMLNS_L3V2V1))
  {
    return true;
  }

  return false;
}

double ArraysASTPlugin::evaluateASTNode(const ASTNode * node, const Model * m) const
{
  //Alternatively, we could actually evaluate the draws from the distributions?  Or return means?
  return numeric_limits<double>::quiet_NaN();
}

UnitDefinition * ArraysASTPlugin::getUnitDefinitionFromPackage(UnitFormulaFormatter* uff, const ASTNode * node, bool inKL, int reactNo) const
{
  //OK, this *definitely* could be actually coded, so we could do unit checking on draws from distributions.  But it will be a fair amount of work to get right.
  return NULL;
}

bool ArraysASTPlugin::isMathMLNodeTag(const std::string & node) const
{
  if (node == "vector")
  {
    return true;
  }
  return false;
}

bool ArraysASTPlugin::isMathMLNodeTag(ASTNodeType_t type) const
{
  if (type == AST_LINEAR_ALGEBRA_VECTOR)
  {
    return true;
  }
  return false;
}

//void ArraysASTPlugin::readMathML(ASTNode& node, XMLInputStream& stream, std::string reqd_prefix, bool inRead) const
//{
//}


#endif /* __cplusplus */


/** @cond doxygenIgnored */
/** @endcond */


bool
ArraysASTPlugin::hasPackageOnlyInfixSyntax() const
{
  ASTNode* function = const_cast<ASTNode*>(getParentASTObject());
  if (function == NULL) return false;
  if (!defines(function->getType())) return false;

  switch(function->getType()) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
  case AST_LINEAR_ALGEBRA_VECTOR:
    return true; //x[y] and {x, y} and {x, y; p, q}
  default:
    return false;
  }
  return false;
}

int
ArraysASTPlugin::getL3PackageInfixPrecedence() const
{
  ASTNode* function = const_cast<ASTNode*>(getParentASTObject());
  if (function == NULL) return false;
  if (!defines(function->getType())) return false;

  switch(function->getType()) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
  case AST_LINEAR_ALGEBRA_VECTOR:
    return 8; //Everything is either a function or has unambiguous syntax.
  default:
    return -1;
  }
  return -1;
}

bool ArraysASTPlugin::hasUnambiguousPackageInfixGrammar(const ASTNode *child) const
{
  ASTNode* function = const_cast<ASTNode*>(getParentASTObject());
  if (function == NULL) return false;
  if (!defines(function->getType())) return false;

  switch(function->getType()) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
    if (function->getNumChildren() == 0) return true;
    if (function->getChild(0) == child) return false; //The *first* child of the selector needs parentheses in some situations!
    return true; //All other children are separated by commas, and thus don't need parentheses.
  case AST_LINEAR_ALGEBRA_VECTOR:
    return true; //Everything is either a function or has unambiguous syntax.
  default:
    return false;
  }
  return false;
}

void ArraysASTPlugin::visitPackageInfixSyntax ( const ASTNode *parent,
                                           const ASTNode *node,
                                           StringBuffer_t  *sb,
                                           const L3ParserSettings* settings) const
{
  ASTNodeType_t type = node->getType();
  if (!defines(type)) return;

  switch(type) {
  case AST_LINEAR_ALGEBRA_SELECTOR:
    return visitSelector(parent, node, sb, settings);
  case AST_LINEAR_ALGEBRA_VECTOR:
    return visitVector(parent, node, sb, settings);
  default:
    return;
  }
  return;
}

int ArraysASTPlugin::checkNumArguments(const ASTNode* function, std::stringstream& error) const
{
  if (function == NULL) return false;
  if (!defines(function->getType())) return false;
  string product = "";
  //unsigned int c=0;
  //unsigned int firstrow=0;

  switch(function->getType()) 
  {
    case AST_LINEAR_ALGEBRA_SELECTOR:
      switch(function->getNumChildren()) 
      {
      case 0:
        error << "The 'selector' function must have at least one argument: the vector or matrix in question.";
        return -1;
      case 1:
      case 2:
      case 3:
        return 1; //correct number
      default:
        error << "The 'selector' function may not have more than three arguments ('selector(a, x, y)'):  the first for the vector or matrix from which to select, the second for the index of the vector or the matrixrow of the matrix, and an optional third, which only applies to matrices, for the index of the selected matrixrow of the matrix.  Similarly, the bracketed form may have no more than two ('a[x, y]'), for the same reason.";
        return -1;
      }
    case AST_LINEAR_ALGEBRA_VECTOR:
      return 1; //Vectors and matrix rows can have any number of arguments.
    default:
      return 0;
  }
  return 0;
}


ASTNode*
ArraysASTPlugin::parsePackageInfix(L3ParserGrammarLineType_t type, 
    vector<ASTNode*> *nodeList, vector<std::string*> *stringList,
    vector<double> *doubleList) const
{
  ASTNode *node = NULL;
  //None of the arrays grammar lines have strings or doubles:
  if(stringList != NULL) return NULL;
  if(doubleList != NULL) return NULL;

  switch (type)
  {
  case INFIX_SYNTAX_NAMED_SQUARE_BRACKETS:
    if (nodeList == NULL) return NULL;
    if (nodeList->size() == 2)
    {
      node = parseNamedSquareBrackets(nodeList->at(0), nodeList->at(1));
    }
    else if (nodeList->size() == 1)
    {
      node = parseNamedSquareBrackets(nodeList->at(0), NULL);
    }
    break;
  case INFIX_SYNTAX_CURLY_BRACES:
    if (nodeList != NULL && nodeList->size() == 1)
    {
      node = parseCurlyBracesList(nodeList->at(0));
    }
    else if (nodeList == NULL)
    {
      node = parseCurlyBracesList(NULL);
    }
    break;
  case INFIX_SYNTAX_CURLY_BRACES_SEMICOLON:
    if (nodeList != NULL && nodeList->size() == 1)
    {
      node = parseCurlyBracesSemicolonList(nodeList->at(0));
    }
    break;
  default:
    break;
  }

  return node;
}




ASTNode* ArraysASTPlugin::parseCurlyBracesList(ASTNode* nodelist) const
{
  if (nodelist == NULL) 
  {
    nodelist = new ASTNode(AST_LINEAR_ALGEBRA_VECTOR);
  }
  else {
    nodelist->setType(AST_LINEAR_ALGEBRA_VECTOR);
  }
  return nodelist;
}

ASTNode* ArraysASTPlugin::parseCurlyBracesSemicolonList(ASTNode* nodelist) const
{
  return NULL;
}

ASTNode* ArraysASTPlugin::parseNamedSquareBrackets(ASTNode* parent, ASTNode* nodelist) const
{
  if (nodelist == NULL) {
    nodelist = new ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
  }
  else {
    nodelist->setType(AST_LINEAR_ALGEBRA_SELECTOR);
  }
  nodelist->insertChild(0, parent);
  return nodelist;
  //The following code can be used to allow square brackets with 3+ children:
  /*
  ASTNode* ret = nodelist;
  while (ret->getNumChildren() > 2) {
    ASTNode* newtop = new ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
    while (ret->getNumChildren() > 2) {
      ASTNode* thirdchild = ret->getChild(2);
      ret->removeChild(2);
      newtop->addChild(thirdchild);
    }
    newtop->insertChild(0, ret);
    ret = newtop;
  }
  return ret;
  */
}

void ArraysASTPlugin::visitSelector( const ASTNode *parent,
                                     const ASTNode *node,
                                     StringBuffer_t  *sb,
                                     const L3ParserSettings* settings) const
{
  unsigned int numChildren = node->getNumChildren();
  if (numChildren == 0) {
    //Invalid, but we still have to write something
    StringBuffer_append(sb, "selector()");
    return;
  }
  ASTNode* firstchild = node->getChild(0);
  L3FormulaFormatter_visit(node, firstchild, sb, settings);
  StringBuffer_appendChar(sb, '[');
  for (unsigned int child=1; child<numChildren; child++) {
    if (child>1) {
      StringBuffer_appendChar(sb, ',');
      StringBuffer_appendChar(sb, ' ');
    }
    L3FormulaFormatter_visit(node, node->getChild(child), sb, settings);
  }
  StringBuffer_appendChar(sb, ']');
}

void ArraysASTPlugin::visitVector( const ASTNode *parent,
                                   const ASTNode *node,
                                   StringBuffer_t  *sb,
                                   const L3ParserSettings* settings) const
{
  unsigned int numChildren = node->getNumChildren();
  StringBuffer_appendChar(sb, '{');
  for (unsigned int child=0; child<numChildren; child++) {
    if (child>0) {
      StringBuffer_appendChar(sb, ',');
      StringBuffer_appendChar(sb, ' ');
    }
    L3FormulaFormatter_visit(node, node->getChild(child), sb, settings);
  }
  StringBuffer_appendChar(sb, '}');
}


LIBSBML_CPP_NAMESPACE_END

