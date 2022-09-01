/**
 * @file    ExpressionAnalyser.h
 * @brief   Definition of ExpressionAnalyser, a class for analysing expressions 
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
 *
 * @class ExpressionAnalyser
 * @sbmlbrief{core} Converter that sorts SBML rules and assignments.
 *
 * @htmlinclude libsbml-facility-only-warning.html
 *
 * ADD DESCRIPTION
 *
 * @section ExpressionAnalyser-usage Configuration and use of ExpressionAnalyser
 *
 * ExpressionAnalyser is enabled by creating a ConversionProperties object
 * with the option @c "inferReactions", and passing this properties object to
 * SBMLDocument::convert(@if java ConversionProperties@endif).  This
 * converter offers no other options.
 *
 * Implementation is based on the algorithm described in Fages et al, Theoretical Computer Science, 2015.
 *
 * @copydetails doc_section_using_sbml_converters
 */


#ifndef ExpressionAnalyser_h
#define ExpressionAnalyser_h

#include <sbml/common/extern.h>
#include <sbml/math/ASTNode.h>

#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN
typedef std::pair<std::string, std::string > pairString;

typedef enum
{
  TYPE_K_MINUS_X_MINUS_Y
  , TYPE_K_PLUS_V_MINUS_X_MINUS_Y
  , TYPE_K_MINUS_X_PLUS_W_MINUS_Y
  , TYPE_K_MINUS_X
  , TYPE_K_PLUS_V_MINUS_
  , TYPE_UNKNOWN
} ExpressionType_t;

/*
*
|*/
struct SubstitutionValues_t {
  std::string k_value;
  std::string x_value;
  std::string y_value;
  ASTNode* v_expression;
  ASTNode* w_expression;
  ExpressionType_t type;
  ASTNode* current;
  std::string z_value;
  unsigned int odeIndex;
};



class LIBSBML_EXTERN ExpressionAnalyser
{
public:


  /**
   * Creates a new ExpressionAnalyser object.
   */
  ExpressionAnalyser();


  /**
   * Copy constructor; creates a copy of an ExpressionAnalyser
   * object.
   *
   * @param obj the ExpressionAnalyser object to copy.
   */
  ExpressionAnalyser(const ExpressionAnalyser& obj);


  /**
  * Assignment operator for SBMLInferUnitsConverter.
  *
  * @param rhs the object whose values are used as the basis of the
  * assignment.
  */
  ExpressionAnalyser& operator=(const ExpressionAnalyser& rhs);


  /**
   * Creates and returns a deep copy of this ExpressionAnalyser
   * object.
   *
   * @return a (deep) copy of this converter.
   */
  virtual ExpressionAnalyser* clone() const;


  /**
   * Destroy this ExpressionAnalyser object.
   */
  virtual ~ExpressionAnalyser ();

  int setODEPairs(std::vector< std::pair< std::string, ASTNode*> > odes);

  int setModel(Model* m);

  void detectHiddenSpecies(List * hiddenSpecies);


private:
  /** @cond doxygenLibsbmlInternal */

  // functions that represents steps of algo 3.1

  void reorderMinusXPlusYIteratively(ASTNode* odeRHS);

  bool addHiddenVariablesForKMinusX(List* hiddenSpecies);

  void analyse();
  bool analyseNode(ASTNode* node, SubstitutionValues_t* value);
  // additional helper functions for algo 3.1

  /**
   * Checks whether a node is the plus sign in an expression -x+y, where x and y are variable species or variable parameters in a model.
   *
   * @param node the node to check
   * @return true if node is + in -x+y, otherwise false
   */
  bool isMinusXPlusY(ASTNode* node);

  /**
   * Checks whether a node is the second minus sign in an expression k-x-y, where x and y are variable species or variable parameters, and k is constant number or constant parameter, in a model.
   * 
   * @param node the node to check
   * @return true if node is second minus in k-x-y, other false
   */
  bool isKMinusXMinusY(ASTNode* node);

  /**
   * Checks whether a node is the minus sign in a k-x expression, where x is a variable species or variable parameter, and k is constant number or constant parameter, in a model.
   *
   * @param node the node to check
   * @return true if the node is the minus in k-x, false otherwise
   */
  pairString isKMinusX(ASTNode* node);

  /**
   * Searches for a node's parent and its index as the parent's child in a one-directional tree (nodes know their children, but not their parent).
   * E.g. if the node is the first child of a node, this function will return a pair (parent, 0).
   *
   * @param child node whose parent should be found
   * @param root root node of the tree to search
   * @return pair of parent and index - or (nullptr, NAN) if not found.
   */
  std::pair<ASTNode*, int> getParentNode(ASTNode* child, ASTNode* root);

  /**
   * Checks whether a node is a variable species or a variable parameter in a model.
   * 
   * @param node the node to check
   * @return true if the node is a variable species/parameter
   */
  bool isVariableSpeciesOrParameter(ASTNode* node);
  
  /**
   * Checks whether a node is a constant number or constant parameter in a model.
   *
   * @param node the node to check
   * @return true if the node is a constant number/parameter
   */
  bool isNumericalConstantOrConstantParameter(ASTNode* node);

  /*
  * takes a pair of strings aleady identified as representing k-x
  * return index of pair if a parameter has already been created for this
  */
  std::string parameterAlreadyCreated(SubstitutionValues_t *value);
 
  // member variables populated during analysis
  std::vector< std::pair< std::string, ASTNode*> > mODEs;

  std::vector< std::pair< pairString, std::string > > kMinusX;

  Model* mModel;

  std::vector <SubstitutionValues_t*> mExpressions;

  /** @endcond */

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* ExpressionAnalyser_h */

