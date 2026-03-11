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
#include <sbml/conversion/SBMLRateRuleConverter.h>

#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN

typedef enum
{
    TYPE_K_MINUS_X_MINUS_Y
  , TYPE_K_PLUS_V_MINUS_X_MINUS_Y
  , TYPE_K_MINUS_X_PLUS_W_MINUS_Y
  , TYPE_K_MINUS_X
  , TYPE_K_PLUS_V_MINUS_X
  , TYPE_MINUS_X_PLUS_Y
  , TYPE_UNKNOWN
} ExpressionType_t;

/*
* the structure to contain the substitution values
*/
struct SubstitutionValues_t {
  std::string k_value;
  double k_real_value;
  std::string x_value;
  std::string y_value;
  ASTNode * dxdt_expression;
  ASTNode * dydt_expression;
  ASTNode* v_expression;
  ASTNode* w_expression;
  ExpressionType_t type;
  ASTNode* current;
  std::string z_value;
  ASTNode* z_expression;
  unsigned int odeIndex;
  unsigned int levelInExpression;
};



class LIBSBML_EXTERN ExpressionAnalyser
{
public:

    
    /**
    * Creates a new ExpressionAnalyser object.
    */
    ExpressionAnalyser();


    /**
    * Creates a new ExpressionAnalyser object with Model and ODEs
    */
    ExpressionAnalyser(Model* m, pairODEs ode);


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

    /**
    * Returns the list of hidden species
    */
    List* getHiddenSpecies();

    /**
    * Returns the number of hidden species
    */
    unsigned int getNumHiddenSpecies();

    unsigned int getNumHiddenNodes();


    Parameter* getHiddenSpecies(unsigned int index);

    /**
    * Function to work through all the expressions and detect hidden species
    */
    void detectHiddenSpecies(bool testing = false);

    /**
    * Function to create a blank substitution value
    */
    SubstitutionValues_t* createBlankSubstitutionValues();
    
    /**
    * Returns the number of expressions
    * Only used in testing
    */
    unsigned int getNumExpressions();

    /**
    * Returns the expression at the given index
    * Only used in testing
    */
    SubstitutionValues_t* getExpression(unsigned int index);

    /**
    * Function to work through all the mathematical expressions and create substitution values
    */
    bool analyseNode(ASTNode* node, SubstitutionValues_t* value);


    /**
    * Function to work through all the ODE expressions and invoke analyseNode where appropriate
    */
    void analyse();


    /**
    * Function to order the expressions by type
    */
    void orderExpressions();

    /**
    * Function to identify hidden species within expressions
    * and substitute with the correct mathematical expression
    * ie if we find k+v-x-y and k-x-y then replace k-x-y with newVar and replace k+v-x-y with newVar+v
    */
    void identifyHiddenSpeciesWithinExpressions();
  bool replaceExpressionInNodeWithNode(ASTNode* node, ASTNode* replaced, ASTNode* replacement);

    private:
    /** @cond doxygenLibsbmlInternal */
        pairODEs deepCopyODEs(pairODEs odes);

    /**
    * Function to match two substitution values matching the k parameter
    */
    bool matchesK(SubstitutionValues_t* values1, SubstitutionValues_t* values2);

    /**
    * Function to match two substitution values matching the k parameter string value
    */
    bool matchesKValue(SubstitutionValues_t* values1, SubstitutionValues_t* values2);
    
    /**
    * Function to match two substitution values matching the k parameter numerical value
    */
    bool matchesKRealValue(SubstitutionValues_t* values1, SubstitutionValues_t* values2);
    
    /**
    * Function to match two substitution values matching the x parameter
    */
    bool matchesXValue(SubstitutionValues_t* values1, SubstitutionValues_t* values2);

    /**
    * Function to match two substitution values matching the y parameter
    */
    bool matchesYValue(SubstitutionValues_t* values1, SubstitutionValues_t* values2);
    
    /**
    * Function to match two substitution values matching the v expression
    */
    bool matchesVExpression(SubstitutionValues_t* values1, SubstitutionValues_t* values2);

    /**
    * Function to match two substitution values matching the w expression
    */
    bool matchesWExpression(SubstitutionValues_t* values1, SubstitutionValues_t* values2);

    /**
    * Function to match two substitution values matching the dx/dt expression
    */
    bool matchesDxdtExpression(SubstitutionValues_t* values1, SubstitutionValues_t* values2);

    /**
    * Function to match two substitution values matching the dy/dt expression
    */
    bool matchesDydtExpression(SubstitutionValues_t* values1, SubstitutionValues_t* values2);

    /**
    * Function to match two substitution values matching the type
    */
    bool matchesType(SubstitutionValues_t* values1, SubstitutionValues_t* values2);

    /**
    * Function to get the substitution values by type
    */
    SubstitutionValues_t* getSubstitutionValuesByType(ExpressionType_t type);

    /**
    * Function to get the matching parent expression for the given expression
    * suppose current expression is k-x+w-y it's parent expression will be k-x-y
    */
    int getMatchingParentExpression(SubstitutionValues_t* value, unsigned int index);


    void addSingleNewParameter(SubstitutionValues_t* exp);

    void addNewParameterPlusVOrW(SubstitutionValues_t* exp, std::string var = "v");

    void addPreviousParameterPlusVOrW(SubstitutionValues_t* exp, SubstitutionValues_t* previous, std::string var = "v");

    /*
    * Return the ODE for the given variable
    * or an ASTNode representing zero if there is no time derivative
    */
    ASTNode* getODEFor(std::string name);

    ASTNode* getODE(unsigned int odeIndex);


    /*
    * Check whether the expression has a parent expression which may already have been analysed
    * in which case we do not need to re analyse the child expression
    * e.g. if we have k-x-y do not need to analyse k-x
    */
    bool parentExpressionExists(SubstitutionValues_t* current, SubstitutionValues_t* mightAdd);
    
    /*
    * Check whether the expression should be added to the list of expressions 
    * This involves checking whether it has a parent expression which may already have been analysed
    * and checking whether it already exists
    */
    bool shouldAddExpression(SubstitutionValues_t* value, ASTNodePair currentNode);

    /*
    * Check whether the expression already exists in the list of expressions
    */
    bool expressionExists(SubstitutionValues_t* current, SubstitutionValues_t* mightAdd);


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
    bool isNumericalConstantOrConstantParameter(ASTNode* node, bool& isNumber);
    
    /*
    * Function to get a unique name for a new parameter
    */
    std::string getUniqueNewParameterName();

    void substituteNodes();
    /*
    * Function to check whether the expressions are of the form k-x-y
    */
    bool isTypeKminusXminusY(unsigned int numChildren, ASTNode* rightChild, 
        ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value);


    /*
    * Function to check whether the expressions are of the form k-x
    */
    bool isTypeKminusX(unsigned int numChildren, ASTNode* rightChild,
        ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value);


    /*
    * Function to check whether the expressions are of the form k+v-x
    */
    bool isTypeKplusVminusX(unsigned int numChildren, ASTNode* rightChild,
        ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value);
 

    /*
    * Function to check whether the expressions are of the form k+v
    */
    bool isTypeKplusV(unsigned int numChildren, ASTNode* rightChild,
        ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value);

    /*
    * Function to check whether the expressions are of the form k+v-x-y
    */
    bool isTypeKplusVminusXminusY(unsigned int numChildren, ASTNode* rightChild,
        ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value);
  std::vector <SubstitutionValues_t*> mExpressions;
    friend class SBMLReactionConverter;


    /*
    * Function to check whether the expressions are of the form k-x+w-y
    */
    bool isTypeKminusXplusWminusY(unsigned int numChildren, ASTNode* rightChild,
        ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value);


    /*
    * Function to check whether the expressions are of the form k+w-x
    */
    bool isTypeWplusKminusX(unsigned int numChildren, ASTNode* rightChild,
        ASTNode* leftChild, ASTNodeType_t type, SubstitutionValues_t* value);

    // to do see if this is necessary ====================================================
     /**
    * Searches for a node's parent and its index as the parent's child in a one-directional tree (nodes know their children, but not their parent).
    * E.g. if the node is the first child of a node, this function will return a pair (parent, 0).
    *
    * @param child node whose parent should be found
    * @param root root node of the tree to search
    * @return pair of parent and index - or (nullptr, NAN) if not found.
    */
    std::pair<ASTNode*, int> getParentNode(const ASTNode* child, const ASTNode* root);

    /*
* THIS NEEDS PROPERLY SORTING IS DO THE FUNCTIONS THAT DEAL WITH MATCHING WHETHER A PARAMETER IS ALREADY IN THE MODEL
* ALSO NEED TO TAKE THE MATCHING OF THE ODES OUT OF THE COMPARISON OF EXPRESSIONS
* AND CHECK THAT THE MATCHES VARIABLES INCLUDES ALL THE VARIABLES
*/
    void addParametersAndRateRules(SubstitutionValues_t* exp = NULL);

//    bool replaceExpressionInNodeWithNode(ASTNode* node, ASTNode* replaced, ASTNode* replacement);

    friend class SBMLReactionConverter;
    // member variables populated during analysis
    pairODEs mODEs;
    

    Model* mModel;

 //   std::vector <SubstitutionValues_t*> mExpressions;
   
    // list of hidden species that are identified during the analysis
    List* mHiddenSpecies;

    // list of hidden nodes that are identified during the analysis
    List* mHiddenNodes;

    // variables to ensure unique new parameter name

    std::string mNewVarName;

    unsigned int mNewVarCount;
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

  