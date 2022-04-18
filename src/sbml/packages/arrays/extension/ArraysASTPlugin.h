/**
 * @file ArraysASTPlugin.h
 * @brief Definition of the ArraysASTPlugin class.
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
 *
 * @class ArraysASTPlugin
 * @sbmlbrief{Arrays} Extension of AST.
 */


#ifndef ArraysASTPlugin_h
#define ArraysASTPlugin_h

#include <sbml/extension/ASTBasePlugin.h>


#ifdef __cplusplus

#include <vector>
#include <string>
#include <map>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN ArraysASTPlugin : public ASTBasePlugin
{
private:
  /** @cond doxygenLibsbmlInternal */
  void populateNodeTypes();
  /** @endcond */

public:

  /**
   * Creates a new ArraysASTPlugin object.
   *
   */
  ArraysASTPlugin();

  ArraysASTPlugin(const   ArraysASTPlugin& orig);

  
  /**
  * Assignment operator for ArraysASTPlugin.
  *
  * @param orig the object whose values are used as the basis of the
  * assignment.
  */
  ArraysASTPlugin& operator=(const ArraysASTPlugin& orig);


  /**
  * Creates and returns a deep copy of this ArraysASTPlugin object.
  *
  * @return the (deep) copy of this ArraysASTPlugin object.
  */
  virtual ArraysASTPlugin* clone() const;



  /**
  * Destructor for ArraysASTPlugin.
  */
  virtual ~ArraysASTPlugin();




  //virtual const char* getConstCharFor(ASTNodeType_t type) const;
  virtual bool hasCorrectNamespace(SBMLNamespaces* namespaces) const;
  virtual double evaluateASTNode(const ASTNode * node, const Model * m = NULL) const;
  virtual UnitDefinition * getUnitDefinitionFromPackage(UnitFormulaFormatter* uff, const ASTNode * node, bool inKL, int reactNo) const;
  virtual bool isMathMLNodeTag(const std::string& node) const;
  virtual bool isMathMLNodeTag(ASTNodeType_t type) const;
  //void readMathML(ASTNode & node, XMLInputStream & stream, std::string reqd_prefix, bool inRead) const;

  /**
   * Check if the node type is known to be allowed inside function definitions.
   *
   * Function definitions must be able to be evaluated without resort to outside information.
   * Therefore, some ASTNodes (like AST_TIME and AST_FUNCTION_RATE_OF) are disallowed
   * from appearing there.  This function checks whether this is true for a given type:
   * a return value of '-1' means the plugin has no knowledge of that type; a return
   * value of '1' means the plugin knows that the type is indeed allowed, and a
   * return value of '0' means that the plugin knows that the type is not allowed.
   */
  virtual int allowedInFunctionDefinition(ASTNodeType_t type) const;

  ArraysASTPlugin(const std::string& uri);


  friend class L3ParserSettings;
protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns true if this is a package function which should be written
   * special syntax that the package knows about, false otherwise.
   */
  virtual bool hasPackageOnlyInfixSyntax() const;

  /**
   * Get the precedence of this package function, or -1 if unknown.
   */
  virtual int getL3PackageInfixPrecedence() const;

  /**
   * Returns true if this is a package function which should be written
   * special syntax that the package knows about, false otherwise.
   */
  virtual bool hasUnambiguousPackageInfixGrammar(const ASTNode *child) const;

  /**
   * Visits the given ASTNode_t and continues the inorder traversal for nodes whose syntax are determined by packages.
   */
  void visitPackageInfixSyntax ( const ASTNode *parent,
                            const ASTNode *node,
                            StringBuffer_t  *sb,
                            const L3ParserSettings* settings) const;

  /**
   * This function checks the provided ASTNode function to see if it is a 
   * known function with the wrong number of arguments.  If so, 'error' is
   * set and '-1' is returned.  If it has the correct number of arguments,
   * '1' is returned.  If the plugin knows nothing about the function, '0' 
   * is returned.
   */
  virtual int checkNumArguments(const ASTNode* function, std::stringstream& error) const;

  /**
   * If 'type' indicates that the grammar line in question is one that the
   * arrays package can handle, the arguments from the three lists will be parsed,
   * and the parsed node returned.  If not, NULL is returned.
   */
  virtual ASTNode* parsePackageInfix(L3ParserGrammarLineType_t type, 
    std::vector<ASTNode*> *nodeList = NULL, std::vector<std::string*> *stringList = NULL,
    std::vector<double> *doubleList = NULL) const;

  /** @endcond */


private:
  void visitSelector      ( const ASTNode *parent,
                            const ASTNode *node,
                            StringBuffer_t  *sb,
                            const L3ParserSettings* settings) const;

  void visitVector        ( const ASTNode *parent,
                            const ASTNode *node,
                            StringBuffer_t  *sb,
                            const L3ParserSettings* settings) const;
  
  ASTNode* parseNamedSquareBrackets(ASTNode* parent, ASTNode* nodelist) const;

  ASTNode* parseCurlyBracesList(ASTNode* nodelist) const;

  ASTNode* parseCurlyBracesSemicolonList(ASTNode* nodelist) const;


  /** @endcond */
};


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif

#endif /* ArraysASTPlugin_h */
