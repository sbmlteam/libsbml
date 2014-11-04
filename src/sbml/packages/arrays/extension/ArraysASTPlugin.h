/**
 * @file    ArraysASTPlugin.h
 * @brief   Definition of ArraysASTPlugin, the plugin class of
 *          arrays package for the AST element.
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
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

#ifndef ArraysASTPlugin_h
#define ArraysASTPlugin_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/arrays/common/arraysfwd.h>
#include <sbml/SBMLTypeCodes.h>

#ifdef __cplusplus

#include <sbml/SBMLErrorLog.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/extension/ASTBasePlugin.h>
#include <sbml/packages/arrays/math/ASTArraysVectorFunctionNode.h>
#if (0)
#include <sbml/packages/arrays/math/ASTArraysMatrixFunctionNode.h>
#endif
LIBSBML_CPP_NAMESPACE_BEGIN


typedef enum
{
  AST_TYPECODE_VECTOR_CONSTRUCTOR = 50
#if (0)
, AST_TYPECODE_MATRIX_CONSTRUCTOR
#endif
} ASTArrays_Class_TypeCode_t;



class LIBSBML_EXTERN ArraysASTPlugin : public ASTBasePlugin
{
public:

  /**
   * Constructor
   */
  ArraysASTPlugin (const std::string &uri);


  /**
   * Copy constructor. Creates a copy of this SBase object.
   */
  ArraysASTPlugin(const ArraysASTPlugin& orig);


  /**
   * Destroy this object.
   */
  virtual ~ArraysASTPlugin ();


  /**
   * Assignment operator for ArraysASTPlugin.
   */
  ArraysASTPlugin& operator=(const ArraysASTPlugin& orig);


  /**
   * Creates and returns a deep copy of this ArraysASTPlugin object.
   * 
   * @return a (deep) copy of this SBase object
   */
  virtual ArraysASTPlugin* clone () const;




  // ---------------------------------------------------------
  //
  // virtual functions (internal implementation) which should
  // be overridden by subclasses.
  //
  // ---------------------------------------------------------

  /** @cond doxygenLibsbmlInternal */


  /**
   * Sets the parent SBML object of this plugin object to
   * this object and child elements (if any).
   * (Creates a child-parent relationship by this plugin object)
   *
   * This function is called when this object is created by
   * the parent element.
   * Subclasses must override this this function if they have one
   * or more child elements.Also, SBasePlugin::connectToParent()
   * must be called in the overridden function.
   *
   * @param sbase the SBase object to use
   *
   * @see setSBMLDocument
   * @see enablePackageInternal
   */
  void connectToParent (ASTBase *astbase);


  /**
   * Enables/Disables the given package with child elements in this plugin
   * object (if any).
   * (This is an internal implementation invoked from
   *  SBase::enablePakcageInternal() function)
   *
   * @note Subclasses in which one or more SBase derived elements are
   * defined must override this function.
   *
   * @see setSBMLDocument
   * @see connectToParent
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);
  /** @endcond */
  virtual const std::string& getPackageName() const;


  //virtual void write(XMLOutputStream& stream) const;
  virtual bool read(XMLInputStream& stream, const std::string& reqd_prefix,
                                            const XMLToken& currentElement);

  //virtual void addExpectedAttributes(ExpectedAttributes& attributes, 
  //                                   XMLInputStream& stream, int type);

  //virtual bool readAttributes (const XMLAttributes& attributes,
  //                             const ExpectedAttributes& expectedAttributes,
  //                             XMLInputStream& stream, XMLToken element,
  //                             int type);


  //virtual void writeAttributes(XMLOutputStream& stream, int type) const;
  //virtual void writeXMLNS(XMLOutputStream& stream) const;
  virtual int getTypeFromName(const std::string& name) const;
  virtual const char * getNameFromType(int type) const;
  virtual bool isFunction(int type) const;
  virtual bool representsUnaryFunction(int type) const;
  virtual bool representsBinaryFunction(int type) const;
  virtual bool representsNaryFunction(int type) const;
  virtual bool representsQualifier(int type) const;
  virtual bool isFunctionNode(int type) const;
  virtual bool isTopLevelMathMLFunctionNodeTag(const std::string& name) const;

  virtual const ASTBase * getMath() const;

  virtual bool isSetMath() const;

  virtual void createMath(int type);

  virtual int addChild(ASTBase * child);

  virtual ASTBase* getChild (unsigned int n) const;

  virtual unsigned int getNumChildren() const;

  virtual int insertChild(unsigned int n, ASTBase* newChild);

  virtual int prependChild(ASTBase* newChild);

  virtual int removeChild(unsigned int n);

  virtual int replaceChild(unsigned int n, ASTBase* newChild, bool delreplaced);

  virtual int swapChildren(ASTFunction* that);

  ASTArraysVectorFunctionNode * getVector() const;

#if (0)
  ASTArraysMatrixFunctionNode * getMatrix() const;
#endif

  ArraysASTNodeType_t getASTType() const;

  friend class L3ParserSettings;
protected:
  /** @cond doxygenLibsbmlInternal */

  virtual bool hasCorrectNumberArguments(int type) const;

  virtual bool isWellFormedNode(int type) const;

  void reset();

  bool readVector(XMLInputStream& stream, const std::string& reqd_prefix,
                        const XMLToken& currentElement);
  
  
#if (0) // take out matrix
  bool readMatrix(XMLInputStream& stream, const std::string& reqd_prefix,
                        const XMLToken& currentElement);


  bool readMatrixRow(XMLInputStream& stream, const std::string& reqd_prefix,
                        const XMLToken& currentElement);
#endif
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns true if this is a package function which should be written as
   * "functionname(argumentlist)", false otherwise.
   */
  virtual bool isPackageInfixFunction() const;

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


  /**
   * The user input a string of the form "name(...)", and we want to know if
   * 'name' is recognized by a package as being a particular function.  We already
   * know that it is not used in the Model as a FunctionDefinition.  Should do
   * caseless string comparison.  Return the type of the function, or AST_UNKNOWN
   * if nothing found.
   */
  virtual int getPackageFunctionFor(const std::string& name) const;
  /** @endcond */

  /*-- data members --*/
  ASTArraysVectorFunctionNode* mVector;

#if (0)
  ASTArraysMatrixFunctionNode* mMatrix;
#endif
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
#if (0)
  void visitMatrix        ( const ASTNode *parent,
                            const ASTNode *node,
                            StringBuffer_t  *sb,
                            const L3ParserSettings* settings) const;
#endif
  
  ASTNode* parseNamedSquareBrackets(ASTNode* parent, ASTNode* nodelist) const;

  ASTNode* parseCurlyBracesList(ASTNode* nodelist) const;

  ASTNode* parseCurlyBracesSemicolonList(ASTNode* nodelist) const;


  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* ArraysASTPlugin_h */
