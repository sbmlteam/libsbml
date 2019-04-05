/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    ASTBasePlugin.h
 * @brief   Definition of ASTBasePlugin, the base class of extension entities
 *          plugged in AST derived classes in the SBML Core package.
 * @author  Sarah Keating
 * @author  Lucian Smith
 *
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
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
 * @class ASTBasePlugin
 * @sbmlbrief{core} Base class for extensions that plug into AST classes.
 *
 * @htmlinclude not-sbml-warning.html
 */

#ifndef ASTBasePlugin_h
#define ASTBasePlugin_h

#include <sbml/common/libsbml-config-common.h>

#include <sbml/common/sbmlfwd.h>

#ifdef __cplusplus
#include <vector>
#include <string>
#include <map>
#include <sbml/math/ASTNodeType.h>


#include <sbml/math/L3ParserSettings.h>
#include <sbml/SBMLTransforms.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class UnitDefinition;
class UnitFormulaFormatter;
class ArgumentsUnitsCheck;


struct ASTNodeValues_t {
  std::string name;
  ASTNodeType_t type;
  bool isFunction;
  std::string csymbolURL;
  AllowedChildrenType_t allowedChildrenType;
  std::vector<unsigned int> numAllowedChildren;
};


class LIBSBML_EXTERN ASTBasePlugin
{
public:

//#ifndef SWIG
//  typedef std::pair<double, bool>   ValueSet;
//  typedef std::map<const std::string, ValueSet> IdValueMap;
//#endif
  virtual const std::string& getStringFor(ASTNodeType_t type) const;
  virtual const char* getConstCharFor(ASTNodeType_t type) const;
  virtual const char* getConstCharCsymbolURLFor(ASTNodeType_t type) const;
  virtual ASTNodeType_t getASTNodeTypeFor(const std::string& symbol) const;
  virtual ASTNodeType_t getASTNodeTypeForCSymbolURL(const std::string& url) const;
  virtual bool hasCorrectNamespace(SBMLNamespaces* namespaces) const;
  virtual bool defines(ASTNodeType_t type) const;
  virtual bool defines(const std::string& name, bool strCmpIsCaseSensitive = false) const;
  virtual bool isFunction(ASTNodeType_t type) const;
  virtual bool isLogical(ASTNodeType_t type) const;
  virtual bool isMathMLNodeTag(const std::string& node) const;
  virtual bool isMathMLNodeTag(ASTNodeType_t type) const;
  virtual ExtendedMathType_t getExtendedMathType() const;
  virtual double evaluateASTNode(const ASTNode * node, const Model * m = NULL) const;
  virtual UnitDefinition * getUnitDefinitionFromPackage(UnitFormulaFormatter* uff, const ASTNode * node, bool inKL, int reactNo) const;

  const ASTNodeValues_t* getASTNodeValue(unsigned int n) const;
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
  //virtual void checkUnits(ArgumentsUnitsCheck* auc, const Model& m, const ASTNode& node, const SBase & sb, bool inKL, int reactNo) const;


  /**
  * Destroy this object.
  */
  virtual ~ASTBasePlugin();


  /**
  * Assignment operator for ASTBasePlugin.
  *
  * @param orig the object whose values are used as the basis of the
  * assignment.
  */
  ASTBasePlugin& operator=(const ASTBasePlugin& orig);


  /**
  * Creates and returns a deep copy of this ASTBasePlugin object.
  *
  * @return the (deep) copy of this ASTBasePlugin object.
  */
  virtual ASTBasePlugin* clone() const;


  /**
  * Returns the XML namespace (URI) of the package extension
  * of this plugin object.
  *
  * @return the URI of the package extension of this plugin object.
  */
  const std::string& getElementNamespace() const;


  /**
  * Returns the prefix of the package extension of this plugin object.
  *
  * @return the prefix of the package extension of this plugin object.
  */
  virtual const std::string& getPrefix() const;


  /**
  * Returns the package name of this plugin object.
  *
  * @return the package name of this plugin object.
  */
  virtual const std::string& getPackageName() const;




  /* open doxygen comment */

  // ---------------------------------------------------------
  //
  // virtual functions (internal implementation) which should 
  // be overridden by subclasses.
  //
  // ---------------------------------------------------------

  virtual int setSBMLExtension(const SBMLExtension* ext);

  virtual int setPrefix(const std::string& prefix);
  /**
  * Sets the parent SBML object of this plugin object to
  * this object and child elements (if any).
  * (Creates a child-parent relationship by this plugin object)
  *
  * This function is called when this object is created by
  * the parent element.
  * Subclasses must override this this function if they have one
  * or more child elements. Also, ASTBasePlugin::connectToParent(@if java SBase@endif)
  * must be called in the overridden function.
  *
  * @param sbase the SBase object to use.
  *
  * @see setSBMLDocument
  * @see enablePackageInternal
  */
  virtual void connectToParent(ASTNode *astbase);


  /**
  * Enables/Disables the given package with child elements in this plugin
  * object (if any).
  * (This is an internal implementation invoked from
  *  SBase::enablePackageInternal() function)
  *
  * Subclasses which contain one or more SBase derived elements should
  * override this function if elements defined in them can be extended by
  * some other package extension.
  *
  * @see setSBMLDocument
  * @see connectToParent
  */
  virtual void enablePackageInternal(const std::string& pkgURI,
    const std::string& pkgPrefix, bool flag);


  virtual bool stripPackage(const std::string& pkgPrefix, bool flag);


  /* end doxygen comment */

  // ----------------------------------------------------------


  /**
  * Gets the URI to which this element belongs to.
  * For example, all elements that belong to SBML Level&nbsp;3 Version&nbsp;1 Core
  * must would have the URI "http://www.sbml.org/sbml/level3/version1/core";
  * all elements that belong to Layout Extension Version&nbsp;1 for SBML Level&nbsp;3
  * Version&nbsp;1 Core must would have the URI
  * "http://www.sbml.org/sbml/level3/version1/layout/version1/"
  *
  * Unlike getElementNamespace, this function first returns the URI for this
  * element by looking into the SBMLNamespaces object of the document with
  * the its package name. if not found it will return the result of
  * getElementNamespace
  *
  * @return the URI of this ASTBasePlugin.
  *
  * @see getPackageName
  * @see getElementNamespace
  * @see SBMLDocument::getSBMLNamespaces
  * @see getSBMLDocument
  */
  std::string getURI() const;

  /* open doxygen comment */

  /**
  * Returns the parent ASTNode object to which this plugin
  * object connected.
  *
  * @return the parent ASTNode object to which this plugin
  * object connected.
  */
  ASTNode* getParentASTObject();

  /* end doxygen comment */

  /* open doxygen comment */

  /**
  * Returns the parent ASTNode object to which this plugin
  * object connected.
  *
  * @return the parent ASTNode object to which this plugin
  * object connected.
  */
  const ASTNode* getParentASTObject() const;


  /* end doxygen comment */

  /**
  * Sets the XML namespace to which this element belongs to.
  * For example, all elements that belong to SBML Level&nbsp;3 Version&nbsp;1 Core
  * must set the namespace to "http://www.sbml.org/sbml/level3/version1/core";
  * all elements that belong to Layout Extension Version&nbsp;1 for SBML Level&nbsp;3
  * Version&nbsp;1 Core must set the namespace to
  * "http://www.sbml.org/sbml/level3/version1/layout/version1/"
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
  */
  int setElementNamespace(const std::string &uri);

  /**
  * Returns the SBML level of the package extension of
  * this plugin object.
  *
  * @return the SBML level of the package extension of
  * this plugin object.
  */
  unsigned int getLevel() const;


  /**
  * Returns the SBML version of the package extension of
  * this plugin object.
  *
  * @return the SBML version of the package extension of
  * this plugin object.
  */
  unsigned int getVersion() const;


  /**
  * Returns the package version of the package extension of
  * this plugin object.
  *
  * @return the package version of the package extension of
  * this plugin object.
  */
  unsigned int getPackageVersion() const;



  /* open doxygen comment */
  virtual SBMLNamespaces * getSBMLNamespaces() const;



  /* end doxygen comment */
  friend class L3ParserSettings;
  friend class ASTNode;

  //The following functions don't do anything on their own, and are meant to be overridden by relevant plugins.
  /**
  * Renames all the SIdRef attributes on this node and its child nodes.
  *
  * @param oldid the old identifier.
  * @param newid the new identifier.
  */
  virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
  * Renames all the UnitSIdRef attributes on this node and its child nodes.
  *
  * The only place UnitSIDRefs appear in MathML <code>&lt;cn&gt;</code>
  * elements, so the effects of this method are limited to that.
  *
  * @param oldid the old identifier.
  * @param newid the new identifier.
  */
  virtual void renameUnitSIdRefs(const std::string& oldid, const std::string& newid);


  /** @cond doxygenLibsbmlInternal */
  /**
  * Replace any nodes of type AST_NAME with the name 'id' from the child
  * 'math' object with the provided ASTNode.
  *
  */
  virtual void replaceIDWithFunction(const std::string& id, const ASTNode* function);

  /**
  * This function checks the provided ASTNode function to see if it is a
  * known function with the wrong number of arguments.  If so, 'error' is
  * set and '-1' is returned.  If it has the correct number of arguments,
  * '1' is returned.  If the plugin knows nothing about the function, '0'
  * is returned.
  */
  virtual int checkNumArguments(const ASTNode* function, std::stringstream& error) const;
  /**
  * Get the precedence of this package function, or @c -1 if unknown
  */
  virtual int getL3PackageInfixPrecedence() const;

  /**
  * This function checks the provided ASTNode function to see if it is a
  * known function with the wrong number of arguments
  */
  virtual bool hasCorrectNumArguments(const ASTNode* function) const;
  /**
  * Returns @c true if this is a package function which should be written
  * special syntax that the package knows about, @c false otherwise.
  */
  virtual bool hasPackageOnlyInfixSyntax() const;

  /**
  * Returns @c true if this is a package function which should be written
  * special syntax that the package knows about, @c false otherwise.
  */
  virtual bool hasUnambiguousPackageInfixGrammar(const ASTNode *child) const;

  /**
  * Returns @c true if this is a package function which should be written as
  * "functionname(argumentlist)", @c false otherwise.
  */
  virtual bool isPackageInfixFunction() const;


protected:
  /* open doxygen comment */
  /**
  * Constructor. Creates an ASTBasePlugin object with the URI and
  * prefix of an package extension.
  */
  ASTBasePlugin(const std::string &uri);

  ASTBasePlugin();

  /**
  * Copy constructor. Creates a copy of this SBase object.
  *
  * @param orig the instance to copy.
  */
  ASTBasePlugin(const ASTBasePlugin& orig);

  /**
  * Visits the given ASTNode_t and continues the inorder traversal for nodes whose syntax are determined by packages.
  */
  virtual void visitPackageInfixSyntax(const ASTNode *parent,
    const ASTNode *node,
    StringBuffer_t  *sb,
    const L3ParserSettings* settings) const;


  /**
  * The generic parsing function for grammar lines that packages recognize, but not core.
  * When a package recognizes the 'type', it will parse and return the correct ASTNode.
  * If it does not recognize the 'type', or if the arguments are incorrect, NULL is returend.
  */
  virtual ASTNode* parsePackageInfix(L3ParserGrammarLineType_t type,
    std::vector<ASTNode*> *nodeList = NULL, std::vector<std::string*> *stringList = NULL,
    std::vector<double> *doubleList = NULL) const;


  /**
  * The user input a string of the form "name(...)", and we want to know if
  * 'name' is recognized by a package as being a particular function.  We already
  * know that it is not used in the Model as a FunctionDefinition.  Should do
  * caseless string comparison.  Return the type of the function, or @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}
  * if nothing found.
  */
  virtual ASTNodeType_t getPackageFunctionFor(const std::string& name, bool strCmpIsCaseSensitive = false) const;

  /*-- data members --*/

  //
  // An SBMLExtension derived object of corresponding package extension
  // The owner of this object is SBMLExtensionRegistry class.
  //
  const SBMLExtension  *mSBMLExt;

  //
  // Parent ASTNode object to which this plugin object
  // connected.
  //
  ASTNode                *mParentASTNode;

  //
  // XML namespace of corresponding package extension
  //
  std::string          mURI;

  //
  // SBMLNamespaces derived object of this plugin object.
  //
  SBMLNamespaces      *mSBMLNS;

  //
  // Prefix of corresponding package extension
  //
  std::string          mPrefix;

    std::vector<ASTNodeValues_t> mPkgASTNodeValues;
    ExtendedMathType_t mExtendedMathType;
  /* end doxygen comment */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */




#endif  /* ASTBasePlugin_h */
/** @endcond */

