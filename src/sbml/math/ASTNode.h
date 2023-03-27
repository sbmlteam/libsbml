/**
 * @file    ASTNode.h
 * @brief   Abstract Syntax Tree (AST) for representing formula trees.
 * @author  Ben Bornstein
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class ASTNode
 * @sbmlbrief{core} Abstract Syntax Tree (AST) representation of a
 * mathematical expression.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * Abstract Syntax Trees (ASTs) are a simple kind of data structure used in
 * libSBML for storing mathematical expressions.  The ASTNode is the
 * cornerstone of libSBML's AST representation.  An AST "node" represents the
 * most basic, indivisible part of a mathematical formula and come in many
 * types.  For instance, there are node types to represent numbers (with
 * subtypes to distinguish integer, real, and rational numbers), names
 * (e.g., constants or variables), simple mathematical operators, logical
 * or relational operators and functions. LibSBML ASTs provide a canonical,
 * in-memory representation for all mathematical formulas regardless of
 * their original format (which might be MathML or might be text strings).
 *
 * @copydetails doc_what_is_astnode
 *
 * @if clike <h3><a class="anchor" name="ASTNodeType_t">
 * ASTNodeType_t</a></h3> @else <h3><a class="anchor"
 * name="ASTNodeType_t">The set of possible %ASTNode types</a></h3> @endif@~
 *
 * @copydetails doc_astnode_types
 *
 * <h3><a class="anchor" name="math-convert">Converting between ASTs and text strings</a></h3>
 *
 * The text-string form of mathematical formulas produced by 
 * @sbmlfunction{formulaToString,ASTNode_t} and @sbmlfunction{formulaToL3String,ASTNode_t}, 
 * and read by @sbmlfunction{parseFormula,String} and @sbmlfunction{parseL3Formula,String}
 * are in a simple C-inspired infix notation.  A
 * formula in this text-string form can be handed to a program that
 * understands SBML mathematical expressions, or used as part
 * of a translation system.  The libSBML distribution comes with an example
 * program in the @c "examples" subdirectory called @c translateMath that
 * implements an interactive command-line demonstration of translating
 * infix formulas into MathML and vice-versa.
 *
 * The formula strings may contain operators, function calls, symbols, and
 * white space characters.  The allowable white space characters are tab
 * and space.  The following are illustrative examples of formulas
 * expressed in the syntax:
 * 
 * @verbatim
0.10 * k4^2
@endverbatim
 * @verbatim
(vm * s1)/(km + s1)
@endverbatim
 *
 * The following table shows the precedence rules in this syntax.  In the
 * Class column, @em operand implies the construct is an operand, @em
 * prefix implies the operation is applied to the following arguments, @em
 * unary implies there is one argument, and @em binary implies there are
 * two arguments.  The values in the Precedence column show how the order
 * of different types of operation are determined.  For example, the
 * expression <em>a * b + c</em> is evaluated as <em>(a * b) + c</em>
 * because the <code>*</code> operator has higher precedence.  The
 * Associates column shows how the order of similar precedence operations
 * is determined; for example, <em>a - b + c</em> is evaluated as <em>(a -
 * b) + c</em> because the <code>+</code> and <code>-</code> operators are
 * left-associative.  The precedence and associativity rules are taken from
 * the C programming language, except for the symbol <code>^</code>, which
 * is used in C for a different purpose.  (Exponentiation can be invoked
 * using either <code>^</code> or the function @c power.)
 * 
 * @htmlinclude math-precedence-table.html 
 *
 * A program parsing a formula in an SBML model should assume that names
 * appearing in the formula are the identifiers of Species, Parameter,
 * Compartment, FunctionDefinition, Reaction (in SBML Levels&nbsp;2
 * and&nbsp;3), or SpeciesReference (in SBML Level&nbsp;3 only) objects
 * defined in a model.  When a function call is involved, the syntax
 * consists of a function identifier, followed by optional white space,
 * followed by an opening parenthesis, followed by a sequence of zero or
 * more arguments separated by commas (with each comma optionally preceded
 * and/or followed by zero or more white space characters), followed by a
 * closing parenthesis.  There is an almost one-to-one mapping between the
 * list of predefined functions available, and those defined in MathML.
 * All of the MathML functions are recognized; this set is larger than the
 * functions defined in SBML Level&nbsp;1.  In the subset of functions that
 * overlap between MathML and SBML Level&nbsp;1, there exist a few
 * differences.  The following table summarizes the differences between the
 * predefined functions in SBML Level&nbsp;1 and the MathML equivalents in
 * SBML Levels&nbsp;2 and &nbsp;3:
 * 
 * @htmlinclude math-functions.html
 * 
 * @copydetails doc_note_l3_parser_encouraged
 *
 * @see @sbmlfunction{parseL3Formula, String}
 * @see @sbmlfunction{parseL3FormulaWithSettings, String\, L3ParserSettings}
 * @see @sbmlfunction{parseL3FormulaWithModel, String\, Model}
 * @see @sbmlfunction{parseFormula, String}
 * @see @sbmlfunction{formulaToL3StringWithSettings, ASTNode\, L3ParserSettings}
 * @see @sbmlfunction{formulaToL3String, ASTNode}
 * @see @sbmlfunction{formulaToString, ASTNode}
 * @see @sbmlfunction{getDefaultL3ParserSettings,}
 */

/**
 * <!-- ~ ~ ~ ~ ~ Start of common documentation strings ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 * The following text is used as common documentation blocks copied multiple
 * times elsewhere in this file.  The use of @class is a hack needed because
 * Doxygen's @copydetails command has limited functionality.  Symbols
 * beginning with "doc_" are marked as ignored in our Doxygen configuration.
 * ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~  -->
 *
 * @class doc_warning_modifying_structure
 *
 * @warning Explicitly adding, removing or replacing children of an
 * @if conly ASTNode_t structure@else ASTNode object@endif@~ may change the
 * structure of the mathematical formula it represents, and may even render
 * the representation invalid.  Callers need to be careful to use this method
 * in the context of other operations to create complete and correct
 * formulas.  The method @if conly ASTNode_isWellFormedASTNode()@else
 * ASTNode::isWellFormedASTNode()@endif@~ may also be useful for checking the
 * results of node modifications.
 * <!-- ------------------------------------------------------------------- -->
 * @class doc_note_mathml_semantic_annotations_uncommon
 *
 * @note Although SBML permits the use of the MathML
 * <code>&lt;semantics&gt;</code> annotation construct, the truth is that
 * this construct has so far (at this time of this writing, which is early
 * 2014) seen very little use in SBML software.  The full implications of
 * using these annotations are still poorly understood.  If you wish to
 * use this construct, we urge you to discuss possible uses and applications
 * on the SBML discussion lists, particularly <a target="_blank"
 * href="http://sbml.org/Forums">sbml-discuss</a> and/or <a target="_blank"
 * href="http://sbml.org/Forums">sbml-interoperability</a>.
 */

#ifndef ASTNode_h
#define ASTNode_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/math/ASTNodeType.h>

#include <sbml/math/FormulaTokenizer.h>
#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/SyntaxChecker.h>

#include <sbml/common/operationReturnValues.h>
#include <sbml/extension/ASTBasePlugin.h>
//#include <sbml/math/ExtendedMathList.h>

LIBSBML_CPP_NAMESPACE_BEGIN


/**
 * @typedef ASTNodePredicate
 * @brief Function signature for use with
 * @if conly ASTNode_fillListOfNodes() @else ASTNode::fillListOfNodes() @endif
 * and @if conly ASTNode_getListOfNodes() @else ASTNode::getListOfNodes() @endif.
 *
 * A pointer to a function that takes an ASTNode and returns @if conly @c 1
 * (true) or @c 0 (false) @else @c true (nonzero) or @c false (0)@endif.
 *
 * @if conly @see ASTNode_getListOfNodes()@else @see ASTNode::getListOfNodes()@endif
 * @if conly @see ASTNode_fillListOfNodes()@else @see ASTNode::fillListOfNodes()@endif
 */
typedef int (*ASTNodePredicate) (const ASTNode_t *node);


LIBSBML_CPP_NAMESPACE_END

#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

class List;
class ASTBasePlugin;
class ExtendedMathList;

class ASTNode
{
public:

  /**
   * Creates and returns a new ASTNode.
   *
   * Unless the argument @p type is given, the returned node will by default
   * have a type of @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}.  If the type
   * isn't supplied when caling this constructor, the caller should set the
   * node type to something else as soon as possible using @if clike
   * setType()@else ASTNode::setType(int)@endif.
   *
   * @param type an optional @if clike #ASTNodeType_t@else type@endif@~
   * code indicating the type of node to create.
   *
   * @ifnot hasDefaultArgs @htmlinclude warn-default-args-in-docs.html @endif@~
   */
  LIBSBML_EXTERN
  ASTNode (ASTNodeType_t type = AST_UNKNOWN);


  /**
   * Creates a new ASTNode from the given Token.  The resulting ASTNode
   * will contain the same data as the @p token.
   *
   * @param token the token to use as a starting point for creating the
   * ASTNode object.
   */
  LIBSBML_EXTERN
  ASTNode (Token_t *token);

  
  /**
   * Copy constructor; creates a deep copy of the given ASTNode.
   *
   * @param orig the ASTNode to be copied.
   */
  LIBSBML_EXTERN
  ASTNode (const ASTNode& orig);
  

  /**
   * Assignment operator for ASTNode.
   *
   * @param rhs the object whose values are used as the basis of the
   * assignment.
   */
  LIBSBML_EXTERN
  ASTNode& operator=(const ASTNode& rhs);


  /**
   * Destroys this ASTNode, including any child nodes.
   */
  LIBSBML_EXTERN
  virtual ~ASTNode ();


  /**
   * Frees the name of this ASTNode and sets it to @c NULL.
   * 
   * This operation is only applicable to ASTNode objects corresponding to
   * operators, numbers, or @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}.  This 
   * method has no effect on other types of nodes.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_UNEXPECTED_ATTRIBUTE, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int freeName ();


  /**
   * Converts this ASTNode to a canonical form and returns @c true if
   * successful, @c false otherwise.
   *
   * The rules determining the canonical form conversion are as follows:
   *
   * @li If the node type is @sbmlconstant{AST_NAME, ASTNodeType_t}
   * and the node name matches @c "ExponentialE", @c "Pi", @c "True" or @c
   * "False" the node type is converted to the corresponding 
   * <code>AST_CONSTANT_</code><em><span class="placeholder">X</span></em> type.
   *
   * @li If the node type is an @sbmlconstant{AST_FUNCTION, ASTNodeType_t} 
   * and the node name matches an SBML (MathML) function name, logical operator name,
   * or relational operator name, the node is converted to the corresponding
   * <code>AST_FUNCTION_</code><em><span class="placeholder">X</span></em> or
   * <code>AST_LOGICAL_</code><em><span class="placeholder">X</span></em> type.
   *
   * SBML Level&nbsp;1 function names are searched first; thus, for
   * example, canonicalizing @c log will result in a node type of @sbmlconstant{AST_FUNCTION_LN, ASTNodeType_t}.  (See the SBML
   * Level&nbsp;1 Version&nbsp;2 Specification, Appendix C.)
   *
   * Sometimes, canonicalization of a node results in a structural
   * conversion of the node as a result of adding a child.  For example, a
   * node with the SBML Level&nbsp;1 function name @c sqr and a single
   * child node (the argument) will be transformed to a node of type
   * @sbmlconstant{AST_FUNCTION_POWER, ASTNodeType_t} with
   * two children.  The first child will remain unchanged, but the second
   * child will be an ASTNode of type @sbmlconstant{AST_INTEGER, ASTNodeType_t} and a value of 2.  The function names that result
   * in structural changes are: @c log10, @c sqr, and @c sqrt.
   */
  LIBSBML_EXTERN
  bool canonicalize ();


  /**
   * Adds the given node as a child of this ASTNode.
   *
   * Child nodes are added in-order, from left to right.
   *
   * @param disownedChild the ASTNode instance to add.
   * @param inRead @c false by default; may be set to @c true when 
   * reading XML where there may be a lambda function with no
   * bvar arguments.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_warning_modifying_structure
   *
   * @see prependChild(ASTNode* disownedChild)
   * @see replaceChild(unsigned int n, ASTNode* disownedChild, bool delreplaced)
   * @see insertChild(unsigned int n, ASTNode* disownedChild)
   * @see removeChild(unsigned int n)
   * @see isWellFormedASTNode()
   */
  LIBSBML_EXTERN
  int addChild (ASTNode* disownedChild, bool inRead = false);


  /**
   * Adds the given node as a child of this ASTNode.  This method adds
   * child nodes from right to left.
   *
   * @param disownedChild the ASTNode instance to add.
   * Will become a child of the parent node.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_warning_modifying_structure
   *
   * @see addChild(ASTNode* disownedChild)
   * @see replaceChild(unsigned int n, ASTNode* disownedChild, bool delreplaced)
   * @see insertChild(unsigned int n, ASTNode* disownedChild)
   * @see removeChild(unsigned int n)
   */
  LIBSBML_EXTERN
  int prependChild (ASTNode* disownedChild);


  /**
   * Removes the nth child of this ASTNode object.
   *
   * @param n unsigned int the index of the child to remove.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INDEX_EXCEEDS_SIZE, OperationReturnValues_t}
   *
   * @copydetails doc_warning_modifying_structure
   *
   * @see addChild(ASTNode* disownedChild)
   * @see prependChild(ASTNode* disownedChild)
   * @see replaceChild(unsigned int n, ASTNode* disownedChild, bool delreplaced)
   * @see insertChild(unsigned int n, ASTNode* disownedChild)
   */
  LIBSBML_EXTERN
  int removeChild(unsigned int n, bool delremoved = false);


  /**
   * Replaces and optionally deletes the nth child of this ASTNode with the given ASTNode.
   *
   * @param n unsigned int the index of the child to replace.
   * @param disownedChild ASTNode to replace the nth child.
   * Will become a child of the parent node.
   * @param delreplaced Boolean indicating whether to delete the replaced child.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INDEX_EXCEEDS_SIZE, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   *
   * @copydetails doc_warning_modifying_structure
   *
   * @see addChild(ASTNode* disownedChild)
   * @see prependChild(ASTNode* disownedChild)
   * @see insertChild(unsigned int n, ASTNode* disownedChild)
   * @see removeChild(unsigned int n)
   */
  LIBSBML_EXTERN
  int replaceChild(unsigned int n, ASTNode *disownedChild, bool delreplaced=false);


  /**
   * Inserts the given ASTNode at point n in the list of children
   * of this ASTNode.
   *
   * @param n unsigned int the index of the ASTNode being added.
   * @param disownedChild ASTNode to insert as the nth child.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INDEX_EXCEEDS_SIZE, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   *
   * @copydetails doc_warning_modifying_structure
   *
   * @see addChild(ASTNode* disownedChild)
   * @see prependChild(ASTNode* disownedChild)
   * @see replaceChild(unsigned int n, ASTNode* disownedChild, bool delreplaced)
   * @see removeChild(unsigned int n)
   */
  LIBSBML_EXTERN
  int insertChild(unsigned int n, ASTNode *disownedChild);


  /**
   * Creates a recursive copy of this node and all its children.
   *
   * @return a copy of this ASTNode and all its children.  The caller owns
   * the returned ASTNode and is responsible for deleting it.
   */
  LIBSBML_EXTERN
  ASTNode* deepCopy () const;


  /**
   * Returns the child at index n of this node.
   *
   * @param n the index of the child to get.
   *
   * @return the nth child of this ASTNode or @c NULL if this node has no nth
   * child (<code>n &gt; </code>
   * @if clike getNumChildren()@else ASTNode::getNumChildren()@endif@~
   * <code>- 1</code>).
   *
   * @see getNumChildren()
   * @see getLeftChild()
   * @see getRightChild()
   */
  LIBSBML_EXTERN
  ASTNode* getChild (unsigned int n) const;


  /**
   * Returns the left child of this node.
   *
   * @return the left child of this ASTNode.  This is equivalent to calling
   * @if clike getChild()@else ASTNode::getChild(unsigned int)@endif@~
   * with an argument of @c 0.
   *
   * @see getNumChildren()
   * @see getChild(@if java unsigned int@endif)
   * @see getRightChild()
   */
  LIBSBML_EXTERN
  ASTNode* getLeftChild () const;


  /**
   * Returns the right child of this node.
   *
   * @return the right child of this ASTNode, or @c NULL if this node has no
   * right child.  If
   * @if clike getNumChildren()@else ASTNode::getNumChildren()@endif@~
   * <code>&gt; 1</code>, then this is equivalent to:
   * @verbatim
getChild( getNumChildren() - 1 );
@endverbatim
   *
   * @see getNumChildren()
   * @see getLeftChild()
   * @see getChild(@if java unsigned int@endif)
   */
  LIBSBML_EXTERN
  ASTNode* getRightChild () const;


  /**
   * Returns the number of children of this node.
   *
   * @return the number of children of this ASTNode, or 0 is this node has
   * no children.
   */
  LIBSBML_EXTERN
  unsigned int getNumChildren () const;


  /**
   * Adds the given XMLNode as a MathML <code>&lt;semantics&gt;</code>
   * element to this ASTNode.
   *
   * @htmlinclude about-semantic-annotations.html
   *
   * @param disownedAnnotation the annotation to add.
   * Will become a child of the parent node.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_mathml_semantic_annotations_uncommon
   *
   * @see getNumSemanticsAnnotations()
   * @see getSemanticsAnnotation(@if java unsigned int@endif)
   */
  LIBSBML_EXTERN
  int addSemanticsAnnotation (XMLNode* disownedAnnotation);


  /**
   * Returns the number of MathML <code>&lt;semantics&gt;</code> element
   * elements on this node.
   *
   * @htmlinclude about-semantic-annotations.html
   * 
   * @return the number of annotations of this ASTNode.
   *
   * @copydetails doc_note_mathml_semantic_annotations_uncommon
   *
   * @see addSemanticsAnnotation(@if java XMLNode@endif)
   * @see getSemanticsAnnotation(@if java unsigned int@endif)
   */
  LIBSBML_EXTERN
  unsigned int getNumSemanticsAnnotations () const;


  /**
   * Returns the nth MathML <code>&lt;semantics&gt;</code> element on this
   * ASTNode.
   *
   * @htmlinclude about-semantic-annotations.html
   * 
   * @param n the index of the annotation to return.  Callers should
   * use ASTNode::getNumSemanticsAnnotations() to first find out how
   * many annotations there are.
   *
   * @return the nth annotation inside this ASTNode, or @c NULL if this node has
   * no nth annotation (<code>n &gt;</code>
   * @if clike getNumSemanticsAnnotations()@else ASTNode::getNumSemanticsAnnotations()@endif@~
   * <code>- 1</code>).
   *
   * @copydetails doc_note_mathml_semantic_annotations_uncommon
   *
   * @see getNumSemanticsAnnotations()
   * @see addSemanticsAnnotation(@if java XMLNode@endif)
   */
  LIBSBML_EXTERN
  XMLNode* getSemanticsAnnotation (unsigned int n) const;


  /**
   * Returns a list of nodes satisfying a given predicate.
   *
   * This performs a depth-first search of the tree rooted at this ASTNode
   * object, and returns a List of nodes for which the given function
   * <code>predicate(node)</code> returns @c true (nonzero).
   *
   * For portability between different programming languages, the predicate
   * is passed in as a pointer to a function.  @if clike The function
   * definition must have the type 
   * @link ASTNode.h::ASTNodePredicate ASTNodePredicate@endlink, which is defined as
   * @verbatim
int (*ASTNodePredicate) (const ASTNode *node);
@endverbatim
   * where a return value of nonzero represents @c true and zero
   * represents @c false. @endif
   *
   * @param predicate the predicate to use.
   *
   * @return the list of nodes for which the predicate returned @c true
   * (nonzero).  The List returned is owned by the caller and should be
   * deleted after the caller is done using it.  The ASTNode objects in the
   * list; however, are not owned by the caller (as they still belong to
   * the tree itself), and therefore should not be deleted.
   */
  LIBSBML_EXTERN
  List* getListOfNodes (ASTNodePredicate predicate) const;


  /**
   * Returns a list of nodes rooted at a given node and satisfying a given
   * predicate.
   *
   * This method is identical to calling
   * getListOfNodes(ASTNodePredicate predicate) const,
   * except that instead of creating a new List object, it uses the one
   * passed in as argument @p lst.  This method a depth-first search of the
   * tree rooted at this ASTNode object, and adds to the list @p lst the
   * nodes for which the given function <code>predicate(node)</code> returns
   * @c true (nonzero).
   *
   * For portability between different programming languages, the predicate
   * is passed in as a pointer to a function.  The function definition must
   * have the type @link ASTNode.h::ASTNodePredicate ASTNodePredicate
   *@endlink, which is defined as
   * @verbatim
int (*ASTNodePredicate) (const ASTNode_t *node);
@endverbatim
   * where a return value of nonzero represents @c true and zero
   * represents @c false.
   *
   * @param predicate the predicate to use.
   *
   * @param lst the List to which ASTNode objects should be added.
   *
   * @see getListOfNodes(@if java ASTNodePredicate@endif)
   */
  LIBSBML_EXTERN
  void fillListOfNodes (ASTNodePredicate predicate, List* lst) const;


  /**
   * Returns the value of this node as a single character.
   *
   * This function should be called only when
   * @if clike getType()@else ASTNode::getType()@endif@~ returns
   * @sbmlconstant{AST_PLUS, ASTNodeType_t},
   * @sbmlconstant{AST_MINUS, ASTNodeType_t},
   * @sbmlconstant{AST_TIMES, ASTNodeType_t},
   * @sbmlconstant{AST_DIVIDE, ASTNodeType_t} or
   * @sbmlconstant{AST_POWER, ASTNodeType_t}.
   *
   * @return the value of this ASTNode as a single character
   */
  LIBSBML_EXTERN
  char getCharacter () const;


  /**
   * Returns the MathML @c id attribute value of this ASTNode.
   *
   * @return the MathML id of this ASTNode.
   *
   * @see isSetId()
   * @see setId(const std::string& id)
   * @see unsetId()
   */
  LIBSBML_EXTERN
  std::string getId () const;


  /**
   * Returns the MathML @c class attribute value of this ASTNode.
   *
   * @return the MathML class of this ASTNode, or an empty string if it does not exist.
   *
   * @see isSetClass()
   * @see @if java setClassName(const std::string& id)@else setClass()@endif@~
   * @see unsetClass()
   */
  LIBSBML_EXTERN
  std::string getClass () const;


  /**
   * Returns the MathML @c style attribute value of this ASTNode.
   *
   * @return the MathML style of this ASTNode, or an empty string if it does not exist.
   *
   * @see isSetStyle()
   * @see setStyle(const std::string& id)
   * @see unsetStyle()
   */
  LIBSBML_EXTERN
  std::string getStyle () const;


  /**
   * Returns the value of this node as an integer.
   *
   * If this node type is @sbmlconstant{AST_RATIONAL, ASTNodeType_t}, this
   * method returns the value of the numerator.
   *
   * @return the value of this ASTNode as a (<code>long</code>) integer if type @sbmlconstant{AST_INTEGER, ASTNodeType_t}; the numerator if type @sbmlconstant{AST_RATIONAL, ASTNodeType_t}, and @c 0 otherwise.
   *
   * @note This function should be called only when
   * @if clike getType()@else ASTNode::getType()@endif@~ returns
   * @sbmlconstant{AST_INTEGER, ASTNodeType_t} or
   * @sbmlconstant{AST_RATIONAL, ASTNodeType_t}.
   * It will return @c 0 if the node type is @em not one of these, but since
   * @c 0 may be a valid value for integer, it is important to be sure that
   * the node type is one of the expected types in order to understand if
   * @c 0 is the actual value.
   *
   * @see getNumerator()
   * @see getDenominator()
   */
  LIBSBML_EXTERN
  long getInteger () const;


  /**
   * Returns the value of this node as a string.
   *
   * This function may be called on nodes that (1) are not operators, i.e.,
   * nodes for which @if clike isOperator()@else
   * ASTNode::isOperator()@endif@~ returns @c false, and (2) are not numbers,
   * i.e., @if clike isNumber()@else ASTNode::isNumber()@endif@~ returns
   * @c NULL.
   *
   * @return the value of this ASTNode as a string, or @c NULL if it is
   * a node that does not have a name equivalent (e.g., if it is a number).
   */
  LIBSBML_EXTERN
  const char* getName () const;


  /**
   * Returns the value of this operator node as a string.
   *
   * This function may be called on nodes that are operators, i.e., nodes for
   * which @if clike isOperator()@else ASTNode::isOperator()@endif@~ returns
   * @c true.
   *
   * @return the name of this operator ASTNode as a string (or @c NULL if not
   * an operator).
   */
  LIBSBML_EXTERN
  const char* getOperatorName () const;


  /**
   * Returns the value of the numerator of this node if of
   * type @sbmlconstant{AST_RATIONAL, ASTNodeType_t}, or the 
   * numerical value of the node if of type 
   * @sbmlconstant{AST_INTEGER, ASTNodeType_t}; @c 0 otherwise.
   *
   * This function should be called only when
   * @if clike getType()@else ASTNode::getType()@endif@~ returns
   * @sbmlconstant{AST_RATIONAL, ASTNodeType_t} or
   * @sbmlconstant{AST_INTEGER, ASTNodeType_t}.
   * It will return @c 0 if the node type is another type, but since @c 0 may
   * be a valid value for the denominator of a rational number or of an integer, it is
   * important to be sure that the node type is the correct type in order to
   * correctly interpret the returned value.
   *
   * @return the value of the numerator of this ASTNode if
   * @sbmlconstant{AST_RATIONAL, ASTNodeType_t}, the value if 
   * @sbmlconstant{AST_INTEGER, ASTNodeType_t}, or @c 0 otherwise.
   *
   * @see getDenominator()
   * @see getInteger()
   */
  LIBSBML_EXTERN
  long getNumerator () const;


  /**
   * Returns the value of the denominator of this node.
   *
   * @return the value of the denominator of this ASTNode, or @c 1 (true) if
   * this node is not of type @sbmlconstant{AST_RATIONAL, ASTNodeType_t}.
   *
   * @note This function should be called only when
   * @if clike getType()@else ASTNode::getType()@endif@~ returns
   * @sbmlconstant{AST_RATIONAL, ASTNodeType_t}.
   * It will return @c 1 if the node type is another type, but since @c 1 may
   * be a valid value for the denominator of a rational number, it is
   * important to be sure that the node type is the correct type in order to
   * correctly interpret the returned value.
   *
   * @see getNumerator()
   */
  LIBSBML_EXTERN
  long getDenominator () const;


  /**
   * Returns the real-numbered value of this node.
   *
   * This function performs the necessary arithmetic if the node type is
   * @sbmlconstant{AST_REAL_E, ASTNodeType_t} (<em>mantissa *
   * 10<sup>exponent</sup></em>) or
   * @sbmlconstant{AST_RATIONAL, ASTNodeType_t}
   * (<em>numerator / denominator</em>).
   *
   * @return the value of this ASTNode as a real (double), or @c 0
   * if this is not a node that holds a number.
   *
   * @note This function should be called only when this ASTNode has a
   * numerical value type.  It will return @c 0 if the node type is another
   * type, but since @c 0 may be a valid value, it is important to be sure
   * that the node type is the correct type in order to correctly interpret
   * the returned value.
   */
  LIBSBML_EXTERN
  double getReal () const;


  /**
   * Returns the mantissa value of this node.
   *
   * If @if clike getType()@else ASTNode::getType()@endif@~ returns
   * @sbmlconstant{AST_REAL, ASTNodeType_t}, this method is
   * identical to ASTNode::getReal().
   *
   * @return the value of the mantissa of this ASTNode, or @c 0 if this
   * node is not a type that has a real-numbered value.
   *
   * @note This function should be called only when
   * @if clike getType()@else ASTNode::getType()@endif@~ returns
   * @sbmlconstant{AST_REAL_E, ASTNodeType_t},
   * @sbmlconstant{AST_REAL, ASTNodeType_t} or
   * @sbmlconstant{AST_NAME_AVOGADRO, ASTNodeType_t}.  It
   * will return @c 0 if the node type is another type, but since @c 0 may be
   * a valid value, it is important to be sure that the node type is the
   * correct type in order to correctly interpret the returned value.
   *
   * @see getExponent()
   */
LIBSBML_EXTERN
  double getMantissa () const;


  /**
   * Returns the exponent value of this ASTNode.
   *
   * @return the value of the exponent of this ASTNode, or @c 0 if this
   * is not a type of node that has an exponent.
   *
   * @note This function should be called only when
   * @if clike getType()@else ASTNode::getType()@endif@~
   * returns @sbmlconstant{AST_REAL_E, ASTNodeType_t}.
   * It will return @c 0 if the node type is another type, but since @c 0 may
   * be a valid value, it is important to be sure that the node type is the
   * correct type in order to correctly interpret the returned value.
   *
   * @see getMantissa()
   */
  LIBSBML_EXTERN
  long getExponent () const;


  /**
   * Returns the numerical value of this ASTNode.
   *
   * @return the numerical value of this ASTNode, or @c NaN if this
   * is not a type of node that has a numerical value.
   *
   * @note This function will return a numerical value (as a double) for 
   * any ASTNode_t that represents a number, a constant such as 
   * @sbmlconstant{AST_CONSTANT_PI, ASTNodeType_t}, 
   * @sbmlconstant{AST_CONSTANT_E, ASTNodeType_t}, or 
   * @sbmlconstant{AST_NAME_AVOGADRO, ASTNodeType_t}, or 
   * @c 1 for nodes of type 
   * @sbmlconstant{AST_CONSTANT_TRUE, ASTNodeType_t} and @c 0 for nodes of type
   * @sbmlconstant{AST_CONSTANT_FALSE, ASTNodeType_t}. It does not evaluate
   * the node in any way so, for example, it will not return the value of 
   * a named ASTNode_t or attempt to evaluate a function. 
   * This includes a node representing @c time i.e. nodes
   * of type @sbmlconstant{AST_NAME_TIME, ASTNodeType_t}.
   */
  LIBSBML_EXTERN
  double getValue () const;

  /**
   * Returns the precedence of this node in the infix math syntax of SBML
   * Level&nbsp;1.  For more information about the infix syntax, see the
   * discussion about <a href="#math-convert">text string formulas</a> at
   * the top of the documentation for ASTNode.
   * 
   * @return an integer indicating the precedence of this ASTNode
   */
  LIBSBML_EXTERN
  int getPrecedence () const;


  /**
   * Returns the type of this ASTNode.
   *
   * The value returned is one of the Core AST type codes such as
   * @sbmlconstant{AST_LAMBDA, ASTNodeType_t},
   * @sbmlconstant{AST_PLUS, ASTNodeType_t}, etc.
   *
   * @return the type of this ASTNode.
   */
  LIBSBML_EXTERN
  ASTNodeType_t getType () const;


  /**
   * Returns the units of this ASTNode.  
   *
   * @htmlinclude about-sbml-units-attrib.html
   * 
   * @return the units of this ASTNode.
   *
   * @note The <code>sbml:units</code> attribute is only available in SBML
   * Level&nbsp;3.  It may not be used in Levels 1&ndash;2 of SBML.
   *
   * @see @sbmlfunction{parseL3Formula, String}
   */
  LIBSBML_EXTERN
  std::string getUnits () const;


  /**
   * Returns @c true (nonzero) if this node is the special 
   * symbol @c avogadro.  The predicate returns @c false (zero) otherwise.
   * 
   * SBML Level&nbsp;3 introduced a predefined MathML <code>&lt;csymbol&gt;</code>
   * for the value of Avogadro's constant.  LibSBML stores this internally as
   * a node of type @sbmlconstant{AST_NAME_AVOGADRO, ASTNodeType_t}.
   * This method returns @c true if this node has that type.
   *
   * @return @c true if this ASTNode is the special symbol avogadro,
   * @c false otherwise.
   *
   * @see @sbmlfunction{parseL3Formula, String}
   */
  LIBSBML_EXTERN
  bool isAvogadro () const;


  /**
   * Returns @c true if this node has a Boolean type.
   *
   * The ASTNode objects that have Boolean types are the logical operators,
   * relational operators, and the constants @c true or @c false.
   *
   * @return @c true if this ASTNode has a Boolean type, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isBoolean () const;


  /**
   * Returns @c true (nonzero) if this node returns a Boolean type
   * or @c false (zero) otherwise.
   *
   * This function looks at the whole ASTNode rather than just the top 
   * level of the ASTNode. Thus it will consider return values from
   * piecewise statements.  In addition, if this ASTNode uses a function
   * call, the return value of the functionDefinition will be determined.
   * Note that this is only possible where the ASTNode can trace its parent
   * Model, that is, the ASTNode must represent the <code>&lt;math&gt;</code> element of some
   * SBML object that has already been added to an instance of an SBMLDocument.
   * If this is not the case, this function will return @c false unless
   * isBoolean() returns @c true.
   *
   * @see isBoolean()
   *
   * @return @c true if this ASTNode returns a Boolean, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool returnsBoolean (const Model* model=NULL) const;


  /**
   * Returns @c true (nonzero) if this node represents a MathML
   * constant (e.g., @c true, @c Pi).
   * 
   * @return @c true if this ASTNode is a MathML constant, @c false otherwise.
   * 
   * @note this function will also return @c true for nodes of type
   * @sbmlconstant{AST_NAME_AVOGADRO, ASTNodeType_t} in SBML Level&nbsp;3.
   */
  LIBSBML_EXTERN
  bool isConstant () const;



  /**
  * Returns @c true (nonzero) if this node represents a MathML
  * ci element representing a value not a function (e.g., @c true, @c Pi).
  *
  * @return @c true if this ASTNode is a MathML ci element, @c false otherwise.
  */
  LIBSBML_EXTERN
  bool isCiNumber() const;


  /**
  * Returns @c true (nonzero) if this node represents a MathML
  * constant with numeric value (e.g., @c Pi).
  *
  * @return @c true if this ASTNode is a MathML constant, @c false otherwise.
  *
  * @note this function will also return @c true for 
  * @sbmlconstant{AST_NAME_AVOGADRO, ASTNodeType_t} in SBML Level&nbsp;3.
  */

  LIBSBML_EXTERN
  bool isConstantNumber() const;

  /**
   * Returns @c true (nonzero) if this node represents a MathML
   * csymbol representing a function.
   *
   * @return @c true if this ASTNode is a MathML csymbol function, @c false otherwise.
   */

  LIBSBML_EXTERN
  bool isCSymbolFunction() const;

  /**
   * Returns @c true if this node represents a function.
   *
   * The three types of functions in SBML are MathML functions (e.g.,
   * <code>abs()</code>), SBML Level&nbsp;1 functions (in the SBML
   * Level&nbsp;1 math syntax), and user-defined functions (using
   * FunctionDefinition in SBML Level&nbsp;2 and&nbsp;3).
   *
   * @return @c true if this ASTNode is a function, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isFunction () const;


  /**
   * Returns @c true (nonzero) if this node represents
   * the special IEEE 754 value infinity, @c false (zero) otherwise.
   *
   * @return @c true if this ASTNode is the special IEEE 754 value infinity,
   * @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isInfinity () const;


  /**
   * Returns @c true (nonzero) if this node contains an
   * integer value, @c false (zero) otherwise.
   *
   * @return @c true if this ASTNode is of type @sbmlconstant{AST_INTEGER, ASTNodeType_t}, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isInteger () const;


  /**
   * Returns @c true (nonzero) if this node is a MathML
   * <code>&lt;lambda&gt;</code>, @c false (zero) otherwise.
   * 
   * @return @c true if this ASTNode is of type @sbmlconstant{AST_LAMBDA, ASTNodeType_t}, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isLambda () const;


  /**
   * Returns @c true (nonzero) if this node represents a 
   * @c log10 function, @c false (zero) otherwise. 
   *
   * More precisely, this predicate returns @c true if the node type is 
   * @sbmlconstant{AST_FUNCTION_LOG, ASTNodeType_t} with two
   * children, the first of which is an @sbmlconstant{AST_INTEGER, ASTNodeType_t} equal to 10.
   * 
   * @return @c true if the given ASTNode represents a log10() function,
   * @c false otherwise.
   *
   * @see @sbmlfunction{parseL3Formula, String}
   */
  LIBSBML_EXTERN
  bool isLog10 () const;


  /**
   * Returns @c true (nonzero) if this node is a MathML
   * logical operator.
   *
   * The possible MathML logical operators in SBML core are @c and, @c or, @c not,
   * @c xor, and (as of SBML Level&nbsp;3 Version&nbsp;2) @c implies.  If
   * the node represents a logical operator defined in a Level&nbsp;3 package,
   * it will also return @c true.
   *
   * @return @c true if this ASTNode is a MathML logical operator, @c false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isLogical () const;


  /**
   * Returns @c true if this node is a user-defined variable name
   * or the symbols for time or Avogadro's constant.
   *
   * SBML Levels&nbsp;2 and&nbsp;3 provides <code>&lt;csymbol&gt;</code>
   * definitions for "time" and "avogadro", which can be used to represent
   * simulation time and Avogadro's constant in MathML.  Note that this
   * method does @em not return @c true for the other <code>csymbol</code>
   * values defined by SBML, "delay", because the "delay" is a function
   * and not a constant or variable.  Similarly, this function returns
   * @c false for the csymbol functions added by the 'Distributions' package.
   *
   * @return @c true if this ASTNode is a user-defined variable name in SBML
   * or the special symbols for time or Avogadro's constant. It returns
   * @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isName () const;


  /**
   * Returns @c true (nonzero) if this node represents the
   * special IEEE 754 value "not a number" (NaN), @c false (zero)
   * otherwise.
   * 
   * @return @c true if this ASTNode is the special IEEE 754 NaN, @c false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isNaN () const;


  /**
   * Returns @c true (nonzero) if this node represents the
   * special IEEE 754 value "negative infinity", @c false (zero) otherwise.
   * 
   * @return @c true if this ASTNode is the special IEEE 754 value negative
   * infinity, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isNegInfinity () const;


  /**
   * Returns @c true (nonzero) if this node contains a number,
   * @c false (zero) otherwise.  This is functionally equivalent to the
   * following code:
   * @verbatim
 isInteger() || isReal()
 @endverbatim
   * 
   * @return @c true if this ASTNode is a number, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isNumber () const;


  /**
   * Returns @c true if this node is a mathematical
   * operator.
   *
   * The possible mathematical operators in the MathML syntax supported by
   * SBML are <code>+</code>, <code>-</code>, <code>*</code>, <code>/</code>
   * and <code>^</code> (power).
   *
   * @return @c true if this ASTNode is an operator, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isOperator () const;


  /**
   * Returns @c true (nonzero) if this node is the MathML
   * <code>&lt;piecewise&gt;</code> construct.
   * 
   * @return @c true if this ASTNode is a MathML @c piecewise function, 
   * @c false (zero) otherwise.
   */
  LIBSBML_EXTERN
  bool isPiecewise () const;


  /**
   * Returns @c true (nonzero) if this node represents a rational
   * number.
   * 
   * @return @c true if this ASTNode is of type 
   * @sbmlconstant{AST_RATIONAL, ASTNodeType_t}, @c false (zero) otherwise.
   */
  LIBSBML_EXTERN
  bool isRational () const;


  /**
   * Returns @c true (nonzero) if this node can represent a
   * real number, @c false (zero) otherwise.
   *
   * More precisely, this node must be of one of the following types: @sbmlconstant{AST_REAL, ASTNodeType_t}, @sbmlconstant{AST_REAL_E, ASTNodeType_t} or @sbmlconstant{AST_RATIONAL, ASTNodeType_t}.
   *
   * @return @c true if the value of this ASTNode can represented as a real
   * number, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isReal () const;


  /**
   * Returns @c true if this node is a MathML
   * relational operator.
   *
   * The MathML relational operators are <code>==</code>, <code>&gt;=</code>,
   * <code>&gt;</code>, <code>&lt;</code>, and <code>!=</code>.
   *
   * @return @c true if this ASTNode is a MathML relational operator, @c
   * false otherwise
   */
  LIBSBML_EXTERN
  bool isRelational () const;


  /**
   * Returns @c true (nonzero) if this node represents a
   * square root function, @c false (zero) otherwise.
   *
   * More precisely, the node type must be @sbmlconstant{AST_FUNCTION_ROOT, ASTNodeType_t} with two
   * children, the first of which is an @sbmlconstant{AST_INTEGER, ASTNodeType_t} node having value equal to 2.
   * 
   * @return @c true if the given ASTNode represents a sqrt() function,
   * @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isSqrt () const;


  /**
   * Returns @c true (nonzero) if this node is a unary minus
   * operator, @c false (zero) otherwise.
   *
   * A node is defined as a unary minus node if it is of type @sbmlconstant{AST_MINUS, ASTNodeType_t} and has exactly one child.
   *
   * For numbers, unary minus nodes can be "collapsed" by negating the
   * number.  In fact, @sbmlfunction{parseFormula, String} 
   * does this during its parsing process, and @sbmlfunction{parseL3Formula, String} 
   * has a configuration option that allows this behavior to be turned
   * on or off.  However, unary minus nodes for symbols
   * (@sbmlconstant{AST_NAME, ASTNodeType_t}) cannot
   * be "collapsed", so this predicate function is necessary.
   * 
   * @return @c true if this ASTNode is a unary minus, @c false otherwise.
   *
   * @see @sbmlfunction{parseL3Formula, String}
   */
  LIBSBML_EXTERN
  bool isUMinus () const;


  /**
   * Returns @c true (nonzero) if this node is a unary plus
   * operator, @c false (zero) otherwise.  A node is defined as a unary
   * minus node if it is of type @sbmlconstant{AST_MINUS, ASTNodeType_t} and has exactly one child.
   *
   * @return @c true if this ASTNode is a unary plus, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isUPlus () const;


  /**
  * Returns @c true (nonzero) if this node represents a
  * MathML user-defined function.
  *
  * @return @c true if this ASTNode is a user-defined function, @c false otherwise.
  */
  LIBSBML_EXTERN
  bool isUserFunction() const;


  /**
  * Returns @c true if this node is of type @p type
  * and has @p numchildren number of children.  Designed
  * for use in cases where it is useful to discover if the node is
  * a unary not or unary minus, or a times node with no children, etc.
  *
  * @return @c true if this ASTNode is has the specified type and number
  *         of children, @c false otherwise.
  */
  LIBSBML_EXTERN
  int hasTypeAndNumChildren(ASTNodeType_t type, unsigned int numchildren) const;


  /**
   * Returns @c true (nonzero) if this node has an unknown type.
   *
   * "Unknown" nodes have the type @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}.  
   * Nodes with unknown types will not appear in an
   * ASTNode tree returned by libSBML based upon valid SBML input; the only
   * situation in which a node with type @sbmlconstant{AST_UNKNOWN, ASTNodeType_t} 
   * may appear is immediately after having create a
   * new, untyped node using the ASTNode constructor.  Callers creating
   * nodes should endeavor to set the type to a valid node type as soon as
   * possible after creating new nodes.
   * 
   * @return @c true if this ASTNode is of type @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool isUnknown () const;


  /**
   * Returns @c true (nonzero) if this node has a value for the MathML
   * attribute "id".
   *
   * @return @c true if this ASTNode has an attribute id, @c false otherwise.
   *
   * @see isSetClass()
   * @see isSetStyle()
   * @see setId(const std::string& id)
   * @see unsetId()
   */
  LIBSBML_EXTERN
  bool isSetId() const;


  /**
   * Returns @c true (nonzero) if this node has a value for the MathML
   * attribute "class".
   *
   * @return @c true if this ASTNode has an attribute class, @c false otherwise.
   *
   * @see isSetId()
   * @see isSetStyle()
   * @see @if java setClassName(const std::string& id)@else setClass()@endif@~
   * @see unsetClass()
   */
  LIBSBML_EXTERN
  bool isSetClass() const;


  /**
   * Returns @c true (nonzero) if this node has a value for the MathML
   * attribute "style".
   *
   * @return @c true if this ASTNode has an attribute style, @c false otherwise.
   *
   * @see isSetClass()
   * @see isSetId()
   * @see setStyle(const std::string& id)
   * @see unsetStyle()
   */
  LIBSBML_EXTERN
  bool isSetStyle() const;


  /**
   * Returns @c true (nonzero) if this node has the attribute
   * <code>sbml:units</code>.
   *
   * @htmlinclude about-sbml-units-attrib.html
   *
   * @return @c true if this ASTNode has units associated with it, @c false otherwise.
   *
   * @note The <code>sbml:units</code> attribute is only available in SBML
   * Level&nbsp;3.  It may not be used in Levels 1&ndash;2 of SBML.
   *
   * @see hasUnits()
   * @see setUnits(const std::string& units)
   */
  LIBSBML_EXTERN
  bool isSetUnits() const;


  /**
   * Returns @c true (nonzero) if this node or any of its
   * children nodes have the attribute <code>sbml:units</code>.
   *
   * @htmlinclude about-sbml-units-attrib.html
   *
   * @return @c true if this ASTNode or its children has units associated
   * with it, @c false otherwise.
   *
   * @note The <code>sbml:units</code> attribute is only available in SBML
   * Level&nbsp;3.  It may not be used in Levels 1&ndash;2 of SBML.
   *
   * @see isSetUnits()
   * @see setUnits(const std::string& units)
   */
  LIBSBML_EXTERN
  bool hasUnits() const;


  /**
   * Sets the value of this ASTNode to the given character.  If character
   * is one of @c +, @c -, <code>*</code>, <code>/</code> or @c ^, the node
   * type will be set accordingly.  For all other characters, the node type
   * will be set to @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}.
   *
   * @param value the character value to which the node's value should be
   * set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int setCharacter (char value);


  /**
   * Sets the MathML attribute @c id of this ASTNode.
   *
   * @param id @c string representing the identifier.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @see isSetId()
   * @see getId()
   * @see unsetId()
   */
  LIBSBML_EXTERN
  int setId (const std::string& id);


  /**
   * Sets the MathML attribute @c class of this ASTNode to @p className.
   *
   * @param className @c string representing the MathML class for this node.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @if java
   * @note In the API interfaces for languages other than Java, this method
   * is named <code>setClass()</code>, but in Java it is renamed
   * <code>setClassName()</code> to avoid a name collision with Java's
   * standard object method of the same name.
   * @endif@~
   *
   * @see isSetClass()
   * @see getClass()
   * @see unsetClass()
   */
  LIBSBML_EXTERN
  int setClass (const std::string& className);


  /**
   * Sets the MathML attribute @c style of this ASTNode to style.
   *
   * @param style @c string representing the identifier.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @see isSetStyle()
   * @see getStyle()
   * @see unsetStyle()
   */
  LIBSBML_EXTERN
  int setStyle (const std::string& style);


  /**
   * Sets the value of this ASTNode to the given name.
   *
   * As a side effect, this ASTNode object's type will be reset to
   * @sbmlconstant{AST_NAME, ASTNodeType_t} if (and <em>only
   * if</em>) the ASTNode was previously an operator (
   * @if clike isOperator()@else ASTNode::isOperator()@endif@~
   * <code>== true</code>), number (
   * @if clike isNumber()@else ASTNode::isNumber()@endif@~
   * <code>== true</code>), or unknown.
   * This allows names to be set for @sbmlconstant{AST_FUNCTION, ASTNodeType_t} nodes and the like.
   *
   * @param name the string containing the name to which this node's value
   * should be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int setName (const char *name);


  /**
   * Sets the value of this ASTNode to the given integer and sets the node
   * type to @sbmlconstant{AST_INTEGER, ASTNodeType_t}.
   *
   * @param value the integer to which this node's value should be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int setValue (int value);


  /**
   * Sets the value of this ASTNode to the given (@c long) integer and sets
   * the node type to @sbmlconstant{AST_INTEGER, ASTNodeType_t}.
   *
   * @param value the integer to which this node's value should be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int setValue (long value);


  /**
   * Sets the value of this ASTNode to the given rational in two parts: the
   * numerator and denominator.  The node type is set to @sbmlconstant{AST_RATIONAL, ASTNodeType_t}.
   *
   * @param numerator the numerator value of the rational.
   * @param denominator the denominator value of the rational.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int setValue (long numerator, long denominator);


  /**
   * Sets the value of this ASTNode to the given real (@c double) and sets
   * the node type to @sbmlconstant{AST_REAL, ASTNodeType_t}.
   *
   * This is functionally equivalent to:
   * @verbatim
setValue(value, 0);
@endverbatim
   *
   * @param value the @c double format number to which this node's value
   * should be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int setValue (double value);


  /**
   * Sets the value of this ASTNode to the given real (@c double) in two
   * parts: the mantissa and the exponent.  The node type is set to
   * @sbmlconstant{AST_REAL_E, ASTNodeType_t}.
   *
   * @param mantissa the mantissa of this node's real-numbered value.
   * @param exponent the exponent of this node's real-numbered value.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int setValue (double mantissa, long exponent);


  /**
   * Sets the type of this ASTNode to the given type code.
   *
   * @param type the type to which this node should be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   *
   * @note A side-effect of doing this is that any numerical values previously
   * stored in this node are reset to zero.
   */
  LIBSBML_EXTERN
  int setType (ASTNodeType_t type);


  /**
   * Sets the units of this ASTNode to units.
   *
   * The units will be set @em only if this ASTNode object represents a
   * MathML <code>&lt;cn&gt;</code> element, i.e., represents a number.
   * Callers may use
   * @if clike isNumber()@else ASTNode::isNumber()@endif@~
   * to inquire whether the node is of that type.
   *
   * @htmlinclude about-sbml-units-attrib.html
   *
   * @param units @c string representing the unit identifier.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_UNEXPECTED_ATTRIBUTE, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   *
   * @note The <code>sbml:units</code> attribute is only available in SBML
   * Level&nbsp;3.  It may not be used in Levels 1&ndash;2 of SBML.
   *
   * @see isSetUnits()
   * @see hasUnits()
   */
  LIBSBML_EXTERN
  int setUnits (const std::string& units);


  /**
   * Swaps the children of this ASTNode object with the children of the
   * given ASTNode object.
   *
   * @param that the other node whose children should be used to replace
   * <em>this</em> node's children.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int swapChildren (ASTNode *that);


  /**
   * Renames all the SIdRef attributes on this node and any child node
   */
  LIBSBML_EXTERN
  virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
   * Renames all the UnitSIdRef attributes on this node and any child node.
   *
   * The only place UnitSIDRefs appear is in MathML <code>&lt;cn&gt;</code>
   * elements, so the effects of this method are limited to that.
   *
   * @param oldid the old identifier.
   * @param newid the new identifier.
   */
  LIBSBML_EXTERN
  virtual void renameUnitSIdRefs(const std::string& oldid, const std::string& newid);


  /** @cond doxygenLibsbmlInternal */
  /**
   * Replace any nodes of type AST_NAME with the name 'id' from the child 'math' object with the provided ASTNode. 
   *
   */
  LIBSBML_EXTERN
  virtual void replaceIDWithFunction(const std::string& id, const ASTNode* function);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Replaces any 'AST_NAME_TIME' nodes with a node that multiplies time by the given function.
   *
   */
  LIBSBML_EXTERN
  virtual void multiplyTimeBy(const ASTNode* function);
  /** @endcond */


  /**
   * Unsets the units of this ASTNode.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_UNEXPECTED_ATTRIBUTE, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int unsetUnits ();

  /**
   * Unsets the MathML @c id attribute of this ASTNode.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int unsetId ();


  /**
   * Unsets the MathML @c class attribute of this ASTNode.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int unsetClass ();


  /**
   * Unsets the MathML @c style attribute of this ASTNode.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int unsetStyle ();


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the flag indicating that this ASTNode has semantics attached.
   *
   * @htmlinclude about-semantic-annotations.html
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int setSemanticsFlag();
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Unsets the flag indicating that this ASTNode has semantics attached.
   *
   * @htmlinclude about-semantic-annotations.html
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  LIBSBML_EXTERN
  int unsetSemanticsFlag();
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns the flag indicating that this ASTNode has semantics attached.
   *
   * @htmlinclude about-semantic-annotations.html
   *
   * @return @c true if this node has semantics attached, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool getSemanticsFlag() const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the MathML attribute @c definitionURL.
   *
   * @param url the URL value for the @c definitionURL attribute.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @see setDefinitionURL(const std::string& url)
   * @see getDefinitionURL()
   * @see getDefinitionURLString()
   */
  LIBSBML_EXTERN
  int setDefinitionURL(XMLAttributes url);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the MathML attribute @c definitionURL.
   *
   * @param url the URL value for the @c definitionURL attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   *
   * @see setDefinitionURL(XMLAttributes url)
   * @see getDefinitionURL()
   * @see getDefinitionURLString()
   */
  LIBSBML_EXTERN
  int setDefinitionURL(const std::string& url);
  /** @endcond */


  /**
   * Returns the MathML @c definitionURL attribute value.
   *
   * @return the value of the @c definitionURL attribute, in the form of
   * a libSBML XMLAttributes object.
   *
   * @see setDefinitionURL(XMLAttributes url)
   * @see setDefinitionURL(const std::string& url)
   * @see getDefinitionURLString()
   */
  LIBSBML_EXTERN
  XMLAttributes* getDefinitionURL() const;


  /**
   * Replaces occurrences of a given name with a given ASTNode.
   *
   * For example, if the formula in this ASTNode is <code>x + y</code>,
   * and the function is called with @c bvar = @c "x" and @c arg = an ASTNode
   * representing the real value @c 3.  This method would substitute @c 3 for
   * @c x within this ASTNode object, resulting in the formula <code>3 + y</code>.
   *
   * @param bvar a string representing the variable name to be substituted.
   * @param arg an ASTNode representing the name/value/formula to use as
   * a replacement.
   */
  LIBSBML_EXTERN
  void replaceArgument(const std::string& bvar, ASTNode * arg);

  /**
   * Replaces occurrences of each given name with the corresponding ASTNode.
   *
   * For example, if the formula in this ASTNode is <code>x - y</code>,
   * and the function is called with bvars = {"x", "y"} and args = ASTNodes
   * representing objects with names {"y", "x"}, the result would be <code>y - x</code>.
   *
   * @param bvars a vector of strings representing the variable names to be substituted.
   * @param args a vector of ASTNodes representing the name/value/formula to use as
   * a replacement for each variable name
   */
  LIBSBML_EXTERN
  void replaceArguments(const std::vector<std::string>& bvars, std::vector<ASTNode *>& args);


    /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBML object of this node.  Is not recursive, and will not set the parent SBML object of any children of this node.
   *
   * @param sb the parent SBML object of this ASTNode.
   *
   * @see isSetParentSBMLObject()
   * @see getParentSBMLObject()
   * @see unsetParentSBMLObject()
   */
  LIBSBML_EXTERN
  void setParentSBMLObject(SBase * sb);

  /** @endcond */


  /**
   * Returns the parent SBML object.
   * 
   * @return the parent SBML object of this ASTNode.
   *
   * @see isSetParentSBMLObject()
   * @if clike @see setParentSBMLObject()@endif@~
   * @see unsetParentSBMLObject()
   */
  LIBSBML_EXTERN
  SBase * getParentSBMLObject() const;


  /**
   * Unsets the parent SBML object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @see isSetParentSBMLObject()
   * @see getParentSBMLObject()
   * @if clike @see setParentSBMLObject()@endif@~
   */
  LIBSBML_EXTERN
  int unsetParentSBMLObject();


  /**
   * Returns @c true if this node has a value for the parent SBML
   * object.
   *
   * @return @c true if this ASTNode has an parent SBML object set, @c false otherwise.
   *
   * @see getParentSBMLObject()
   * @if clike @see setParentSBMLObject()@endif@~
   * @see unsetParentSBMLObject()
   */
  LIBSBML_EXTERN
  bool isSetParentSBMLObject() const;


  /**
   * Reduces this ASTNode to a binary tree.
   *
   * Example: if this ASTNode is <code>and(x, y, z)</code>, then the
   * formula of the reduced node is <code>and(and(x, y), z)</code>.  The
   * operation replaces the formula stored in the current ASTNode object.
   */
  LIBSBML_EXTERN
  void reduceToBinary();


 /**
  * Sets the user data of this node.
  *
  * The user data can be used by the application developer to attach custom
  * information to the node.  In case of a deep copy, this attribute will
  * passed as it is. The attribute will be never interpreted by this class.
  *
  * @param userData specifies the new user data.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
  *
  * @if clike
  * @see isSetUserData()
  * @see getUserData()
  * @see unsetUserData()
  * @endif
  */
  LIBSBML_EXTERN
  int setUserData(void *userData);


 /**
  * Returns the user data that has been previously set via setUserData().
  *
  * @return the user data of this node, or @c NULL if no user data has been
  * set.
  *
  * @if clike
  * @see isSetUserData()
  * @see setUserData()
  * @see unsetUserData()
  * @endif@~
  */
  LIBSBML_EXTERN
  void *getUserData() const;


 /**
  * Unsets the user data of this node.
  *
  * The user data can be used by the application developer to attach custom
  * information to the node.  In case of a deep copy, this attribute will
  * passed as it is. The attribute will be never interpreted by this class.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
  *
  * @if clike
  * @see setUserData()
  * @see getUserData()
  * @see isSetUserData()
  * @endif@~
  */
  LIBSBML_EXTERN
  int unsetUserData();


 /**
  * Returns @c true if this node has a user data object.
  *
  * @return @c true if this ASTNode has a user data object set, @c false
  * otherwise.
  *
  * @if clike
  * @see setUserData()
  * @see getUserData()
  * @see unsetUserData()
  * @endif@~
  */
  LIBSBML_EXTERN
  bool isSetUserData() const;


 /**
  * Returns @c true or @c false depending on whether this
  * ASTNode is well-formed.
  *
  * @note An ASTNode may be well-formed, with each node and its children
  * having the appropriate number of children for the given type, but may
  * still be invalid in the context of its use within an SBML model.
  *
  * @return @c true if this ASTNode is well-formed, @c false otherwise.
  *
  * @see hasCorrectNumberArguments()
  */
  LIBSBML_EXTERN
  bool isWellFormedASTNode() const;


 /**
  * Returns @c true if this ASTNode has the correct number of children for
  * its type.
  *
  * For example, an ASTNode with type @sbmlconstant{AST_MINUS, ASTNodeType_t}
  * expects 1 or 2 child nodes.
  *
  * @return @c true if this ASTNode has the appropriate number of children
  * for its type, @c false otherwise.
  *
  * @note This function performs a check on the top-level node only.  Child
  * nodes are not checked.
  *
  * @see isWellFormedASTNode()
  */
  LIBSBML_EXTERN
  bool hasCorrectNumberArguments() const;

  /**
   * Returns the MathML @c definitionURL attribute value as a string.
   *
   * @return the value of the @c definitionURL attribute, as a string.
   *
   * @see getDefinitionURL()
   * @see setDefinitionURL(const std::string& url)
   * @see setDefinitionURL(XMLAttributes url)
   */
  LIBSBML_EXTERN
  std::string getDefinitionURLString() const;


  /** @cond doxygenLibsbmlInternal */

  LIBSBML_EXTERN
  bool representsBvar() const;


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
    
  LIBSBML_EXTERN
  bool isBvar() const;
  
  LIBSBML_EXTERN
  void setBvar();

  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns @c true if this ASTNode has uses math constructs introduced in L3V2.
   *
   * @return @c true if this ASTNode uses math constructs introduced in
   * L3V2, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool usesL3V2MathConstructs() const;

  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns @c true if this ASTNode has uses math rateOf constructs introduced in L3V2.
   *
   * @return @c true if this ASTNode uses math constructs introduced in
   * L3V2, @c false otherwise.
   */
  LIBSBML_EXTERN
  bool usesRateOf() const;

  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
  * Predicate returning @c true (nonzero) if this node is a MathML
  * qualifier (i.e., @c bvar, @c degree, @c base, @c piece, @c otherwise),
  * @c false (zero) otherwise.
  *
  * @return @c true if this ASTNode is a MathML qualifier.
  */
  LIBSBML_EXTERN
  virtual bool isQualifier() const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
  * Predicate returning @c true (nonzero) if this node is a MathML
  * semantics node, @c false (zero) otherwise.
  *
  * @return @c true if this ASTNode is a MathML semantics node.
  */
  LIBSBML_EXTERN
  virtual bool isSemantics() const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  LIBSBML_EXTERN
  unsigned int getNumBvars() const;
  /** @endcond */

  // ------------------------------------------------------------------
  //
  // public functions for EXTENSION
  //
  // ------------------------------------------------------------------

  /** @cond doxygenLibsbmlInternal */

  LIBSBML_EXTERN
  void addPlugin(ASTBasePlugin* plugin);

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */

  LIBSBML_EXTERN
  void loadASTPlugins(const SBMLNamespaces * sbmlns);

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */

  LIBSBML_EXTERN
    void loadASTPlugin(const std::string& pkgName);

  /** @endcond */


  /**
   * Returns a plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension with the given @p sbmlns.
   *
   * @param sbmlns the namespace of the plugin to return.
   *
   * @return the plug-in object (the libSBML extension interface) of
   * a package extension with the given package name or URI, or @c NULL
   * if none exist.
   */
  LIBSBML_EXTERN
    ASTBasePlugin * getASTPlugin(const SBMLNamespaces * sbmlns);

  /**
   * Returns a plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension for the package that defines the given @p type.
   *
   * @param type the @if clike #ASTNodeType_t@else type@endif@~ that is defined by the given plugin.
   *
   * @return the plug-in object (the libSBML extension interface) of
   * a package extension that defines the given @p type, or @c NULL
   * if none exist.
   */
  LIBSBML_EXTERN
    ASTBasePlugin * getASTPlugin(ASTNodeType_t type);

  /**
   * Returns a plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension for the package with the given constraints.
   *
   * @param name the type or csymbol defined by the returned plugin.
   * @param isCsymbol Boolean indicator of whether the @p name is a csymbol
   * (if @c true) or type (if @c false).
   * @param strCmpIsCaseSensitive whether to search for the matching type
   * or csymbol in case-sensitve manner (if @c true) or case-insensitive
   * manner (if @c false).
   *
   * @return the plug-in object (the libSBML extension interface) of
   * a package extension that defines the given @p name, or @c NULL
   * if none exist.
   */
  LIBSBML_EXTERN
    ASTBasePlugin * getASTPlugin(const std::string& name, bool isCsymbol = false, bool strCmpIsCaseSensitive = false);

  /**
   * Returns a plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension with the given @p sbmlns.
   *
   * @param sbmlns the namespace of the plugin to return.
   *
   * @return the plug-in object (the libSBML extension interface) of
   * a package extension with the given package name or URI, or @c NULL
   * if none exist.
   */
  LIBSBML_EXTERN
    const ASTBasePlugin * getASTPlugin(const SBMLNamespaces * sbmlns) const;

  /**
   * Returns a plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension for the package that defines the given @p type.
   *
   * @param type the @if clike #ASTNodeType_t@else type@endif@~ that is defined by the given plugin.
   *
   * @return the plug-in object (the libSBML extension interface) of
   * a package extension that defines the given @p type, or @c NULL
   * if none exist.
   */
  LIBSBML_EXTERN
    const ASTBasePlugin * getASTPlugin(ASTNodeType_t type) const;

  /**
   * Returns a plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension for the package with the given constraints.
   *
   * @param name the type or csymbol defined by the returned plugin.
   * @param isCsymbol Boolean indicator of whether the @p name is a csymbol
   * (if @c true) or type (if @c false).
   * @param strCmpIsCaseSensitive whether to search for the matching type
   * or csymbol in case-sensitve manner (if @c true) or case-insensitive
   * manner (if @c false).
   *
   * @return the plug-in object (the libSBML extension interface) of
   * a package extension that defines the given @p name, or @c NULL
   * if none exist.
   */
  LIBSBML_EXTERN
    const ASTBasePlugin * getASTPlugin(const std::string& name, bool isCsymbol = false, bool strCmpIsCaseSensitive = false) const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns a plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension with the given package name or URI.
   *
   * @param package the name or URI of the package.
   *
   * @return the plug-in object (the libSBML extension interface) of
   * a package extension with the given package name or URI.
   */
  LIBSBML_EXTERN
  ASTBasePlugin* getPlugin(const std::string& package);

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns a plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension with the given package name or URI.
   *
   * @param package the name or URI of the package.
   *
   * @return the plug-in object (the libSBML extension interface) of a
   * package extension with the given package name or URI.
   */
  LIBSBML_EXTERN
  const ASTBasePlugin* getPlugin(const std::string& package) const;

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns the nth plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension.
   *
   * @param n the index of the plug-in to return.
   *
   * @return the plug-in object (the libSBML extension interface) of
   * a package extension with the given package name or URI.
   */
  LIBSBML_EXTERN
  ASTBasePlugin* getPlugin(unsigned int n);

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns the nth plug-in object (extension interface) for an SBML Level&nbsp;3
   * package extension.
   *
   * @param n the index of the plug-in to return.
   *
   * @return the plug-in object (the libSBML extension interface) of a
   * package extension with the given package name or URI.
   */
  LIBSBML_EXTERN
  const ASTBasePlugin* getPlugin(unsigned int n) const;

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Returns the number of plug-in objects (extenstion interfaces) for SBML
   * Level&nbsp;3 package extensions known.
   *
   * @return the number of plug-in objects (extension interfaces) of
   * package extensions known by this instance of libSBML.
   */
  LIBSBML_EXTERN
  unsigned int getNumPlugins() const;

  /** @endcond */

  /* returns true if astnodes are exactly the same
  *  so 'a+4' == 'a+4'   
  * but 'a+4' != '4+a'
  */
  LIBSBML_EXTERN
  bool exactlyEqual(const ASTNode& rhs);

  /* calls
    refactorNumbers();
    encompassUnaryMinus();
    createNonBinaryTree();
    reorderArguments();
  */
  LIBSBML_EXTERN
  void refactor();

  /*
  * a decomposed ast is one where if the top level func is *or /
  * the arguments are not sums
  * (a + B) * c becomes ac + Bc
  * (5 + 3)/(a-4) becomes 8/(a-4)
  * (a + 4)/4 becomes 1 + a/4
  */
  LIBSBML_EXTERN
  void decompose();

  /*
  * Returns an ASTNode representing the derivative w.r.t variable
  * e.g. Node represents 2*x^2 
  *      variable = "x"
  * returns Node representing 4 * x 
  * since d(2*x^2)/dx = 4*x
  */
  LIBSBML_EXTERN
  ASTNode* derivative(const std::string& variable);

  LIBSBML_EXTERN
  void printMath(unsigned int level = 0);

  /** @cond doxygenLibsbmlInternal */
  LIBSBML_EXTERN
  XMLNamespaces* getDeclaredNamespaces() const;

  LIBSBML_EXTERN
  void setDeclaredNamespaces(const XMLNamespaces* xmlns);

  LIBSBML_EXTERN
  void unsetDeclaredNamespaces();
  /** @endcond */


protected:

  friend class SBMLRateRuleConverter;

//  void printMath(unsigned int level = 0);

  /* change all numbers to real*/
  void refactorNumbers();

  /*
  * simplify the node based on math i.e 1 * x becomes x
  * see inline
  */
  void simplify();

  /* for plus or times order arguments so we have number + names + functions
  * 3.1 +b + 2 becomes 5.1 + b
  * 2 * 5 becomes 10
  * sin(2+3) + 3.1 + b becomes 3.1 +b + sin(5)
  */
  bool reorderArguments(unsigned int level=0 );

  /* remove any instances of unary minus
  * Level 0: -
  * Level 1: 2
  * becomes
  * Level 0: -2
  *
  * Level 0: -
  * Level 1: 2 * a
  * Level 2: 2
  * Level 2: a
  * becomes
  * Level 0: -2 * a
  * Level 1: -2
  * Level 1: a
  *
  * Level 0: -
  * Level 1: b / a
  * Level 2: b
  * Level 2: a
  * becomes
  * Level 0: (-1*b)/a
  * Level 1: -1*b
  * Level 2: -1
  * Level 2: b
  * Level 1: a
  */
  void encompassUnaryMinus();
    

  /* create AST the is non binary
    * Binary each node has 2 children 
    * Level 0: a + b + (c + s)
    * Level 1: a + b
    * Level 2: a
    * Level 2: b
    * Level 1: c + s
    * Level 2: c
    * Level 2: s
    *
    * Non binary Node at Level 0 has 4 children
    * Level 0: a + b + c + s
    * Level 1: a
    * Level 1: b
    * Level 1: c
    * Level 1: s
  */
  void createNonBinaryTree();

  /*
  * change a root node to power ie root(2, x) becomes x^0.5
  */
  void convertRootToPower();

  /*
  * returns derivativeof particular function
  * i.e. derivativePlus gets a function A + B and returns d(A+B)/dx
  */
  ASTNode* derivativePlus(const std::string& variable);
  ASTNode* derivativeMinus(const std::string& variable);
  ASTNode* derivativeTimes(const std::string& variable);
  ASTNode* derivativeDivide(const std::string& variable);
  ASTNode* derivativePower(const std::string& variable);
  ASTNode* derivativeLog(const std::string& variable);
  ASTNode* derivativeLn(const std::string& variable);
  ASTNode* derivativeExp(const std::string& variable);


  /*
  * produce three vectors of the child index of the ASTNodes
  * numbers : any nodes representing just a number
  * names : any nodes representing a variable i.e.a node of type AST_NAME (in alphabetical order)
  * others : any nodes that are not numbers/names - usually functions
  */
  void createVectorOfChildTypes(std::vector<unsigned int>& numbers,
    std::vector<unsigned int>& names,
    std::vector<unsigned int>& others);


  /* combine numbers:
  * return an AST representing combined no 
  */
  ASTNode* combineNumbers(std::vector<unsigned int>& numbers);

  //========================================================


  /** @cond doxygenLibsbmlInternal */

  LIBSBML_EXTERN
  bool containsVariable(const std::string& id) const;

  LIBSBML_EXTERN
  unsigned int getNumVariablesWithUndeclaredUnits(Model * m = NULL) const;

  friend class UnitFormulaFormatter;
  /**
   * Internal helper function for canonicalize().
   */

  bool canonicalizeConstant   ();
  bool canonicalizeFunction   ();
  bool canonicalizeFunctionL1 ();
  bool canonicalizeLogical    ();
  bool canonicalizeRelational ();


  ASTNodeType_t mType;

  char   mChar;
  char*  mName;
  long   mInteger;
  double mReal;
  long mDenominator;
  long mExponent;

  XMLAttributes* mDefinitionURL;
  bool hasSemantics;

  List *mChildren;

  List *mSemanticsAnnotations;

  SBase *mParentSBMLObject;

  std::string mUnits;

  // additional MathML attributes
  std::string mId;
  std::string mClass;
  std::string mStyle;

  bool mIsBvar;
  void *mUserData;
  std::string mPackageName;


  XMLNamespaces* mNamespaces;
  
  friend class MathMLFormatter;
  friend class MathMLHandler;

  //----------------------------------------------------------------------
  //
  // Additional data members for Extension
  //
  //----------------------------------------------------------------------

  //
  // ASTBasePlugin derived classes will be stored in mPlugins.
  std::vector<ASTBasePlugin*> mPlugins;


  /** @endcond */

private:
  void clearPlugins();
};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


/**
 * Creates a new ASTNode_t structure and returns a pointer to it.
 *
 * The returned node will have a type of @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}.  The caller should
 * be set the node type to something else as soon as possible using
 * ASTNode_setType().
 *
 * @returns a pointer to the fresh ASTNode_t structure.
 *
 * @see ASTNode_createWithType()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_create (void);


/**
 * Creates a new ASTNode_t structure and sets its type.
 *
 * @param type the type of node to create.
 *
 * @returns a pointer to the fresh ASTNode_t structure.
 *
 * @see ASTNode_create()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_createWithType (ASTNodeType_t type);


/**
 * Creates a new ASTNode_t structure from the given Token_t data and returns
 * a pointer to it.
 *
 * The returned ASTNode_t structure will contain the same data as the Token_t
 * structure.  The Token_t structure is used to store a token returned by
 * FormulaTokenizer_nextToken().  It contains a union whose members can store
 * different types of tokens, such as numbers and symbols.
 *
 * @param token the Token_t structure to use.
 *
 * @returns a pointer to the new ASTNode_t structure.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_createFromToken (Token_t *token);


/**
 * Frees the given ASTNode_t structure, including any child nodes.
 *
 * @param node the node to be freed.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
void
ASTNode_free (ASTNode_t *node);


/**
 * Frees the name field of a given node and sets it to null.
 *
 * This operation is only applicable to ASTNode_t structures corresponding to
 * operators, numbers, or @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}.  This
 * method will have no effect on other types of nodes.
 *
 * @param node the node whose name field should be freed.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_UNEXPECTED_ATTRIBUTE, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_freeName (ASTNode_t *node);


/**
 * Converts a given node to a canonical form and returns @c 1 (true) if successful,
 * @c 0 (false) otherwise.
 *
 * The rules determining the canonical form conversion are as follows:
 *
 * @li If the node type is @sbmlconstant{AST_NAME, ASTNodeType_t}
 * and the node name matches @c "ExponentialE", @c "Pi", @c "True" or
 * @c "False" the node type is converted to the corresponding
 * <code>AST_CONSTANT_</code><em><span class="placeholder">X</span></em> type.
 * @li If the node type is an @sbmlconstant{AST_FUNCTION, ASTNodeType_t} and
 * the node name matches an SBML (MathML) function name, logical operator
 * name, or relational operator name, the node is converted to the
 * corresponding <code>AST_FUNCTION_</code><em><span
 * class="placeholder">X</span></em> or <code>AST_LOGICAL_</code><em><span
 * class="placeholder">X</span></em> type.
 *
 * SBML Level&nbsp;1 function names are searched first; thus, for example,
 * canonicalizing @c log will result in a node type of
 * @sbmlconstant{AST_FUNCTION_LN, ASTNodeType_t}.  (See the SBML Level&nbsp;1
 * Version&nbsp;2 Specification, Appendix C.)
 *
 * Sometimes, canonicalization of a node results in a structural
 * conversion of the node as a result of adding a child.  For example, a
 * node with the SBML Level&nbsp;1 function name @c sqr and a single
 * child node (the argument) will be transformed to a node of type
 * @sbmlconstant{AST_FUNCTION_POWER, ASTNodeType_t} with
 * two children.  The first child will remain unchanged, but the second child
 * will be an ASTNode_t of type @sbmlconstant{AST_INTEGER, ASTNodeType_t} and
 * a value of 2.  The function names that result in structural changes are:
 * @c log10, @c sqr, and @c sqrt.
 *
 * @param node the node to be converted.
 *
 * @returns @c 1 (true) if successful, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_canonicalize (ASTNode_t *node);


/**
 * Adds a node as a child of another node.
 *
 * Child nodes are added in order from "left-to-right".
 *
 * @param node the node which will get the new child node.
 * @param disownedChild the ASTNode_t instance to add.
 * Will become a child of the parent node.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @copydetails doc_warning_modifying_structure
 *
 * @see ASTNode_prependChild()
 * @see ASTNode_replaceChild()
 * @see ASTNode_insertChild()
 * @see ASTNode_removeChild()
 * @see ASTNode_isWellFormedASTNode()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_addChild (ASTNode_t *node, ASTNode_t *disownedChild);


/**
 * Adds a node as a child of another node.
 *
 * This method adds child nodes from right to left.
 *
 * @param node the node that will receive the given child node.
 * @param disownedChild the ASTNode_t instance to add.
 * Will become a child of the parent node.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @copydetails doc_warning_modifying_structure
 *
 * @see ASTNode_addChild()
 * @see ASTNode_replaceChild()
 * @see ASTNode_insertChild()
 * @see ASTNode_removeChild()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_prependChild (ASTNode_t *node, ASTNode_t *disownedChild);


/**
 * Removes the nth child of a given node.
 *
 * @param node the node whose child element is to be removed.
 * @param n unsigned int the index of the child to remove.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INDEX_EXCEEDS_SIZE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @copydetails doc_warning_modifying_structure
 *
 * @see ASTNode_addChild()
 * @see ASTNode_prependChild()
 * @see ASTNode_replaceChild()
 * @see ASTNode_insertChild()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_removeChild(ASTNode_t* node, unsigned int n);


/**
 * Replaces but does not delete the nth child of a given node.
 *
 * @param node the ASTNode_t node to modify.
 * @param n unsigned int the index of the child to replace.
 * @param disownedChild ASTNode_t structure to replace the nth child.
 * Will become a child of the parent node.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INDEX_EXCEEDS_SIZE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @copydetails doc_warning_modifying_structure
 *
 * @see ASTNode_addChild()
 * @see ASTNode_prependChild()
 * @see ASTNode_insertChild()
 * @see ASTNode_removeChild()
 * @see ASTNode_replaceAndDeleteChild()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_replaceChild(ASTNode_t* node, unsigned int n, ASTNode_t * disownedChild);


/**
 * Replaces and deletes the nth child of a given node.
 *
 * @param node the ASTNode_t node to modify.
 * @param n unsigned int the index of the child to replace.
 * @param disownedChild ASTNode_t structure to replace the nth child.
 * Will become a child of the parent node.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INDEX_EXCEEDS_SIZE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @copydetails doc_warning_modifying_structure
 *
 * @see ASTNode_addChild()
 * @see ASTNode_prependChild()
 * @see ASTNode_insertChild()
 * @see ASTNode_removeChild()
 * @see ASTNode_replaceChild()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_replaceAndDeleteChild(ASTNode_t* node, unsigned int n, ASTNode_t * disownedChild);


/**
 * Insert a new child node at a given point in the list of children of a
 * node.
 *
 * @param node the ASTNode_t structure to modify.
 * @param n unsigned int the index of the location where the @p disownedChild is
 * to be added.
 * @param disownedChild ASTNode_t structure to insert as the nth child.
 * Will become a child of the parent node.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INDEX_EXCEEDS_SIZE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @copydetails doc_warning_modifying_structure
 *
 * @see ASTNode_addChild()
 * @see ASTNode_prependChild()
 * @see ASTNode_replaceChild()
 * @see ASTNode_removeChild()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_insertChild(ASTNode_t* node, unsigned int n, ASTNode_t * disownedChild);


/**
 * Creates a recursive copy of a node and all its children.
 *
 * @param node the ASTNode_t structure to copy.
 *
 * @return a copy of this ASTNode_t structure and all its children.  The
 * caller owns the returned structure and is reponsible for deleting it.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_deepCopy (const ASTNode_t *node);


/**
 * Returns a child of a node according to its index number.
 *
 * @param node the node whose child should be obtained.
 * @param n the index of the desired child node.
 *
 * @return the nth child of this ASTNode_t or a null pointer if this node has
 * no nth child (<code>n &gt; </code> ASTNode_getNumChildre() <code>- 1</code>).
 *
 * @see ASTNode_getNumChildren()
 * @see ASTNode_getLeftChild()
 * @see ASTNode_getRightChild()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getChild (const ASTNode_t *node, unsigned int n);


/**
 * Returns the left-most child of a given node.
 *
 * This is equivalent to <code>ASTNode_getChild(node, 0)</code>.
 *
 * @param node the node whose child is to be returned.
 *
 * @return the left child, or a null pointer if there are no children.
 *
 * @see ASTNode_getNumChildren()
 * @see ASTNode_getChild()
 * @see ASTNode_getRightChild()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getLeftChild (const ASTNode_t *node);


/**
 * Returns the right-most child of a given node.
 *
 * If <code>ASTNode_getNumChildren(node) > 1</code>, then this is equivalent
 * to:
 * @verbatim
ASTNode_getChild(node, ASTNode_getNumChildren(node) - 1);
@endverbatim
 *
 * @param node the node whose child node is to be obtained.
 *
 * @return the right child of @p node, or a null pointer if @p node has no
 * right child.
 *
 * @see ASTNode_getNumChildren()
 * @see ASTNode_getLeftChild()
 * @see ASTNode_getChild()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
ASTNode_t *
ASTNode_getRightChild (const ASTNode_t *node);


/**
 * Returns the number of children of a given node.
 *
 * @param node the ASTNode_t structure whose children are to be counted.
 *
 * @return the number of children of @p node, or @c 0 is it has no children.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
unsigned int
ASTNode_getNumChildren (const ASTNode_t *node);


/**
 * Returns a list of nodes rooted at a given node and satisfying a given
 * predicate.
 *
 * This function performs a depth-first search of the tree rooted at the
 * given ASTNode_t structure, and returns a List_t structure of nodes for
 * which the given function <code>predicate(node)</code> returns true (i.e.,
 * nonzero).
 *
 * The predicate is passed in as a pointer to a function.  The function
 * definition must have the type @link ASTNode.h::ASTNodePredicate
 * ASTNodePredicate@endlink, which is defined as
 * @verbatim
 int (*ASTNodePredicate) (const ASTNode_t *node);
 @endverbatim
 * where a return value of @c nonzero represents true and @c zero
 * represents false.
 *
 * @param node the node at which the search is to be started.
 * @param predicate the predicate to use.
 *
 * @return the list of nodes for which the predicate returned true (i.e.,
 * nonzero).  The List_t structure returned is owned by the caller and
 * should be deleted after the caller is done using it.  The ASTNode_t
 * structures in the list, however, are @em not owned by the caller (as they
 * still belong to the tree itself), and therefore should @em not be deleted.
 *
 * @see ASTNode_fillListOfNodes()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
List_t *
ASTNode_getListOfNodes (const ASTNode_t *node, ASTNodePredicate predicate);


/**
 * Returns a list of nodes rooted at a given node and satisfying a given
 * predicate.
 *
 * This method is identical to ASTNode_getListOfNodes(), except that instead
 * of creating a new List_t structure, it uses the one passed in as argument
 * @p lst.  This function performs a depth-first search of the tree rooted at
 * the given ASTNode_t structure, and adds to @p lst the nodes for which the
 * given function <code>predicate(node)</code> returns true (i.e., nonzero).
 *
 * The predicate is passed in as a pointer to a function.  The function
 * definition must have the type @link ASTNode.h::ASTNodePredicate ASTNodePredicate
 *@endlink, which is defined as
 * @verbatim
 int (*ASTNodePredicate) (const ASTNode_t *node);
 @endverbatim
 * where a return value of @c nonzero represents true and @c zero
 * represents false.
 *
 * @param node the node at which the search is to be started.
 * @param predicate the predicate to use.
 * @param lst the list to use.
 *
 * @see ASTNode_getListOfNodes()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
void
ASTNode_fillListOfNodes ( const ASTNode_t  *node,
                          ASTNodePredicate predicate,
                          List_t           *lst );


/**
 * Returns the value of a node as a single character.
 *
 * This function should be called only when ASTNode_getType() returns
 * @sbmlconstant{AST_PLUS, ASTNodeType_t},
 * @sbmlconstant{AST_MINUS, ASTNodeType_t},
 * @sbmlconstant{AST_TIMES, ASTNodeType_t},
 * @sbmlconstant{AST_DIVIDE, ASTNodeType_t} or
 * @sbmlconstant{AST_POWER, ASTNodeType_t} for the given
 * @p node.
 *
 * @param node the node whose value is to be returned.
 *
 * @return the value of @p node as a single character, or the value
 * @c CHAR_MAX if @p node is @c NULL.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
char
ASTNode_getCharacter (const ASTNode_t *node);


/**
 * Returns the value of a node as an integer.
 *
 * This function should be called only when ASTNode_getType() returns
 * @sbmlconstant{AST_INTEGER, ASTNodeType_t} for the given @p node.
 *
 * @param node the node whose value is to be returned.
 *
 * @return the value of the given ASTNode_t structure as a
 * (<code>long</code>) integer, or the value @c LONG_MAX if @p node is @c NULL.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
long
ASTNode_getInteger (const ASTNode_t *node);


/**
 * Returns the value of a node as a string.
 *
 * This function may be called on nodes that (1) are not operators, i.e.,
 * nodes for which ASTNode_isOperator() returns false (@c 0), and (2) are not
 * numbers, i.e., for which ASTNode_isNumber() also returns false (@c 0).
 *
 * @param node the node whose value is to be returned.
 *
 * @return the value of @p node as a string, or a null pointer if @p node is @c NULL.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
const char *
ASTNode_getName (const ASTNode_t *node);


/**
 * Returns the numerator value of a node representing a rational number.
 *
 * This function should be called only when ASTNode_getType() returns
 * @sbmlconstant{AST_RATIONAL, ASTNodeType_t} for the given @p node.
 *
 * @param node the node whose value is to be returned.

 * @return the value of the numerator of @p node, or the value @c LONG_MAX if
 * @p is @c NULL.
 *
 * @see ASTNode_getDenominator()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
long
ASTNode_getNumerator (const ASTNode_t *node);


/**
 * Returns the numerator value of a node representing a rational number.
 *
 * This function should be called only when ASTNode_getType() returns
 * @sbmlconstant{AST_RATIONAL, ASTNodeType_t} for the given @p node.
 *
 * @param node the node whose value is to be returned.
 *
 * @return the value of the denominator of @p node, or the value @c LONG_MAX
 * if @p node is @c NULL.
 *
 * @see ASTNode_getNumerator()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
long
ASTNode_getDenominator (const ASTNode_t *node);


/**
 * Get the real-numbered value of a node.
 *
 * This function should be called only when ASTNode_isReal() returns nonzero
 * for @p node. This function performs the necessary arithmetic if the node
 * type is @sbmlconstant{AST_REAL_E, ASTNodeType_t} (<em>mantissa * 
 * 10<sup>exponent</sup></em>) or @sbmlconstant{AST_RATIONAL, ASTNodeType_t}
 * (<em>numerator / denominator</em>).
 *
 * @param node the node whose value is to be returned.
 *
 * @return the value of @p node as a real (double), or NaN if @p node
 * is @c NULL.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
double
ASTNode_getReal (const ASTNode_t *node);


/**
 * Get the mantissa value of a node.
 *
 * This function should be called only when ASTNode_getType() returns
 * @sbmlconstant{AST_REAL_E, ASTNodeType_t} or
 * @sbmlconstant{AST_REAL, ASTNodeType_t} for the given @p node.  If
 * ASTNode_getType() returns @sbmlconstant{AST_REAL, ASTNodeType_t} for @p
 * node, this method behaves identically to ASTNode_getReal().
 *
 * @param node the node whose value is to be returned.
 *
 * @return the value of the mantissa of @p node, or NaN if @p node is @c NULL.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
double
ASTNode_getMantissa (const ASTNode_t *node);


/**
 * Get the exponent value of a node.
 *
 * This function should be called only when ASTNode_getType() returns
 * @sbmlconstant{AST_REAL_E, ASTNodeType_t} or @sbmlconstant{AST_REAL,
 * ASTNodeType_t} for the given @p node.
 *
 * @param node the node whose value is to be returned.
 *
 * @return the value of the exponent field in the given @p node ASTNode_t
 * structure, or NaN if @p node is @c NULL.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
long
ASTNode_getExponent (const ASTNode_t *node);


/**
 * Returns the numerical value of this ASTNode_t.
 *
 * @param node the ASTNode_t whose value is to be returned.
 *
 * @return the numerical value of this ASTNode_t, or @c NaN if this
 * is not a type of node that has a numerical value.
 *
 * @note This function will return a numerical value (as a double) for 
 * any ASTNode_t that represents a number, a constant such as pi or
 * avogadro or @c 1 for nodes of type 
 * @sbmlconstant{AST_CONSTANT_TRUE, ASTNodeType_t} and @c 0 for nodes of type
 * @sbmlconstant{AST_CONSTANT_FALSE, ASTNodeType_t}. It does not evaluate
 * the node in any way so, for example, it will not return the value of 
 * a named ASTNode_t or attempt to evaluate a function. 
 * This includes a node representing @c time i.e. nodes
 * of type @sbmlconstant{AST_NAME_TIME, ASTNodeType_t}.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
double 
ASTNode_getValue(const ASTNode_t *node);
  
  
/**
 * Returns the precedence of a node in the infix math syntax of SBML
 * Level&nbsp;1.
 *
 * @copydetails doc_summary_of_string_math
 *
 * @param node the node whose precedence is to be calculated.
 *
 * @return the precedence of @p node (as defined in the SBML Level&nbsp;1
 * specification).
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_getPrecedence (const ASTNode_t *node);


/**
 * Returns the type of the given node.
 *
 * @param node the node.
 *
 * @return the type of the given ASTNode_t structure.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
ASTNodeType_t
ASTNode_getType (const ASTNode_t *node);


/**
 * Returns the MathML @c id attribute of a given node.
 *
 * @param node the node whose identifier should be returned.
 *
 * @returns the identifier of the node, or @c NULL if @p node is a null pointer.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
char *
ASTNode_getId(const ASTNode_t * node);


/**
 * Returns the MathML @c class attribute of a given node.
 *
 * @param node the node whose class should be returned.
 *
 * @returns the class identifier, or @c NULL if @p node is a null pointer.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
char *
ASTNode_getClass(const ASTNode_t * node);


/**
 * Returns the MathML @c style attribute of a given node.
 *
 * @param node the node.
 *
 * @return a string representing the @c style value, or a null value if @p node is
 * a null pointer.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
char *
ASTNode_getStyle(const ASTNode_t * node);


/**
 * Returns the SBML "units" attribute of a given node.
 *
 * @htmlinclude about-sbml-units-attrib.html
 *
 * @param node the node whose units are to be returned.
 *
 * @return the units, as a string, or a null value if @p node is a null pointer.
 *
 * @note The <code>sbml:units</code> attribute for MathML expressions is only
 * defined in SBML Level&nbsp;3.  It may not be used in Levels 1&ndash;2 of
 * SBML.
 *
 * @see SBML_parseL3Formula()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
char *
ASTNode_getUnits(const ASTNode_t * node);


/**
 * Returns @c 1 (true) if the given node represents the special symbol @c avogadro.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if this stands for @c avogadro, @c 0 (false) otherwise.
 *
 * @see SBML_parseL3Formula()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isAvogadro (const ASTNode_t * node);


/**
 * Returns @c 1 (true) if this node is some type of Boolean value or operator.
 *
 * @param node the node in question.
 *
 * @return @c 1 (true) if @p node is a Boolean (a logical operator, a
 * relational operator, or the constants @c true or @c false),
 * @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isBoolean (const ASTNode_t * node);


/**
 * Returns @c 1 (true) if the given node is something that returns a Boolean value.
 *
 * This function looks at the whole ASTNode_t structure rather than just the
 * top level of @p node. Thus, it will consider return values from MathML
 * @c piecewise statements.  In addition, if the ASTNode_t structure in @p node
 * uses a function call, this function will examine the return value of the
 * function.  Note that this is only possible in cases the ASTNode_t
 * structure can trace its parent Model_t structure; that is, the ASTNode_t
 * structure must represent the <code>&lt;math&gt;</code> element of some
 * SBML object that has already been added to an instance of an
 * SBMLDocument_t structure.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node returns a Boolean, @c 0 (false) otherwise.
 *
 * @see ASTNode_isBoolean()
 * @see ASTNode_returnsBooleanForModel()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_returnsBoolean (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node is something that returns a Boolean value.
 *
 * This function looks at the whole ASTNode_t structure rather than just the
 * top level of @p node. Thus, it will consider return values from MathML
 * @c piecewise statements.  In addition, if the ASTNode_t structure in @p node
 * uses a function call, this function will examine the return value of the
 * function using the definition of the function found in the given Model_t
 * structure given by @p model (rather than the model that might be traced
 * from @p node itself).  This function is similar to
 * ASTNode_returnsBoolean(), but is useful in situations where the ASTNode_t
 * structure has not been hooked into a model yet.
 *
 * @param node the node to query.
 * @param model the model to use as the basis for finding the definition
 * of the function.
 *
 * @return @c 1 (true) if @p node returns a Boolean, @c 0 (false) otherwise.
 *
 * @see ASTNode_isBoolean()
 * @see ASTNode_returnsBoolean()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_returnsBooleanForModel (const ASTNode_t *node, const Model_t* model);


/**
 * Returns @c 1 (true) if the given node represents a MathML constant.
 *
 * Examples of constants in this context are @c Pi, @c true, etc.
 *
 * @param node the node.
 *
 * @return @c 1 (true) if @p node is a MathML constant, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isConstant (const ASTNode_t * node);


/**
 * Returns @c 1 (true) if the given node represents a MathML constant.
 *
 * Examples of constants in this context are @c Pi, @c true, etc.
 *
 * @param node the node
 *
 * @return @c 1 (true) if @p node is a MathML constant, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isConstantNumber(const ASTNode_t * node);


/**
 * Returns @c 1 (true) if the given node represents a function.
 *
 * @param node the node.
 *
 * @return @c 1 (true) if @p node is a function in SBML, whether predefined (in SBML
 * Level&nbsp;1), defined by MathML (SBML Levels&nbsp;2&ndash;3) or
 * user-defined.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isFunction (const ASTNode_t * node);


/**
 * Returns @c 1 (true) if the given node stands for infinity.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is the special IEEE 754 value for infinity,
 * @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isInfinity (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node contains an integer value.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is of type
 * @sbmlconstant{AST_INTEGER, ASTNodeType_t}, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isInteger (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node is a MathML lambda function.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is of type
 * @sbmlconstant{AST_LAMBDA, ASTNodeType_t}, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isLambda (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node represents the log base-10 function.
 *
 * More precisely, this function tests if the given @p node's type is
 * @sbmlconstant{AST_FUNCTION_LOG, ASTNodeType_t} with two children, the
 * first of which is an @sbmlconstant{AST_INTEGER, ASTNodeType_t} equal to
 * @c 10.
 *
 * @return @c 1 (true) if @p node represents a @c log10() function,
 * @c 0 (false) otherwise.
 *
 * @see SBML_parseL3Formula()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isLog10 (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node is a logical operator.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is a MathML logical operator (@c and, @c or,
 * @c not, @c xor), @c 0 otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isLogical (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node is a named entity.
 *
 * More precisely, this returns a true value if @p node is a user-defined
 * variable name or the special symbols @c time or @c avogadro.

 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is a named variable, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isName (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node represents not-a-number.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is the special IEEE 754 value NaN ("not a
 * number"), @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isNaN (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node represents negative infinity.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is the special IEEE 754 value negative infinity,
 * @c 0 (false) otherwise.
 *
 * @see ASTNode_isInfinity()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isNegInfinity (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node contains a number.
 *
 * This is functionally equivalent to:
 * @verbatim
ASTNode_isInteger(node) || ASTNode_isReal(node).
@endverbatim
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is a number, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isNumber (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node is a mathematical operator.
 *
 * The possible mathematical operators are <code>+</code>, <code>-</code>,
 * <code>*</code>, <code>/</code> and <code>^</code> (power).
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is an operator, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isOperator (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node represents the MathML
 * <code>&lt;piecewise&gt;</code> operator.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is the MathML piecewise function, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isPiecewise (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node represents a rational number.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is of type @sbmlconstant{AST_RATIONAL,
 * ASTNodeType_t}, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isRational (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node represents a real number.
 *
 * More precisely, this node must be of one of the following types:
 * @sbmlconstant{AST_REAL, ASTNodeType_t}, @sbmlconstant{AST_REAL_E,
 * ASTNodeType_t} or @sbmlconstant{AST_RATIONAL, ASTNodeType_t}.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if the value of @p node can represent a real number,
 * @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isReal (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node represents a MathML relational operator.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is a MathML relational operator, meaning
 * <code>==</code>, <code>&gt;=</code>, <code>&lt;=</code>, <code>&gt;</code>,
 * <code>&lt;</code>, and <code>!=</code>.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isRelational (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node is the MathML square-root operator.
 *
 * More precisely, the node type must be @sbmlconstant{AST_FUNCTION_ROOT,
 * ASTNodeType_t} with two children, the first of which is an
 * @sbmlconstant{AST_INTEGER, ASTNodeType_t} node having value equal to 2.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node represents a sqrt() function, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isSqrt (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node represents a unary minus.
 *
 * A node is defined as a unary minus node if it is of type
 * @sbmlconstant{AST_MINUS, ASTNodeType_t} and has exactly one child.
 *
 * For numbers, unary minus nodes can be "collapsed" by negating the number.
 * In fact, SBML_parseFormula() does this during its parsing process, and
 * SBML_parseL3Formula() has a configuration option that allows this behavior
 * to be turned on or off.  However, unary minus nodes for symbols
 * (@sbmlconstant{AST_NAME, ASTNodeType_t}) cannot be "collapsed", so this
 * predicate function is still necessary.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is a unary minus, @c 0 (false) otherwise.
 *
 * @see SBML_parseL3Formula()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isUMinus (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node is a unary plus.
 *
 * A node is defined as a unary minus node if it is of type
 * @sbmlconstant{AST_PLUS, ASTNodeType_t} and has exactly one child.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is a unary plus, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isUPlus (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node is of a specific type and has a specific
 * number of children.
 *
 * This function is designed for use in cases such as when callers want to
 * determine if the node is a unary @c not or unary @c minus, or a @c times
 * node with no children, etc.
 *
 * @param node the node to query.
 * @param type the type that the node should have.
 * @param numchildren the number of children that the node should have.
 *
 * @return @c 1 (true) if @p node is has the specified type and number of children,
 * @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_hasTypeAndNumChildren(const ASTNode_t *node, ASTNodeType_t type, unsigned int numchildren);


/**
 * Returns @c 1 (true) if the type of the node is unknown.
 *
 * "Unknown" nodes have the type @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}.
 * Nodes with unknown types will not appear in an ASTNode_t tree returned by
 * libSBML based upon valid SBML input; the only situation in which a node
 * with type @sbmlconstant{AST_UNKNOWN, ASTNodeType_t} may appear is
 * immediately after having create a new, untyped node using the ASTNode_t
 * constructor.  Callers creating nodes should endeavor to set the type to a
 * valid node type as soon as possible after creating new nodes.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is of type @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isUnknown (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node's MathML @c id attribute is set.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if it is set, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isSetId (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node's MathML @c class attribute is set.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if the attribute is set, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isSetClass (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node's MathML @c style attribute is set.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if the attribute is set, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isSetStyle (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if this node's SBML "units" attribute is set.
 *
 * @htmlinclude about-sbml-units-attrib.html
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if the attribute is set, @c 0 (false) otherwise.
 *
 * @note The <code>sbml:units</code> attribute is only available in SBML
 * Level&nbsp;3.  It may not be used in Levels 1&ndash;2 of SBML.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isSetUnits (const ASTNode_t *node);


/**
 * Returns @c 1 (true) if the given node or any of its children have the SBML
 * "units" attribute set.
 *
 * @htmlinclude about-sbml-units-attrib.html
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if the attribute is set, @c 0 (false) otherwise.
 *
 * @note The <code>sbml:units</code> attribute is only available in SBML
 * Level&nbsp;3.  It may not be used in Levels 1&ndash;2 of SBML.
 *
 * @see ASTNode_isSetUnits()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_hasUnits (const ASTNode_t *node);


/**
 * Sets the value of a given node to a character.
 *
 * If character is one of @c +, @c -, <code>*</code>, <code>/</code> or @c ^,
 * the node type will be set accordingly.  For all other characters, the node
 * type will be set to @sbmlconstant{AST_UNKNOWN, ASTNodeType_t}.
 *
 * @param node the node to set.
 * @param value the character value for the node.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setCharacter (ASTNode_t *node, char value);


/**
 * Sets the node to represent a named entity.
 *
 * As a side-effect, this ASTNode_t object's type will be reset to
 * @sbmlconstant{AST_NAME, ASTNodeType_t} if (and <em>only if</em>) the @p
 * node was previously an operator (i.e., ASTNode_isOperator() returns true),
 * number (i.e., ASTNode_isNumber() returns true), or unknown.  This allows
 * names to be set for @sbmlconstant{AST_FUNCTION, ASTNodeType_t} nodes and
 * the like.
 *
 * @param node the node to set.
 * @param name the name value for the node.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setName (ASTNode_t *node, const char *name);


/**
 * Sets the given node to a integer and sets it type
 * to @sbmlconstant{AST_INTEGER, ASTNodeType_t}.
 *
 * @param node the node to set.
 * @param value the value to set it to.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setInteger (ASTNode_t *node, long value);


/**
 * Sets the value of a given node to a rational number and sets its type to
 * @sbmlconstant{AST_RATIONAL, ASTNodeType_t}.
 *
 * @param node the node to set.
 * @param numerator the numerator value to use.
 * @param denominator the denominator value to use.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setRational (ASTNode_t *node, long numerator, long denominator);


/**
 * Sets the value of a given node to a real (@c double) and sets its type to
 * @sbmlconstant{AST_REAL, ASTNodeType_t}.
 *
 * This is functionally equivalent to:
 * @verbatim
ASTNode_setRealWithExponent(node, value, 0);
@endverbatim
 *
 * @param node the node to set.
 * @param value the value to set the node to.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setReal (ASTNode_t *node, double value);


/**
 * Sets the value of a given node to a real (@c double) in two parts, a
 * mantissa and an exponent.
 *
 * As a side-effect, the @p node's type will be set to
 * @sbmlconstant{AST_REAL, ASTNodeType_t}.
 *
 * @param node the node to set.
 * @param mantissa the mantissa of this node's real-numbered value.
 * @param exponent the exponent of this node's real-numbered value.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setRealWithExponent (ASTNode_t *node, double mantissa, long exponent);


/**
 * Explicitly sets the type of the given ASTNode_t structure.
 *
 * @param node the node to set.
 * @param type the new type.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @note A side-effect of doing this is that any numerical values previously
 * stored in this node are reset to zero.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setType (ASTNode_t *node, ASTNodeType_t type);


/**
 * Sets the MathML @c id attribute of the given node.
 *
 * @param node the node to set.
 * @param id the identifier to use.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setId (ASTNode_t *node, const char *id);


/**
 * Sets the MathML @c class of the given node.
 *
 * @param node the node to set.
 * @param className the new value for the @c class attribute.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setClass (ASTNode_t *node, const char *className);


/**
 * Sets the MathML @c style of the given node.
 *
 * @param node the node to set.
 * @param style the new value for the @c style attribute.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setStyle (ASTNode_t *node, const char *style);


/**
 * Sets the units of the given node.
 *
 * The units will be set @em only if the ASTNode_t structure in @p node
 * represents a MathML <code>&lt;cn&gt;</code> element, i.e., represents a
 * number.  Callers may use ASTNode_isNumber() to inquire whether the node is
 * of that type.
 *
 *
 * @htmlinclude about-sbml-units-attrib.html
 *
 * @param node the node to modify.
 * @param units the units to set it to.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @note The <code>sbml:units</code> attribute is only available in SBML
 * Level&nbsp;3.  It may not be used in Levels 1&ndash;2 of SBML.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setUnits (ASTNode_t *node, const char *units);


/**
 * Swaps the children of two nodes.
 *
 * @param node the node to modify.
 *
 * @param that the other node whose children should be used to replace those
 * of @p node.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_swapChildren (ASTNode_t *node, ASTNode_t *that);


/**
 * Unsets the MathML @c id attribute of the given node.
 *
 * @param node the node to modify.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_unsetId (ASTNode_t *node);


/**
 * Unsets the MathML @c class attribute of the given node.
 *
 * @param node the node to modify.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_unsetClass (ASTNode_t *node);


/**
 * Unsets the MathML @c style attribute of the given node.
 *
 * @param node the node to modify.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_unsetStyle (ASTNode_t *node);


/**
 * Unsets the units associated with the given node.
 *
 * @param node the node to modify.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_unsetUnits (ASTNode_t *node);


/**
 * Replaces occurrences of a given name with a new ASTNode_t structure.
 *
 * For example, if the formula in @p node is <code>x + y</code>, then the
 * <code>&lt;bvar&gt;</code> is @c x and @c arg is an ASTNode_t structure
 * representing the real value @c 3.  This function substitutes @c 3 for @c x
 * within the @p node ASTNode_t structure.
 *
 * @param node the node to modify.
 * @param bvar the MathML <code>&lt;bvar&gt;</code> to use.
 * @param arg the replacement node or structure.
 *
 * @memberof ASTNode_t
 *
 * @see ASTNode_replaceAndDeleteArgument()
 */
LIBSBML_EXTERN
void
ASTNode_replaceArgument(ASTNode_t* node, const char * bvar, ASTNode_t* arg);


/**
 * Reduces the given node to a binary true.
 *
 * Example: if @p node is <code>and(x, y, z)</code>, then the formula of the
 * reduced node is <code>and(and(x, y), z)</code>.  The operation replaces
 * the formula stored in the current ASTNode_t structure.
 *
 * @param node the node to modify.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
void
ASTNode_reduceToBinary(ASTNode_t* node);


/**
 * Returns the parent SBase_t structure containing the given node.
 *
 * @param node the node to query.
 *
 * @return a pointer to the structure containing the given node.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
SBase_t *
ASTNode_getParentSBMLObject(ASTNode_t* node);


/**
 * Returns @c 1 (true) if the given node's parent SBML object is set.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if the parent SBML object is set, @c 0 (false) otherwise.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isSetParentSBMLObject(ASTNode_t* node);


/** @cond doxygenLibsbmlInternal */
/**
 * Sets the parent SBase_t structure.
 *
 * @param node the node to modify.
 * @param sb the parent SBase_t structure of this ASTNode_t.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
void 
ASTNode_setParentSBMLObject(ASTNode_t* node, SBase_t * sb);


/**
 * Unsets the parent SBase_t structure.
 *
 * @param node the node to modify.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_unsetParentSBMLObject(ASTNode_t* node);


/** @endcond */


/**
 * Adds a given XML node structure as a MathML <code>&lt;semantics&gt;</code> element
 * of a given ASTNode_t structure.
 *
 * @htmlinclude about-semantic-annotations.html
 *
 * @param node the node to modify.
 * @param disownedAnnotation the annotation to add.
 * Will become a child of the parent node.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @copydetails doc_note_mathml_semantic_annotations_uncommon
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_addSemanticsAnnotation(ASTNode_t* node, XMLNode_t * disownedAnnotation);


/**
 * Returns the number of MathML semantic annotations inside the given node.
 *
 * @htmlinclude about-semantic-annotations.html
 *
 * @param node the node to query.
 *
 * @return a count of the semantic annotations.
 *
 * @see ASTNode_addSemanticsAnnotation()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
unsigned int
ASTNode_getNumSemanticsAnnotations(ASTNode_t* node);


/**
 * Returns the nth MathML semantic annotation attached to the given node.
 *
 * @htmlinclude about-semantic-annotations.html
 *
 * @param node the node to query.
 * @param n the index of the semantic annotation to fetch.
 *
 * @return the nth semantic annotation on @p node , or a null pointer if the
 * node has no nth annotation (which would mean that <code>n &gt;
 * ASTNode_getNumSemanticsAnnotations(node) - 1</code>).
 *
 * @copydetails doc_note_mathml_semantic_annotations_uncommon
 *
 * @see ASTNode_addSemanticsAnnotation()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
XMLNode_t *
ASTNode_getSemanticsAnnotation(ASTNode_t* node, unsigned int n);


/**
 * Sets the user data of the given node.
 *
 * The user data can be used by the application developer to attach custom
 * information to the node. In case of a deep copy, this attribute will
 * passed as it is. The attribute will be never interpreted by this class.
 *
 * @param node the node to modify.
 * @param userData the new user data.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @see ASTNode_getUserData()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setUserData(ASTNode_t* node, void *userData);


/**
 * Returns the user data associated with this node.
 *
 * @param node the node to query.
 *
 * @return the user data of this node, or a null pointer if no user data has
 * been set.
 *
 * @see ASTNode_setUserData()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
void *
ASTNode_getUserData(const ASTNode_t* node);


/**
 * Unsets the user data of the given node.
 *
 * The user data can be used by the application developer to attach custom
 * information to the node. In case of a deep copy, this attribute will
 * passed as it is. The attribute will be never interpreted by this class.
 *
 * @param node the node to modify.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @see ASTNode_getUserData()
 * @see ASTNode_setUserData()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_unsetUserData(ASTNode_t* node);


/**
 * Returns @c 1 (true) if the given node's user data object is set.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if the user data object is set, @c 0 (false) otherwise.
 *
 * @see ASTNode_setUserData()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isSetUserData(const ASTNode_t* node);


/**
 * Returns @c 1 (true) if the given node has the correct number of children for its
 * type.
 *
 * For example, an ASTNode_t structure with type @sbmlconstant{AST_MINUS,
 * ASTNodeType_t} expects 1 or 2 child nodes.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node has the appropriate number of children for its
 * type, @c 0 (false) otherwise.
 *
 * @note This function performs a check on the top-level node only.  Child
 * nodes are not checked.
 *
 * @see ASTNode_isWellFormedASTNode()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_hasCorrectNumberArguments(ASTNode_t* node);


/**
 * Returns @c 1 (true) if the given node is well-formed.
 *
 * @param node the node to query.
 *
 * @return @c 1 (true) if @p node is well-formed, @c 0 (false) otherwise.
 *
 * @note An ASTNode_t may be well-formed, with each node and its children
 * having the appropriate number of children for the given type, but may
 * still be invalid in the context of its use within an SBML model.
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_isWellFormedASTNode(ASTNode_t* node);


/**
 * Returns the MathML @c definitionURL attribute value of the given node.
 *
 * @param node the node to query.
 *
 * @return the value of the @c definitionURL attribute in the form of a
 * libSBML XMLAttributes_t structure, or a null pointer if @p node does not
 * have a value for the attribute.
 *
 * @see ASTNode_getDefinitionURLString()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
XMLAttributes_t *
ASTNode_getDefinitionURL(ASTNode_t* node);


/**
 * Returns the MathML @c definitionURL attribute value of the given node as a
 * string.
 *
 * @param node the node to query.
 *
 * @return the value of the @c definitionURL attribute in the form of a
 * string, or a null pointer if @p node does not have a value for the
 * attribute.
 *
 * @see ASTNode_getDefinitionURL()
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
char *
ASTNode_getDefinitionURLString(ASTNode_t* node);


/**
 * Sets the MathML @c definitionURL attribute of the given node.
 *
 * @param node the node to modify.
 * @param defnURL the value to which the attribute should be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setDefinitionURL(ASTNode_t* node, XMLAttributes_t * defnURL);


/**
 * Sets the MathML @c definitionURL attribute of the given node.
 *
 * @param node the node to modify.
 * @param defnURL a string to which the attribute should be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_setDefinitionURLString(ASTNode_t* node, const char * defnURL);


/** @cond doxygenLibsbmlInternal */
/**
 *
 *
 * @memberof ASTNode_t
 */
LIBSBML_EXTERN
int
ASTNode_true(const ASTNode_t *node);
/** @endcond */


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* ASTNode_h */

