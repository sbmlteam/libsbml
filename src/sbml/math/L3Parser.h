/**
 * @file    L3Parser.h
 * @brief   Definition of the level 3 infix-to-mathml parser.
 * @author  Lucian Smith
 * 
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
 * ---------------------------------------------------------------------- -->*/

/**
 * The L3Parser class is an internal class designed to hold the guts of the bison parser, plus
 * the lexer.  It is designed to be a global singleton object, because that's the cleanest way
 * I could find to incorporate functions into the bison framework.
 *
 * The functions declared in this file are defined in the file L3Parser.ypp, which
 * must be compiled by bison to create L3Parser.tab.cpp, the file included in
 * libsbml.  For more details, see the L3Parser.ypp file.
 *
 * Within the various 'sbml_yylex*' functions that bison creates, functions
 * from the global 'l3p' object (of the L3Parser class) are used to calculate
 * necessary information for the parsing of the string, and to determine appropriate
 * error messages when things go wrong.
 */
#ifndef L3Parser_h
#define L3Parser_h

#include <sbml/common/libsbml-namespace.h>
#include <sbml/common/extern.h>
#include <sbml/math/ASTNode.h>
#include <sbml/Model.h>
#include <sbml/math/L3ParserSettings.h>

#ifdef __cplusplus

#include <sstream>


LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLVisitor;
class FormulaUnitsData;

class LIBSBML_EXTERN L3Parser
{
public:
  std::stringstream input;
  ASTNode* outputNode;
  std::string error;
  std::map<std::string, std::string*> words;
  long exponent;
  long denominator;

  //settings:
  const L3ParserSettings defaultParserSettings;
  Model* model;
  l3p_log_type parselog;
  bool collapseminus;
  bool parseunits;
  bool avocsymbol;

  /** @cond doxygen-libsbml-internal */


  L3Parser();
  ~L3Parser();

  //Functions needed for the parser and the lexer:
  /**
   * Compares 'name' against a list of known constants, and returns the particular
   * constant type (AST_CONSTANT_TRUE, AST_CONSTANT_PI, AST_NAME_TIME) associated
   * with that string (with case ignored).  For the mathematical constants
   * infinity and notanumber, AST_REAL is returned, and the parser must then
   * examine the string again to discover what to do with the ASTNode.
   */
  ASTNodeType_t getSymbolFor(std::string name) const;
  /**
   * Compares 'name' against a list of known functions allowed in the MathML
   * of SBML Level 2 and 3.  Multiple mappings of string->type are present,
   * so that (for example) both the strings 'acos' and 'arccos' return the
   * type AST_FUNCTION_ARCCOS.  "log" returns AST_FUNCTION_LOG, so when 
   * user preference is taken into consideration, 'name' must once again
   * be checked.
   */
  ASTNodeType_t getFunctionFor(std::string name) const;
  /**
   * This function creates an ASTNode that is a 'piecewise' function that
   * mimics the 'modulo' function 'x % y'.  It was modified from the
   * function of the same name in Copasi in its  CEvaluationNodeOperator 
   * class, which itself had modifications submitted by Frank Bergmann.  
   */
  ASTNode*      createModuloTree(ASTNode* x, ASTNode* y) const;
  /**
   * Sets the member variable 'collapseminus' to the provided boolean value.  Used in parsing
   * unary minuses.
   */
  void setCollapseMinus(bool collapse);
  /**
   * Sets the member variable 'parselog' to the provided enum.  Used in parsing
   * strings with the function "log" with a single argument.
   */
  void setParseLog(l3p_log_type parseas);
  /**
   * Sets the member variable 'parseunits' to the provided boolean.  Used in
   * deciding whether strings that assign units to numbers (like "10 mL") 
   * are parsed correctly, or as errors.
   */
  void setParseUnits(bool units);
  /**
   * Sets the member variable 'avocsymbol' to the provided boolean. Used in
   * deciding whether to parse the string 'avogadro' as an ASTNode of type
   * AST_NAME_AVOGADRO or AST_NAME with the name 'avogadro'.
   */
  void setAvoCsymbol(bool avo);
  /**
   * Compares the two strings, and returns 'true' if they are equivalent,
   * ignoring case.  Used in the parser and in the 'getSymbolFor' and 
   * 'getFunctionFor' functions.
   */
  bool caselessStrCmp(const std::string& lhs, const std::string& rhs) const;
  /**
   * Sets the input string to be parsed, copied to the 'input' stringstream
   * member variable.
   */
  void setInput(const char* c);
  /**
   * Sets the error string so that it can be retrieved by the function 
   * 'SBML_getLastParseL3Error'.
   */
  void setError(const char* c);
  /**
   * Sets the error string so that it can be retrieved by the function 
   * 'SBML_getLastParseL3Error'.
   */
  void setError(std::string c);
  /**
   * Resets the L3Parser object, removing any error or input strings,
   * setting the output ASTNode to NULL, and resetting all parser settings
   * to that stored in the 'defaultParserSettings' member variable.
   */
  void clear();
  /**
   * Returns the 'error' member variable, which is either empty or contains
   * the error message set from a 'setError' function.
   */
  std::string getError();
  /**
   * The bison parser needs string pointers to pass around from function to
   * function.  In order to not create too many of these objects, and to
   * ensure that they are properly deleted, the lexer calls this function
   * when it encounters a valid ID string to get a stable pointer that can 
   * be passed to the parser functions.
   * 
   * In this function, 'word' is looked up in a hash map of strings to 
   * string pointers; if it is not found, a new string pointer containing
   * the string is created, added to the hash, and returned.  If it is
   * found, the previously-created pointer is returned.
   */
  std::string* addWord(const std::string& word);
  /**
   * This function checks the provided ASTNode function to see if it is a 
   * known function with the wrong number of arguments.  If so, an error is set
   * (using the 'setError' function) and 'true' is returned.  If the
   * correct number of arguments is provided, 'false' is returned.
   */
  bool checkNumArguments(const ASTNode* function);
  /**
   * Provides a copy of the default parser settings member variable.
   */
  L3ParserSettings getDefaultParserSettings();

  /** @endcond */

  /**
   * Parses the given SBML formula and returns a representation of it as an
   * Abstract Syntax Tree (AST).
   *
   * @if clike The text-string form of mathematical formulas produced by
   * SBML_l3FormulaToString() and read by SBML_parseL3Formula() are expanded
   * versions of the SBML_formulaToString() and SBML_parseFormula() functions
   * that worked to produce and consume the defined L1-infix syntax, with the
   * following differences:
   *
   * @li Bare numbers may be assigned units, using the syntax "[number] [unit string]".
       The number may be in any form (an integer, real, or rational number), and the
       unit string must conform to the syntax of an SBML ID.
   * @li Boolean function symbols '&&', '||', '!', and '!=' may be used.
   * @li The 'modulo' symbol '%' is allowed, and will produce a piecewise function
       in the MathML (thanks to Copasi and Frank Bergmann for the code providing this functionality).
   * @li All 'arc-' trigonometric functions may be defined in the infix either
       using 'arc' as a prefix or simply 'a':  'arccsc' and 'acsc' both define
       the MathML-defined arc-cosecant.  Many functions in the L1 infix parser
       were defined this way as well, but not all.
   * @li "([integer]/[integer])" is parsed as a rational number instead of as a
       'divide' function with two arguments.  No spaces are allowed in this construct.
       This allows you to assign units to a rational number with formulas like
       "(3/4) ml"  "(3 / 4)" will parse as a 'divide' function and thus may not be
       assigned units.
   * @li Various settings may be altered by passing in an L3ParserSettings object to
       this function in addition to the infix string, including:
   * @li @li The function 'log' with a single argument ("log(x)") can be parsed as
       log10(x), ln(x), or as an error.
   * @li @li Unary minuses may be collapsed or expanded.
   * @li @li Unit parsing can be turned off.
   * @li @li The string 'avogadro' can be parsed as a csymbol or as a name.
   * @li @li A Model object may be provided to the parser.  When it is, 
       SIds from that model are used
       in preference to pre-defined MathML definitions:  if the translator is
       given a model with a species 'pi', and a formula '3*pi', the produced
       MathML will contain the construct '&lt;ci&gt; pi &lt;/ci&gt;' instead of the
       construct '&lt;pi/&gt;'.
   * @li @li Similarly, when a model object is provided, SIds of function 
       definitions present in the model are used
       preferentially over pre-defined MathML functions:  if the passed-in Model
       contains a FunctionDefinition 'sin', that function will be used instead
       of the predefined MathML &lt;sin/&gt;.
   *
   * The function returns the root node of the AST corresponding to the formula.  If
   * the formula contains a syntax error, @c NULL is returned instead.  When NULL
   * is returned, an error is set:  @see SBML_getLastParseL3Error()
   *
   * Note that this facility (and the L1-based SBML_parseFormula) is provided as a convenience by libSBML&mdash;the
   * MathML standard does not actually define a "string-form" equivalent to
   * MathML expression trees, so the choice of formula syntax is somewhat
   * arbitrary.  The approach taken by libSBML is to start with the syntax defined by
   * SBML Level&nbsp;1 (which in fact used a text-string representation of
   * formulas and not MathML), and expand it to include the above functionality.  This formula syntax is based mostly on C
   * programming syntax, and may contain operators, function calls, symbols,
   * and white space characters.  The following table provides the precedence
   * rules for the different entities that may appear in formula strings.
   *
   * @htmlinclude math-precedence-table.html
   * 
   * In the table above, @em operand implies the construct is an operand, @em
   * prefix implies the operation is applied to the following arguments, @em
   * unary implies there is one argument, and @em binary implies there are
   * two arguments.  The values in the <b>Precedence</b> column show how the
   * order of different types of operation are determined.  For example, the
   * expression <code>a * b + c</code> is evaluated as <code>(a * b) +
   * c</code> because the @c * operator has higher precedence.  The
   * <b>Associates</b> column shows how the order of similar precedence
   * operations is determined; for example, <code>a && b || c</code> is
   * evaluated as <code>(a && b) || c</code> because the @c && and @c ||
   * operators are left-associative.
   *
   * The function call syntax consists of a function name, followed by optional
   * white space, followed by an opening parenthesis token, followed by a
   * sequence of zero or more arguments separated by commas (with each comma
   * optionally preceded and/or followed by zero or more white space
   * characters, followed by a closing parenthesis token.  The function name
   * must be chosen from one of the pre-defined functions in SBML or a
   * user-defined function in the model.  The following table lists the names
   * of certain common mathematical functions; this table corresponds to
   * Table&nbsp;6 in the <a target="_blank" href="http://sbml.org/Documents/Specifications#SBML_Level_1_Version_2">SBML Level&nbsp;1 Version&nbsp;2 specification</a> with additions based on the 
   * functions adding in SBML Level 2 and Level 3:
   *
   * @htmlinclude string-functions-table-l3.html
   *
   * Note that this function's interpretation of the string 'log' as a function with a single 
   * argument can be changed through the use of an L3ParserSettings object.
   * By default, unlike the Level 1 parser, 'log' is interpreted as log base-10, and
   * not as the natural log.  However, you can change the interpretation to
   * be log base-10, natural log, or as an error:  since the string is
   * ambiguous, you require the use of 'log10' or 'ln' instead, which 
   * are more clear.
   * 
   * In addition, the following symbols will be translated to MathML concepts,
   * if no symbol with that SId is found in the provided Model object:
   *
   * @htmlinclude string-values-table-l3.html
   * 
   * @param formula the text-string formula expression to be parsed
   * @param model the model for which the formula expression is being parsed for, to resolve any ambiguities between pre-defined symbols and model-defined symbols.
   *
   * @return the root node of the AST, or NULL if an error occurred in
   * parsing the formula
   *
   * @if clike @see SBML_formulaToString()@endif
   * @if csharp @see SBML_formulaToString()@endif
   * @if java @see formulaToString(ASTNode tree)@endif
   */
  static ASTNode * parseL3Formula(const std::string formula,
                                  L3ParserSettings settings);


  /**
   * Parses the provided string to an ASTNode, using the default settings, namely:
   * @li No 'Model' object to compare strings against.
   * @li Parses "log(x)" as the log base-10 of x.
   * @li Parses "[number] [id]' by setting id to the unit of the number.
   * @li Parses "avogadro" to an ASTNode of type AST_NAME_AVOGADRO.
   * @li Always creates explicit ASTNodes of type AST_MINUS for all unary minuses.
   *
   * For other aspects of this function, @see SBML_parseL3Formula(const char *formula, L3ParserSettings settings)
   */
  static ASTNode * parseL3Formula(const std::string formula);

  /**
   * If the either versions of the function @see SBML_parseL3Formula() returns NULL, an error 
   * is set internally which is accessible via this function.  The returned error will 
   * report the string it was trying to parse, which character it had parsed when it 
   * encountered the error, and what the error was.
   * 
   */
  static char* getLastParseL3Error();
};

/** @cond doxygen-libsbml-internal */

/**
 * The global l3p object is defined in the L3Parser.ypp file. This 'extern' 
 * declaration lets others interact with it, if they so desire.
 */
extern L3Parser* l3p;
/** @endcond */



LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


LIBSBML_EXTERN
ASTNode_t *
SBML_parseL3Formula (const char *formula);


LIBSBML_EXTERN
ASTNode_t *
SBML_parseL3FormulaWithSettings (const char *formula, L3ParserSettings_t *settings);


LIBSBML_EXTERN
ASTNode_t *
SBML_parseL3FormulaWithModel (const char *formula, Model_t * model);


LIBSBML_EXTERN
char*
SBML_getLastParseL3Error();

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END
#endif /* L3Parser_h */
