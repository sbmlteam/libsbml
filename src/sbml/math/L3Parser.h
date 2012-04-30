/**
 * @file    L3Parser.h
 * @brief   Definition of the level 3 infix-to-mathml parser C functions.
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

#ifndef L3Parser_h
#define L3Parser_h

#include <sbml/common/extern.h>
#include <sbml/math/ASTNode.h>
BEGIN_C_DECLS


/**
 * Parses the given SBML formula and returns a representation of it as an
 * Abstract Syntax Tree (AST).
 *
 * The text-string form of mathematical formulas produced by
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
 * @li Various settings may be altered by using an L3ParserSettings object, including:
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
 * These settings can be set once for all subsequent calls to this function
 * by calling SBML_setDefaultL3ParserSettings, or these settings can be changed
 * on an as-needed basis by using the function SBML_parseL3FormulaWithSettings.
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
 * formulas and not MathML), and expand it to include the above functionality.  
 * This formula syntax is based mostly on C
 * programming syntax, and may contain operators, function calls, symbols,
 * and white space characters.  The following table provides the precedence
 * rules for the different entities that may appear in formula strings.
 *
 * @htmlinclude math-precedence-table-l3.html
 * 
 * In the table above, @em operand implies the construct is an operand, @em
 * prefix implies the operation is applied to the following arguments, @em
 * unary implies there is one argument, and @em binary implies there are
 * two arguments.  The values in the <b>Precedence</b> column show how the
 * order of different types of operation are determined.  For example, the
 * expression <code>a + b * c</code> is evaluated as <code>a + (b * c)</code> 
 * because the @c * operator has higher precedence.  The
 * <b>Associates</b> column shows how the order of similar precedence
 * operations is determined; for example, <code>a && b || c</code> is
 * evaluated as <code>(a && b) || c</code> because the @c && and @c ||
 * operators are left-associative and have the same precedence.
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
 * functions added in SBML Level 2 and Level 3:
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
 * Note that whether or not 'avogadro' is parsed as AST_NAME_AVOGADRO or
 * AST_NAME is settable through the use of an L3ParserSettings object.  This
 * functionality is provided because SBML Level 2 models may not use
 * AST_NAME_AVOGADRO ASTNodes.
 *
 * @param formula the text-string formula expression to be parsed
 * @param settings the settings for the parser behavior
 *
 * @return the root node of the AST, or NULL if an error occurred in
 * parsing the formula
 *
 * @if clike @see SBML_formulaToString()@endif@~
 * @if csharp @see SBML_formulaToString()@endif@~
 * @if python @see libsbml.formulaToString()@endif@~
 * @if java @see formulaToString()@endif@~
 * @if clike @see SBML_parseL3FormulaWithModel()@endif@~
 * @if csharp @see SBML_parseL3FormulaWithModel()@endif@~
 * @if python @see libsbml.SBML_parseL3FormulaWithModel()@endif@~
 * @if java @see SBML_parseL3FormulaWithModel()@endif@~
 * @if clike @see SBML_parseL3FormulaWithSettings()@endif@~
 * @if csharp @see SBML_parseL3FormulaWithSettings()@endif@~
 * @if python @see libsbml.SBML_parseL3FormulaWithSettings()@endif@~
 * @if java @see SBML_parseL3FormulaWithSettings()@endif@~
 */
LIBSBML_EXTERN
ASTNode_t *
SBML_parseL3Formula (const char *formula);

/**
 * Parses the given SBML formula and returns a representation of it as an
 * Abstract Syntax Tree (AST), using the default L3ParserSettings, but with the Model
 * object provided.  If the default settings have been changed by SBML_setDefaultL3ParserSettings,
 * those settings are all used, but the model is changed for parsing this string only.
 *
 * For more details, see the SBML_parseL3Formula() function:
 * 
 * @if clike @see SBML_parseL3Formula()@endif@~
 * @if csharp @see SBML_parseL3Formula()@endif@~
 * @if python @see libsbml.SBML_parseL3Formula()@endif@~
 * @if java @see SBML_parseL3Formula()@endif@~
 * @if clike @see SBML_parseL3FormulaWithSettings()@endif@~
 * @if csharp @see SBML_parseL3FormulaWithSettings()@endif@~
 * @if python @see libsbml.SBML_parseL3FormulaWithSettings()@endif@~
 * @if java @see SBML_parseL3FormulaWithSettings()@endif@~
 */
LIBSBML_EXTERN
ASTNode_t *
SBML_parseL3FormulaWithModel (const char *formula, const Model_t * model);


/**
 * Parses the given SBML formula and returns a representation of it as an
 * Abstract Syntax Tree (AST), using the provided L3ParserSettings object.  This
 * object allows you to change the following behaviors:
 * @li Set a Model object to compare strings against.
 * @li Parse "log(x)" as the log base-10 of x, natural log of x, or as an error.
 * @li Parses "[number] [id]' by setting id to the unit of the number or as an error.
 * @li Parses "avogadro" to an ASTNode of type AST_NAME_AVOGADRO or as type AST_NAME.
 * @li Always creates explicit ASTNodes of type AST_MINUS for all unary minuses, or collapse them when possible.
 *
 * For more details, see the L3ParserSettings class, and the SBML_parseL3Formula() function:
 * 
 * @if clike @see SBML_parseL3Formula()@endif@~
 * @if csharp @see SBML_parseL3Formula()@endif@~
 * @if python @see libsbml.SBML_parseL3Formula()@endif@~
 * @if java @see SBML_parseL3Formula()@endif@~
 * @if clike @see SBML_parseL3FormulaWithModel()@endif@~
 * @if csharp @see SBML_parseL3FormulaWithModel()@endif@~
 * @if python @see libsbml.SBML_parseL3FormulaWithModel()@endif@~
 * @if java @see SBML_parseL3FormulaWithModel()@endif@~
 */
LIBSBML_EXTERN
ASTNode_t *
SBML_parseL3FormulaWithSettings (const char *formula, const L3ParserSettings_t *settings);

/*
 * Get a copy of the default settings for the SBML_parseL3Formula function.
 * 
 * @if clike @see SBML_parseL3Formula()@endif@~
 * @if csharp @see SBML_parseL3Formula()@endif@~
 * @if python @see libsbml.SBML_parseL3Formula()@endif@~
 * @if java @see SBML_parseL3Formula()@endif@~
 * @if clike @see SBML_parseL3FormulaWithModel()@endif@~
 * @if csharp @see SBML_parseL3FormulaWithModel()@endif@~
 * @if python @see libsbml.SBML_parseL3FormulaWithModel()@endif@~
 * @if java @see SBML_parseL3FormulaWithModel()@endif@~
 */
LIBSBML_EXTERN
L3ParserSettings_t*
SBML_getDefaultL3ParserSettings ();


/**
 * If @see SBML_parseL3Formula(), @see SBML_parseL3FormulaWithSettings(), or 
 * @see SBML_parseL3FormulaWithModel() return NULL, an error 
 * is set internally which is accessible via this function.  The returned error will 
 * report the string it was trying to parse, which character it had parsed when it 
 * encountered the error, and what the error was.
 * 
 */
LIBSBML_EXTERN
char*
SBML_getLastParseL3Error();

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END
#endif /* L3Parser_h */
