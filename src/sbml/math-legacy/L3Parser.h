/**
 * @file    L3Parser.h
 * @brief   Definition of the level 3 infix-to-mathml parser C functions.
 * @author  Lucian Smith
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#ifndef L3Parser_h
#define L3Parser_h

#include <sbml/common/extern.h>
#include <sbml/math/ASTNode.h>

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


/**
 * Parses the given mathematical formula and returns a representation of it
 * as an Abstract Syntax Tree (AST).
 *
 * @copydetails doc_summary_of_string_math_l3

 * @param formula the text-string formula expression to be parsed
 *
 * @return the root node of an AST representing the mathematical formula,
 * or @c NULL if an error occurred while parsing the formula.  When @c NULL
 * is returned, an error is recorded internally; information about the
 * error can be retrieved using 
 * @if clike SBML_getLastParseL3Error()@endif@if csharp SBML_getLastParseL3Error()@endif@if python libsbml.getLastParseL3Error()@endif@if java <code><a href="libsbml.html#getLastParseL3Error()">libsbml.getLastParseL3Error()</a></code>@endif@~.
 *
 * @if clike @see SBML_formulaToString()
 * @see SBML_formulaToL3String()
 * @see SBML_parseL3FormulaWithSettings()
 * @see SBML_parseL3Formula()
 * @see SBML_parseL3FormulaWithModel()
 * @see SBML_getLastParseL3Error()
 * @see SBML_getDefaultL3ParserSettings()
 * @endif@~
 * @if csharp @see SBML_formulaToString()
 * @see SBML_formulaToL3String()
 * @see SBML_parseL3FormulaWithSettings()
 * @see SBML_parseL3Formula()
 * @see SBML_parseL3FormulaWithModel()
 * @see SBML_getLastParseL3Error()
 * @see SBML_getDefaultL3ParserSettings()
 * @endif@~
 * @if python @see libsbml.formulaToString()
 * @see libsbml.formulaToL3String()
 * @see libsbml.parseL3FormulaWithSettings()
 * @see libsbml.parseL3Formula()
 * @see libsbml.parseL3FormulaWithModel()
 * @see libsbml.getLastParseL3Error()
 * @see libsbml.getDefaultL3ParserSettings()
 * @endif@~
 * @if java @see <code><a href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode tree)">libsbml.formulaToString(ASTNode tree)</a></code>
 * @see <code><a href="libsbml.html#parseL3FormulaWithSettings(java.lang.String, org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String formula, L3ParserSettings settings)</a></code>
 * @see <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>
 * @see <code><a href="libsbml.html#parseL3FormulaWithModel(java.lang.String, org.sbml.libsbml.Model)">parseL3FormulaWithModel(String formula, Model model)</a></code>
 * @see <code><a href="libsbml.html#getLastParseL3Error()">getLastParseL3Error()</a></code>
 * @see <code><a href="libsbml.html#getDefaultL3ParserSettings()">getDefaultL3ParserSettings()</a></code>
 * @endif@~
 *
 * @if conly
 * @memberof ASTNode_t
 * @endif
 */
LIBSBML_EXTERN
ASTNode_t *
SBML_parseL3Formula (const char *formula);


/**
 * Parses the given mathematical formula using specific a specific Model to
 * resolve symbols, and returns an Abstract Syntax Tree (AST)
 * representation of the result.
 *
 * This is identical to
 * @if clike SBML_parseL3Formula()@endif@if csharp SBML_parseL3Formula()@endif@if python libsbml.parseL3Formula()@endif@if java <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>@endif@~,
 * except that this function uses the given model in the argument @p model
 * to check against identifiers that appear in the @p formula.
 *
 * For more details about the parser, please see the definition of
 * the function @if clike SBML_parseL3Formula()@endif@if csharp SBML_parseL3Formula()@endif@if python libsbml.parseL3Formula()@endif@if java <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>@endif@~.
 *
 * @param formula the mathematical formula expression to be parsed
 *
 * @param model the Model object to use for checking identifiers
 *
 * @return the root node of an AST representing the mathematical formula,
 * or @c NULL if an error occurred while parsing the formula.  When @c NULL
 * is returned, an error is recorded internally; information about the
 * error can be retrieved using
 * @if clike SBML_getLastParseL3Error()@endif@if csharp SBML_getLastParseL3Error()@endif@if python libsbml.getLastParseL3Error()@endif@if java <code><a href="libsbml.html#getLastParseL3Error()">libsbml.getLastParseL3Error()</a></code>@endif@~.
 * 
 * @if clike @see SBML_formulaToString()
 * @see SBML_parseL3FormulaWithSettings()
 * @see SBML_parseL3Formula()
 * @see SBML_getLastParseL3Error()
 * @see SBML_getDefaultL3ParserSettings()
 * @endif@~
 * @if csharp @see SBML_formulaToString()
 * @see SBML_parseL3FormulaWithSettings()
 * @see SBML_parseL3Formula()
 * @see SBML_getLastParseL3Error()
 * @see SBML_getDefaultL3ParserSettings()
 * @endif@~
 * @if python @see libsbml.formulaToString()
 * @see libsbml.parseL3FormulaWithSettings()
 * @see libsbml.parseL3Formula()
 * @see libsbml.getLastParseL3Error()
 * @see libsbml.getDefaultL3ParserSettings()
 * @endif@~
 * @if java @see <code><a href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode tree)">libsbml.formulaToString(ASTNode tree)</a></code>
 * @see <code><a href="libsbml.html#parseL3FormulaWithSettings(java.lang.String, org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String formula, L3ParserSettings settings)</a></code>
 * @see <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>
 * @see <code><a href="libsbml.html#getLastParseL3Error()">getLastParseL3Error()</a></code>
 * @see <code><a href="libsbml.html#getDefaultL3ParserSettings()">getDefaultL3ParserSettings()</a></code>
 * @endif@~
 *
 * @if conly
 * @memberof ASTNode_t
 * @endif
 */
LIBSBML_EXTERN
ASTNode_t *
SBML_parseL3FormulaWithModel (const char *formula, const Model_t * model);


/**
 * Parses the given mathematical formula using specific parser settings and
 * returns an Abstract Syntax Tree (AST) representation of the result.
 *
 * This is identical to
 @if clike SBML_parseL3Formula()@endif@if csharp SBML_parseL3Formula()@endif@if python libsbml.parseL3Formula()@endif@if java <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>@endif@~,
 * except that this function uses the parser settings given in the argument
 * @p settings.  The settings override the default parsing behavior.
 *
 * The parameter @p settings allows callers to change the following parsing
 * behaviors:
 *
 * @li Use a specific Model object against which identifiers to compare
 * identifiers.  This causes the parser to search the Model for identifiers
 * that the parser encounters in the formula.  If a given symbol in the
 * formula matches the identifier of a Species, Compartment, Parameter,
 * Reaction, SpeciesReference or FunctionDefinition in the Model, then the
 * symbol is assumed to refer to that model entity instead of any possible
 * mathematical terms with the same symbol.  For example, if the parser is
 * given a Model containing a Species with the identifier
 * &quot;<code>pi</code>&quot;, and the formula to be parsed is
 * &quot;<code>3*pi</code>&quot;, the MathML produced will contain the
 * construct <code>&lt;ci&gt; pi &lt;/ci&gt;</code> instead of the
 * construct <code>&lt;pi/&gt;</code>.
 * @li Whether to parse &quot;<code>log(x)</code>&quot; with a single
 * argument as the base 10
 * logarithm of x, the natural logarithm of x, or treat the case as an
 * error.
 * @li Whether to parse &quot;<code>number id</code>&quot; by interpreting
 * @c id as the identifier of a unit of measurement associated with the
 * number, or whether to treat the case as an error.
 * @li Whether to parse &quot;<code>avogadro</code>&quot; as an ASTNode of
 * type @link ASTNodeType_t#AST_NAME_AVOGADRO AST_NAME_AVOGADRO@endlink or
 * as type @link ASTNodeType_t#AST_NAME AST_NAME@endlink.
 * @li Whether to always create explicit ASTNodes of type @link
 * ASTNodeType_t#AST_MINUS AST_MINUS@endlink for all unary minuses, or
 * collapse and remove minuses where possible.
 *
 * For more details about the parser, please see the definition of
 * L3ParserSettings and
 * @if clike SBML_parseL3Formula()@endif@if csharp SBML_parseL3Formula()@endif@if python libsbml.parseL3Formula()@endif@if java <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>@endif@~.
 *
 * @param formula the mathematical formula expression to be parsed
 *
 * @param settings the settings to be used for this parser invocation
 *
 * @return the root node of an AST representing the mathematical formula,
 * or @c NULL if an error occurred while parsing the formula.  When @c NULL
 * is returned, an error is recorded internally; information about the
 * error can be retrieved using
 * @if clike SBML_getLastParseL3Error()@endif@if csharp SBML_getLastParseL3Error()@endif@if python libsbml.getLastParseL3Error()@endif@if java <code><a href="libsbml.html#getLastParseL3Error()">libsbml.getLastParseL3Error()</a></code>@endif@~.
 * 
 * @if clike @see SBML_formulaToString()
 * @see SBML_parseL3Formula()
 * @see SBML_parseL3FormulaWithModel()
 * @see SBML_getLastParseL3Error()
 * @see SBML_getDefaultL3ParserSettings()
 * @endif@~
 * @if csharp @see SBML_formulaToString()
 * @see SBML_parseL3Formula()
 * @see SBML_parseL3FormulaWithModel()
 * @see SBML_getLastParseL3Error()
 * @see SBML_getDefaultL3ParserSettings()
 * @endif@~
 * @if python @see libsbml.formulaToString()
 * @see libsbml.parseL3Formula()
 * @see libsbml.parseL3FormulaWithModel()
 * @see libsbml.getLastParseL3Error()
 * @see libsbml.getDefaultL3ParserSettings()
 * @endif@~
 * @if java @see <code><a href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode tree)">libsbml.formulaToString(ASTNode tree)</a></code>
 * @see <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>
 * @see <code><a href="libsbml.html#parseL3FormulaWithModel(java.lang.String, org.sbml.libsbml.Model)">parseL3FormulaWithModel(String formula, Model model)</a></code>
 * @see <code><a href="libsbml.html#getLastParseL3Error()">getLastParseL3Error()</a></code>
 * @see <code><a href="libsbml.html#getDefaultL3ParserSettings()">getDefaultL3ParserSettings()</a></code>
 * @endif@~
 *
 * @if conly
 * @memberof ASTNode_t
 * @endif
 */
LIBSBML_EXTERN
ASTNode_t *
SBML_parseL3FormulaWithSettings (const char *formula, const L3ParserSettings_t *settings);


/**
 * Returns a copy of the default parser settings used by @if clike SBML_parseL3Formula()@endif@if csharp SBML_parseL3Formula()@endif@if python libsbml.parseL3Formula()@endif@if java <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>@endif@~.
 * 
 * The settings structure allows callers to change the following parsing
 * behaviors:
 * 
 * @li Use a specific Model object against which identifiers to compare
 * identifiers.  This causes the parser to search the Model for identifiers
 * that the parser encounters in the formula.  If a given symbol in the
 * formula matches the identifier of a Species, Compartment, Parameter,
 * Reaction, SpeciesReference or FunctionDefinition in the Model, then the
 * symbol is assumed to refer to that model entity instead of any possible
 * mathematical terms with the same symbol.  For example, if the parser is
 * given a Model containing a Species with the identifier
 * &quot;<code>pi</code>&quot;, and the formula to be parsed is
 * &quot;<code>3*pi</code>&quot;, the MathML produced will contain the
 * construct <code>&lt;ci&gt; pi &lt;/ci&gt;</code> instead of the
 * construct <code>&lt;pi/&gt;</code>.
 * @li Whether to parse &quot;<code>log(x)</code>&quot; with a single
 * argument as the base 10
 * logarithm of x, the natural logarithm of x, or treat the case as an
 * error.
 * @li Whether to parse &quot;<code>number id</code>&quot; by interpreting
 * @c id as the identifier of a unit of measurement associated with the
 * number, or whether to treat the case as an error.
 * @li Whether to parse &quot;<code>avogadro</code>&quot; as an ASTNode of
 * type @link ASTNodeType_t#AST_NAME_AVOGADRO AST_NAME_AVOGADRO@endlink or
 * as type @link ASTNodeType_t#AST_NAME AST_NAME@endlink.
 * @li Whether to always create explicit ASTNodes of type @link
 * ASTNodeType_t#AST_MINUS AST_MINUS@endlink for all unary minuses, or
 * collapse and remove minuses where possible.
 *
 * For more details about the parser, please see the definition of
 * L3ParserSettings and
 * @if clike SBML_parseL3Formula()@endif@if csharp SBML_parseL3Formula()@endif@if python libsbml.parseL3Formula()@endif@if java <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>@endif@~.
 * 
 * @if clike @see SBML_formulaToString()
 * @see SBML_parseL3FormulaWithSettings()
 * @see SBML_parseL3Formula()
 * @see SBML_parseL3FormulaWithModel()
 * @see SBML_getLastParseL3Error()
 * @endif@~
 * @if csharp @see SBML_formulaToString()
 * @see SBML_parseL3FormulaWithSettings()
 * @see SBML_parseL3Formula()
 * @see SBML_parseL3FormulaWithModel()
 * @see SBML_getLastParseL3Error()
 * @endif@~
 * @if python @see libsbml.formulaToString()
 * @see libsbml.parseL3FormulaWithSettings()
 * @see libsbml.parseL3Formula()
 * @see libsbml.parseL3FormulaWithModel()
 * @see libsbml.getLastParseL3Error()
 * @endif@~
 * @if java @see <code><a href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode tree)">libsbml.formulaToString(ASTNode tree)</a></code>
 * @see <code><a href="libsbml.html#parseL3FormulaWithSettings(java.lang.String, org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String formula, L3ParserSettings settings)</a></code>
 * @see <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>
 * @see <code><a href="libsbml.html#parseL3FormulaWithModel(java.lang.String, org.sbml.libsbml.Model)">parseL3FormulaWithModel(String formula, Model model)</a></code>
 * @see <code><a href="libsbml.html#getLastParseL3Error()">getLastParseL3Error()</a></code>
 * @endif@~
 *
 * @if conly
 * @memberof L3ParserSettings_t
 * @endif
 */
LIBSBML_EXTERN
L3ParserSettings_t*
SBML_getDefaultL3ParserSettings ();


/**
 * Returns the last error reported by the parser.
 *
 * If @if clike SBML_parseL3Formula()@endif@if csharp SBML_parseL3Formula()@endif@if python libsbml.parseL3Formula()@endif@if java <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>@endif@~, 
 * @if clike SBML_parseL3FormulaWithSettings()@endif@if csharp SBML_parseL3FormulaWithSettings()@endif@if python libsbml.parseL3FormulaWithSettings()@endif@if java <code><a href="libsbml.html#parseL3FormulaWithSettings(java.lang.String, org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String formula, L3ParserSettings settings)</a></code>@endif@~, or
 * @if clike SBML_parseL3FormulaWithModel()@endif@if csharp SBML_parseL3FormulaWithModel()@endif@if python libsbml.parseL3FormulaWithModel()@endif@if java <code><a href="libsbml.html#parseL3FormulaWithModel(java.lang.String, org.sbml.libsbml.Model)">libsbml.parseL3FormulaWithModel(String formula, Model model)</a></code>@endif@~ return @c NULL, an error is set internally which is accessible
 * via this function. 
 *
 * @return a string describing the error that occurred.  This will contain
 * the string the parser was trying to parse, which character it had parsed
 * when it encountered the error, and a description of the error.
 *
 * @if clike @see SBML_formulaToString()
 * @see SBML_parseL3FormulaWithSettings()
 * @see SBML_parseL3Formula()
 * @see SBML_parseL3FormulaWithModel()
 * @see SBML_getDefaultL3ParserSettings()
 * @endif@~
 * @if csharp @see SBML_formulaToString()
 * @see SBML_parseL3FormulaWithSettings()
 * @see SBML_parseL3Formula()
 * @see SBML_parseL3FormulaWithModel()
 * @see SBML_getDefaultL3ParserSettings()
 * @endif@~
 * @if python @see libsbml.formulaToString()
 * @see libsbml.parseL3FormulaWithSettings()
 * @see libsbml.parseL3Formula()
 * @see libsbml.parseL3FormulaWithModel()
 * @see libsbml.getDefaultL3ParserSettings()
 * @endif@~
 * @if java @see <code><a href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode tree)">libsbml.formulaToString(ASTNode tree)</a></code>
 * @see <code><a href="libsbml.html#parseL3FormulaWithSettings(java.lang.String, org.sbml.libsbml.L3ParserSettings)">libsbml.parseL3FormulaWithSettings(String formula, L3ParserSettings settings)</a></code>
 * @see <code><a href="libsbml.html#parseL3Formula(java.lang.String)">libsbml.parseL3Formula(String formula)</a></code>
 * @see <code><a href="libsbml.html#parseL3FormulaWithModel(java.lang.String, org.sbml.libsbml.Model)">parseL3FormulaWithModel(String formula, Model model)</a></code>
 * @see <code><a href="libsbml.html#getDefaultL3ParserSettings()">getDefaultL3ParserSettings()</a></code>
 * @endif@~
 *
 * @if conly
 * @memberof ASTNode_t
 * @endif
 */
LIBSBML_EXTERN
char*
SBML_getLastParseL3Error();

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END
#endif /* L3Parser_h */
